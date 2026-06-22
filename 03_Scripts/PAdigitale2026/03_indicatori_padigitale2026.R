# ============================================================ #
# Script: 03_indicatori_padigitale2026.R
# Fonte: PA digitale 2026 - Open data
#
# Obiettivo:
#   Produrre il DB indicatori PA digitale 2026 nel FORMATO LONG
#   condiviso dal gruppo (come PagoPA), pronto come input della
#   dashboard unica:
#
#     pa | fil | fil_val | sub_fil | sub_fil_val | ind | ind_val
#
#   Output unico:  INDICATORS_PADIGITALE2026.json  (+ .csv, .rds)
#   Legenda:       MET_INDICATORS_PADIGITALE2026   (indicatori + filtri)
#
#   Input: master lista-PAD26 prodotto dallo script 02
#   (lista_pad26_long), che conserva l'intero perimetro MPA:
#   gli enti senza candidatura restano come righe con in_pad26 = 0.
#
# ------------------------------------------------------------------
# CONTRATTO DI LETTURA (importante per chi scrive la dashboard)
# ------------------------------------------------------------------
# La tabella e' in formato EAV "long". Ogni riga = valore di UN
# indicatore (ind) per UN ente (pa), letto lungo UNA dimensione di
# filtro (fil) ed eventualmente una sotto-dimensione (sub_fil).
# Per aggregare correttamente la dashboard deve:
#   1) filtrare su UN SOLO valore di 'fil' (es. fil == "fil_reg");
#   2) (opz.) filtrare anche 'sub_fil' (es. sub_fil == "fil_anno");
#   3) SOMMARE ind_val solo per gli indicatori ADDITIVI (ind1, ind2),
#      raggruppando per fil_val;
#   4) calcolare medie/rapporti DOPO l'aggregazione (vedi legenda).
# NON sommare mai ind_val mescolando valori di 'fil' diversi:
# lo stesso dato compare sotto piu' "lenti" (regione, fg, avviso...)
# e sommarle insieme produrrebbe doppi conteggi.
#
# Gli enti del perimetro senza candidatura compaiono solo nelle lenti
# territoriali / forma giuridica con ind1 = 0 e ind2 = 0: servono come
# DENOMINATORE per la copertura (n. enti finanziati / n. enti MPA).
#
# Le sezioni  # >>> DA DECIDERE  contengono scelte da concordare nel team.
# ============================================================ #


# 0) Pulizia ambiente ---------------------------------------------------------

rm(list = ls())


# 1) Configurazione e helper --------------------------------------------------

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/00_spatial_helpers.R")   # normalizza_codice_regione/provincia
source("03_Scripts/helper_console_log.R")


# 2) Pacchetti ----------------------------------------------------------------

{
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(janitor)
  library(googledrive)
  library(jsonlite)
  library(openxlsx)
}


# 3) Autenticazione Drive -----------------------------------------------------

googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")


# 4) Parametri del run --------------------------------------------------------

delete_local_temp <- FALSE

# RUN_ID del raccordo (script 02) da cui leggere il master lista-PAD26.
# >>> DA AGGIORNARE: copiare il RUN_ID stampato dallo script 02.
RUN_ID_RACCORDO <- "20260622_145431"

# RUN_ID di questo run.
RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Nome del file master salvato dallo script 02 (rds | json | csv).
NOME_FILE_MASTER <- "lista_pad26_long.json"

# Etichetta fonte (per la colonna 'fonte' della legenda).
FONTE_LABEL <- "PA digitale 2026"

# Anno di riferimento: usato per 'fil_anno' se NON c'e' una data per record.
# >>> DA DECIDERE: anno della finestra dati PA digitale 2026.
ANNO_RIFERIMENTO <- 2024

message("RUN_ID_RACCORDO: ", RUN_ID_RACCORDO)
message("RUN_ID indicatori: ", RUN_ID)


# 5) Directory locali e remote ------------------------------------------------
# NB: usare lo stesso nome variabile definito in 00_config.R.
# Nel config attuale la cartella indicatori e' DRIVE_DIR_INDICATORS
# ("01_Dataset/Indicatori"). Se hai rinominato in DRIVE_DIR_INDICATORS,
# allinea config e script (vedi nota nel messaggio di accompagnamento).

{
  DIR_PAD26_PROCESSED_INPUT_LOCAL <- file.path(
    DIR_TEMP, "PADigitale2026", "Processed", RUN_ID_RACCORDO
  )
  DIR_PAD26_INDICATORS_LOCAL <- file.path(
    DIR_TEMP, "PADigitale2026", "Indicatori", RUN_ID
  )
  DIR_PAD26_LOGS_LOCAL <- file.path(
    DIR_TEMP, "PADigitale2026", "Logs", RUN_ID
  )

  DRIVE_PAD26_PROCESSED_INPUT <- file.path(
    DRIVE_DIR_PROCESSED, "PADigitale2026", RUN_ID_RACCORDO
  )
  DRIVE_PAD26_INDICATORS <- file.path(
    DRIVE_DIR_INDICATORS, "PADigitale2026", RUN_ID
  )
  DRIVE_PAD26_LOGS <- file.path(
    DRIVE_DIR_LOGS, "PADigitale2026", RUN_ID
  )
}


# 6) Creazione directory locali -----------------------------------------------

{
  dir.create(DIR_PAD26_PROCESSED_INPUT_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_PAD26_INDICATORS_LOCAL, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_PAD26_LOGS_LOCAL, recursive = TRUE, showWarnings = FALSE)
}


# 7) Avvio console log --------------------------------------------------------

console_log <- start_console_log(
  log_dir = DIR_PAD26_LOGS_LOCAL,
  run_id = RUN_ID,
  script_name = "03_indicatori_PAdigitale2026"
)

message("Console log locale: ", console_log$path)


# 8) Funzioni di supporto -----------------------------------------------------

n_distinct_nona <- function(x) length(unique(x[!is.na(x)]))

leggi_master <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") {
    readRDS(path)
  } else if (ext == "json") {
    jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    stop("Estensione non supportata per il master: ", ext)
  }
}

assicura_colonne <- function(df, cols, tipo = c("character", "numeric")) {
  tipo <- match.arg(tipo)
  vuoto <- if (tipo == "numeric") NA_real_ else NA_character_
  for (cc in cols) if (!cc %in% names(df)) df[[cc]] <- vuoto
  df
}


# 9) Mappa delle colonne attese (lato lista + lato PAD26) ---------------------
# Centralizzata qui: se nello script 02 cambiano i nomi, si corregge solo qui.

COL <- list(
  codice_fiscale = "codice_fiscale",
  in_pad26       = "in_pad26",

  # dimensioni territoriali / strutturali (lato lista)
  cod_regione    = "codice_reg",
  cod_provincia  = "codice_provincia",
  cod_comune     = "codice_comune",
  desc_fg        = "desc_fg",

  # dimensioni / misure (lato PAD26)
  avviso         = "pad26_avviso",
  importo        = "pad26_importo_finanziamento",

  # data della candidatura per derivare l'anno (se presente).
  # >>> DA VERIFICARE il nome esatto nel raccordo (o lasciare NA).
  data_candidatura = "pad26_data_invio_candidatura"
)


# 10) Import master lista-PAD26 da Drive --------------------------------------

file_master_local <- file.path(DIR_PAD26_PROCESSED_INPUT_LOCAL, NOME_FILE_MASTER)

drive_download_from_path(
  drive_file_rel = file.path(DRIVE_PAD26_PROCESSED_INPUT, NOME_FILE_MASTER),
  local_path = file_master_local
)

if (!file.exists(file_master_local)) {
  stop("File master lista-PAD26 non trovato: ", file_master_local)
}

master <- leggi_master(file_master_local) %>%
  tibble::as_tibble() %>%
  janitor::clean_names()

message("Master lista-PAD26 caricato. Righe: ", nrow(master))

cat(
  paste(
    sort(names(master)),
    collapse = "\n"
  )
)

names(master)[
  stringr::str_detect(
    names(master),
    paste0(
      "avviso|misura|destinat|data_|stato|ente|",
      "regione|provincia|comune|desc_fg|ateco"
    )
  )
]
# 11) Preparazione campi base -------------------------------------------------

master <- master %>%
  assicura_colonne(
    c(COL$codice_fiscale, COL$cod_regione, COL$cod_provincia, COL$cod_comune,
      COL$desc_fg, COL$avviso),
    tipo = "character"
  ) %>%
  assicura_colonne(c(COL$in_pad26, COL$importo), tipo = "numeric")

ha_data <- !is.na(COL$data_candidatura) && COL$data_candidatura %in% names(master)

base <- master %>%
  mutate(
    pa         = .data[[COL$codice_fiscale]],
    in_pad26   = dplyr::coalesce(as.integer(.data[[COL$in_pad26]]), 0L),

    # chiavi-filtro standard del progetto (vocabolario fil_)
    fil_reg    = normalizza_codice_regione(.data[[COL$cod_regione]]),
    fil_prov   = normalizza_codice_provincia(.data[[COL$cod_provincia]]),
    fil_com    = as.character(.data[[COL$cod_comune]]),
    fil_fg     = .data[[COL$desc_fg]],
    fil_avviso = .data[[COL$avviso]],

    # importo: si assume gia' numerico dal raccordo; gli enti senza
    # candidatura (in_pad26 = 0) contano 0 nella somma.
    importo    = dplyr::coalesce(suppressWarnings(as.numeric(.data[[COL$importo]])), 0),

    # n. candidature additivo: 1 per ogni riga-candidatura, 0 per i non finanziati
    n_cand     = as.integer(in_pad26 == 1L)
  ) %>%
  mutate(
    fil_anno = if (ha_data) {
      format(as.Date(.data[[COL$data_candidatura]]), "%Y")  # base R, niente lubridate
    } else {
      as.character(ANNO_RIFERIMENTO)
    }
  )

message("Anno disponibile per record: ", ha_data,
        " | enti perimetro: ", n_distinct_nona(base$pa),
        " | candidature: ", sum(base$n_cand))


# 12) Nucleo: emissione di una "lente" di filtro in formato long --------------
# Aggrega le misure additive (ind1, ind2) per pa x dimensione (+ sub-dim),
# poi impila gli indicatori in formato long.

emit_fil <- function(df, fil_name, fil_col,
                     sub_fil_name = NA_character_, sub_fil_col = NA_character_,
                     includi_non_finanziati = TRUE) {

  d <- df
  if (!includi_non_finanziati) d <- dplyr::filter(d, in_pad26 == 1L)

  # scarta righe senza valore sulla dimensione di filtro
  d <- dplyr::filter(d, !is.na(.data[[fil_col]]), .data[[fil_col]] != "")

  group_cols <- c("pa", fil_col)
  if (!is.na(sub_fil_col)) group_cols <- c(group_cols, sub_fil_col)

  d %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      ind1 = sum(n_cand, na.rm = TRUE),   # n. candidature finanziate
      ind2 = sum(importo, na.rm = TRUE),  # importo finanziato (euro)
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(c(ind1, ind2), names_to = "ind", values_to = "ind_val") %>%
    dplyr::transmute(
      pa,
      fil         = fil_name,
      fil_val     = as.character(.data[[fil_col]]),
      sub_fil     = sub_fil_name,
      sub_fil_val = if (!is.na(sub_fil_col)) as.character(.data[[sub_fil_col]]) else NA_character_,
      ind,
      ind_val
    )
}


# 13) Costruzione del DB indicatori long --------------------------------------
# >>> DA DECIDERE: quali lenti tenere. Qui: avviso (specifica PAD26),
# territoriali (reg/prov/com), forma giuridica, + croci con l'anno.

lenti <- list(
  emit_fil(base, "fil_avviso", "fil_avviso", includi_non_finanziati = FALSE),
  emit_fil(base, "fil_reg",  "fil_reg"),
  emit_fil(base, "fil_prov", "fil_prov"),
  emit_fil(base, "fil_com",  "fil_com"),
  emit_fil(base, "fil_fg",   "fil_fg")
)

# croci temporali solo se esiste una data reale per record
if (ha_data) {
  lenti <- c(lenti, list(
    emit_fil(base, "fil_reg",    "fil_reg",    "fil_anno", "fil_anno"),
    emit_fil(base, "fil_avviso", "fil_avviso", "fil_anno", "fil_anno",
             includi_non_finanziati = FALSE)
  ))
}

indicatori_long <- dplyr::bind_rows(lenti) %>%
  dplyr::arrange(ind, fil, fil_val, pa)

message("DB indicatori long creato. Righe: ", nrow(indicatori_long))


# 14) Metadati degli indicatori e dei filtri ---------------------------------
#
# Sono prodotti tre livelli di documentazione:
#   1) legenda_indicatori: descrizione logica degli indicatori;
#   2) legenda_filtri: vocabolario dei filtri;
#   3) metadata_indicatori: tabella operativa, una riga per ogni
#      combinazione indicatore x filtro effettivamente disponibile.
#
# La tabella metadata_indicatori segue il formato condiviso per la dashboard:
# Dataset | Dataset Originale | Nome_variabile | Nome_indicatore |
# Nome_filtro | Nome_sub_filtro | Formula | X1 | X2 | X3 |
# Anno_di_riferimento | Note

legenda_indicatori <- tibble::tribble(
  ~ind, ~label, ~additivo, ~unita_misura, ~formula_descrittiva,
  ~asse_monitoraggio, ~riforma_pnrr,

  "ind1", "Candidature finanziate", TRUE, "numero",
  "Somma dei valori di ind1 dopo aver selezionato una sola lente di filtro.",
  "Transizione digitale della PA", "M1C1 - PA digitale 2026",

  "ind2", "Importo finanziato", TRUE, "euro",
  "Somma dei valori di ind2 dopo aver selezionato una sola lente di filtro.",
  "Transizione digitale della PA", "M1C1 - PA digitale 2026",

  "n_enti_finanziati", "Enti finanziati", FALSE, "numero",
  "Numero distinto di enti con almeno una candidatura finanziata.",
  "Transizione digitale della PA", "M1C1 - PA digitale 2026",

  "n_enti_perimetro", "Enti del perimetro MPA", FALSE, "numero",
  "Numero distinto di enti presenti nel perimetro MPA.",
  "Copertura fonte", "Trasversale",

  "copertura_perc", "Copertura PA digitale 2026", FALSE, "%",
  "Rapporto percentuale tra enti finanziati ed enti del perimetro MPA.",
  "Copertura fonte", "M1C1 - PA digitale 2026",

  "importo_medio_candidatura", "Importo medio per candidatura", FALSE, "euro",
  "Importo finanziato complessivo diviso per il numero di candidature.",
  "Transizione digitale della PA", "M1C1 - PA digitale 2026",

  "importo_medio_ente", "Importo medio per ente finanziato", FALSE, "euro",
  "Importo finanziato complessivo diviso per il numero di enti finanziati.",
  "Transizione digitale della PA", "M1C1 - PA digitale 2026",

  "candidature_per_ente", "Candidature per ente finanziato", FALSE, "rapporto",
  "Numero di candidature diviso per il numero di enti finanziati.",
  "Transizione digitale della PA", "M1C1 - PA digitale 2026",

  "n_avvisi", "Avvisi distinti", FALSE, "numero",
  "Numero distinto di avvisi presenti nella lente fil_avviso.",
  "Transizione digitale della PA", "M1C1 - PA digitale 2026"
) %>%
  dplyr::mutate(
    fonte = FONTE_LABEL,
    run_id = RUN_ID,
    .before = 1
  )

legenda_filtri <- tibble::tribble(
  ~fil, ~descrizione,
  "fil_reg",    "Codice regione ISTAT a 2 cifre dell'ente",
  "fil_prov",   "Codice provincia ISTAT a 3 cifre dell'ente",
  "fil_com",    "Codice comune dell'ente",
  "fil_fg",     "Forma giuridica dell'ente, derivata da desc_fg",
  "fil_avviso", "Avviso o misura PA digitale 2026",
  "fil_anno",   "Anno della candidatura, utilizzato anche come sotto-filtro"
) %>%
  dplyr::mutate(
    fonte = FONTE_LABEL,
    run_id = RUN_ID,
    .before = 1
  )

# Formule operative per la dashboard.
# X1, X2 e X3 indicano gli operandi richiamati nella colonna formula.
regole_indicatori <- tibble::tribble(
  ~nome_variabile, ~nome_indicatore_base, ~formula, ~x1, ~x2, ~x3,
  ~filtri_ammessi, ~note_formula,

  "ind1", "PAD26_candidature_finanziate", "SUM(X1)",
  "ind_val con ind = 'ind1'", NA_character_, NA_character_,
  "tutti",
  "Indicatore additivo. Non sommare valori appartenenti a lenti di filtro diverse.",

  "ind2", "PAD26_importo_finanziato", "SUM(X1)",
  "ind_val con ind = 'ind2'", NA_character_, NA_character_,
  "tutti",
  "Indicatore additivo espresso in euro.",

  "n_enti_finanziati", "PAD26_enti_finanziati", "N_DISTINCT(X1)",
  "pa con ind = 'ind1' e ind_val > 0", NA_character_, NA_character_,
  "tutti",
  "Calcolare dopo aver selezionato una sola combinazione di filtro e sotto-filtro.",

  "n_enti_perimetro", "PAD26_enti_perimetro_MPA", "N_DISTINCT(X1)",
  "pa", NA_character_, NA_character_,
  "perimetro",
  "Non applicabile alla lente fil_avviso, che contiene soltanto enti finanziati.",

  "copertura_perc", "PAD26_copertura_percentuale", "100 * X1 / X2",
  "n_enti_finanziati", "n_enti_perimetro", NA_character_,
  "perimetro",
  "Restituire NA quando X2 è nullo o uguale a zero.",

  "importo_medio_candidatura", "PAD26_importo_medio_candidatura", "X1 / X2",
  "SUM(ind_val con ind = 'ind2')", "SUM(ind_val con ind = 'ind1')", NA_character_,
  "tutti",
  "Restituire NA quando X2 è nullo o uguale a zero.",

  "importo_medio_ente", "PAD26_importo_medio_ente", "X1 / X2",
  "SUM(ind_val con ind = 'ind2')", "n_enti_finanziati", NA_character_,
  "tutti",
  "Restituire NA quando X2 è nullo o uguale a zero.",

  "candidature_per_ente", "PAD26_candidature_per_ente", "X1 / X2",
  "SUM(ind_val con ind = 'ind1')", "n_enti_finanziati", NA_character_,
  "tutti",
  "Restituire NA quando X2 è nullo o uguale a zero.",

  "n_avvisi", "PAD26_avvisi_distinti", "N_DISTINCT(X1)",
  "fil_val con fil = 'fil_avviso'", NA_character_, NA_character_,
  "solo_avviso",
  "Applicabile esclusivamente alla lente fil_avviso."
)

# Combinazioni di filtro realmente presenti nel file indicatori.
filtri_disponibili <- indicatori_long %>%
  dplyr::distinct(fil, sub_fil) %>%
  dplyr::mutate(
    sub_fil = dplyr::na_if(sub_fil, ""),
    suffisso_filtro = stringr::str_remove(fil, "^fil_") %>%
      stringr::str_to_upper(),
    suffisso_sub_filtro = dplyr::if_else(
      is.na(sub_fil),
      NA_character_,
      stringr::str_remove(sub_fil, "^fil_") %>% stringr::str_to_upper()
    )
  )

anni_disponibili <- sort(unique(stats::na.omit(base$fil_anno)))
anno_metadata <- if (length(anni_disponibili) == 0L) {
  as.character(ANNO_RIFERIMENTO)
} else {
  paste(anni_disponibili, collapse = " | ")
}

metadata_indicatori <- tidyr::crossing(
  regole_indicatori,
  filtri_disponibili
) %>%
  dplyr::filter(
    filtri_ammessi == "tutti" |
      (filtri_ammessi == "perimetro" & fil != "fil_avviso") |
      (filtri_ammessi == "solo_avviso" & fil == "fil_avviso")
  ) %>%
  dplyr::left_join(
    legenda_indicatori %>%
      dplyr::select(
        ind,
        label,
        additivo,
        unita_misura,
        formula_descrittiva,
        asse_monitoraggio,
        riforma_pnrr
      ),
    by = c("nome_variabile" = "ind")
  ) %>%
  dplyr::left_join(
    legenda_filtri %>%
      dplyr::select(fil, descrizione_filtro = descrizione),
    by = "fil"
  ) %>%
  dplyr::mutate(
    dataset = "INDICATORS_PADIGITALE2026",
    dataset_originale = NOME_FILE_MASTER,
    nome_indicatore = paste0(
      nome_indicatore_base,
      "_",
      suffisso_filtro,
      dplyr::if_else(
        is.na(suffisso_sub_filtro),
        "",
        paste0("_", suffisso_sub_filtro)
      )
    ),
    nome_filtro = fil,
    nome_sub_filtro = sub_fil,
    anno_di_riferimento = anno_metadata,
    note = paste(
      label,
      note_formula,
      formula_descrittiva,
      sep = " | "
    ),
    fonte = FONTE_LABEL,
    run_id = RUN_ID
  ) %>%
  dplyr::select(
    dataset,
    dataset_originale,
    nome_variabile,
    nome_indicatore,
    nome_filtro,
    nome_sub_filtro,
    formula,
    x1,
    x2,
    x3,
    anno_di_riferimento,
    note,
    label,
    descrizione_filtro,
    additivo,
    unita_misura,
    asse_monitoraggio,
    riforma_pnrr,
    fonte,
    run_id
  ) %>%
  dplyr::arrange(nome_variabile, nome_filtro, nome_sub_filtro)

# Dizionario dei campi del file di metadati.
dizionario_campi_metadata <- tibble::tribble(
  ~campo, ~descrizione,
  "dataset", "Nome tecnico del file di indicatori cui si riferisce il metadato.",
  "dataset_originale", "File sorgente usato per produrre gli indicatori.",
  "nome_variabile", "Codice della variabile o dell'indicatore logico.",
  "nome_indicatore", "Identificativo univoco dell'indicatore per la specifica lente di filtro.",
  "nome_filtro", "Dimensione principale di filtro da applicare alla dashboard.",
  "nome_sub_filtro", "Eventuale seconda dimensione di filtro.",
  "formula", "Espressione operativa dell'indicatore espressa mediante X1, X2 e X3.",
  "x1", "Primo operando della formula.",
  "x2", "Secondo operando della formula.",
  "x3", "Terzo operando della formula, quando necessario.",
  "anno_di_riferimento", "Anno o insieme di anni presenti nei dati.",
  "note", "Spiegazione sintetica e regole di aggregazione.",
  "label", "Etichetta leggibile dell'indicatore.",
  "descrizione_filtro", "Descrizione della dimensione di filtro.",
  "additivo", "TRUE se l'indicatore può essere sommato dopo aver selezionato una sola lente.",
  "unita_misura", "Unità di misura dell'indicatore.",
  "asse_monitoraggio", "Asse analitico o di monitoraggio.",
  "riforma_pnrr", "Riferimento alla misura o componente PNRR.",
  "fonte", "Fonte amministrativa dei dati.",
  "run_id", "Identificativo temporale del run che ha prodotto i metadati."
)

message(
  "Metadati indicatori creati. Righe: ",
  nrow(metadata_indicatori)
)


# 15) Salvataggio output e metadati su Drive ---------------------------------
# Versioning per cartella RUN_ID, coerente con l'architettura del progetto.

salva <- function(obj, base_filename, formati = c("rds", "csv", "json")) {
  paths <- character()

  if ("rds" %in% formati) {
    p <- file.path(DIR_PAD26_INDICATORS_LOCAL, paste0(base_filename, ".rds"))
    saveRDS(obj, p)
    drive_upload_or_update(p, DRIVE_PAD26_INDICATORS)
    paths <- c(paths, p)
  }

  if ("csv" %in% formati) {
    p <- file.path(DIR_PAD26_INDICATORS_LOCAL, paste0(base_filename, ".csv"))
    readr::write_csv(obj, p, na = "")
    drive_upload_or_update(p, DRIVE_PAD26_INDICATORS)
    paths <- c(paths, p)
  }

  if ("json" %in% formati) {
    p <- file.path(DIR_PAD26_INDICATORS_LOCAL, paste0(base_filename, ".json"))
    jsonlite::write_json(
      obj,
      p,
      pretty = TRUE,
      dataframe = "rows",
      na = "null",
      null = "null",
      auto_unbox = TRUE,
      Date = "ISO8601",
      POSIXt = "ISO8601",
      digits = NA
    )
    drive_upload_or_update(p, DRIVE_PAD26_INDICATORS)
    paths <- c(paths, p)
  }

  if (!all(file.exists(paths))) {
    stop(
      "Uno o più file non sono stati salvati per ",
      base_filename
    )
  }

  message("Salvato: ", base_filename)
  invisible(paths)
}

salva_metadata_xlsx <- function(
    metadata,
    legenda_indicatori,
    legenda_filtri,
    dizionario_campi,
    base_filename
) {
  p <- file.path(
    DIR_PAD26_INDICATORS_LOCAL,
    paste0(base_filename, ".xlsx")
  )

  wb <- openxlsx::createWorkbook()

  fogli <- list(
    "Metadati indicatori" = metadata %>%
      dplyr::rename(
        Dataset = dataset,
        `Dataset Originale` = dataset_originale,
        Nome_variabile = nome_variabile,
        Nome_indicatore = nome_indicatore,
        Nome_filtro = nome_filtro,
        Nome_sub_filtro = nome_sub_filtro,
        Formula = formula,
        X1 = x1,
        X2 = x2,
        X3 = x3,
        Anno_di_riferimento = anno_di_riferimento,
        Note = note
      ),
    "Legenda indicatori" = legenda_indicatori,
    "Legenda filtri" = legenda_filtri,
    "Dizionario campi" = dizionario_campi
  )

  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE,
    border = "Bottom"
  )

  for (nome_foglio in names(fogli)) {
    openxlsx::addWorksheet(wb, nome_foglio)
    openxlsx::writeData(
      wb,
      sheet = nome_foglio,
      x = fogli[[nome_foglio]],
      withFilter = TRUE,
      headerStyle = header_style,
      keepNA = FALSE
    )
    openxlsx::freezePane(wb, nome_foglio, firstRow = TRUE)
    openxlsx::setColWidths(wb, nome_foglio, cols = 1:ncol(fogli[[nome_foglio]]), widths = "auto")
    openxlsx::setColWidths(
      wb,
      nome_foglio,
      cols = 1:ncol(fogli[[nome_foglio]]),
      widths = pmin(45, pmax(12, nchar(names(fogli[[nome_foglio]])) + 3))
    )
    openxlsx::addStyle(
      wb,
      nome_foglio,
      style = openxlsx::createStyle(wrapText = TRUE, valign = "top"),
      rows = 2:(nrow(fogli[[nome_foglio]]) + 1),
      cols = 1:ncol(fogli[[nome_foglio]]),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  openxlsx::saveWorkbook(wb, p, overwrite = TRUE)

  if (!file.exists(p)) {
    stop("File XLSX dei metadati non salvato: ", p)
  }

  drive_upload_or_update(p, DRIVE_PAD26_INDICATORS)
  message("Salvato: ", basename(p))
  invisible(p)
}

# File principale degli indicatori.
salva(
  indicatori_long,
  "INDICATORS_PADIGITALE2026"
)

# Metadati operativi per la dashboard: CSV e JSON.
salva(
  metadata_indicatori,
  "MET_INDICATORS_PADIGITALE2026",
  formati = c("csv", "json")
)

# Workbook leggibile per gli utenti, con spiegazioni e dizionari.
salva_metadata_xlsx(
  metadata = metadata_indicatori,
  legenda_indicatori = legenda_indicatori,
  legenda_filtri = legenda_filtri,
  dizionario_campi = dizionario_campi_metadata,
  base_filename = "MET_INDICATORS_PADIGITALE2026"
)

# Mantiene anche i vocabolari separati in formato macchina.
salva(
  legenda_indicatori,
  "MET_LEGENDA_INDICATORS_PADIGITALE2026",
  formati = c("csv", "json")
)

salva(
  legenda_filtri,
  "MET_FILTRI_PADIGITALE2026",
  formati = c("csv", "json")
)

message(
  "Indicatori e metadati PA digitale 2026 caricati in: ",
  DRIVE_PAD26_INDICATORS
)


# 16) Chiusura console log ----------------------------------------------------

console_log_path <- stop_console_log(console_log, status = "completed")
drive_upload_or_update(local_path = console_log_path, drive_folder_rel = DRIVE_PAD26_LOGS)


# 17) Pulizia file temporanei -------------------------------------------------

if (delete_local_temp) {
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Indicatori", RUN_ID), recursive = TRUE)
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID_RACCORDO), recursive = TRUE)
}

message("--- 03_indicatori_padigitale2026 completato. RUN_ID: ", RUN_ID, " ---")
