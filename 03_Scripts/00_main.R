#..............................................................................#
# SCRIPT: costruzione lista.xlsx
# PROGETTO: Monitoraggio-PNRR / MIPA
#
# SCOPO DELLO SCRIPT
# Costruire la lista di riferimento delle amministrazioni a partire dalla lista MPA.
# La lista MPA definisce il perimetro operativo del progetto: ogni record finale
# deve appartenere a MPA. Le altre fonti servono ad arricchire, controllare o
# documentare la lista, ma non ad ampliarne il perimetro.
#
# ARCHITETTURA DEL PROGETTO
# - Gli script sono versionati in GitHub nella cartella 03_Scripts.
# - I dati e gli output sono salvati su Google Drive nella repository
#   Monitoraggio-PNRR.
# - I file vengono scaricati localmente in 07_Temp, elaborati, poi caricati
#   su Drive nelle cartelle corrette.
# - La cartella 07_Temp è solo una cache tecnica locale: può essere cancellata
#   a fine esecuzione.
#
# INPUT DRIVE
# - 01_Dataset/Lists/11 05 2026 Lista MPA_2025.xlsx
# - 01_Dataset/Lists/11 05 2026 Lista S13_2025.xlsx
# - 01_Dataset/Lists/Anagrafe-Enti-BDAP.xlsx
#
# OUTPUT DRIVE
# - 01_Dataset/Lists/lista.xlsx
#   Lista finale pulita da usare nei raccordi e nelle dashboard.
#
# - 05_Logs/lista/lista_audit_<RUN_ID>.xlsx
#   File di audit con controlli, conflitti, duplicati e copertura.
#
# - 02_Metadata/lista/metadata_lista_<RUN_ID>.xlsx
#   Metadati delle variabili della lista finale.
#
# Nota:
# tutti gli output collegati alla costruzione della lista, ad eccezione
# del file operativo lista.xlsx, vengono salvati in un sottfolder "lista".
#
# SCELTE METODOLOGICHE SINTETICHE
# 1. MPA è il perimetro della master list.
# 2. S13 e BDAP sono fonti di arricchimento e controllo.
# 3. Le colonne duplicate tra fonti non vengono tenute nella lista finale:
#    vengono confrontate e documentate nei log.
# 4. I conflitti tra fonti vengono registrati prima di applicare regole
#    di priorità.
# 5. La lista finale contiene solo variabili pulite, stabili e documentate.
#..............................................................................#

#..............................................................................#
rm(list=ls())

#..............................................................................#
#                                 IMPORT                                    ####
#..............................................................................#
source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")

library(readxl) 
library(googledrive)
library(dplyr)
library(writexl)
library(purrr)
library(stringr)
library(tibble)

#..............................................................................#
#                             CONFIGURATIONS                                ####
#..............................................................................#
# Autenticazione a Google Drive.
# Serve per scaricare i file sorgente e ricaricare gli output finali.
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

# Parametro operativo:
# - FALSE: mantiene i file locali in 07_Temp per ispezione/debug;
# - TRUE: cancella i file locali temporanei a fine esecuzione.
delete_local_temp <- FALSE

# Identificativo univoco del run.
# Serve per nominare log e audit in modo tracciabile.
RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("RUN_ID costruzione lista: ", RUN_ID)

DRIVE_DIR_METADATA_LISTA <- file.path(DRIVE_DIR_METADATA, "lista")
DRIVE_DIR_LOGS_LISTA <- file.path(DRIVE_DIR_LOGS, "lista")

#..............................................................................#
#                     PATH DI INPUT E OUTPUT                                ####
#..............................................................................#

# Nomi dei file sorgente su Drive.
file_mpa_name  <- "11 05 2026 Lista MPA_2025.xlsx"
file_s13_name  <- "11 05 2026 Lista S13_2025.xlsx"
file_bdap_name <- "Anagrafe-Enti-BDAP.xlsx"

# Path relativi su Drive.
# Questi path usano le variabili definite in 00_config.R.
drive_file_mpa  <- file.path(DRIVE_DIR_LISTS, file_mpa_name)
drive_file_s13  <- file.path(DRIVE_DIR_LISTS, file_s13_name)
drive_file_bdap <- file.path(DRIVE_DIR_LISTS, file_bdap_name)

# Path locali temporanei.
local_file_mpa  <- file.path(DIR_TEMP, file_mpa_name)
local_file_s13  <- file.path(DIR_TEMP, file_s13_name)
local_file_bdap <- file.path(DIR_TEMP, file_bdap_name)

# Output locali temporanei.
local_lista_file    <- file.path(DIR_TEMP, "lista.xlsx")
local_audit_file    <- file.path(DIR_TEMP, paste0("lista_audit_", RUN_ID, ".xlsx"))
local_metadata_file <- file.path(DIR_TEMP, paste0("metadata_lista_", RUN_ID, ".xlsx"))

#..............................................................................#
#                              FUNCTIONS                                    ####
#..............................................................................#

# Controlla se una chiave identifica univocamente le righe di una fonte.
# Se ci sono duplicati, il merge può moltiplicare i record.
check_keys <- function(df, keys, source_name) {
  missing_keys <- setdiff(keys, names(df))
  
  if (length(missing_keys) > 0) {
    stop(
      "Nella fonte ", source_name, " mancano queste chiavi: ",
      paste(missing_keys, collapse = ", ")
    )
  }
  
  df %>%
    count(across(all_of(keys)), name = "n") %>%
    filter(n > 1) %>%
    mutate(source = source_name, .before = 1)
}


# Aggiunge un suffisso di fonte a tutte le colonne non chiave.
# Evita suffissi automatici come .x/.y e rende leggibile la provenienza.
add_source_suffix <- function(df, source_suffix, keys) {
  df %>%
    rename_with(
      .fn = ~ paste0(.x, "_", source_suffix),
      .cols = -all_of(keys)
    )
}


# Trova variabili presenti in due fonti con la stessa radice.
# Esempio: RAGIONE_SOCIALE_mpa e RAGIONE_SOCIALE_bdap.
find_duplicate_pairs_by_suffix <- function(df, suffix_a, suffix_b) {
  names_df <- names(df)
  
  base_a <- names_df[stringr::str_detect(names_df, paste0("_", suffix_a, "$"))] %>%
    stringr::str_remove(paste0("_", suffix_a, "$"))
  
  base_b <- names_df[stringr::str_detect(names_df, paste0("_", suffix_b, "$"))] %>%
    stringr::str_remove(paste0("_", suffix_b, "$"))
  
  common_base <- intersect(base_a, base_b)
  
  tibble(
    variable_base = common_base,
    var_a = paste0(common_base, "_", suffix_a),
    var_b = paste0(common_base, "_", suffix_b),
    source_a = suffix_a,
    source_b = suffix_b
  )
}


# Registra i conflitti tra due colonne omologhe.
# Un conflitto esiste quando entrambe le fonti hanno un valore non mancante
# e i valori differiscono.
log_conflicts_one_pair <- function(df, id_cols, var_a, var_b, source_a, source_b) {
  df %>%
    mutate(
      value_a = as.character(.data[[var_a]]),
      value_b = as.character(.data[[var_b]]),
      value_a_clean = stringr::str_squish(value_a),
      value_b_clean = stringr::str_squish(value_b),
      conflict = case_when(
        is.na(value_a_clean) & is.na(value_b_clean) ~ FALSE,
        is.na(value_a_clean) | is.na(value_b_clean) ~ FALSE,
        value_a_clean != value_b_clean ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(conflict) %>%
    transmute(
      across(all_of(id_cols)),
      variable = stringr::str_remove(var_a, paste0("_", source_a, "$")),
      source_a = toupper(source_a),
      var_a = var_a,
      value_a = value_a,
      source_b = toupper(source_b),
      var_b = var_b,
      value_b = value_b
    )
}


# Applica il log dei conflitti a tutte le coppie duplicate trovate.
make_conflict_log <- function(df, pairs, id_cols) {
  if (nrow(pairs) == 0) {
    return(tibble())
  }
  
  purrr::pmap_dfr(
    pairs,
    function(variable_base, var_a, var_b, source_a, source_b) {
      log_conflicts_one_pair(
        df = df,
        id_cols = id_cols,
        var_a = var_a,
        var_b = var_b,
        source_a = source_a,
        source_b = source_b
      )
    }
  )
}


# Seleziona il primo valore disponibile secondo un ordine di priorità.
# Usa solo colonne effettivamente presenti nel dataset, così lo script è più robusto
# a piccole differenze nei nomi tra versioni dei file.
coalesce_existing <- function(df, cols) {
  cols <- intersect(cols, names(df))
  
  if (length(cols) == 0) {
    return(rep(NA_character_, nrow(df)))
  }
  
  out <- rep(NA_character_, nrow(df))
  
  for (col in cols) {
    value <- as.character(df[[col]])
    out <- ifelse(is.na(out) & !is.na(value), value, out)
  }
  
  out
}


# Registra la fonte da cui proviene il valore finale selezionato.
source_existing <- function(df, cols, sources) {
  source_map <- tibble(col = cols, source = sources) %>%
    filter(col %in% names(df))
  
  if (nrow(source_map) == 0) {
    return(rep(NA_character_, nrow(df)))
  }
  
  out <- rep(NA_character_, nrow(df))
  
  for (i in seq_len(nrow(source_map))) {
    col <- source_map$col[i]
    src <- source_map$source[i]
    value <- as.character(df[[col]])
    
    out <- ifelse(is.na(out) & !is.na(value), src, out)
  }
  
  out
}


#..............................................................................#
#                              IMPORT LISTS                                 ####
#..............................................................................#

#file temporanei locali
drive_download_from_path(drive_file_mpa,  local_file_mpa,  overwrite = TRUE)
drive_download_from_path(drive_file_s13,  local_file_s13,  overwrite = TRUE)
drive_download_from_path(drive_file_bdap, local_file_bdap, overwrite = TRUE)

#caricamento su R
MPA_raw <- readxl::read_excel(local_file_mpa)
s13_raw <- readxl::read_excel(local_file_s13)
BDAP_raw <- readxl::read_excel(local_file_bdap)


#..............................................................................#
#                       STANDARDIZZAZIONE MINIMA FONTI                      ####
#..............................................................................#

# Chiave operativa di raccordo.
#
# SCELTA OPERATIVA SULLE CHIAVI
# Per il raccordo tra MPA, S13 e BDAP usiamo CODICE_FISCALE come chiave principale.
# CODICE_REG non viene usato nel merge: viene mantenuto come variabile descrittiva/
# territoriale e come controllo di coerenza tra fonti.
#
# Implicazione metodologica:
# il codice fiscale deve identificare univocamente il record MPA.
# Se MPA presenta più righe per lo stesso CODICE_FISCALE, la lista non è a livello
# di ente ma a livello di unità/record MPA, e la chiave va rivalutata.
keys_main <- c("CODICE_FISCALE")

# Creiamo indicatori di presenza nelle fonti.
# Questi indicatori servono a sapere da quali fonti arriva ogni record.
MPA_raw$presente_mpa <- 1
s13_raw$presente_s13 <- 1
BDAP_raw$presente_bdap <- 1

# Uniformiamo i nomi delle chiavi BDAP ai nomi usati nelle altre fonti.
# In BDAP il codice fiscale è CF e il codice regione è Codice_Regione.
BDAP_raw$CODICE_FISCALE <- BDAP_raw$CF
BDAP_raw$CODICE_REG <- BDAP_raw$Codice_Regione


#..............................................................................#
#                   CONTROLLI DI QUALITÀ PRIMA DEI MERGE                    ####
#..............................................................................#

# Controlliamo se le chiavi sono univoche nelle tre fonti.
# Questo log è cruciale: se ci sono duplicati, il join può aumentare il numero
# di righe finali.
duplicate_keys_mpa <- check_keys(MPA_raw, keys_main, "MPA")
duplicate_keys_s13 <- check_keys(s13_raw, keys_main, "S13")
duplicate_keys_bdap <- check_keys(BDAP_raw, keys_main, "BDAP")

duplicate_keys_log <- bind_rows(
  duplicate_keys_mpa,
  duplicate_keys_s13,
  duplicate_keys_bdap
)


# Elenco delle variabili originarie per fonte.
# Utile per completare il dizionario dati.
source_variables <- bind_rows(
  tibble(source = "MPA", variable_original = names(MPA_raw)),
  tibble(source = "S13", variable_original = names(s13_raw)),
  tibble(source = "BDAP", variable_original = names(BDAP_raw))
)


#..............................................................................#
#              GESTIONE BDAP: RECORD STORICIZZATI E DUPLICATI              ####
#..............................................................................#

# BDAP può contenere più righe per lo stesso CODICE_FISCALE.
# Per evitare che il join moltiplichi le righe MPA, BDAP viene ridotta
# a una sola riga per CODICE_FISCALE prima del merge.
#
# Regola adottata:
# - se esiste una sola riga attiva, viene selezionata quella;
# - una riga è attiva se Data_Cessazione è mancante o vuota;
# - se non esiste nessuna riga attiva, il caso viene segnalato;
# - se esistono più righe attive, il caso viene segnalato come ambiguo.
#
# Le righe non selezionate non entrano nel merge operativo ma vengono salvate
# nel file audit.

BDAP_active_raw <- BDAP_raw %>%
  mutate(
    data_cessazione_clean = stringr::str_squish(as.character(Data_Cessazione)),
    is_active_bdap = is.na(data_cessazione_clean) | data_cessazione_clean == ""
  )

bdap_duplicate_keys <- BDAP_active_raw %>%
  count(CODICE_FISCALE, name = "n_righe_bdap") %>%
  filter(n_righe_bdap > 1)

bdap_duplicate_keys_mpa <- bdap_duplicate_keys %>%
  inner_join(
    MPA_raw %>% distinct(CODICE_FISCALE),
    by = "CODICE_FISCALE"
  )

bdap_active_check <- BDAP_active_raw %>%
  semi_join(
    bdap_duplicate_keys_mpa,
    by = "CODICE_FISCALE"
  ) %>%
  group_by(CODICE_FISCALE) %>%
  summarise(
    n_righe_bdap = n(),
    n_attive = sum(is_active_bdap),
    n_cessate = sum(!is_active_bdap),
    denominazione_values = paste(sort(unique(na.omit(as.character(Denominazione)))), collapse = " | "),
    codice_reg_values = paste(sort(unique(na.omit(as.character(CODICE_REG)))), collapse = " | "),
    ateco_values = paste(sort(unique(na.omit(as.character(Codice_ATECO)))), collapse = " | "),
    forma_giuridica_values = paste(sort(unique(na.omit(as.character(Descr_Forma_Giuridica)))), collapse = " | "),
    data_cessazione_values = paste(sort(unique(na.omit(data_cessazione_clean))), collapse = " | "),
    .groups = "drop"
  ) %>%
  mutate(
    bdap_duplicate_case = case_when(
      n_attive == 1 ~ "una_attiva",
      n_attive == 0 ~ "nessuna_attiva",
      n_attive > 1 ~ "multiple_attive",
      TRUE ~ "controllare"
    )
  )

bdap_active_problems <- bdap_active_check %>%
  filter(bdap_duplicate_case != "una_attiva")
bdap_duplicate_case_summary <- bdap_active_check %>%
  count(bdap_duplicate_case, name = "n_codici_fiscali")

if (nrow(bdap_active_problems) > 0) {
  stop(
    "BDAP contiene codici fiscali duplicati con zero o più di una riga attiva. ",
    "Controllare bdap_active_problems prima di procedere."
  )
}

BDAP_ranked_raw <- BDAP_active_raw %>%
  group_by(CODICE_FISCALE) %>%
  arrange(
    desc(is_active_bdap),
    Data_Cessazione,
    .by_group = TRUE
  ) %>%
  mutate(
    row_selected_bdap = row_number() == 1,
    n_righe_bdap_originali = n(),
    n_righe_bdap_attive = sum(is_active_bdap),
    bdap_record_storicizzato = as.integer(n_righe_bdap_originali > 1)
  ) %>%
  ungroup()

BDAP_for_merge_raw <- BDAP_ranked_raw %>%
  filter(row_selected_bdap)

bdap_rows_excluded_by_dedup <- BDAP_ranked_raw %>%
  filter(!row_selected_bdap)

bdap_dedup_rule_log <- BDAP_ranked_raw %>%
  group_by(CODICE_FISCALE) %>%
  summarise(
    n_righe_bdap_originali = n(),
    n_righe_attive = sum(is_active_bdap),
    n_righe_cessate = sum(!is_active_bdap),
    selected_is_active = any(row_selected_bdap & is_active_bdap),
    
    n_denominazioni_distinte = n_distinct(Denominazione, na.rm = TRUE),
    denominazione_values = paste(sort(unique(na.omit(as.character(Denominazione)))), collapse = " | "),
    
    n_codici_reg_distinti = n_distinct(CODICE_REG, na.rm = TRUE),
    codice_reg_values = paste(sort(unique(na.omit(as.character(CODICE_REG)))), collapse = " | "),
    
    n_ateco_distinti = n_distinct(Codice_ATECO, na.rm = TRUE),
    ateco_values_originali = paste(sort(unique(na.omit(as.character(Codice_ATECO)))), collapse = " | "),
    
    n_forme_giuridiche_distinte = n_distinct(Descr_Forma_Giuridica, na.rm = TRUE),
    forma_giuridica_values_originali = paste(sort(unique(na.omit(as.character(Descr_Forma_Giuridica)))), collapse = " | "),
    
    data_cessazione_values = paste(sort(unique(na.omit(data_cessazione_clean))), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(n_righe_bdap_originali > 1) %>%
  mutate(
    bdap_storicizzazione_ambigua = case_when(
      n_denominazioni_distinte > 1 & n_forme_giuridiche_distinte > 1 ~ 1,
      n_denominazioni_distinte > 1 & n_ateco_distinti > 1 ~ 1,
      n_codici_reg_distinti > 1 ~ 1,
      TRUE ~ 0
    )
  )

bdap_storicizzazioni_ambigue <- bdap_dedup_rule_log %>%
  filter(bdap_storicizzazione_ambigua == 1)

bdap_storicizzazione_flags <- bdap_dedup_rule_log %>%
  transmute(
    CODICE_FISCALE,
    bdap_storicizzazione_ambigua = bdap_storicizzazione_ambigua
  )

BDAP_for_merge_raw <- BDAP_for_merge_raw %>%
  left_join(
    bdap_storicizzazione_flags,
    by = "CODICE_FISCALE"
  ) %>%
  mutate(
    bdap_storicizzazione_ambigua = if_else(
      is.na(bdap_storicizzazione_ambigua),
      0,
      bdap_storicizzazione_ambigua
    )
  )


#..............................................................................#
#                 PREPARAZIONE DELLE FONTI PER IL MERGE ####
#..............................................................................#

# Aggiungiamo suffissi espliciti alle colonne non chiave.

MPA <- MPA_raw %>%
  add_source_suffix(source_suffix = "mpa", keys = keys_main)

s13 <- s13_raw %>%
  add_source_suffix(source_suffix = "s13", keys = keys_main)

BDAP <- BDAP_for_merge_raw %>%
  add_source_suffix(source_suffix = "bdap", keys = keys_main)
# 
# BDAP <- BDAP_raw %>%
#   add_source_suffix(source_suffix = "bdap", keys = keys_main)


#..............................................................................#
#                      COSTRUZIONE DELLA MASTER LIST                        ####
#..............................................................................#
# BDAP_raw
# ↓
# creo is_active_bdap da Data_Cessazione
# ↓
# identifico duplicati su CODICE_FISCALE
# ↓
# loggo duplicati e casi problematici
# ↓
# scelgo riga per merge:
#   1. se esiste una sola riga attiva, tengo quella
# 2. se non esistono righe attive, tengo la più recente per Data_Cessazione e flaggo problema
# 3. se esistono più righe attive, non scelgo automaticamente oppure mi fermo
# ↓
# BDAP_for_merge_raw
# ↓
# merge con MPA

# SCELTA METODOLOGICA:
# MPA è il perimetro della lista.
# Partiamo quindi da MPA e aggiungiamo S13 e BDAP.
# S13 e BDAP vengono agganciati a MPA, ma non aggiungono nuove righe
# se contengono enti assenti da MPA.
#
# Nota:
# se dopo il join il numero di righe cresce, il problema non è il left_join
# in sé, ma la presenza di duplicati nelle chiavi di raccordo.

n_mpa_before_join <- nrow(MPA)

MPA_S13 <- MPA %>%
  left_join(s13, by = keys_main)

n_after_s13_join <- nrow(MPA_S13)

MPA_S13_BDAP <- MPA_S13 %>%
  left_join(BDAP, by = keys_main)

n_after_bdap_join <- nrow(MPA_S13_BDAP)


# Log per verificare se i join hanno moltiplicato le righe.
join_row_count_log <- tibble(
  step = c(
    "MPA iniziale",
    "Dopo join con S13",
    "Dopo join con BDAP"
  ),
  n_rows = c(
    n_mpa_before_join,
    n_after_s13_join,
    n_after_bdap_join
  ),
  run_id = RUN_ID
)


#..............................................................................#
#           ENTI PRESENTI IN ALTRE FONTI MA FUORI DAL PERIMETRO MPA         ####
#..............................................................................#

# Questi enti non entrano nella lista finale perché MPA è il perimetro.
# Li salviamo però in audit, perché sono informativamente utili:
# indicano copertura differenziale tra fonti.

s13_fuori_perimetro_mpa <- s13 %>%
  anti_join(MPA, by = keys_main)

bdap_fuori_perimetro_mpa <- BDAP %>%
  anti_join(MPA, by = keys_main)


#..............................................................................#
#       IDENTIFICAZIONE e LOG DELLE VARIABILI DUPLICATE TRA FONTI           ####
#..............................................................................#

# Individuiamo variabili omologhe tra fonti.
# Esempio: RAGIONE_SOCIALE_mpa e RAGIONE_SOCIALE_s13.
pairs_mpa_s13 <- find_duplicate_pairs_by_suffix(MPA_S13_BDAP, "mpa", "s13")
pairs_mpa_bdap <- find_duplicate_pairs_by_suffix(MPA_S13_BDAP, "mpa", "bdap")
pairs_s13_bdap <- find_duplicate_pairs_by_suffix(MPA_S13_BDAP, "s13", "bdap")

duplicate_pairs_log <- bind_rows(
  pairs_mpa_s13,
  pairs_mpa_bdap,
  pairs_s13_bdap
)

# Registriamo i conflitti prima di applicare le regole di priorità.
conflict_log <- bind_rows(
  make_conflict_log(MPA_S13_BDAP, pairs_mpa_s13, keys_main),
  make_conflict_log(MPA_S13_BDAP, pairs_mpa_bdap, keys_main),
  make_conflict_log(MPA_S13_BDAP, pairs_s13_bdap, keys_main)
) %>%
  mutate(run_id = RUN_ID, .before = 1)


#..............................................................................#
#                   COSTRUZIONE DELLA LISTA FINALE PULITA                   ####
#..............................................................................#

# Costruiamo variabili finali senza suffissi tecnici.
#
# Per ogni variabile finale, scegliamo i valori secondo una priorità esplicita.
# Manteniamo anche alcune colonne "fonte_*" per sapere da quale fonte arriva
# il valore finale selezionato.
#
# Importante:
# le colonne originarie con suffissi _mpa, _s13, _bdap restano nel file audit,
# ma non nella lista finale.


# Regole di priorità adottate in questa versione:
#
# - Perimetro: MPA.
# - Codice fiscale e codice regione: MPA, perché definiscono il record finale.
# - Ragione sociale: MPA > BDAP > S13.
#   Questa scelta è coerente con MPA come lista di riferimento.
#   BDAP resta fonte di controllo e fallback.
# - FG: MPA > S13 > BDAP.
# - Codici fonte-specifici: dalla rispettiva fonte.
# - ATECO: BDAP.
#
# Le colonne originali con suffissi _mpa, _s13, _bdap non vengono tenute nella
# lista finale, ma restano documentate nel file audit.


lista <- MPA_S13_BDAP %>%
  mutate(
    # Identificativi principali.
    codice_fiscale = CODICE_FISCALE,
    codice_reg = coalesce_existing(
      .,
      c("CODICE_REG_mpa", "CODICE_REG_bdap", "CODICE_REG_s13")
    ),
    
    fonte_codice_reg = source_existing(
      .,
      cols = c("CODICE_REG_mpa", "CODICE_REG_bdap", "CODICE_REG_s13"),
      sources = c("MPA", "BDAP", "S13")
    ),
    
    # Denominazione / ragione sociale.
     ragione_sociale = coalesce_existing(
      .,
      c("RAGIONE_SOCIALE_mpa", "RAGIONE_SOCIALE_bdap", "RAGIONE_SOCIALE_s13")
      # c("RAGIONE_SOCIALE_bdap", "RAGIONE_SOCIALE_mpa", "RAGIONE_SOCIALE_s13")
    ),
    
    fonte_ragione_sociale = source_existing(
      .,
      cols = c("RAGIONE_SOCIALE_mpa", "RAGIONE_SOCIALE_bdap", "RAGIONE_SOCIALE_s13"),
      sources = c("MPA", "BDAP", "S13")
      # cols = c("RAGIONE_SOCIALE_bdap", "RAGIONE_SOCIALE_mpa", "RAGIONE_SOCIALE_s13"),
      # sources = c("BDAP", "MPA", "S13")
    ),
    
    # Campo FG.
    fg = coalesce_existing(
      .,
      c("FG_mpa", "FG_s13", "FG_bdap")
    ),
    
    fonte_fg = source_existing(
      .,
      cols = c("FG_mpa", "FG_s13", "FG_bdap"),
      sources = c("MPA", "S13", "BDAP")
    ),
    
    # Indicatori di presenza nelle fonti.
    presente_mpa = coalesce_existing(., c("presente_mpa_mpa")),
    presente_s13 = coalesce_existing(., c("presente_s13_s13")),
    presente_bdap = coalesce_existing(., c("presente_bdap_bdap")),
    
    # Codici fonte-specifici.
    codice_unita_mpa = coalesce_existing(., c("CODICE_UNITA_UG_mpa", "CODICE_UNITA_mpa")),
    codice_unita_s13 = coalesce_existing(., c("CODICE_UNITA_s13")),
    
    bdap_record_storicizzato = coalesce_existing(
      .,
      c("bdap_record_storicizzato_bdap")
    ),
    
    bdap_storicizzazione_ambigua = coalesce_existing(
      .,
      c("bdap_storicizzazione_ambigua_bdap")
    ),
    
    bdap_n_righe_originali = coalesce_existing(
      .,
      c("n_righe_bdap_originali_bdap")
    ),
    
    
    # Classificazione ATECO da BDAP.
    ateco_bdap = coalesce_existing(., c("Codice_ATECO_bdap", "CODICE_ATECO_bdap")),
    run_id = RUN_ID
  ) %>%
  select(
    codice_fiscale,
    codice_reg,
    fonte_codice_reg,
    ragione_sociale,
    fonte_ragione_sociale,
    fg,
    fonte_fg,
    codice_unita_mpa,
    codice_unita_s13,
    ateco_bdap,
    presente_mpa,
    presente_s13,
    presente_bdap,
    bdap_record_storicizzato,
    bdap_storicizzazione_ambigua,
    bdap_n_righe_originali,
    run_id
  )


#..............................................................................#
#                  LOG DI COPERTURA DELLA LISTA FINALE                      ####
#..............................................................................#

# Questo log dà una sintesi della qualità e completezza della lista finale.
# È utile per capire rapidamente quanti record MPA sono stati arricchiti
# con informazioni S13 e BDAP.

coverage_log <- lista %>%
  summarise(
    run_id = RUN_ID,
    n_record_lista = n(),
    n_codice_fiscale_mancante = sum(is.na(codice_fiscale)),
    n_codice_reg_mancante = sum(is.na(codice_reg)),
    n_ragione_sociale_mancante = sum(is.na(ragione_sociale)),
    n_fg_mancante = sum(is.na(fg)),
    n_presenti_in_mpa = sum(!is.na(presente_mpa)),
    n_presenti_anche_in_s13 = sum(!is.na(presente_s13)),
    n_presenti_anche_in_bdap = sum(!is.na(presente_bdap)),
    n_ateco_bdap_valorizzato = sum(!is.na(ateco_bdap)),
    n_bdap_record_storicizzati = sum(as.numeric(bdap_record_storicizzato) == 1, na.rm = TRUE),
    n_bdap_storicizzazioni_ambigue = sum(as.numeric(bdap_storicizzazione_ambigua) == 1, na.rm = TRUE)
  )


technical_suffix_cols <- names(lista)[
  stringr::str_detect(names(lista), "\\.x$|\\.y$|_x$|_y$")
]

merge_quality_check <- tibble(
  check = c(
    "Lista finale conserva numero righe MPA",
    "Nessun duplicato chiave in MPA",
    "Nessuna moltiplicazione dopo join S13",
    "Nessuna moltiplicazione dopo join BDAP",
    "Tutti i record finali hanno presente_mpa",
    "Lista finale senza suffissi tecnici automatici"
  ),
  esito = c(
    nrow(lista) == nrow(MPA_raw),
    nrow(duplicate_keys_mpa) == 0,
    n_after_s13_join == n_mpa_before_join,
    n_after_bdap_join == n_mpa_before_join,
    sum(is.na(lista$presente_mpa)) == 0,
    length(technical_suffix_cols) == 0
  )
)


if (any(!merge_quality_check$esito)) {
  warning("Alcuni controlli di qualità del merge non sono superati. Controllare merge_quality_check nel file audit.")
}

#..............................................................................#
#                     METADATI DELLA LISTA FINALE                           ####
#..............................................................................#

# Primo dizionario delle variabili finali.
#
# Questo non sostituisce la documentazione metodologica completa, ma crea già
# una base strutturata: per ogni variabile finale indichiamo fonte, contenuto,
# regola di costruzione e note di qualità.

metadata_variabili <- tibble::tribble(
  ~variabile, ~label, ~descrizione, ~fonte_originaria, ~campo_originario, ~tipo_variabile, ~regola_costruzione, ~priorita_fonti, ~note_qualita,
  
  "codice_fiscale",
  "Codice fiscale ente",
  "Identificativo fiscale dell'amministrazione o del record amministrativo.",
  "MPA",
  "CODICE_FISCALE",
  "identificativo",
  "Mantenuto dalla fonte MPA, che definisce il perimetro della lista.",
  "MPA",
  "Verificare nel log se CODICE_FISCALE identifica univocamente i record MPA.",
  
  "codice_reg",
  "Codice regione",
  "Codice regione associato all'ente o al record amministrativo.",
  "MPA/BDAP/S13",
  "CODICE_REG / Codice_Regione",
  "identificativo territoriale",
  "Valore selezionato con coalesce secondo priorità dichiarata.",
  "MPA > BDAP > S13",
  "Non usato come chiave di merge; mantenuto come informazione territoriale e controllo di coerenza tra fonti.",
  
  "fonte_codice_reg",
  "Fonte codice regione",
  "Fonte da cui proviene il valore finale del codice regione.",
  "Pipeline",
  "Derivata",
  "audit",
  "Derivata dalla prima fonte valorizzata secondo la priorità.",
  "MPA > BDAP > S13",
  "Serve per verificare se il codice regione finale proviene da MPA o da una fonte di fallback.",
  
  "ragione_sociale",
  "Ragione sociale",
  "Denominazione o ragione sociale dell'amministrazione.",
  "MPA/BDAP/S13",
  # "BDAP/MPA/S13",
  "RAGIONE_SOCIALE",
  "descrittiva",
  "Valore selezionato con coalesce secondo priorità dichiarata.",
  "MPA > BDAP > S13",
  # "BDAP > MPA > S13",
  "I conflitti tra fonti sono registrati nel foglio conflict_log.",
  
  "fonte_ragione_sociale",
  "Fonte ragione sociale",
  "Fonte da cui proviene il valore finale della ragione sociale.",
  "Pipeline",
  "Derivata",
  "audit",
  "Derivata dalla prima fonte valorizzata secondo la priorità.",
  "MPA > BDAP > S13",
  # "BDAP > MPA > S13",
  "Serve per tracciabilità leggera nella lista finale.",
  
  "fg",
  "FG",
  "Campo classificatorio presente nelle liste di input.",
  "MPA/S13/BDAP",
  "FG",
  "classificatoria",
  "Valore selezionato con coalesce secondo priorità dichiarata.",
  "MPA > S13 > BDAP",
  "Verificare nella documentazione delle fonti il significato esatto del campo.",
  
  "fonte_fg",
  "Fonte FG",
  "Fonte da cui proviene il valore finale del campo FG.",
  "Pipeline",
  "Derivata",
  "audit",
  "Derivata dalla prima fonte valorizzata secondo la priorità.",
  "MPA > S13 > BDAP",
  "Serve per tracciabilità leggera nella lista finale.",
  
  "codice_unita_mpa",
  "Codice unità MPA",
  "Codice unità specifico della fonte MPA.",
  "MPA",
  "CODICE_UNITA_UG oppure CODICE_UNITA",
  "identificativo fonte-specifico",
  "Rinominato dalla fonte MPA.",
  "MPA",
  "Da usare come identificativo interno MPA, non necessariamente come chiave generale.",
  
  "codice_unita_s13",
  "Codice unità S13",
  "Codice unità specifico della fonte S13.",
  "S13",
  "CODICE_UNITA",
  "identificativo fonte-specifico",
  "Rinominato dalla fonte S13.",
  "S13",
  "Valorizzato solo per record MPA raccordati a S13.",
  
  "ateco_bdap",
  "Codice ATECO BDAP",
  "Codice ATECO associato all'ente nella fonte BDAP.",
  "BDAP",
  "Codice_ATECO",
  "classificatoria",
  "Rinominato dalla fonte BDAP.",
  "BDAP",
  "Valorizzato solo per record MPA raccordati a BDAP.",
  
  "presente_mpa",
  "Presenza in MPA",
  "Indicatore di presenza nella fonte MPA.",
  "MPA",
  "presente_mpa",
  "indicatore",
  "Vale 1 per i record appartenenti al perimetro MPA.",
  "MPA",
  "Dovrebbe essere sempre valorizzato nella lista finale.",
  
  "presente_s13",
  "Presenza in S13",
  "Indicatore di presenza anche nella fonte S13.",
  "S13",
  "presente_s13",
  "indicatore",
  "Vale 1 se il record MPA trova corrispondenza in S13.",
  "S13",
  "Se mancante, il record MPA non è stato raccordato a S13.",
  
  "presente_bdap",
  "Presenza in BDAP",
  "Indicatore di presenza anche nella fonte BDAP.",
  "BDAP",
  "presente_bdap",
  "indicatore",
  "Vale 1 se il record MPA trova corrispondenza in BDAP.",
  "BDAP",
  "Se mancante, il record MPA non è stato raccordato a BDAP.", 
  
  "bdap_record_storicizzato",
  "Record BDAP storicizzato",
  "Indicatore che segnala se il codice fiscale aveva più righe in BDAP.",
  "BDAP/Pipeline",
  "Data_Cessazione",
  "audit qualità",
  "Vale 1 se BDAP conteneva più righe per lo stesso codice fiscale; per il merge è stata selezionata la riga attiva.",
  "Riga attiva BDAP, definita da Data_Cessazione mancante o vuota",
  "Le righe escluse sono conservate nel file audit.",
  
  "bdap_storicizzazione_ambigua",
  "Storicizzazione BDAP ambigua",
  "Indicatore che segnala casi BDAP storicizzati con variazioni sostanziali di denominazione, forma giuridica, ATECO o regione.",
  "BDAP/Pipeline",
  "Derivata",
  "audit qualità",
  "Derivata dal confronto tra righe BDAP con stesso codice fiscale.",
  "Pipeline",
  "Serve a identificare casi da interpretare con cautela.",
  
  "bdap_n_righe_originali",
  "Numero righe BDAP originali",
  "Numero di righe BDAP originariamente associate allo stesso codice fiscale.",
  "BDAP/Pipeline",
  "CODICE_FISCALE",
  "audit qualità",
  "Derivata prima della deduplica BDAP.",
  "Pipeline",
  "Valore maggiore di 1 indica record BDAP storicizzato o duplicato.",
  
  "run_id",
  "Identificativo run",
  "Identificativo temporale dell'esecuzione dello script.",
  "Pipeline",
  "Derivata",
  "audit",
  "Creato con format(Sys.time(), '%Y%m%d_%H%M%S').",
  "Pipeline",
  "Serve per collegare lista, log e metadati."
) %>%
  mutate(run_id = RUN_ID, .before = 1)



#..............................................................................#
#                     EXPORT LOCALE IN 07_TEMP                              ####
#..............................................................................#

# Lista finale pulita.
writexl::write_xlsx(
  lista,
  path = local_lista_file
)

## File audit completo.
writexl::write_xlsx(
  list(
    lista = lista,
    metadata_variabili = metadata_variabili,
    coverage_log = coverage_log,
    join_row_count_log = join_row_count_log,
    merge_quality_check = merge_quality_check,
    duplicate_keys_log = duplicate_keys_log,
    duplicate_pairs_log = duplicate_pairs_log,
    conflict_log = conflict_log,
    source_variables = source_variables,
    bdap_duplicate_keys = bdap_duplicate_keys,
    bdap_duplicate_keys_mpa = bdap_duplicate_keys_mpa,
    bdap_active_check = bdap_active_check,
    bdap_duplicate_case_summary = bdap_duplicate_case_summary,
    bdap_active_problems = bdap_active_problems,
    bdap_dedup_rule_log = bdap_dedup_rule_log,
    bdap_storicizzazioni_ambigue = bdap_storicizzazioni_ambigue,
    bdap_rows_excluded_by_dedup = bdap_rows_excluded_by_dedup,
    s13_fuori_perimetro_mpa = s13_fuori_perimetro_mpa,
    bdap_fuori_perimetro_mpa = bdap_fuori_perimetro_mpa
  ),
  path = local_audit_file
)


# File metadati separato.
writexl::write_xlsx(
  list(
    metadata_variabili = metadata_variabili,
    source_variables = source_variables
  ),
  path = local_metadata_file
)



#..............................................................................#
#                               EXPORT TO DRIVE                             ####
#..............................................................................#
# lista.xlsx è il file operativo: viene caricato/aggiornato in 01_Dataset/Lists.
drive_upload_or_update(
  local_path = local_lista_file,
  drive_folder_rel = DRIVE_DIR_LISTS,
  drive_name = "lista.xlsx"
)

# Il file audit è run-specific: lo salviamo nei log.
drive_upload_or_update(
  local_path = local_audit_file,
  drive_folder_rel = DRIVE_DIR_LOGS_LISTA,
  drive_name = basename(local_audit_file)
)


# I metadati sono run-specific: li salviamo nella cartella metadata.
drive_upload_or_update(
  local_path = local_metadata_file,
  drive_folder_rel = DRIVE_DIR_METADATA_LISTA,
  drive_name = basename(local_metadata_file)
)


#..............................................................................#
#                          PULIZIA FILE TEMPORANEI                          ####
#..............................................................................#

if (delete_local_temp) {
  files_to_delete <- c(
    local_file_mpa,
    local_file_s13,
    local_file_bdap,
    local_lista_file,
    local_audit_file,
    local_metadata_file
  )
  
  file.remove(files_to_delete[file.exists(files_to_delete)])
  message("File temporanei cancellati da: ", DIR_TEMP)
} else {
  message("File temporanei mantenuti in: ", DIR_TEMP)
}

#..............................................................................#
technical_suffix_cols <- names(lista)[
  stringr::str_detect(names(lista), "\\.x$|\\.y$|_x$|_y$")
]

merge_quality_check <- tibble(
  check = c(
    "Lista finale conserva numero righe MPA",
    "Nessun duplicato chiave in MPA",
    "Nessuna moltiplicazione dopo join S13",
    "Nessuna moltiplicazione dopo join BDAP",
    "Tutti i record finali hanno presente_mpa",
    "Lista finale senza suffissi tecnici automatici"
  ),
  esito = c(
    nrow(lista) == nrow(MPA_raw),
    nrow(duplicate_keys_mpa) == 0,
    n_after_s13_join == n_mpa_before_join,
    n_after_bdap_join == n_mpa_before_join,
    sum(is.na(lista$presente_mpa)) == 0,
    length(technical_suffix_cols) == 0
  )
)
# duplicate_keys_log
# 
# duplicate_keys_log %>%
#   count(source, name = "n_chiavi_duplicate")
# 
# nrow(MPA_raw)
# nrow(lista)
# 
# lista %>%
#   summarise(
#     n = n(),
#     presente_mpa_missing = sum(is.na(presente_mpa)),
#     presente_s13_missing = sum(is.na(presente_s13)),
#     presente_bdap_missing = sum(is.na(presente_bdap)),
#     presente_s13_rate = mean(!is.na(presente_s13)),
#     presente_bdap_rate = mean(!is.na(presente_bdap))
#   )
# 
# mpa_non_match_s13 <- lista %>%
#   filter(is.na(presente_s13))
# 
# mpa_non_match_bdap <- lista %>%
#   filter(is.na(presente_bdap))
# 
# nrow(mpa_non_match_s13)
# nrow(mpa_non_match_bdap)
# mpa_non_match_s13 %>%
#   select(codice_fiscale, codice_reg, ragione_sociale, fg) %>%
#   head(30)
# 
# mpa_non_match_bdap %>%
#   select(codice_fiscale, codice_reg, ragione_sociale, fg) %>%
#   head(30)
# 
# tibble(
#   fonte = c("MPA", "S13", "BDAP"),
#   classe_codice_fiscale = c(
#     class(MPA_raw$CODICE_FISCALE)[1],
#     class(s13_raw$CODICE_FISCALE)[1],
#     class(BDAP_raw$CODICE_FISCALE)[1]
#   ),
#   classe_codice_reg = c(
#     class(MPA_raw$CODICE_REG)[1],
#     class(s13_raw$CODICE_REG)[1],
#     class(BDAP_raw$CODICE_REG)[1]
#   )
# )
# 
# MPA_raw %>%
#   mutate(
#     CODICE_FISCALE = as.character(CODICE_FISCALE),
#     CODICE_REG = as.character(CODICE_REG)
#   ) %>%
#   summarise(
#     min_len_cf = min(nchar(CODICE_FISCALE), na.rm = TRUE),
#     max_len_cf = max(nchar(CODICE_FISCALE), na.rm = TRUE),
#     n_cf_missing = sum(is.na(CODICE_FISCALE)),
#     n_reg_missing = sum(is.na(CODICE_REG))
#   )
# 
# s13_raw %>%
#   mutate(
#     CODICE_FISCALE = as.character(CODICE_FISCALE),
#     CODICE_REG = as.character(CODICE_REG)
#   ) %>%
#   summarise(
#     min_len_cf = min(nchar(CODICE_FISCALE), na.rm = TRUE),
#     max_len_cf = max(nchar(CODICE_FISCALE), na.rm = TRUE),
#     n_cf_missing = sum(is.na(CODICE_FISCALE)),
#     n_reg_missing = sum(is.na(CODICE_REG))
#   )
# 
# BDAP_raw %>%
#   mutate(
#     CODICE_FISCALE = as.character(CODICE_FISCALE),
#     CODICE_REG = as.character(CODICE_REG)
#   ) %>%
#   summarise(
#     min_len_cf = min(nchar(CODICE_FISCALE), na.rm = TRUE),
#     max_len_cf = max(nchar(CODICE_FISCALE), na.rm = TRUE),
#     n_cf_missing = sum(is.na(CODICE_FISCALE)),
#     n_reg_missing = sum(is.na(CODICE_REG))
#   )
# 
# 
# conflict_log %>%
#   count(variable, source_a, source_b, sort = TRUE)
# 
# 
# conflict_log %>%
#   filter(variable %in% c("RAGIONE_SOCIALE", "FG")) %>%
#   arrange(variable, CODICE_FISCALE, CODICE_REG) %>%
#   head(50)
# 
# duplicate_pairs_log %>%
#   arrange(variable_base, source_a, source_b)
# 
# duplicate_pairs_log %>%
#   count(variable_base, sort = TRUE)
# 
# names(lista)[stringr::str_detect(names(lista), "_mpa$|_s13$|_bdap$|\\.x$|\\.y$|_x$|_y$")]
# 
# lista %>%
#   count(fonte_ragione_sociale, sort = TRUE)
# 
# lista %>%
#   count(fonte_fg, sort = TRUE)
# 
# nrow(s13_fuori_perimetro_mpa)
# nrow(bdap_fuori_perimetro_mpa)
# 
# s13_fuori_perimetro_mpa %>%
#   select(CODICE_FISCALE, CODICE_REG, contains("RAGIONE"), contains("FG")) %>%
#   head(30)
# 
# bdap_fuori_perimetro_mpa %>%
#   select(CODICE_FISCALE, CODICE_REG, contains("RAGIONE"), contains("DENOM"), contains("FG")) %>%
#   head(30)
# 
# merge_quality_check <- tibble(
#   check = c(
#     "Lista finale conserva numero righe MPA",
#     "Nessun duplicato chiave in MPA",
#     "Nessuna moltiplicazione dopo join S13",
#     "Nessuna moltiplicazione dopo join BDAP",
#     "Tutti i record finali hanno presente_mpa",
#     "Lista finale senza suffissi tecnici"
#   ),
#   esito = c(
#     nrow(lista) == nrow(MPA_raw),
#     nrow(duplicate_keys_mpa) == 0,
#     n_after_s13_join == n_mpa_before_join,
#     n_after_bdap_join == n_mpa_before_join,
#     sum(is.na(lista$presente_mpa)) == 0,
#     length(names(lista)[stringr::str_detect(names(lista), "_mpa$|_s13$|_bdap$|\\.x$|\\.y$|_x$|_y$")]) == 0
#   )
# )
# 
# merge_quality_check
