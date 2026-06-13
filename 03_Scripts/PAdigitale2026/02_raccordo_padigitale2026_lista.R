# ============================================================ #
# Script: 05_raccordo_padigitale2026_lista.R
# Fonte: PA digitale 2026 - Open data
#
# Obiettivo:
#   1. leggere i dati PA digitale 2026 processed
#   2. importare la master list da Drive
#   3. raccordare candidature finanziate alla master list
#   4. produrre log di qualità del raccordo
#   5. produrre indicatori per dashboard
#
# NOTE:
# - Il valore aggiunto è leggere PA digitale 2026 per tipologia PA,
#   macro-gruppo PA, perimetro S13/MPA e territorio della master list.
# ============================================================ #

rm(list = ls())


# 1) PACCHETTI ---------------------------------------------------------------

{library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)
library(tidyr)
library(janitor)
library(googledrive)
library(openxlsx)
library(lubridate)}


# 2) PARAMETRI ---------------------------------------------------------------

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")

googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")

# parametro per pulire la cartella temp alla fine del run
delete_local_temp <- FALSE

RUN_ID_IMPORT <- "20260610_120200"  # da copiare dall'output dello script 01
RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")

message("RUN_ID_IMPORT: ", RUN_ID_IMPORT)
message("RUN_ID raccordo: ", RUN_ID)

DIR_PAD26_PROCESSED_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID_IMPORT)
DIR_PAD26_OUTPUT_LOCAL    <- file.path(DIR_TEMP, "PADigitale2026", "Output", RUN_ID)
DIR_PAD26_LOGS_LOCAL      <- file.path(DIR_TEMP, "PADigitale2026", "Logs", RUN_ID)
DIR_PAD26_INDICATORI_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Indicatori", RUN_ID)
DIR_PAD26_RACCORDATO_LOCAL <- file.path( DIR_TEMP, "PADigitale2026", "Processed_raccordato", RUN_ID)


DRIVE_PAD26_PROCESSED <- file.path(DRIVE_DIR_PROCESSED, "PADigitale2026", RUN_ID_IMPORT)
DRIVE_PAD26_OUTPUT    <- file.path(DRIVE_DIR_OUTPUT, "PADigitale2026", RUN_ID)
DRIVE_PAD26_LOGS      <- file.path(DRIVE_DIR_LOGS, "PADigitale2026", RUN_ID)
DRIVE_PAD26_INDICATORI <- file.path(DRIVE_DIR_INDICATORI,  "PADigitale2026",  RUN_ID)
DRIVE_PAD26_RACCORDATO <- file.path(DRIVE_DIR_PROCESSED,  "PADigitale2026_raccordato",  RUN_ID)

dir.create(DIR_PAD26_PROCESSED_LOCAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PAD26_OUTPUT_LOCAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PAD26_LOGS_LOCAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PAD26_INDICATORI_LOCAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PAD26_RACCORDATO_LOCAL, recursive = TRUE, showWarnings = FALSE)

file_candidature <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "candidature_finanziate_padigitale2026.rds"
)

file_avvisi <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "avvisi_padigitale2026.csv"
)

drive_download_from_path(
  drive_file_rel = file.path(DRIVE_PAD26_PROCESSED, "candidature_finanziate_padigitale2026.rds"),
  local_path = file_candidature
)

drive_download_from_path(
  drive_file_rel = file.path(DRIVE_PAD26_PROCESSED, "avvisi_padigitale2026.csv"),
  local_path = file_avvisi
)


if (!file.exists(file_candidature)) {
  stop("File candidature PA digitale 2026 non trovato: ", file_candidature)
}

if (!file.exists(file_avvisi)) {
  stop("File avvisi PA digitale 2026 non trovato: ", file_avvisi)
}


# 3) FUNZIONI ---------------------------------------------------------------

normalizza_testo <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("’", "'") %>%
    stringr::str_replace_all("`", "'") %>%
    stringr::str_squish() %>%
    dplyr::na_if("")
}

normalizza_codice_fiscale <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("[^A-Z0-9]", "") %>%
    dplyr::na_if("")
}

normalizza_codice_regione <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_squish() %>%
    dplyr::na_if("") %>%
    stringr::str_pad(width = 2, pad = "0")
}

# 4) IMPORT DATI PA DIGITALE 2026 -------------------------------------------

candidature_pad26 <- readRDS(file_candidature) %>%
  janitor::clean_names()

avvisi <- readr::read_csv(file_avvisi, show_col_types = FALSE) %>%
  janitor::clean_names()

# Check nomi colonne
message("Colonne candidature:")
print(names(candidature_pad26))

message("Colonne avvisi:")
print(names(avvisi))

# 5) STANDARDIZZAZIONE PA DIGITALE 2026 -------------------------------------

pad26_std <- candidature_pad26 %>%
  mutate(
    ente_key = normalizza_testo(ente),
    codice_ipa_key = normalizza_testo(codice_ipa),
    
    # Nei file PA digitale il codice fiscale potrebbe non esserci sempre.
    # Manteniamo una colonna standard se esiste.
    # codice_fiscale_ente = if ("codice_fiscale" %in% names(.)) {
    #   normalizza_codice_fiscale(codice_fiscale)
    # } else {
    #   NA_character_
    # },
    
    cod_regione = normalizza_codice_regione(cod_regione),
    cod_provincia = stringr::str_pad(as.character(cod_provincia), 3, pad = "0"),
    cod_comune = as.character(cod_comune),
    
    regione_key = normalizza_testo(regione),
    provincia_key = normalizza_testo(provincia),
    comune_key = normalizza_testo(comune),
    tipologia_ente_key = normalizza_testo(tipologia_ente),
    stato_candidatura_key = normalizza_testo(stato_candidatura),
    
    avviso = as.character(avviso),
    importo_finanziamento = readr::parse_number(as.character(importo_finanziamento))
  )

# str(pad26_std)
# summary(as.factor(pad26_std$avviso))

# 6) IMPORT MASTER LIST DA DRIVE --------------------------------------------

file_lista_local <- file.path(DIR_TEMP, "lista.xlsx")

drive_download_from_path(
  drive_file_rel = file.path(DRIVE_DIR_LISTS, "lista.xlsx"),
  local_path = file_lista_local
)

lista <- readxl::read_excel(
  file_lista_local,
  col_types = "text"
) %>%
  janitor::clean_names()

# lista <- readxl::read_excel(file_lista_local) %>%
#   janitor::clean_names()


# 7) PREPARAZIONE MASTER LIST ------------------------------------------------

names(lista)
str(lista)

lista_base <- lista %>%
  mutate(
    lista_row_id = row_number(),
    lista_ind = 1,
    
    codice_fiscale_key = normalizza_codice_fiscale(codice_fiscale),
    codice_ipa_key = normalizza_testo(codice_ente_ipa),
    codice_siope_key = normalizza_testo(codice_ente_siope),
    ragione_sociale_key = normalizza_testo(ragione_sociale),
    
    codice_regione = normalizza_codice_regione(codice_reg),
    codice_provincia = stringr::str_pad(as.character(codice_provincia), 3, pad = "0"),
    codice_comune = as.character(codice_comune),
    
    presente_mpa = as.integer(presente_mpa == "1"),
    presente_s13 = as.integer(presente_s13 == "1"),
    presente_bdap = as.integer(presente_bdap == "1"),
    bdap_record_storicizzato = as.integer(bdap_record_storicizzato == "1"),
    bdap_storicizzazione_ambigua = as.integer(bdap_storicizzazione_ambigua == "1"),
    bdap_n_righe_originali = suppressWarnings(as.integer(bdap_n_righe_originali)),
    
    fg = as.character(fg),
    desc_fg = as.character(desc_fg),
    ateco_bdap = as.character(ateco_bdap),
    descr_ateco_bdap = as.character(descr_ateco_bdap)
  )

# Controlli preliminari di copertura

check_pad26_ipa <- pad26_std %>%
  summarise(
    n_righe_pad26 = n(),
    n_enti_pad26 = n_distinct(ente_key, na.rm = TRUE),
    n_codici_ipa_distinti = n_distinct(codice_ipa_key, na.rm = TRUE),
    n_codici_ipa_missing = sum(is.na(codice_ipa_key) | codice_ipa_key == ""),
    quota_codici_ipa_missing = n_codici_ipa_missing / n_righe_pad26
  )

check_lista_ipa <- lista_base %>%
  summarise(
    n_enti_lista = n(),
    n_codici_ipa_distinti = n_distinct(codice_ipa_key, na.rm = TRUE),
    n_codici_ipa_missing = sum(is.na(codice_ipa_key) | codice_ipa_key == ""),
    quota_codici_ipa_missing = n_codici_ipa_missing / n_enti_lista
  )

check_pad26_in_lista <- pad26_std %>%
  distinct(codice_ipa_key, ente_key) %>%
  mutate(has_codice_ipa = !is.na(codice_ipa_key) & codice_ipa_key != "") %>%
  left_join(
    lista_base %>%
      distinct(codice_ipa_key) %>%
      mutate(in_lista = TRUE),
    by = "codice_ipa_key"
  ) %>%
  summarise(
    n_enti_pad26 = n(),
    n_enti_pad26_con_ipa = sum(has_codice_ipa),
    n_enti_pad26_in_lista = sum(in_lista %in% TRUE, na.rm = TRUE),
    quota_enti_pad26_in_lista = n_enti_pad26_in_lista / n_enti_pad26_con_ipa
  )

check_lista_in_pad26 <- lista_base %>%
  distinct(codice_ipa_key, ragione_sociale) %>%
  mutate(has_codice_ipa = !is.na(codice_ipa_key) & codice_ipa_key != "") %>%
  left_join(
    pad26_std %>%
      distinct(codice_ipa_key) %>%
      mutate(in_pad26 = TRUE),
    by = "codice_ipa_key"
  ) %>%
  summarise(
    n_enti_lista = n(),
    n_enti_lista_con_ipa = sum(has_codice_ipa),
    n_enti_lista_in_pad26 = sum(in_pad26 %in% TRUE, na.rm = TRUE),
    quota_enti_lista_in_pad26 = n_enti_lista_in_pad26 / n_enti_lista_con_ipa
  )

# write_csv(check_pad26_ipa, file.path(DIR_PAD26_LOGS_LOCAL, "check_pad26_codici_ipa.csv"))
# write_csv(check_lista_ipa, file.path(DIR_PAD26_LOGS_LOCAL, "check_lista_codici_ipa.csv"))
# write_csv(check_pad26_in_lista, file.path(DIR_PAD26_LOGS_LOCAL, "check_pad26_in_lista.csv"))
# write_csv(check_lista_in_pad26, file.path(DIR_PAD26_LOGS_LOCAL, "check_lista_in_pad26.csv"))
# 
# drive_upload_or_update(file.path(DIR_PAD26_LOGS_LOCAL, "check_pad26_codici_ipa.csv"), DRIVE_PAD26_LOGS)
# drive_upload_or_update(file.path(DIR_PAD26_LOGS_LOCAL, "check_lista_codici_ipa.csv"), DRIVE_PAD26_LOGS)
# drive_upload_or_update(file.path(DIR_PAD26_LOGS_LOCAL, "check_pad26_in_lista.csv"), DRIVE_PAD26_LOGS)
# drive_upload_or_update(file.path(DIR_PAD26_LOGS_LOCAL, "check_lista_in_pad26.csv"), DRIVE_PAD26_LOGS)

# Crea un log dei duplicati IPA
ipa_ambigui_lista <- lista_base %>%
  filter(!is.na(codice_ipa_key), codice_ipa_key != "") %>%
  distinct(codice_ipa_key, lista_row_id) %>%
  count(codice_ipa_key, name = "n_enti_lista") %>%
  filter(n_enti_lista > 1)

local_log_ipa_ambigui <- file.path(
  DIR_PAD26_LOGS_LOCAL,
  "pad26_codici_ipa_ambigui_lista.csv"
)

# write_csv(ipa_ambigui_lista, local_log_ipa_ambigui)
# drive_upload_or_update(local_log_ipa_ambigui, DRIVE_PAD26_LOGS)

lista_match_ipa <- lista_base %>%
  filter(!is.na(codice_ipa_key), codice_ipa_key != "") %>%
  anti_join(ipa_ambigui_lista, by = "codice_ipa_key")


# Match per codice IPA, se disponibile
lista_match_ipa <- lista_base %>%
  filter(!is.na(codice_ipa_key), codice_ipa_key != "") %>%
  distinct(codice_ipa_key, .keep_all = TRUE)

# Match per ragione sociale
lista_keys_den <- lista_base %>%
  transmute(
    lista_row_id,
    chiave_lista_tipo = "ragione_sociale",
    ente_key = ragione_sociale_key
  ) %>%
  filter(!is.na(ente_key), ente_key != "") %>%
  distinct()

chiavi_ambigue_lista_den <- lista_keys_den %>%
  distinct(ente_key, lista_row_id) %>%
  count(ente_key, name = "n_enti_lista") %>%
  filter(n_enti_lista > 1)

local_log_chiavi_ambigue <- file.path(
  DIR_PAD26_LOGS_LOCAL,
  "pad26_chiavi_ambigue_lista_ragione_sociale.csv"
)

# write_csv(chiavi_ambigue_lista_den, local_log_chiavi_ambigue)
# drive_upload_or_update(local_log_chiavi_ambigue, DRIVE_PAD26_LOGS)

lista_match_den <- lista_keys_den %>%
  anti_join(chiavi_ambigue_lista_den, by = "ente_key") %>%
  distinct(ente_key, .keep_all = TRUE) %>%
  left_join(lista_base, by = "lista_row_id")

# write_csv(
#   chiavi_ambigue_lista_den,
#   file.path(DIR_LOGS, "pad26_chiavi_ambigue_lista_denominazione.csv")
# )

# local_log_chiavi_ambigue <- file.path(
#   DIR_PAD26_LOGS_LOCAL,
#   "pad26_chiavi_ambigue_lista_denominazione.csv"
# )

# write_csv(chiavi_ambigue_lista_den, local_log_chiavi_ambigue)

# drive_upload_or_update(
#   local_path = local_log_chiavi_ambigue,
#   drive_folder_rel = DRIVE_PAD26_LOGS
# )

lista_match_den <- lista_keys_den %>%
  anti_join(chiavi_ambigue_lista_den, by = "ente_key") %>%
  left_join(lista_base, by = "lista_row_id")

# Variabili lista da portare nel raccordo
colonne_lista_utili <- c(
  "lista_row_id",
  "lista_ind",
  "chiave_lista_tipo",
  
  "codice_fiscale",
  "codice_fiscale_key",
  "codice_ente_ipa",
  "codice_ipa_key",
  "codice_ente_siope",
  "codice_siope_key",
  
  "ragione_sociale",
  "ragione_sociale_key",
  "fonte_ragione_sociale",
  
  "fg",
  "desc_fg",
  "fonte_fg",
  
  "codice_reg",
  "codice_regione",
  "fonte_codice_reg",
  
  "codice_unita_mpa",
  "codice_unita_s13",
  
  "id_ente_bdap",
  "ateco_bdap",
  "descr_ateco_bdap",
  
  "codice_forma_giuridica_bdap",
  "descr_forma_giuridica_bdap",
  "codice_tipologia_siope_bdap",
  "descr_tipologia_siope_bdap",
  "codice_categoria_ipa_bdap",
  "descr_categoria_ipa_bdap",
  "codice_tipologia_ipa_bdap",
  "descr_tipologia_ipa_bdap",
  
  "presente_mpa",
  "presente_s13",
  "presente_bdap",
  
  "bdap_record_storicizzato",
  "bdap_storicizzazione_ambigua",
  "bdap_n_righe_originali",
  "run_id"
)

# 8) RACCORDO: PRIMA IPA, POI DENOMINAZIONE ---------------------------------

# 8.1 Match forte per codice IPA
pad26_match_ipa <- pad26_std %>%
  left_join(
    lista_match_ipa %>%
      select(any_of(c("codice_ipa_key", colonne_lista_utili))) %>%
      mutate(chiave_lista_tipo = "codice_ipa"),
    by = "codice_ipa_key"
  ) %>%
  mutate(
    match_lista_ipa = if_else(!is.na(lista_ind), 1, 0)
  )

# 8.2 Per chi non matcha via IPA, provo denominazione
pad26_non_match_ipa <- pad26_match_ipa %>%
  filter(match_lista_ipa == 0) %>%
  select(names(pad26_std))

pad26_match_den <- pad26_non_match_ipa %>%
  left_join(
    lista_match_den %>%
      select(any_of(c("ente_key", colonne_lista_utili))),
    by = "ente_key"
  ) %>%
  mutate(
    match_lista_den = if_else(!is.na(lista_ind), 1, 0)
  )

# 8.3 Ricompongo
pad26_match_ipa_ok <- pad26_match_ipa %>%
  filter(match_lista_ipa == 1) %>%
  mutate(
    match_lista_den = 0
  )

pad26_raccordato <- bind_rows(
  pad26_match_ipa_ok,
  pad26_match_den
) %>%
  mutate(
    pad26_ind = 1,
    match_lista = if_else(!is.na(lista_ind), 1, 0),
    tipo_match = case_when(
      match_lista_ipa == 1 ~ "match_codice_ipa",
      match_lista_den == 1 ~ paste0("match_", chiave_lista_tipo),
      TRUE ~ "no_match"
    ),
    codice_regione_lista = codice_regione,
    forma_giuridica = fg,
    descr_forma_giuridica = desc_fg
  )

# 8.4 Check copertura e partecipazione

lista_partecipazione_pad26 <- lista_base %>%
  mutate(
    has_codice_ipa_lista = !is.na(codice_ipa_key) & codice_ipa_key != ""
  ) %>%
  left_join(
    pad26_std %>%
      filter(!is.na(codice_ipa_key), codice_ipa_key != "") %>%
      group_by(codice_ipa_key) %>%
      summarise(
        in_pad26 = 1,
        n_candidature_pad26 = n(),
        n_misure_pad26 = n_distinct(avviso, na.rm = TRUE),
        importo_finanziato_pad26 = sum(importo_finanziamento, na.rm = TRUE),
        enti_pad26 = paste(sort(unique(ente)), collapse = " | "),
        .groups = "drop"
      ),
    by = "codice_ipa_key"
  ) %>%
  mutate(
    in_pad26 = if_else(is.na(in_pad26), 0L, as.integer(in_pad26)),
    n_candidature_pad26 = replace_na(n_candidature_pad26, 0L),
    n_misure_pad26 = replace_na(n_misure_pad26, 0L),
    importo_finanziato_pad26 = replace_na(importo_finanziato_pad26, 0)
  )

log_partecipazione_lista_pad26 <- lista_partecipazione_pad26 %>%
  summarise(
    n_enti_lista = n(),
    n_enti_lista_con_codice_ipa = sum(has_codice_ipa_lista, na.rm = TRUE),
    n_enti_lista_senza_codice_ipa = sum(!has_codice_ipa_lista, na.rm = TRUE),
    n_enti_lista_in_pad26 = sum(in_pad26 == 1, na.rm = TRUE),
    n_enti_lista_non_in_pad26 = sum(in_pad26 == 0, na.rm = TRUE),
    quota_enti_lista_in_pad26_su_totale = n_enti_lista_in_pad26 / n_enti_lista,
    quota_enti_lista_in_pad26_su_enti_con_ipa = n_enti_lista_in_pad26 / n_enti_lista_con_codice_ipa
  )

log_lista_non_in_pad26 <- lista_partecipazione_pad26 %>%
  filter(in_pad26 == 0) %>%
  select(
    codice_fiscale,
    codice_ente_ipa,
    codice_ipa_key,
    ragione_sociale,
    fg,
    desc_fg,
    codice_reg,
    codice_regione,
    presente_mpa,
    presente_s13,
    presente_bdap,
    has_codice_ipa_lista
  ) %>%
  arrange(desc(has_codice_ipa_lista), ragione_sociale)

log_pad26_non_match_lista <- pad26_raccordato %>%
  filter(match_lista == 0) %>%
  group_by(
    codice_ipa,
    codice_ipa_key,
    ente_key,
    tipologia_ente,
    comune,
    provincia,
    regione,
    cod_comune,
    cod_provincia,
    cod_regione
  ) %>%
  summarise(
    n_candidature = n(),
    n_misure = n_distinct(avviso, na.rm = TRUE),
    importo_finanziato = sum(importo_finanziamento, na.rm = TRUE),
    esempi_ente = paste(sort(unique(na.omit(ente)))[1:min(5, length(unique(na.omit(ente))))], collapse = " | "),
    avvisi = paste(sort(unique(na.omit(avviso)))[1:min(10, length(unique(na.omit(avviso))))], collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(desc(importo_finanziato), desc(n_candidature))

log_copertura_pad26_lista <- pad26_raccordato %>%
  summarise(
    n_candidature_pad26 = n(),
    n_candidature_match_lista = sum(match_lista == 1, na.rm = TRUE),
    n_candidature_non_match_lista = sum(match_lista == 0, na.rm = TRUE),
    quota_candidature_match_lista = n_candidature_match_lista / n_candidature_pad26,
    
    n_enti_pad26 = n_distinct(ente_key, na.rm = TRUE),
    n_enti_pad26_match_lista = n_distinct(ente_key[match_lista == 1], na.rm = TRUE),
    n_enti_pad26_non_match_lista = n_distinct(ente_key[match_lista == 0], na.rm = TRUE),
    quota_enti_pad26_match_lista = n_enti_pad26_match_lista / n_enti_pad26,
    
    importo_totale_pad26 = sum(importo_finanziamento, na.rm = TRUE),
    importo_match_lista = sum(importo_finanziamento[match_lista == 1], na.rm = TRUE),
    importo_non_match_lista = sum(importo_finanziamento[match_lista == 0], na.rm = TRUE),
    quota_importo_match_lista = importo_match_lista / importo_totale_pad26
  )

local_log_partecipazione_lista <- file.path(
  DIR_PAD26_LOGS_LOCAL,
  "log_partecipazione_lista_padigitale2026.csv"
)

local_log_lista_non_in_pad26 <- file.path(
  DIR_PAD26_LOGS_LOCAL,
  "log_lista_non_in_padigitale2026.csv"
)

local_log_copertura_pad26 <- file.path(
  DIR_PAD26_LOGS_LOCAL,
  "log_copertura_padigitale2026_lista.csv"
)

local_log_pad26_non_match <- file.path(
  DIR_PAD26_LOGS_LOCAL,
  "log_padigitale2026_non_match_lista_dettaglio.csv"
)

# write_csv(log_partecipazione_lista_pad26, local_log_partecipazione_lista)
# write_csv(log_lista_non_in_pad26, local_log_lista_non_in_pad26)
# write_csv(log_copertura_pad26_lista, local_log_copertura_pad26)
# write_csv(log_pad26_non_match_lista, local_log_pad26_non_match)
# 
# drive_upload_or_update(local_log_partecipazione_lista, DRIVE_PAD26_LOGS)
# drive_upload_or_update(local_log_lista_non_in_pad26, DRIVE_PAD26_LOGS)
# drive_upload_or_update(local_log_copertura_pad26, DRIVE_PAD26_LOGS)
# drive_upload_or_update(local_log_pad26_non_match, DRIVE_PAD26_LOGS)

log_lista_senza_codice_ipa <- lista_base %>%
  filter(is.na(codice_ipa_key) | codice_ipa_key == "") %>%
  select(
    codice_fiscale,
    ragione_sociale,
    fg,
    desc_fg,
    codice_reg,
    presente_mpa,
    presente_s13,
    presente_bdap,
    fonte_ragione_sociale,
    fonte_fg
  ) %>%
  arrange(ragione_sociale)

local_log_lista_senza_ipa <- file.path(
  DIR_PAD26_LOGS_LOCAL,
  "log_lista_senza_codice_ipa.csv"
)

# write_csv(log_lista_senza_codice_ipa, local_log_lista_senza_ipa)
# drive_upload_or_update(local_log_lista_senza_ipa, DRIVE_PAD26_LOGS)

# 9) LOG MATCH ---------------------------------------------------------------

log_match_pad26 <- pad26_raccordato %>%
  group_by(tipo_file_candidatura, dataset_id) %>%
  summarise(
    n_candidature = n(),
    n_candidature_match = sum(match_lista == 1, na.rm = TRUE),
    quota_match = n_candidature_match / n_candidature,
    n_enti_pad26 = n_distinct(ente_key, na.rm = TRUE),
    n_enti_match = n_distinct(ente_key[match_lista == 1], na.rm = TRUE),
    importo_totale = sum(importo_finanziamento, na.rm = TRUE),
    importo_match = sum(importo_finanziamento[match_lista == 1], na.rm = TRUE),
    tipi_match = paste(sort(unique(tipo_match)), collapse = " | "),
    .groups = "drop"
  ) %>%
  mutate(
    quota_match_pct = round(100 * quota_match, 1),
    quota_importo_match_pct = round(100 * importo_match / importo_totale, 1)
  )

# write_csv(
#   log_match_pad26,
#   file.path(DIR_LOGS, "log_match_padigitale2026_lista.csv")
# )

# local_log_match <- file.path(
#   DIR_PAD26_LOGS_LOCAL,
#   "log_match_padigitale2026_lista.csv"
# )
# 
# write_csv(log_match_pad26, local_log_match)
# 
# drive_upload_or_update(
#   local_path = local_log_match,
#   drive_folder_rel = DRIVE_PAD26_LOGS
# )


log_pad26_non_match_lista <- pad26_raccordato %>%
  filter(match_lista == 0) %>%
  group_by(
    tipo_file_candidatura,
    dataset_id,
    codice_ipa,
    codice_ipa_key,
    ente_key,
    tipologia_ente,
    comune,
    provincia,
    regione
  ) %>%
  summarise(
    n_candidature = n(),
    n_misure = n_distinct(avviso, na.rm = TRUE),
    importo_totale = sum(importo_finanziamento, na.rm = TRUE),
    esempi_ente = paste(sort(unique(na.omit(ente)))[1:min(5, length(unique(na.omit(ente))))], collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(desc(importo_totale), desc(n_candidature))


# write_csv(
#   pad26_non_match,
#   file.path(DIR_LOGS, "padigitale2026_non_match_lista.csv")
# )
# local_log_non_match <- file.path(
#   DIR_PAD26_LOGS_LOCAL,
#   "padigitale2026_non_match_lista.csv"
# )
# 
# write_csv(pad26_non_match, local_log_non_match)
# 
# drive_upload_or_update(
#   local_path = local_log_non_match,
#   drive_folder_rel = DRIVE_PAD26_LOGS
# )

# 9.X SALVATAGGIO LOG IN UNICO FILE EXCEL ------------------------------------

log_list <- list(
  "01_check_pad26_ipa" = check_pad26_ipa,
  "02_check_lista_ipa" = check_lista_ipa,
  "03_check_pad26_in_lista" = check_pad26_in_lista,
  "04_check_lista_in_pad26" = check_lista_in_pad26,
  "05_partecipazione_lista" = log_partecipazione_lista_pad26,
  "06_copertura_pad26_lista" = log_copertura_pad26_lista,
  "07_lista_senza_codice_ipa" = log_lista_senza_codice_ipa,
  "08_lista_non_in_pad26" = log_lista_non_in_pad26,
  "09_pad26_non_match_lista" = log_pad26_non_match_lista,
  "10_ipa_ambigui_lista" = ipa_ambigui_lista,
  "11_chiavi_testuali_ambigue" = chiavi_ambigue_lista_den,
  "12_sintesi_match_dataset" = log_match_pad26,
  "13_dettaglio_partecipazione" = lista_partecipazione_pad26
)

local_log_excel <- file.path(
  DIR_PAD26_LOGS_LOCAL,
  "log_raccordo_padigitale2026_lista.xlsx"
)

openxlsx::write.xlsx(
  x = log_list,
  file = local_log_excel,
  overwrite = TRUE
)

drive_upload_or_update(
  local_path = local_log_excel,
  drive_folder_rel = DRIVE_PAD26_LOGS
)

# 10) INDICATORI -------------------------------------------------------------

## 10.1 Indicatori per ente ####
indicatori_pad26_ente <- pad26_raccordato %>%
  group_by(
    ente_key,
    codice_ipa_key,
    ente,
    tipologia_ente,
    comune,
    provincia,
    regione,
    cod_comune,
    cod_provincia,
    cod_regione,
    match_lista,
    tipo_match,
    
    codice_fiscale,
    codice_ente_ipa,
    ragione_sociale,
    forma_giuridica,
    descr_forma_giuridica,
    codice_unita_s13,
    codice_unita_mpa,
    presente_mpa,
    presente_s13,
    presente_bdap,
    ateco_bdap,
    descr_ateco_bdap
  ) %>%
  summarise(
    n_candidature = n(),
    n_misure = n_distinct(avviso, na.rm = TRUE),
    importo_finanziato = sum(importo_finanziamento, na.rm = TRUE),
    n_cup = n_distinct(codice_cup, na.rm = TRUE),
    n_candidature_completate = sum(str_detect(stato_candidatura_key, "COMPLET"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    importo_medio_candidatura = if_else(
      n_candidature > 0,
      importo_finanziato / n_candidature,
      NA_real_
    )
  )

local_indicatori_ente_csv <- file.path(
  DIR_PAD26_INDICATORI_LOCAL,
  "indicatori_padigitale2026_ente.csv"
)

local_indicatori_ente_rds <- file.path(
  DIR_PAD26_INDICATORI_LOCAL,
  "indicatori_padigitale2026_ente.rds"
)

write_csv(indicatori_pad26_ente, local_indicatori_ente_csv)
saveRDS(indicatori_pad26_ente, local_indicatori_ente_rds)

drive_upload_or_update(local_indicatori_ente_csv, DRIVE_PAD26_INDICATORI)
drive_upload_or_update(local_indicatori_ente_rds, DRIVE_PAD26_INDICATORI)


## 10.2 Indicatori per misura / avviso ####
indicatori_pad26_misura <- pad26_raccordato %>%
  group_by(
    avviso,
    tipo_file_candidatura,
    forma_giuridica,
    descr_forma_giuridica,
    regione,
    cod_regione,
    match_lista
  ) %>%
  summarise(
    n_candidature = n(),
    n_enti = n_distinct(ente_key, na.rm = TRUE),
    importo_finanziato = sum(importo_finanziamento, na.rm = TRUE),
    n_cup = n_distinct(codice_cup, na.rm = TRUE),
    .groups = "drop"
  )

local_indicatori_misura_csv <- file.path(
  DIR_PAD26_INDICATORI_LOCAL,
  "indicatori_padigitale2026_misura.csv"
)

local_indicatori_misura_rds <- file.path(
  DIR_PAD26_INDICATORI_LOCAL,
  "indicatori_padigitale2026_misura.rds"
)

write_csv(indicatori_pad26_misura, local_indicatori_misura_csv)
saveRDS(indicatori_pad26_misura, local_indicatori_misura_rds)

drive_upload_or_update(local_indicatori_misura_csv, DRIVE_PAD26_INDICATORI)
drive_upload_or_update(local_indicatori_misura_rds, DRIVE_PAD26_INDICATORI)


## 10.3 Indicatori per macro-gruppo PA - Forma giuridica ####
indicatori_pad26_forma_giuridica <- pad26_raccordato %>%
  group_by(
    forma_giuridica,
    descr_forma_giuridica,
    presente_mpa,
    presente_s13,
    presente_bdap,
    match_lista
  ) %>%
  summarise(
    n_candidature = n(),
    n_enti = n_distinct(ente_key, na.rm = TRUE),
    n_misure = n_distinct(avviso, na.rm = TRUE),
    importo_finanziato = sum(importo_finanziamento, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(importo_finanziato))

local_indicatori_fg_csv <- file.path(
  DIR_PAD26_INDICATORI_LOCAL,
  "indicatori_padigitale2026_forma_giuridica.csv"
)

write_csv(indicatori_pad26_forma_giuridica, local_indicatori_fg_csv)
drive_upload_or_update(local_indicatori_fg_csv, DRIVE_PAD26_INDICATORI)


## 10.4 Indicatori territoriali ####
indicatori_pad26_regione <- pad26_raccordato %>%
  group_by(
    cod_regione,
    regione,
    forma_giuridica,
    descr_forma_giuridica,
    match_lista
  ) %>%
  summarise(
    n_candidature = n(),
    n_enti = n_distinct(ente_key, na.rm = TRUE),
    importo_finanziato = sum(importo_finanziamento, na.rm = TRUE),
    .groups = "drop"
  )

local_indicatori_regione_csv <- file.path(
  DIR_PAD26_INDICATORI_LOCAL,
  "indicatori_padigitale2026_regione.csv"
)

local_indicatori_regione_rds <- file.path(
  DIR_PAD26_INDICATORI_LOCAL,
  "indicatori_padigitale2026_regione.rds"
)

write_csv(indicatori_pad26_regione, local_indicatori_regione_csv)
saveRDS(indicatori_pad26_regione, local_indicatori_regione_rds)

drive_upload_or_update(local_indicatori_regione_csv, DRIVE_PAD26_INDICATORI)
drive_upload_or_update(local_indicatori_regione_rds, DRIVE_PAD26_INDICATORI)


## 10.5 Dataset dashboard ####
dashboard_pad26 <- pad26_raccordato %>%
  select(
    tipo_file_candidatura,
    dataset_id,
    avviso,
    ente,
    ente_key,
    codice_ipa,
    codice_ipa_key,
    tipologia_ente,
    comune,
    provincia,
    regione,
    cod_comune,
    cod_provincia,
    cod_regione,
    importo_finanziamento,
    stato_candidatura,
    stato_candidatura_key,
    codice_cup,
    
    match_lista,
    tipo_match,
    
    codice_fiscale,
    codice_ente_ipa,
    codice_ente_siope,
    ragione_sociale,
    
    forma_giuridica,
    descr_forma_giuridica,
    ateco_bdap,
    descr_ateco_bdap,
    
    presente_mpa,
    presente_s13,
    presente_bdap,
    codice_unita_mpa,
    codice_unita_s13,
    
    codice_regione_lista,
    fonte_codice_reg,
    fonte_ragione_sociale,
    fonte_fg,
    
    bdap_record_storicizzato,
    bdap_storicizzazione_ambigua,
    bdap_n_righe_originali
  )

local_dashboard_csv <- file.path(
  DIR_PAD26_INDICATORI_LOCAL,
  "dashboard_padigitale2026.csv"
)

local_dashboard_rds <- file.path(
  DIR_PAD26_INDICATORI_LOCAL,
  "dashboard_padigitale2026.rds"
)

write_csv(dashboard_pad26, local_dashboard_csv)
saveRDS(dashboard_pad26, local_dashboard_rds)

drive_upload_or_update(local_dashboard_csv, DRIVE_PAD26_INDICATORI)
drive_upload_or_update(local_dashboard_rds, DRIVE_PAD26_INDICATORI)

# 11) OUTPUT COMPLETO --------------------------------------------------------

local_padigitale2026_raccordato_lista_csv <- file.path(
  DIR_PAD26_RACCORDATO_LOCAL,
  "padigitale2026_raccordato_lista.csv"
)

local_padigitale2026_raccordato_lista_rds <- file.path(
  DIR_PAD26_RACCORDATO_LOCAL,
  "padigitale2026_raccordato_lista.rds"
)

write_csv(pad26_raccordato, local_padigitale2026_raccordato_lista_csv)
saveRDS(pad26_raccordato, local_padigitale2026_raccordato_lista_rds)

drive_upload_or_update(
  local_path = local_padigitale2026_raccordato_lista_csv,
  drive_folder_rel = DRIVE_PAD26_RACCORDATO
)

drive_upload_or_update(
  local_path = local_padigitale2026_raccordato_lista_rds,
  drive_folder_rel = DRIVE_PAD26_RACCORDATO
)


message("Raccordo PA digitale 2026 completato.")
message("RUN_ID_IMPORT: ", RUN_ID_IMPORT)
message("RUN_ID raccordo: ", RUN_ID)
message("- Drive raccordato: ", DRIVE_PAD26_RACCORDATO)
message("- Drive indicatori/dashboard: ", DRIVE_PAD26_INDICATORI)
message("- Drive logs: ", DRIVE_PAD26_LOGS)

if (delete_local_temp) {
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID_IMPORT), recursive = TRUE)
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Output", RUN_ID), recursive = TRUE)
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Logs", RUN_ID), recursive = TRUE)
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Indicatori", RUN_ID), recursive = TRUE)
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Processed_raccordato", RUN_ID), recursive = TRUE)
  unlink(file.path(DIR_TEMP, "lista.xlsx"), recursive = FALSE)
}

