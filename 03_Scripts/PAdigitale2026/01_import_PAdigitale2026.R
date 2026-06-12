# ============================================================ #
# Script: 01_import_PAdigitale2026.R
# Fonte: PA digitale 2026 - Open data
# Obiettivo:
#   1. scaricare i CSV da GitHub
#   2. salvare raw e processed
#   3. creare un dataset unico candidature finanziate
# ============================================================ #

# 0) Pulizia ambiente e pacchetti --------------------------------------------

rm(list = ls())

library(dplyr)
library(readr)
library(janitor)
library(stringr)
library(purrr)
library(lubridate)
library(googledrive)


# 1) Configurazione ----------------------------------------------------------

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")

googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")

# parametro per pulire la cartella temp alla fine del run
delete_local_temp <- FALSE

RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("RUN_ID import: ", RUN_ID)

base_raw_url <- "https://raw.githubusercontent.com/teamdigitale/padigitale2026-opendata/main/data"


# 2) Path locali e Drive -----------------------------------------------------

DIR_PAD26_RAW_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Source", RUN_ID)
DIR_PAD26_PROCESSED_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID)
DIR_PAD26_METADATA_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Metadata", RUN_ID)

DRIVE_PAD26_RAW <- file.path(DRIVE_DIR_SOURCE, "PADigitale2026", RUN_ID)
DRIVE_PAD26_PROCESSED <- file.path(DRIVE_DIR_PROCESSED, "PADigitale2026", RUN_ID)
DRIVE_PAD26_METADATA <- file.path(DRIVE_DIR_METADATA, "Source_met", "PADigitale2026", RUN_ID)

dir.create(DIR_PAD26_RAW_LOCAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PAD26_PROCESSED_LOCAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PAD26_METADATA_LOCAL, recursive = TRUE, showWarnings = FALSE)

run_info <- tibble::tibble(
  run_id = RUN_ID,
  data_run = Sys.time(),
  fonte = "PA digitale 2026",
  base_raw_url = base_raw_url
)

local_run_info <- file.path(DIR_PAD26_PROCESSED_LOCAL, "run_info.csv")
write_csv(run_info, local_run_info)
# drive_upload_or_update(local_run_info, DRIVE_PAD26_PROCESSED)
drive_upload_or_update(
  local_path = local_run_info,
  drive_folder_rel = DRIVE_PAD26_PROCESSED
)


# 3) Elenco file sorgente ----------------------------------------------------

file_pad26 <- tibble::tribble(
  ~dataset_id, ~filename,
  "avvisi", "avvisi.csv",
  "candidature_comuni_finanziate", "candidature_comuni_finanziate.csv",
  "candidature_scuole_finanziate", "candidature_scuole_finanziate.csv",
  "candidature_altrienti_finanziate", "candidature_altrienti_finanziate.csv"
)


# 4) Funzioni ----------------------------------------------------------------

scarica_pad26 <- function(dataset_id, filename) {
  
  url <- paste0(base_raw_url, "/", filename)
  raw_path <- file.path(DIR_PAD26_RAW_LOCAL, filename)
  
  message("Scarico: ", filename)
  
  download.file(
    url = url,
    destfile = raw_path,
    mode = "wb",
    quiet = FALSE
  )
  
  drive_upload_or_update(
    local_path = raw_path,
    drive_folder_rel = DRIVE_PAD26_RAW
  )
  
  df <- readr::read_csv(
    raw_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    mutate(
      dataset_id = dataset_id,
      file_origine = filename,
      .before = 1
    )
  
  df
}


# 5) Download raw e import ---------------------------------------------------

pad26_list <- purrr::map2(
  file_pad26$dataset_id,
  file_pad26$filename,
  scarica_pad26
)

names(pad26_list) <- file_pad26$dataset_id

# names(pad26_list$avvisi)
# names(pad26_list$candidature_comuni_finanziate)
# names(pad26_list$candidature_scuole_finanziate)
# names(pad26_list$candidature_altrienti_finanziate)

# 6) Creazione dataset processed --------------------------------------------

avvisi <- pad26_list$avvisi

candidature_pad26 <- bind_rows(
  pad26_list$candidature_comuni_finanziate,
  pad26_list$candidature_scuole_finanziate,
  pad26_list$candidature_altrienti_finanziate,
  .id = "tipo_file_candidatura"
)

candidature_pad26 <- candidature_pad26 %>%
  mutate(
    importo_finanziamento = readr::parse_number(importo_finanziamento),
    data_invio_candidatura = lubridate::ymd_hms(data_invio_candidatura, quiet = TRUE),
    data_finanziamento = lubridate::ymd(data_finanziamento, quiet = TRUE),
    data_stato_candidatura = lubridate::ymd(data_stato_candidatura, quiet = TRUE),
    cod_regione = stringr::str_pad(as.character(cod_regione), 2, pad = "0"),
    cod_provincia = stringr::str_pad(as.character(cod_provincia), 3, pad = "0"),
    cod_comune = as.character(cod_comune)
  )


# 7) Salvataggio processed ---------------------------------------------------

local_avvisi <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "avvisi_padigitale2026.csv"
)

local_candidature_csv <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "candidature_finanziate_padigitale2026.csv"
)

local_candidature_rds <- file.path(
  DIR_PAD26_PROCESSED_LOCAL,
  "candidature_finanziate_padigitale2026.rds"
)

write_csv(avvisi, local_avvisi)
write_csv(candidature_pad26, local_candidature_csv)
saveRDS(candidature_pad26, local_candidature_rds)

drive_upload_or_update(local_avvisi, DRIVE_PAD26_PROCESSED)
drive_upload_or_update(local_candidature_csv, DRIVE_PAD26_PROCESSED)
drive_upload_or_update(local_candidature_rds, DRIVE_PAD26_PROCESSED)


# 8) Metadati tecnici della run ----------------------------------------------

# Dataset da documentare:
# - raw: singoli file scaricati da GitHub
# - processed: dataset prodotti dallo script
dataset_metadata_list <- c(
  pad26_list,
  list(
    avvisi_processed = avvisi,
    candidature_finanziate_processed = candidature_pad26
  )
)

metadata_file_pad26 <- purrr::imap_dfr(
  dataset_metadata_list,
  function(df, dataset_id) {
    tibble::tibble(
      run_id = RUN_ID,
      data_run = Sys.time(),
      dataset_id = dataset_id,
      file_origine = if ("file_origine" %in% names(df)) {
        paste(unique(df$file_origine), collapse = " | ")
      } else {
        NA_character_
      },
      livello = ifelse(str_detect(dataset_id, "processed"), "processed", "raw"),
      n_righe = nrow(df),
      n_colonne = ncol(df),
      variabili = paste(names(df), collapse = " | ")
    )
  }
)

metadata_variabili_pad26 <- purrr::imap_dfr(
  dataset_metadata_list,
  function(df, dataset_id) {
    
    livello_dataset <- ifelse(str_detect(dataset_id, "processed"), "processed", "raw")
    
    purrr::map_dfr(
      names(df),
      function(var) {
        
        x <- df[[var]]
        x_chr <- as.character(x)
        valori_non_missing <- x_chr[!is.na(x_chr) & x_chr != ""]
        
        tibble::tibble(
          run_id = RUN_ID,
          data_run = Sys.time(),
          dataset_id = dataset_id,
          livello = livello_dataset,
          file_origine = if ("file_origine" %in% names(df)) {
            paste(unique(df$file_origine), collapse = " | ")
          } else {
            NA_character_
          },
          variabile = var,
          tipo_r = class(x)[1],
          n_righe = length(x),
          n_missing = sum(is.na(x_chr) | x_chr == ""),
          pct_missing = round(100 * n_missing / n_righe, 2),
          n_valori_distinti = dplyr::n_distinct(x_chr, na.rm = TRUE),
          esempi_valori = paste(head(unique(valori_non_missing), 5), collapse = " | ")
        )
      }
    )
  }
)


# 9) Salvataggio metadati ----------------------------------------------------

local_metadata_file <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  "metadata_file_padigitale2026.csv"
)

local_metadata_variabili <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  "metadata_variabili_padigitale2026.csv"
)

local_run_info_metadata <- file.path(
  DIR_PAD26_METADATA_LOCAL,
  "run_info.csv"
)

write_csv(metadata_file_pad26, local_metadata_file)
write_csv(metadata_variabili_pad26, local_metadata_variabili)
write_csv(run_info, local_run_info_metadata)

drive_upload_or_update(
  local_path = local_metadata_file,
  drive_folder_rel = DRIVE_PAD26_METADATA
)

drive_upload_or_update(
  local_path = local_metadata_variabili,
  drive_folder_rel = DRIVE_PAD26_METADATA
)

drive_upload_or_update(
  local_path = local_run_info_metadata,
  drive_folder_rel = DRIVE_PAD26_METADATA
)


# 10) Chiusura ---------------------------------------------------------------

message("Import PA digitale 2026 completato.")
message("- RUN_ID: ", RUN_ID)
message("- Drive raw: ", DRIVE_PAD26_RAW)
message("- Drive processed: ", DRIVE_PAD26_PROCESSED)
message("- Drive metadata: ", DRIVE_PAD26_METADATA)

if (delete_local_temp) {
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Source", RUN_ID), recursive = TRUE)
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Processed", RUN_ID), recursive = TRUE)
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Metadata", RUN_ID), recursive = TRUE)
}
