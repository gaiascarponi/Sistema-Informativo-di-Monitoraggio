# =================README=====================
# 03_ca_indicatori_sim.R
# Fonte: Conto Annuale
# Fase: calcolo indicatori SIM da master MPA arricchito CA

# Il master contiene tutto il perimetro MPA. Le PA non coperte
# dal Conto Annuale rimangono nel dataset con valori NA.
# Legge l'ultimo master versionato prodotto dallo script 02.
# Salva output versionati su Drive, senza cancellare file esistenti.

rm(list = ls())

source("03_Scripts/00_config.R")
source("03_Scripts/00_sim_helpers.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(googledrive)
})

if (exists("SIM_DRIVE_EMAIL")) {
  googledrive::drive_auth(
    email = SIM_DRIVE_EMAIL,
    scopes = "https://www.googleapis.com/auth/drive"
  )
} else {
  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")
}

# Funzioni locali ----------------------------------------------------------

drive_latest_file <- function(drive_path, pattern) {
  dir_drive <- sim_drive_ls_path(drive_path, create = FALSE)
  file <- googledrive::drive_ls(dir_drive) %>%
    dplyr::filter(
      stringr::str_detect(
        .data$name,
        stringr::regex(pattern, ignore_case = TRUE)
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$name)) %>%
    dplyr::slice(1)

  if (nrow(file) == 0) {
    stop("Nessun file trovato in ", drive_path, " con pattern: ", pattern)
  }
  file
}

read_latest_rds <- function(drive_path, pattern) {
  file <- drive_latest_file(drive_path, pattern)
  local_file <- sim_drive_download_to_temp(file, local_name = file$name[1], overwrite = TRUE)
  obj <- readRDS(local_file)
  unlink(local_file)
  message("File letto da Drive: ", file$name[1])
  obj
}

ca_save_rds_upload_versioned <- function(obj, drive_path, filename) {
  dir_drive <- sim_drive_mkdir_path(drive_path)
  local_file <- file.path(DIR_TEMP, filename)
  saveRDS(obj, local_file)
  googledrive::drive_upload(media = local_file, path = dir_drive, name = filename)
  unlink(local_file)
}

ca_write_csv_upload_versioned <- function(obj, drive_path, filename) {
  dir_drive <- sim_drive_mkdir_path(drive_path)
  local_file <- file.path(DIR_TEMP, filename)
  readr::write_csv(obj, local_file)
  googledrive::drive_upload(media = local_file, path = dir_drive, name = filename)
  unlink(local_file)
}

add_missing_numeric <- function(df, cols) {
  for (cc in cols) {
    if (!cc %in% names(df)) df[[cc]] <- NA_real_
  }
  df
}

# Lettura ultimo master_CA_multianno -------------------------------------------

master_ca <- read_latest_rds(
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  pattern = "^master_CA_multianno_.*\\.rds$"
)

cols_numeriche_attese <- c(
  "PERSONALE_UOMINI", "PERSONALE_DONNE", "PERSONALE_TOT",
  "TEMPO_PIENO_UOMINI", "TEMPO_PIENO_DONNE", "TEMPO_PIENO_TOT",
  "PART_TIME_UOMINI", "PART_TIME_DONNE", "TOT_PART_TIME",
  "ASSUN_UOMINI", "ASSUN_DONNE", "ASSUN_TOT",
  "CESS_UOMINI", "CESS_DONNE", "CESS_TOT",
  "PERSONALE_TOT_ETA", "ETA_MEDIA_PA",
  "UNDER35_UOMINI", "UNDER35_DONNE", "UNDER35",
  "OVER55_UOMINI", "OVER55_DONNE", "OVER55",
  "OVER65_UOMINI", "OVER65_DONNE", "OVER65",
  "QUOTA_UNDER35_PERC", "QUOTA_UNDER35_UOMINI_PERC", "QUOTA_UNDER35_DONNE_PERC",
  "QUOTA_OVER55_PERC", "QUOTA_OVER65_PERC", "INDICE_RICAMBIO_GENERAZIONALE",
  "GIORNI_FORM_UOMINI", "GIORNI_FORM_DONNE", "GIORNI_FORM_TOT",
  "FORM_MEDIA_UOMINI_CA", "FORM_MEDIA_DONNE_CA",
  "TOTALE_SPESA", "SPESA_FORMAZIONE_L020", "PERC_PART_TIME",
  "GIORNI_FORM_PER_DIPENDENTE", "SPESA_FORMAZIONE_PER_DIPENDENTE",
  "INCIDENZA_SPESA_FORMAZIONE_PERC", "fonte_conto_annuale"
)

master_ca <- add_missing_numeric(master_ca, cols_numeriche_attese)


# Indicatori PA-anno -----------------------------------------------------------

indicatori_ca <- master_ca %>%
  dplyr::mutate(
    PERSONALE_TOT = dplyr::coalesce(PERSONALE_TOT, PERSONALE_UOMINI + PERSONALE_DONNE),
    ASSUN_TOT = dplyr::coalesce(ASSUN_TOT, ASSUN_UOMINI + ASSUN_DONNE),
    CESS_TOT = dplyr::coalesce(CESS_TOT, CESS_UOMINI + CESS_DONNE),

    SALDO_ASS_CESS = ASSUN_TOT - CESS_TOT,
    #SALDO_TOT = SALDO_ASS_CESS,

    TURNOVER_PERC = sim_safe_div(ASSUN_TOT + CESS_TOT, PERSONALE_TOT, 100),
    TURNOVER_PCT = TURNOVER_PERC,

    TASSO_CRESCITA_PERC = sim_safe_div(ASSUN_TOT - CESS_TOT, PERSONALE_TOT, 100),
    CRESCITA_PCT = TASSO_CRESCITA_PERC,

    PERC_DONNE = sim_safe_div(PERSONALE_DONNE, PERSONALE_TOT, 100),
    PERC_PART_TIME = sim_safe_div(TOT_PART_TIME, PERSONALE_TOT, 100),

    ETA_MEDIA_TOT = ETA_MEDIA_PA,
    UNDER35_PCT = QUOTA_UNDER35_PERC,
    OVER55_PCT = QUOTA_OVER55_PERC,
    OVER65_PCT = QUOTA_OVER65_PERC,

    FORM_MEDIA_TOT = sim_safe_div(GIORNI_FORM_TOT, PERSONALE_TOT, 1),
    GIORNI_FORM_PER_DIPENDENTE = dplyr::coalesce(
      GIORNI_FORM_PER_DIPENDENTE,
      sim_safe_div(GIORNI_FORM_TOT, PERSONALE_TOT, 1)
    ),
    SPESA_FORMAZIONE_PER_DIPENDENTE = dplyr::coalesce(
      SPESA_FORMAZIONE_PER_DIPENDENTE,
      sim_safe_div(SPESA_FORMAZIONE_L020, PERSONALE_TOT, 1)
    ),
    INCIDENZA_SPESA_FORMAZIONE_PERC = dplyr::coalesce(
      INCIDENZA_SPESA_FORMAZIONE_PERC,
      sim_safe_div(SPESA_FORMAZIONE_L020, TOTALE_SPESA, 100)
    ),

    fonte = "Conto Annuale",
    livello_aggregazione = "PA-anno"
  )

indicatori_cols <- c(
  "PERSONALE_TOT", "PERSONALE_UOMINI", "PERSONALE_DONNE",
  "ASSUN_TOT", "ASSUN_UOMINI", "ASSUN_DONNE",
  "CESS_TOT", "CESS_UOMINI", "CESS_DONNE", "SALDO_TOT", "SALDO_ASS_CESS",
  "TURNOVER_PCT", "TURNOVER_PERC", "CRESCITA_PCT", "TASSO_CRESCITA_PERC",
  "PERC_DONNE", "TOT_PART_TIME", "PERC_PART_TIME",
  "ETA_MEDIA_TOT", "ETA_MEDIA_PA",
  "UNDER35", "OVER55", "OVER65",
  "UNDER35_PCT", "OVER55_PCT", "OVER65_PCT",
  "QUOTA_UNDER35_PERC", "QUOTA_OVER55_PERC", "QUOTA_OVER65_PERC",
  "INDICE_RICAMBIO_GENERAZIONALE",
  "GIORNI_FORM_TOT", "FORM_MEDIA_TOT", "GIORNI_FORM_PER_DIPENDENTE",
  "TOTALE_SPESA", "SPESA_FORMAZIONE_L020", "SPESA_FORMAZIONE_PER_DIPENDENTE",
  "INCIDENZA_SPESA_FORMAZIONE_PERC"
)

indicatori_long <- indicatori_ca %>%
  dplyr::select(
    dplyr::any_of(c(
      "anno",
      "codice_fiscale",
      "codice_reg",
      "ragione_sociale",
      "denominazione",
      
      "fg",
      "desc_fg",
      
      "codice_unita_s13",
      "codice_unita_mpa",
      
      "codice_regione_bdap",
      "regione_bdap",
      "codice_provincia",
      "sigla_provincia",
      "provincia",
      "codice_comune",
      "comune",
      
      "desc_tipo_istituzione_ca",
      "desc_istituzione_ca",
      "descr_tipologia_istat_s13_bdap",
      "descr_forma_giuridica_bdap",
      "descr_categoria_ipa_bdap",
      "descr_tipologia_ipa_bdap",
      
      "presente_mpa",
      "presente_MPA",
      "fonte_conto_annuale",
      
      indicatori_cols
    ))
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::any_of(indicatori_cols),
    names_to = "indicatore_id",
    values_to = "valore"
  ) %>%
  dplyr::mutate(
    fonte = "Conto Annuale",
    livello_aggregazione = "PA-anno"
  )


# Output versionati su Drive --------------------------------------------------

timestamp_output <- format(Sys.time(), "%Y%m%d_%H%M%S")

filename_indicatori_rds  <- paste0("indicatori_CA_PA_multianno_", timestamp_output, ".rds")
filename_indicatori_csv  <- paste0("indicatori_CA_PA_multianno_", timestamp_output, ".csv")
filename_indicatori_json <- paste0("indicatori_CA_PA_multianno_", timestamp_output, ".json")

filename_long_rds  <- paste0("indicatori_SIM_CA_long_multianno_", timestamp_output, ".rds")
filename_long_csv  <- paste0("indicatori_SIM_CA_long_multianno_", timestamp_output, ".csv")
filename_long_json <- paste0("indicatori_SIM_CA_long_multianno_", timestamp_output, ".json")

filename_overview_csv  <- paste0("sim_CA_overview_multianno_", timestamp_output, ".csv")
filename_overview_json <- paste0("sim_CA_overview_multianno_", timestamp_output, ".json")

ca_save_rds_upload_versioned(
  indicatori_ca,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_indicatori_rds
)

ca_write_csv_upload_versioned(
  indicatori_ca,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_indicatori_csv
)

ca_write_json_upload_versioned(
  indicatori_ca,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_indicatori_json
)

ca_save_rds_upload_versioned(
  indicatori_long,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_long_rds
)

ca_write_csv_upload_versioned(
  indicatori_long,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_long_csv
)

ca_write_json_upload_versioned(
  indicatori_long,
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  filename = filename_long_json
)

ca_write_csv_upload_versioned(
  overview,
  drive_path = file.path(DRIVE_DIR_OUTPUT, "Conto_annuale"),
  filename = filename_overview_csv
)

ca_write_json_upload_versioned(
  overview,
  drive_path = file.path(DRIVE_DIR_OUTPUT, "Conto_annuale"),
  filename = filename_overview_json
)

message("File Indicatori CA caricati su Drive:")
message(" - ", filename_indicatori_rds)
message(" - ", filename_indicatori_csv)
message(" - ", filename_indicatori_json)
message(" - ", filename_long_rds)
message(" - ", filename_long_csv)
message(" - ", filename_long_json)
message(" - ", filename_overview_csv)
message(" - ", filename_overview_json)