# =================README=====================
# 04_ca_aggregazioni_dashboard.R
# Fonte: Conto Annuale
# Fase: preparazione dataset aggregati per dashboard SIM
#
# Logica:
# - parte dall'ultimo dataset wide PA-anno prodotto dallo script 03:
#     indicatori_CA_PA_multianno_<timestamp>.rds
# - legge anche l'ultimo dataset long:
#     indicatori_SIM_CA_long_multianno_<timestamp>.rds
# - costruisce dataset aggregati già pronti per la visualizzazione;
# - usa desc_fg come classificazione della forma giuridica;
# - sposta qui overview/copertura indicatori;
# - salva output versionati su Drive, senza cancellare file esistenti.
#
# Output principali:
# - indicatori_ca_overview
# - indicatori_ca_tempo
# - indicatori_ca_regione
# - indicatori_ca_tipologia
# - indicatori_ca_forma_giuridica
# - indicatori_ca_copertura
# - overview
# - metadati_indicatori_ca_dashboard
# ============================================================

rm(list = ls())

source("03_Scripts/00_config.R")
source("03_Scripts/00_sim_helpers.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(googledrive)
  library(jsonlite)
  library(tibble)
})

if (exists("SIM_DRIVE_EMAIL")) {
  googledrive::drive_auth(
    email = SIM_DRIVE_EMAIL,
    scopes = "https://www.googleapis.com/auth/drive"
  )
} else {
  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")
}

# Funzioni locali --------------------------------------------------------

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
  
  local_file <- sim_drive_download_to_temp(
    file,
    local_name = file$name[1],
    overwrite = TRUE
  )
  
  obj <- readRDS(local_file)
  unlink(local_file)
  
  message("File letto da Drive: ", file$name[1])
  
  obj
}

ca_upload_file_versioned <- function(local_file, drive_path, filename) {
  dir_drive <- sim_drive_mkdir_path(drive_path)
  
  googledrive::drive_upload(
    media = local_file,
    path = dir_drive,
    name = filename,
    overwrite = FALSE
  )
  
  invisible(filename)
}

ca_save_rds_upload_versioned <- function(obj, drive_path, filename) {
  local_file <- file.path(DIR_TEMP, filename)
  
  saveRDS(obj, local_file)
  
  ca_upload_file_versioned(
    local_file = local_file,
    drive_path = drive_path,
    filename = filename
  )
  
  unlink(local_file)
}

ca_write_csv_upload_versioned <- function(obj, drive_path, filename) {
  local_file <- file.path(DIR_TEMP, filename)
  
  readr::write_csv(obj, local_file)
  
  ca_upload_file_versioned(
    local_file = local_file,
    drive_path = drive_path,
    filename = filename
  )
  
  unlink(local_file)
}

ca_write_json_upload_versioned <- function(obj, drive_path, filename) {
  local_file <- file.path(DIR_TEMP, filename)
  
  jsonlite::write_json(
    obj,
    path = local_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
  
  ca_upload_file_versioned(
    local_file = local_file,
    drive_path = drive_path,
    filename = filename
  )
  
  unlink(local_file)
}

add_missing_numeric <- function(df, cols) {
  for (cc in cols) {
    if (!cc %in% names(df)) df[[cc]] <- NA_real_
  }
  
  df
}

add_missing_character <- function(df, cols) {
  for (cc in cols) {
    if (!cc %in% names(df)) df[[cc]] <- NA_character_
  }
  
  df
}

safe_div <- function(num, den, mult = 1) {
  dplyr::if_else(
    is.na(den) | den == 0,
    NA_real_,
    mult * num / den
  )
}

safe_wmean <- function(x, w) {
  if (length(x) == 0 || all(is.na(x)) || sum(w, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  weighted.mean(x, w = w, na.rm = TRUE)
}

ca_agg_dashboard <- function(df, grouping_vars) {
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
    dplyr::summarise(
      n_pa_mpa = dplyr::n_distinct(codice_fiscale),
      n_pa_con_ca = dplyr::n_distinct(codice_fiscale[fonte_conto_annuale == 1]),
      copertura_ca = safe_div(n_pa_con_ca, n_pa_mpa, 100),
      
      personale_totale = sim_safe_sum(PERSONALE_TOT),
      personale_uomini = sim_safe_sum(PERSONALE_UOMINI),
      personale_donne = sim_safe_sum(PERSONALE_DONNE),
      
      assunti = sim_safe_sum(ASSUN_TOT),
      assunti_uomini = sim_safe_sum(ASSUN_UOMINI),
      assunti_donne = sim_safe_sum(ASSUN_DONNE),
      
      cessati = sim_safe_sum(CESS_TOT),
      cessati_uomini = sim_safe_sum(CESS_UOMINI),
      cessati_donne = sim_safe_sum(CESS_DONNE),
      
      saldo = assunti - cessati,
      tasso_assunzione = safe_div(assunti, personale_totale, 100),
      tasso_cessazione = safe_div(cessati, personale_totale, 100),
      turnover = safe_div(assunti + cessati, personale_totale, 100),
      tasso_crescita = safe_div(saldo, personale_totale, 100),
      
      personale_tot_eta = sim_safe_sum(PERSONALE_TOT_ETA),
      eta_media = safe_wmean(ETA_MEDIA_PA, PERSONALE_TOT_ETA),
      
      under35 = sim_safe_sum(UNDER35),
      over55 = sim_safe_sum(OVER55),
      over65 = sim_safe_sum(OVER65),
      
      quota_under35 = safe_div(under35, personale_tot_eta, 100),
      quota_over55 = safe_div(over55, personale_tot_eta, 100),
      quota_over65 = safe_div(over65, personale_tot_eta, 100),
      indice_ricambio_generazionale = safe_div(under35, over55, 1),
      
      perc_donne = safe_div(personale_donne, personale_totale, 100),
      part_time_tot = sim_safe_sum(TOT_PART_TIME),
      perc_part_time = safe_div(part_time_tot, personale_totale, 100),
      
      giorni_form_tot = sim_safe_sum(GIORNI_FORM_TOT),
      giorni_form_uomini = sim_safe_sum(GIORNI_FORM_UOMINI),
      giorni_form_donne = sim_safe_sum(GIORNI_FORM_DONNE),
      giorni_form_per_dipendente = safe_div(giorni_form_tot, personale_totale, 1),
      
      spesa_totale = sim_safe_sum(TOTALE_SPESA),
      spesa_formazione_l020 = sim_safe_sum(SPESA_FORMAZIONE_L020),
      spesa_formazione_per_dipendente = safe_div(spesa_formazione_l020, personale_totale, 1),
      incidenza_spesa_formazione = safe_div(spesa_formazione_l020, spesa_totale, 100),
      
      .groups = "drop"
    )
}

save_dashboard_dataset <- function(obj, base_filename, timestamp_output, drive_path_dashboard) {
  filename_rds <- paste0(base_filename, "_", timestamp_output, ".rds")
  filename_csv <- paste0(base_filename, "_", timestamp_output, ".csv")
  filename_json <- paste0(base_filename, "_", timestamp_output, ".json")
  
  ca_save_rds_upload_versioned(obj, drive_path_dashboard, filename_rds)
  ca_write_csv_upload_versioned(obj, drive_path_dashboard, filename_csv)
  ca_write_json_upload_versioned(obj, drive_path_dashboard, filename_json)
  
  message("Dataset dashboard caricato: ", base_filename)
  
  invisible(c(filename_rds, filename_csv, filename_json))
}

# Lettura indicatori -----------------------------------------------------

indicatori_ca_pa <- read_latest_rds(
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  pattern = "^indicatori_CA_PA_multianno_.*\\.rds$"
)

indicatori_long <- read_latest_rds(
  drive_path = file.path(DRIVE_DIR_PROCESSED, "Conto_annuale"),
  pattern = "^indicatori_SIM_CA_long_multianno_.*\\.rds$"
)

cols_num <- c(
  "fonte_conto_annuale",
  "PERSONALE_TOT", "PERSONALE_UOMINI", "PERSONALE_DONNE",
  "ASSUN_TOT", "ASSUN_UOMINI", "ASSUN_DONNE",
  "CESS_TOT", "CESS_UOMINI", "CESS_DONNE",
  "PERSONALE_TOT_ETA", "ETA_MEDIA_PA",
  "UNDER35", "OVER55", "OVER65",
  "TOT_PART_TIME",
  "GIORNI_FORM_TOT", "GIORNI_FORM_UOMINI", "GIORNI_FORM_DONNE",
  "TOTALE_SPESA", "SPESA_FORMAZIONE_L020"
)

cols_chr <- c(
  "codice_fiscale",
  "ragione_sociale",
  "denominazione",
  "regione_bdap",
  "provincia",
  "comune",
  "descr_tipologia_istat_s13_bdap",
  "desc_tipo_istituzione_ca",
  "fg",
  "desc_fg"
)

indicatori_ca_pa <- indicatori_ca_pa %>%
  add_missing_numeric(cols_num) %>%
  add_missing_character(cols_chr) %>%
  dplyr::mutate(
    fonte_conto_annuale = dplyr::coalesce(fonte_conto_annuale, 0),
    
    PERSONALE_TOT = dplyr::coalesce(PERSONALE_TOT, PERSONALE_UOMINI + PERSONALE_DONNE),
    ASSUN_TOT = dplyr::coalesce(ASSUN_TOT, ASSUN_UOMINI + ASSUN_DONNE),
    CESS_TOT = dplyr::coalesce(CESS_TOT, CESS_UOMINI + CESS_DONNE),
    PERSONALE_TOT_ETA = dplyr::coalesce(PERSONALE_TOT_ETA, PERSONALE_TOT)
  )

# Indicatori nazionali ---------------------------------------------------

indicatori_ca_overview <- ca_agg_dashboard(
  indicatori_ca_pa,
  grouping_vars = c("anno")
)

# Indicatori temporali ---------------------------------------------------

indicatori_ca_tempo <- indicatori_ca_overview %>%
  dplyr::select(
    anno,
    personale_totale,
    assunti,
    cessati,
    saldo,
    tasso_assunzione,
    tasso_cessazione,
    tasso_crescita,
    eta_media,
    quota_under35,
    quota_over55,
    giorni_form_per_dipendente,
    spesa_formazione_per_dipendente,
    copertura_ca
  )

# Indicatori regionali ---------------------------------------------------

indicatori_ca_regione <- ca_agg_dashboard(
  indicatori_ca_pa,
  grouping_vars = c("anno", "regione_bdap")
) %>%
  dplyr::filter(!is.na(regione_bdap), regione_bdap != "")

# Indicatori per tipologia -----------------------------------------------

indicatori_ca_tipologia <- indicatori_ca_pa %>%
  dplyr::mutate(
    descr_tipologia_istat_s13_bdap = dplyr::coalesce(
      descr_tipologia_istat_s13_bdap,
      desc_tipo_istituzione_ca
    )
  ) %>%
  ca_agg_dashboard(
    grouping_vars = c("anno", "descr_tipologia_istat_s13_bdap")
  ) %>%
  dplyr::filter(
    !is.na(descr_tipologia_istat_s13_bdap),
    descr_tipologia_istat_s13_bdap != ""
  )

# Indicatori per forma giuridica -----------------------------------------

indicatori_ca_forma_giuridica <- ca_agg_dashboard(
  indicatori_ca_pa,
  grouping_vars = c("anno", "desc_fg")
) %>%
  dplyr::filter(!is.na(desc_fg), desc_fg != "")

# Indicatori copertura ---------------------------------------------------

indicatori_ca_copertura <- indicatori_ca_pa %>%
  dplyr::group_by(anno) %>%
  dplyr::summarise(
    n_pa_mpa = dplyr::n_distinct(codice_fiscale),
    n_pa_con_ca = dplyr::n_distinct(codice_fiscale[fonte_conto_annuale == 1]),
    n_pa_senza_ca = n_pa_mpa - n_pa_con_ca,
    quota_copertura_ca = safe_div(n_pa_con_ca, n_pa_mpa, 100),
    .groups = "drop"
  )

# overview <- indicatori_long %>%
#   dplyr::group_by(anno, indicatore_id) %>%
#   dplyr::summarise(
#     n_pa_perimetro_mpa = dplyr::n_distinct(codice_fiscale),
#     n_pa_con_valore = dplyr::n_distinct(codice_fiscale[!is.na(valore)]),
#     quota_copertura_indicatore = n_pa_con_valore / n_pa_perimetro_mpa,
#     
#     valore_totale = sum(valore, na.rm = TRUE),
#     valore_medio = mean(valore, na.rm = TRUE),
#     
#     .groups = "drop"
#   )

# Metadati dashboard -----------------------------------------------------

metadati_indicatori_ca_dashboard <- tibble::tribble(
  ~indicatore, ~label, ~asse_monitoraggio, ~riforma_pnrr, ~unita_misura, ~nota_aggregazione,
  
  "personale_totale", "Personale totale", "Accesso e reclutamento", "Riforma 2.1", "unità", "Somma PERSONALE_TOT",
  "assunti", "Assunti", "Accesso e reclutamento", "Riforma 2.1", "unità", "Somma ASSUN_TOT",
  "cessati", "Cessati", "Accesso e reclutamento", "Riforma 2.1", "unità", "Somma CESS_TOT",
  "saldo", "Saldo assunti-cessati", "Accesso e reclutamento", "Riforma 2.1", "unità", "Assunti meno cessati",
  "tasso_assunzione", "Tasso di assunzione", "Accesso e reclutamento", "Riforma 2.1", "%", "Assunti / personale totale",
  "tasso_cessazione", "Tasso di cessazione", "Accesso e reclutamento", "Riforma 2.1", "%", "Cessati / personale totale",
  "tasso_crescita", "Tasso di crescita del personale", "Accesso e reclutamento", "Riforma 2.1", "%", "Saldo / personale totale",
  
  "eta_media", "Età media", "Profilo demografico", "Riforma 2.1", "anni", "Media ponderata con PERSONALE_TOT_ETA",
  "quota_under35", "Quota under 35", "Profilo demografico", "Riforma 2.1", "%", "Under35 / PERSONALE_TOT_ETA",
  "quota_over55", "Quota over 55", "Profilo demografico", "Riforma 2.1", "%", "Over55 / PERSONALE_TOT_ETA",
  "quota_over65", "Quota over 65", "Profilo demografico", "Riforma 2.1", "%", "Over65 / PERSONALE_TOT_ETA",
  "indice_ricambio_generazionale", "Indice di ricambio generazionale", "Profilo demografico", "Riforma 2.1", "rapporto", "Under35 / Over55",
  
  "perc_donne", "Quota donne", "Composizione del personale", "Riforma 2.1", "%", "Donne / personale totale",
  "perc_part_time", "Incidenza part-time", "Composizione del personale", "Riforma 2.1", "%", "Part-time / personale totale",
  
  "giorni_form_tot", "Giorni di formazione", "Competenze e carriere", "Riforma 2.3", "giorni", "Somma GIORNI_FORM_TOT",
  "giorni_form_per_dipendente", "Giorni formazione per dipendente", "Competenze e carriere", "Riforma 2.3", "giorni", "Giorni formazione / personale totale",
  "spesa_formazione_l020", "Spesa formazione L020", "Competenze e carriere", "Riforma 2.3", "euro", "Somma SPESA_FORMAZIONE_L020",
  "spesa_formazione_per_dipendente", "Spesa formazione per dipendente", "Competenze e carriere", "Riforma 2.3", "euro", "Spesa L020 / personale totale",
  "incidenza_spesa_formazione", "Incidenza spesa formazione", "Competenze e carriere", "Riforma 2.3", "%", "Spesa L020 / totale spesa",
  
  "copertura_ca", "Copertura Conto Annuale", "Copertura fonte", "Trasversale", "%", "PA MPA con dati CA / PA MPA"
)

# Output su Drive --------------------------------------------------------

timestamp_output <- format(Sys.time(), "%Y%m%d_%H%M%S")

drive_path_dashboard <- file.path(DRIVE_DIR_OUTPUT, "Conto_annuale", "Dashboard")

save_dashboard_dataset(
  indicatori_ca_overview,
  "INDICATORS_CA_OVERVIEW",
  timestamp_output,
  drive_path_dashboard
)

save_dashboard_dataset(
  indicatori_ca_tempo,
  "INDICATORS_CA_TEMPO",
  timestamp_output,
  drive_path_dashboard
)

save_dashboard_dataset(
  indicatori_ca_regione,
  "INDICATORS_CA_REGIONE",
  timestamp_output,
  drive_path_dashboard
)

save_dashboard_dataset(
  indicatori_ca_tipologia,
  "INDICATORS_CA_TIPOLOGIA",
  timestamp_output,
  drive_path_dashboard
)

save_dashboard_dataset(
  indicatori_ca_forma_giuridica,
  "INDICATORS_CA_FORMA_GIURIDICA",
  timestamp_output,
  drive_path_dashboard
)

save_dashboard_dataset(
  indicatori_ca_copertura,
  "INDICATORS_CA_COPERTURA",
  timestamp_output,
  drive_path_dashboard
)

# save_dashboard_dataset(
#   overview,
#   "INDICATORS_CA_OVERVIEW_INDICATORI",
#   timestamp_output,
#   drive_path_dashboard
# )

save_dashboard_dataset(
  metadati_indicatori_ca_dashboard,
  "MET_INDICATORS_CA",
  timestamp_output,
  drive_path_dashboard
)

message("Aggregazioni dashboard CA completate.")
