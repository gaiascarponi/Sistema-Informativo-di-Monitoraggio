# ============================================================= #
# Script: 06_run_dashboard_SIM_ContoAnnuale.R
#
# Obiettivo:
#   1. scaricare da Drive l'ultimo master Conto annuale;
#   2. avviare localmente la dashboard R Markdown con runtime Shiny.
# ============================================================= #

rm(list = ls())

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/helper_console_log.R")
source("03_Scripts/Conto_annuale/00_ca_config.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(googledrive)
  library(rmarkdown)
})

# 1) AUTENTICAZIONE DRIVE ----------------------------------------------------

if (exists("SIM_DRIVE_EMAIL")) {
  options(gargle_oauth_email = SIM_DRIVE_EMAIL)
  
  googledrive::drive_auth(
    email = SIM_DRIVE_EMAIL,
    scopes = "https://www.googleapis.com/auth/drive",
    cache = TRUE
  )
} else {
  googledrive::drive_auth(
    scopes = "https://www.googleapis.com/auth/drive"
  )
}

# 2) PARAMETRI ---------------------------------------------------------------

FILE_RMD <- file.path(
  "03_Scripts",
  "Conto_annuale",
  "05_dashboard_SIM_ContoAnnuale.Rmd"
)

if (!file.exists(FILE_RMD)) {
  stop("File Rmd non trovato: ", FILE_RMD)
}

FILE_RMD <- normalizePath(
  FILE_RMD,
  winslash = "/",
  mustWork = TRUE
)

DRIVE_CONTO_ANNUALE <- DRIVE_DIR_PROCESSED_CONTO_ANNUALE
PATTERN_CONTO_ANNUALE <- "^master_CA_multianno_.*\\.rds$"

RUN_ID_DASHBOARD <- format(Sys.time(), "%Y%m%d_%H%M%S")

DIR_DASH_LOCAL <- normalizePath(
  file.path(
    DIR_TEMP,
    "Conto_annuale",
    "Dashboard",
    RUN_ID_DASHBOARD
  ),
  winslash = "/",
  mustWork = FALSE
)

DIR_INPUT_LOCAL <- file.path(
  DIR_DASH_LOCAL,
  "input"
)

DIR_LOGS_LOCAL <- file.path(
  DIR_DASH_LOCAL,
  "logs"
)

dir.create(
  DIR_INPUT_LOCAL,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  DIR_LOGS_LOCAL,
  recursive = TRUE,
  showWarnings = FALSE
)

DRIVE_LOGS <- file.path(
  DRIVE_CA_LOGS,
  "Dashboard",
  RUN_ID_DASHBOARD
)

console_log <- start_console_log(
  log_dir = DIR_LOGS_LOCAL,
  run_id = RUN_ID_DASHBOARD,
  script_name = "06_run_dashboard_SIM_ContoAnnuale"
)

# 3) HELPERS -----------------------------------------------------------------

find_latest_drive_file <- function(drive_folder_rel, pattern) {
  folder <- sim_drive_ls_path(
    drive_folder_rel,
    create = FALSE
  )
  
  files <- googledrive::drive_ls(folder) %>%
    dplyr::filter(
      stringr::str_detect(
        .data$name,
        stringr::regex(pattern, ignore_case = TRUE)
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$name)) %>%
    dplyr::slice(1)
  
  if (nrow(files) == 0L) {
    stop(
      "Nessun file trovato in ",
      drive_folder_rel,
      " con pattern ",
      pattern
    )
  }
  
  files
}

download_latest <- function(drive_folder_rel, pattern, local_name = NULL) {
  file <- find_latest_drive_file(
    drive_folder_rel = drive_folder_rel,
    pattern = pattern
  )
  
  if (is.null(local_name) || !nzchar(local_name)) {
    local_name <- file$name[[1]]
  }
  
  local_path <- file.path(
    DIR_INPUT_LOCAL,
    local_name
  )
  
  googledrive::drive_download(
    file = file,
    path = local_path,
    overwrite = TRUE
  )
  
  if (!file.exists(local_path)) {
    stop("Download non riuscito: ", local_path)
  }
  
  normalizePath(
    local_path,
    winslash = "/",
    mustWork = TRUE
  )
}

# 4) RUN DASHBOARD -----------------------------------------------------------

status_run <- "failed"

tryCatch({
  
  local_master_ca <- download_latest(
    drive_folder_rel = DRIVE_CONTO_ANNUALE,
    pattern = PATTERN_CONTO_ANNUALE
  )
  
  message("Dashboard Conto annuale:")
  message(" - Rmd: ", FILE_RMD)
  message(" - Master CA: ", local_master_ca)
  message(" - RUN_ID_DASHBOARD: ", RUN_ID_DASHBOARD)
  
  status_run <- "running"
  
  rmarkdown::run(
    file = FILE_RMD,
    shiny_args = list(
      launch.browser = TRUE
    ),
    render_args = list(
      params = list(
        file_master_ca = local_master_ca
      ),
      knit_root_dir = getwd(),
      envir = new.env(parent = globalenv())
    )
  )
  
  status_run <- "completed"
  
}, error = function(e) {
  
  message(
    "ERRORE dashboard Conto annuale: ",
    conditionMessage(e)
  )
  
  status_run <<- "failed"
  stop(e)
  
}, finally = {
  
  console_log_path <- stop_console_log(
    console_log,
    status = status_run
  )
  
  message(
    "Log generato: ",
    basename(console_log_path),
    " | Percorso locale: ",
    console_log_path
  )
  
  drive_upload_or_update(
    local_path = console_log_path,
    drive_folder_rel = DRIVE_LOGS
  )
  
  message(
    "Log caricato su Drive: ",
    DRIVE_LOGS,
    "/",
    basename(console_log_path)
  )
})

message(
  "--- Dashboard Conto annuale terminata. RUN_ID: ",
  RUN_ID_DASHBOARD,
  " | status: ",
  status_run,
  " ---"
)