# ============================================================ #
# Script: 05_render_dashboard_SIM_PADigitale2026.R
# Obiettivo:
#   1. scaricare gli output dello script indicatori;
#   2. renderizzare la dashboard flexdashboard Rmd;
#   3. caricare l'HTML in Drive.
#
# Nota:
#   Questo script usa percorsi LOCALI ASSOLUTI per evitare crash di
#   normalizePath()/rmarkdown::render quando R cambia working directory.
# ============================================================ #

rm(list = ls())

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")
source("03_Scripts/helper_console_log.R")

suppressPackageStartupMessages({
  library(googledrive)
  library(rmarkdown)
})

googledrive::drive_auth(
  scopes = "https://www.googleapis.com/auth/drive"
)

# --------------------------------------------------------------------------- #
# PARAMETRI DA AGGIORNARE
# --------------------------------------------------------------------------- #

# RUN_ID prodotto dallo script 03_indicatori_padigitale2026.
RUN_ID_INDICATORS <- "20260622_181949"

# File Rmd della dashboard.
# FILE_RMD <- "03_Scripts/05_dashboard_SIM_PADigitale2026.Rmd"
FILE_RMD <- file.path(
  "03_Scripts",
  "PAdigitale2026",
  "05_dashboard_SIM_PADigitale2026_fix_nuts_name.Rmd"
)

# Anno e risoluzione delle geometrie NUTS.
ANNO_NUTS <- 2024
RISOLUZIONE_NUTS <- "10"

# Pulizia locale a fine esecuzione.
delete_local_temp <- FALSE

if (
  !nzchar(RUN_ID_INDICATORS) ||
  identical(RUN_ID_INDICATORS, "INSERIRE_RUN_ID_INDICATORI")
) {
  stop("Aggiornare RUN_ID_INDICATORS prima di eseguire lo script.")
}

if (!file.exists(FILE_RMD)) {
  stop("File Rmd non trovato: ", FILE_RMD)
}

FILE_RMD <- normalizePath(
  FILE_RMD,
  winslash = "/",
  mustWork = TRUE
)

RUN_ID_DASHBOARD <- format(Sys.time(), "%Y%m%d_%H%M%S")

get_config_value <- function(name, default = NULL) {
  if (exists(name, inherits = TRUE)) {
    get(name, inherits = TRUE)
  } else {
    default
  }
}

# Compatibilità con le varianti di nome usate nel progetto.
DRIVE_DIR_INDICATORS_BASE <- get_config_value(
  "DRIVE_DIR_INDICATORS",
  get_config_value(
    "DRIVE_DIR_INDICATORI",
    file.path("01_Dataset", "Indicators")
  )
)

DRIVE_DIR_METADATA_BASE <- get_config_value(
  "DRIVE_DIR_METADATA",
  "02_Metadata"
)

DRIVE_DIR_INDICATORS_MET_BASE <- get_config_value(
  "DRIVE_DIR_INDICATORS_MET",
  file.path(DRIVE_DIR_METADATA_BASE, "Indicators_met")
)

DRIVE_DIR_OUTPUT_BASE <- get_config_value(
  "DRIVE_DIR_OUTPUT",
  "04_Output"
)

# --------------------------------------------------------------------------- #
# FUNZIONI LOCALI
# --------------------------------------------------------------------------- #

make_abs <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

assert_file_exists <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " non trovato localmente: ", path)
  }
  invisible(path)
}

download_and_check <- function(drive_file_rel, local_path, label) {
  local_path_abs <- normalizePath(
    local_path,
    winslash = "/",
    mustWork = FALSE
  )

  dir.create(
    dirname(local_path_abs),
    recursive = TRUE,
    showWarnings = FALSE
  )

  drive_download_from_path(
    drive_file_rel = drive_file_rel,
    local_path = local_path_abs
  )

  assert_file_exists(local_path_abs, label)

  local_path_abs <- normalizePath(
    local_path_abs,
    winslash = "/",
    mustWork = TRUE
  )

  message(
    label,
    " scaricato e verificato: ",
    local_path_abs
  )

  local_path_abs
}

# --------------------------------------------------------------------------- #
# CARTELLE LOCALI ASSOLUTE
# --------------------------------------------------------------------------- #

DIR_PAD26_DASH_LOCAL <- make_abs(file.path(
  DIR_TEMP,
  "PADigitale2026",
  "Dashboard",
  RUN_ID_DASHBOARD
))

DIR_PAD26_DASH_INPUT_LOCAL <- file.path(
  DIR_PAD26_DASH_LOCAL,
  "input"
)

DIR_PAD26_DASH_OUTPUT_LOCAL <- file.path(
  DIR_PAD26_DASH_LOCAL,
  "output"
)

DIR_PAD26_LOGS_LOCAL <- make_abs(file.path(
  DIR_TEMP,
  "PADigitale2026",
  "Logs",
  RUN_ID_DASHBOARD
))

dir.create(
  DIR_PAD26_DASH_INPUT_LOCAL,
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  DIR_PAD26_DASH_OUTPUT_LOCAL,
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  DIR_PAD26_LOGS_LOCAL,
  recursive = TRUE,
  showWarnings = FALSE
)

# --------------------------------------------------------------------------- #
# CARTELLE DRIVE
# --------------------------------------------------------------------------- #

DRIVE_PAD26_INDICATORS <- file.path(
  DRIVE_DIR_INDICATORS_BASE,
  "PADigitale2026",
  RUN_ID_INDICATORS
)

DRIVE_PAD26_INDICATORS_MET <- file.path(
  DRIVE_DIR_INDICATORS_MET_BASE,
  "PADigitale2026",
  RUN_ID_INDICATORS
)

DRIVE_PAD26_DASH_OUTPUT <- file.path(
  DRIVE_DIR_OUTPUT_BASE,
  "PADigitale2026",
  RUN_ID_DASHBOARD
)

DRIVE_PAD26_LOGS <- file.path(
  DRIVE_DIR_LOGS,
  "PADigitale2026",
  RUN_ID_DASHBOARD
)

# --------------------------------------------------------------------------- #
# LOG
# --------------------------------------------------------------------------- #

console_log <- start_console_log(
  log_dir = DIR_PAD26_LOGS_LOCAL,
  run_id = RUN_ID_DASHBOARD,
  script_name = "05_render_dashboard_SIM_PADigitale2026"
)

status_run <- "failed"

tryCatch({

  message("RUN_ID indicatori: ", RUN_ID_INDICATORS)
  message("RUN_ID dashboard: ", RUN_ID_DASHBOARD)
  message("Input indicatori: ", DRIVE_PAD26_INDICATORS)
  message("Input metadati: ", DRIVE_PAD26_INDICATORS_MET)
  message("Output dashboard: ", DRIVE_PAD26_DASH_OUTPUT)

  # ------------------------------------------------------------------------- #
  # INPUT
  # ------------------------------------------------------------------------- #

  FILE_INDICATORS_LONG <- "INDICATORS_PADIGITALE2026.json"
  FILE_INDICATORS_WIDE <- "INDICATORS_PADIGITALE2026_WIDE.json"
  FILE_METADATA <- "MET_INDICATORS_PADIGITALE2026.json"

  LOCAL_INDICATORS_LONG <- file.path(
    DIR_PAD26_DASH_INPUT_LOCAL,
    FILE_INDICATORS_LONG
  )
  LOCAL_INDICATORS_WIDE <- file.path(
    DIR_PAD26_DASH_INPUT_LOCAL,
    FILE_INDICATORS_WIDE
  )
  LOCAL_METADATA <- file.path(
    DIR_PAD26_DASH_INPUT_LOCAL,
    FILE_METADATA
  )

  LOCAL_INDICATORS_LONG <- download_and_check(
    drive_file_rel = file.path(
      DRIVE_PAD26_INDICATORS,
      FILE_INDICATORS_LONG
    ),
    local_path = LOCAL_INDICATORS_LONG,
    label = "Indicatori long"
  )

  LOCAL_INDICATORS_WIDE <- download_and_check(
    drive_file_rel = file.path(
      DRIVE_PAD26_INDICATORS,
      FILE_INDICATORS_WIDE
    ),
    local_path = LOCAL_INDICATORS_WIDE,
    label = "Indicatori wide"
  )

  LOCAL_METADATA <- download_and_check(
    drive_file_rel = file.path(
      DRIVE_PAD26_INDICATORS_MET,
      FILE_METADATA
    ),
    local_path = LOCAL_METADATA,
    label = "Metadati indicatori"
  )

  # ------------------------------------------------------------------------- #
  # RENDER
  # ------------------------------------------------------------------------- #

  OUTPUT_HTML_NAME <- paste0(
    "DASHBOARD_SIM_PADIGITALE2026_",
    RUN_ID_DASHBOARD,
    ".html"
  )

  OUTPUT_HTML <- file.path(
    DIR_PAD26_DASH_OUTPUT_LOCAL,
    OUTPUT_HTML_NAME
  )

  message("Rmd assoluto: ", FILE_RMD)
  message("Long assoluto: ", LOCAL_INDICATORS_LONG)
  message("Wide assoluto: ", LOCAL_INDICATORS_WIDE)
  message("Metadata assoluto: ", LOCAL_METADATA)

  rendered_file <- rmarkdown::render(
    input = FILE_RMD,
    output_file = basename(OUTPUT_HTML),
    output_dir = dirname(OUTPUT_HTML),
    params = list(
      file_indicatori_long = LOCAL_INDICATORS_LONG,
      file_indicatori_wide = LOCAL_INDICATORS_WIDE,
      file_metadata_indicatori = LOCAL_METADATA,
      run_id_indicatori = RUN_ID_INDICATORS,
      anno_nuts = ANNO_NUTS,
      risoluzione_nuts = RISOLUZIONE_NUTS
    ),
    envir = new.env(parent = globalenv()),
    knit_root_dir = getwd(),
    clean = TRUE,
    quiet = FALSE
  )

  assert_file_exists(rendered_file, "Dashboard HTML")

  # ------------------------------------------------------------------------- #
  # UPLOAD OUTPUT
  # ------------------------------------------------------------------------- #

  drive_upload_or_update(
    local_path = rendered_file,
    drive_folder_rel = DRIVE_PAD26_DASH_OUTPUT
  )

  message("Dashboard prodotta: ", rendered_file)
  message("Dashboard caricata in: ", DRIVE_PAD26_DASH_OUTPUT)

  status_run <- "completed"

}, error = function(e) {
  message("ERRORE durante il rendering dashboard: ", conditionMessage(e))
  status_run <<- "failed"
  stop(e)
}, finally = {
  console_log_path <- stop_console_log(
    console_log,
    status = status_run
  )

  drive_upload_or_update(
    local_path = console_log_path,
    drive_folder_rel = DRIVE_PAD26_LOGS
  )
})

if (delete_local_temp) {
  unlink(
    DIR_PAD26_DASH_LOCAL,
    recursive = TRUE
  )
}

message(
  "--- Dashboard PA digitale 2026 terminata. RUN_ID: ",
  RUN_ID_DASHBOARD,
  " | status: ",
  status_run,
  " ---"
)
