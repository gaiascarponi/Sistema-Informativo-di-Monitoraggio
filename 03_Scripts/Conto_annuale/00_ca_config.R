source("03_Scripts/00_sim_helpers.R")

# 1) PATH DIRECTORIES CONTO ANNUALE ------------------------------------------------------------

DRIVE_CA_SOURCE <- file.path(DRIVE_DIR_SOURCE, "Conto_annuale")

DRIVE_CA_PROCESSED <- file.path(DRIVE_DIR_PROCESSED, "Conto_annuale")

DRIVE_CA_INDICATORS <- file.path(DRIVE_DIR_INDICATORS, "Conto_annuale")

DRIVE_CA_OUTPUT <- file.path(DRIVE_DIR_OUTPUT, "Conto_annuale")

DRIVE_CA_LOGS <- file.path(DRIVE_DIR_LOGS, "Conto_annuale")

DRIVE_CA_DOCS <- file.path(DRIVE_DIR_DOCS, "Conto_annuale")

# DRIVE_CA_SOURCE_MET <- file.path(DIR_SOURCE_MET, "Conto_annuale")

DRIVE_CA_VARIABLES_MET <- file.path(DRIVE_DIR_METADATA, "Source_met", "Variables_met", "Conto_annuale")

DRIVE_CA_INDICATORS_MET <- file.path(DRIVE_DIR_METADATA, "Indicators_met", "Conto_annuale")

# 2) CREAZIONE CARTELLE DRIVE CONTO ANNUALE -------------------------------

cartelle_ca <- c(
  DRIVE_CA_SOURCE,
  DRIVE_CA_PROCESSED,
  DRIVE_CA_INDICATORS,
  DRIVE_CA_OUTPUT,
  DRIVE_CA_LOGS,
  DRIVE_CA_DOCS,
  DRIVE_CA_VARIABLES_MET,
  DRIVE_CA_INDICATORS_MET
)

purrr::walk(
  cartelle_ca,
  sim_drive_mkdir_path
)

message("Struttura Drive Conto Annuale verificata.")
