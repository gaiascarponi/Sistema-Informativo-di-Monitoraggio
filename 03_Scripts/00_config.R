# ============================================================
# 00_config.R
# Configurazioni comuni del progetto
# ============================================================

# Google Drive folder IDs
DRIVE_ROOT_ID  <- "1-fv7jTXnJVkEQLtJ9fNXBmzstvSD33iA"
DRIVE_LISTS_ID <- "15Y8dcyzbFOEdIJc0wRszx9uJT16kqyEs"

# Local project paths
DIR_SOURCE    <- file.path("01_Dataset", "Source")
DIR_PROCESSED <- file.path("01_Dataset", "Processed")
DIR_LISTS     <- file.path("01_Dataset", "Lists")
DIR_METADATA  <- "02_Metadata"
DIR_SOURCE_MET <- file.path("02_Metadata", "Source_met")
DIR_SCRIPTS   <- "03_Scripts"
DIR_OUTPUT    <- "04_Output"
DIR_LOGS      <- "05_Logs"
DIR_DOCS      <- "06_Docs"
DIR_TEMP      <- "07_Temp"


# Create local folders if missing
dir.create(DIR_SOURCE, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PROCESSED, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_LISTS, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_METADATA, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_SOURCE_MET, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_SCRIPTS, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUTPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_LOGS, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_DOCS, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TEMP, recursive = TRUE, showWarnings = FALSE)