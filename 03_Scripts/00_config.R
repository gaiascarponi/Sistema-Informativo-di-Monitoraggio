# ============================================================
# 00_config.R
# Configurazioni comuni del progetto
# ============================================================

# ============================================================
# Google Drive
# ============================================================

# Google Drive folder IDs
DRIVE_ROOT_ID  <- "14jMYmLq78M-0LxuaIBAGao16ZhF59xDc"

# Local project paths
DRIVE_DIR_SOURCE    <- "01_Dataset/Source"
DRIVE_DIR_PROCESSED <- "01_Dataset/Processed"
DRIVE_DIR_LISTS     <- "01_Dataset/Lists"
DRIVE_DIR_METADATA  <- "02_Metadata"
DIR_SOURCE_MET      <- "02_Metadata/Source_met"
DRIVE_DIR_OUTPUT    <- "04_Output"
DRIVE_DIR_LOGS      <- "05_Logs"
DRIVE_DIR_DOCS      <- "06_Docs"

# ============================================================
# Path locali: solo cache/appoggio tecnico
# ============================================================

DIR_TEMP      <- "07_Temp"

# Create local folders if missing

dir.create(DIR_TEMP, recursive = TRUE, showWarnings = FALSE)