# ============================================================
# 00_config.R
# Configurazioni comuni del progetto
# ============================================================

# ============================================================
# Google Drive
# ============================================================

# Google Drive folder IDs
DRIVE_ROOT_ID  <- "14jMYmLq78M-0LxuaIBAGao16ZhF59xDc"

# E-mail associato a Google Drive 
SIM_DRIVE_EMAIL <- "mipa.sistemainformativo@gmail.com"

# Local project paths

DRIVE_DIR_SOURCE    <- "01_Dataset/Source"
DRIVE_DIR_SOURCE_ANAC <-  "01_Dataset/Source/ANAC"
DRIVE_DIR_SOURCE_ANAC_GIC2023 <-  "01_Dataset/Source/ANAC/GIC 2023"


DRIVE_DIR_PROCESSED <- "01_Dataset/Processed"
DRIVE_DIR_PROCESSED_ANAC <- "01_Dataset/Processed/ANAC"
DRIVE_DIR_PROCESSED_ANAC_CIG <-  "01_Dataset/Processed/ANAC/GIC"

DRIVE_DIR_INDICATORS <- "01_Dataset/Indicators"
DRIVE_DIR_LISTS     <- "01_Dataset/Lists"
DRIVE_DIR_METADATA  <- "02_Metadata"
DIR_SOURCE_MET      <- "02_Metadata/Source_met"
DRIVE_DIR_OUTPUT    <- "04_Output"

# Conto annuale
DRIVE_DIR_LOGS      <- "05_Logs"
DRIVE_DIR_LOGS_CA   <- "05_Logs/Conto_annuale"

DRIVE_DIR_DOCS      <- "06_Docs"
DRIVE_DIR_DOCS      <- "06_Docs/Conto_annuale"

DRIVE_DIR_METADATA_CA  <- "02_Metadata/Conto_annuale"
DIR_SOURCE_MET_CA      <- "02_Metadata/Source_met/Conto_annuale"
DRIVE_DIR_OUTPUT_CA    <- "04_Output/Conto_annuale"

# ============================================================
# Path locali: solo cache/appoggio tecnico
# ============================================================

DIR_TEMP      <- "07_Temp"

# Create local folders if missing

dir.create(DIR_TEMP, recursive = TRUE, showWarnings = FALSE)
