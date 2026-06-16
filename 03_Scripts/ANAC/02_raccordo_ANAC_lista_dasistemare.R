################################################################################
#                                 IMPORT
################################################################################
library(googledrive)
library(purrr)
library(stringr)
library(readr)
library(dplyr)
library(readxl)

################################################################################
#                             CONFIGURATIONS
################################################################################
drive_auth(scopes = "https://www.googleapis.com/auth/drive")


################################################################################
#                           IMPORT DATASET
###############################################################################
DRIVE_ROOT_ID  <- "14jMYmLq78M-0LxuaIBAGao16ZhF59xDc" # da eliminare alla fine
DRIVE_DIR_LISTS     <- "01_Dataset/Lists"
DRIVE_DIR_PROCESSED_ANAC <- "01_Dataset/Processed/ANAC"
DRIVE_DIR_PROCESSED_ANAC_CIG <-  file.path(DRIVE_DIR_PROCESSED_ANAC, "CIG")

file_lista <- drive_ls(
  path = DRIVE_DIR_LISTS,
  pattern = "Lista_raccordo_SIM.xlsx$"  
)
file_CIG_2023 <- drive_ls(
  path = DRIVE_DIR_PROCESSED_ANAC_CIG,
  pattern = "CIG_2023.xlsx$"  
)

drive_download(
  file = file_lista$id,
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE
) -> temp_list
drive_download(
  file = file_CIG_2023$id, 
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE
) -> temp_cig_2023

lista <- read_excel(temp_list$local_path)
cig_2023 <- read_excel(temp_cig_2023$local_path)

################################################################################
#                           IMPORT DATASET
###############################################################################


class(cig_2023$cf_amministrazione_appaltante)
class(cig_2023$cig)


class(lista$codice_fiscale)
