################################################################################
#                                 IMPORT
################################################################################
library(googledrive)
library(readr)
library(readxl)
library(dplyr)

################################################################################
#                             CONFIGURATIONS
################################################################################
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

temp_dir <- "07_Temp"
################################################################################
#                           IMPORT DATASET
################################################################################
file_CIG <- drive_ls(as_id("1uCyXCfMh-2da9AKRF73QbP_Okm8yQzhi")) %>% 
  filter(name == "CIG_2023.rds") 
file_raccordo <- drive_ls(as_id("15Y8dcyzbFOEdIJc0wRszx9uJT16kqyEs")) %>% 
  filter(name == "Lista_raccordo_SIM.xlsx")

#import Lista
path_lista <- file.path(temp_dir, "Lista_raccordo_SIM.xlsx")
drive_download(
  file = as_id(file_raccordo$id), 
  path = path_lista,
  overwrite = TRUE
)
lista <- read_excel(path_lista)
message("File Lista_raccordo_SIM.xlsx caricato correttamente")


#import CIG
path_cig <- file.path(temp_dir, "CIG_2023.rds")
drive_download(
  file = as_id(file_CIG$id), 
  path = path_cig,
  overwrite = TRUE
)
cig_2023 <- read_rds(path_cig)
message("File CIG_2023.rds caricato correttamente")

################################################################################
#                               PULIZIA
################################################################################
unlink(c(path_lista, path_cig))
# Verifica dimensioni
message("Righe caricatere in Lista: ", nrow(lista))
message("Righe caricate in CIG 2023: ", nrow(cig_2023))

rm(file_CIG, file_raccordo)
################################################################################
#                              LEFT JOIN
################################################################################

dataset_unito <- lista %>%
  left_join(cig_2023, by = c("codice_fiscale" = "cf_amministrazione_appaltante"))
message("Matchate: ", nrow(dataset_unito) , " gare")

################################################################################
#                              EXPORT RDS
################################################################################
id_destinazione <- as_id("1-P9OBSKJZr4EFhyXCIQJE39ERBoZmYGh")
nome_file_output <- "Master.rds"
path_temp <- file.path("07_Temp", nome_file_output)

# 1. Salvataggio locale in formato RDS
tryCatch({
  # Usiamo write_rds (pacchetto readr) o saveRDS (base R)
  # L'oggetto da salvare è 'dataset_unito'
  write_rds(dataset_unito, file = path_temp, compress = "gz")
  message("File RDS creato localmente in 07_Temp")
}, error = function(e) {
  stop("Errore durante la creazione del file RDS: ", e$message)
})

# 2. Caricamento su Google Drive
if (file.exists(path_temp)) {
  drive_upload(
    media = path_temp,
    path = id_destinazione,
    name = nome_file_output,
    overwrite = TRUE
  )
  
  # 3. Pulizia file temporaneo
  unlink(path_temp) 
  message("File Master.rds caricato correttamente su Drive")
} else {
  stop("Errore critico: Il file ", path_temp, " non esiste sul disco!")
}

