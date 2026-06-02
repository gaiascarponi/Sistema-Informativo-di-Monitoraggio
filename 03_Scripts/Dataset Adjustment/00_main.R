################################################################################
#                                 IMPORT
################################################################################
library(readxl) 
library(googledrive)
library(dplyr)
library(writexl)

################################################################################
#                             CONFIGURATIONS
################################################################################
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

################################################################################
#                              IMPORT LISTS
###############################################################################
cartella_liste <- drive_get(as_id("15Y8dcyzbFOEdIJc0wRszx9uJT16kqyEs"))

file_s13 <- drive_find(
  pattern = "11 05 2026 Lista S13_2025.xlsx",
  q = paste0("'", cartella_liste$id, "' in parents"),
  n_max = 1
)
file_MPA <- drive_find(
  pattern = "11 05 2026 Lista MPA_2025.xlsx",
  q = paste0("'", cartella_liste$id, "' in parents"),
  n_max = 1
)
file_anagrafe <- drive_find(
  pattern = "Anagrafe-Enti-BDAP.xlsx",
  q = paste0("'", cartella_liste$id, "' in parents"),
  n_max = 1
)

# crea cartella temporanea se non esiste
dir.create("07_Temp", showWarnings = FALSE, recursive = TRUE)

#file temporanei locali
drive_download(file_s13, path = "07_Temp/lista_s13.xlsx", overwrite = TRUE)
drive_download(file_MPA, path = "07_Temp/lista_MPA.xlsx", overwrite = TRUE)
drive_download(file_anagrafe, path = "07_Temp/lista_anag.xlsx", overwrite = TRUE)

#caricamento su R
s13 <- read_excel("07_Temp/lista_s13.xlsx")
MPA <- read_excel("07_Temp/lista_MPA.xlsx")
BDAP <- read_excel("07_Temp/lista_anag.xlsx")

rm(file_MPA, file_s13, file_anagrafe, cartella_liste)
################################################################################
#                           LISTS MANIPULATION
###############################################################################
# Creazione variabili indicatrici
MPA$MPA_ind <- 1
s13$s13_ind <- 1

# Primo Match: s13 + MPA
s13_MPA <- s13 %>% 
  left_join(MPA, by = c("CODICE_FISCALE", "CODICE_REG", "RAGIONE_SOCIALE", "FG"))

# Uniformiamo i nomi delle colonne chiave di BDAP per il secondo match
BDAP$CODICE_FISCALE <- BDAP$CF
BDAP$CODICE_REG <- BDAP$Codice_Regione

# Secondo Match: s13_MPA + BDAP
s13_MPA_BDAP <- s13_MPA %>% 
  left_join(BDAP, by = c("CODICE_FISCALE", "CODICE_REG"))

# Creazione nuove variabili rinominate
s13_MPA_BDAP$CODICE_UNITA_S13 <- s13_MPA_BDAP$CODICE_UNITA
s13_MPA_BDAP$CODICE_UNITA_MPA <- s13_MPA_BDAP$CODICE_UNITA_UG
s13_MPA_BDAP$ATECO_BDAP <- s13_MPA_BDAP$Codice_ATECO

# Rimozione delle vecchie variabili doppie o inutili
lista <- s13_MPA_BDAP %>% 
  select(-CODICE_REG, -CF, -DESC_FG, -FLAG_S13, -STATO, -CODICE_UNITA, -CODICE_UNITA_UG, -Codice_ATECO)

################################################################################
#                               EXPORT TO DRIVE
################################################################################
write_xlsx(lista, path = "07_Temp/dataset_esportato.xlsx")

drive_upload(
  media = "07_Temp/dataset_esportato.xlsx",
  path = as_id("15Y8dcyzbFOEdIJc0wRszx9uJT16kqyEs"), 
  name = "lista.xlsx"             
)

cancella <- c(
  "07_Temp/lista_s13.xlsx",
  "07_Temp/lista_MPA.xlsx",
  "07_Temp/lista_anag.xlsx",
  "07_Temp/dataset_esportato.xlsx"
)
file.remove(cancella)
