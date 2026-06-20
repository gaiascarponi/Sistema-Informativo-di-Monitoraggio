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
file_master <- drive_ls(as_id("1-P9OBSKJZr4EFhyXCIQJE39ERBoZmYGh")) %>% 
  filter(name == "Master.rds")

#import master
path_master <- file.path(temp_dir, "CIG_2023.rds")
drive_download(
  file = as_id(file_master$id),
  path = path_master,
  overwrite = TRUE
)
master <- read_rds(path_master)
message("File CIG_2023.rds caricato correttamente")

################################################################################
#                       INDICATORS CREATION
################################################################################


################################################################################
#                   CREAZIONE INDICATORS DAL FILE MASTER
################################################################################
pa_totali_n <- n_distinct(master$codice_fiscale)
pa_matchate_n <- n_distinct(master$codice_fiscale[!is.na(master$cig)])
percentuale_totale <- round((pa_matchate_n / pa_totali_n) * 100, 2)

indicators <- master %>%
  filter(!is.na(cig)) %>% 
  group_by(codice_fiscale) %>% 
  summarise(
    ind1 = pa_matchate_n,        #Numero totale di PA matchate
    ind2 = percentuale_totale,   #Percentuale di PA matchate rispetto al totale (10179 lista MPA)
    ind3 = n(),                  #Numero di gare per ogni PA
    ind4 = sum(as.numeric(importo_lotto), na.rm = TRUE) #Somma degli importi per questa PA
  ) %>%
  ungroup()

################################################################################
#                               VERIFICA
################################################################################


head(indicators)




