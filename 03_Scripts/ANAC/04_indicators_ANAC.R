################################################################################
#                                 IMPORT
################################################################################
library(googledrive)
library(readr)
library(readxl)
library(tidyverse)
library(jsonlite)

################################################################################
#                          CONFIGURATION LOG 
################################################################################

#Recupero il nome dello script attuale
nome_script <- basename(rstudioapi::getActiveDocumentContext()$path) %>% 
  str_remove("\\.[rR]$") 

#Creo il nome del file log: log_NOMESCRIPT_YYYYMMDD.txt
data_oggi <- format(Sys.time(), "%Y%m%d")
log_filename <- paste0("log_", nome_script, "_", data_oggi, ".txt")

#Definisco il percorso locale 
if (!dir.exists("05_Logs/ANAC")) dir.create("05_Log/ANAC", recursive = TRUE)
log_path <- file.path("05_Logs/ANAC", log_filename)
#attivazione log
con <- file(log_path, open = "wt")
sink(con, type = "output")
sink(con, type = "message")

message("--- INIZIO ELABORAZIONE: ", Sys.time(), " ---")
message("Script in esecuzione: ", nome_script)

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
path_master <- file.path(temp_dir, "Master.rds")
drive_download(
  file = as_id(file_master$id),
  path = path_master,
  overwrite = TRUE
)
master <- read_rds(path_master)
message("File master.rds caricato correttamente")

################################################################################
#                       DATASET CLEANING
################################################################################

master_clean <- master %>%
  mutate(
    importo_lotto = as.numeric(gsub(",", ".", gsub("[^0-9,]", "", importo_lotto))),
    DURATA_PREVISTA = as.numeric(DURATA_PREVISTA),
    data_scadenza_offerta = as.Date(data_scadenza_offerta),
    DATA_COMUNICAZIONE_ESITO = as.Date(DATA_COMUNICAZIONE_ESITO),
    periodo = paste0(anno_pubblicazione, "-", sprintf("%02d", as.numeric(mese_pubblicazione)))
  )

master_matched <- master_clean %>% filter(!is.na(cig))
names(master_matched)
################################################################################
#                        INDICATORS_GLOBALE
################################################################################

pa_totali_n <- n_distinct(master_clean$codice_fiscale) # Totale MPA (es. 10179)
pa_matchate_n <- n_distinct(master_matched$codice_fiscale)

indicators_globale <- tibble(
  totale_pa_lista_mpa = pa_totali_n,
  ind1 = pa_matchate_n,                                  
  ind2 = round((pa_matchate_n / pa_totali_n) * 100, 2),  
  ind3 = nrow(master_matched),                          # NUMERO TOTALE GARE 2023
  ind4 = sum(master_matched$ESITO == "AGGIUDICATA", na.rm = TRUE), # Totale aggiudicazioni nazionali
  ind5 = round(ind3 / ind1, 2),                         # Media gare (totali) per PA
  ind6 = round(mean(master_matched$importo_lotto, na.rm = TRUE), 2), # Media GLOBALE importo lotto
  ind7 = round(mean(master_matched$DURATA_PREVISTA, na.rm = TRUE), 1), # Media GLOBALE durata
  ind8 = round(mean(as.numeric(difftime(master_matched$DATA_COMUNICAZIONE_ESITO,    # Tempo medio tra Scadenza e Esito
                                        master_matched$data_scadenza_offerta, 
                                        units = "days")), na.rm = TRUE), 1),
  ind9 = round(mean(as.numeric(difftime(master_matched$data_scadenza_offerta,   # Tempo medio tra Pubblicazione e Scadenza
                                        master_matched$data_pubblicazione, 
                                        units = "days")), na.rm = TRUE), 1),
  ind10 = round(ind4 / ind1, 2) #Numero medio di gare AGGIUDICATE 
)
################################################################################
#                        INDICATORS_ANNUALE
################################################################################

indicators_annuale <- master_matched %>%
  group_by(codice_fiscale, anno_pubblicazione) %>%
  summarise(
    ind11 = n(),                               # Numero gare per PA
    ind12 = sum(importo_lotto, na.rm = TRUE),  # Importo lotto per PA
    ind13 = mean(importo_lotto, na.rm = TRUE), # Importo lotto medio per PA
    ind14 = mean(DURATA_PREVISTA, na.rm = TRUE), # Durata prevista media per PA
    ind15 = sum(ESITO == "AGGIUDICATA", na.rm = TRUE), #numero gare aggiudicate per PA
    ind16 = round((ind14 / ind11) * 100, 2), #perc gare aggiudicate per PA
    ind17 = mean(as.numeric(difftime(DATA_COMUNICAZIONE_ESITO,  # Tempistica media per questa PA
                                                  data_scadenza_offerta, 
                                                  units = "days")), na.rm = TRUE),
    ind18 = mean(as.numeric(difftime(data_scadenza_offerta,  # Media giorni per partecipare offerta
                                                         data_pubblicazione, 
                                                         units = "days")), na.rm = TRUE),
    .groups = "drop"
  )
################################################################################
#                       3. DATASET: INDICATORS_MENSILE
################################################################################

indicators_mensile <- master_matched %>%
  group_by(codice_fiscale, anno_pubblicazione, mese_pubblicazione) %>%
  summarise(
    ind19 = n(),                                        # Numero gare per PA nel mese
    ind20 = sum(importo_lotto, na.rm = TRUE),           # Importo totale per PA nel mese
    ind21 = mean(importo_lotto, na.rm = TRUE),          # Importo medio per PA nel mese
    ind22 = mean(DURATA_PREVISTA, na.rm = TRUE),        # Durata media per PA nel mese
    ind23 = sum(ESITO == "AGGIUDICATA", na.rm = TRUE),  # Conteggio aggiudicazioni (ESITO == "AGGIUDICATA")
    ind24 = round((ind23 / ind19) * 100, 2),            # Percentuale gare aggiudicate nel mese (ind23 / ind19)
    ind25 = mean(as.numeric(difftime(DATA_COMUNICAZIONE_ESITO,  # Tempistica media comunicazione esito nel mese
                                     data_scadenza_offerta, 
                                     units = "days")), na.rm = TRUE),
    ind26 = mean(as.numeric(difftime(data_scadenza_offerta, # Media giorni per preparare l'offerta nel mese
                                     data_pubblicazione, 
                                     units = "days")), na.rm = TRUE),
    .groups = "drop"
  )


################################################################################
#                               Manipolazione
################################################################################

pa_uniche <- master_matched %>% 
  select(pa = codice_fiscale) %>% 
  distinct()
indicators_globale_final <- pa_uniche %>%
  cross_join(indicators_globale) 
indicators_globale_final <- subset(indicators_globale_final, 
                                   select = -totale_pa_lista_mpa)
ind01 <- indicators_globale_final %>%
  mutate(fil_anno = "2023") %>% 
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(
    fil = "fil_anno",
    sub_fil = NA_character_,     
    sub_fil_val = NA_character_  
  ) %>%
  rename(fil_val = fil_anno) %>%
  select(pa, fil, fil_val, sub_fil, sub_fil_val, ind, ind_val)


indicators_annuale_final <- indicators_annuale %>%
  rename(
    pa = codice_fiscale,
    fil_anno = anno_pubblicazione
  )
ind02 <- indicators_annuale_final %>%
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(
    fil = "fil_anno",
    sub_fil = NA_character_,    
    sub_fil_val = NA_character_
  ) %>%
  rename(fil_val = fil_anno) %>%
  select(pa, fil, fil_val, sub_fil, sub_fil_val, ind, ind_val)


indicators_mensile_final <- indicators_mensile %>%
  rename(
    pa = codice_fiscale,
    fil_anno = anno_pubblicazione,
    fil_mese = mese_pubblicazione
  )
ind03 <- indicators_mensile_final %>%
  pivot_longer(
    cols = starts_with("ind"), 
    names_to = "ind", 
    values_to = "ind_val"
  ) %>%
  mutate(
    fil = "fil_anno",
    sub_fil = "fil_mese"
  ) %>%
  rename(
    fil_val = fil_anno,
    sub_fil_val = fil_mese
  ) %>%
  relocate(pa, fil, fil_val, sub_fil, sub_fil_val, ind, ind_val)


#Append
db <- bind_rows(
  ind01, ind02, ind03
)

message("Dataset unico creato")

################################################################################
#                       ESPORTAZIONE SU GOOGLE DRIVE
################################################################################
id_destinazione <- as_id("1orS5j-XxGi5_v_Cb0MfFyoXavAlPsqFH")
nome_file_output <- "INDICATORS_ANAC.json"
path_temp <- file.path("07_Temp", nome_file_output)

tryCatch({
  write_json(db, path = path_temp, pretty = TRUE, dataframe = "rows")
}, error = function(e) {
  stop("Errore durante la creazione del file JSON: ", e$message)
})

#Carico su drive
if (file.exists(path_temp)) {
  drive_upload(
    media = path_temp,
    path = id_destinazione,
    name = nome_file_output,
    overwrite = TRUE
  )
  unlink(path_temp) 
} else {
  stop("Errore critico: Il file ", path_temp, " non esiste sul disco!")
}
message("Files caricati correttamente su Drive")

################################################################################
#                       PULIZIA 07_TEMP
################################################################################

file_xlsx <- list.files(
  path = "07_Temp",
  pattern = "\\.xlsx$",
  full.names = TRUE
)
file_rds <- list.files(
  path = "07_Temp",
  pattern = "\\.rds$",
  full.names = TRUE
)
file.remove(file_xlsx)
file.remove(file_rds)

message("Rimossi i files in 07_Temp")

message("--- FINE ELABORAZIONE: ", Sys.time(), " ---")

# Chiudiamo registrazione del log
sink(type = "message")
sink(type = "output")
close(con)

# Carica il LOG anche su Drive
id_cartella_log_drive <- as_id("1sZo_8mL2qSMk50_qOoOb1nfk9Bu6KOn0") 
drive_upload(
  media = log_path,
  path = id_cartella_log_drive,
  name = log_filename
)


