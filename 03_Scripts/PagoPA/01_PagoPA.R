################################################################################
#                                 IMPORT
################################################################################
library(googledrive)
library(purrr)
library(stringr)
library(readr)
library(dplyr)
library(readxl)
library(tidyr)

################################################################################
#                             CONFIGURATIONS
################################################################################
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

################################################################################
#                            IMPORT PagoPA
################################################################################
cartella_pagoPA <- drive_get(as_id("1qDUxN8X-dIhI6xzDpKzadWckfQOuerwK"))

file_disponibili <- drive_ls(cartella_pagoPA)

walk2(file_disponibili$id, file_disponibili$name, ~ {
  nome_variabile <- .y %>% 
    str_remove("\\.xlsx$") %>% 
    str_replace_all("[\\s-]+", "_")
    percorso_file <- file.path("07_Temp", .y)
  drive_download(as_id(.x), path = percorso_file, overwrite = TRUE, verbose = FALSE)
  dataset <- read_excel(percorso_file)
  assign(nome_variabile, dataset, envir = .GlobalEnv)
  message(paste("Scaricato in 07_Temp e caricato in R:", nome_variabile))
})

rm(file_disponibili, cartella_pagoPA)
################################################################################
#                          INDICATORI PagoPA
################################################################################
db1 <- IO_Distribuzione_geografica_enti_e_servizi[IO_Distribuzione_geografica_enti_e_servizi$categoria=="Comuni",]
db2 <- IO_Distribuzione_geografica_enti_e_servizi[IO_Distribuzione_geografica_enti_e_servizi$categoria=="Istruzione",]

db1 <- db1 %>% 
  mutate(
    ind1 = numero_enti,
    ind2 = numero_servizi ) %>% 
  select(-numero_enti, -numero_servizi, -categoria)

db2 <- db2 %>% 
  mutate(
    ind3 = numero_enti,
    ind4 = numero_servizi ) %>% 
  select(-numero_enti, -numero_servizi, -categoria)

db3 <- left_join(db1, db2, by = "regione")

################################################################################
db4 <- SEND_Distribuzione_geografica_dei_Comuni_su_SEND

db4 <- db4 %>% 
  mutate(
    ind5 = numero_comuni,
    ind6 = percentuale_comuni ) %>% 
  select(-numero_comuni, -percentuale_comuni)
  
db5 <- left_join(db3, db4, by = "regione")

indicatori_per_regioni <- db5
rm(SEND_Distribuzione_geografica_dei_Comuni_su_SEND, IO_Distribuzione_geografica_enti_e_servizi, db1, db2, db3, db4, db5)
################################################################################
db6 <- IO_Messaggi_inviati_da_servizi
db6 <- db6 %>% 
  mutate(
    ind1 = numero_messaggi ) %>% 
  select(-numero_messaggi)

db7 <- pagoPA_Distribuzione_mensile_del_numero_di_transazioni_per_categoria_di_ente_creditore
db8 <- db7 %>% 
  filter(categoria != "Tutte") %>% 
  pivot_wider(
    names_from = categoria,            
    values_from = numero_transazioni,  
    values_fill = 0                    
  )

db8 <- db8 %>% 
  mutate(
    ind2 = ACI,
    ind3 = Comuni,
    ind4 = `Consorzi universitari`,
    ind5 = `Enti comunali`,
    ind6 = `Enti provinciali`,
    ind7 = `Enti regionali`,
    ind8 = `Ordini, collegi e consigli professionali`,
    ind9 = Province,
    ind10 = `Pubbliche amministrazioni centrali`,
    ind11 = Regioni,
    ind12 = Ricerca,
    ind13 = `Salute centrale`,
    ind14 = `Salute locale`,
    ind15 = `Salute regionale`,
    ind16 = `Salute servizi`,
    ind17 = Scuola,
    ind18 = Università,
    ind19 = `Altri enti territoriali`,
    ind20 = Utility
  ) %>% 
  select(anno_mese, ind2:ind20)

db9 <- left_join(db6, db8, by = "anno_mese")

db10 <- SEND_Distribuzione_del_numero_di_notifiche %>% 
  mutate(
    ind21 = notifiche_analogiche,
    ind22 = notifiche_digitali,
    ind23 = notifiche_totali
  ) %>% 
  select(anno_mese, ind21:ind23)

db11 <- left_join(db9, db10, by = "anno_mese")
indicatori_per_tempo <- db11

rm(db6, db7, db8, db9, db10, db11, IO_Messaggi_inviati_da_servizi, pagoPA_Distribuzione_mensile_del_numero_di_transazioni_per_categoria_di_ente_creditore, SEND_Distribuzione_del_numero_di_notifiche)
################################################################################

db12 <- SEND_Distribuzione_dei_principali_ambiti_di_notifica %>% 
  mutate(
    ind1 = numero_notifiche,
  ) %>% 
  select(ambito, ind1)
indicatori_per_ambito <- db12

rm(db12, SEND_Distribuzione_dei_principali_ambiti_di_notifica)
################################################################################

db13 <- pagoPA_Distribuzione_del_numero_di_transazioni_per_fascia_di_importo_e_categoria_di_ente_creditore %>% 
  filter(categoria != "Tutte") %>% 
  pivot_wider(
    names_from = categoria,            
    values_from = numero_transazioni,  
    values_fill = 0                    
  )
db14 <- db13 %>% 
  mutate(
    ind1 = ACI,
    ind2 = Comuni,
    ind3 = `Consorzi universitari`,
    ind4 = `Enti comunali`,
    ind5 = `Enti provinciali`,
    ind6 = `Enti regionali`,
    ind7 = `Ordini, collegi e consigli professionali`,
    ind8 = Province,
    ind9 = `Pubbliche amministrazioni centrali`,
    ind10 = Regioni,
    ind11 = Ricerca,
    ind12 = `Salute centrale`,
    ind13 = `Salute locale`,
    ind14 = `Salute regionale`,
    ind15 = `Salute servizi`,
    ind16 = Scuola,
    ind17 = Università,
    ind18 = `Altri enti territoriali`,
    ind19 = Utility, 
    ind20 = `Enti donazioni`
  ) %>% 
  select(anno, fascia_importo, ind1:ind20)
indicatori_per_fascia_importo <- db14
rm(db13, db13a, db14, db15, db16, prova, pagoPA_Distribuzione_del_numero_di_transazioni_per_fascia_di_importo_e_categoria_di_ente_creditore)

################################################################################
#                       ESPORTAZIONE SU GOOGLE DRIVE
################################################################################
id_destinazione <- as_id("13TnQhe08KN5J4mZdG66WA1wQddFr8h-4")

dataset_da_caricare <- c(
  "indicatori_per_regioni",
  "indicatori_per_tempo",
  "indicatori_per_ambito",
  "indicatori_per_fascia_importo"
)

walk(dataset_da_caricare, function(nome_dataset) {
  dati <- get(nome_dataset)
  path_temp <- file.path(
    "07_Temp",
    paste0(nome_dataset, ".xlsx")
  )
  write_excel_csv2(dati, file = path_temp)
  drive_upload(
    media = path_temp,
    path = id_destinazione,
    name = paste0(nome_dataset, ".xlsx"),
    overwrite = TRUE
  )
  unlink(path_temp)
})

################################################################################
#                       PULIZIA 07_TEMP
################################################################################

file_xlsx <- list.files(
  path = "07_Temp",
  pattern = "\\.xlsx$",
  full.names = TRUE
)
file.remove(file_xlsx)
