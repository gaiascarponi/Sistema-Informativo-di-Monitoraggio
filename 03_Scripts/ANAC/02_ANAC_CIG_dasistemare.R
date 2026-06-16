################################################################################
#                                 IMPORT
################################################################################
library(googledrive)
library(purrr)
library(stringr)
library(readr)
library(dplyr)

################################################################################
#                             CONFIGURATIONS
################################################################################
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

################################################################################
#                              IMPORT CIG
###############################################################################
cartella_cig2023 <- drive_get(as_id("11IUYIsWbSqDGRPJ6HXFI6ooI-yO36hiz"))

# Cicla e trova tutti i mesi di CIG 2023
ricerche <- map(1:12, ~ list(
  pattern = str_glue("cig_csv_2023_{str_pad(.x, 2, pad = '0')}.csv"),
  id_cartella = cartella_cig2023$id
)) %>% 
  set_names(str_glue("cig_{str_pad(1:12, 2, pad = '0')}"))

# Cosa ha trovato? 
risultati <- map(ricerche, ~ drive_find(
  pattern = .x$pattern,
  q = str_glue("'{.x$id_cartella}' in parents"),
  n_max = 1
))

# Creiamo i nomi standardizzati per i file 
mesi <- str_pad(1:12, 2, pad = "0")

# Facciamo sia download sia la lettura in un colpo solo
walk(mesi, function(m) {
  path_locale <- str_glue("07_Temp/cig_csv_2023_{m}.csv")
  nome_oggetto <- str_glue("cig_{m}") 
  # Download da Google Drive
  drive_download(
    file = risultati[[nome_oggetto]],
    path = path_locale,
    overwrite = TRUE
  )
  # Lettura del file (con col_types per evitare warning e mutate per aggiungere il mese)
  df_mese <- read_csv2(path_locale, col_types = cols(.default = "c")) %>% 
    mutate(mese = m)
  assign(nome_oggetto, df_mese, envir = .GlobalEnv)
})

# Rimozione dei file CSV dalla cartella locale per non occupare spazio
file_da_cancellare <- str_glue("07_Temp/cig_csv_2023_{mesi}.csv")
unlink(file_da_cancellare)
# Pulizia environment R-Studio 
rm(risultati, ricerche)

################################################################################
#                        MANIPULATION CIG (APPEND)
###############################################################################

nomi_dataset <- str_glue("cig_{mesi}")
dataset_puliti <- mget(nomi_dataset) %>% 
  map(~ mutate(.x, DURATA_PREVISTA = as.character(DURATA_PREVISTA)))
cig_2023 <- bind_rows(dataset_puliti)
rm(list = c(nomi_dataset, "mesi", "nomi_dataset", "dataset_puliti"))

################################################################################
#                       ESPORTAZIONE SU GOOGLE DRIVE
################################################################################
id_destinazione <- as_id("1-P9OBSKJZr4EFhyXCIQJE39ERBoZmYGh")

path_output_temp <- "07_Temp/cig_2023.csv"
write_excel_csv2(cig_2023, file = path_output_temp)
drive_upload(
  media = path_output_temp,
  path = id_destinazione,
  name = "cig_2023.csv",          # nome su Drive
  overwrite = TRUE                # se esiste già, lo sovrascrive
)

unlink(path_output_temp)
