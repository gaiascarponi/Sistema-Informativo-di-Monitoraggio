# OPENCIVITAS OPEN DATA ===============================================

# 0) OPZIONI ----------------------------------------------------------

rm(list=ls())
base_url  <- "https://www.opencivitas.it/it/open-data"
site_root <- "https://www.opencivitas.it"

sleep_sec <- 0.5
save_log  <- TRUE

# output_dir <- file.path(getwd(), "output", "OpenCivitas", "open_data")
output_dir <- file.path(getwd(), "data", "OpenCivitas")
output_dir <- path.expand(output_dir)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# 1) PACCHETTI --------------------------------------------------------

# install.packages(c(
#   "rvest", "xml2", "dplyr", "readr", "purrr",
#   "stringr", "tibble", "lubridate", "httr2", "cli"
# ))

library(rvest)
library(xml2)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(lubridate)
library(httr2)
library(cli)


# 2) OBIETTIVO DELLO SCRIPT -------------------------------------------

# Questo script serve a costruire una tabella riassuntiva dei dataset
# presenti nella sezione Open Data di OpenCivitas.
#
# Per ogni dataset vengono estratti:
# - nome
# - data di pubblicazione
# - descrizione
# - versione
# - link download CSV
# - link download PDF
# - link metadati enti
# - link metadati variabili/indicatori
# - link pagina dettaglio
#
# L'output finale è pensato come catalogo sintetico dei DB disponibili.
# In un secondo momento potrai usare i link salvati per scaricare
# solo i dataset che ti interessano.


# 3) FUNZIONI HELPER --------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

txt_clean <- function(x) {
  x %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

abs_url <- function(x, base = site_root) {
  ifelse(
    is.na(x) | x == "",
    NA_character_,
    url_absolute(x, base)
  )
}

read_html_safe <- function(url) {
  tryCatch(
    {
      read_html(url)
    },
    error = function(e) {
      cli_alert_danger(paste("Errore nel download/parsing della pagina:", url))
      return(NULL)
    }
  )
}

node_text_safe <- function(node) {
  tryCatch(
    html_text2(node) %>% txt_clean(),
    error = function(e) NA_character_
  )
}

node_attr_safe <- function(node, attr) {
  tryCatch(
    html_attr(node, attr),
    error = function(e) NA_character_
  )
}


# 4) RILEVAZIONE DEL NUMERO DI PAGINE ---------------------------------

# La prima pagina è:
#   /it/open-data
#
# Le pagine successive sono del tipo:
#   /it/open-data?page=1
#   /it/open-data?page=2
#   ...
#
# Questa funzione cerca il massimo indice di pagina presente
# nei link di paginazione.

get_last_page_index <- function(base_url) {
  
  doc <- read_html_safe(base_url)
  
  if (is.null(doc)) {
    stop("Impossibile leggere la pagina iniziale.")
  }
  
  hrefs <- html_elements(doc, "a[href*='open-data?page=']") %>%
    html_attr("href")
  
  hrefs <- hrefs[!is.na(hrefs)]
  
  page_ids <- str_extract(hrefs, "(?<=page=)\\d+") %>%
    as.integer()
  
  page_ids <- page_ids[!is.na(page_ids)]
  
  if (length(page_ids) == 0) {
    return(0L)
  }
  
  max(page_ids)
}


# 5) COSTRUZIONE URL DI PAGINA ----------------------------------------

build_page_url <- function(page_index, base_url) {
  if (page_index == 0L) {
    base_url
  } else {
    paste0(base_url, "?page=", page_index)
  }
}


# 6) ESTRAZIONE DI UN CAMPO LABEL -> VALUE ----------------------------

# Ogni dataset è rappresentato come una tabella HTML.
# Ogni informazione è in una riga:
# - colonna sinistra = label
# - colonna destra   = valore
#
# Questa funzione cerca una riga che contenga una certa label
# e restituisce il testo della cella valore.

extract_field_from_table <- function(tbl, label_name) {
  
  rows <- html_elements(tbl, "tr")
  
  for (r in rows) {
    label_txt <- html_element(r, "td.label strong") %>%
      node_text_safe()
    
    if (!is.na(label_txt) && str_detect(
      tolower(label_txt),
      fixed(tolower(label_name))
    )) {
      value_txt <- html_element(r, xpath = ".//td[2]") %>%
        node_text_safe()
      
      return(value_txt)
    }
  }
  
  NA_character_
}


# 7) PARSING DI UNA SINGOLA TABELLA DATASET ---------------------------

# Ogni dataset nella pagina corrisponde a una tabella HTML.
# Il titolo è nel caption.
# I download sono identificabili tramite classi specifiche.

parse_dataset_table <- function(tbl, page_index) {
  
  # 7.1) TITOLO E LINK DETTAGLIO --------------------------------------
  
  title_node <- html_element(tbl, "caption a")
  
  nome <- node_text_safe(title_node)
  detail_url <- node_attr_safe(title_node, "href") %>% abs_url()
  
  
  # 7.2) CAMPI PRINCIPALI ---------------------------------------------
  
  data_pubblicazione <- extract_field_from_table(tbl, "Data di pubblicazione")
  descrizione        <- extract_field_from_table(tbl, "Descrizione")
  versione           <- extract_field_from_table(tbl, "Versione")
  
  
  # 7.3) LINK DOWNLOAD ------------------------------------------------
  
  csv_url <- html_element(tbl, ".dati-csv a") %>%
    node_attr_safe("href") %>%
    abs_url()
  
  pdf_url <- html_element(tbl, ".dati-rdf a") %>%
    node_attr_safe("href") %>%
    abs_url()
  
  metadati_enti_url <- html_element(tbl, ".enti-xlsx a") %>%
    node_attr_safe("href") %>%
    abs_url()
  
  metadati_variabili_url <- html_element(tbl, ".indicatori-xlsx a") %>%
    node_attr_safe("href") %>%
    abs_url()
  
  
  # 7.4) OUTPUT -------------------------------------------------------
  
  tibble(
    page_index = page_index,
    nome = nome,
    data_pubblicazione_chr = data_pubblicazione,
    descrizione = descrizione,
    versione = versione,
    csv_url = csv_url,
    pdf_url = pdf_url,
    metadati_enti_url = metadati_enti_url,
    metadati_variabili_url = metadati_variabili_url,
    detail_url = detail_url
  )
}


# 8) SCRAPING DI UNA SINGOLA PAGINA ----------------------------------

# La pagina contiene più dataset, ciascuno in una tabella.
# Questa funzione:
# - scarica la pagina
# - individua le tabelle dei dataset
# - applica il parser a ogni tabella
# - restituisce sia i dati sia un log

scrape_open_data_page <- function(page_index, base_url, sleep_sec = 0.5) {
  
  page_url <- build_page_url(page_index, base_url)
  
  cli_alert_info(paste("Scarico pagina", page_index, "->", page_url))
  
  Sys.sleep(sleep_sec)
  
  doc <- read_html_safe(page_url)
  
  if (is.null(doc)) {
    return(list(
      data = tibble(),
      log = tibble(
        page_index = page_index,
        page_url = page_url,
        status = "error_read_html",
        n_tables = 0,
        n_records = 0,
        scraped_at = Sys.time()
      )
    ))
  }
  
  
  # 8.1) ESTRAZIONE TABELLE DATASET -----------------------------------
  
  tables <- html_elements(doc, "div.view-content table")
  
  
  # 8.2) GESTIONE PAGINE VUOTE O NON LETTE ----------------------------
  
  if (length(tables) == 0) {
    cli_alert_warning(paste("Nessuna tabella dataset trovata nella pagina", page_index))
    
    return(list(
      data = tibble(),
      log = tibble(
        page_index = page_index,
        page_url = page_url,
        status = "no_tables_found",
        n_tables = 0,
        n_records = 0,
        scraped_at = Sys.time()
      )
    ))
  }
  
  
  # 8.3) PARSING DELLE TABELLE ----------------------------------------
  
  page_data <- map_dfr(tables, parse_dataset_table, page_index = page_index)
  
  
  # 8.4) PULIZIA ------------------------------------------------------
  
  page_data <- page_data %>%
    filter(
      !is.na(nome),
      nome != "",
      !is.na(detail_url),
      str_detect(detail_url, "/it/dataset/")
    ) %>%
    distinct()
  
  
  # 8.5) LOG ----------------------------------------------------------
  
  page_log <- tibble(
    page_index = page_index,
    page_url = page_url,
    status = "ok",
    n_tables = length(tables),
    n_records = nrow(page_data),
    scraped_at = Sys.time()
  )
  
  list(
    data = page_data,
    log = page_log
  )
}


# 9) SCRAPING COMPLETO DI TUTTE LE PAGINE -----------------------------

# La funzione:
# - trova l'ultimo indice di pagina
# - itera su tutte le pagine
# - unisce tutti i record in una tabella finale
# - unisce il log di esecuzione

scrape_all_open_data <- function(base_url, sleep_sec = 0.5) {
  
  last_page <- get_last_page_index(base_url)
  
  cli_alert_success(
    paste("Ultimo page index rilevato:", last_page,
          "- pagine totali:", last_page + 1)
  )
  
  all_results <- map(
    0:last_page,
    ~ scrape_open_data_page(
      page_index = .x,
      base_url = base_url,
      sleep_sec = sleep_sec
    )
  )
  
  data_tbl <- map_dfr(all_results, "data")
  log_tbl  <- map_dfr(all_results, "log")
  
  list(
    data = data_tbl,
    log  = log_tbl
  )
}


# 10) PULIZIA FINALE E STANDARDIZZAZIONE ------------------------------

clean_final_dataset <- function(df) {
  
  df %>%
    mutate(
      nome = txt_clean(nome),
      descrizione = txt_clean(descrizione),
      versione = txt_clean(versione),
      data_pubblicazione = suppressWarnings(dmy(data_pubblicazione_chr))
    ) %>%
    select(
      page_index,
      nome,
      data_pubblicazione,
      descrizione,
      versione,
      csv_url,
      pdf_url,
      metadati_enti_url,
      metadati_variabili_url,
      detail_url
    ) %>%
    distinct()
}


# 11) ESECUZIONE PIPELINE ---------------------------------------------

results <- scrape_all_open_data(
  base_url  = base_url,
  sleep_sec = sleep_sec
)

open_data_raw <- results$data
scrape_log    <- results$log

open_data_final <- clean_final_dataset(open_data_raw)


# 12) CONTROLLI DI QUALITÀ --------------------------------------------

cli_alert_info(paste("Numero record estratti:", nrow(open_data_final)))

quality_summary <- open_data_final %>%
  summarise(
    n_record = n(),
    n_nome = sum(!is.na(nome) & nome != ""),
    n_data_pubblicazione = sum(!is.na(data_pubblicazione)),
    n_descrizione = sum(!is.na(descrizione) & descrizione != ""),
    n_versione = sum(!is.na(versione) & versione != ""),
    n_csv_url = sum(!is.na(csv_url) & csv_url != ""),
    n_pdf_url = sum(!is.na(pdf_url) & pdf_url != ""),
    n_metadati_enti_url = sum(!is.na(metadati_enti_url) & metadati_enti_url != ""),
    n_metadati_variabili_url = sum(!is.na(metadati_variabili_url) & metadati_variabili_url != "")
  )

print(quality_summary)
print(scrape_log, n = 20)
print(open_data_final, n = 20, width = Inf)


# 13) PARSING DEL CAMPO "NOME" ---------------------------------------

# Obiettivo:
# scomporre la variabile "nome" in componenti più analitiche:
# - anno
# - codice
# - ente / universo di riferimento
# - ambito
# - tipo
#
# Esempi:
# "2022 - Comuni - Rifiuti - Indicatori e determinanti"
# "2018 FC50A Comuni - Dati strutturali - Questionario"
# "13 FC10E Unioni - Dati contabili - Questionari"

open_data_final <- open_data_final %>%
  mutate(
    
    # 13.1) ESTRAZIONE ANNO GREZZO -----------------------------------
    
    # Prende 4 cifre iniziali, oppure 2 cifre iniziali se presenti
    anno_raw = str_extract(nome, "^\\d{4}|^\\d{2}(?=\\s)"),
    
    # 13.2) NORMALIZZAZIONE ANNO -------------------------------------
    
    anno = case_when(
      is.na(anno_raw) ~ NA_integer_,
      str_length(anno_raw) == 4 ~ as.integer(anno_raw),
      str_length(anno_raw) == 2 ~ as.integer(paste0("20", anno_raw)),
      TRUE ~ NA_integer_
    ),
    
    # 13.3) RIMOZIONE ANNO DAL NOME ----------------------------------
    
    nome_senza_anno = nome %>%
      str_remove("^\\d{4}\\s*") %>%
      str_remove("^\\d{2}\\s*") %>%
      str_trim(),
    
    # 13.4) RIMOZIONE EVENTUALE TRATTINO INIZIALE --------------------
    
    # Per gestire casi tipo:
    # "2022 - Comuni - Rifiuti - ..."
    # dopo la rimozione dell'anno resta:
    # "- Comuni - Rifiuti - ..."
    nome_senza_anno = nome_senza_anno %>%
      str_remove("^\\-\\s*") %>%
      str_trim(),
    
    # 13.5) SPLIT DEL NOME SU " - " ----------------------------------
    
    parts = str_split(nome_senza_anno, "\\s+-\\s+"),
    
    # 13.6) BLOCCO INIZIALE ------------------------------------------
    
    blocco_iniziale = map_chr(parts, ~ .x[1] %||% NA_character_),
    
    # 13.7) RICONOSCIMENTO CODICE ------------------------------------
    
    # Il codice viene valorizzato SOLO se il primo token del blocco
    # ha davvero forma di codice, ad es. FC10E, FC50A, FP20U.
    #
    # Non deve prendere la "C" di "Comuni".
    primo_token = str_extract(blocco_iniziale, "^[^\\s]+"),
    
    codice = case_when(
      str_detect(primo_token, "^[A-Z]{1,3}\\d{1,3}[A-Z]{0,2}$") ~ primo_token,
      TRUE ~ NA_character_
    ),
    
    # 13.8) ENTE / UNIVERSO DI RIFERIMENTO ---------------------------
    
    ente = case_when(
      !is.na(codice) ~ str_remove(blocco_iniziale, paste0("^", codice, "\\s+")) %>% str_trim(),
      TRUE ~ blocco_iniziale
    ),
    
    # 13.9) AMBITO ---------------------------------------------------
    
    ambito = map_chr(parts, ~ .x[2] %||% NA_character_),
    
    # 13.10) TIPO ----------------------------------------------------
    
    tipo = map_chr(parts, ~ .x[3] %||% NA_character_),
    
    # 13.11) EVENTUALI PARTI EXTRA -----------------------------------
    
    extra = map_chr(parts, ~ {
      if (length(.x) >= 4) {
        paste(.x[4:length(.x)], collapse = " - ")
      } else {
        NA_character_
      }
    })
  ) %>%
  select(
    everything(),
    anno,
    codice,
    ente,
    ambito,
    tipo,
    extra
  ) %>%
  select(
    -anno_raw,
    -nome_senza_anno,
    -parts,
    -blocco_iniziale,
    -primo_token
  )


# 14) SALVATAGGIO OUTPUT ----------------------------------------------

run_tag <- format(Sys.time(), "%Y%m%d_%H%M%S")

csv_data_path <- file.path(
  output_dir,
  paste0("opencivitas_open_data_catalogo_", run_tag, ".csv")
)

rds_data_path <- file.path(
  output_dir,
  paste0("opencivitas_open_data_catalogo_", run_tag, ".rds")
)

csv_log_path <- file.path(
  output_dir,
  paste0("opencivitas_open_data_log_", run_tag, ".csv")
)

write_csv(open_data_final, csv_data_path)
saveRDS(open_data_final, rds_data_path)

if (save_log) {
  readr::write_excel_csv(open_data_final, csv_data_path)
}

cli_alert_success(paste("Catalogo CSV salvato in:", csv_data_path))
cli_alert_success(paste("Catalogo RDS salvato in:", rds_data_path))

if (save_log) {
  cli_alert_success(paste("Log salvato in:", csv_log_path))
}



# 15) ESEMPI DI UTILIZZO ----------------------------------------------

# 15.1) Vedere i dataset più recenti
open_data_final %>%
  arrange(desc(data_pubblicazione)) %>%
  select(nome, data_pubblicazione, versione) %>%
  slice_head(n = 20)

# 15.2) Filtrare i dataset che hanno un CSV disponibile
open_data_final %>%
  filter(!is.na(csv_url)) %>%
  select(nome, csv_url)

# 15.3) Filtrare i dataset che hanno metadati enti
open_data_final %>%
  filter(!is.na(metadati_enti_url)) %>%
  select(nome, metadati_enti_url)

# 15.4) Filtrare i dataset che hanno metadati variabili
open_data_final %>%
  filter(!is.na(metadati_variabili_url)) %>%
  select(nome, metadati_variabili_url)