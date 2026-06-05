# OPENBDAP - CATALOGO OPEN DATA ======================================

rm(list=ls())

# 0) OPZIONI ----------------------------------------------------------

base_url  <- "https://bdap-opendata.rgs.mef.gov.it/catalog/?h=search0&search1&search2&search0&search1&search2&search0&search0&search1&search2&search3&search4&search5&search6&search7&search8&search9"
site_root <- "https://bdap-opendata.rgs.mef.gov.it"

sleep_sec <- 0.4
save_log  <- TRUE

output_dir <- file.path(getwd(), "data", "OpenBDAP", "catalogo")
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

# Questo script costruisce una tabella riassuntiva del catalogo OpenBDAP.
#
# Per ciascun risultato estrae:
# - titolo
# - descrizione
# - fonte
# - data creazione
# - tema
# - link "Visualizza"
# - link "Scarica"
#
# L'idea è usare questa tabella come catalogo sintetico dei dataset,
# per poi decidere in un secondo momento quali scaricare.


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

# La pagina mostra "Pagina 1 di 354", quindi la paginazione è ampia.
# Qui proviamo a ricavare l'ultima pagina dai link di paginazione.
# Se non ci riusciamo, puoi forzare manualmente max_pages.

get_last_page_index <- function(base_url) {
  
  doc <- read_html_safe(base_url)
  
  if (is.null(doc)) {
    stop("Impossibile leggere la pagina iniziale.")
  }
  
  hrefs <- html_elements(doc, "a[href*='/catalog/']") %>%
    html_attr("href")
  
  hrefs <- hrefs[!is.na(hrefs)]
  
  # cerca parametri ?page= o /page/N se presenti
  page_ids_qs <- str_extract(hrefs, "(?<=page=)\\d+") %>% as.integer()
  page_ids_pt <- str_match(hrefs, "/page/(\\d+)")[, 2] %>% as.integer()
  
  page_ids <- c(page_ids_qs, page_ids_pt)
  page_ids <- page_ids[!is.na(page_ids)]
  
  if (length(page_ids) == 0) {
    # fallback prudente: dalla pagina attuale sappiamo che il totale è 354
    return(354L)
  }
  
  max(page_ids)
}


# 5) COSTRUZIONE URL DI PAGINA ----------------------------------------

# Attenzione:
# alcuni cataloghi usano ?page=N, altri /page/N.
# Qui partiamo da ?page=N, che è l'ipotesi più probabile.
# Se non dovesse funzionare, va cambiata solo questa funzione.

build_page_url <- function(page_index, base_url) {
  
  if (page_index == 1L) {
    return(base_url)
  }
  
  sep <- ifelse(str_detect(base_url, "\\?"), "&", "?")
  paste0(base_url, sep, "page=", page_index)
}


# 6) ESTRAZIONE CAMPO LABEL -> VALUE ----------------------------------

extract_tag_value <- function(item, label_name) {
  
  blocks <- html_elements(item, ".search-item-tags")
  
  if (length(blocks) == 0) {
    return(NA_character_)
  }
  
  for (b in blocks) {
    label_txt <- html_element(b, ".search-item-label") %>%
      node_text_safe()
    
    if (!is.na(label_txt) &&
        str_detect(tolower(label_txt), fixed(tolower(label_name)))) {
      
      value_txt <- html_element(b, ".search-item-text, .taxonomy-result-term, a") %>%
        node_text_safe()
      
      return(value_txt)
    }
  }
  
  NA_character_
}


# 7) PARSING DI UN SINGOLO RISULTATO ----------------------------------

parse_catalog_item <- function(item, page_index) {
  
  # 7.1) TITOLO -------------------------------------------------------
  
  title_node <- html_element(item, "h3.title a")
  
  titolo <- node_text_safe(title_node)
  detail_url <- node_attr_safe(title_node, "href") %>% abs_url()
  
  
  # 7.2) DESCRIZIONE --------------------------------------------------
  
  descrizione <- html_element(item, ".search-snippet") %>%
    node_text_safe()
  
  
  # 7.3) METADATI -----------------------------------------------------
  
  fonte <- html_element(
    item,
    "[itemprop='author'] .author-label, [itemprop='author'] span[itemprop='name'], [itemprop='author'] a"
  ) %>%
    node_text_safe()
  
  data_creazione_chr <- extract_tag_value(item, "Data creazione")
  tema <- extract_tag_value(item, "Tema")
  
  
  # 7.4) LINK AZIONI --------------------------------------------------
  
  view_url <- html_element(item, ".search-result-view-link a, a[title='Visualizza']") %>%
    node_attr_safe("href") %>%
    abs_url()
  
  download_url <- html_element(item, ".search-result-download-link a, a[title='Scarica']") %>%
    node_attr_safe("href") %>%
    abs_url()
  
  
  # 7.5) OUTPUT ------------------------------------------------------
  
  tibble(
    page_index = page_index,
    titolo = titolo,
    descrizione = descrizione,
    fonte = fonte,
    data_creazione_chr = data_creazione_chr,
    tema = tema,
    view_url = view_url,
    download_url = download_url,
    detail_url = detail_url
  )
}


# 8) SCRAPING DI UNA SINGOLA PAGINA ----------------------------------

scrape_catalog_page <- function(page_index, base_url, sleep_sec = 0.4) {
  
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
        n_items = 0,
        n_records = 0,
        scraped_at = Sys.time()
      )
    ))
  }
  
  items <- html_elements(doc, "li.metadata-search-result")
  
  if (length(items) == 0) {
    cli_alert_warning(paste("Nessun risultato trovato nella pagina", page_index))
    
    return(list(
      data = tibble(),
      log = tibble(
        page_index = page_index,
        page_url = page_url,
        status = "no_items_found",
        n_items = 0,
        n_records = 0,
        scraped_at = Sys.time()
      )
    ))
  }
  
  page_data <- map_dfr(items, parse_catalog_item, page_index = page_index) %>%
    filter(
      !is.na(titolo),
      titolo != ""
    ) %>%
    distinct()
  
  page_log <- tibble(
    page_index = page_index,
    page_url = page_url,
    status = "ok",
    n_items = length(items),
    n_records = nrow(page_data),
    scraped_at = Sys.time()
  )
  
  list(
    data = page_data,
    log = page_log
  )
}


# 9) SCRAPING COMPLETO ------------------------------------------------

scrape_all_catalog <- function(base_url, sleep_sec = 0.4, max_pages = NULL) {
  
  if (is.null(max_pages)) {
    max_pages <- get_last_page_index(base_url)
  }
  
  cli_alert_success(paste("Numero pagine da processare:", max_pages))
  
  all_results <- map(
    1:max_pages,
    ~ scrape_catalog_page(
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


# 10) PULIZIA FINALE --------------------------------------------------

clean_final_catalog <- function(df) {
  
  df %>%
    mutate(
      titolo = txt_clean(titolo),
      descrizione = txt_clean(descrizione),
      fonte = txt_clean(fonte),
      tema = txt_clean(tema),
      data_creazione = suppressWarnings(dmy(data_creazione_chr))
    ) %>%
    select(
      page_index,
      titolo,
      descrizione,
      fonte,
      data_creazione,
      tema,
      view_url,
      download_url,
      detail_url
    ) %>%
    distinct()
}


# 11) TEST SU UNA PAGINA ----------------------------------------------

# Prima di lanciare tutto, conviene fare un test.
# Se questo blocco funziona, allora si passa a tutto il catalogo.

test_page <- scrape_catalog_page(
  page_index = 1,
  base_url = base_url,
  sleep_sec = 0
)

print(test_page$log)
print(test_page$data, n = 10, width = Inf)


# 12) ESECUZIONE COMPLETA ---------------------------------------------

# Quando il test della pagina 1 ti convince, scommenta:

results <- scrape_all_catalog(
  base_url = base_url,
  sleep_sec = sleep_sec,
  max_pages = 354
)

openbdap_raw <- results$data
scrape_log   <- results$log

openbdap_final <- clean_final_catalog(openbdap_raw)


# 13) CONTROLLI DI QUALITÀ --------------------------------------------

# Dopo l'esecuzione completa:

quality_summary <- openbdap_final %>%
  summarise(
    n_record = n(),
    n_titolo = sum(!is.na(titolo) & titolo != ""),
    n_descrizione = sum(!is.na(descrizione) & descrizione != ""),
    n_fonte = sum(!is.na(fonte) & fonte != ""),
    n_data_creazione = sum(!is.na(data_creazione)),
    n_tema = sum(!is.na(tema) & tema != ""),
    n_view_url = sum(!is.na(view_url) & view_url != ""),
    n_download_url = sum(!is.na(download_url) & download_url != "")
  )

print(quality_summary)
print(scrape_log, n = 20)
print(openbdap_final, n = 20, width = Inf)


# 14) SALVATAGGIO OUTPUT ----------------------------------------------

# Dopo l'esecuzione completa:

run_tag <- format(Sys.time(), "%Y%m%d_%H%M%S")

csv_data_path <- file.path(
  output_dir,
  paste0("openbdap_catalogo_", run_tag, ".csv")
)

xlsx_data_path <- file.path(
  output_dir,
  paste0("openbdap_catalogo_", run_tag, ".xlsx")
)

csv_log_path <- file.path(
  output_dir,
  paste0("openbdap_catalogo_log_", run_tag, ".csv")
)

readr::write_excel_csv(openbdap_final, csv_data_path)

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
openxlsx::write.xlsx(openbdap_final, xlsx_data_path)

if (save_log) {
  readr::write_excel_csv(scrape_log, csv_log_path)
}

cli_alert_success(paste("Catalogo CSV salvato in:", csv_data_path))
cli_alert_success(paste("Catalogo XLSX salvato in:", xlsx_data_path))

if (save_log) {
  cli_alert_success(paste("Log salvato in:", csv_log_path))
}
