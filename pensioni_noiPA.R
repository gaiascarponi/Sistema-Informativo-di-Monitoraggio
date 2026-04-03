# DAG - SPESA PENSIONI NoiPA =====================================
rm(list=ls())

# 1) PACCHETTI ----
# install.packages(c("rvest", "xml2", "tibble", "dplyr", "stringr", "readr", "purrr", "openxlsx", "httr2"))

library(rvest)
library(xml2)
library(tibble)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(openxlsx)
library(httr2)

# 2) OPZIONI ----
load_data_from_local <- TRUE

cache_dir <- file.path(getwd(), "data", "MEF - Spesa Pensioni")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

options(timeout = 600)

# 3) URL BASE ----
download_page_url <- "https://datipensioni.mef.gov.it/datipensioni/download"
download_endpoint <- "https://datipensioni.mef.gov.it/datipensioni/downloadFile"

# 4) CATALOGO DATASET ----
catalogo_pensioni <- tibble::tibble(
  dataset_name = c(
    "Spesa pensioni - Amministrazione",
    "Spesa pensioni - Tipo Pensione"
  ),
  categoria = c(
    "oneri",
    "pensioni"
  ),
  filename = c(
    "Dati_Amministrazione_totale.csv",
    "Dati_Tipo_Pensione_totale.csv"
  ),
  formato = c("CSV", "CSV")
)

# 5) FUNZIONI DI SUPPORTO ----

read_html_safe <- function(url) {
  tryCatch(
    rvest::read_html(url),
    error = function(e) NULL
  )
}

clean_text <- function(x) {
  stringr::str_squish(x)
}

guess_delimiter <- function(file_path) {
  candidates <- c(";", ",", "\t", "|")
  
  ncols <- sapply(candidates, function(sep) {
    out <- tryCatch(
      suppressWarnings(
        readr::read_delim(
          file_path,
          delim = sep,
          n_max = 5,
          show_col_types = FALSE,
          progress = FALSE
        )
      ),
      error = function(e) NULL
    )
    
    if (is.null(out)) 0 else ncol(out)
  })
  
  candidates[which.max(ncols)]
}

# Estrae il testo della pagina
extract_download_page_text <- function(pg) {
  pg |>
    rvest::html_text2() |>
    clean_text()
}

# Estrae i metadati testuali dei due blocchi dalla pagina
extract_pensioni_page_metadata <- function(page_text) {
  
  block_amministrazione <- stringr::str_extract(
    page_text,
    "AMMINISTRAZIONE.*?TIPO PENSIONE"
  )
  
  block_tipo_pensione <- stringr::str_extract(
    page_text,
    "TIPO PENSIONE.*"
  )
  
  extract_single_block <- function(block, section_name) {
    if (is.na(block)) {
      return(
        tibble::tibble(
          dataset_name = section_name,
          descrizione = NA_character_,
          aggiornato_il = NA_character_,
          fonte = NA_character_
        )
      )
    }
    
    descrizione <- stringr::str_match(
      block,
      "Descrizione\\s*(.*?)\\s*Dati aggiornati al"
    )[, 2]
    
    aggiornato_il <- stringr::str_match(
      block,
      "Dati aggiornati al\\s*([0-9]{2}/[0-9]{2}/[0-9]{4})"
    )[, 2]
    
    fonte <- stringr::str_match(
      block,
      "Fonte dei dati\\s*(.*?)\\s*Licenza"
    )[, 2]
    
    tibble::tibble(
      dataset_name = section_name,
      descrizione = clean_text(descrizione),
      aggiornato_il = aggiornato_il,
      fonte = clean_text(fonte)
    )
  }
  
  dplyr::bind_rows(
    extract_single_block(block_amministrazione, "Spesa pensioni - Amministrazione"),
    extract_single_block(block_tipo_pensione, "Spesa pensioni - Tipo Pensione")
  )
}

# Scarica il CSV via POST oppure lo riusa da cache
download_pensioni_csv <- function(filename,
                                  categoria,
                                  cache_dir,
                                  load_data_from_local = TRUE) {
  
  local_path <- file.path(cache_dir, filename)
  
  if (!(load_data_from_local && file.exists(local_path))) {
    req <- httr2::request(download_endpoint) |>
      httr2::req_method("POST") |>
      httr2::req_body_form(
        filename = filename,
        categoria = categoria
      )
    
    httr2::req_perform(req, path = local_path)
  }
  
  delim <- guess_delimiter(local_path)
  
  df <- readr::read_delim(
    local_path,
    delim = delim,
    show_col_types = FALSE,
    progress = FALSE
  )
  
  list(
    data = tibble::as_tibble(df),
    local_path = local_path,
    delimiter = delim
  )
}

# Costruisce la tabella variabili
build_variables_table <- function(df, dataset_name, nome_file) {
  tibble::tibble(
    dataset_name = dataset_name,
    nome_file = nome_file,
    variabile = names(df),
    posizione_variabile = seq_along(names(df))
  )
}

# 6) SCRAPING METADATI PAGINA ----
pg <- read_html_safe(download_page_url)

if (is.null(pg)) {
  stop("Impossibile leggere la pagina del portale Spesa Pensioni.")
}

page_text <- extract_download_page_text(pg)
metadata_scraped <- extract_pensioni_page_metadata(page_text)

# 7) DOWNLOAD CSV E COSTRUZIONE OUTPUT ----
estrazioni_pensioni <- purrr::pmap(
  catalogo_pensioni,
  function(dataset_name, categoria, filename, formato) {
    
    page_meta <- metadata_scraped |>
      dplyr::filter(dataset_name == !!dataset_name)
    
    out <- tryCatch(
      download_pensioni_csv(
        filename = filename,
        categoria = categoria,
        cache_dir = cache_dir,
        load_data_from_local = load_data_from_local
      ),
      error = function(e) NULL
    )
    
    if (is.null(out)) {
      metadata_row <- tibble::tibble(
        `Dati pensioni NoiPA - dataset` = dataset_name,
        `periodo/annualità disponibili` = NA_character_,
        `ultimo aggiornamento disponibile` = page_meta$aggiornato_il,
        `variabili di interesse` = NA_character_,
        `n_osservazioni` = NA_integer_,
        `n_variabili` = NA_integer_,
        `modalità di accesso` = "POST a endpoint di download del portale",
        `limiti tecnici (rate limit)` = NA_character_,
        `formati scarico dati` = formato,
        `note` = paste0(
          "Descrizione: ", page_meta$descrizione,
          "; Fonte: ", page_meta$fonte,
          "; errore lettura dataset"
        )
      )
      
      return(list(
        metadata = metadata_row,
        variables = NULL
      ))
    }
    
    df <- out$data
    
    metadata_row <- tibble::tibble(
      `Dati pensioni NoiPA - dataset` = dataset_name,
      `periodo/annualità disponibili` = NA_character_,
      `ultimo aggiornamento disponibile` = page_meta$aggiornato_il,
      `variabili di interesse` = paste(names(df), collapse = " | "),
      `n_osservazioni` = nrow(df),
      `n_variabili` = ncol(df),
      `modalità di accesso` = "POST a endpoint di download del portale",
      `limiti tecnici (rate limit)` = NA_character_,
      `formati scarico dati` = formato,
      `note` = paste0(
        "Descrizione: ", page_meta$descrizione,
        "; Fonte: ", page_meta$fonte,
        "; separatore: ", out$delimiter
      )
    )
    
    variables_table <- build_variables_table(
      df = df,
      dataset_name = dataset_name,
      nome_file = filename
    )
    
    list(
      metadata = metadata_row,
      variables = variables_table
    )
  }
)

mappatura_pensioni <- purrr::map_dfr(estrazioni_pensioni, "metadata")
variabili_pensioni <- purrr::map_dfr(estrazioni_pensioni, "variables")

# 8) EXPORT EXCEL ----
wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#D9D9D9",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "bottom"
)

openxlsx::addWorksheet(wb, "mappatura_dataset")
openxlsx::writeData(
  wb,
  sheet = "mappatura_dataset",
  x = mappatura_pensioni,
  withFilter = TRUE
)
openxlsx::addStyle(
  wb,
  sheet = "mappatura_dataset",
  style = header_style,
  rows = 1,
  cols = 1:ncol(mappatura_pensioni),
  gridExpand = TRUE
)
openxlsx::freezePane(wb, "mappatura_dataset", firstRow = TRUE)
openxlsx::setColWidths(
  wb,
  sheet = "mappatura_dataset",
  cols = 1:ncol(mappatura_pensioni),
  widths = "auto"
)

openxlsx::addWorksheet(wb, "variabili")
openxlsx::writeData(
  wb,
  sheet = "variabili",
  x = variabili_pensioni,
  withFilter = TRUE
)
openxlsx::addStyle(
  wb,
  sheet = "variabili",
  style = header_style,
  rows = 1,
  cols = 1:ncol(variabili_pensioni),
  gridExpand = TRUE
)
openxlsx::freezePane(wb, "variabili", firstRow = TRUE)
openxlsx::setColWidths(
  wb,
  sheet = "variabili",
  cols = 1:ncol(variabili_pensioni),
  widths = "auto"
)

openxlsx::saveWorkbook(
  wb,
  file = "mappatura_spesa_pensioni.xlsx",
  overwrite = TRUE
)
