# OPENBDAP - ANAGRAFE ENTI =================================

# 0) OPZIONI ----
load_data_from_local <- TRUE

cache_dir <- "~/Desktop/cache_open_data_pa"
cache_dir <- path.expand(cache_dir)

if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}


# 1) PACCHETTI ----
# install.packages(c("rvest", "xml2", "tibble", "dplyr", "stringr", "readr", "openxlsx", "purrr"))

library(rvest)
library(xml2)
library(tibble)
library(dplyr)
library(stringr)
library(readr)
library(openxlsx)
library(purrr)

options(timeout = 600)

# 2) CATALOGO DATASET ----
# Catalogo semi-manuale:
# - detail_url: pagina del dataset
# - request_url: endpoint XHR osservato nel portale
# - csv_url: URL finale del CSV
#
# Nota:
# request_url e csv_url sono stati recuperati manualmente tramite DevTools.
catalogo_openbdap <- tibble::tibble(
  dataset_name = c(
    "Anagrafe Enti - Organismo Strumentale",
    "Anagrafe Enti - Ente",
    "Anagrafe Enti - Mail Ente",
    "Anagrafe Enti - Classificazione ISTAT S13",
    "Anagrafe Enti - Partecipazioni Ente",
    "Anagrafe Enti - Classificazione SIOPE",
    "Anagrafe Enti - Classificazione MIUR",
    "Anagrafe Enti - Unità Organizzativa",
    "Anagrafe Enti - Classificazione D.lgs. 118-2011",
    "Anagrafe Enti - Eventi Ente ISTAT S13"
  ),
  detail_url = c(
    "https://bdap-opendata.rgs.mef.gov.it/content/anagrafica-enti-organismo-strumentale?t=Scarica",
    "https://bdap-opendata.rgs.mef.gov.it/content/anagrafica-enti-ente?t=Scarica",
    "https://bdap-opendata.rgs.mef.gov.it/content/anagrafica-enti-mail-ente?t=Scarica",
    "https://bdap-opendata.rgs.mef.gov.it/content/anagrafica-enti-classificazione-istat-s13?t=Scarica",
    "https://bdap-opendata.rgs.mef.gov.it/content/anagrafica-enti-partecipazioni-ente?t=Scarica",
    "https://bdap-opendata.rgs.mef.gov.it/content/anagrafica-enti-classificazione-siope?t=Scarica",
    "https://bdap-opendata.rgs.mef.gov.it/content/anagrafica-enti-classificazione-miur?t=Scarica",
    "https://bdap-opendata.rgs.mef.gov.it/content/anagrafica-enti-unit%C3%A0-organizzativa?t=Scarica",
    "https://bdap-opendata.rgs.mef.gov.it/content/anagrafica-enti-classificazione-dlgs-118-2011?t=Scarica",
    "https://bdap-opendata.rgs.mef.gov.it/content/anagrafica-enti-evento-enti?t=Scarica"
  ),
  request_url = c(
    "https://bdap-opendata.rgs.mef.gov.it/metadata_download_page/34884/csv/1928/14b0a4db-3991-4852-a7cf-0e3c5dcb010e@rgs",
    "https://bdap-opendata.rgs.mef.gov.it/metadata_download_page/34883/csv/1686/c5638e9f-8613-4e6d-9e92-25412f464f85@rgs",
    "https://bdap-opendata.rgs.mef.gov.it/metadata_download_page/34882/csv/1683/484fae8b-be5d-4c25-9d40-e941bace9610@rgs",
    "https://bdap-opendata.rgs.mef.gov.it/metadata_download_page/34881/csv/1930/e9f1c6b9-6372-4589-af57-e0a914bd75b1@rgs",
    "https://bdap-opendata.rgs.mef.gov.it/metadata_download_page/34880/csv/1684/7c278807-4f08-456e-b53b-5cd69bfd50cd@rgs",
    "https://bdap-opendata.rgs.mef.gov.it/metadata_download_page/34879/csv/1681/b395a5e5-5f28-4074-a416-d019c059f4b6@rgs",
    "https://bdap-opendata.rgs.mef.gov.it/metadata_download_page/34878/csv/1680/14bf15f6-8b35-4bd6-a70f-4c0d37a495c1@rgs",
    "https://bdap-opendata.rgs.mef.gov.it/metadata_download_page/34877/csv/1687/36783f75-979b-450d-9638-66d2ff54dea3@rgs",
    "https://bdap-opendata.rgs.mef.gov.it/metadata_download_page/34876/csv/1929/2856be41-d4f1-4086-81c6-94b3a756e02c@rgs",
    "https://bdap-opendata.rgs.mef.gov.it/metadata_download_page/34875/csv/1682/325e8daa-9661-4ff0-b43d-52431518e41a@rgs"
  ),
  csv_url = c(
    "https://bdap-opendata.rgs.mef.gov.it/export/csv/Anagrafe-Enti---Organismo-Strumentale.csv",
    "https://bdap-opendata.rgs.mef.gov.it/export/csv/Anagrafe-Enti---Ente.csv",
    "https://bdap-opendata.rgs.mef.gov.it/export/csv/Anagrafe-Enti---Mail-Ente.csv",
    "https://bdap-opendata.rgs.mef.gov.it/export/csv/Anagrafe-Enti---Classificazione-ISTAT-S13.csv",
    "https://bdap-opendata.rgs.mef.gov.it/export/csv/Anagrafe-Enti---Partecipazioni-Ente.csv",
    "https://bdap-opendata.rgs.mef.gov.it/export/csv/Anagrafe-Enti---Classificazione-SIOPE.csv",
    "https://bdap-opendata.rgs.mef.gov.it/export/csv/Anagrafe-Enti---Classificazione-MIUR.csv",
    "https://bdap-opendata.rgs.mef.gov.it/export/csv/Anagrafe-Enti---Unita-Organizzativa.csv",
    "https://bdap-opendata.rgs.mef.gov.it/export/csv/Anagrafe-Enti---Classificazione-Dlgs-118-2011.csv",
    "https://bdap-opendata.rgs.mef.gov.it/export/csv/Anagrafe-Enti---Eventi-Ente-ISTAT-S13.csv"
  ),
  formato = rep("CSV", 10),
  note_source = rep(
    "request_url e csv_url recuperati manualmente dalla request/response XHR del portale",
    10
  )
)


# 3) FUNZIONI DI SUPPORTO ----
# Estrae dalla pagina di dettaglio:
# - fonte
# - data di ultimo aggiornamento disponibile
extract_openbdap_page_metadata <- function(detail_url) {
  pg <- rvest::read_html(detail_url)
  
  page_text <- pg |>
    rvest::html_text2()
  
  lines <- unlist(strsplit(page_text, "\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  
  idx_fonte <- which(tolower(lines) == "fonte")
  idx_agg <- which(toupper(lines) == "AGGIORNATO IL")
  
  fonte <- if (length(idx_fonte) > 0 && idx_fonte[1] < length(lines)) {
    lines[idx_fonte[1] + 1]
  } else {
    NA_character_
  }
  
  aggiornato_il <- if (length(idx_agg) > 0 && idx_agg[1] < length(lines)) {
    lines[idx_agg[1] + 1]
  } else {
    NA_character_
  }
  
  tibble::tibble(
    fonte = fonte,
    aggiornato_il = aggiornato_il
  )
}

safe_extract_openbdap_page_metadata <- function(detail_url) {
  tryCatch(
    extract_openbdap_page_metadata(detail_url),
    error = function(e) {
      tibble::tibble(
        fonte = NA_character_,
        aggiornato_il = NA_character_
      )
    }
  )
}

# Scarica e legge il CSV finale
read_openbdap_csv <- function(csv_url, delim = ";", cache_dir = NULL, load_data_from_local = TRUE) {
  file_name <- basename(csv_url)

  if (!is.null(cache_dir)) {
    local_path <- file.path(cache_dir, file_name)
  } else {
    local_path <- tempfile(fileext = ".csv")
  }

  # Se il file esiste già e vogliamo riusarlo, non riscarichiamo
  if (!(load_data_from_local && file.exists(local_path))) {
    utils::download.file(
      url = csv_url,
      destfile = local_path,
      mode = "wb",
      method = "libcurl",
      quiet = FALSE
    )
  }

  df <- readr::read_delim(
    file = local_path,
    delim = delim,
    show_col_types = FALSE,
    progress = FALSE
  )

  list(
    data = tibble::as_tibble(df),
    local_path = local_path
  )
}


# 4) ESTRAZIONE DATASET E COSTRUZIONE OUTPUT ----
# Per ogni dataset:
# - scarica il CSV
# - legge i metadati dalla pagina web
# - costruisce una riga del catalogo finale
# - costruisce la tabella long delle variabili
estrazioni_openbdap <- purrr::pmap(
  catalogo_openbdap,
  function(dataset_name, detail_url, request_url, csv_url, formato, note_source) {
    
    out <- tryCatch(
      read_openbdap_csv(
        csv_url = csv_url,
        delim = ";",
        cache_dir = cache_dir,
        load_data_from_local = load_data_from_local
      ),
      error = function(e) NULL
    )
    
    page_meta <- safe_extract_openbdap_page_metadata(detail_url)
    
    if (is.null(out)) {
      metadata_row <- tibble::tibble(
        `Open Bdap - Anagrafe enti della PA - dataset` = dataset_name,
        `periodo/annualità disponibili` = NA_character_,
        `ultimo aggiornamento disponibile` = page_meta$aggiornato_il,
        `variabili di interesse` = NA_character_,
        `n_osservazioni` = NA_integer_,
        `n_variabili` = NA_integer_,
        `modalità di accesso` = "Download diretto CSV",
        `limiti tecnici (rate limit)` = NA_character_,
        `formati scarico dati` = formato,
        `note` = paste0(
          "Fonte: ", page_meta$fonte,
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
      `Open Bdap - Anagrafe enti della PA - dataset` = dataset_name,
      `periodo/annualità disponibili` = NA_character_,
      `ultimo aggiornamento disponibile` = page_meta$aggiornato_il,
      `variabili di interesse` = paste(names(df), collapse = " | "),
      `n_osservazioni` = nrow(df),
      `n_variabili` = ncol(df),
      `modalità di accesso` = "Download diretto CSV",
      `limiti tecnici (rate limit)` = NA_character_,
      `formati scarico dati` = formato,
      `note` = paste0(
        "Fonte: ", page_meta$fonte
      )
    )
    
    variables_table <- tibble::tibble(
      dataset_name = dataset_name,
      nome_file = basename(csv_url),
      variabile = names(df),
      posizione_variabile = seq_along(names(df))
    )
    
    list(
      metadata = metadata_row,
      variables = variables_table
    )
  }
)

mappatura_openbdap <- purrr::map_dfr(estrazioni_openbdap, "metadata")
variabili_openbdap <- purrr::map_dfr(estrazioni_openbdap, "variables")


# 5) EXPORT EXCEL ----
wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  fontColour = "white",
  fgFill = "#2E75B6",
  halign = "center",
  textDecoration = "bold",
  border = "Bottom"
)

openxlsx::addWorksheet(wb, "metadata")
openxlsx::writeData(wb, "metadata", mappatura_openbdap, withFilter = TRUE)
openxlsx::addStyle(
  wb,
  sheet = "metadata",
  style = header_style,
  rows = 1,
  cols = 1:ncol(mappatura_openbdap),
  gridExpand = TRUE
)
openxlsx::freezePane(wb, "metadata", firstRow = TRUE)
openxlsx::setColWidths(wb, "metadata", cols = 1:ncol(mappatura_openbdap), widths = "auto")

openxlsx::addWorksheet(wb, "variabili")
openxlsx::writeData(wb, "variabili", variabili_openbdap, withFilter = TRUE)
openxlsx::addStyle(
  wb,
  sheet = "variabili",
  style = header_style,
  rows = 1,
  cols = 1:ncol(variabili_openbdap),
  gridExpand = TRUE
)
openxlsx::freezePane(wb, "variabili", firstRow = TRUE)
openxlsx::setColWidths(wb, "variabili", cols = 1:ncol(variabili_openbdap), widths = "auto")

openxlsx::saveWorkbook(
  wb,
  file = "mappatura_openbdap_anagrafe_enti.xlsx",
  overwrite = TRUE
)