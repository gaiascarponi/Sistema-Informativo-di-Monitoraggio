# Import ---- 

library(chromote)
library(rvest)
library(stringr)
library(httr)
library(readxl)
library(writexl)
library(purrr)
library(data.table)
library(tidyverse)
library(dplyr)
library(xml2)

# Setup & Configuration ---- 

totale_pagine  <- 200
data_dir <- "data/statistiche_sanitarie"
data_dir_clean <- "data/statistiche_sanitarie_clean"
metadata_dir<- "data/statistiche_sanitarie/metadata_date"
temp_dir <- file.path(data_dir, "temp_extraction")
cartella_output <- "data"
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
if (!dir.exists(metadata_dir)) dir.create(metadata_dir, recursive = TRUE)
if (!dir.exists(data_dir_clean)) dir.create(data_dir_clean, recursive = TRUE)
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
pulisci_nome_file <- function(titolo, estensione) {
  titolo %>%
    str_to_lower() %>%
    str_replace_all("[àáâãäå]", "a") %>%
    str_replace_all("[èéêë]",   "e") %>%
    str_replace_all("[ìíîï]",   "i") %>%
    str_replace_all("[òóôõö]",  "o") %>%
    str_replace_all("[ùúûü]",   "u") %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    paste0(".", estensione)
}

# Scraping (w/ Chrome e Chromote) ----

scarica_da_scheda <- function(b, url_scheda) {
  tryCatch({
    b$Page$navigate(url_scheda)
    Sys.sleep(5)
    html_raw <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    pagina   <- read_html(html_raw)
    titolo <- pagina %>%
      html_node("h1") %>%
      html_text(trim = TRUE)
    if (is.na(titolo) || titolo == "") {
      titolo <- basename(url_scheda)
      message("Titolo non trovato, uso: ", titolo)
    } else {
      message("Titolo: ", titolo)
    }
    testo_pagina <- pagina %>% html_text(trim = TRUE)
    data_aggiornamento <- ""
    regex_aggiornamento <- "(?i)ultimo\\s+aggiornamento\\s*:?\\s*(\\d{2}/\\d{2}/\\d{4})"
    match_agg <- str_match(testo_pagina, regex_aggiornamento)
    regex_caricamento <- "(?i)data\\s+caricamento\\s*:?\\s*(\\d{2}/\\d{2}/\\d{4})"
    match_caric <- str_match(testo_pagina, regex_caricamento)
    if (!is.na(match_agg[1, 2])) {
      data_aggiornamento <- match_agg[1, 2]
      message(" Ultimo aggiornamento trovato: ", data_aggiornamento)
    } else if (!is.na(match_caric[1, 2])) {
      data_aggiornamento <- match_caric[1, 2]
      message(" Data caricamento trovata: ", data_aggiornamento)
    } else {
      righe_tabella <- pagina %>% html_nodes("tr")
      for (riga in righe_tabella) {
        testo_riga <- html_text(riga, trim = TRUE)
        if (grepl("(?i)ultimo aggiornamento|(?i)data caricamento", testo_riga)) {
          estrazione_data <- str_extract(testo_riga, "\\d{2}/\\d{2}/\\d{4}")
          if (!is.na(estrazione_data)) {
            data_aggiornamento <- estrazione_data
            message(" Data trovata nella riga: ", data_aggiornamento)
            break
          }
        }
      }
    }
    
    if (data_aggiornamento == "") {
      message("Nessuna data di aggiornamento o caricamento trovata")
    }
    link_href <- pagina %>% html_nodes("a") %>% html_attr("href")
    indici <- which(str_detect(link_href, "(?i)\\.csv|\\.zip") & !is.na(link_href))
    if (length(indici) == 0) {
      message("Nessun CSV/ZIP trovato, cerco altri formati ")
      indici <- which(str_detect(link_href, "(?i)\\.xlsx$|\\.xls$|\\.json$|\\.xml$") & !is.na(link_href))
    }
    if (length(indici) == 0) {
      message("Cerco file scaricabile")
      indici <- which(str_detect(link_href, "/download/|/scarica/") & !is.na(link_href))
    }
    if (length(indici) == 0) {
      message("Nessun file utile trovato")
      b$Page$navigate("https://www.dati.salute.gov.it/")
      Sys.sleep(8)
      return(invisible(NULL))
    }
    
    for (j in seq_along(indici)) {
      file_target <- link_href[indici[j]]
      url_file    <- ifelse(
        str_starts(file_target, "http"),
        file_target,
        paste0("https://www.dati.salute.gov.it", file_target)
      )
      estensione <- str_extract(url_file, "(?i)(csv|zip|xlsx|xls|json|xml)$") %>% str_to_lower()
      if(is.na(estensione)) estensione <- "file"
      suffisso   <- ifelse(length(indici) > 1, paste0("_", j), "")
      nome_file  <- pulisci_nome_file(paste0(titolo, suffisso), estensione)
      dest       <- file.path(data_dir, nome_file)
      download.file(url_file, destfile = dest, mode = "wb", quiet = TRUE)
      message("Scaricato: ", nome_file)
      if (data_aggiornamento != "") {
        nome_file_data <- pulisci_nome_file(paste0(titolo, suffisso), "txt")
        writeLines(data_aggiornamento, file.path(metadata_dir, nome_file_data))
      }
      Sys.sleep(0.3)
    }
    b$Page$navigate("https://www.dati.salute.gov.it/")
    Sys.sleep(8)
  }, error = function(e) {
    message("Errore: ", e$message)
    try({ b$Page$navigate("https://www.dati.salute.gov.it/"); Sys.sleep(8) }, silent = TRUE)
  })
}

vai_pagina_successiva <- function(b) {
  js <- "
    (function() {
      var bottoni = document.querySelectorAll('button');
      for (var btn of bottoni) {
        if (btn.innerText.trim() === 'Pagina successiva') {
          btn.click();
          return 'OK';
        }
      }
      return 'ERRORE: bottone non trovato';
    })()
  "
  risultato <- b$Runtime$evaluate(js)$result$value
  message("    Cambio pagina: ", risultato)
  return(risultato)
}

estrai_link_dataset <- function(b) {
  html_raw <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  pagina   <- read_html(html_raw)
  tag_a <- pagina %>% html_nodes("main a, [role='main'] a, #content a, article a") %>% html_attr("href")
  if (length(tag_a) == 0 || all(is.na(tag_a))) {
    tag_a <- pagina %>% html_nodes("a") %>% html_attr("href")
  }
  link <- tag_a[str_detect(tag_a, "/dataset/") & !is.na(tag_a)]
  link <- unique(link)
  link <- ifelse(str_starts(link, "http"), link, paste0("https://www.dati.salute.gov.it", link))
  return(link)
}

b <- ChromoteSession$new()
b$Page$navigate("https://www.dati.salute.gov.it/")
Sys.sleep(8)
link_gia_elaborati <- character(0)

for (pag in 1:totale_pagine) {
  message(paste0("\nPAGINA ", pag, "/", totale_pagine))
  link_pagina <- estrai_link_dataset(b)
  link_nuovi  <- setdiff(link_pagina, link_gia_elaborati)
  message(paste0("Dataset trovati: ", length(link_pagina), " | Nuovi da scaricare: ", length(link_nuovi)))
  
  for (i in seq_along(link_nuovi)) {
    message(paste0("   -> [", i, "/", length(link_nuovi), "] ", link_nuovi[i]))
    scarica_da_scheda(b, link_nuovi[i])
  }
  link_gia_elaborati <- union(link_gia_elaborati, link_pagina)
  
  if (pag < totale_pagine) {
    risultato <- vai_pagina_successiva(b)
    if (str_starts(risultato, "ERRORE")) {
      message("ottone non trovato — probabilmente ultima pagina reale. Fermo.")
      break
    }
    Sys.sleep(8)
  }
}
try(b$parent$close(), silent = TRUE)
message("Scraping terminato!")

## PULIZIA CARTELLA ----

# Pulizia zip ----
tutti_i_zip <- list.files(data_dir, pattern = "\\.zip$", full.names = TRUE)
if (length(tutti_i_zip) > 0) {
  df_files <- data.frame(path = tutti_i_zip) %>%
    mutate(nome_zip = basename(path), gruppo = str_replace(nome_zip, "_[1-9]\\.zip$", ""))
  
  file_da_tenere <- df_files %>% group_by(gruppo) %>%
    filter(if(any(str_detect(nome_zip, "_1\\.zip"))) str_detect(nome_zip, "_1\\.zip") else row_number() == 1) %>%
    ungroup()
  
  for (i in 1:nrow(file_da_tenere)) {
    zip_attuale <- file_da_tenere$path[i]
    nome_pulito <- file_da_tenere$gruppo[i]
    unzip(zip_attuale, exdir = temp_dir)
    file_estratto <- list.files(temp_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)[1]
    
    if (!is.na(file_estratto)) {
      nuovo_percorso <- file.path(data_dir, paste0(nome_pulito, ".csv"))
      file.rename(file_estratto, nuovo_percorso)
      message("Estratto e rinominato: ", nome_pulito, ".csv")
      nome_txt_vecchio <- str_replace(basename(zip_attuale), "\\.zip$", ".txt")
      percorso_txt_vecchio <- file.path(data_dir, nome_txt_vecchio)
      if (file.exists(percorso_txt_vecchio)) file.rename(percorso_txt_vecchio, file.path(data_dir, paste0(nome_pulito, ".txt")))
    }
    unlink(list.files(temp_dir, full.names = TRUE), recursive = TRUE)
  }
  unlink(temp_dir, recursive = TRUE)
  file.remove(tutti_i_zip)
}

# Pulizia _2 files ----

file_target <- list.files(
  data_dir, 
  pattern = "_(1|2)\\.(xlsx|xls|xml|csv)$", 
  full.names = TRUE
)
if (length(file_target) > 0) {
  for (percorso_completo in file_target) {
    nome_file <- basename(percorso_completo)
    if (str_detect(nome_file, "_1\\.(xlsx|xls|xml|csv)$")) {
      nuovo_nome <- str_replace(nome_file, "_1\\.", ".")
      percorso_nuovo <- file.path(data_dir, nuovo_nome)
      file.rename(percorso_completo, percorso_nuovo)
      message("Rinominato: ", nome_file, " -> ", nuovo_nome)
    }
    if (str_detect(nome_file, "_2\\.(xlsx|xls|xml|csv)$")) {
      file.remove(percorso_completo)
      message("Eliminato: ", nome_file)
    }
  }
} else {
  message("Nessun file terminante con _1 o _2 trovato nella cartella.")
}

# Pulizia .txt ----

file_txt <- list.files(metadata_dir, pattern = "\\.txt$", full.names = TRUE)
for (file_completo in file_txt) {
  nome_file <- basename(file_completo)
  if (str_detect(nome_file, "_1\\.txt$")) {
    nuovo_nome <- str_replace(nome_file, "_1\\.txt$", ".txt")
    file.rename(file_completo, file.path(metadata_dir, nuovo_nome))
    message("Rinominato: ", nome_file, " -> ", nuovo_nome)
  }
  if (str_detect(nome_file, "_[2-9]\\.txt$")) {
    file.remove(file_completo)
    message("Eliminato: ", nome_file)
  }
}

# Modifiche su dataset specifici ----

file_da_pulire <- file.path(data_dir, "farmaci_50_sop_e_otc_piu_venduti_nel_2_semestre_2021.csv")
if (file.exists(file_da_pulire)) {
  righe <- readLines(file_da_pulire, warn = FALSE)
  if (length(righe) > 0) {
    writeLines(righe[-1], file_da_pulire)
  }
}

nomi_file_da_pulire <- c("personale_dei_serd_anno_2021.csv",
  "personale_dei_serd_anno_2022.csv")
for (nome in nomi_file_da_pulire) {
  file_da_pulire <- file.path(data_dir, nome)
  if (file.exists(file_da_pulire)) {
    righe <- readLines(file_da_pulire, warn = FALSE)
    if (length(righe) > 3) {
      writeLines(righe[-(1:3)], file_da_pulire)
      message("[OK] Rimosse le prime 3 righe da: ", nome)
    }
  } else {
    message("File non trovato (salto il passaggio): ", nome)
  }
}

nomi_file_da_pulire <- c("personale_dei_serd_anno_2023.csv")
for (nome in nomi_file_da_pulire) {
  file_da_pulire <- file.path(data_dir, nome)
  if (file.exists(file_da_pulire)) {
    righe <- readLines(file_da_pulire, warn = FALSE)
    if (length(righe) > 0) {
      writeLines(righe[-(1:2)], file_da_pulire)
      message("[OK] Rimosse le prime due righe da: ", nome)
    }
  } else {
    message("File non trovato (salto il passaggio): ", nome)
  }
}

# Catalogo Dati ----

file_presenti <- list.files(data_dir, full.names = TRUE)
file_presenti <- file_presenti[tools::file_ext(file_presenti) != ""]
catalogo <- map_df(file_presenti, function(percorso_file) {
  nome_file <- basename(percorso_file)
  ext <- paste0(".", tolower(tools::file_ext(nome_file)))
  cat("Mappatura di:", nome_file, "...\n")
  n_oss <- NA
  n_var <- NA
  variabili_interesse <- ""
  intervallo_anni <- ""
  dati <- NULL
  if (ext == ".csv") {
    dati <- tryCatch({
      fread(percorso_file, sep = ";", encoding = "UTF-8", fill = TRUE, header = TRUE)
    }, error = function(e) {
      cat("Problema con fread, provo la lettura riga per riga\n")
      tryCatch({
        con <- file(percorso_file, "rb")
        linee_raw <- readLines(con, warn = FALSE)
        close(con)
        linee_pulite <- iconv(linee_raw, from = "latin1", to = "UTF-8", sub = " ")
        as.data.table(read.csv(text = linee_pulite, sep = ";", stringsAsFactors = FALSE, check.names = FALSE, quote = ""))
      }, error = function(e2) { return(NULL) })
    })
    
    if (!is.null(dati) && nrow(dati) > 0) {
      colnames(dati) <- iconv(colnames(dati), to = "UTF-8", sub = " ")
      colnames(dati) <- make.unique(colnames(dati))
      dati <- dati %>%
        mutate(across(where(is.character), function(x) {
          iconv(x, to = "UTF-8", sub = " ")
        }))
    }
  } 
  else if (ext %in% c(".xlsx", ".xls")) {
    dati <- tryCatch({
      as.data.frame(read_excel(percorso_file, sheet = 1, col_types = "text"))
    }, error = function(e) {
      cat("ERRORRR\n")
      return(NULL)
    })
  } 
  else if (ext == ".xml") {
    cat("File XML rilevato. Tento il conteggio dei nodi...\n")
    tryCatch({
      xml_doc <- read_xml(percorso_file)
      nodi <- xml_children(xml_doc)
      if (length(nodi) == 1 && xml_name(nodi[1]) == "channel") {
        nodi <- xml_children(nodi[1])
      }
      nodi_item <- nodi[xml_name(nodi) == "item"]
      if (length(nodi_item) > 0) {
        n_oss <- length(nodi_item)
        campi_primo_item <- xml_name(xml_children(nodi_item[1]))
        campi_unici <- unique(tolower(campi_primo_item))
        n_var <- length(campi_unici)
        variabili_interesse <- paste(campi_unici, collapse = " | ")
        
      } else {
        n_oss <- length(nodi)
        if (n_oss > 0) {
          campi_nodo <- xml_name(xml_children(nodi[1]))
          if (length(campi_nodo) == 0) campi_nodo <- names(xml_attrs(nodi[1]))
          
          campi_unici <- unique(tolower(campi_nodo))
          if (length(campi_unici) > 0) {
            n_var <- length(campi_unici)
            variabili_interesse <- paste(campi_unici, collapse = " | ")
          }
        }
      }
    }, error = function(e) {
      cat("Impossibile leggere la struttura del file XML\n")
    })
  }
  else {
    cat("Formato", ext, "non analizzabile. Lo inserisco comunque nel catalogo.\n")
  }
  if (!is.null(dati) && nrow(dati) > 0) {
    n_oss <- nrow(dati)
    n_var <- ncol(dati)
    nomi_colonne <- colnames(dati)
    variabili_interesse <- paste(tolower(nomi_colonne), collapse = " | ")
    colonna_anno <- nomi_colonne[grepl("(?i)^anno$|(?i)^anno.*riferimento$", nomi_colonne)]
    if (length(colonna_anno) > 0) {
      vettore_anni <- as.character(dati[[colonna_anno[1]]])
      vettore_anni <- iconv(vettore_anni, to = "UTF-8", sub = " ")
      vettore_anni <- vettore_anni[!is.na(vettore_anni) & vettore_anni != ""]
      if (length(vettore_anni) > 0) {
        intervallo_anni <- paste0(head(vettore_anni, 1), " - ", tail(vettore_anni, 1))
      }
    }
  }
  nome_base <- tools::file_path_sans_ext(nome_file)
  percorso_txt <- file.path(metadata_dir, paste0(nome_base, ".txt"))
  data_agg <- ""
  if (file.exists(percorso_txt)) {
    data_agg <- readLines(percorso_txt, warn = FALSE)[1]
  }
  if (data_agg == "") {
    nome_pulito <- str_replace(nome_file, "\\.zip\\.[a-zA-Z0-9]+$", "")
    percorso_txt_eccezione <- file.path(metadata_dir, paste0(nome_pulito, ".txt"))
    if (file.exists(percorso_txt_eccezione)) {
      data_agg <- readLines(percorso_txt_eccezione, warn = FALSE)[1]
      cat("Data trovata tramite eccezione\n")
    }
  }
  nota_file <- ""
  if (ext == ".xml") {
    if (!is.na(n_oss)) {
      nota_file <- "File XML analizzato contando i nodi record."
    } else {
      nota_file <- "File XML non strutturato o non leggibile"
    }
  } else if (is.null(dati) && !ext %in% c(".csv", ".xlsx", ".xls")) {
    nota_file <- "File non analizzabile: impossibile contare righe e variabili"
  } else if (!is.null(dati) && ext %in% c(".xlsx", ".xls")) {
    nota_file <- "File Excel letto con successo (Foglio 1)"
  }
  data.frame(
    `nome dataset` = nome_file,
    `periodo/annualità disponibili` = intervallo_anni,
    `ultimo aggiornamento disponibile` = data_agg,
    `variabili di interesse` = variabili_interesse,
    `n_osservazioni` = n_oss,
    `n_variabili` = n_var,
    `modalità di accesso` = "Automazione browser (chromote)",
    `limiti tecnici (rate limit)` = "Nessuna API. Rischio blocco IP se non si inseriscono attese tra le navigazioni (React rendering)",
    `formati scarico dati` = ext,
    `note` = nota_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})

percorso_finalissimo <- file.path(cartella_output, "data_catalog_statistiche_sanitarie.xlsx")
write_xlsx(catalogo, percorso_finalissimo)