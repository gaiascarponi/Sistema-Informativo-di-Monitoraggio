#### STATISTICHE ISTRUZIONE ----

# Import ---- 
library(rvest)
library(httr)
library(data.table)
library(dplyr)
library(purrr)
library(stringr)
library(writexl)
library(xml2)
library(readxl)
library(tidyverse)

# Setup & Configuration ---- 
lista_url_aree <- c(
  "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Scuole",
  "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Studenti",
  "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Personale%20Scuola",
  "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Sistema%20Nazionale%20di%20Valutazione",
  "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Edilizia%20Scolastica",
  "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Adozioni%20libri%20di%20testo",
  "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Bilancio%20integrato%20delle%20scuole",
  "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Dati%20di%20monitoraggio"
)
input_folder <- "data/statistiche_istruzione"
data <- "data"
if (!dir.exists(input_folder)) dir.create(input_folder, recursive = TRUE)
                                          
# 1. Scraping ---- 
walk(lista_url_aree, function(url_area) {
  cat("Analisi area:", url_area, "\n")
  nome_settore <- gsub(".*area=", "", url_area)
  nome_settore <- URLdecode(nome_settore) # Converte %20 in spazi
  nome_settore_pulito <- gsub(" ", "_", nome_settore) 
  tryCatch({
    risposta_indice <- GET(url_area, config(ssl_verifypeer = FALSE))
    pagina <- read_html(risposta_indice)
    testi_completi <- pagina %>% html_nodes("p, div, span") %>% html_text()
    date_modifica <- testi_completi[grepl("Modified:", testi_completi)]
    links <- pagina %>% html_nodes("a") %>% html_attr("href")
    link_csv_relativi <- links[grepl("\\.csv$", links)] %>% unique()
    cat("Trovati", length(link_csv_relativi), "file CSV in quest'area.\n")
    for (i in seq_along(link_csv_relativi)) {
      link <- link_csv_relativi[i]
      nome_file_originale <- basename(link)
      nome_file_nuovo <- paste0(nome_settore_pulito, "_", nome_file_originale)
      percorso_salvataggio <- file.path(input_folder, nome_file_nuovo)
      url_completo <- paste0("https://dati.istruzione.it/opendata/opendata/catalogo/elements1/", gsub("^/", "", link))
      comando <- sprintf('curl -k -L -A "Mozilla/5.0" "%s" -o "%s"', url_completo, percorso_salvataggio)
      system(comando)
      if (i <= length(date_modifica) && file.exists(percorso_salvataggio)) {
        data_estratta <- gsub(".*Modified:\\s*([0-9]{2}/[0-9]{2}/[0-9]{4}).*", "\\1", date_modifica[i])
        try({
          df_temporaneo <- fread(percorso_salvataggio, colClasses = "character", encoding = "UTF-8")
          df_temporaneo[, Ultima_Modifica_Portale := data_estratta]
          fwrite(df_temporaneo, percorso_salvataggio)
          cat("Data", data_estratta, "inserita con successo in", nome_file_nuovo, "\n")
        }, silent = TRUE)
      }
    }
    cat("\n")
  }, error = function(e) {
    cat("Errore URL:", url_area, "\n", e$message, "\n\n")
  })
})
print("fine")

# 2. DATA MANIPULATION/CLEANING ----
# Creazione Variabile Anno
file_list <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)
walk(file_list, function(file) {
  nome_file <- basename(file)
  df <- fread(file, colClasses = "character", encoding = "UTF-8")
  modificato <- FALSE
  anno_estratto <- stringr::str_extract(nome_file, "20[0-2][0-9]")
  anno_num <- suppressWarnings(as.numeric(anno_estratto))
  if (!is.na(anno_num) && anno_num >= 2000 && anno_num <= 2026) {
    df[, Anno := as.character(anno_num)]
    modificato <- TRUE
    message(paste("File da nome file:", nome_file, "->", anno_num))
  } else if ("ANNOSCOLASTICO" %in% names(df)) {
    anni_colonna <- stringr::str_extract(df$ANNOSCOLASTICO, "20[0-2][0-9]")
    if (!all(is.na(anni_colonna))) {
      df[, Anno := anni_colonna]
      modificato <- TRUE
      message(paste("Prendo Anno da ANNOSCOLASTICO:", nome_file))
    } else {
      message(paste("ANNOSCOLASTICO presente ma non leggibile:", nome_file))
    }
  } else if ("Anno" %in% names(df)) {
    message(paste("File già con Anno:", nome_file))
  } else {
    message(paste("File senza anno:", nome_file))
  }
  if (modificato) {
    fwrite(df, file)
    cat("   Anni trovati:", paste(unique(df$Anno), collapse = ", "), "\n")
  }
})
file_scaricati <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)
walk(file_scaricati, function(percorso_file) {
  nome_file <- basename(percorso_file)
  if (grepl("^Personale_", nome_file)) {
    nuovo_nome <- gsub("AS[0-9]{4}", "", nome_file)
    percorso_nuovo <- file.path(input_folder, nuovo_nome)
    if (percorso_file != percorso_nuovo) {
      file.rename(percorso_file, percorso_nuovo)
      cat("Pulito nome Personale:", nome_file, "->", nuovo_nome, "\n")
    }
  }
})
# Rinominare
file_scaricati <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)
cat("Trovati", length(file_scaricati), "file totali da elaborare.\n\n")
get_nome_pulito <- function(nome_file) {
  if (grepl("Scuole_", nome_file))        return(gsub("[0-9]{14}\\.csv$", ".csv", nome_file))
  if (grepl("Studenti_", nome_file))      return(gsub("[0-9]{14}\\.csv$", ".csv", nome_file))
  if (grepl("Bilancio", nome_file))       return(gsub("[0-9]{12}\\.csv$", ".csv", nome_file))
  if (grepl("Dati_di_", nome_file))       return(gsub("[0-9]{12}\\.csv$", ".csv", nome_file))
  if (grepl("Adozioni_", nome_file))      return(gsub("[0-9]{12}\\.csv$", ".csv", nome_file))
  if (grepl("Personale_", nome_file))     return(gsub("[0-9]+\\.csv$", ".csv", nome_file))
  if (grepl("Sistema_", nome_file))       return(gsub("[0-9]+\\.csv$", ".csv", nome_file))
  return(nome_file)
}
mappa_gruppi <- split(
  file_scaricati,
  sapply(basename(file_scaricati), get_nome_pulito)
)
file_effettivamente_accorpati <- c()
walk(names(mappa_gruppi), function(nome_pulito) {
  if (grepl("Edilizia", nome_pulito)) return(NULL)
  file_da_unire <- mappa_gruppi[[nome_pulito]]
  percorso_output <- file.path(input_folder, nome_pulito)
  if (length(file_da_unire) > 1) {
    cat("Unisco", length(file_da_unire), "file per:", nome_pulito, "\n")
    lista_df <- lapply(file_da_unire, function(f) {
      fread(f, colClasses = "character", encoding = "UTF-8")
    })
    db_unito <- rbindlist(lista_df, fill = TRUE)
    fwrite(db_unito, percorso_output)
    file_effettivamente_accorpati <<- c(file_effettivamente_accorpati, file_da_unire)
  } else {
    if (file_da_unire[1] != percorso_output) {
      file.rename(file_da_unire[1], percorso_output)
    }
  }
})
if (length(file_effettivamente_accorpati) > 0) {
  file.remove(file_effettivamente_accorpati)
  cat("Rimossi", length(file_effettivamente_accorpati), "file originali.\n")
}

#Edilizia con doppio vincolo ---- 
tutti_i_file <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)
file_edilizia <- tutti_i_file[grepl("Edilizia", basename(tutti_i_file))]
if (length(file_edilizia) > 0) {
  nomi_puliti <- gsub("[0-9]+\\.csv$", ".csv", basename(file_edilizia))
  lunghezze   <- nchar(basename(file_edilizia))
  df_nomi <- data.frame(
    Percorso_Completo = file_edilizia,
    Originale         = basename(file_edilizia),
    Pulito            = nomi_puliti,
    Lunghezza         = lunghezze,
    stringsAsFactors  = FALSE
  )
  gruppi_da_unire <- df_nomi %>%
    group_by(Pulito, Lunghezza) %>%
    summarise(Quanti = n(), .groups = "drop") %>%
    filter(Quanti > 1)
  file_effettivamente_accorpati <- c()
  if (nrow(gruppi_da_unire) > 0) {
    for (i in 1:nrow(gruppi_da_unire)) {
      nome_target      <- gruppi_da_unire$Pulito[i]
      lunghezza_target <- gruppi_da_unire$Lunghezza[i]
      file_da_fondere <- df_nomi %>%
        filter(Pulito == nome_target, Lunghezza == lunghezza_target) %>%
        pull(Percorso_Completo)
      lista_df <- lapply(file_da_fondere, function(f) {
        fread(f, colClasses = "character", encoding = "UTF-8")
      })
      db_unito <- rbindlist(lista_df, fill = TRUE)
      percorso_salvataggio <- file.path(input_folder, nome_target)
      if (file.exists(percorso_salvataggio)) {
        nome_modificato      <- gsub("\\.csv$", "_2.csv", nome_target)
        percorso_salvataggio <- file.path(input_folder, nome_modificato)
        cat("-> Nome già esistente! Salvo come:", nome_modificato, "\n")
      }
      fwrite(db_unito, percorso_salvataggio)
      file_effettivamente_accorpati <- c(file_effettivamente_accorpati, file_da_fondere)
    }
    if (length(file_effettivamente_accorpati) > 0) {
      file.remove(file_effettivamente_accorpati)
      cat("\nCancellati", length(file_effettivamente_accorpati), "file originali Edilizia.\n")
    }
  } else {
    cat("Nessun gruppo di file dell'Edilizia rispetta i criteri di accorpamento.\n")
  }
  file_rimasti <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)
  file_da_eliminare_extra <- file_rimasti[grepl("Edilizia_.*[0-9]{5,}", basename(file_rimasti))]
  if (length(file_da_eliminare_extra) > 0) {
    file.remove(file_da_eliminare_extra)
    cat("\nPulizia finale: eliminati", length(file_da_eliminare_extra), "file Edilizia con più di 4 cifre.\n")
    print(basename(file_da_eliminare_extra))
  } else {
    cat("\nNessun file Edilizia extra trovato.\n")
  }
} else {
  cat("Non ci sono file che contengono 'Edilizia' nella cartella!\n")
}

# Data Catalog ---- 
if (!exists("cartella_output")) cartella_output <- input_folder
file_presenti <- list.files(input_folder, full.names = TRUE)
file_presenti <- file_presenti[tools::file_ext(file_presenti) != ""]
catalogo <- map_df(file_presenti, function(percorso_file) {
  nome_file <- basename(percorso_file)
  ext <- paste0(".", tolower(tools::file_ext(nome_file)))
  cat("Mappatura di:", nome_file, "...\n")
  n_oss <- NA; n_var <- NA; variabili_interesse <- ""; intervallo_anni <- ""; data_agg <- ""; dati <- NULL
  if (ext == ".csv") {
    dati <- tryCatch({
      fread(percorso_file, colClasses = "character", encoding = "UTF-8", fill = TRUE, header = TRUE)
    }, error = function(e) {
      tryCatch({
        con <- file(percorso_file, "rb")
        linee_raw <- readLines(con, warn = FALSE)
        close(con)
        linee_pulite <- iconv(linee_raw, from = "latin1", to = "UTF-8", sub = " ")
        as.data.table(read.csv(text = linee_pulite, sep = ",", stringsAsFactors = FALSE, check.names = FALSE, quote = ""))
      }, error = function(e2) NULL)
    })
    if (!is.null(dati) && nrow(dati) > 0) {
      colnames(dati) <- make.unique(iconv(colnames(dati), to = "UTF-8", sub = " "))
      dati <- dati %>% mutate(across(where(is.character), ~iconv(.x, to = "UTF-8", sub = " ")))
    }
  } else if (ext %in% c(".xlsx", ".xls")) {
    dati <- tryCatch({ as.data.frame(read_excel(percorso_file, sheet = 1, col_types = "text")) }, error = function(e) NULL)
  } else if (ext == ".xml") {
    tryCatch({
      xml_doc <- read_xml(percorso_file)
      nodi <- xml_children(xml_doc)
      if (length(nodi) == 1 && xml_name(nodi[1]) == "channel") nodi <- xml_children(nodi[1])
      n_item <- nodi[xml_name(nodi) == "item"]
      target <- if(length(n_item) > 0) n_item else nodi
      if (length(target) > 0) {
        n_oss <- length(target)
        campi <- unique(tolower(xml_name(xml_children(target[1]))))
        n_var <- length(campi)
        variabili_interesse <- paste(campi, collapse = " | ")
      }
    }, error = function(e) NULL)
  }
  if (!is.null(dati) && nrow(dati) > 0) {
    n_oss <- nrow(dati)
    nomi_col <- colnames(dati)
    col_agg <- nomi_col[grepl("Ultima_Modifica_Portale$", nomi_col)]
    col_anno <- nomi_col[grepl("(?i)^anno$|(?i)^anno.*riferimento$", nomi_col)]
    if (length(col_agg) > 0) data_agg <- as.character(dati[[col_agg[1]]][1])
    if (length(col_anno) > 0) {
      v_anni <- na.omit(as.character(dati[[col_anno[1]]]))
      v_anni <- v_anni[v_anni != ""]
      if (length(v_anni) > 0) intervallo_anni <- paste0(min(v_anni), " - ", max(v_anni))
    }
    cols_clean <- nomi_col[!(nomi_col %in% c(col_agg, col_anno))]
    n_var <- length(cols_clean)
    variabili_interesse <- paste(tolower(cols_clean), collapse = " | ")
  }
  nota_file <- case_when(
    ext == ".xml" & !is.na(n_oss) ~ "File XML analizzato per nodi",
    is.null(dati) & !ext %in% c(".csv", ".xlsx", ".xls") ~ "Formato non analizzabile",
    !is.null(dati) & ext %in% c(".xlsx", ".xls") ~ "Excel letto con successo",
    TRUE ~ ""
  )
  data.frame(
    `nome dataset` = nome_file,
    `periodo/annualità disponibili` = intervallo_anni,
    `ultimo aggiornamento disponibile` = data_agg,
    `variabili di interesse` = variabili_interesse,
    `n_osservazioni` = n_oss,
    `n_variabili` = n_var,
    `modalità di accesso` = "Download diretto (Scraping rvest/curl)",
    `limiti tecnici (rate limit)` = "Nessuna API",
    `formati scarico dati` = ext,
    `note` = nota_file,
    stringsAsFactors = FALSE, check.names = FALSE
  )
})

file_catalogo <- file.path(data, "data_catalog_statistiche_istruzione.xlsx")
write_xlsx(catalogo, path = file_catalogo)
cat("Catalogo pronto:", file_catalogo, "\n")