# Statistiche Giudiziarie ----

## 1. Imports ----

#install.packages(c("rvest", "stringr", "purrr","httr"))
library(rvest)
library(stringr)
library(purrr)
library(httr)
library(chromote)
library(readxl)
library(writexl)
library(tidyr)
library(dplyr)
library(data.table)

## 2. Globals & Setup ----


if (!dir.exists("data/statistiche_giudiziarie")) {
  dir.create("data/statistiche_giudiziarie")
}
data<- "data/statistiche_giudiziarie"
out<- "data"

url_pages <- c(
  "civ_trib_appello" = "https://datiestatistiche.giustizia.it/page/it/flussi-tribunali-ordinari-e-corti-di-appello",
  "civ_giudici_pace" = "https://datiestatistiche.giustizia.it/page/it/flussi-dei-giudici-di-pace",
  "civ_trib_minori"  = "https://datiestatistiche.giustizia.it/page/it/flussi-per-i-tribunali-per-i-minorenni",
  "pen_sorveglianza" = "https://datiestatistiche.giustizia.it/it/sorveglianza.page",
  "pen_intercettazioni" = "https://datiestatistiche.giustizia.it/it/intercettazioni.page"
)
base_url <- "https://datiestatistiche.giustizia.it/page/it/statistiche-giudiziarie"
u_agent <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36")

## 3. Functions & Utils ----

scarica_dataset <- function(url_pagina, nome_file, folder) {
  message("\n--- Tentativo su: ", nome_file)
  ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36")
  res_pag <- GET(url_pagina, ua, config(followlocation = TRUE))
    if (status_code(res_pag) != 200) {
    message("ERROR CODE:", status_code(res_pag), ", Access Denied")
    return(FALSE)
  }
  pagina_html <- read_html(res_pag)
  link_nodo <- pagina_html %>% 
    html_element("a[href*='documents'], a[href*='.xlsx'], a[href*='.zip']")
  href <- html_attr(link_nodo, "href")
  if (is.na(href)) {
    message("ERROR: no download link")
    return(FALSE)
  }
  url_file <- if(str_starts(href, "http")) href else paste0("https://datiestatistiche.giustizia.it", href)
  #download temporaneo
  temp_f <- tempfile()
  res_file <- GET(url_file, ua, write_disk(temp_f, overwrite = TRUE))
  #capire tipo di contenuto
  tipo_contenuto <- headers(res_file)$`content-type`
  estensione <- if(grepl("zip", tipo_contenuto, ignore.case = TRUE)) ".zip" else ".xlsx"
  percorso_finale <- file.path(folder, paste0(nome_file, estensione))
  file.copy(temp_f, percorso_finale, overwrite = TRUE)
  unlink(temp_f) 
  message("Saved in:", percorso_finale)
  return(TRUE)
}

## Execution ---- 

for(i in seq_along(url_pages)) {
  scarica_dataset(url_pages[i], names(url_pages)[i], data)
  Sys.sleep(2) 
}

## Flussi tributari ----

b <- ChromoteSession$new()
b$view()
b$Browser$setDownloadBehavior(
  behavior = "allow",
  downloadPath = data
)
url <- "https://reportistica.dgstat.giustizia.it/VisualizzatoreReport.Aspx?Report=/Pubblica/Statistiche%20della%20DGSTAT/Materia%20Penale/1.%20Movimento%20dei%20procedimenti/1.%20dati%20nazionali/1.%20tutti%20gli%20uffici%20in%20serie%20storica"
b$Page$navigate(url)
Sys.sleep(10)
b$Runtime$evaluate('
  document.getElementById("ReportViewer1_ctl05_ctl04_ctl00_ButtonImg").click()
')
Sys.sleep(3)

X_EXCEL <- 440 
Y_EXCEL <- 88 

b$Runtime$evaluate(paste0('
  const div = document.createElement("div");
  div.style.position = "absolute";
  div.style.left = "', X_EXCEL, 'px";
  div.style.top = "', Y_EXCEL, 'px";
  div.style.width = "10px";
  div.style.height = "10px";
  div.style.background = "red";
  div.style.borderRadius = "50%";
  div.style.zIndex = "99999";
  document.body.appendChild(div);
'))
Sys.sleep(1) 

#SPOSTAMENTO MOUSE
b$Input$dispatchMouseEvent(type = "mouseMoved", x = X_EXCEL, y = Y_EXCEL)
b$Input$dispatchMouseEvent(type = "mousePressed", x = X_EXCEL, y = Y_EXCEL, button = "left", clickCount = 1)
b$Input$dispatchMouseEvent(type = "mouseReleased", x = X_EXCEL, y = Y_EXCEL, button = "left", clickCount = 1)

## Manipolazione excel --- 

# Penale Flussi per Ufficio 

file_info <- file.info(list.files(data, pattern = "\\.xlsx$", full.names = TRUE))
ultimo_excel <- rownames(file_info)[which.max(file_info$mtime)]
if (length(ultimo_excel) > 0) {
  message("\n--- Trovato l'ultimo file scaricato: ", basename(ultimo_excel))
  nomi_fogli_originali <- excel_sheets(ultimo_excel)
  nuovi_nomi <- c("Iscritti", "Definiti", "Pendenti")
  lista_fogli_puliti <- map2(nomi_fogli_originali, seq_along(nomi_fogli_originali), function(foglio, idx) {
    dati <- read_excel(ultimo_excel, sheet = foglio, skip = 12)
    if (ncol(dati) >= 5) {
      dati <- dati[, -c(2:5)]
    }
    nome_valore <- if (length(nomi_fogli_originali) == length(nuovi_nomi)) nuovi_nomi[idx] else foglio
    dati_long <- dati %>% 
      pivot_longer(
        cols = -Ufficio, 
        names_to = "Anno", 
        values_to = nome_valore
      )
    dati_long$Anno <- str_replace_all(dati_long$Anno, "(?i)anno\\s*", "")
    return(dati_long)
  })
  if (length(lista_fogli_puliti) == 3) {
    foglio_unico <- lista_fogli_puliti %>% 
      reduce(full_join, by = c("Ufficio", "Anno"))
    risultato_finale <- list("foglio 1" = foglio_unico)
    message("Fogli uniti con successo!")
    cartella <- dirname(ultimo_excel)
    nuovo_percorso <- file.path(cartella, "pen_flussi_per_ufficio.xlsx")
    write_xlsx(risultato_finale, nuovo_percorso)
    if (ultimo_excel != nuovo_percorso) {
      file.remove(ultimo_excel)
    }
  } else {
    message("Trovati ", length(lista_fogli_puliti), " fogli invece di 3. Operazione annullata per sicurezza.")
  }
} else {
  message("Nessun file Excel trovato nella cartella ", data)
}

# Pulizia file civili ----

file_civili <- list.files(data, pattern = "^civ_.*\\.xlsx$", full.names = TRUE)
if (length(file_civili) > 0) {
  cat("\n--- Trovati", length(file_civili), "file civili da elaborare ---\n")
  walk(file_civili, function(percorso_file) {
    nome_file <- basename(percorso_file)
    nomi_fogli <- excel_sheets(percorso_file)
    if (length(nomi_fogli) >= 2) {
      cat("Elaborazione di:", nome_file, "\n")
      nome_secondo_foglio <- nomi_fogli[2]
      dati_secondo_foglio <- read_excel(percorso_file, sheet = nome_secondo_foglio)
      lista_output <- list()
      lista_output[[nome_secondo_foglio]] <- dati_secondo_foglio
      write_xlsx(lista_output, percorso_file)
      cat("File sovrascritto con successo")
    } else {
      cat("Salto", nome_file, ": ha meno di 2 fogli")
    }
  })
  cat("Pulizia dei file civili completata!")
} else {
  cat("Nessun file che inizia con 'civ_' trovato nella cartella")
}

## separazione dei file penali (Intercettazioni e Sorveglianza) ----

file_penali <- list.files(data, pattern = "^pen_intercettazioni\\.xlsx$|^pen_sorveglianza\\.xlsx$", full.names = TRUE)
if (length(file_penali) > 0) {
  walk(file_penali, function(percorso_file) {
    nome_file_base <- tools::file_path_sans_ext(basename(percorso_file))
    nomi_fogli <- excel_sheets(percorso_file)
    if (length(nomi_fogli) >= 3) {
      fogli_da_salvare <- nomi_fogli[2:3]
      walk(fogli_da_salvare, function(nome_foglio) {
        dati_foglio <- read_excel(percorso_file, sheet = nome_foglio)
        lista_output <- list()
        lista_output[[nome_foglio]] <- dati_foglio
        nuovo_nome_file <- file.path(data, paste0(nome_file_base, "_", nome_foglio, ".xlsx"))
        write_xlsx(lista_output, nuovo_nome_file)
        cat("Creato file:", basename(nuovo_nome_file))
      })
      file.remove(percorso_file)
    } else {
      cat("Salto", basename(percorso_file))
    }
  })
  cat("Operazione di separazione fogli completata")
} else {
  cat("Nessun file 'pen_intercettazioni' o 'pen_sorveglianza' trovato")
}

## Data_catalog ----

file_presenti <- list.files(data, pattern = "\\.xlsx$|\\.csv$", full.names = TRUE)
cat("Trovati", length(file_presenti), "file da mappare.\n\n")
catalogo <- map_df(file_presenti, function(percorso_file) {
  nome_file <- basename(percorso_file)
  cat("Mappatura di:", nome_file)
  dati <- tryCatch({
    if (grepl("\\.xlsx$", nome_file)) {
      read_excel(percorso_file, sheet = 1)
    } else {
      fread(percorso_file, encoding = "UTF-8")
    }
  }, error = function(e) {
    cat("Errore nella lettura di", nome_file, ":", e$message)
    return(NULL)
  })
  if (is.null(dati) || nrow(dati) == 0) return(NULL)
  n_oss <- nrow(dati)
  n_var <- ncol(dati)
  nomi_colonne <- colnames(dati)
  variabili_interesse <- paste(tolower(nomi_colonne), collapse = " | ")
  colonna_anno <- nomi_colonne[grepl("(?i)^anno$", nomi_colonne)]
  intervallo_anni <- ""
  if (length(colonna_anno) > 0) {
    vettore_anni <- as.character(dati[[colonna_anno[1]]])
    vettore_anni <- vettore_anni[!is.na(vettore_anni) & vettore_anni != ""]
    if (length(vettore_anni) > 0) {
      intervallo_anni <- paste0(min(vettore_anni), " - ", max(vettore_anni))
    }
  }
  mod_accesso <- if (grepl("pen_flussi_per_ufficio", nome_file)) {
    "Automazione browser (chromote)"
  } else {
    "Web scraping con R (rvest)"
  }
  data.frame(
    `nome dataset` = nome_file,
    `periodo/annualità disponibili` = intervallo_anni,
    `ultimo aggiornamento disponibile` = "ND",
    `variabili di interesse` = variabili_interesse,
    `n_osservazioni` = n_oss,
    `n_variabili` = n_var,
    `modalità di accesso` = mod_accesso,
    `limiti tecnici (rate limit)` = "Nessuna API. Rischio ban IP senza pause tra richieste",
    `formati scarico dati` = ".xlsx",
    `note` = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})

percorso_finalissimo <- file.path(out, "data_catalog_statistiche_giudiziarie.xlsx")
write_xlsx(catalogo, percorso_finalissimo)
