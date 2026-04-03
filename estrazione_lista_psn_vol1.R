rm(list=ls())
#..............................................................................#
# PROGETTO: Estrazione catalogo PSN da PDF
# VERSIONE: 2.0
# OBIETTIVO:
#   Estrarre l'intero catalogo dei lavori e relativi metadati da un PDF,
#   costruire una tabella strutturata e salvarla in Excel/CSV.
#
# OUTPUT:
#   - catalogo_psn_estratto.xlsx
#   - catalogo_psn_estratto.csv
#   - catalogo_psn_log.txt
#
# AUTORE:
#   Script pensato per uso condivisibile in team di lavoro.
#..............................................................................#

 
# 0) PACCHETTI E SETUP --------------------------------------------------------
 

required_packages <- c(
  "pdftools",
  "stringr",
  "dplyr",
  "purrr",
  "tibble",
  "tidyr",
  "readr",
  "openxlsx"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

options(stringsAsFactors = FALSE)
options(scipen = 999)

 
# 1) PARAMETRI DI PROGETTO ----------------------------------------------------
 

pdf_path          <- "PSN 23-25/Vol 1 evoluzione dell informazione statistica.pdf"
excel_output_path <- "catalogo_psn_estratto.xlsx"
csv_output_path   <- "catalogo_psn_estratto.csv"
log_output_path   <- "catalogo_psn_log.txt"

 
# 2) LOGGING ------------------------------------------------------------------
 

log_messages <- character()

log_info <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  message(msg)
  assign("log_messages", c(get("log_messages", envir = .GlobalEnv), msg), envir = .GlobalEnv)
}

 
# 3) FUNZIONI DI UTILITÀ TESTO ------------------------------------------------
 

#..............................................................................
# clean_text()
# Pulizia base del testo estratto dal PDF
#..............................................................................
clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\r", "\n")
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_replace_all(x, "[ \t]+", " ")
  x <- stringr::str_replace_all(x, " +\n", "\n")
  x <- stringr::str_replace_all(x, "\n{2,}", "\n")
  x <- stringr::str_trim(x)
  x
}

#..............................................................................
# remove_headers_footers()
# Rimozione prudente di intestazioni / piè di pagina ricorrenti
#..............................................................................
remove_headers_footers <- function(x) {
  x <- as.character(x)
  
  patterns_to_remove <- c(
    "Programma statistico nazionale 2023-2025\\. Aggiornamento 2024-2025",
    "Supplemento ordinario n\\. 39 alla GAZZETTA UFFICIALE Serie generale - n\\. 296",
    "22-12-2025",
    "— ?\\d+ ?—",
    "<PARSED TEXT FOR PAGE:.*?>",
    "<IMAGE FOR PAGE:.*?>"
  )
  
  for (pat in patterns_to_remove) {
    x <- stringr::str_replace_all(x, pat, "")
  }
  
  x <- clean_text(x)
  x
}

#..............................................................................
# normalize_page_text()
# Applica la pulizia standard a una pagina del PDF
#..............................................................................
normalize_page_text <- function(x) {
  x |>
    clean_text() |>
    remove_headers_footers()
}

#..............................................................................
# clean_field_value()
# FUNZIONE VETTORIALE:
# pulisce valori di campo e funziona correttamente dentro mutate(across()).
#..............................................................................
clean_field_value <- function(x) {
  x <- as.character(x)
  
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_replace_all(x, "\\(\\s+", "(")
  x <- stringr::str_replace_all(x, "\\s+\\)", ")")
  x <- stringr::str_trim(x)
  
  x[x == ""] <- NA_character_
  x
}

#..............................................................................
# escape_regex()
# Escape di caratteri speciali per costruire regex più sicure
#..............................................................................
escape_regex <- function(x) {
  stringr::str_replace_all(x, "([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1")
}

 
# 4) FUNZIONI DI ESTRAZIONE ---------------------------------------------------
 

#..............................................................................
# extract_field()
# Estrae il testo tra una label e la successiva
#..............................................................................
extract_field <- function(block, field_label, next_labels = character()) {
  field_label_esc <- escape_regex(field_label)
  
  if (length(next_labels) > 0) {
    next_labels_esc <- escape_regex(next_labels)
    next_pattern <- paste(next_labels_esc, collapse = "|")
    
    pattern <- paste0(
      field_label_esc,
      "\\s*(.*?)\\s*(?=",
      next_pattern,
      "|$)"
    )
  } else {
    pattern <- paste0(field_label_esc, "\\s*(.*?)\\s*$")
  }
  
  out <- stringr::str_match(
    block,
    stringr::regex(pattern, dotall = TRUE)
  )[, 2]
  
  clean_field_value(out)
}

#..............................................................................
# detect_new_flag()
# Verifica la presenza di marcatori di nuovo inserimento
#..............................................................................
detect_new_flag <- function(text_block) {
  stringr::str_detect(text_block, fixed("(*)"))
}

#..............................................................................
# get_page_context_lines()
# Suddivide una pagina in linee informative pulite
#..............................................................................
get_page_context_lines <- function(page_text) {
  lines <- page_text |>
    clean_text() |>
    stringr::str_split("\n", simplify = FALSE) |>
    purrr::pluck(1) |>
    stringr::str_trim()
  
  lines[lines != ""]
}

#..............................................................................
# extract_last_context_from_page()
# Recupera ultimo settore / area tematica visibili nella pagina
#..............................................................................
extract_last_context_from_page <- function(page_text) {
  lines <- get_page_context_lines(page_text)
  
  settore_lines <- lines[stringr::str_detect(lines, "^Settore:")]
  area_lines    <- lines[stringr::str_detect(lines, "^Area tematica:")]
  
  settore <- if (length(settore_lines) > 0) {
    stringr::str_remove(settore_lines[length(settore_lines)], "^Settore:\\s*") |>
      clean_field_value()
  } else {
    NA_character_
  }
  
  area_tematica <- if (length(area_lines) > 0) {
    stringr::str_remove(area_lines[length(area_lines)], "^Area tematica:\\s*") |>
      clean_field_value()
  } else {
    NA_character_
  }
  
  tibble::tibble(
    settore_page = settore,
    area_tematica_page = area_tematica
  )
}

#..............................................................................
# extract_context_inside_block()
# Se il blocco contiene esplicitamente settore/area, li estrae
#..............................................................................
extract_context_inside_block <- function(block) {
  settore <- stringr::str_match(
    block,
    stringr::regex("Settore:\\s*(.*?)\\s*(?=Area tematica:|Codice lavoro:|$)", dotall = TRUE)
  )[, 2] |>
    clean_field_value()
  
  area_tematica <- stringr::str_match(
    block,
    stringr::regex("Area tematica:\\s*(.*?)\\s*(?=Codice lavoro:|$)", dotall = TRUE)
  )[, 2] |>
    clean_field_value()
  
  tibble::tibble(
    settore_block = settore,
    area_tematica_block = area_tematica
  )
}

#..............................................................................
# extract_page_number_from_block()
# Estrae il numero pagina dal marcatore PAGE_START_n
#..............................................................................
extract_page_number_from_block <- function(block) {
  page_no <- stringr::str_match(block, "PAGE_START_(\\d+)")[, 2]
  as.integer(page_no)
}

#..............................................................................
# split_into_record_blocks()
# Suddivide il testo totale in blocchi record
#
# Nota:
# la regex è volutamente pragmatica. Ogni blocco parte da un "Codice lavoro:"
# e si ferma prima del successivo.
#..............................................................................
split_into_record_blocks <- function(full_text) {
  
  starts <- stringr::str_locate_all(full_text, "Codice lavoro:")[[1]][, 1]
  
  if (length(starts) == 0) return(character())
  
  ends <- c(starts[-1] - 1, nchar(full_text))
  
  blocks <- purrr::map2_chr(starts, ends, ~ substr(full_text, .x, .y))
  
  # Per ogni blocco, recuperiamo anche il PAGE_START più vicino precedente
  page_positions <- stringr::str_locate_all(full_text, "PAGE_START_\\d+")[[1]][, 1]
  page_tokens <- stringr::str_extract_all(full_text, "PAGE_START_\\d+")[[1]]
  
  get_previous_page_token <- function(start_pos) {
    idx <- max(which(page_positions <= start_pos))
    page_tokens[idx]
  }
  
  page_token_for_block <- purrr::map_chr(starts, get_previous_page_token)
  
  paste(page_token_for_block, blocks, sep = "\n")
}

normalize_codice <- function(x) {
  x |>
    as.character() |>
    str_to_upper() |>
    str_replace_all("PAGE_START_\\d+", "") |>
    # stringr::str_replace_all("[^A-Z0-9-]", "") |>  # 🔴 rimuove TUTTI i caratteri strani
    str_replace_all("\\*", "") |>   # 🔴 QUESTO È CRUCIALE
    str_replace_all("\\s+", "") |>
    str_trim()
}

 
# 5) LETTURA PDF --------------------------------------------------------------
 

log_info("Avvio lettura PDF: ", pdf_path)

if (!file.exists(pdf_path)) {
  stop("File PDF non trovato: ", pdf_path)
}

pdf_pages_raw <- pdftools::pdf_text(pdf_path)
log_info("Numero pagine lette: ", length(pdf_pages_raw))

pdf_pages_clean <- purrr::map_chr(pdf_pages_raw, normalize_page_text)

page_context <- purrr::map_dfr(pdf_pages_clean, extract_last_context_from_page) |>
  dplyr::mutate(pagina_pdf = dplyr::row_number())

 
# 6) COSTRUZIONE TESTO COMPLESSIVO --------------------------------------------
 

pages_with_markers <- purrr::map2_chr(
  pdf_pages_clean,
  seq_along(pdf_pages_clean),
  ~ paste0("PAGE_START_", .y, "\n", .x)
)

full_text <- paste(pages_with_markers, collapse = "\n")

 
# 7) INDIVIDUAZIONE RECORD ----------------------------------------------------
 

record_blocks <- split_into_record_blocks(full_text)

if (length(record_blocks) == 0) {
  stop("Nessun record trovato. Verificare la struttura del PDF o le regex.")
}

log_info("Numero blocchi record individuati: ", length(record_blocks))

 
# 8) PARSING RECORD -----------------------------------------------------------
 

catalogo_raw <- purrr::map_dfr(record_blocks, function(block) {
  
  page_no <- extract_page_number_from_block(block)
  context_in_block <- extract_context_inside_block(block)
  
  tibble::tibble(
    pagina_pdf = page_no,
    
    settore_block = context_in_block$settore_block,
    area_tematica_block = context_in_block$area_tematica_block,
    
    codice_lavoro = extract_field(
      block,
      "Codice lavoro:",
      c("Denominazione:", "Ente titolare:", "Tipologia:", "Obiettivo:", "Origine:")
    ),
    
    denominazione = extract_field(
      block,
      "Denominazione:",
      c("Ente titolare:", "Tipologia:", "Obiettivo:", "Origine:")
    ),
    
    ente_titolare = extract_field(
      block,
      "Ente titolare:",
      c("Tipologia:", "Obiettivo:", "Origine:")
    ),
    
    tipologia = extract_field(
      block,
      "Tipologia:",
      c("Obiettivo:", "Origine:")
    ),
    
    obiettivo = extract_field(
      block,
      "Obiettivo:",
      c("Origine:")
    ),
    
    origine = extract_field(
      block,
      "Origine:",
      character()
    ),
    
    nuovo_inserimento = detect_new_flag(block),
    block_raw = block
  )
})

remove_page_markers <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\s*PAGE_START_\\d+\\s*", " ")
  x <- stringr::str_squish(x)
  x[x == ""] <- NA_character_
  x
}

 
# 9) ARRICCHIMENTO CONTESTO ---------------------------------------------------
 

catalogo <- catalogo_raw |>
  dplyr::left_join(page_context, by = "pagina_pdf") |>
  dplyr::mutate(
    settore = dplyr::coalesce(settore_block, settore_page),
    area_tematica = dplyr::coalesce(area_tematica_block, area_tematica_page)
  ) |>
  dplyr::arrange(pagina_pdf) |>
  tidyr::fill(settore, area_tematica, .direction = "down") |>
  dplyr::mutate(
    dplyr::across(
      c(
        settore, area_tematica, codice_lavoro, denominazione,
        ente_titolare, tipologia, obiettivo, origine
      ),
      clean_field_value
    )
  ) |>
  dplyr::filter(!is.na(codice_lavoro)) |>
  dplyr::mutate(
    codice_lavoro = stringr::str_replace_all(codice_lavoro, "\\s+", " "),
    codice_lavoro = stringr::str_trim(codice_lavoro),
    sezione_pdf = dplyr::if_else(
      !is.na(settore) & !is.na(area_tematica),
      paste(settore, area_tematica, sep = " | "),
      dplyr::coalesce(settore, area_tematica, NA_character_)
    )
  )


catalogo <- catalogo |>
  mutate(
    across(
      c(
        codice_lavoro, denominazione, ente_titolare,
        tipologia, obiettivo, origine,
        settore, area_tematica, sezione_pdf
      ),
      remove_page_markers
    )
  )

catalogo <- catalogo |>
  mutate(codice_lavoro_std = normalize_codice(codice_lavoro))

codici_anomali <- catalogo |>
  filter(
    !is.na(codice_lavoro),
    !grepl("^[A-Z]{3}-\\d{5}\\*?$", codice_lavoro)
  ) |>
  select(pagina_pdf, codice_lavoro, denominazione)

# 10) DEDUPLICA E PULIZIA FINALE ----------------------------------------------
 
# Strategia:
# - se stesso codice compare più volte, tieni il record con più contenuto utile
catalogo <- catalogo |>
  dplyr::mutate(
    completezza_score =
      (!is.na(settore)) +
      (!is.na(area_tematica)) +
      (!is.na(denominazione)) +
      (!is.na(ente_titolare)) +
      (!is.na(tipologia)) +
      (!is.na(obiettivo)) +
      (!is.na(origine))
  ) |>
  dplyr::arrange(codice_lavoro, dplyr::desc(completezza_score), pagina_pdf) |>
  dplyr::distinct(codice_lavoro, .keep_all = TRUE)

log_info("Numero record dopo deduplica: ", nrow(catalogo))

 
# 11) CONTROLLI QUALITÀ -------------------------------------------------------
 

controlli_qualita <- catalogo |>
  dplyr::transmute(
    pagina_pdf,
    sezione_pdf,
    codice_lavoro,
    denominazione,
    check_settore_mancante       = is.na(settore),
    check_area_mancante          = is.na(area_tematica),
    check_denominazione_mancante = is.na(denominazione),
    check_ente_mancante          = is.na(ente_titolare),
    check_tipologia_mancante     = is.na(tipologia),
    check_obiettivo_mancante     = is.na(obiettivo),
    check_origine_mancante       = is.na(origine),
    check_codice_vuoto           = is.na(codice_lavoro),
    check_codice_formato         = !is.na(codice_lavoro_std) &
      !stringr::str_detect(codice_lavoro_std, "^[A-Z]{3}-\\d{5}$"),
    check_denominazione_corta    = !is.na(denominazione) & nchar(denominazione) < 5
  ) |>
  dplyr::filter(
    check_settore_mancante |
      check_area_mancante |
      check_denominazione_mancante |
      check_ente_mancante |
      check_tipologia_mancante |
      check_obiettivo_mancante |
      check_origine_mancante |
      check_codice_vuoto |
      check_codice_formato |
      check_denominazione_corta
  )

log_info("Numero record con anomalie: ", nrow(controlli_qualita))

# 12) TABELLE DI RIEPILOGO ----------------------------------------------------
 

riepilogo <- tibble::tibble(
  indicatore = c(
    "numero_record",
    "numero_settori",
    "numero_aree_tematiche",
    "numero_enti_titolari",
    "numero_record_nuovo_inserimento",
    "numero_record_con_anomalie",
    "numero_pagine_pdf"
  ),
  valore = c(
    nrow(catalogo),
    dplyr::n_distinct(catalogo$settore, na.rm = TRUE),
    dplyr::n_distinct(catalogo$area_tematica, na.rm = TRUE),
    dplyr::n_distinct(catalogo$ente_titolare, na.rm = TRUE),
    sum(catalogo$nuovo_inserimento, na.rm = TRUE),
    nrow(controlli_qualita),
    length(pdf_pages_raw)
  )
)

pivot_settore <- catalogo |>
  dplyr::count(settore, name = "n_record", sort = TRUE)

pivot_area <- catalogo |>
  dplyr::count(settore, area_tematica, name = "n_record", sort = TRUE)

pivot_ente <- catalogo |>
  dplyr::count(ente_titolare, name = "n_record", sort = TRUE)

 
# 13) EXPORT CSV --------------------------------------------------------------
 

catalogo_export <- catalogo |>
  dplyr::select(
    pagina_pdf,
    sezione_pdf,
    settore,
    area_tematica,
    codice_lavoro,
    denominazione,
    ente_titolare,
    tipologia,
    obiettivo,
    origine,
    nuovo_inserimento
  )

readr::write_csv(catalogo_export, csv_output_path)
log_info("CSV creato: ", csv_output_path)

 
# 14) EXPORT LOG --------------------------------------------------------------
 

writeLines(log_messages, con = log_output_path)
log_info("Log scritto: ", log_output_path)

 
# 15) CREAZIONE EXCEL ---------------------------------------------------------
 

wb <- openxlsx::createWorkbook()

#..............................................................................
# Stili Excel
#..............................................................................
style_title <- openxlsx::createStyle(
  textDecoration = "bold",
  fontSize = 13
)

style_header <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom",
  wrapText = TRUE
)

style_wrap <- openxlsx::createStyle(
  wrapText = TRUE,
  valign = "top"
)

style_note <- openxlsx::createStyle(
  textDecoration = "italic",
  fontColour = "#555555"
)


#..............................................................................
# Foglio: indice
#..............................................................................
openxlsx::addWorksheet(wb, "indice", gridLines = TRUE)

indice_tbl <- tibble::tibble(
  foglio = c(
    "indice",
    "riepilogo",
    "catalogo",
    "controlli_qualita",
    "pivot_settore",
    "pivot_area",
    "pivot_ente",
    "log"
  ),
  descrizione = c(
    "Indice del file e descrizione dei fogli",
    "Indicatori sintetici dell'estrazione",
    "Catalogo completo estratto dal PDF",
    "Record che richiedono verifica manuale",
    "Conteggio record per settore",
    "Conteggio record per settore e area tematica",
    "Conteggio record per ente titolare",
    "Messaggi di log dell'elaborazione"
  )
)

openxlsx::writeData(wb, "indice", "Indice del workbook", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "indice", indice_tbl, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "indice", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "indice", style_header, rows = 3, cols = 1:ncol(indice_tbl), gridExpand = TRUE)
openxlsx::addStyle(wb, "indice", style_wrap, rows = 4:(nrow(indice_tbl) + 3), cols = 1:ncol(indice_tbl), gridExpand = TRUE)
openxlsx::setColWidths(wb, "indice", cols = 1:2, widths = c(22, 70))
openxlsx::freezePane(wb, "indice", firstActiveRow = 4, firstActiveCol = 1)

#..............................................................................
# Foglio: riepilogo
#..............................................................................
openxlsx::addWorksheet(wb, "riepilogo", gridLines = TRUE)

openxlsx::writeData(wb, "riepilogo", "Riepilogo estrazione", startRow = 1, startCol = 1)
openxlsx::writeData(
  wb, "riepilogo",
  "Questo foglio contiene indicatori sintetici del catalogo estratto.",
  startRow = 2, startCol = 1
)
openxlsx::writeData(wb, "riepilogo", riepilogo, startRow = 4, withFilter = FALSE)

openxlsx::addStyle(wb, "riepilogo", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "riepilogo", style_note, rows = 2, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "riepilogo", style_header, rows = 4, cols = 1:ncol(riepilogo), gridExpand = TRUE)
openxlsx::setColWidths(wb, "riepilogo", cols = 1:2, widths = c(40, 18))

#..............................................................................
# Foglio: catalogo
#..............................................................................
openxlsx::addWorksheet(wb, "catalogo", gridLines = TRUE)

openxlsx::writeData(wb, "catalogo", "Catalogo estratto dal PDF", startRow = 1, startCol = 1)
openxlsx::writeData(
  wb, "catalogo",
  "Tabella principale da utilizzare per analisi, filtri e verifiche.",
  startRow = 2, startCol = 1
)
openxlsx::writeData(wb, "catalogo", catalogo_export, startRow = 4, withFilter = TRUE)

openxlsx::addStyle(wb, "catalogo", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "catalogo", style_note, rows = 2, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "catalogo", style_header, rows = 4, cols = 1:ncol(catalogo_export), gridExpand = TRUE)

if (nrow(catalogo_export) > 0) {
  openxlsx::addStyle(
    wb, "catalogo", style_wrap,
    rows = 5:(nrow(catalogo_export) + 4),
    cols = 1:ncol(catalogo_export),
    gridExpand = TRUE
  )
}

openxlsx::setColWidths(wb, "catalogo", cols = 1,  widths = 10)
openxlsx::setColWidths(wb, "catalogo", cols = 2,  widths = 45)
openxlsx::setColWidths(wb, "catalogo", cols = 3,  widths = 28)
openxlsx::setColWidths(wb, "catalogo", cols = 4,  widths = 35)
openxlsx::setColWidths(wb, "catalogo", cols = 5,  widths = 15)
openxlsx::setColWidths(wb, "catalogo", cols = 6,  widths = 50)
openxlsx::setColWidths(wb, "catalogo", cols = 7,  widths = 35)
openxlsx::setColWidths(wb, "catalogo", cols = 8,  widths = 18)
openxlsx::setColWidths(wb, "catalogo", cols = 9,  widths = 90)
openxlsx::setColWidths(wb, "catalogo", cols = 10, widths = 60)
openxlsx::setColWidths(wb, "catalogo", cols = 11, widths = 16)

openxlsx::freezePane(wb, "catalogo", firstActiveRow = 5, firstActiveCol = 1)

#..............................................................................
# Foglio: controlli_qualita
#..............................................................................
openxlsx::addWorksheet(wb, "controlli_qualita", gridLines = TRUE)

openxlsx::writeData(wb, "controlli_qualita", "Record da verificare", startRow = 1, startCol = 1)
openxlsx::writeData(
  wb, "controlli_qualita",
  "Questo foglio evidenzia record incompleti o con pattern anomali.",
  startRow = 2, startCol = 1
)
openxlsx::writeData(wb, "controlli_qualita", controlli_qualita, startRow = 4, withFilter = TRUE)

openxlsx::addStyle(wb, "controlli_qualita", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "controlli_qualita", style_note, rows = 2, cols = 1, gridExpand = FALSE)

if (ncol(controlli_qualita) > 0) {
  openxlsx::addStyle(wb, "controlli_qualita", style_header, rows = 4, cols = 1:ncol(controlli_qualita), gridExpand = TRUE)
}

if (nrow(controlli_qualita) > 0) {
  openxlsx::addStyle(
    wb, "controlli_qualita", style_wrap,
    rows = 5:(nrow(controlli_qualita) + 4),
    cols = 1:ncol(controlli_qualita),
    gridExpand = TRUE
  )
}

openxlsx::setColWidths(wb, "controlli_qualita", cols = 1:ncol(controlli_qualita), widths = "auto")
openxlsx::freezePane(wb, "controlli_qualita", firstActiveRow = 5, firstActiveCol = 1)

#..............................................................................
# Foglio: pivot_settore
#..............................................................................
openxlsx::addWorksheet(wb, "pivot_settore", gridLines = TRUE)
openxlsx::writeData(wb, "pivot_settore", "Conteggio record per settore", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "pivot_settore", pivot_settore, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "pivot_settore", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "pivot_settore", style_header, rows = 3, cols = 1:ncol(pivot_settore), gridExpand = TRUE)
openxlsx::setColWidths(wb, "pivot_settore", cols = 1:2, widths = c(45, 12))

#..............................................................................
# Foglio: pivot_area
#..............................................................................
openxlsx::addWorksheet(wb, "pivot_area", gridLines = TRUE)
openxlsx::writeData(wb, "pivot_area", "Conteggio record per settore e area tematica", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "pivot_area", pivot_area, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "pivot_area", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "pivot_area", style_header, rows = 3, cols = 1:ncol(pivot_area), gridExpand = TRUE)
openxlsx::setColWidths(wb, "pivot_area", cols = 1:3, widths = c(35, 45, 12))

#..............................................................................
# Foglio: pivot_ente
#..............................................................................
openxlsx::addWorksheet(wb, "pivot_ente", gridLines = TRUE)
openxlsx::writeData(wb, "pivot_ente", "Conteggio record per ente titolare", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "pivot_ente", pivot_ente, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "pivot_ente", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "pivot_ente", style_header, rows = 3, cols = 1:ncol(pivot_ente), gridExpand = TRUE)
openxlsx::setColWidths(wb, "pivot_ente", cols = 1:2, widths = c(45, 12))

#..............................................................................
# Foglio: log
#..............................................................................
openxlsx::addWorksheet(wb, "log", gridLines = TRUE)
log_tbl <- tibble::tibble(log = log_messages)

openxlsx::writeData(wb, "log", "Log esecuzione", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "log", log_tbl, startRow = 3, withFilter = FALSE)
openxlsx::addStyle(wb, "log", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "log", style_header, rows = 3, cols = 1, gridExpand = TRUE)
openxlsx::setColWidths(wb, "log", cols = 1, widths = 120)

 
# 16) ORDINE FOGLI E SALVATAGGIO ----------------------------------------------
 

desired_order <- c(
  "indice",
  "riepilogo",
  "catalogo",
  "controlli_qualita",
  "pivot_settore",
  "pivot_area",
  "pivot_ente",
  "log"
)

current_sheets <- names(wb)
sheet_indices <- match(desired_order, current_sheets)
wb$sheetOrder <- sheet_indices

openxlsx::saveWorkbook(wb, excel_output_path, overwrite = TRUE)
log_info("Excel creato: ", excel_output_path)

 
# 17) REPORT FINALE -----------------------------------------------------------
 
log_info("Elaborazione completata con successo.")
log_info("Record finali: ", nrow(catalogo))
log_info("Settori distinti: ", dplyr::n_distinct(catalogo$settore, na.rm = TRUE))
log_info("Aree tematiche distinte: ", dplyr::n_distinct(catalogo$area_tematica, na.rm = TRUE))
log_info("Enti distinti: ", dplyr::n_distinct(catalogo$ente_titolare, na.rm = TRUE))

cat("\n============================================================\n")
cat("ESTRAZIONE COMPLETATA\n")
cat("Excel:", excel_output_path, "\n")
cat("CSV:  ", csv_output_path, "\n")
cat("Log:  ", log_output_path, "\n")
cat("============================================================\n")


# 18) ULTERIORI CONTROLLI QUALITÀ -----------------------------------------------------------

# COMPLETEZZA CAMPO
# Non solo quanti NA, ma anche quanti valori vuoti, troppo corti o sospetti.

controllo_completezza <- catalogo |>
  summarise(
    n_record = n(),
    settore_na = sum(is.na(settore)),
    area_na = sum(is.na(area_tematica)),
    codice_na = sum(is.na(codice_lavoro)),
    denominazione_na = sum(is.na(denominazione)),
    ente_na = sum(is.na(ente_titolare)),
    tipologia_na = sum(is.na(tipologia)),
    obiettivo_na = sum(is.na(obiettivo)),
    origine_na = sum(is.na(origine))
  )

controllo_lunghezze <- catalogo |>
  mutate(
    nchar_denominazione = nchar(denominazione),
    nchar_obiettivo = nchar(obiettivo),
    nchar_origine = nchar(origine)
  ) |>
  summarise(
    denom_troppo_corta = sum(!is.na(nchar_denominazione) & nchar_denominazione < 5),
    obiettivo_troppo_corto = sum(!is.na(nchar_obiettivo) & nchar_obiettivo < 15),
    origine_troppo_corta = sum(!is.na(nchar_origine) & nchar_origine < 5)
  )

# UNICITÀ DEL CODICE LAVORO
# Se codice_lavoro è la chiave logica, non dovrebbero esserci duplicati reali.

duplicati_codice <- catalogo |>
  count(codice_lavoro, sort = TRUE) |>
  filter(!is.na(codice_lavoro), n > 1)

duplicati_dettaglio <- catalogo |>
  semi_join(duplicati_codice, by = "codice_lavoro") |>
  arrange(codice_lavoro, pagina_pdf)

# CONTROLLO DEL FORMATO DEI CODICI

catalogo |>
  count(codice_lavoro, sort = TRUE) |>
  mutate(formato_ok = grepl("^[A-Z]{3}-\\d{5}$", codice_lavoro))

# ESTRARRE CASI NON CONFORMI DA ISPEZIONARE MANUALMENTE
codici_anomali <- catalogo |>
  filter(
    !is.na(codice_lavoro),
    !grepl("^[A-Z]{3}-\\d{5}$", codice_lavoro)
  ) |>
  select(pagina_pdf, codice_lavoro, denominazione)

# DIZIONARI CHIUSI O QUASI-CHIUSI
catalogo |>
  count(tipologia, sort = TRUE)

tipologie_attese <- c(
  "SDA", "SIS", "STU", "VAL"
)

tipologie_anomale <- catalogo |>
  filter(!is.na(tipologia), !tipologia %in% tipologie_attese) |>
  select(pagina_pdf, codice_lavoro, tipologia, denominazione)

# COERENZA GERARCHICA SETTORE > AREA TEMATICA

coerenza_area_settore <- catalogo |>
  distinct(settore, area_tematica) |>
  count(area_tematica, name = "n_settori") |>
  filter(!is.na(area_tematica), n_settori > 1)

dettaglio_coerenza_area_settore <- catalogo |>
  semi_join(coerenza_area_settore, by = "area_tematica") |>
  distinct(settore, area_tematica) |>
  arrange(area_tematica, settore)


# PRESENZA DI LABEL RESIDUE DENTRO I CAMPI

label_residue <- catalogo |>
  mutate(
    problema_denominazione = grepl("Ente titolare:|Tipologia:|Obiettivo:|Origine:", denominazione),
    problema_ente = grepl("Tipologia:|Obiettivo:|Origine:", ente_titolare),
    problema_tipologia = grepl("Obiettivo:|Origine:", tipologia),
    problema_obiettivo = grepl("Codice lavoro:|Denominazione:|Ente titolare:|Tipologia:", obiettivo),
    problema_origine = grepl("Codice lavoro:|Denominazione:|Ente titolare:|Tipologia:|Obiettivo:", origine)
  ) |>
  filter(
    problema_denominazione |
      problema_ente |
      problema_tipologia |
      problema_obiettivo |
      problema_origine
  )

# CONTROLLO SUI CARATTERI SPORCHI
caratteri_sospetti <- catalogo |>
  mutate(
    testo_unito = paste(
      settore, area_tematica, codice_lavoro, denominazione,
      ente_titolare, tipologia, obiettivo, origine,
      sep = " | "
    )
  ) |>
  filter(grepl("GAZZETTA UFFICIALE|Supplemento ordinario|PAGE_START_|— [0-9]+ —", testo_unito)) |>
  select(pagina_pdf, codice_lavoro, denominazione, testo_unito)

# CONTROLLO LUNGHEZZE ESTREME
outlier_lunghezze <- catalogo |>
  mutate(
    len_denominazione = nchar(denominazione),
    len_ente = nchar(ente_titolare),
    len_tipologia = nchar(tipologia),
    len_obiettivo = nchar(obiettivo),
    len_origine = nchar(origine)
  ) |>
  filter(
    len_denominazione > 300 |
      len_ente > 200 |
      len_tipologia > 100 |
      len_obiettivo > 3000 |
      len_origine > 1500
  )

# CONTROLLO COPERTURA RISPETTO AL PDF
n_blocchi_pdf <- length(record_blocks)
n_record_catalogo <- nrow(catalogo)

copertura <- tibble(
  metrica = c("blocchi_pdf", "record_catalogo", "differenza"),
  valore = c(n_blocchi_pdf, n_record_catalogo, n_blocchi_pdf - n_record_catalogo)
)

# VALIDAZIONE MANUALE SUL CAMPIONE
set.seed(123)
campione_revisione <- catalogo |>
  sample_n(min(30, nrow(catalogo))) |>
  select(
    pagina_pdf, settore, area_tematica, codice_lavoro,
    denominazione, ente_titolare, tipologia
  )

# # su campione stratificato
# campione_stratificato <- catalogo |>
#   group_by(settore) |>
#   slice_sample(n = min(5, n())) |>
#   ungroup()


# INDICE SINTETICO DI QUALITÀ

catalogo_qc <- catalogo |>
  mutate(
    qc_codice_presente = !is.na(codice_lavoro),
    qc_denominazione_presente = !is.na(denominazione),
    qc_ente_presente = !is.na(ente_titolare),
    qc_tipologia_presente = !is.na(tipologia),
    qc_obiettivo_presente = !is.na(obiettivo),
    qc_origine_presente = !is.na(origine),
    qc_settore_presente = !is.na(settore),
    qc_area_presente = !is.na(area_tematica),
    qc_label_residue = !(
      grepl("Ente titolare:|Tipologia:|Obiettivo:|Origine:", denominazione) |
        grepl("Tipologia:|Obiettivo:|Origine:", ente_titolare) |
        grepl("Obiettivo:|Origine:", tipologia) |
        grepl("Codice lavoro:|Denominazione:|Ente titolare:|Tipologia:", obiettivo) |
        grepl("Codice lavoro:|Denominazione:|Ente titolare:|Tipologia:|Obiettivo:", origine)
    ),
    qc_score =
      qc_codice_presente +
      qc_denominazione_presente +
      qc_ente_presente +
      qc_tipologia_presente +
      qc_obiettivo_presente +
      qc_origine_presente +
      qc_settore_presente +
      qc_area_presente +
      qc_label_residue
  )

# isolare record peggiori
record_critici <- catalogo_qc |>
  filter(qc_score <= 8) |>
  arrange(qc_score, pagina_pdf)


# 19) JOIN  selezione indagini istituzioni_PSN_IST.csv -------------------------

# "ISTAT - monitoraggio riforme PA/materiali progetto Istat/selezione indagini istituzioni_PSN_IST.csv"

SEL_IND_ISTAT <- readr::read_csv2("ISTAT - monitoraggio riforme PA/materiali progetto Istat/selezione indagini istituzioni_PSN_IST.csv", show_col_types = FALSE) |>
  dplyr::filter(!if_all(dplyr::everything(), ~ is.na(.x) | .x == ""))

SEL_IND_ISTAT <- SEL_IND_ISTAT |>
  mutate(codice_lavoro_std = normalize_codice(CODICE))

duplicati_SEL_IND_ISTAT <- SEL_IND_ISTAT |>
  count(codice_lavoro_std, sort = TRUE) |>
  filter(!is.na(codice_lavoro_std), n > 1)

codici_sel_presenti <- SEL_IND_ISTAT |>
  filter(!is.na(codice_lavoro_std)) |>
  distinct(codice_lavoro_std)

catalogo <- catalogo |>
  mutate(
    presente_in_SEL_IND_ISTAT = if_else(
      codice_lavoro_std %in% codici_sel_presenti$codice_lavoro_std,
      1L,
      0L
    )
  )


readr::write_csv(catalogo, csv_output_path)
log_info("CSV creato: ", csv_output_path)


SEL_IND_ISTAT_unico <- SEL_IND_ISTAT |>
  distinct(codice_lavoro_std, .keep_all = TRUE)


summary(catalogo)
str(catalogo)
str(catalogo_merge)
summary(as.factor(catalogo_merge$presente_altro_csv))

sum(catalogo$codice_lavoro_std %in% SEL_IND_ISTAT$codice_lavoro_std)

catalogo_merge <- catalogo |>
  mutate(
    presente_altro_csv = if_else(
      codice_lavoro_std %in% SEL_IND_ISTAT$codice_lavoro_std,
      # codice_lavoro_std %in% SEL_IND_ISTAT_unico$codice_lavoro_std,
      1L,
      0L
    )
  ) #|>
  # left_join(
  #   altro_unico |>
  #     select(codice_lavoro_std, fonte_dati, anno_riferimento, note),
  #   by = "codice_lavoro_std"
  # )

# TABELLE DI CONTROLLO DEL MERGE

check_merge <- tibble(
  n_catalogo = nrow(catalogo),
  n_SEL_IND_ISTAT = nrow(SEL_IND_ISTAT_unico),
  match_catalogo = sum(catalogo$codice_lavoro_std %in% SEL_IND_ISTAT_unico$codice_lavoro_std),
  match_SEL_IND_ISTAT = sum(SEL_IND_ISTAT_unico$codice_lavoro_std %in% catalogo$codice_lavoro_std),
  non_match_SEL_IND_ISTAT = sum(!SEL_IND_ISTAT_unico$codice_lavoro_std %in% catalogo$codice_lavoro_std)
)

check_merge

summary_merge <- tibble(
  n_catalogo = nrow(catalogo),
  n_SEL_IND_ISTAT = nrow(SEL_IND_ISTAT_unico),
  n_match = sum(catalogo_merge$presente_SEL_IND_ISTAT_csv, na.rm = TRUE),
  n_non_match = sum(catalogo_merge$presente_SEL_IND_ISTAT_csv == 0, na.rm = TRUE)
)

summary_merge

non_match_catalogo <- catalogo_merge |>
  filter(presente_SEL_IND_ISTAT_csv == 0) |>
  select(codice_lavoro, denominazione, settore, area_tematica)

solo_SEL_IND_ISTAT <- SEL_IND_ISTAT_unico |>
  filter(!codice_lavoro_std %in% catalogo$codice_lavoro_std)

# EXPORT FINALE
readr::write_csv(catalogo_merge, "catalogo_merge.csv")
readr::write_csv(non_match_catalogo, "catalogo_non_match.csv")
readr::write_csv(solo_SEL_IND_ISTAT, "solo_SEL_IND_ISTAT_csv.csv")
