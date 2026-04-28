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

 
rm(list=ls())

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
# collect_multiline_value()
# Funzione di supporto a extract_last_context_from_page
#..............................................................................
collect_multiline_value <- function(lines, start_index, label_pattern, stop_patterns, max_extra_lines = 2) {
  if (length(start_index) == 0 || is.na(start_index)) return(NA_character_)
  
  first_part <- stringr::str_remove(lines[start_index], label_pattern)
  parts <- c(first_part)
  
  i <- start_index + 1
  extra_count <- 0
  
  while (i <= length(lines) && extra_count < max_extra_lines) {
    current_line <- stringr::str_trim(lines[i])
    
    if (any(stringr::str_detect(current_line, stop_patterns))) {
      break
    }
    
    if (stringr::str_detect(current_line, "^\\d+$")) {
      i <- i + 1
      next
    }
    
    if (current_line == "") {
      i <- i + 1
      next
    }
    
    parts <- c(parts, current_line)
    extra_count <- extra_count + 1
    i <- i + 1
  }
  
  parts |>
    paste(collapse = " ") |>
    clean_field_value()
}

#..............................................................................
# extract_last_context_from_page()
# Recupera ultimo settore / area tematica visibili nella pagina
#..............................................................................

extract_last_context_from_page <- function(page_text) {
  lines <- get_page_context_lines(page_text)
  
  # pattern di stop: quando parte una nuova sezione o un campo record
  stop_patterns <- c(
    "^Settore:",
    "^Area tematica:",
    "^Codice lavoro:",
    "^Denominazione:",
    "^Ente titolare:",
    "^Tipologia:",
    "^Obiettivo:",
    "^Origine:"
  )
  
  # prendi l'ultima occorrenza nella pagina, ma cattura anche le righe successive
  settore_idx <- which(stringr::str_detect(lines, "^Settore:"))
  area_idx    <- which(stringr::str_detect(lines, "^Area tematica:"))
  
  settore <- if (length(settore_idx) > 0) {
    collect_multiline_value(
      lines = lines,
      start_index = settore_idx[length(settore_idx)],
      label_pattern = "^Settore:\\s*",
      stop_patterns = stop_patterns
    )
  } else {
    NA_character_
  }
  
  area_tematica <- if (length(area_idx) > 0) {
    collect_multiline_value(
      lines = lines,
      start_index = area_idx[length(area_idx)],
      label_pattern = "^Area tematica:\\s*",
      stop_patterns = stop_patterns
    )
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

normalize_codice_match_psn <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("PAGE_START_\\d+", "") |>
    stringr::str_replace_all("[[:space:]]+", "") |>
    stringr::str_replace_all("[*∗•⋆]", "") |>
    stringr::str_replace("/.*$", "") |>
    stringr::str_replace_all("[^A-Z0-9_-]", "") |>
    stringr::str_trim()
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

#..............................................................................
# 9) ARRICCHIMENTO CONTESTO E PULIZIA -----------------------------------------
#..............................................................................

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
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        codice_lavoro, denominazione, ente_titolare,
        tipologia, obiettivo, origine,
        settore, area_tematica, sezione_pdf
      ),
      remove_page_markers
    )
  ) |>
  dplyr::mutate(
    codice_lavoro_std = normalize_codice(codice_lavoro)
  )

# Controllo diagnostico sulla chiave originale
codici_anomali <- catalogo |>
  dplyr::filter(
    !is.na(codice_lavoro_std),
    !stringr::str_detect(codice_lavoro_std, "^[A-Z]{3}-\\d{5}$")
  ) |>
  dplyr::select(pagina_pdf, codice_lavoro, codice_lavoro_std, denominazione)

log_info("Codici anomali dopo normalizzazione: ", nrow(codici_anomali))

#..............................................................................
# 10) DEDUPLICA E PULIZIA FINALE ----------------------------------------------
#..............................................................................

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
  dplyr::arrange(codice_lavoro_std, dplyr::desc(completezza_score), pagina_pdf) |>
  dplyr::distinct(codice_lavoro_std, .keep_all = TRUE)

log_info("Numero record dopo deduplica: ", nrow(catalogo))

#..............................................................................
# 11) JOIN SEL_IND_ISTAT ------------------------------------------------------
#..............................................................................

# Percorso file esterno
sel_ind_path <- "ISTAT - monitoraggio riforme PA/materiali progetto Istat/selezione indagini istituzioni_PSN_IST.csv"

# Funzione specifica per il match con PSN:
# rimuove eventuali suffissi tipo /3, /1, ecc.
# normalize_codice_match_psn <- function(x) {
#   x |>
#     as.character() |>
#     stringr::str_to_upper() |>
#     stringr::str_replace_all("PAGE_START_\\d+", "") |>
#     stringr::str_replace_all("[[:space:]]+", "") |>
#     stringr::str_replace_all("[*∗•⋆]", "") |>
#     stringr::str_replace("/.*$", "") |>
#     stringr::str_replace_all("[^A-Z0-9_-]", "") |>
#     stringr::str_trim()
# }

# Chiave match anche sul catalogo PSN
catalogo <- catalogo |>
  dplyr::mutate(
    codice_lavoro_match_psn = normalize_codice_match_psn(codice_lavoro)
  )

SEL_IND_ISTAT <- readr::read_csv2(
  sel_ind_path,
  show_col_types = FALSE
) |>
  dplyr::filter(
    !if_all(
      dplyr::everything(),
      ~ is.na(.) | (is.character(.) & stringr::str_trim(.) == "")
    )
  ) |>
  dplyr::mutate(
    codice_lavoro_std = normalize_codice(CODICE),
    codice_lavoro_match_psn = normalize_codice_match_psn(CODICE)
  )

log_info("Righe lette da SEL_IND_ISTAT (al netto righe vuote): ", nrow(SEL_IND_ISTAT))

# Diagnostica sui codici con suffisso /...
codici_con_suffix <- SEL_IND_ISTAT |>
  dplyr::filter(
    !is.na(CODICE),
    stringr::str_detect(as.character(CODICE), "/")
  ) |>
  dplyr::select(CODICE, codice_lavoro_std, codice_lavoro_match_psn)

log_info("Codici SEL_IND_ISTAT con suffisso '/...': ", nrow(codici_con_suffix))

# Duplicati sul codice standard "pieno"
duplicati_SEL_IND_ISTAT_std <- SEL_IND_ISTAT |>
  dplyr::count(codice_lavoro_std, sort = TRUE) |>
  dplyr::filter(!is.na(codice_lavoro_std), codice_lavoro_std != "", n > 1)

log_info("Codici duplicati in SEL_IND_ISTAT (chiave std): ", nrow(duplicati_SEL_IND_ISTAT_std))

# Duplicati sulla chiave di match col PSN
duplicati_SEL_IND_ISTAT_match <- SEL_IND_ISTAT |>
  dplyr::count(codice_lavoro_match_psn, sort = TRUE) |>
  dplyr::filter(!is.na(codice_lavoro_match_psn), codice_lavoro_match_psn != "", n > 1)

log_info("Codici duplicati in SEL_IND_ISTAT (chiave match PSN): ", nrow(duplicati_SEL_IND_ISTAT_match))

# Lista unica codici per la dummy di presenza
codici_sel_presenti_match <- SEL_IND_ISTAT |>
  dplyr::filter(!is.na(codice_lavoro_match_psn), codice_lavoro_match_psn != "") |>
  dplyr::distinct(codice_lavoro_match_psn)

# Tabella aggregata per futuri join di variabili
SEL_IND_ISTAT_join <- SEL_IND_ISTAT |>
  dplyr::filter(!is.na(codice_lavoro_match_psn), codice_lavoro_match_psn != "") |>
  dplyr::group_by(codice_lavoro_match_psn) |>
  dplyr::summarise(
    n_occorrenze_SEL_IND_ISTAT = dplyr::n(),
    n_codici_originali_SEL_IND_ISTAT = dplyr::n_distinct(CODICE),
    codici_originali_SEL_IND_ISTAT = paste(sort(unique(CODICE)), collapse = " | "),
    .groups = "drop"
  )

# Join finale sul catalogo
catalogo <- catalogo |>
  dplyr::mutate(
    presente_in_SEL_IND_ISTAT = dplyr::if_else(
      codice_lavoro_match_psn %in% codici_sel_presenti_match$codice_lavoro_match_psn,
      1L,
      0L
    )
  ) |>
  dplyr::left_join(
    SEL_IND_ISTAT_join,
    by = "codice_lavoro_match_psn"
  ) |>
  dplyr::mutate(
    n_occorrenze_SEL_IND_ISTAT = dplyr::coalesce(n_occorrenze_SEL_IND_ISTAT, 0L),
    n_codici_originali_SEL_IND_ISTAT = dplyr::coalesce(n_codici_originali_SEL_IND_ISTAT, 0L),
    presente_su_PSN_anche_in_SEL_IND_ISTAT = dplyr::if_else(
      presente_in_SEL_IND_ISTAT == 1L,
      "Sì",
      "No"
    )
  )

# Tabelle di controllo del merge
check_merge <- tibble::tibble(
  metrica = c(
    "n_catalogo",
    "n_SEL_IND_ISTAT_righe",
    "n_SEL_IND_ISTAT_codici_std_unici",
    "n_SEL_IND_ISTAT_codici_match_psn_unici",
    "match_catalogo_su_SEL_IND_ISTAT",
    "match_SEL_IND_ISTAT_su_catalogo",
    "non_match_SEL_IND_ISTAT_su_catalogo"
  ),
  valore = c(
    nrow(catalogo),
    nrow(SEL_IND_ISTAT),
    dplyr::n_distinct(SEL_IND_ISTAT$codice_lavoro_std, na.rm = TRUE),
    nrow(codici_sel_presenti_match),
    sum(catalogo$presente_in_SEL_IND_ISTAT, na.rm = TRUE),
    sum(codici_sel_presenti_match$codice_lavoro_match_psn %in% catalogo$codice_lavoro_match_psn),
    sum(!codici_sel_presenti_match$codice_lavoro_match_psn %in% catalogo$codice_lavoro_match_psn)
  )
)

non_match_catalogo_SEL_IND_ISTAT <- catalogo |>
  dplyr::filter(presente_in_SEL_IND_ISTAT == 0) |>
  dplyr::select(
    pagina_pdf, settore, area_tematica, codice_lavoro,
    codice_lavoro_std, codice_lavoro_match_psn,
    denominazione
  )

solo_SEL_IND_ISTAT <- codici_sel_presenti_match |>
  dplyr::filter(!codice_lavoro_match_psn %in% catalogo$codice_lavoro_match_psn)


#..............................................................................
# 12) CONTROLLI QUALITÀ -------------------------------------------------------
#..............................................................................

controlli_qualita <- catalogo |>
  dplyr::transmute(
    pagina_pdf,
    sezione_pdf,
    codice_lavoro,
    codice_lavoro_std,
    codice_lavoro_match_psn,
    denominazione,
    presente_in_SEL_IND_ISTAT,
    presente_su_PSN_anche_in_SEL_IND_ISTAT,
    n_occorrenze_SEL_IND_ISTAT,
    check_settore_mancante       = is.na(settore),
    check_area_mancante          = is.na(area_tematica),
    check_denominazione_mancante = is.na(denominazione),
    check_ente_mancante          = is.na(ente_titolare),
    check_tipologia_mancante     = is.na(tipologia),
    check_obiettivo_mancante     = is.na(obiettivo),
    check_origine_mancante       = is.na(origine),
    check_codice_vuoto           = is.na(codice_lavoro_std) | codice_lavoro_std == "",
    check_codice_formato         = !is.na(codice_lavoro_match_psn) &
      !stringr::str_detect(codice_lavoro_match_psn, "^[A-Z]{3}-\\d{5}$"),
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

#..............................................................................
# 13) ULTERIORI CONTROLLI QUALITÀ ---------------------------------------------
#..............................................................................

controllo_completezza <- catalogo |>
  dplyr::summarise(
    n_record = dplyr::n(),
    settore_na = sum(is.na(settore)),
    area_na = sum(is.na(area_tematica)),
    codice_na = sum(is.na(codice_lavoro)),
    codice_std_na = sum(is.na(codice_lavoro_std)),
    denominazione_na = sum(is.na(denominazione)),
    ente_na = sum(is.na(ente_titolare)),
    tipologia_na = sum(is.na(tipologia)),
    obiettivo_na = sum(is.na(obiettivo)),
    origine_na = sum(is.na(origine))
  )

controllo_lunghezze <- catalogo |>
  dplyr::mutate(
    nchar_denominazione = nchar(denominazione),
    nchar_obiettivo = nchar(obiettivo),
    nchar_origine = nchar(origine)
  ) |>
  dplyr::summarise(
    denom_troppo_corta = sum(!is.na(nchar_denominazione) & nchar_denominazione < 5),
    obiettivo_troppo_corto = sum(!is.na(nchar_obiettivo) & nchar_obiettivo < 15),
    origine_troppo_corta = sum(!is.na(nchar_origine) & nchar_origine < 5)
  )

duplicati_codice <- catalogo |>
  dplyr::count(codice_lavoro_std, sort = TRUE) |>
  dplyr::filter(!is.na(codice_lavoro_std), n > 1)

duplicati_dettaglio <- catalogo |>
  dplyr::semi_join(duplicati_codice, by = "codice_lavoro_std") |>
  dplyr::arrange(codice_lavoro_std, pagina_pdf)

tipologie_osservate <- catalogo |>
  dplyr::count(tipologia, sort = TRUE)

label_residue <- catalogo |>
  dplyr::mutate(
    problema_denominazione = grepl("Ente titolare:|Tipologia:|Obiettivo:|Origine:", denominazione),
    problema_ente = grepl("Tipologia:|Obiettivo:|Origine:", ente_titolare),
    problema_tipologia = grepl("Obiettivo:|Origine:", tipologia),
    problema_obiettivo = grepl("Codice lavoro:|Denominazione:|Ente titolare:|Tipologia:", obiettivo),
    problema_origine = grepl("Codice lavoro:|Denominazione:|Ente titolare:|Tipologia:|Obiettivo:", origine)
  ) |>
  dplyr::filter(
    problema_denominazione |
      problema_ente |
      problema_tipologia |
      problema_obiettivo |
      problema_origine
  )

caratteri_sospetti <- catalogo |>
  dplyr::mutate(
    testo_unito = paste(
      settore, area_tematica, codice_lavoro, denominazione,
      ente_titolare, tipologia, obiettivo, origine,
      sep = " | "
    )
  ) |>
  dplyr::filter(grepl("GAZZETTA UFFICIALE|Supplemento ordinario|PAGE_START_|— [0-9]+ —", testo_unito)) |>
  dplyr::select(pagina_pdf, codice_lavoro, codice_lavoro_std, denominazione, testo_unito)

outlier_lunghezze <- catalogo |>
  dplyr::mutate(
    len_denominazione = nchar(denominazione),
    len_ente = nchar(ente_titolare),
    len_tipologia = nchar(tipologia),
    len_obiettivo = nchar(obiettivo),
    len_origine = nchar(origine)
  ) |>
  dplyr::filter(
    len_denominazione > 300 |
      len_ente > 200 |
      len_tipologia > 100 |
      len_obiettivo > 3000 |
      len_origine > 1500
  )

n_blocchi_pdf <- length(record_blocks)
n_record_catalogo <- nrow(catalogo)

copertura <- tibble::tibble(
  metrica = c("blocchi_pdf", "record_catalogo", "differenza"),
  valore = c(n_blocchi_pdf, n_record_catalogo, n_blocchi_pdf - n_record_catalogo)
)

set.seed(123)
campione_revisione <- catalogo |>
  dplyr::slice_sample(n = min(30, nrow(catalogo))) |>
  dplyr::select(
    pagina_pdf, settore, area_tematica, codice_lavoro,
    codice_lavoro_std, denominazione, ente_titolare, tipologia,
    presente_in_SEL_IND_ISTAT
  )

catalogo_qc <- catalogo |>
  dplyr::mutate(
    qc_codice_presente = !is.na(codice_lavoro_std) & codice_lavoro_std != "",
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

record_critici <- catalogo_qc |>
  dplyr::filter(qc_score <= 8) |>
  dplyr::arrange(qc_score, pagina_pdf)

#..............................................................................
# 14) TABELLE DI RIEPILOGO ----------------------------------------------------
#..............................................................................

riepilogo <- tibble::tibble(
  indicatore = c(
    "numero_record",
    "numero_settori",
    "numero_aree_tematiche",
    "numero_enti_titolari",
    "numero_record_nuovo_inserimento",
    "numero_record_con_anomalie",
    "numero_pagine_pdf",
    "numero_record_presenti_in_SEL_IND_ISTAT",
    "numero_record_non_presenti_in_SEL_IND_ISTAT"
  ),
  valore = c(
    nrow(catalogo),
    dplyr::n_distinct(catalogo$settore, na.rm = TRUE),
    dplyr::n_distinct(catalogo$area_tematica, na.rm = TRUE),
    dplyr::n_distinct(catalogo$ente_titolare, na.rm = TRUE),
    sum(catalogo$nuovo_inserimento, na.rm = TRUE),
    nrow(controlli_qualita),
    length(pdf_pages_raw),
    sum(catalogo$presente_in_SEL_IND_ISTAT, na.rm = TRUE),
    sum(catalogo$presente_in_SEL_IND_ISTAT == 0, na.rm = TRUE)
  )
)

pivot_settore <- catalogo |>
  dplyr::count(settore, name = "n_record", sort = TRUE)


catalogo |>
  dplyr::count(ente_titolare, sort = TRUE)

pivot_area <- catalogo |>
  dplyr::mutate(
    ente_titolare_std = stringr::str_squish(stringr::str_to_lower(ente_titolare)),
    is_istat = ente_titolare_std == stringr::str_to_lower("Istat - Istituto nazionale di statistica")
  ) |>
# pivot_area <- catalogo |>
#   dplyr::mutate(
#     is_istat = ente_titolare == "Istat - Istituto nazionale di statistica"
#   ) |>
  dplyr::group_by(settore, area_tematica) |>
  dplyr::summarise(
    n_record = dplyr::n(),
    n_istat = sum(is_istat, na.rm = TRUE),
    n_non_istat = sum(!is_istat, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(settore, area_tematica)

pivot_ente <- catalogo |>
  dplyr::count(ente_titolare, name = "n_record", sort = TRUE)

pivot_sel_ind <- catalogo |>
  dplyr::count(presente_in_SEL_IND_ISTAT, name = "n_record", sort = TRUE)



#==============================================================================
# 14B) DATA CATALOG E TABELLE DI LAVORO ---------------------------------------
#==============================================================================

#..............................................................................
# Import Data Catalog
# Nota:
# - separatore ; 
# - encoding latin1/cp1252
# - eliminiamo colonne vuote finali
#..............................................................................
data_catalog_path <- "All_1_catalogo.csv"

data_catalog_raw <- readr::read_csv2(
  data_catalog_path,
  locale = readr::locale(encoding = "Latin1"),
  show_col_types = FALSE,
  trim_ws = TRUE
)

# rimuove righe completamente vuote
data_catalog_raw <- data_catalog_raw |>
  dplyr::filter(
    !if_all(
      dplyr::everything(),
      ~ is.na(.) | (is.character(.) & stringr::str_trim(.) == "")
    )
  )

# rimuove eventuali colonne unnamed/vuote
data_catalog_raw <- data_catalog_raw |>
  dplyr::select(
    where(~ !all(is.na(.x))) &
      !matches("^Unnamed")
  )

# Controllo numero colonne atteso
expected_ncol <- 16
if (ncol(data_catalog_raw) != expected_ncol) {
  stop(
    paste0(
      "Il data catalog ha ", ncol(data_catalog_raw),
      " colonne, ma lo script si aspetta ", expected_ncol,
      ". Verificare intestazioni e struttura del file."
    )
  )
}

# Rinomina con nomi tecnici coerenti col codice
data_catalog <- data_catalog_raw
names(data_catalog) <- c(
  "fonte_ente_gestore",
  "denominazione_data_catalog",
  "url_fonte",
  "url_accesso_dati",
  "ambito_riferimento",
  "livello_pa_disaggregato",
  "unita_osservazione",
  "contenuto_informativo",
  "tematica",
  "annualita_disponibili",
  "periodicita",
  "ultimo_aggiornamento",
  "processo_rilevazione_acquisizione_produzione_dati",
  "documentazione_metodologica",
  "formato_dati",
  "note"
)

#..............................................................................
# Funzioni di normalizzazione testo per match "soft"
#..............................................................................
normalize_text_match <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[[:punct:]]", " ") |>
    stringr::str_replace_all("[[:space:]]+", " ") |>
    stringr::str_squish()
}

#..............................................................................
# Liste codici PA Istat / non Istat
# NB: usiamo la chiave normalizzata per il match sul catalogo PSN
#..............................................................................
codici_pa_istat <- c(
  "IST-00229",
  "IST-00232",
  "IST-00233",
  "IST-01944",
  "IST-02082",
  "IST-02397",
  "IST-02538",
  "IST-02575",
  "IST-02683",
  "IST-02719",
  "IST-02817"
) |>
  normalize_codice_match_psn()

codici_pa_non_istat <- c(
  "ACT-00007",
  "IAI-0017",
  "IAP-00025",
  "INT-00001",
  "INT-00022",
  "INT-00063",
  "IPS-00092",
  "MAE-00005",
  "MGG-00109",
  "MID-00004",
  "MID-00008",
  "MUR-00034",
  "PAT-00027",
  "PAT-00033",
  "PCM-00030",
  "PRM-00003",
  "TES-00021",
  "TES-00022",
  "TES-00023",
  "TES-00024",
  "TES-00033",
  "TES-00034",
  "TES-00035",
  "UCC-00010"
) |>
  normalize_codice_match_psn()

#..............................................................................
# Tabelle filtrate del catalogo PSN: PA Istat / PA non Istat
#..............................................................................
catalogo_pa_istat <- catalogo |>
  dplyr::filter(codice_lavoro_match_psn %in% codici_pa_istat) |>
  dplyr::arrange(codice_lavoro_match_psn) |>
  dplyr::select(
    pagina_pdf,
    settore,
    area_tematica,
    codice_lavoro,
    codice_lavoro_std,
    codice_lavoro_match_psn,
    denominazione,
    ente_titolare,
    tipologia,
    obiettivo,
    origine,
    presente_in_SEL_IND_ISTAT,
    presente_su_PSN_anche_in_SEL_IND_ISTAT
  )

catalogo_pa_non_istat <- catalogo |>
  dplyr::filter(codice_lavoro_match_psn %in% codici_pa_non_istat) |>
  dplyr::arrange(codice_lavoro_match_psn) |>
  dplyr::select(
    pagina_pdf,
    settore,
    area_tematica,
    codice_lavoro,
    codice_lavoro_std,
    codice_lavoro_match_psn,
    denominazione,
    ente_titolare,
    tipologia,
    obiettivo,
    origine,
    presente_in_SEL_IND_ISTAT,
    presente_su_PSN_anche_in_SEL_IND_ISTAT
  )

pivot_pa_fonti <- tibble::tibble(
  tipo_fonte = c("Istat", "Altri enti"),
  n_record = c(nrow(catalogo_pa_istat), nrow(catalogo_pa_non_istat))
)

#..............................................................................
# Fonti non Istat PA selezionate dal catalogo PSN
#..............................................................................
fonti_non_istat_tbl <- catalogo_pa_non_istat |>
  dplyr::mutate(
    denominazione_norm = normalize_text_match(denominazione)
  )

#..............................................................................
# Rilevazioni Istat target
# NB: includo anche i codici che hai indicato negli screenshot
#..............................................................................
istat_target <- catalogo_pa_istat |>
  dplyr::transmute(
    codice_target = codice_lavoro,
    codice_target_match = codice_lavoro_match_psn,
    denominazione_target = denominazione,
    periodicita = NA_character_,
    tipologia_unita_campione = NA_character_,
    psn_target = denominazione,
    denominazione_target_norm = normalize_text_match(denominazione)
  )

#..............................................................................
# Data catalog: filtro ragionato per temi vicini alla PA
# NB: qui NON facciamo ancora matching definitivo col PSN
#..............................................................................
data_catalog_pa <- data_catalog |>
  dplyr::mutate(
    denominazione_data_catalog_norm = normalize_text_match(denominazione_data_catalog),
    note_norm = normalize_text_match(note),
    tematica_norm = normalize_text_match(tematica),
    ambito_riferimento_norm = normalize_text_match(ambito_riferimento),
    contenuto_informativo_norm = normalize_text_match(contenuto_informativo)
  ) |>
  dplyr::filter(
    stringr::str_detect(tematica_norm, "pubblica|amministrazione|personale|bilancio|appalti|patrimonio|istituzioni|difesa|giustizia") |
      stringr::str_detect(ambito_riferimento_norm, "pubblica|amministrazione|ente|enti|istituzioni|comuni|province|regioni|camere di commercio") |
      stringr::str_detect(contenuto_informativo_norm, "personale|bilancio|spesa|amministratori|incarichi|procurement|ricerca|statistica") |
      stringr::str_detect(note_norm, "istat|camere di commercio|regioni|province autonome|istituzioni pubbliche|ricerca e sviluppo|sistan")
  )

#..............................................................................
# Mapping template: data catalog -> catalogo PSN
#..............................................................................
mapping_data_catalog_psn <- data_catalog_pa |>
  dplyr::select(
    fonte_ente_gestore,
    denominazione_data_catalog,
    tematica,
    periodicita,
    formato_dati,
    note,
    denominazione_data_catalog_norm
  ) |>
  dplyr::mutate(
    match_psn_codice = NA_character_,
    match_psn_denominazione = NA_character_,
    match_psn_tipo = NA_character_,         # Esatto / Plausibile / Da verificare / No match
    match_psn_note = NA_character_
  )

#..............................................................................
# Griglia di confronto: fonti non Istat PA vs rilevazioni Istat target
#..............................................................................
griglia_copertura_pa <- tidyr::crossing(
  fonti_non_istat_tbl |>
    dplyr::select(
      codice_lavoro,
      denominazione,
      ente_titolare,
      tipologia,
      obiettivo
    ),
  istat_target |>
    dplyr::select(
      codice_target,
      denominazione_target,
      periodicita,
      tipologia_unita_campione
    )
) |>
  dplyr::mutate(
    stesso_perimetro_istituzionale = NA_character_,
    stessa_unita_statistica = NA_character_,
    stesso_fenomeno = NA_character_,
    periodicita_compatibile = NA_character_,
    granularita_compatibile = NA_character_,
    copertura_valutazione = NA_character_,  # Copertura piena / parziale / complementare / non copre / non comparabile
    note_valutazione = NA_character_
  )

log_info("Data catalog importato: ", nrow(data_catalog))
log_info("Data catalog filtrato area PA/allargata: ", nrow(data_catalog_pa))
log_info("Fonti PA Istat selezionate nel catalogo: ", nrow(catalogo_pa_istat))
log_info("Fonti PA non Istat selezionate nel catalogo: ", nrow(catalogo_pa_non_istat))
log_info("Griglia confronto PA creata: ", nrow(griglia_copertura_pa))


#..............................................................................
# 15) EXPORT CSV --------------------------------------------------------------
#..............................................................................

# Per esportazione sicura verso Excel:
# - manteniamo solo le colonne utili
# - rimuoviamo gli a-capo interni dai campi testuali
flatten_for_export <- function(x) {
  if (!is.character(x)) return(x)
  x |>
    stringr::str_replace_all("[\r\n]+", " ") |>
    stringr::str_squish()
}

catalogo_export <- catalogo |>
  dplyr::select(
    pagina_pdf,
    sezione_pdf,
    settore,
    area_tematica,
    codice_lavoro,
    codice_lavoro_std,
    codice_lavoro_match_psn,
    denominazione,
    ente_titolare,
    tipologia,
    obiettivo,
    origine,
    nuovo_inserimento,
    presente_in_SEL_IND_ISTAT,
    presente_su_PSN_anche_in_SEL_IND_ISTAT,
    n_occorrenze_SEL_IND_ISTAT,
    n_codici_originali_SEL_IND_ISTAT,
    codici_originali_SEL_IND_ISTAT
  ) |>
  dplyr::mutate(
    dplyr::across(dplyr::everything(), flatten_for_export)
  )


# CSV "Excel-friendly" per ambiente italiano
readr::write_excel_csv2(catalogo_export, csv_output_path)
log_info("CSV creato: ", csv_output_path)

# Export ulteriori file di controllo
readr::write_excel_csv2(non_match_catalogo_SEL_IND_ISTAT, "catalogo_non_match_SEL_IND_ISTAT.csv")
readr::write_excel_csv2(solo_SEL_IND_ISTAT, "solo_SEL_IND_ISTAT.csv")
readr::write_excel_csv2(controlli_qualita, "controlli_qualita.csv")
readr::write_excel_csv2(check_merge, "check_merge.csv")
readr::write_excel_csv2(catalogo_pa_istat, "catalogo_pa_istat.csv")
readr::write_excel_csv2(catalogo_pa_non_istat, "catalogo_pa_non_istat.csv")
readr::write_excel_csv2(pivot_pa_fonti, "pivot_pa_fonti.csv")

#..............................................................................
# 16) EXPORT LOG --------------------------------------------------------------
#..............................................................................

writeLines(log_messages, con = log_output_path)
log_info("Log scritto: ", log_output_path)

#..............................................................................
# 17) CREAZIONE EXCEL ---------------------------------------------------------
#..............................................................................

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
    "sel_ind_istat",
    "check_merge",
    "non_match_sel",
    "solo_sel",
    "pivot_settore",
    "pivot_area",
    "pivot_ente",
    "pivot_sel_ind",
    "data_catalog",
    "data_catalog_pa",
    "fonti_non_istat_pa",
    "istat_target",
    "griglia_copertura_pa",
    "mapping_data_catalog_psn",
    "catalogo_pa_istat",
    "catalogo_pa_non_istat",
    "pivot_pa_fonti",
    "log"
  ),
  descrizione = c(
    "Indice del file e descrizione dei fogli",
    "Indicatori sintetici dell'estrazione",
    "Catalogo completo estratto dal PDF con dummy SEL_IND_ISTAT",
    "Record che richiedono verifica manuale",
    "Dataset importato da SEL_IND_ISTAT con chiave standardizzata",
    "Metriche di match tra catalogo PSN e SEL_IND_ISTAT",
    "Record del catalogo PSN non presenti in SEL_IND_ISTAT",
    "Codici presenti in SEL_IND_ISTAT ma non nel catalogo PSN",
    "Conteggio record per settore",
    "Conteggio record per settore e area tematica",
    "Conteggio record per ente titolare",
    "Conteggio record per presenza in SEL_IND_ISTAT",
    "Dataset importato dal data catalog",
    "Subset del data catalog filtrato per temi PA/allargati",
    "Fonti non Istat del catalogo PSN selezionate per l'analisi",
    "Rilevazioni Istat target per il confronto di copertura",
    "Griglia di confronto tra fonti non Istat PSN e rilevazioni Istat target",
    "Template di lavoro per riconduzione fonti del data catalog a voci del catalogo PSN",
    "Fonti PA Istat selezionate dal catalogo PSN",
    "Fonti PA non Istat selezionate dal catalogo PSN",
    "Conteggio sintetico fonti PA Istat / non Istat",
    "Messaggi di log dell'elaborazione"
  )
)

openxlsx::writeData(wb, "indice", "Indice del workbook", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "indice", indice_tbl, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "indice", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "indice", style_header, rows = 3, cols = 1:ncol(indice_tbl), gridExpand = TRUE)
openxlsx::addStyle(wb, "indice", style_wrap, rows = 4:(nrow(indice_tbl) + 3), cols = 1:ncol(indice_tbl), gridExpand = TRUE)
openxlsx::setColWidths(wb, "indice", cols = 1:2, widths = c(22, 75))
openxlsx::freezePane(wb, "indice", firstActiveRow = 4, firstActiveCol = 1)

#..............................................................................
# Foglio: riepilogo
#..............................................................................
openxlsx::addWorksheet(wb, "riepilogo", gridLines = TRUE)
openxlsx::writeData(wb, "riepilogo", "Riepilogo estrazione", startRow = 1, startCol = 1)
openxlsx::writeData(
  wb, "riepilogo",
  "Questo foglio contiene indicatori sintetici del catalogo estratto e del match con SEL_IND_ISTAT.",
  startRow = 2, startCol = 1
)
openxlsx::writeData(wb, "riepilogo", riepilogo, startRow = 4, withFilter = FALSE)
openxlsx::addStyle(wb, "riepilogo", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "riepilogo", style_note, rows = 2, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "riepilogo", style_header, rows = 4, cols = 1:ncol(riepilogo), gridExpand = TRUE)
openxlsx::setColWidths(wb, "riepilogo", cols = 1:2, widths = c(45, 18))

#..............................................................................
# Foglio: catalogo
#..............................................................................
openxlsx::addWorksheet(wb, "catalogo", gridLines = TRUE)
openxlsx::writeData(wb, "catalogo", "Catalogo estratto dal PDF", startRow = 1, startCol = 1)
openxlsx::writeData(
  wb, "catalogo",
  "Tabella principale da utilizzare per analisi, filtri e verifiche. Include dummy di presenza in SEL_IND_ISTAT.",
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
openxlsx::setColWidths(wb, "catalogo", cols = 6,  widths = 15)
openxlsx::setColWidths(wb, "catalogo", cols = 7,  widths = 50)
openxlsx::setColWidths(wb, "catalogo", cols = 8,  widths = 35)
openxlsx::setColWidths(wb, "catalogo", cols = 9,  widths = 18)
openxlsx::setColWidths(wb, "catalogo", cols = 10, widths = 90)
openxlsx::setColWidths(wb, "catalogo", cols = 11, widths = 60)
openxlsx::setColWidths(wb, "catalogo", cols = 12, widths = 16)
openxlsx::setColWidths(wb, "catalogo", cols = 13, widths = 16)
openxlsx::setColWidths(wb, "catalogo", cols = 14, widths = 20)

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
# Foglio: sel_ind_istat
#..............................................................................
openxlsx::addWorksheet(wb, "sel_ind_istat", gridLines = TRUE)

openxlsx::writeData(
  wb, "sel_ind_istat",
  "Dataset importato da SEL_IND_ISTAT",
  startRow = 1, startCol = 1
)

openxlsx::writeData(
  wb, "sel_ind_istat",
  "Il foglio contiene il file esterno importato, ripulito dalle righe vuote e con la chiave standardizzata codice_lavoro_std.",
  startRow = 2, startCol = 1
)

openxlsx::writeData(
  wb, "sel_ind_istat",
  SEL_IND_ISTAT,
  startRow = 4,
  withFilter = TRUE
)

openxlsx::addStyle(wb, "sel_ind_istat", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "sel_ind_istat", style_note, rows = 2, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "sel_ind_istat", style_header, rows = 4, cols = 1:ncol(SEL_IND_ISTAT), gridExpand = TRUE)

if (nrow(SEL_IND_ISTAT) > 0) {
  openxlsx::addStyle(
    wb, "sel_ind_istat", style_wrap,
    rows = 5:(nrow(SEL_IND_ISTAT) + 4),
    cols = 1:ncol(SEL_IND_ISTAT),
    gridExpand = TRUE
  )
}

openxlsx::setColWidths(wb, "sel_ind_istat", cols = 1:ncol(SEL_IND_ISTAT), widths = "auto")
openxlsx::freezePane(wb, "sel_ind_istat", firstActiveRow = 5, firstActiveCol = 1)


#..............................................................................
# Foglio: check_merge
#..............................................................................
openxlsx::addWorksheet(wb, "check_merge", gridLines = TRUE)
openxlsx::writeData(wb, "check_merge", "Controlli del match con SEL_IND_ISTAT", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "check_merge", check_merge, startRow = 3, withFilter = FALSE)
openxlsx::addStyle(wb, "check_merge", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "check_merge", style_header, rows = 3, cols = 1:ncol(check_merge), gridExpand = TRUE)
openxlsx::setColWidths(wb, "check_merge", cols = 1:2, widths = c(45, 18))

#..............................................................................
# Foglio: non_match_sel
#..............................................................................
openxlsx::addWorksheet(wb, "non_match_sel", gridLines = TRUE)
openxlsx::writeData(wb, "non_match_sel", "Record del catalogo non presenti in SEL_IND_ISTAT", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "non_match_sel", non_match_catalogo_SEL_IND_ISTAT, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "non_match_sel", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "non_match_sel", style_header, rows = 3, cols = 1:ncol(non_match_catalogo_SEL_IND_ISTAT), gridExpand = TRUE)
openxlsx::setColWidths(wb, "non_match_sel", cols = 1:ncol(non_match_catalogo_SEL_IND_ISTAT), widths = "auto")

#..............................................................................
# Foglio: solo_sel
#..............................................................................
openxlsx::addWorksheet(wb, "solo_sel", gridLines = TRUE)
openxlsx::writeData(wb, "solo_sel", "Codici presenti in SEL_IND_ISTAT ma non nel catalogo PSN", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "solo_sel", solo_SEL_IND_ISTAT, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "solo_sel", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "solo_sel", style_header, rows = 3, cols = 1:ncol(solo_SEL_IND_ISTAT), gridExpand = TRUE)
openxlsx::setColWidths(wb, "solo_sel", cols = 1:ncol(solo_SEL_IND_ISTAT), widths = "auto")

#..............................................................................
# Fogli pivot
#..............................................................................
openxlsx::addWorksheet(wb, "pivot_settore", gridLines = TRUE)
openxlsx::writeData(wb, "pivot_settore", "Conteggio record per settore", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "pivot_settore", pivot_settore, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "pivot_settore", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "pivot_settore", style_header, rows = 3, cols = 1:ncol(pivot_settore), gridExpand = TRUE)
openxlsx::setColWidths(wb, "pivot_settore", cols = 1:2, widths = c(45, 12))

openxlsx::addWorksheet(wb, "pivot_area", gridLines = TRUE)
openxlsx::writeData(wb, "pivot_area", "Conteggio record per settore e area tematica", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "pivot_area", pivot_area, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "pivot_area", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "pivot_area", style_header, rows = 3, cols = 1:ncol(pivot_area), gridExpand = TRUE)
openxlsx::setColWidths(wb, "pivot_area", cols = 1:3, widths = c(35, 45, 12))

openxlsx::addWorksheet(wb, "pivot_ente", gridLines = TRUE)
openxlsx::writeData(wb, "pivot_ente", "Conteggio record per ente titolare", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "pivot_ente", pivot_ente, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "pivot_ente", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "pivot_ente", style_header, rows = 3, cols = 1:ncol(pivot_ente), gridExpand = TRUE)
openxlsx::setColWidths(wb, "pivot_ente", cols = 1:2, widths = c(45, 12))

openxlsx::addWorksheet(wb, "pivot_sel_ind", gridLines = TRUE)
openxlsx::writeData(wb, "pivot_sel_ind", "Conteggio record per presenza in SEL_IND_ISTAT", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "pivot_sel_ind", pivot_sel_ind, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "pivot_sel_ind", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "pivot_sel_ind", style_header, rows = 3, cols = 1:ncol(pivot_sel_ind), gridExpand = TRUE)
openxlsx::setColWidths(wb, "pivot_sel_ind", cols = 1:2, widths = c(25, 12))

#..............................................................................
# Foglio: data_catalog
#..............................................................................
openxlsx::addWorksheet(wb, "data_catalog", gridLines = TRUE)
openxlsx::writeData(wb, "data_catalog", "Data catalog importato", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "data_catalog", data_catalog, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "data_catalog", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "data_catalog", style_header, rows = 3, cols = 1:ncol(data_catalog), gridExpand = TRUE)
openxlsx::setColWidths(wb, "data_catalog", cols = 1:ncol(data_catalog), widths = "auto")

#..............................................................................
# Foglio: data_catalog_pa
#..............................................................................
openxlsx::addWorksheet(wb, "data_catalog_pa", gridLines = TRUE)
openxlsx::writeData(wb, "data_catalog_pa", "Data catalog - subset PA/allargato", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "data_catalog_pa", data_catalog_pa, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "data_catalog_pa", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "data_catalog_pa", style_header, rows = 3, cols = 1:ncol(data_catalog_pa), gridExpand = TRUE)
openxlsx::setColWidths(wb, "data_catalog_pa", cols = 1:ncol(data_catalog_pa), widths = "auto")

#..............................................................................
# Foglio: fonti_non_istat_pa
#..............................................................................
openxlsx::addWorksheet(wb, "fonti_non_istat_pa", gridLines = TRUE)
openxlsx::writeData(wb, "fonti_non_istat_pa", "Fonti non Istat selezionate nel catalogo PSN", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "fonti_non_istat_pa", fonti_non_istat_tbl, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "fonti_non_istat_pa", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "fonti_non_istat_pa", style_header, rows = 3, cols = 1:ncol(fonti_non_istat_tbl), gridExpand = TRUE)
openxlsx::setColWidths(wb, "fonti_non_istat_pa", cols = 1:ncol(fonti_non_istat_tbl), widths = "auto")

#..............................................................................
# Foglio: istat_target
#..............................................................................
openxlsx::addWorksheet(wb, "istat_target", gridLines = TRUE)
openxlsx::writeData(wb, "istat_target", "Rilevazioni Istat target", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "istat_target", istat_target, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "istat_target", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "istat_target", style_header, rows = 3, cols = 1:ncol(istat_target), gridExpand = TRUE)
openxlsx::setColWidths(wb, "istat_target", cols = 1:ncol(istat_target), widths = "auto")

#..............................................................................
# Foglio: griglia_copertura_pa
#..............................................................................
openxlsx::addWorksheet(wb, "griglia_copertura_pa", gridLines = TRUE)
openxlsx::writeData(wb, "griglia_copertura_pa", "Griglia confronto fonti non Istat vs rilevazioni Istat", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "griglia_copertura_pa", griglia_copertura_pa, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "griglia_copertura_pa", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "griglia_copertura_pa", style_header, rows = 3, cols = 1:ncol(griglia_copertura_pa), gridExpand = TRUE)
openxlsx::setColWidths(wb, "griglia_copertura_pa", cols = 1:ncol(griglia_copertura_pa), widths = "auto")

#..............................................................................
# Foglio: mapping_data_catalog_psn
#..............................................................................
openxlsx::addWorksheet(wb, "mapping_data_catalog_psn", gridLines = TRUE)
openxlsx::writeData(wb, "mapping_data_catalog_psn", "Template mapping data catalog -> catalogo PSN", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "mapping_data_catalog_psn", mapping_data_catalog_psn, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "mapping_data_catalog_psn", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "mapping_data_catalog_psn", style_header, rows = 3, cols = 1:ncol(mapping_data_catalog_psn), gridExpand = TRUE)
openxlsx::setColWidths(wb, "mapping_data_catalog_psn", cols = 1:ncol(mapping_data_catalog_psn), widths = "auto")

#..............................................................................
# Foglio: catalogo_pa_istat
#..............................................................................
openxlsx::addWorksheet(wb, "catalogo_pa_istat", gridLines = TRUE)
openxlsx::writeData(wb, "catalogo_pa_istat", "Fonti PA Istat selezionate nel catalogo PSN", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "catalogo_pa_istat", catalogo_pa_istat, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "catalogo_pa_istat", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "catalogo_pa_istat", style_header, rows = 3, cols = 1:ncol(catalogo_pa_istat), gridExpand = TRUE)
openxlsx::setColWidths(wb, "catalogo_pa_istat", cols = 1:ncol(catalogo_pa_istat), widths = "auto")

#..............................................................................
# Foglio: catalogo_pa_non_istat
#..............................................................................
openxlsx::addWorksheet(wb, "catalogo_pa_non_istat", gridLines = TRUE)
openxlsx::writeData(wb, "catalogo_pa_non_istat", "Fonti PA non Istat selezionate nel catalogo PSN", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "catalogo_pa_non_istat", catalogo_pa_non_istat, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "catalogo_pa_non_istat", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "catalogo_pa_non_istat", style_header, rows = 3, cols = 1:ncol(catalogo_pa_non_istat), gridExpand = TRUE)
openxlsx::setColWidths(wb, "catalogo_pa_non_istat", cols = 1:ncol(catalogo_pa_non_istat), widths = "auto")

#..............................................................................
# Foglio: pivot_pa_fonti
#..............................................................................
openxlsx::addWorksheet(wb, "pivot_pa_fonti", gridLines = TRUE)
openxlsx::writeData(wb, "pivot_pa_fonti", "Conteggio fonti PA Istat / non Istat", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "pivot_pa_fonti", pivot_pa_fonti, startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, "pivot_pa_fonti", style_title, rows = 1, cols = 1, gridExpand = FALSE)
openxlsx::addStyle(wb, "pivot_pa_fonti", style_header, rows = 3, cols = 1:ncol(pivot_pa_fonti), gridExpand = TRUE)
openxlsx::setColWidths(wb, "pivot_pa_fonti", cols = 1:ncol(pivot_pa_fonti), widths = "auto")

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

#..............................................................................
# 18) ORDINE FOGLI E SALVATAGGIO ----------------------------------------------
#..............................................................................

desired_order <- c(
  "indice",
  "riepilogo",
  "catalogo",
  "controlli_qualita",
  "sel_ind_istat",
  "check_merge",
  "non_match_sel",
  "solo_sel",
  "pivot_settore",
  "pivot_area",
  "pivot_ente",
  "pivot_sel_ind",
  "data_catalog",
  "data_catalog_pa",
  "fonti_non_istat_pa",
  "istat_target",
  "griglia_copertura_pa",
  "mapping_data_catalog_psn",
  "catalogo_pa_istat",
  "catalogo_pa_non_istat",
  "pivot_pa_fonti",
  "log"
)


current_sheets <- names(wb)
sheet_indices <- match(desired_order, current_sheets)
wb$sheetOrder <- sheet_indices

openxlsx::saveWorkbook(wb, excel_output_path, overwrite = TRUE)
log_info("Excel creato: ", excel_output_path)

#..............................................................................
# 19) REPORT FINALE -----------------------------------------------------------
#..............................................................................

log_info("Elaborazione completata con successo.")
log_info("Record finali: ", nrow(catalogo))
log_info("Settori distinti: ", dplyr::n_distinct(catalogo$settore, na.rm = TRUE))
log_info("Aree tematiche distinte: ", dplyr::n_distinct(catalogo$area_tematica, na.rm = TRUE))
log_info("Enti distinti: ", dplyr::n_distinct(catalogo$ente_titolare, na.rm = TRUE))
log_info("Record presenti in SEL_IND_ISTAT: ", sum(catalogo$presente_in_SEL_IND_ISTAT, na.rm = TRUE))

cat("\n============================================================\n")
cat("ESTRAZIONE COMPLETATA\n")
cat("Excel:", excel_output_path, "\n")
cat("CSV:  ", csv_output_path, "\n")
cat("Log:  ", log_output_path, "\n")
cat("============================================================\n")