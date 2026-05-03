# ============================================================
# Scraping OpenBDAP - Bilanci degli Enti della Pubblica Amministrazione
# URL di partenza:
# https://bdap-opendata.rgs.mef.gov.it/tema/bilanci-degli-enti-della-pubblica-amministrazione/?h=search0
#
# Obiettivo:
# 1. Scorrere tutte le pagine del tema "Bilanci degli Enti della PA".
# 2. Estrarre metadati dei dataset pubblicati nel tema.
# 3. Recuperare i link di dettaglio, visualizzazione e download.
# 4. Scaricare, quando possibile, i file CSV.
# 5. Leggere i nomi delle variabili di ciascun dataset.
# 6. Esportare un file Excel di sintesi.
#
# Nota metodologica:
# Questo script serve a costruire un inventario dei dataset OpenBDAP
# potenzialmente alternativi/complementari alle fonti PSN, non a stabilire
# automaticamente l'equivalenza sostanziale tra fonti.
# ============================================================

# -----------------------------
# 0. Pacchetti
# -----------------------------
required_packages <- c(
  "rvest", "xml2", "httr2", "dplyr", "purrr", "stringr", "tibble",
  "readr", "janitor", "openxlsx", "glue", "urltools"
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) {
    install.packages(missing, dependencies = TRUE)
  }
}

install_if_missing(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# -----------------------------
# 1. Parametri utente
# -----------------------------
start_url <- "https://bdap-opendata.rgs.mef.gov.it/tema/bilanci-degli-enti-della-pubblica-amministrazione/?h=search0"
base_domain <- "https://bdap-opendata.rgs.mef.gov.it"

out_dir <- "output_openbdap_bilanci"
csv_dir <- file.path(out_dir, "csv")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)

# Per non sovraccaricare il sito: pausa tra chiamate.
sleep_seconds <- 0.6

# Numero massimo di pagine da provare.
# La pagina mostrava 157 risultati; con 10 risultati per pagina bastano circa 16 pagine.
# Tengo un margine alto per robustezza.
max_pages <- 40

# Se TRUE scarica i CSV; se FALSE crea solo il catalogo.
download_csv <- TRUE

# -----------------------------
# 2. Funzioni di supporto
# -----------------------------

safe_read_html <- function(url, sleep = sleep_seconds) {
  Sys.sleep(sleep)
  tryCatch({
    request(url) |>
      req_user_agent("R scraping script for research - OpenBDAP metadata extraction") |>
      req_timeout(60) |>
      req_perform() |>
      resp_body_html()
  }, error = function(e) {
    message("Errore lettura pagina: ", url, " | ", conditionMessage(e))
    NULL
  })
}

absolute_url <- function(x, base = base_domain) {
  if (is.na(x) || length(x) == 0 || x == "") return(NA_character_)
  xml2::url_absolute(x, base)
}

clean_text <- function(x) {
  x |>
    stringr::str_replace_all("\\u00a0", " ") |>
    stringr::str_squish()
}

# Prova a costruire l'URL della pagina n-esima.
# Molti cataloghi CKAN/DKAN usano il parametro page=N.
make_page_url <- function(page_number) {
  if (page_number <= 1) return(start_url)
  sep <- ifelse(stringr::str_detect(start_url, "\\?"), "&", "?")
  paste0(start_url, sep, "page=", page_number)
}

# Estrae card dataset da una pagina del tema.
# La funzione è volutamente difensiva: usa più selettori possibili perché
# il markup del portale può cambiare nel tempo.
extract_dataset_cards <- function(html_doc, page_url) {
  if (is.null(html_doc)) return(tibble())

  # Strategia 1: link dei titoli dataset dentro la lista risultati.
  # Nota: NON fare pipe direttamente dentro tibble(), altrimenti il nodeset
  # viene passato come prima colonna e tibble va in errore.
  page_links <- html_doc |> html_elements("a")

  all_links <- tibble::tibble(
      text = html_text2(page_links),
      href = html_attr(page_links, "href")
    ) |>
    mutate(
      text = clean_text(text),
      href = purrr::map_chr(href, absolute_url),
      page_url = page_url
    ) |>
    filter(!is.na(href), text != "")

  # Link candidati: pagine dataset OpenBDAP.
  dataset_links <- all_links |>
    filter(
      stringr::str_detect(href, "/dataset/|/catalog/|/tema/") == TRUE,
      !stringr::str_detect(text, regex("visualizza|scarica|tema|banca dati|licenza|supporto|api|cosa sono|dataset", ignore_case = TRUE))
    ) |>
    filter(stringr::str_detect(text, regex("consuntivo|preventivo|bilancio|conto|patrimonio|situazione|rendiconto|enti", ignore_case = TRUE))) |>
    distinct(text, href, .keep_all = TRUE)

  # Strategia 2: se i titoli sono h2/h3 con link, privilegia quelli.
  title_links <- html_doc |>
    html_elements("h2 a, h3 a, .views-row h2 a, .views-row h3 a, .search-result a")

  if (length(title_links) > 0) {
    title_tbl <- tibble(
      dataset_name = clean_text(html_text2(title_links)),
      detail_url = purrr::map_chr(html_attr(title_links, "href"), absolute_url),
      page_url = page_url
    ) |>
      filter(dataset_name != "", !is.na(detail_url)) |>
      filter(stringr::str_detect(dataset_name, regex("consuntivo|preventivo|bilancio|conto|patrimonio|situazione|rendiconto|enti", ignore_case = TRUE))) |>
      distinct(dataset_name, detail_url, .keep_all = TRUE)
  } else {
    title_tbl <- tibble()
  }

  out <- bind_rows(
    dataset_links |> transmute(dataset_name = text, detail_url = href, page_url = page_url),
    title_tbl
  ) |>
    distinct(dataset_name, detail_url, .keep_all = TRUE)

  out
}

# Estrae descrizione e metadati testuali dalla pagina di dettaglio.
extract_detail_metadata <- function(detail_url) {
  html_doc <- safe_read_html(detail_url)
  if (is.null(html_doc)) {
    return(tibble(
      detail_url = detail_url,
      description = NA_character_,
      fonte = NA_character_,
      data_creazione = NA_character_,
      tema = NA_character_,
      licenza = NA_character_,
      all_download_links = NA_character_,
      csv_url = NA_character_,
      error_detail = "pagina dettaglio non letta"
    ))
  }

  page_text <- html_doc |> html_text2() |> clean_text()

  # Descrizione: spesso è il primo paragrafo informativo.
  description <- html_doc |>
    html_elements("p") |>
    html_text2() |>
    clean_text()
  description <- description[description != ""]
  description <- ifelse(length(description) > 0, description[1], NA_character_)

  links <- html_doc |>
    html_elements("a")

  links_tbl <- tibble(
    link_text = clean_text(html_text2(links)),
    href = purrr::map_chr(html_attr(links, "href"), absolute_url)
  ) |>
    filter(!is.na(href), href != "") |>
    distinct()

  download_links <- links_tbl |>
    filter(
      stringr::str_detect(link_text, regex("scarica|download|csv", ignore_case = TRUE)) |
        stringr::str_detect(href, regex("/export/|\\.csv($|\\?)|download", ignore_case = TRUE))
    )

  csv_url <- download_links |>
    filter(stringr::str_detect(href, regex("\\.csv($|\\?)|/export/csv/", ignore_case = TRUE))) |>
    pull(href) |>
    unique()
  csv_url <- ifelse(length(csv_url) > 0, csv_url[1], NA_character_)

  # Se non trova un CSV diretto, prova a convertire link di download noti.
  if (is.na(csv_url) && nrow(download_links) > 0) {
    csv_url <- download_links$href[1]
  }

  # Estrazione metadati semplici da testo pagina.
  extract_after_label <- function(label) {
    pattern <- paste0(label, "[:\\s]+([^\\n\\r]+)")
    m <- stringr::str_match(page_text, regex(pattern, ignore_case = TRUE))
    ifelse(!is.na(m[, 2]), clean_text(m[, 2]), NA_character_)
  }

  tibble(
    detail_url = detail_url,
    description = description,
    fonte = extract_after_label("Fonte"),
    data_creazione = extract_after_label("Data creazione"),
    tema = extract_after_label("Tema"),
    licenza = extract_after_label("Licenze?"),
    all_download_links = paste(download_links$href, collapse = " | "),
    csv_url = csv_url,
    error_detail = NA_character_
  )
}

# Classificazione grezza del tipo di dataset dal nome.
classify_dataset <- function(dataset_name) {
  dplyr::case_when(
    str_detect(dataset_name, regex("patrimonio attivo|attivo", ignore_case = TRUE)) ~ "Patrimonio attivo",
    str_detect(dataset_name, regex("patrimonio passivo|passivo", ignore_case = TRUE)) ~ "Patrimonio passivo",
    str_detect(dataset_name, regex("situazione amministrativa", ignore_case = TRUE)) ~ "Situazione amministrativa",
    str_detect(dataset_name, regex("conto economico", ignore_case = TRUE)) ~ "Conto economico",
    str_detect(dataset_name, regex("bilancio finanziario", ignore_case = TRUE)) ~ "Bilancio finanziario",
    str_detect(dataset_name, regex("rendiconto", ignore_case = TRUE)) ~ "Rendiconto",
    TRUE ~ "Altro / da classificare"
  )
}

classify_phase <- function(dataset_name) {
  dplyr::case_when(
    str_detect(dataset_name, regex("consuntivo", ignore_case = TRUE)) ~ "Consuntivo",
    str_detect(dataset_name, regex("previsione|preventivo", ignore_case = TRUE)) ~ "Previsione",
    TRUE ~ "Non specificato"
  )
}

# Download CSV e lettura nomi variabili.
safe_download_and_read_vars <- function(csv_url, dataset_name) {
  if (is.na(csv_url) || csv_url == "") {
    return(tibble(
      dataset_name = dataset_name,
      csv_url = csv_url,
      local_file = NA_character_,
      n_columns = NA_integer_,
      variable_names = NA_character_,
      download_status = "nessun csv_url",
      read_error = NA_character_
    ))
  }

  safe_name <- dataset_name |>
    janitor::make_clean_names() |>
    stringr::str_sub(1, 120)

  local_file <- file.path(csv_dir, paste0(safe_name, ".csv"))

  download_ok <- tryCatch({
    request(csv_url) |>
      req_user_agent("R scraping script for research - OpenBDAP CSV download") |>
      req_timeout(180) |>
      req_perform(path = local_file)
    TRUE
  }, error = function(e) {
    message("Errore download CSV: ", dataset_name, " | ", conditionMessage(e))
    FALSE
  })

  if (!download_ok || !file.exists(local_file)) {
    return(tibble(
      dataset_name = dataset_name,
      csv_url = csv_url,
      local_file = local_file,
      n_columns = NA_integer_,
      variable_names = NA_character_,
      download_status = "download fallito",
      read_error = NA_character_
    ))
  }

  # Legge solo l'header e poche righe per ricavare le variabili.
  read_attempt <- tryCatch({
    df <- readr::read_delim(
      local_file,
      delim = ";",
      n_max = 5,
      show_col_types = FALSE,
      locale = readr::locale(encoding = "UTF-8")
    )
    if (ncol(df) == 1) {
      df <- readr::read_csv(
        local_file,
        n_max = 5,
        show_col_types = FALSE,
        locale = readr::locale(encoding = "UTF-8")
      )
    }
    tibble(
      dataset_name = dataset_name,
      csv_url = csv_url,
      local_file = local_file,
      n_columns = ncol(df),
      variable_names = paste(names(df), collapse = " | "),
      download_status = "ok",
      read_error = NA_character_
    )
  }, error = function(e) {
    tibble(
      dataset_name = dataset_name,
      csv_url = csv_url,
      local_file = local_file,
      n_columns = NA_integer_,
      variable_names = NA_character_,
      download_status = "scaricato ma non letto",
      read_error = conditionMessage(e)
    )
  })

  read_attempt
}

# -----------------------------
# 3. Scraping catalogo tema
# -----------------------------
message("Avvio scraping catalogo OpenBDAP - Bilanci Enti PA")

catalog_pages <- purrr::map_dfr(seq_len(max_pages), function(i) {
  page_url <- make_page_url(i)
  message("Pagina ", i, ": ", page_url)
  html_doc <- safe_read_html(page_url)
  cards <- extract_dataset_cards(html_doc, page_url)

  # Se una pagina dopo la prima non restituisce risultati, continuiamo ma la tabella sarà vuota.
  cards |> mutate(page_number = i)
})

catalog_raw <- catalog_pages |>
  distinct(dataset_name, detail_url, .keep_all = TRUE) |>
  filter(!is.na(dataset_name), dataset_name != "") |>
  arrange(dataset_name)

message("Dataset candidati trovati: ", nrow(catalog_raw))

# -----------------------------
# 4. Metadati pagine dettaglio
# -----------------------------
message("Estrazione metadati dalle pagine dettaglio...")

# Nota tecnica: non uso split(.$row_id) con la pipe nativa `|>`.
# Con `|>` il placeholder `.` non e' disponibile come in magrittr `%>%`;
# in alcune versioni di R questo produce: Error: object '.' not found.
catalog_indexed <- catalog_raw |>
  mutate(row_id = row_number())

catalog_rows <- split(catalog_indexed, catalog_indexed$row_id)

detail_meta <- purrr::map_dfr(catalog_rows, function(row) {
    message("Dettaglio: ", row$dataset_name)
    meta <- extract_detail_metadata(row$detail_url)
    bind_cols(row |> select(-row_id), meta |> select(-detail_url))
  }) |>
  mutate(
    tipo_dataset = classify_dataset(dataset_name),
    fase_bilancio = classify_phase(dataset_name),
    candidato_ist_00229 = case_when(
      fase_bilancio == "Consuntivo" & str_detect(tipo_dataset, regex("Bilancio finanziario|Situazione amministrativa|Rendiconto", ignore_case = TRUE)) ~ "Alto",
      fase_bilancio == "Consuntivo" ~ "Medio",
      TRUE ~ "Basso / da verificare"
    )
  )

# -----------------------------
# 5. Download CSV e variabili
# -----------------------------
if (download_csv) {
  message("Download CSV e lettura variabili...")

  vars_indexed <- detail_meta |>
    select(dataset_name, csv_url) |>
    distinct() |>
    mutate(row_id = row_number())

  vars_rows <- split(vars_indexed, vars_indexed$row_id)

  vars_tbl <- purrr::map_dfr(vars_rows, function(row) {
      message("CSV: ", row$dataset_name)
      safe_download_and_read_vars(row$csv_url, row$dataset_name)
    })

} else {
  vars_tbl <- tibble(
    dataset_name = character(),
    csv_url = character(),
    local_file = character(),
    n_columns = integer(),
    variable_names = character(),
    download_status = character(),
    read_error = character()
  )
}

# -----------------------------
# 6. Output
# -----------------------------
final_catalog <- detail_meta |>
  left_join(vars_tbl, by = c("dataset_name", "csv_url")) |>
  relocate(dataset_name, tipo_dataset, fase_bilancio, candidato_ist_00229)

# Salva CSV intermedi.
readr::write_csv(catalog_raw, file.path(out_dir, "catalogo_bilanci_openbdap_raw.csv"))
readr::write_csv(detail_meta, file.path(out_dir, "catalogo_bilanci_openbdap_metadata.csv"))
readr::write_csv(vars_tbl, file.path(out_dir, "catalogo_bilanci_openbdap_variabili.csv"))
readr::write_csv(final_catalog, file.path(out_dir, "catalogo_bilanci_openbdap_finale.csv"))

# Salva Excel.
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Catalogo_finale")
openxlsx::writeData(wb, "Catalogo_finale", final_catalog)
openxlsx::setColWidths(wb, "Catalogo_finale", cols = 1:ncol(final_catalog), widths = "auto")
openxlsx::freezePane(wb, "Catalogo_finale", firstRow = TRUE)

openxlsx::addWorksheet(wb, "Metadati")
openxlsx::writeData(wb, "Metadati", detail_meta)
openxlsx::setColWidths(wb, "Metadati", cols = 1:ncol(detail_meta), widths = "auto")
openxlsx::freezePane(wb, "Metadati", firstRow = TRUE)

openxlsx::addWorksheet(wb, "Variabili")
openxlsx::writeData(wb, "Variabili", vars_tbl)
openxlsx::setColWidths(wb, "Variabili", cols = 1:ncol(vars_tbl), widths = "auto")
openxlsx::freezePane(wb, "Variabili", firstRow = TRUE)

openxlsx::addWorksheet(wb, "Raw_catalogo")
openxlsx::writeData(wb, "Raw_catalogo", catalog_raw)
openxlsx::setColWidths(wb, "Raw_catalogo", cols = 1:ncol(catalog_raw), widths = "auto")
openxlsx::freezePane(wb, "Raw_catalogo", firstRow = TRUE)

xlsx_path <- file.path(out_dir, "catalogo_bilanci_openbdap.xlsx")
openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

message("\nCompletato.")
message("Dataset candidati nel catalogo: ", nrow(catalog_raw))
message("Output Excel: ", xlsx_path)
message("Cartella CSV scaricati: ", csv_dir)

# -----------------------------
# 7. Controlli consigliati dopo l'esecuzione
# -----------------------------
# 1. Verifica che nrow(final_catalog) sia vicino a 157.
#    Se è molto più basso, il parametro page= potrebbe non essere quello usato dal sito.
# 2. Controlla la colonna csv_url: se è spesso vuota, la pagina dettaglio usa un link
#    generato via JavaScript o un endpoint diverso.
# 3. Usa candidato_ist_00229 solo come primo filtro euristico, non come giudizio finale.

# Quante righe grezze prima del distinct?
nrow(catalog_pages)

# Quanti duplicati hai eliminato?
catalog_pages |>
  dplyr::count(dataset_name, detail_url, sort = TRUE) |>
  dplyr::filter(n > 1)

# Numero di dataset unici per nome
catalog_pages |>
  dplyr::summarise(
    righe_grezze = dplyr::n(),
    nomi_unici = dplyr::n_distinct(dataset_name),
    url_unici = dplyr::n_distinct(detail_url),
    coppie_nome_url_uniche = dplyr::n_distinct(paste(dataset_name, detail_url))
  )

final_catalog |>
  dplyr::summarise(
    n_dataset = dplyr::n(),
    csv_vuoti = sum(is.na(csv_url) | csv_url == ""),
    csv_unici = dplyr::n_distinct(csv_url),
    csv_duplicati = dplyr::n() - dplyr::n_distinct(csv_url)
  )

final_catalog |>
  dplyr::count(csv_url, sort = TRUE) |>
  dplyr::filter(n > 1)

final_catalog |>
  dplyr::count(tema, sort = TRUE)

final_catalog |>
  dplyr::count(tipo_dataset, fase_bilancio, sort = TRUE)

catalog_pages |>
  dplyr::count(page_number) |>
  print(n = Inf)
