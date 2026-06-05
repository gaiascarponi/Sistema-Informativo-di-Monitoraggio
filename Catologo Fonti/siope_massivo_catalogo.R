# ............................................................
# SIOPE - Catalogo dataset da download massivo            ####
# ............................................................
# Obiettivo:
# 1. individuare le annualità disponibili per SIOPE USCITE e SIOPE ENTRATE
# 2. scaricare:
#    - SIOPE_USCITE.<anno_target>.zip
#    - SIOPE_ENTRATE.<anno_target>.zip
#    - SIOPE_ANAGRAFICHE.zip
# 3. estrarre dai file scaricati:
#    - n_osservazioni
#    - n_variabili
#    - variabili di interesse
# 4. opzionalmente leggere da una pagina HTML locale del portale SIOPE:
#    - data di creazione / ultimo aggiornamento disponibile
# 5. esportare un file Excel finale nel formato richiesto
#
# NOTE:
# - il campo "ultimo aggiornamento disponibile" viene valorizzato solo se
#   è disponibile un file HTML locale salvato dalla pagina "Download massivo"
# - il flusso NON dipende dal click del frontend del sito
# ............................................................

rm(list=ls())

# ............................................................
# 0. Pacchetti                                            ####
# ............................................................
required_pkgs <- c(
  "httr2",
  "dplyr",
  "purrr",
  "stringr",
  "tibble",
  "readr",
  "xml2",
  "rvest",
  "data.table",
  "openxlsx",
  "fs"
)

missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

library(httr2)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(xml2)
library(rvest)
library(data.table)
library(openxlsx)
library(fs)


# ............................................................
# 1. Parametri e percorsii                                ####
# ............................................................
anno_target <- 2026

base_url <- "https://www.siope.it/Siope/documenti/siope2/open/last"

dir_input  <- "data/SIOPE/input_siope"
dir_raw    <- "data/SIOPE/raw_siope"
dir_temp   <- "data/SIOPE/temp_siope"
dir_output <- "data/SIOPE/output_siope"

dir_create(c(dir_input, dir_raw, dir_temp, dir_output), recurse = TRUE)

# File HTML opzionale:
# salva manualmente dal browser la pagina "Download massivo" come HTML
# e mettila qui, se vuoi valorizzare la data di creazione/aggiornamento
download_massivo_html_file <- file.path(dir_input, "download_massivo.html")

# intervallo di ricerca delle annualità
# puoi ampliarlo se vuoi essere più conservativa
anni_candidati <- 2000:(anno_target + 1)


# ............................................................
# 2. Funzioni helper - rete e URLi                        ####
# ............................................................
safe_head_status <- function(url) {
  out <- tryCatch(
    {
      resp <- request(url) |>
        req_method("HEAD") |>
        req_options(timeout = 30L, followlocation = TRUE) |>
        req_perform()
      
      resp_status(resp)
    },
    error = function(e) NA_integer_
  )
  out
}

safe_get_status <- function(url) {
  out <- tryCatch(
    {
      resp <- request(url) |>
        req_method("GET") |>
        req_options(timeout = 60L, followlocation = TRUE) |>
        req_perform()
      
      resp_status(resp)
    },
    error = function(e) NA_integer_
  )
  out
}

url_exists <- function(url) {
  status_head <- safe_head_status(url)
  
  if (!is.na(status_head) && status_head == 200) {
    return(TRUE)
  }
  
  status_get <- safe_get_status(url)
  isTRUE(!is.na(status_get) && status_get == 200)
}

build_file_url <- function(tipo = c("uscite", "entrate", "anagrafiche"), anno = NULL) {
  tipo <- match.arg(tipo)
  
  if (tipo == "anagrafiche") {
    return(paste0(base_url, "/SIOPE_ANAGRAFICHE.zip"))
  }
  
  if (is.null(anno)) {
    stop("Per 'uscite' ed 'entrate' bisogna specificare l'anno.")
  }
  
  file_name <- switch(
    tipo,
    "uscite"  = paste0("SIOPE_USCITE.", anno, ".zip"),
    "entrate" = paste0("SIOPE_ENTRATE.", anno, ".zip")
  )
  
  paste0(base_url, "/", file_name)
}

discover_available_years <- function(tipo = c("uscite", "entrate"), anni = anni_candidati) {
  tipo <- match.arg(tipo)
  
  tibble(
    anno = anni,
    url = map_chr(anni, ~ build_file_url(tipo = tipo, anno = .x))
  ) |>
    mutate(disponibile = map_lgl(url, url_exists)) |>
    filter(disponibile) |>
    arrange(anno)
}

download_if_needed <- function(url, destfile, overwrite = FALSE) {
  if (file_exists(destfile) && !overwrite) {
    message("File già presente: ", destfile)
    return(destfile)
  }
  
  message("Download: ", url)
  
  download.file(
    url      = url,
    destfile = destfile,
    mode     = "wb",
    quiet    = FALSE
  )
  
  if (!file_exists(destfile)) {
    stop("Download fallito: file non trovato dopo il download.")
  }
  
  destfile
}


# ............................................................
# 3. Funzioni helper - zip e file tabellarii              ####
# ............................................................
list_zip_files <- function(zip_path) {
  unzip(zip_path, list = TRUE)
}

pick_first_tabular_file <- function(zip_listing) {
  # priorità ai formati tabellari più comuni
  pattern <- "\\.(csv|txt|tsv|dat)$"
  
  candidates <- zip_listing |>
    mutate(Name = as.character(Name)) |>
    filter(str_detect(tolower(Name), pattern))
  
  if (nrow(candidates) == 0) {
    return(NA_character_)
  }
  
  candidates$Name[[1]]
}

extract_one_file_from_zip <- function(zip_path, internal_file, exdir = dir_temp) {
  out <- unzip(
    zipfile = zip_path,
    files   = internal_file,
    exdir   = exdir,
    overwrite = TRUE
  )
  
  out[[1]]
}

detect_delimiter <- function(file_path, n = 5) {
  lines <- readLines(file_path, n = n, warn = FALSE, encoding = "UTF-8")
  
  if (length(lines) == 0) {
    return(",")
  }
  
  first_nonempty <- lines[str_trim(lines) != ""][1]
  
  if (is.na(first_nonempty) || length(first_nonempty) == 0) {
    return(",")
  }
  
  delimiters <- c(";" = ";", "," = ",", "\t" = "\t", "|" = "|")
  
  counts <- vapply(delimiters, function(d) str_count(first_nonempty, fixed(d)), numeric(1))
  best <- names(which.max(counts))
  
  if (length(best) == 0 || is.na(best) || counts[[best]] == 0) {
    return("auto")
  }
  
  best
}

read_tabular_file <- function(file_path) {
  delim <- detect_delimiter(file_path)
  
  dt <- tryCatch(
    {
      if (identical(delim, "auto")) {
        data.table::fread(file_path, encoding = "UTF-8")
      } else {
        data.table::fread(file_path, sep = delim, encoding = "UTF-8")
      }
    },
    error = function(e) {
      # fallback più tollerante
      data.table::fread(file_path, sep = "auto", encoding = "UTF-8")
    }
  )
  
  as_tibble(dt)
}

inspect_zip_dataset <- function(zip_path) {
  zip_listing <- list_zip_files(zip_path)
  
  internal_file <- pick_first_tabular_file(zip_listing)
  
  if (is.na(internal_file)) {
    return(tibble(
      source_zip = basename(zip_path),
      file_letto = NA_character_,
      n_osservazioni = NA_integer_,
      n_variabili = NA_integer_,
      variabili_di_interesse = NA_character_,
      formato_file_interno = NA_character_,
      note_tecniche = "Nessun file tabellare riconosciuto nello zip"
    ))
  }
  
  extracted_file <- extract_one_file_from_zip(
    zip_path = zip_path,
    internal_file = internal_file,
    exdir = dir_temp
  )
  
  dati <- read_tabular_file(extracted_file)
  
  tibble(
    source_zip = basename(zip_path),
    file_letto = basename(extracted_file),
    n_osservazioni = nrow(dati),
    n_variabili = ncol(dati),
    variabili_di_interesse = paste(names(dati), collapse = " | "),
    formato_file_interno = tools::file_ext(extracted_file),
    note_tecniche = NA_character_
  )
}


# ............................................................
# 4. Funzioni helper - parsing opzionale HTML locale      ####
# ............................................................
# Questa parte è opzionale e serve solo per valorizzare la colonna
# "ultimo aggiornamento disponibile".
# Lo script funziona anche se il file HTML locale NON esiste.

parse_massivo_metadata_from_html <- function(html_file) {
  if (!file_exists(html_file)) {
    return(list(
      uscite = tibble(),
      entrate = tibble(),
      anagrafiche = tibble()
    ))
  }
  
  doc <- read_html(html_file)
  
  # ---- uscite / entrate ----
  rows_eu <- html_elements(doc, "tr.elencoHeader")
  
  eu_tbl <- purrr::map_dfr(rows_eu, function(r) {
    td <- html_elements(r, "td")
    a  <- html_elements(r, "a.linkDownloadUp")
    
    # scarto righe che non hanno la struttura attesa
    if (length(td) < 6 || length(a) < 4) {
      return(tibble())
    }
    
    tibble(
      anno = suppressWarnings(as.integer(str_squish(html_text2(td[[1]])))),
      data_creazione_uscite = str_squish(html_text2(td[[2]])),
      file_uscite = str_squish(html_text2(a[[1]])),
      href_uscite = html_attr(a[[1]], "href"),
      data_creazione_entrate = str_squish(html_text2(td[[5]])),
      file_entrate = str_squish(html_text2(a[[3]])),
      href_entrate = html_attr(a[[3]], "href")
    )
  })
  
  uscite_tbl <- eu_tbl |>
    transmute(
      anno,
      file = file_uscite,
      data_creazione = data_creazione_uscite,
      href = href_uscite
    ) |>
    filter(!is.na(anno), !is.na(file), file != "")
  
  entrate_tbl <- eu_tbl |>
    transmute(
      anno,
      file = file_entrate,
      data_creazione = data_creazione_entrate,
      href = href_entrate
    ) |>
    filter(!is.na(anno), !is.na(file), file != "")
  
  # ---- anagrafiche ----
  # Cerco il link al file anagrafiche e risalgo alla riga / tabella vicina
  anag_links <- html_elements(doc, "a.linkDownloadUp") |>
    keep(~ str_detect(html_attr(.x, "href") %||% "", "SIOPE_ANAGRAFICHE\\.zip$"))
  
  anag_tbl <- tibble()
  
  if (length(anag_links) > 0) {
    link <- anag_links[[1]]
    href <- html_attr(link, "href")
    file <- str_squish(html_text2(link))
    
    # prendo il tr più vicino e ne leggo i td
    tr_node <- xml2::xml_find_first(link, "ancestor::tr[1]")
    td <- html_elements(tr_node, "td")
    
    data_creazione <- if (length(td) >= 1) str_squish(html_text2(td[[1]])) else NA_character_
    
    anag_tbl <- tibble(
      anno = NA_integer_,
      file = file,
      data_creazione = data_creazione,
      href = href
    )
  }
  
  list(
    uscite = uscite_tbl,
    entrate = entrate_tbl,
    anagrafiche = anag_tbl
  )
}

normalize_id_ente <- function(x) {
  stringr::str_pad(x, width = 15, side = "left", pad = "0")
}


# ............................................................
# 5. Annualità disponibili e intervallo di copertura      ####
# ............................................................
message("Ricerca annualità disponibili per USCITE...")
meta_uscite_anni <- discover_available_years("uscite", anni = anni_candidati)

message("Ricerca annualità disponibili per ENTRATE...")
meta_entrate_anni <- discover_available_years("entrate", anni = anni_candidati)

if (nrow(meta_uscite_anni) == 0) {
  stop("Nessuna annualità disponibile trovata per SIOPE USCITE.")
}
if (nrow(meta_entrate_anni) == 0) {
  stop("Nessuna annualità disponibile trovata per SIOPE ENTRATE.")
}

copertura_uscite <- paste0(min(meta_uscite_anni$anno), "-", max(meta_uscite_anni$anno))
copertura_entrate <- paste0(min(meta_entrate_anni$anno), "-", max(meta_entrate_anni$anno))

annualita_uscite <- paste(meta_uscite_anni$anno, collapse = " | ")
annualita_entrate <- paste(meta_entrate_anni$anno, collapse = " | ")


# ............................................................
# 6. Download dei file targeti                            ####
# ............................................................
url_uscite_target <- build_file_url("uscite", anno_target)
url_entrate_target <- build_file_url("entrate", anno_target)
url_anagrafiche <- build_file_url("anagrafiche")

zip_uscite_target <- file.path(dir_raw, basename(url_uscite_target))
zip_entrate_target <- file.path(dir_raw, basename(url_entrate_target))
zip_anagrafiche <- file.path(dir_raw, basename(url_anagrafiche))

download_if_needed(url_uscite_target, zip_uscite_target)
download_if_needed(url_entrate_target, zip_entrate_target)
download_if_needed(url_anagrafiche, zip_anagrafiche)


# ............................................................
# 7. Ispezione dei file scaricatii                        ####
# ............................................................
info_uscite <- inspect_zip_dataset(zip_uscite_target)
info_entrate <- inspect_zip_dataset(zip_entrate_target)
info_anagrafiche <- inspect_zip_dataset(zip_anagrafiche)


# ............................................................
# 8. Parsing opzionale dei metadati dalla pagina HTML locale ####
# ............................................................
html_meta <- parse_massivo_metadata_from_html(download_massivo_html_file)

ultimo_aggiornamento_uscite <- html_meta$uscite |>
  filter(anno == anno_target) |>
  summarise(val = dplyr::first(data_creazione)) |>
  pull(val)

ultimo_aggiornamento_entrate <- html_meta$entrate |>
  filter(anno == anno_target) |>
  summarise(val = dplyr::first(data_creazione)) |>
  pull(val)

ultimo_aggiornamento_anagrafiche <- html_meta$anagrafiche |>
  summarise(val = dplyr::first(data_creazione)) |>
  pull(val)

if (length(ultimo_aggiornamento_uscite) == 0) ultimo_aggiornamento_uscite <- NA_character_
if (length(ultimo_aggiornamento_entrate) == 0) ultimo_aggiornamento_entrate <- NA_character_
if (length(ultimo_aggiornamento_anagrafiche) == 0) ultimo_aggiornamento_anagrafiche <- NA_character_


# ............................................................
# 9. Lettura dei flussi annuali completi                    ####
# ............................................................

read_first_data_file_from_zip <- function(zip_path, exdir = dir_temp) {
  internal_file <- pick_first_tabular_file(list_zip_files(zip_path))
  
  if (is.na(internal_file)) {
    stop("Nessun file tabellare trovato nello zip: ", zip_path)
  }
  
  extracted_file <- extract_one_file_from_zip(
    zip_path = zip_path,
    internal_file = internal_file,
    exdir = exdir
  )
  
  dati <- data.table::fread(
    extracted_file,
    header = FALSE,
    sep = ",",
    quote = "\"",
    encoding = "UTF-8",
    colClasses = c("character", "integer", "character", "character", "numeric")
  ) |>
    as_tibble()
  
  names(dati) <- c("V1", "V2", "V3", "V4", "V5")
  
  list(
    data = dati,
    extracted_file = extracted_file,
    internal_file = internal_file
  )
}

uscite_obj <- read_first_data_file_from_zip(zip_uscite_target)
entrate_obj <- read_first_data_file_from_zip(zip_entrate_target)

uscite_raw <- uscite_obj$data
entrate_raw <- entrate_obj$data

# ............................................................
# 10. Estrazione anagrafiche                                 ####
# ............................................................

extract_anag_file <- function(zip_path, filename, exdir = dir_temp) {
  unzip(
    zipfile = zip_path,
    files = filename,
    exdir = exdir,
    overwrite = TRUE
  )[[1]]
}

anag_files <- list_zip_files(zip_anagrafiche)$Name

file_codgest_usc <- extract_anag_file(zip_anagrafiche, grep("^ANAG_CODGEST_USCITE", anag_files, value = TRUE))
file_codgest_ent <- extract_anag_file(zip_anagrafiche, grep("^ANAG_CODGEST_ENTRATE", anag_files, value = TRUE))
file_enti        <- extract_anag_file(zip_anagrafiche, grep("^ANAG_ENTI_SIOPE", anag_files, value = TRUE))
file_comparti    <- extract_anag_file(zip_anagrafiche, grep("^ANAG_COMPARTI", anag_files, value = TRUE))
file_sottocomp   <- extract_anag_file(zip_anagrafiche, grep("^ANAG_SOTTOCOMPARTI", anag_files, value = TRUE))
file_reg_prov    <- extract_anag_file(zip_anagrafiche, grep("^ANAG_REG_PROV", anag_files, value = TRUE))
file_comuni      <- extract_anag_file(zip_anagrafiche, grep("^ANAGRAFE_COMUNI", anag_files, value = TRUE))


# ............................................................
# 11. Lettura anagrafiche e dizionari                        ####
# ............................................................

read_anag_csv <- function(path) {
  data.table::fread(
    path,
    header = FALSE,
    sep = ",",
    quote = "\"",
    encoding = "UTF-8",
    colClasses = "character"
  ) |>
    as_tibble()
}

anag_codgest_usc <- read_anag_csv(file_codgest_usc) |>
  setNames(c(
    "codice_gestionale_siope",
    "profilo_codgest",
    "descrizione_codgest",
    "data_inizio_validita_codgest",
    "data_fine_validita_codgest"
  ))

anag_codgest_ent <- read_anag_csv(file_codgest_ent) |>
  setNames(c(
    "codice_gestionale_siope",
    "profilo_codgest",
    "descrizione_codgest",
    "data_inizio_validita_codgest",
    "data_fine_validita_codgest"
  ))

anag_enti <- read_anag_csv(file_enti) |>
  setNames(c(
    "codice_ente_siope",
    "data_inizio_validita_ente",
    "data_fine_validita_ente",
    "codice_fiscale_ente",
    "denominazione_ente",
    "codice_comune",
    "codice_provincia",
    "campo_territoriale_altro",
    "codice_sottocomparto"
  )) |>
  mutate(
    codice_ente_siope = as.character(codice_ente_siope),
    codice_comune = as.character(codice_comune),
    codice_provincia = as.character(codice_provincia),
    codice_ente_join = normalize_id_ente(codice_ente_siope)
  )

anag_comparti <- read_anag_csv(file_comparti) |>
  setNames(c(
    "codice_comparto",
    "descrizione_comparto"
  ))

anag_sottocomparti <- read_anag_csv(file_sottocomp) |>
  setNames(c(
    "codice_sottocomparto",
    "descrizione_sottocomparto",
    "codice_comparto"
  ))

anag_reg_prov <- read_anag_csv(file_reg_prov) |>
  setNames(c(
    "macroarea",
    "codice_regione",
    "denominazione_regione",
    "codice_provincia",
    "denominazione_provincia"
  )) |>
  mutate(codice_provincia = as.character(codice_provincia))

anagrafe_comuni <- read_anag_csv(file_comuni) |>
  setNames(c(
    "codice_comune",
    "denominazione_comune",
    "codice_provincia"
  )) |>
  mutate(
    codice_comune = as.character(codice_comune),
    codice_provincia = as.character(codice_provincia)
  )


# ............................................................
# 12. Rinomina prudente dei flussi                           ####
# ............................................................

uscite <- uscite_raw |>
  rename(
    codice_ente_siope = V1,
    anno = V2,
    V3 = V3,
    codice_gestionale_siope = V4,
    importo = V5
  ) |>
  mutate(
    codice_ente_siope = as.character(codice_ente_siope),
    anno = as.integer(anno),
    V3 = as.character(V3),
    codice_gestionale_siope = as.character(codice_gestionale_siope),
    importo = as.numeric(importo),
    codice_ente_join = normalize_id_ente(codice_ente_siope)
  )

entrate <- entrate_raw |>
  rename(
    codice_ente_siope = V1,
    anno = V2,
    V3 = V3,
    codice_gestionale_siope = V4,
    importo = V5
  ) |>
  mutate(
    codice_ente_siope = as.character(codice_ente_siope),
    anno = as.integer(anno),
    V3 = as.character(V3),
    codice_gestionale_siope = as.character(codice_gestionale_siope),
    importo = as.numeric(importo),
    codice_ente_join = normalize_id_ente(codice_ente_siope)
  )

# ............................................................
# 13. Dizionario enti arricchito                             ####
# ............................................................

diz_enti <- anag_enti |>
  left_join(anag_sottocomparti, by = "codice_sottocomparto") |>
  left_join(anag_comparti, by = "codice_comparto") |>
  left_join(anag_reg_prov, by = "codice_provincia") |>
  left_join(anagrafe_comuni, by = c("codice_comune", "codice_provincia"))

# ............................................................
# 14. Merge con le anagrafiche                               ####
# ............................................................

uscite_arricchite <- uscite |>
  left_join(
    diz_enti |> select(-codice_ente_siope),
    by = "codice_ente_join"
  ) |>
  left_join(anag_codgest_usc, by = "codice_gestionale_siope")

entrate_arricchite <- entrate |>
  left_join(
    diz_enti |> select(-codice_ente_siope),
    by = "codice_ente_join"
  ) |>
  left_join(anag_codgest_ent, by = "codice_gestionale_siope")

# ............................................................
# 15. Test di coerenza                                       ####
# ............................................................
head(uscite$codice_ente_siope, 20)
head(anag_enti$codice_ente_siope, 20)

uscite |> mutate(nchar = nchar(codice_ente_siope)) |> count(nchar)
anag_enti |> mutate(nchar = nchar(codice_ente_siope)) |> count(nchar)


length(intersect(
  unique(uscite$codice_ente_siope),
  unique(anag_enti$codice_ente_siope)
))

uscite_match <- uscite_arricchite |>
  summarise(
    pct_match_enti = mean(!is.na(denominazione_ente)),
    pct_match_codgest = mean(!is.na(descrizione_codgest))
  )

entrate_match <- entrate_arricchite |>
  summarise(
    pct_match_enti = mean(!is.na(denominazione_ente)),
    pct_match_codgest = mean(!is.na(descrizione_codgest))
  )

print(uscite_match)
print(entrate_match)

# ............................................................
# 16. Metadati finali per catalogo                          ####
# ............................................................

# Variabili interpretate per i flussi annuali
variabili_flussi <- "codice_ente_siope | anno | V3 | codice_gestionale_siope | importo"

# Unità di osservazione interpretata
unita_osservazione_flussi <- paste(
  "Osservazione aggregata annuale per ente, codice gestionale SIOPE e valore di V3,",
  "con importo monetario registrato secondo il criterio di cassa.",
  "La chiave empiricamente osservata è: codice_ente_siope + anno + V3 + codice_gestionale_siope."
)

# Variabili anagrafiche principali usate per arricchire i flussi
variabili_anagrafica_enti <- paste(
  c(
    "codice_ente_siope",
    "data_inizio_validita_ente",
    "data_fine_validita_ente",
    "codice_fiscale_ente",
    "denominazione_ente",
    "codice_comune",
    "codice_provincia",
    "campo_territoriale_altro",
    "codice_sottocomparto",
    "codice_ente_join",
    "descrizione_sottocomparto",
    "codice_comparto",
    "descrizione_comparto",
    "macroarea",
    "codice_regione",
    "denominazione_regione",
    "denominazione_provincia",
    "denominazione_comune"
  ),
  collapse = " | "
)

unita_osservazione_anagrafiche <- paste(
  "Record anagrafico di riferimento per ente o classificazione di supporto",
  "(enti, codici gestionali, comparti, sottocomparti, comuni, province e regioni)."
)

# Numero osservazioni / variabili per anagrafiche:
# qui uso ANAG_ENTI_SIOPE come riferimento principale, perché è la tabella anagrafica chiave
n_osservazioni_anagrafiche <- nrow(anag_enti)
n_variabili_anagrafiche <- ncol(anag_enti)

# Se vuoi usare invece tutte le anagrafiche come 'pacchetto', puoi scriverlo nelle note


# ............................................................
# 17. Costruzione catalogo finale                           ####
# ............................................................

catalogo_finale <- tibble(
  `Dati SIOPE - dataset` = c(
    "SIOPE uscite",
    "SIOPE entrate",
    "SIOPE anagrafiche"
  ),
  `periodo/annualità disponibili` = c(
    copertura_uscite,
    copertura_entrate,
    NA_character_
  ),
  `ultimo aggiornamento disponibile` = c(
    ultimo_aggiornamento_uscite,
    ultimo_aggiornamento_entrate,
    ultimo_aggiornamento_anagrafiche
  ),
  `variabili di interesse` = c(
    variabili_flussi,
    variabili_flussi,
    variabili_anagrafica_enti
  ),
  `n_osservazioni` = c(
    nrow(uscite),
    nrow(entrate),
    n_osservazioni_anagrafiche
  ),
  `n_variabili` = c(
    ncol(uscite),
    ncol(entrate),
    n_variabili_anagrafiche
  ),
  `unità di osservazione` = c(
    unita_osservazione_flussi,
    unita_osservazione_flussi,
    unita_osservazione_anagrafiche
  ),
  `modalità di accesso` = c(
    "Download diretto via URL HTTPS",
    "Download diretto via URL HTTPS",
    "Download diretto via URL HTTPS"
  ),
  `limiti tecnici (rate limit)` = c(
    NA_character_,
    NA_character_,
    NA_character_
  ),
  `formati scarico dati` = c(
    paste0("zip (contiene .", info_uscite$formato_file_interno, ")"),
    paste0("zip (contiene .", info_entrate$formato_file_interno, ")"),
    "zip (contiene più file CSV anagrafici)"
  ),
  `note` = c(
    paste0(
      "Annualità disponibili rilevate via URL: ", annualita_uscite,
      ". Match codici gestionali = ",
      round(uscite_match$pct_match_codgest, 3),
      ". Match enti dopo normalizzazione del codice a 15 cifre = ",
      round(uscite_match$pct_match_enti, 3),
      ". V3 mantenuta non interpretata."
    ),
    paste0(
      "Annualità disponibili rilevate via URL: ", annualita_entrate,
      ". Match codici gestionali = ",
      round(entrate_match$pct_match_codgest, 3),
      ". Match enti dopo normalizzazione del codice a 15 cifre = ",
      round(entrate_match$pct_match_enti, 3),
      ". V3 mantenuta non interpretata."
    ),
    paste0(
      "Pacchetto anagrafico composto da più file CSV: ANAG_ENTI_SIOPE, ANAG_CODGEST_ENTRATE, ",
      "ANAG_CODGEST_USCITE, ANAG_COMPARTI, ANAG_SOTTOCOMPARTI, ANAG_REG_PROV, ANAGRAFE_COMUNI. ",
      if (file_exists(download_massivo_html_file)) {
        "Data aggiornamento letta da HTML locale."
      } else {
        "Campo aggiornamento eventualmente vuoto: HTML locale non disponibile."
      }
    )
  )
)


# ............................................................
# 18. Export metadati di supporto                           ####
# ............................................................

write_csv(meta_uscite_anni, file.path(dir_output, "siope_annualita_disponibili_uscite.csv"))
write_csv(meta_entrate_anni, file.path(dir_output, "siope_annualita_disponibili_entrate.csv"))
write_csv(catalogo_finale, file.path(dir_output, "catalogo_dataset_siope.csv"))


# ............................................................
# 19. Export Excel finale                                   ####
# ............................................................

wb <- createWorkbook()

addWorksheet(wb, "catalogo_siope")
writeData(wb, "catalogo_siope", catalogo_finale)

header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE
)

body_style <- createStyle(
  valign = "top",
  wrapText = TRUE
)

addStyle(
  wb, "catalogo_siope",
  style = header_style,
  rows = 1, cols = 1:ncol(catalogo_finale),
  gridExpand = TRUE
)

addStyle(
  wb, "catalogo_siope",
  style = body_style,
  rows = 2:(nrow(catalogo_finale) + 1),
  cols = 1:ncol(catalogo_finale),
  gridExpand = TRUE,
  stack = TRUE
)

setColWidths(wb, "catalogo_siope", cols = 1:ncol(catalogo_finale), widths = "auto")
freezePane(wb, "catalogo_siope", firstRow = TRUE)

saveWorkbook(
  wb,
  file = file.path(dir_output, "catalogo_dataset_siope.xlsx"),
  overwrite = TRUE
)

# ............................................................
# 20. Output finale a console                               ####
# ............................................................

message("Catalogo finale creato con successo.")
print(catalogo_finale)

