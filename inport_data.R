library(tidyverse)
library(openxlsx)

det_proc <- read_delim("data/ItaliaSemplice_DettaglioProcedure.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

summary(det_proc)
names(det_proc)
# col_to_parse <- c("ID", "URL", "Nome Procedura", "Settore", "Categoria", "Beneficiario", "Intervento", "Tipo PA Responsabile", "Tipo Intervento", "Natura Intervento", "Anno", "Descrizione Intervento", "Riferimenti")

col_to_parse <- c("Settore", "Categoria", "Beneficiario",  "Tipo PA Responsabile", "Tipo Intervento", "Natura Intervento", "Anno", "Riferimenti")

det_proc[col_to_parse] <- lapply(det_proc[col_to_parse], factor)  ## as.factor() could also be used

summary(det_proc)
colSums(is.na(det_proc))



# ---- Helper: export data.frame -> styled Excel ----
write_styled_xlsx <- function(df,
                              path,
                              sheet_name = "Dati",
                              header_fill = "#1F4E79",
                              header_font = "#FFFFFF",
                              wrap_cols = NULL,         # NULL = wrap su tutte le colonne
                              max_col_width = 60,       # limite per non creare colonne enormi
                              min_col_width = 10,
                              freeze_first_row = TRUE,
                              add_filters = TRUE) {
  
  stopifnot(is.data.frame(df))
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name)
  
  # Scrivi dati (con header)
  writeData(wb, sheet = sheet_name, x = df, headerStyle = NULL)
  
  n_rows <- nrow(df) + 1  # + header
  n_cols <- ncol(df)
  
  # Stile header
  header_style <- createStyle(
    fgFill = header_fill,
    fontColour = header_font,
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE,
    border = "Bottom",
    borderColour = "#9AA4AF"
  )
  addStyle(wb, sheet = sheet_name, style = header_style,
           rows = 1, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
  
  # Freeze riga header
  if (freeze_first_row) {
    freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  }
  
  # Filtri sulla riga header
  if (add_filters) {
    addFilter(wb, sheet = sheet_name, rows = 1, cols = 1:n_cols)
  }
  
  # Wrap testo (tutte o subset colonne)
  wrap_style <- createStyle(wrapText = TRUE, valign = "top")
  if (is.null(wrap_cols)) {
    wrap_cols <- 1:n_cols
  } else {
    # consenti passare nomi di colonne
    if (is.character(wrap_cols)) {
      wrap_cols <- match(wrap_cols, names(df))
      wrap_cols <- wrap_cols[!is.na(wrap_cols)]
    }
  }
  if (length(wrap_cols) > 0) {
    addStyle(wb, sheet = sheet_name, style = wrap_style,
             rows = 2:n_rows, cols = wrap_cols, gridExpand = TRUE, stack = TRUE)
  }
  
  # Auto width colonne + clamp min/max
  # openxlsx calcola width dai contenuti; poi possiamo limitarlo.
  setColWidths(wb, sheet = sheet_name, cols = 1:n_cols, widths = "auto")
  
  # Clamp widths: openxlsx non espone direttamente le widths dopo "auto" in modo comodissimo
  # quindi applichiamo una strategia: ricalcoliamo una stima con nchar.
  # (è un'approssimazione, ma funziona bene in pratica)
  est_width <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    # prendi la stringa più lunga, ma limita l'impatto di outlier estremi
    p <- quantile(nchar(x), probs = 0.95, names = FALSE, type = 7)
    # aggiusta per header
    p <- max(p, nchar(deparse(substitute(x))))
    # scala a "larghezza excel-like"
    w <- p * 0.9 + 2
    w
  }
  
  widths <- vapply(df, est_width, numeric(1))
  # considera anche i nomi colonna
  widths <- pmax(widths, nchar(names(df)) * 0.9 + 2)
  widths <- pmin(pmax(widths, min_col_width), max_col_width)
  
  setColWidths(wb, sheet = sheet_name, cols = 1:n_cols, widths = widths)
  
  # Altezze righe: opzionale. Con wrap, Excel gestisce, ma a volte serve un minimo.
  # Qui lasciamo default per non appesantire file.
  
  # Formato tabella "semplice": bordi leggeri sulla griglia (facoltativo)
  grid_style <- createStyle(border = "TopBottomLeftRight", borderColour = "#E5E7EB")
  addStyle(wb, sheet = sheet_name, style = grid_style,
           rows = 1:n_rows, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
  
  saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# ---- Esempio 1: se hai già un data frame in R ----
write_styled_xlsx(det_proc, "dati_puliti.xlsx", sheet_name = "Procedure")
