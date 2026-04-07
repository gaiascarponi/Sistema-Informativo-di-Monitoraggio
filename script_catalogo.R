# ============================================================
# 📌 COME USARE LA FUNZIONE export_catalogo_report
# ============================================================
#
# Questa funzione crea un file Word dove:
# - ogni riga del foglio "Catalogo" diventa una SCHEDA
# - ogni scheda è su UNA PAGINA
# - i dati sono in formato verticale (tipo report)
#
# ------------------------------------------------------------
# 🔹 OPZIONE 1: ESPORTARE TUTTE LE SCHEDE
# ------------------------------------------------------------
# Usa questa quando vuoi esportare tutto il catalogo
#
# export_catalogo_report(
#   file_excel = "Data_catalog v.4.xlsx",   # nome file Excel
#   file_word  = "catalogo_tutto.docx",     # nome file Word output
#   export_all = TRUE                      # TRUE = prende tutte le righe
# )
#
#
# ------------------------------------------------------------
# 🔹 OPZIONE 2: ESPORTARE SOLO ALCUNE DENOMINAZIONI
# ------------------------------------------------------------
# Inserisci i nomi ESATTI delle denominazioni
#
# export_catalogo_report(
#   file_excel = "Data_catalog v.4.xlsx",
#   file_word  = "catalogo_selezionato.docx",
#   denominazioni = c("Conto annuale", "OpenBdap"),  # elenco
#   export_all = FALSE
# )
#
#
# ------------------------------------------------------------
# 🔹 OPZIONE 3: RICERCA LIBERA (per parola)
# ------------------------------------------------------------
# Trova tutte le denominazioni che CONTENGONO una parola
#
# export_catalogo_report(
#   file_excel = "Data_catalog v.4.xlsx",
#   file_word  = "catalogo_ricerca.docx",
#   denominazioni = "conto",   # parola da cercare
#   export_all = FALSE,
#   match_exact = FALSE        # FALSE = ricerca parziale
# )



# ============================================================
# ESPORTAZIONE FOGLIO "Catalogo" IN WORD
# FORMATO: 1 PAGINA PER OGNI SCHEDA
# ============================================================

# install.packages(c("readxl", "dplyr", "tidyr", "officer", "flextable", "stringr"))

library(readxl)
library(dplyr)
library(tidyr)
library(officer)
library(flextable)
library(stringr)

export_catalogo_report <- function(file_excel,
                                   file_word = "catalogo_report.docx",
                                   foglio = "Catalogo",
                                   denominazioni = NULL,
                                   export_all = FALSE,
                                   match_exact = TRUE) {
  
  # ------------------------------------------------------------
  # 1. Legge il file Excel
  # ------------------------------------------------------------
  dati <- read_excel(file_excel, sheet = foglio)
  
  # Elimina colonne completamente vuote
  dati <- dati %>% select(where(~ !all(is.na(.))))
  
  # Controlla colonna Denominazione
  if (!"Denominazione" %in% names(dati)) {
    stop("Non trovo la colonna 'Denominazione' nel foglio.")
  }
  
  # Pulisce spazi
  dati <- dati %>%
    mutate(Denominazione = str_trim(as.character(Denominazione)))
  
  # ------------------------------------------------------------
  # 2. Filtra righe
  # ------------------------------------------------------------
  if (export_all) {
    dati_filtrati <- dati
  } else {
    if (is.null(denominazioni) || length(denominazioni) == 0) {
      stop("Devi indicare almeno una denominazione oppure usare export_all = TRUE.")
    }
    
    denominazioni <- str_trim(denominazioni)
    
    if (match_exact) {
      dati_filtrati <- dati %>%
        filter(Denominazione %in% denominazioni)
    } else {
      pattern <- paste(denominazioni, collapse = "|")
      dati_filtrati <- dati %>%
        filter(str_detect(Denominazione, regex(pattern, ignore_case = TRUE)))
    }
  }
  
  if (nrow(dati_filtrati) == 0) {
    stop("Nessuna riga trovata.")
  }
  
  # ------------------------------------------------------------
  # 3. Crea documento Word
  # ------------------------------------------------------------
  doc <- read_docx()
  
  # ------------------------------------------------------------
  # 4. Crea una pagina per ogni riga
  # ------------------------------------------------------------
  for (i in seq_len(nrow(dati_filtrati))) {
    
    riga <- dati_filtrati[i, , drop = FALSE]
    
    titolo <- paste0("Fonte dati: ", riga$Denominazione)
    
    # Trasforma la riga in formato verticale
    riga_long <- riga %>%
      mutate(id_temp = 1) %>%
      pivot_longer(
        cols = -id_temp,
        names_to = "Variabile",
        values_to = "Valore"
      ) %>%
      select(-id_temp) %>%
      mutate(
        Variabile = as.character(Variabile),
        Valore = ifelse(is.na(Valore), "", as.character(Valore))
      )
    
    # Facoltativo: togli righe completamente vuote
    riga_long <- riga_long %>%
      filter(!(Valore == "" & Variabile == ""))
    
    # Crea tabella più leggibile
    ft <- flextable(riga_long)
    
    ft <- theme_vanilla(ft)
    ft <- autofit(ft)
    
    # RIMUOVE la riga "Variabile | Valore"
    ft <- delete_part(ft, part = "header")
    
    # grassetto sulla colonna sinistra (nomi variabili)
    ft <- bold(ft, j = 1, bold = TRUE)
    
    # allineamenti
    ft <- align(ft, j = 1, align = "left", part = "all")
    ft <- align(ft, j = 2, align = "left", part = "all")
    
    # larghezze
    ft <- width(ft, j = 1, width = 2.6)
    ft <- width(ft, j = 2, width = 4.8)
    
    # testo più compatto
    ft <- fontsize(ft, size = 10, part = "all")
    
    # Aggiunge titolo + tabella
    doc <- doc %>%
      body_add_par(value = titolo, style = "heading 1") %>%
      body_add_par(value = "", style = "Normal") %>%
      body_add_flextable(ft)
    
    # Interruzione di pagina, tranne dopo l'ultima scheda
    if (i < nrow(dati_filtrati)) {
      doc <- doc %>%
        body_add_break(pos = "after")
    }
  }
  
  # ------------------------------------------------------------
  # 5. Salva Word
  # ------------------------------------------------------------
  print(doc, target = file_word)
  
  message("File Word creato correttamente: ", file_word)
}
