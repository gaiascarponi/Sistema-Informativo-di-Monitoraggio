# ............................................................. #
# Script: 05_dashboard_SIM_ANAC.R
# ............................................................. #
library(shiny)
library(bslib)
library(jsonlite)
library(dplyr)
library(plotly)
library(tidyr)
library(leaflet)
library(sf)
library(DT)
library(ggplot2)

# --- 1. CARICAMENTO E PREPARAZIONE DATI ---

# Caricamento Indicatori ANAC (livello PA)
df_anac_raw <- fromJSON("07_Temp/INDICATORS_ANAC.json") %>%
  mutate(ind_val = as.numeric(ind_val))

# --- CARICAMENTO INDICATORI CPV ---
df_cpv_raw <- fromJSON("07_Temp/INDICATORS_CPV_ANAC.json") %>%
  mutate(ind_val = as.numeric(ind_val))

# Estrazione lista settori (CPV)
lista_cpv <- df_cpv_raw %>%
  filter(!is.na(cpv)) %>%
  pull(cpv) %>% unique() %>% sort()

# Estrazione lista Forme Giuridiche dal dataset CPV
lista_fg_cpv <- df_cpv_raw %>%
  filter(sub_fil == "fil_fg" | subsub_fil == "fil_fg") %>%
  pull(ifelse(sub_fil == "fil_fg", sub_fil_val, subsub_fil_val)) %>%
  unique() %>% sort()

# Caricamento Mappatura Regioni
mappatura_regioni <- readRDS("07_Temp/Lista_raccordo_SIM.rds") %>%
  select(codice_fiscale, codice_reg, regione_bdap) %>%
  distinct()

# Download Confini Regioni
geojson_url <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_regions.geojson"
regioni_shape <- read_sf(geojson_url) %>%
  mutate(reg_code = sprintf("%02d", as.numeric(reg_istat_code_num)))

# PRE-AGGREGAZIONE NAZIONALE (ind1-ind9)
df_anac_nat <- df_anac_raw %>%
  filter(ind %in% paste0("ind", 1:9), fil == "fil_anno") %>%
  group_by(fil_val, ind) %>%
  summarise(val_nazionale = mean(ind_val, na.rm = TRUE), .groups = "drop")

# Mappatura pulita per nomi regioni
nomi_regioni_univoci <- mappatura_regioni %>%
  filter(!is.na(regione_bdap)) %>%
  select(codice_reg, regione_bdap) %>%
  distinct(codice_reg, .keep_all = TRUE)

# Lista Forme Giuridiche (dal dataset ANAC principale)
lista_fg <- df_anac_raw %>%
  filter(!is.na(subsub_fil_val), subsub_fil == "fil_fg") %>%
  pull(subsub_fil_val) %>%
  unique() %>%
  sort()

# ............................................................. #
# AGGREGAZIONE REGIONALE — versione corretta
#
# ATTENZIONE: ind14/ind16/ind18/ind19/ind20 (e i loro equivalenti
# mensili ind25/ind27/ind29/ind30/ind31) sono percentuali o medie
# calcolate a livello di singola PA: non si possono sommare tra PA
# per ottenere il dato regionale. Li ricalcoliamo come media pesata
# sul numero di gare di ciascuna PA (ind10 / ind21), usando solo
# gli indicatori "grezzi" additivi (ind10, ind11, ind15 / ind21,
# ind22, ind26) come base.
# ............................................................. #

# --- Tier annuale (ind10, ind11, ind15, ind18, ind19, ind20) ---
df_pa_annuale <- df_anac_raw %>%
  filter(ind %in% c("ind10", "ind11", "ind15", "ind18", "ind19", "ind20"), fil == "fil_anno") %>%
  left_join(mappatura_regioni %>% select(codice_fiscale, codice_reg), by = c("pa" = "codice_fiscale")) %>%
  filter(!is.na(codice_reg)) %>%
  mutate(codice_reg = sprintf("%02d", as.numeric(codice_reg))) %>%
  select(codice_reg, fil_val, pa, ind, ind_val) %>%
  pivot_wider(names_from = ind, values_from = ind_val)

df_reg_annuale <- df_pa_annuale %>%
  group_by(codice_reg, fil_val) %>%
  summarise(
    ind10 = sum(ind10, na.rm = TRUE),
    ind11 = sum(ind11, na.rm = TRUE),
    ind15 = sum(ind15, na.rm = TRUE),
    ind18 = if (sum(ind10, na.rm = TRUE) > 0) sum(ind18 * ind10, na.rm = TRUE) / sum(ind10, na.rm = TRUE) else NA_real_,
    ind19 = if (sum(ind10, na.rm = TRUE) > 0) sum(ind19 * ind10, na.rm = TRUE) / sum(ind10, na.rm = TRUE) else NA_real_,
    ind20 = if (sum(ind10, na.rm = TRUE) > 0) sum(ind20 * ind10, na.rm = TRUE) / sum(ind10, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    ind14 = if_else(ind10 > 0, ind11 / ind10 * 100, NA_real_),  # % aggiudicazione regionale (corretta)
    ind16 = if_else(ind10 > 0, ind15 / ind10, NA_real_)          # importo medio per gara regionale (corretto)
  ) %>%
  pivot_longer(cols = starts_with("ind"), names_to = "ind", values_to = "ind_val") %>%
  mutate(sub_fil_val = NA_character_)

# --- Tier mensile (ind21, ind22, ind26, ind29, ind30, ind31) ---
df_pa_mensile <- df_anac_raw %>%
  filter(ind %in% c("ind21", "ind22", "ind26", "ind29", "ind30", "ind31"), fil == "fil_anno", sub_fil == "fil_mese") %>%
  left_join(mappatura_regioni %>% select(codice_fiscale, codice_reg), by = c("pa" = "codice_fiscale")) %>%
  filter(!is.na(codice_reg)) %>%
  mutate(codice_reg = sprintf("%02d", as.numeric(codice_reg))) %>%
  select(codice_reg, fil_val, sub_fil_val, pa, ind, ind_val) %>%
  pivot_wider(names_from = ind, values_from = ind_val)

df_reg_mensile <- df_pa_mensile %>%
  group_by(codice_reg, fil_val, sub_fil_val) %>%
  summarise(
    ind21 = sum(ind21, na.rm = TRUE),
    ind22 = sum(ind22, na.rm = TRUE),
    ind26 = sum(ind26, na.rm = TRUE),
    ind29 = if (sum(ind21, na.rm = TRUE) > 0) sum(ind29 * ind21, na.rm = TRUE) / sum(ind21, na.rm = TRUE) else NA_real_,
    ind30 = if (sum(ind21, na.rm = TRUE) > 0) sum(ind30 * ind21, na.rm = TRUE) / sum(ind21, na.rm = TRUE) else NA_real_,
    ind31 = if (sum(ind21, na.rm = TRUE) > 0) sum(ind31 * ind21, na.rm = TRUE) / sum(ind21, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    ind25 = if_else(ind21 > 0, ind22 / ind21 * 100, NA_real_),
    ind27 = if_else(ind21 > 0, ind26 / ind21, NA_real_)
  ) %>%
  pivot_longer(cols = starts_with("ind"), names_to = "ind", values_to = "ind_val")

# --- Unione tier annuale + mensile, join nomi regione ---
df_anac_reg <- bind_rows(df_reg_annuale, df_reg_mensile) %>%
  left_join(nomi_regioni_univoci, by = "codice_reg")

# Etichette leggibili per il selettore "Indicatore" regionale (annuali + mensili)
nomi_indicatori_reg <- c(
  ind10 = "Numero Gare", ind11 = "Gare Aggiudicate (n)", ind14 = "% Aggiudicazione",
  ind15 = "Importo Totale (€)", ind16 = "Importo Medio per Gara (€)",
  ind18 = "Durata Media Prevista (gg)", ind19 = "Giorni Scadenza-Esito",
  ind20 = "Finestra Partecipazione (gg)",
  ind21 = "Gare (Mese)", ind22 = "Aggiudicate (Mese)", ind25 = "% Aggiudicazione (Mese)",
  ind26 = "Importo Totale Mese (€)", ind27 = "Importo Medio per Gara - Mese (€)",
  ind29 = "Durata Media Prevista - Mese (gg)", ind30 = "Giorni Scadenza-Esito - Mese",
  ind31 = "Finestra Partecipazione - Mese (gg)"
)

# Helper: formatta un valore in base al tipo di indicatore (%, €, gg, conteggio)
fmt_valore_indicatore <- function(x, ind_code) {
  if (ind_code %in% c("ind14", "ind25", "ind36")) {
    paste0(format(round(x, 1), decimal.mark = ","), "%")
  } else if (ind_code %in% c("ind15", "ind16", "ind26", "ind27", "ind37", "ind38")) {
    paste0("€ ", format(round(x, 0), big.mark = ".", decimal.mark = ","))
  } else if (ind_code %in% c("ind18", "ind19", "ind20", "ind29", "ind30", "ind31", "ind40", "ind41", "ind42")) {
    paste0(format(round(x, 1), decimal.mark = ","), " gg")
  } else {
    format(round(x, 0), big.mark = ".", decimal.mark = ",")
  }
}

# ............................................................. #
# CONFRONTO FORME GIURIDICHE (ind32-42) — stessa logica di
# aggregazione pesata, applicata raggruppando per forma giuridica
# invece che per regione.
# ............................................................. #

df_pa_fg <- df_anac_raw %>%
  filter(ind %in% c("ind32", "ind33", "ind37", "ind40", "ind41", "ind42"), fil == "fil_anno", sub_fil == "fil_fg") %>%
  select(fil_val, sub_fil_val, pa, ind, ind_val) %>%
  pivot_wider(names_from = ind, values_from = ind_val)

df_fg_confronto <- df_pa_fg %>%
  group_by(sub_fil_val, fil_val) %>%
  summarise(
    ind32 = sum(ind32, na.rm = TRUE),
    ind33 = sum(ind33, na.rm = TRUE),
    ind37 = sum(ind37, na.rm = TRUE),
    ind40 = if (sum(ind32, na.rm = TRUE) > 0) sum(ind40 * ind32, na.rm = TRUE) / sum(ind32, na.rm = TRUE) else NA_real_,
    ind41 = if (sum(ind32, na.rm = TRUE) > 0) sum(ind41 * ind32, na.rm = TRUE) / sum(ind32, na.rm = TRUE) else NA_real_,
    ind42 = if (sum(ind32, na.rm = TRUE) > 0) sum(ind42 * ind32, na.rm = TRUE) / sum(ind32, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    ind36 = if_else(ind32 > 0, ind33 / ind32 * 100, NA_real_),
    ind38 = if_else(ind32 > 0, ind37 / ind32, NA_real_)
  )

nomi_indicatori_fg <- c(
  ind32 = "Numero Gare", ind33 = "Gare Aggiudicate (n)", ind36 = "% Aggiudicazione",
  ind37 = "Importo Totale (€)", ind38 = "Importo Medio per Gara (€)",
  ind40 = "Durata Media Prevista (gg)", ind41 = "Giorni Scadenza-Esito",
  ind42 = "Finestra Partecipazione (gg)"
)

# --- 2. INTERFACCIA UTENTE (UI) ---
ui <- page_navbar(
  title = "Monitoraggio ANAC",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  header = tags$head(tags$style(HTML("
    .container-fluid { padding: 0 !important; }
    .value-box .value { font-size: 2.2rem !important; font-weight: 700; }
    .value-box .title { font-size: 0.9rem !important; text-transform: uppercase; opacity: 0.8; }
  "))),
  
  # SCHEDA 1: OVERVIEW
  nav_panel("Overview Nazionale",
            layout_sidebar(
              sidebar = sidebar(
                width = 250, title = "Filtro",
                selectInput("anno_ov", "Seleziona Anno:", choices = sort(unique(df_anac_nat$fil_val), decreasing = TRUE))
              ),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                value_box(title = "PA Matchate (n)", value = textOutput("val_ind1"), showcase = icon("university"), theme = "primary"),
                value_box(title = "PA Matchate (%)", value = textOutput("val_ind2"), showcase = icon("percent"), theme = "primary"),
                value_box(title = "Numero totale Gare", value = textOutput("val_ind3"), showcase = icon("folder-open"), theme = "primary")
              ),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                value_box(title = "Gare Aggiudicate (n)", value = textOutput("val_ind4"), showcase = icon("check-circle"), theme = "success"),
                value_box(title = "Gare Aggiudicate (%)", value = textOutput("val_ind9"), showcase = icon("chart-pie"), theme = "success"),
                value_box(title = "Importo di Lotto Medio", value = textOutput("val_ind5"), showcase = icon("euro-sign"), theme = "success")
              ),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                value_box(title = "Durata Prevista Media", value = textOutput("val_ind6"), showcase = icon("calculator"), theme = "orange"),
                value_box(title = "Tempo medio scadenza-esito", value = textOutput("val_ind7"), showcase = icon("clock"), theme = "orange"),
                value_box(title = "Tempo medio finestra part.", value = textOutput("val_ind8"), showcase = icon("hourglass-half"), theme = "orange")
              ),
              layout_column_wrap(
                width = 1/2,
                card(card_header("Top 5 Settori per Valore Economico (€)"), plotlyOutput("ov_plot_cpv_imp", height = "350px")),
                card(card_header("Top 5 Settori per Volume di Gare (n.)"), plotlyOutput("ov_plot_cpv_gare", height = "350px"))
              )
            )
  ),
  
  # SCHEDA 2: TERRITORIALE
  nav_panel("Ripartizione Territoriale",
            layout_sidebar(
              sidebar = sidebar(
                width = 250, title = "Filtri Geografici",
                selectInput("anno_reg", "Seleziona Anno:", choices = sort(unique(df_anac_reg$fil_val), decreasing = TRUE)),
                selectInput("ind_reg", "Seleziona Indicatore:",
                            choices = list(
                              "Dati Annuali" = c(
                                "Numero Gare" = "ind10",
                                "Aggiudicate" = "ind11",
                                "% Aggiudicazione" = "ind14",
                                "Importo Totale" = "ind15",
                                "Importo Medio per Gara" = "ind16",
                                "Durata Media Prevista (gg)" = "ind18",
                                "Giorni Scadenza-Esito" = "ind19",
                                "Finestra Partecipazione (gg)" = "ind20"
                              ),
                              "Dati Mensili" = c(
                                "Gare (Mese)" = "ind21",
                                "Aggiudicate (Mese)" = "ind22",
                                "% Aggiudicazione (Mese)" = "ind25",
                                "Importo (Mese)" = "ind26",
                                "Importo Medio per Gara (Mese)" = "ind27",
                                "Durata Media Prevista (Mese)" = "ind29",
                                "Giorni Scadenza-Esito (Mese)" = "ind30",
                                "Finestra Partecipazione (Mese)" = "ind31"
                              )
                            )),
                conditionalPanel(
                  condition = "['ind21','ind22','ind25','ind26','ind27','ind29','ind30','ind31'].includes(input.ind_reg)",
                  selectInput("mese_reg", "Seleziona Mese:", choices = sprintf("%02d", 1:12))
                )
              ),
              layout_column_wrap(
                width = 1,
                card(
                  card_header("Confronto tra tutte le Regioni (anno selezionato)"),
                  DTOutput("tabella_reg_confronto")
                )
              ),
              layout_column_wrap(
                width = 1/2,
                card(card_header("Mappa Distribuzione"), leafletOutput("mappa_anac", height = "600px")),
                card(card_header("Classifica Regionale"), plotlyOutput("plot_regioni_bar", height = "600px"))
              )
            )
  ),
  
  # SCHEDA 3: CONFRONTO FORME GIURIDICHE (nuova)
  nav_panel("Confronto Forme Giuridiche",
            layout_sidebar(
              sidebar = sidebar(
                width = 280, title = "Filtri",
                selectInput("anno_fg", "Anno:", choices = sort(unique(df_fg_confronto$fil_val), decreasing = TRUE)),
                selectInput("metrica_fg", "Metrica da confrontare:",
                            choices = setNames(names(nomi_indicatori_fg), nomi_indicatori_fg))
              ),
              card(
                card_header("Confronto tra tutte le Forme Giuridiche (anno selezionato)"),
                DTOutput("tabella_fg_confronto")
              ),
              card(
                card_header(textOutput("titolo_fg_confronto")),
                plotlyOutput("plot_fg_confronto", height = "600px")
              )
            )
  ),
  
  # SCHEDA 4: DETTAGLIO MENSILE
  nav_panel("Analisi Dettaglio Mensile per PA",
            layout_sidebar(
              sidebar = sidebar(
                width = 300, title = "Parametri di Analisi",
                selectInput("anno_det", "1. Anno:", choices = sort(unique(df_anac_raw$fil_val), decreasing = TRUE)),
                selectInput("mese_det", "2. Mese:", choices = sprintf("%02d", 1:12)),
                selectInput("fg_det", "3. Forma Giuridica:",
                            choices = c("Tutte le Forme" = "TUTTE", lista_fg))
              ),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                value_box(title = "Gare nel Mese", value = textOutput("det_val1"), showcase = icon("list-ol"), theme = "info"),
                value_box(title = "Aggiudicate nel Mese", value = textOutput("det_val2"), showcase = icon("check-double"), theme = "success"),
                value_box(title = "Importo Totale Mese", value = textOutput("det_val3"), showcase = icon("euro-sign"), theme = "success")
              ),
              layout_column_wrap(
                width = 1/2,
                card(card_header("Performance Temporale (GG)"), plotlyOutput("plot_det_tempi")),
                card(card_header("Medie Economiche per Ente"), plotlyOutput("plot_det_medie"))
              )
            )
  ),
  
  # SCHEDA 5: SETTORE CPV
  nav_panel("Analisi per Settore (CPV)",
            layout_sidebar(
              sidebar = sidebar(
                width = 300, title = "Filtri Settore",
                selectInput("cpv_sel", "1. Scegli Settore (CPV):", choices = lista_cpv),
                selectInput("anno_cpv", "2. Anno:", choices = sort(unique(df_cpv_raw$fil_val), decreasing = TRUE)),
                selectInput("mese_cpv", "3. Mese:", choices = c("Tutti i mesi" = "TUTTI", sprintf("%02d", 1:12))),
                selectInput("fg_cpv", "4. Forma Giuridica:", choices = c("Tutte le PA" = "TUTTE", lista_fg_cpv))
              ),
              card(
                card_header("Confronto tra tutti i Settori (anno/mese/forma selezionati)"),
                DTOutput("tabella_cpv_confronto")
              ),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                value_box(title = "Gare nel Settore", value = textOutput("cpv_val1"), showcase = icon("shopping-cart"), theme = "indigo"),
                value_box(title = "Aggiudicate (%)", value = textOutput("cpv_val2"), showcase = icon("chart-pie"), theme = "indigo"),
                value_box(title = "Importo Totale Settore", value = textOutput("cpv_val3"), showcase = icon("euro-sign"), theme = "indigo")
              ),
              layout_column_wrap(
                width = 1/2,
                card(card_header("Performance Temporale Settore (GG)"), plotlyOutput("plot_cpv_tempi")),
                card(card_header("Rapporto Gare/PA nel Settore"), plotlyOutput("plot_cpv_pa"))
              )
            )
  )
)

# --- 3. LOGICA SERVER ---
server <- function(input, output, session) {
  
  # --- LOGICA OVERVIEW ---
  get_val_nat <- function(indicatore) {
    res <- df_anac_nat %>% filter(ind == indicatore, fil_val == input$anno_ov) %>% pull(val_nazionale)
    if (length(res) == 0) return(0) else return(res[1])
  }
  
  output$val_ind1 <- renderText({ format(get_val_nat("ind1"), big.mark = ".", decimal.mark = ",") })
  output$val_ind2 <- renderText({ paste0(format(get_val_nat("ind2"), big.mark = ".", decimal.mark = ",", nsmall = 2), "%") })
  output$val_ind3 <- renderText({ format(get_val_nat("ind3"), big.mark = ".", decimal.mark = ",") })
  output$val_ind4 <- renderText({ format(get_val_nat("ind4"), big.mark = ".", decimal.mark = ",") })
  output$val_ind9 <- renderText({ paste0(format(get_val_nat("ind9"), big.mark = ".", decimal.mark = ",", nsmall = 2), "%") })
  output$val_ind5 <- renderText({ paste0("€ ", format(get_val_nat("ind5"), big.mark = ".", decimal.mark = ",", nsmall = 2)) })
  output$val_ind6 <- renderText({ paste0(format(get_val_nat("ind6"), big.mark = ".", decimal.mark = ",", nsmall = 2), " gg") })
  output$val_ind7 <- renderText({ paste0(format(get_val_nat("ind7"), big.mark = ".", decimal.mark = ",", nsmall = 2), " gg") })
  output$val_ind8 <- renderText({ paste0(format(get_val_nat("ind8"), big.mark = ".", decimal.mark = ",", nsmall = 2), " gg") })
  
  output$ov_plot_cpv_imp <- renderPlotly({
    req(input$anno_ov)
    top <- df_cpv_raw %>% filter(ind == "ind7", fil == "fil_anno", fil_val == input$anno_ov) %>%
      group_by(cpv) %>% summarise(v = sum(ind_val, na.rm = TRUE)) %>% arrange(desc(v)) %>% head(5)
    p <- ggplot(top, aes(x = reorder(cpv, v), y = v)) + geom_col(fill = "#27ae60") + coord_flip() + theme_minimal() + labs(x = NULL, y = "Importo Totale (€)")
    ggplotly(p) %>% layout(margin = list(l = 150))
  })
  
  output$ov_plot_cpv_gare <- renderPlotly({
    req(input$anno_ov)
    top <- df_cpv_raw %>% filter(ind == "ind1", fil == "fil_anno", fil_val == input$anno_ov) %>%
      group_by(cpv) %>% summarise(v = sum(ind_val, na.rm = TRUE)) %>% arrange(desc(v)) %>% head(5)
    p <- ggplot(top, aes(x = reorder(cpv, v), y = v)) + geom_col(fill = "#2980b9") + coord_flip() + theme_minimal() + labs(x = NULL, y = "Numero totale Gare")
    ggplotly(p) %>% layout(margin = list(l = 150))
  })
  
  # --- LOGICA RIPARTIZIONE TERRITORIALE ---
  output$mappa_anac <- renderLeaflet({
    req_df <- df_anac_reg %>% filter(fil_val == input$anno_reg, ind == input$ind_reg)
    if (input$ind_reg %in% c("ind21", "ind22", "ind25", "ind26", "ind27", "ind29", "ind30", "ind31")) {
      req_df <- req_df %>% filter(sub_fil_val == input$mese_reg)
    }
    shiny::validate(shiny::need(nrow(req_df) > 0, "Dati non disponibili."))
    mappa_final <- regioni_shape %>% left_join(req_df, by = c("reg_code" = "codice_reg"))
    valori_validi <- mappa_final$ind_val[!is.na(mappa_final$ind_val)]
    shiny::validate(shiny::need(length(valori_validi) > 0, "Nessun valore disponibile per questa selezione."))
    pal <- colorNumeric(palette = "YlGnBu", domain = valori_validi, na.color = "#f0f0f0")
    etichetta_ind <- unname(nomi_indicatori_reg[input$ind_reg])
    
    # Pre-calcolo label e colori come vettori R "normali", fuori dalla
    # formula ~ di leaflet (mischiare colonne dei dati e input$... dentro
    # una formula leaflet e' fragile e fonte di bug difficili da isolare).
    testo_valore <- ifelse(
      is.na(mappa_final$ind_val), "Nessun dato",
      fmt_valore_indicatore(mappa_final$ind_val, input$ind_reg)
    )
    etichette_popup <- paste0(mappa_final$reg_name, ": ", testo_valore)
    colori_poligoni <- ifelse(is.na(mappa_final$ind_val), "#f0f0f0", pal(mappa_final$ind_val))
    
    leaflet(mappa_final) %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = 12.5, lat = 41.9, zoom = 5) %>%
      addPolygons(fillColor = colori_poligoni, weight = 1, color = "white", fillOpacity = 0.8,
                  label = etichette_popup) %>%
      addLegend(pal = pal, values = valori_validi, title = etichetta_ind, position = "bottomright", na.label = "Nessun dato")
  })
  
  output$plot_regioni_bar <- renderPlotly({
    res_reg <- df_anac_reg %>% filter(fil_val == input$anno_reg, ind == input$ind_reg)
    if (input$ind_reg %in% c("ind21", "ind22", "ind25", "ind26", "ind27", "ind29", "ind30", "ind31")) {
      res_reg <- res_reg %>% filter(sub_fil_val == input$mese_reg)
    }
    res_reg <- res_reg %>% arrange(desc(ind_val))
    etichetta_ind <- unname(nomi_indicatori_reg[input$ind_reg])
    p <- ggplot(res_reg, aes(x = reorder(regione_bdap, ind_val), y = ind_val)) +
      geom_col(fill = "#007bff") + coord_flip() + theme_minimal() + labs(x = NULL, y = etichetta_ind)
    ggplotly(p) %>% layout(margin = list(l = 150))
  })
  
  # --- TABELLA COMPARATIVA DI TUTTE LE REGIONI (nuova) ---
  # Mostra sempre il dettaglio ANNUALE completo (ind10,11,14,15,16,18-20)
  # per l'anno selezionato, indipendentemente dall'indicatore scelto
  # per la mappa: serve come confronto multi-metrica a colpo d'occhio.
  output$tabella_reg_confronto <- renderDT({
    req(input$anno_reg)
    tab <- df_anac_reg %>%
      filter(fil_val == input$anno_reg, is.na(sub_fil_val),
             ind %in% c("ind10", "ind11", "ind14", "ind15", "ind16", "ind18", "ind19", "ind20")) %>%
      select(regione_bdap, ind, ind_val) %>%
      pivot_wider(names_from = ind, values_from = ind_val) %>%
      arrange(desc(ind15))
    
    datatable(
      tab,
      rownames = FALSE,
      colnames = c(
        "Regione" = "regione_bdap", "N. Gare" = "ind10", "Aggiudicate (n)" = "ind11",
        "Aggiudicazione (%)" = "ind14", "Importo Totale (€)" = "ind15", "Importo Medio (€)" = "ind16",
        "Durata Prevista (gg)" = "ind18", "Scadenza-Esito (gg)" = "ind19", "Finestra Partecip. (gg)" = "ind20"
      ),
      filter = "top",
      options = list(pageLength = 21, order = list(list(4, "desc")))
    ) %>%
      formatRound(c("Importo Totale (€)", "Importo Medio (€)"), 0) %>%
      formatRound(c("Aggiudicazione (%)", "Durata Prevista (gg)", "Scadenza-Esito (gg)", "Finestra Partecip. (gg)"), 1)
  })
  
  # --- LOGICA CONFRONTO FORME GIURIDICHE (nuova) ---
  # --- TABELLA COMPARATIVA DI TUTTE LE FORME GIURIDICHE (nuova) ---
  output$tabella_fg_confronto <- renderDT({
    req(input$anno_fg)
    tab <- df_fg_confronto %>%
      filter(fil_val == input$anno_fg) %>%
      select(sub_fil_val, ind32, ind33, ind36, ind37, ind38, ind40, ind41, ind42) %>%
      arrange(desc(ind37))
    
    datatable(
      tab,
      rownames = FALSE,
      colnames = c(
        "Forma Giuridica" = "sub_fil_val", "N. Gare" = "ind32", "Aggiudicate (n)" = "ind33",
        "Aggiudicazione (%)" = "ind36", "Importo Totale (€)" = "ind37", "Importo Medio (€)" = "ind38",
        "Durata Prevista (gg)" = "ind40", "Scadenza-Esito (gg)" = "ind41", "Finestra Partecip. (gg)" = "ind42"
      ),
      filter = "top",
      options = list(pageLength = 15, order = list(list(4, "desc")))
    ) %>%
      formatRound(c("Importo Totale (€)", "Importo Medio (€)"), 0) %>%
      formatRound(c("Aggiudicazione (%)", "Durata Prevista (gg)", "Scadenza-Esito (gg)", "Finestra Partecip. (gg)"), 1)
  })
  
  output$titolo_fg_confronto <- renderText({
    paste0("Confronto tra Forme Giuridiche — ", unname(nomi_indicatori_fg[input$metrica_fg]), " (Anno ", input$anno_fg, ")")
  })
  
  output$plot_fg_confronto <- renderPlotly({
    metrica <- input$metrica_fg
    d <- df_fg_confronto %>%
      filter(fil_val == input$anno_fg) %>%
      select(sub_fil_val, valore = all_of(metrica)) %>%
      filter(!is.na(valore)) %>%
      arrange(desc(valore))
    
    p <- ggplot(d, aes(x = reorder(sub_fil_val, valore), y = valore)) +
      geom_col(fill = "#8e44ad") + coord_flip() + theme_minimal() +
      labs(x = NULL, y = unname(nomi_indicatori_fg[metrica]))
    ggplotly(p) %>% layout(margin = list(l = 180))
  })
  
  # --- LOGICA ANALISI DETTAGLIO MENSILE ---
  data_mensile <- reactive({
    req(input$anno_det, input$mese_det, input$fg_det)
    is_fg <- input$fg_det != "TUTTE"
    offset <- if (is_fg) 22 else 0
    d <- df_anac_raw %>% filter(fil_val == input$anno_det, sub_fil_val == input$mese_det)
    if (is_fg) { d <- d %>% filter(subsub_fil_val == input$fg_det) }
    return(list(df = d, offset = offset))
  })
  
  get_det_ind <- function(base_id) {
    info <- data_mensile()
    target_id <- paste0("ind", base_id + info$offset)
    val <- info$df %>% filter(ind == target_id) %>% pull(ind_val) %>% sum(na.rm = TRUE)
    return(val)
  }
  
  output$det_val1 <- renderText({ format(get_det_ind(21), big.mark = ".", decimal.mark = ",") })
  output$det_val2 <- renderText({ format(get_det_ind(22), big.mark = ".", decimal.mark = ",") })
  output$det_val3 <- renderText({ paste0("€ ", format(get_det_ind(26), big.mark = ".", decimal.mark = ",")) })
  
  output$plot_det_tempi <- renderPlotly({
    info <- data_mensile()
    labels <- c("Durata", "Esito", "Finestra")
    vals <- c(get_det_ind(29), get_det_ind(30), get_det_ind(31))
    p <- ggplot(data.frame(labels, vals), aes(x = labels, y = vals, fill = labels)) + geom_col() + theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$plot_det_medie <- renderPlotly({
    info <- data_mensile()
    labels <- c("Media", "Ponderata")
    vals <- c(get_det_ind(27), get_det_ind(28))
    p <- ggplot(data.frame(labels, vals), aes(x = labels, y = vals)) + geom_col(fill = "#27ae60") + theme_minimal()
    ggplotly(p)
  })
  
  # --- LOGICA ANALISI CPV ---
  dati_cpv <- reactive({
    req(input$cpv_sel, input$anno_cpv, input$mese_cpv, input$fg_cpv)
    is_tutti_mesi <- input$mese_cpv == "TUTTI"; is_tutte_forme <- input$fg_cpv == "TUTTE"
    base_id <- case_when(is_tutti_mesi & is_tutte_forme ~ 1, !is_tutti_mesi & is_tutte_forme ~ 13, is_tutti_mesi & !is_tutte_forme ~ 25, TRUE ~ 37)
    d <- df_cpv_raw %>% filter(cpv == input$cpv_sel, fil_val == input$anno_cpv)
    if (!is_tutti_mesi) d <- d %>% filter(sub_fil_val == input$mese_cpv)
    if (!is_tutte_forme) { col_fg <- if (is_tutti_mesi) "sub_fil_val" else "subsub_fil_val"; d <- d %>% filter(!!sym(col_fg) == input$fg_cpv) }
    indices <- paste0("ind", base_id:(base_id + 11))
    res <- setNames(lapply(indices, function(i) sum(d$ind_val[d$ind == i], na.rm = TRUE)), c("gare", "agg_n", "agg_perc", "pa_n", "pa_perc", "media_gare_pa", "imp_tot", "imp_med", "imp_pond", "durata", "esito", "finestra"))
    return(list(res = res, base_id = base_id))
  })
  
  output$cpv_val1 <- renderText({ format(dati_cpv()$res$gare, big.mark = ".", decimal.mark = ",") })
  output$cpv_val2 <- renderText({ paste0(format(dati_cpv()$res$agg_perc, nsmall = 2), "%") })
  output$cpv_val3 <- renderText({ paste0("€ ", format(dati_cpv()$res$imp_tot, big.mark = ".", decimal.mark = ",")) })
  
  output$plot_cpv_tempi <- renderPlotly({
    d <- dati_cpv()$res
    df_p <- data.frame(Label = c("Durata", "Esito", "Finestra"), Valore = c(d$durata, d$esito, d$finestra))
    p <- ggplot(df_p, aes(x = Label, y = Valore, fill = Label)) + geom_col() + scale_fill_brewer(palette = "Set1") + theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$plot_cpv_pa <- renderPlotly({
    d <- dati_cpv()$res
    df_p <- data.frame(Label = c("Media Gare/PA", "PA"), Valore = c(d$media_gare_pa, d$pa_n))
    p <- ggplot(df_p, aes(x = Label, y = Valore)) + geom_col(fill = "#48dbfb") + theme_minimal()
    ggplotly(p)
  })
  
  # --- TABELLA COMPARATIVA DI TUTTI I SETTORI CPV (nuova) ---
  # Usa lo stesso schema "a tier" (base_id) di dati_cpv(), ma senza
  # filtrare per cpv_sel: mostra TUTTI i settori in una sola tabella,
  # rispettando solo anno/mese/forma giuridica già selezionati.
  output$tabella_cpv_confronto <- renderDT({
    req(input$anno_cpv, input$mese_cpv, input$fg_cpv)
    is_tutti_mesi <- input$mese_cpv == "TUTTI"
    is_tutte_forme <- input$fg_cpv == "TUTTE"
    base_id <- case_when(is_tutti_mesi & is_tutte_forme ~ 1, !is_tutti_mesi & is_tutte_forme ~ 13, is_tutti_mesi & !is_tutte_forme ~ 25, TRUE ~ 37)
    
    d <- df_cpv_raw %>% filter(fil_val == input$anno_cpv)
    if (!is_tutti_mesi) d <- d %>% filter(sub_fil_val == input$mese_cpv)
    if (!is_tutte_forme) {
      col_fg <- if (is_tutti_mesi) "sub_fil_val" else "subsub_fil_val"
      d <- d %>% filter(!!sym(col_fg) == input$fg_cpv)
    }
    
    idx <- c(gare = base_id, agg_n = base_id + 1, agg_perc = base_id + 2, pa_n = base_id + 3, imp_tot = base_id + 6)
    mappa_idx <- setNames(paste0("ind", idx), names(idx))
    
    tab <- d %>%
      filter(ind %in% mappa_idx) %>%
      mutate(metrica = names(mappa_idx)[match(ind, mappa_idx)]) %>%
      group_by(cpv, metrica) %>%
      summarise(valore = sum(ind_val, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = metrica, values_from = valore) %>%
      arrange(desc(imp_tot))
    
    datatable(
      tab,
      rownames = FALSE,
      colnames = c(
        "Settore (CPV)" = "cpv", "N. Gare" = "gare", "Aggiudicate (n)" = "agg_n",
        "Aggiudicate (%)" = "agg_perc", "N. PA acquirenti" = "pa_n", "Importo Totale (€)" = "imp_tot"
      ),
      filter = "top",
      options = list(pageLength = 15, order = list(list(5, "desc")))
    ) %>%
      formatRound(c("Importo Totale (€)"), 0) %>%
      formatRound(c("Aggiudicate (%)"), 1)
  })
}

shinyApp(ui, server)
