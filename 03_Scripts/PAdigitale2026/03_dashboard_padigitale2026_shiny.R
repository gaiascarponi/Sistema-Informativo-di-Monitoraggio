# ============================================================ #
# Script: 06_dashboard_padigitale2026_shiny.R
# Fonte: PA digitale 2026 - Open data
#
# Obiettivo:
#   Dashboard Shiny esplorativa sui dati PA digitale 2026
#   raccordati alla master list.
# ============================================================ #

rm(list = ls())

# 1) PACCHETTI ---------------------------------------------------------------

library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(plotly)
library(DT)
library(htmltools)
library(leaflet)
library(googledrive)

# 2) PARAMETRI ---------------------------------------------------------------

source("03_Scripts/00_config.R")
source("03_Scripts/00_drive_helpers.R")
source(file.path(DIR_SCRIPTS, "00_spatial_helpers.R"))

googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")

# parametro per pulire la cartella temp alla fine del run
# delete_local_temp <- FALSE

clear_dashboard_cache <- FALSE

if (clear_dashboard_cache) {
  unlink(file.path(DIR_TEMP, "PADigitale2026", "Dashboard"), recursive = TRUE)
}

RUN_ID_IMPORT <- "20260608_183000"  # da copiare dall'output dello script 01
RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
message("RUN_ID: ", RUN_ID)


DIR_PAD26_DASH_LOCAL <- file.path(DIR_TEMP, "PADigitale2026", "Dashboard")
dir.create(DIR_PAD26_DASH_LOCAL, recursive = TRUE, showWarnings = FALSE)

DRIVE_PAD26_OUTPUT <- file.path(DRIVE_DIR_OUTPUT, "PADigitale2026")
DRIVE_PAD26_LOGS <- DRIVE_DIR_LOGS

file_dashboard <- file.path(
  DIR_PAD26_DASH_LOCAL,
  "dashboard_padigitale2026.rds"
)

file_log_match <- file.path(
  DIR_PAD26_DASH_LOCAL,
  "log_match_padigitale2026_lista.csv"
)

drive_download_from_path(
  drive_file_rel = file.path(DRIVE_PAD26_OUTPUT, "dashboard_padigitale2026.rds"),
  local_path = file_dashboard
)

drive_download_from_path(
  drive_file_rel = file.path(DRIVE_PAD26_LOGS, "log_match_padigitale2026_lista.csv"),
  local_path = file_log_match
)

# 3) IMPORT DATI -------------------------------------------------------------

dashboard_pad26 <- readRDS(file_dashboard)
log_match_pad26 <- readr::read_csv(file_log_match, show_col_types = FALSE)

nuts2_it <- scarica_nuts2_italia(year = 2024, resolution = "10")

# 4) FUNZIONI ---------------------------------------------------------------

safe_choices <- function(x) {
  x <- sort(unique(as.character(x)))
  x <- x[!is.na(x) & x != ""]
  x
}

format_num <- function(x) {
  format(round(x, 0), big.mark = ".", decimal.mark = ",")
}

format_euro <- function(x) {
  paste0("€ ", format(round(x, 0), big.mark = ".", decimal.mark = ","))
}

filter_multi <- function(data, var, values) {
  if (is.null(values) || length(values) == 0) {
    data
  } else {
    data %>% filter(.data[[var]] %in% values)
  }
}

plot_empty <- function(msg = "Nessun dato disponibile con i filtri selezionati.") {
  plot_ly() %>%
    layout(
      title = msg,
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

# 5) UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { font-family: Arial, sans-serif; }
      .small-note {
        color: #555;
        font-size: 12px;
        line-height: 1.35;
      }
      .metric-box {
        border: 1px solid #ddd;
        border-radius: 6px;
        padding: 12px;
        margin-bottom: 10px;
        background-color: #fafafa;
      }
      .metric-number {
        font-size: 24px;
        font-weight: bold;
      }
      .metric-label {
        color: #555;
        font-size: 12px;
      }
    "))
  ),
  
  titlePanel("PA digitale 2026 - Dashboard esplorativa"),
  
  fluidRow(
    column(
      width = 12,
      tags$p(
        class = "small-note",
        "Dashboard costruita a partire dagli open data PA digitale 2026, raccordati ove possibile alla master list S13+/MPA/BDAP. ",
        "Il valore aggiunto rispetto alla dashboard pubblica è la lettura per macro-gruppo PA, tipologia istituzionale, perimetro S13/MPA e territorio."
      )
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Filtri"),
      
      checkboxGroupInput(
        inputId = "avviso",
        label = "Avviso / misura",
        choices = safe_choices(dashboard_pad26$avviso),
        selected = safe_choices(dashboard_pad26$avviso)
      ),
      
      checkboxGroupInput(
        inputId = "macro_gruppo_pa",
        label = "Macro-gruppo PA",
        choices = safe_choices(dashboard_pad26$macro_gruppo_pa),
        selected = safe_choices(dashboard_pad26$macro_gruppo_pa)
      ),
      
      checkboxGroupInput(
        inputId = "tipologia_ente",
        label = "Tipologia ente PA digitale 2026",
        choices = safe_choices(dashboard_pad26$tipologia_ente),
        selected = safe_choices(dashboard_pad26$tipologia_ente)
      ),
      
      checkboxGroupInput(
        inputId = "match_lista",
        label = "Raccordo con master list",
        choices = c("Raccordato" = "1", "Non raccordato" = "0"),
        selected = c("1", "0")
      ),
      
      selectInput(
        inputId = "regione",
        label = "Regione",
        choices = safe_choices(dashboard_pad26$regione),
        selected = safe_choices(dashboard_pad26$regione),
        multiple = TRUE,
        selectize = TRUE
      ),
      
      selectizeInput(
        inputId = "ente",
        label = "Ente",
        choices = NULL,
        selected = character(0),
        multiple = TRUE,
        options = list(
          placeholder = "Cerca uno o più enti...",
          maxOptions = 100
        )
      ),
      
      # selectInput(
      #   inputId = "ente",
      #   label = "Ente",
      #   choices = safe_choices(dashboard_pad26$ente_key),
      #   selected = character(0),
      #   multiple = TRUE,
      #   selectize = TRUE
      # ),
      
      selectInput(
        inputId = "indicatore_mappa",
        label = "Indicatore mappa",
        choices = c(
          "Importo finanziato" = "importo_finanziamento",
          "Numero candidature" = "n_candidature",
          "Numero enti" = "n_enti"
        ),
        selected = "importo_finanziamento"
      ),
      
      tags$p(
        class = "small-note",
        "Nota: se non selezioni enti specifici, la dashboard mostra tutti gli enti inclusi nei filtri."
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        
        tabPanel(
          "Sintesi",
          br(),
          fluidRow(
            column(width = 4, uiOutput("box_candidature")),
            column(width = 4, uiOutput("box_enti")),
            column(width = 4, uiOutput("box_importo"))
          ),
          plotlyOutput("plot_misure", height = "480px"),
          br(),
          plotlyOutput("plot_macro_pa", height = "480px")
        ),
        
        tabPanel(
          "Mappa",
          br(),
          h4("Distribuzione regionale"),
          leafletOutput("mappa_regionale", height = "650px"),
          br(),
          DTOutput("tabella_regionale")
        ),
        
        tabPanel(
          "Enti",
          br(),
          plotlyOutput("plot_top_enti", height = "520px"),
          br(),
          DTOutput("tabella_enti")
        ),
        
        tabPanel(
          "Raccordo",
          br(),
          plotlyOutput("plot_match", height = "480px"),
          br(),
          DTOutput("tabella_match")
        ),
        
        tabPanel(
          "Dati",
          br(),
          DTOutput("tabella_dati")
        )
      )
    )
  )
)

# 6) SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  
  updateSelectizeInput(
    session = session,
    inputId = "ente",
    choices = safe_choices(dashboard_pad26$ente_key),
    selected = character(0),
    server = TRUE
  )
  
  dati_filtrati <- reactive({
    
    data <- dashboard_pad26
    
    data <- filter_multi(data, "avviso", input$avviso)
    data <- filter_multi(data, "macro_gruppo_pa", input$macro_gruppo_pa)
    data <- filter_multi(data, "tipologia_ente", input$tipologia_ente)
    data <- filter_multi(data, "regione", input$regione)
    
    if (!is.null(input$match_lista) && length(input$match_lista) > 0) {
      data <- data %>%
        filter(as.character(match_lista) %in% input$match_lista)
    }
    
    if (!is.null(input$ente) && length(input$ente) > 0) {
      data <- data %>% filter(ente_key %in% input$ente)
    }
    
    data
  })
  
  # Metriche ----------------------------------------------------------------
  
  output$box_candidature <- renderUI({
    totale <- nrow(dati_filtrati())
    
    div(
      class = "metric-box",
      div(class = "metric-number", format_num(totale)),
      div(class = "metric-label", "Candidature finanziate")
    )
  })
  
  output$box_enti <- renderUI({
    totale <- dati_filtrati() %>%
      summarise(n = n_distinct(ente_key, na.rm = TRUE)) %>%
      pull(n)
    
    div(
      class = "metric-box",
      div(class = "metric-number", format_num(totale)),
      div(class = "metric-label", "Enti")
    )
  })
  
  output$box_importo <- renderUI({
    totale <- dati_filtrati() %>%
      summarise(importo = sum(importo_finanziamento, na.rm = TRUE)) %>%
      pull(importo)
    
    div(
      class = "metric-box",
      div(class = "metric-number", format_euro(totale)),
      div(class = "metric-label", "Importo finanziato")
    )
  })
  
  # Grafico misure -----------------------------------------------------------
  
  output$plot_misure <- renderPlotly({
    
    data <- dati_filtrati() %>%
      group_by(avviso) %>%
      summarise(
        n_candidature = n(),
        n_enti = n_distinct(ente_key, na.rm = TRUE),
        importo_finanziato = sum(importo_finanziamento, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(importo_finanziato))
    
    if (nrow(data) == 0) return(plot_empty())
    
    plot_ly(
      data,
      x = ~importo_finanziato,
      y = ~reorder(avviso, importo_finanziato),
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      text = ~paste0(
        "Avviso: ", avviso,
        "<br>Importo: ", format_euro(importo_finanziato),
        "<br>Candidature: ", format_num(n_candidature),
        "<br>Enti: ", format_num(n_enti)
      )
    ) %>%
      layout(
        title = "Importo finanziato per misura/avviso",
        xaxis = list(title = "Importo finanziato"),
        yaxis = list(title = ""),
        margin = list(l = 260)
      )
  })
  
  # Grafico macro PA ---------------------------------------------------------
  
  output$plot_macro_pa <- renderPlotly({
    
    data <- dati_filtrati() %>%
      group_by(macro_gruppo_pa) %>%
      summarise(
        n_candidature = n(),
        n_enti = n_distinct(ente_key, na.rm = TRUE),
        importo_finanziato = sum(importo_finanziamento, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(importo_finanziato))
    
    if (nrow(data) == 0) return(plot_empty())
    
    plot_ly(
      data,
      x = ~importo_finanziato,
      y = ~reorder(macro_gruppo_pa, importo_finanziato),
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      text = ~paste0(
        "Macro-gruppo PA: ", macro_gruppo_pa,
        "<br>Importo: ", format_euro(importo_finanziato),
        "<br>Candidature: ", format_num(n_candidature),
        "<br>Enti: ", format_num(n_enti)
      )
    ) %>%
      layout(
        title = "Importo finanziato per macro-gruppo PA",
        xaxis = list(title = "Importo finanziato"),
        yaxis = list(title = ""),
        margin = list(l = 220)
      )
  })
  
  # Mappa --------------------------------------------------------------------
  
  dati_regione <- reactive({
    
    dati_filtrati() %>%
      filter(!is.na(cod_regione), cod_regione != "") %>%
      mutate(
        cod_regione = stringr::str_pad(as.character(cod_regione), 2, pad = "0")
      ) %>%
      group_by(cod_regione, regione) %>%
      summarise(
        n_candidature = n(),
        n_enti = n_distinct(ente_key, na.rm = TRUE),
        importo_finanziamento = sum(importo_finanziamento, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  mappa_regionale_sf <- reactive({
    
    dati_nuts <- dati_regione() %>%
      rename(codice_regione = cod_regione) %>%
      left_join(
        get_raccordo_regioni_nuts(),
        by = "codice_regione"
      )
    
    nuts2_it %>%
      left_join(dati_nuts, by = "NUTS_ID")
  })
  
  output$mappa_regionale <- renderLeaflet({
    
    dati_mappa <- mappa_regionale_sf()
    
    indicatore <- input$indicatore_mappa
    
    if (is.null(indicatore) || !(indicatore %in% names(dati_mappa))) {
      indicatore <- "importo_finanziamento"
    }
    
    dati_mappa <- dati_mappa %>%
      mutate(
        valore_mappa = as.numeric(.data[[indicatore]]),
        valore_label = case_when(
          indicatore == "importo_finanziamento" & !is.na(valore_mappa) ~ format_euro(valore_mappa),
          !is.na(valore_mappa) ~ format_num(valore_mappa),
          TRUE ~ "Nessun dato"
        )
      )
    
    valori_validi <- dati_mappa$valore_mappa[!is.na(dati_mappa$valore_mappa)]
    
    if (length(valori_validi) == 0) {
      pal <- leaflet::colorNumeric("YlOrRd", domain = c(0, 1), na.color = "#eeeeee")
    } else {
      pal <- leaflet::colorNumeric("YlOrRd", domain = valori_validi, na.color = "#eeeeee")
    }
    
    label_indicatore <- case_when(
      indicatore == "importo_finanziamento" ~ "Importo finanziato",
      indicatore == "n_candidature" ~ "Numero candidature",
      indicatore == "n_enti" ~ "Numero enti",
      TRUE ~ indicatore
    )
    
    leaflet(dati_mappa) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(valore_mappa),
        fillOpacity = 0.75,
        weight = 1,
        color = "#444444",
        popup = ~paste0(
          "<b>", NUTS_NAME, "</b>",
          "<br>", label_indicatore, ": ", valore_label,
          "<br>Candidature: ", ifelse(is.na(n_candidature), "Nessun dato", format_num(n_candidature)),
          "<br>Enti: ", ifelse(is.na(n_enti), "Nessun dato", format_num(n_enti)),
          "<br>Importo: ", ifelse(is.na(importo_finanziamento), "Nessun dato", format_euro(importo_finanziamento))
        )
      ) %>%
      addLegend(
        pal = pal,
        values = valori_validi,
        opacity = 0.75,
        title = label_indicatore,
        position = "bottomright"
      )
  })
  
  output$tabella_regionale <- renderDT({
    
    dati_regione() %>%
      arrange(regione) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 20, scrollX = TRUE)
      )
  })
  
  # Enti ---------------------------------------------------------------------
  
  output$plot_top_enti <- renderPlotly({
    
    data <- dati_filtrati() %>%
      group_by(ente_key, tipologia_ente, regione, macro_gruppo_pa) %>%
      summarise(
        n_candidature = n(),
        importo_finanziato = sum(importo_finanziamento, na.rm = TRUE),
        n_misure = n_distinct(avviso, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(importo_finanziato)) %>%
      slice_head(n = 25)
    
    if (nrow(data) == 0) return(plot_empty())
    
    plot_ly(
      data,
      x = ~importo_finanziato,
      y = ~reorder(ente_key, importo_finanziato),
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      text = ~paste0(
        "Ente: ", ente_key,
        "<br>Regione: ", regione,
        "<br>Macro-gruppo PA: ", macro_gruppo_pa,
        "<br>Importo: ", format_euro(importo_finanziato),
        "<br>Candidature: ", format_num(n_candidature),
        "<br>Misure: ", format_num(n_misure)
      )
    ) %>%
      layout(
        title = "Top enti per importo finanziato",
        xaxis = list(title = "Importo finanziato"),
        yaxis = list(title = ""),
        margin = list(l = 280)
      )
  })
  
  output$tabella_enti <- renderDT({
    
    dati_filtrati() %>%
      group_by(ente_key, tipologia_ente, regione, provincia, comune, macro_gruppo_pa, match_lista, tipo_match) %>%
      summarise(
        n_candidature = n(),
        n_misure = n_distinct(avviso, na.rm = TRUE),
        importo_finanziato = sum(importo_finanziamento, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(importo_finanziato)) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 20, scrollX = TRUE)
      )
  })
  
  # Raccordo -----------------------------------------------------------------
  
  output$plot_match <- renderPlotly({
    
    data <- log_match_pad26 %>%
      arrange(quota_match_pct)
    
    plot_ly(
      data,
      x = ~quota_match_pct,
      y = ~reorder(dataset_id, quota_match_pct),
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      text = ~paste0(
        "Dataset: ", dataset_id,
        "<br>Quota match: ", quota_match_pct, "%",
        "<br>Candidature: ", n_candidature,
        "<br>Candidature matchate: ", n_candidature_match,
        "<br>Enti: ", n_enti_pad26,
        "<br>Enti matchati: ", n_enti_match,
        "<br>Importo matchato: ", quota_importo_match_pct, "%"
      )
    ) %>%
      layout(
        title = "Copertura del raccordo PA digitale 2026-master list",
        xaxis = list(title = "% candidature raccordate"),
        yaxis = list(title = ""),
        margin = list(l = 220)
      )
  })
  
  output$tabella_match <- renderDT({
    
    log_match_pad26 %>%
      arrange(desc(quota_match_pct)) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE)
      )
  })
  
  # Dati ---------------------------------------------------------------------
  
  output$tabella_dati <- renderDT({
    
    dati_filtrati() %>%
      arrange(regione, ente_key, avviso) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 20, scrollX = TRUE)
      )
  })
}

# 7) RUN APP -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
