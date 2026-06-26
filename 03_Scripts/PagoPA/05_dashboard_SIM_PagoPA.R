library(shiny)
library(bslib)
library(jsonlite)
library(dplyr)
library(plotly)
library(tidyr)
library(leaflet)
library(sf)

# --- CONFIGURAZIONI E MAPPATURE ---
geojson_url <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_regions.geojson"
regioni_shape <- read_sf(geojson_url) %>% 
  mutate(reg_code = sprintf("%02d", as.numeric(reg_istat_code_num)))

mappatura_nomi <- readRDS("07_Temp/fil_reg.rds") %>% 
  select(codice_reg, reg) 

ind_pagoPA_categorie <- c(
  "ind8" = "ACI", "ind9" = "Comuni", "ind10" = "Consorzi universitari",
  "ind11" = "Enti comunali", "ind12" = "Enti provinciali", "ind13" = "Enti regionali",
  "ind14" = "Ordini e collegi", "ind15" = "Province", "ind16" = "PA Centrali",
  "ind17" = "Regioni", "ind18" = "Ricerca", "ind19" = "Salute centrale",
  "ind20" = "Salute locale", "ind21" = "Salute regionale", "ind22" = "Salute servizi",
  "ind23" = "Scuola", "ind24" = "Università", "ind25" = "Altri enti territoriali",
  "ind26" = "Utility"
)

ind_fasce_nomi <- c(
  "ind31" = "ACI", "ind32" = "Comuni", "ind33" = "Consorzi universitari",
  "ind34" = "Enti comunali", "ind35" = "Enti provinciali", "ind36" = "Enti regionali",
  "ind37" = "Ordini e collegi", "ind38" = "Province", "ind39" = "PA Centrali",
  "ind40" = "Regioni", "ind41" = "Ricerca", "ind42" = "Salute centrale",
  "ind43" = "Salute locale", "ind44" = "Salute regionale", "ind45" = "Salute servizi",
  "ind46" = "Scuola", "ind47" = "Università", "ind48" = "Altri enti territoriali",
  "ind49" = "Utility", "ind50" = "Enti donazioni"
)

# 1. CARICAMENTO E PULIZIA DATI
data_path <- "07_Temp/INDICATORS_PAGOPA.json" 
df <- fromJSON(data_path) %>%
  filter(fil_val != "Complessivo") %>%
  mutate(ind_val = as.numeric(ind_val))

# Calcolo periodo di riferimento per i totali storici
anni_presenti <- as.numeric(df$fil_val[df$fil == "fil_anno"])
label_periodo <- paste0(" (", min(anni_presenti, na.rm=T), "-", max(anni_presenti, na.rm=T), ")")

ind_regionali_map <- c(
  "ind1" = "Enti IO (Comuni)", "ind2" = "Servizi IO (Comuni)", "ind3" = "Enti IO (Istruzione)",
  "ind4" = "Servizi IO (Istruzione)", "ind5" = "Comuni attivi su SEND (n.)", "ind6" = "Comuni attivi su SEND (%)"
)

# 3. INTERFACCIA UTENTE (UI)
ui <- page_navbar(
  title = "Monitoraggio PA Digitale 2026",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel("Overview Nazionale",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtro Overview",
                selectInput("anno_ov", "Seleziona Anno:", choices = unique(df$fil_val[df$fil == "fil_anno"]))
              ),
              # PRIMA RIGA: TOTALI STORICI (Tutti gli anni)
              layout_column_wrap(
                width = 1/3,
                value_box(title = paste0("Messaggi IO Totali", label_periodo), value = textOutput("val_io_tot"), showcase = icon("database"), theme = "secondary"),
                value_box(title = paste0("Notifiche SEND Totali", label_periodo), value = textOutput("val_send_tot_storico"), showcase = icon("database"), theme = "secondary"),
                value_box(title = paste0("Transazioni pagoPA Totali", label_periodo), value = textOutput("val_pago_tot_storico"), showcase = icon("database"), theme = "secondary")
              ),
              # SECONDA RIGA: TOTALI FILTRATI (Per anno)
              layout_column_wrap(
                width = 1/3,
                value_box(title = textOutput("titolo_io_anno"), value = textOutput("val_messaggi"), showcase = icon("envelope"), theme = "purple"),
                value_box(title = textOutput("titolo_pago_anno"), value = textOutput("val_transazioni"), showcase = icon("euro-sign"), theme = "success"),
                value_box(title = textOutput("titolo_send_anno"), value = textOutput("val_send"), showcase = icon("paper-plane"), theme = "orange")
              ),
              layout_column_wrap(
                width = 1/2,
                card(card_header("Top 5 Categorie pagoPA"), plotlyOutput("ov_plot_categorie", height = "300px")),
                card(card_header("Trend Mensile Messaggi IO"), plotlyOutput("plot_trend_messaggi", height = "300px"))
              )
            )
  ),
  
  nav_panel("Distribuzione Regionale IO",
            layout_sidebar(
              sidebar = sidebar(
                title = "Parametri Regionali",
                selectInput("sel_ind_reg", "Scegli Indicatore:", choices = setNames(names(ind_regionali_map), ind_regionali_map))
              ),
              layout_column_wrap(
                width = 1/2,
                card(card_header("Mappa Geografica"), leafletOutput("mappa_regioni", height = "600px")),
                card(card_header("Classifica Regioni"), plotlyOutput("plot_regionale_bar", height = "600px"))
              )
            )
  ),

  nav_panel("Analisi pagoPA (Enti & Fasce)",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtri pagoPA",
                selectInput("anno_pago", "1. Seleziona Anno:", choices = unique(df$fil_val[df$fil == "fil_anno"])),
                selectInput("mese_pago", "2. Seleziona Mese (per Enti):", choices = sprintf("%02d", 1:12)),
                hr(),
                selectInput("cat_fasce", "3. Categoria per Fasce d'Importo:", choices = setNames(names(ind_fasce_nomi), ind_fasce_nomi))
              ),
              layout_column_wrap(
                width = 1,
                card(
                  card_header("A. Transazioni per Categoria di Ente (Mensile)"),
                  plotlyOutput("plot_pagoPA_categorie", height = "350px")
                ),
                card(
                  card_header(textOutput("titolo_fasce")),
                  plotlyOutput("plot_pagoPA_fasce", height = "350px")
                )
              )
            )
  ),

  nav_panel("Messaggi IO",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtri Messaggi IO",
                selectInput("anno_io", "Anno:", choices = unique(df$fil_val[df$fil == "fil_anno"])),
                selectInput("mese_io", "Mese:", choices = sprintf("%02d", 1:12))
              ),
              value_box(title = "Messaggi nel mese selezionato", value = textOutput("val_io_mese"), showcase = icon("comment-dots"), theme = "purple"),
              card(card_header("Andamento messaggi nell'anno"), plotlyOutput("plot_io_anno"))
            )
  ),
  
  nav_panel("Notifiche SEND",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtri SEND",
                selectInput("anno_send", "Anno:", choices = unique(df$fil_val[df$fil == "fil_anno"])),
                selectInput("mese_send", "Mese:", choices = sprintf("%02d", 1:12))
              ),
              layout_column_wrap(
                width = 1/3,
                value_box(title = "Analogiche", value = textOutput("val_send_an"), theme = "secondary"),
                value_box(title = "Digitali", value = textOutput("val_send_dig"), theme = "info"),
                value_box(title = "Totale", value = textOutput("val_send_tot"), theme = "orange")
              ),
              card(card_header("Ripartizione Mensile"), plotlyOutput("plot_send_mese", height = "250px")),
              card(card_header("Distribuzione per Ambito (Dato Complessivo)"), plotlyOutput("plot_send_ambito", height = "350px"))
            )
  )
)

# 4. LOGICA SERVER
server <- function(input, output, session) {
  
  # --- OVERVIEW LOGICA TITOLI ---
  output$titolo_io_anno <- renderText({ paste0("Messaggi IO (", input$anno_ov, ")") })
  output$titolo_pago_anno <- renderText({ paste0("Transazioni pagoPA (", input$anno_ov, ")") })
  output$titolo_send_anno <- renderText({ paste0("Notifiche SEND (", input$anno_ov, ")") })

  # --- OVERVIEW KPI STORICI (TUTTI GLI ANNI) ---
  output$val_io_tot <- renderText({ format(sum(df$ind_val[df$ind == "ind7"], na.rm=T), big.mark=".") })
  output$val_send_tot_storico <- renderText({ format(sum(df$ind_val[df$ind == "ind29"], na.rm=T), big.mark=".") })
  output$val_pago_tot_storico <- renderText({ format(sum(df$ind_val[df$ind %in% paste0("ind", 8:26)], na.rm=T), big.mark=".") })

  # --- OVERVIEW KPI FILTRATI (ANNO) ---
  output$val_messaggi <- renderText({ format(sum(df$ind_val[df$ind == "ind7" & df$fil_val == input$anno_ov], na.rm=T), big.mark=".") })
  output$val_transazioni <- renderText({ format(sum(df$ind_val[df$ind %in% paste0("ind", 8:26) & df$fil_val == input$anno_ov], na.rm=T), big.mark=".") })
  output$val_send <- renderText({ format(sum(df$ind_val[df$ind == "ind29" & df$fil_val == input$anno_ov], na.rm=T), big.mark=".") })
  
  output$ov_plot_categorie <- renderPlotly({
    p_df <- df %>% filter(ind %in% paste0("ind", 8:26), fil_val == input$anno_ov) %>% group_by(ind) %>% summarise(tot=sum(ind_val, na.rm=T)) %>% mutate(nome=ind_pagoPA_categorie[ind]) %>% arrange(desc(tot)) %>% head(5)
    p <- ggplot(p_df, aes(x=reorder(nome, tot), y=tot)) + geom_col(fill="#198754") + coord_flip() + theme_minimal() + labs(x=NULL, y=NULL)
    ggplotly(p)
  })
  
  output$plot_trend_messaggi <- renderPlotly({
    t_df <- df %>% filter(ind == "ind7", fil_val == input$anno_ov) %>% arrange(sub_fil_val)
    p <- ggplot(t_df, aes(x=sub_fil_val, y=ind_val, group=1)) + geom_line(color="#6f42c1", size=1) + theme_minimal() + labs(x="Mese", y=NULL)
    ggplotly(p)
  })

  # --- DISTRIBUZIONE REGIONALE ---
  output$plot_regionale_bar <- renderPlotly({
    reg_df <- df %>% filter(fil=="fil_reg", ind==input$sel_ind_reg) %>% left_join(mappatura_nomi, by=c("fil_val"="codice_reg")) %>% arrange(desc(ind_val))
    p <- ggplot(reg_df, aes(x=reorder(reg, ind_val), y=ind_val)) + geom_col(fill="#007bff") + coord_flip() + theme_minimal() + theme(axis.text.y=element_text(angle=30, size=8, hjust=1))
    ggplotly(p) %>% layout(margin=list(l=100))
  })

  output$mappa_regioni <- renderLeaflet({
    dati_m <- df %>% filter(fil=="fil_reg", ind==input$sel_ind_reg)
    mf <- regioni_shape %>% left_join(dati_m, by=c("reg_code"="fil_val"))
    pal <- colorNumeric("YlGnBu", mf$ind_val)
    leaflet(mf) %>% addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(fillColor=~pal(ind_val), weight=1, color="white", fillOpacity=0.8, label=~paste0(reg_name,": ", format(ind_val, big.mark=".")))
  })

  # --- ANALISI UNIFICATA PAGOPA ---
  output$plot_pagoPA_categorie <- renderPlotly({
    c_df <- df %>% filter(fil=="fil_anno", fil_val==input$anno_pago, sub_fil_val==input$mese_pago, ind %in% paste0("ind", 8:26)) %>% mutate(nome=ind_pagoPA_categorie[ind]) %>% arrange(desc(ind_val))
    if(nrow(c_df)==0) return(NULL)
    p <- ggplot(c_df, aes(x=reorder(nome, ind_val), y=ind_val)) + geom_col(fill="#28a745") + coord_flip() + theme_minimal() + labs(x=NULL, y="N. Transazioni")
    ggplotly(p) %>% layout(margin=list(l=150))
  })

  output$titolo_fasce <- renderText({ paste("B. Fasce d'Importo per:", ind_fasce_nomi[input$cat_fasce], "(Anno", input$anno_pago, ")") })
  
  output$plot_pagoPA_fasce <- renderPlotly({
    f_df <- df %>% filter(fil=="fil_anno", fil_val==input$anno_pago, sub_fil=="fil_fascia", ind==input$cat_fasce)
    if(nrow(f_df)==0) return(NULL)
    p <- ggplot(f_df, aes(x=sub_fil_val, y=ind_val)) + geom_col(fill="#198754") + theme_minimal() + labs(x="Fascia (€)", y="N. Transazioni") + theme(axis.text.x=element_text(angle=45, hjust=1))
    ggplotly(p)
  })

  # --- MESSAGGI IO ---
  output$val_io_mese <- renderText({ format(sum(df$ind_val[df$ind=="ind7" & df$fil_val==input$anno_io & df$sub_fil_val==input$mese_io], na.rm=T), big.mark=".") })
  output$plot_io_anno <- renderPlotly({
    i_df <- df %>% filter(ind=="ind7", fil_val==input$anno_io) %>% arrange(sub_fil_val)
    p <- ggplot(i_df, aes(x=sub_fil_val, y=ind_val, group=1)) + geom_area(fill="#6f42c1", alpha=0.3) + geom_line(color="#6f42c1") + theme_minimal()
    ggplotly(p)
  })

  # --- SEND ---
  output$val_send_an <- renderText({ format(sum(df$ind_val[df$ind=="ind27" & df$fil_val==input$anno_send & df$sub_fil_val==input$mese_send], na.rm=T), big.mark=".") })
  output$val_send_dig <- renderText({ format(sum(df$ind_val[df$ind=="ind28" & df$fil_val==input$anno_send & df$sub_fil_val==input$mese_send], na.rm=T), big.mark=".") })
  output$val_send_tot <- renderText({ format(sum(df$ind_val[df$ind=="ind29" & df$fil_val==input$anno_send & df$sub_fil_val==input$mese_send], na.rm=T), big.mark=".") })
  
  output$plot_send_mese <- renderPlotly({
    comp <- data.frame(T=c("Analo","Digit"), V=c(sum(df$ind_val[df$ind=="ind27" & df$fil_val==input$anno_send & df$sub_fil_val==input$mese_send]), sum(df$ind_val[df$ind=="ind28" & df$fil_val==input$anno_send & df$sub_fil_val==input$mese_send])))
    p <- ggplot(comp, aes(x=T, y=V, fill=T)) + geom_col() + theme_minimal() + theme(legend.position="none")
    ggplotly(p)
  })

  output$plot_send_ambito <- renderPlotly({
    a_df <- df %>% filter(ind=="ind30", fil=="fil_ambito") %>% arrange(desc(ind_val))
    p <- ggplot(a_df, aes(x=reorder(fil_val, ind_val), y=ind_val)) + geom_col(fill="#fd7e14") + coord_flip() + theme_minimal()
    ggplotly(p) %>% layout(margin=list(l=150))
  })
}

shinyApp(ui, server)