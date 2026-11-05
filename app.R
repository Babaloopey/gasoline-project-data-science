library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)

ui <- fluidPage(
  titlePanel("Treibstoffpreis-Vergleich: Schweiz (BFS) vs. Europa (EU Bulletin)"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Zeitraum auswählen:",
                     start = "2019-01-01",
                     end = Sys.Date(),
                     format = "yyyy-mm-dd",
                     language = "de",
                     separator = " bis ")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Benzin 95", 
                 br(),
                 plotOutput("plot_benzin"),
                 br(),
                 h4("Datentabelle Benzin 95 (Monatlich)"),
                 tableOutput("table_benzin")),
        
        tabPanel("Diesel", 
                 br(),
                 plotOutput("plot_diesel"),
                 br(),
                 h4("Datentabelle Diesel (Monatlich)"),
                 tableOutput("table_diesel"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # 1. BFS-Daten einlesen (Spalte 1 = Datum, Spalte 2 = Benzin, Spalte 3 = Diesel)
  data_bfs <- reactive({
    file_path <- "Landesindex CHF Benzin95 und Diesel.xlsx"
    validate(need(file.exists(file_path), paste("Datei nicht gefunden:", file_path)))
    
    df <- read_excel(file_path)
    
    df <- df %>%
      drop_na(1) %>%
      mutate(
        raw_val = .[[1]],
        Date = case_when(
          inherits(raw_val, c("POSIXt", "Date", "POSIXct")) ~ floor_date(as.Date(raw_val), "month"),
          is.numeric(raw_val) ~ floor_date(as.Date(raw_val, origin = "1899-12-30"), "month"),
          TRUE ~ floor_date(as.Date(paste0("01/", as.character(raw_val)), format = "%d/%m/%Y"), "month")
        )
      ) %>%
      filter(!is.na(Date)) %>%
      select(Date, Benzin95 = 2, Diesel = 3) %>%
      mutate(across(c(Benzin95, Diesel), as.numeric))
    
    return(df)
  })
  
  # 2. EU-Daten einlesen (Spalte 1 = Datum, Spalte 2 = Benzin, Spalte 3 = Diesel)
  data_eu <- reactive({
    file_path <- "Weekly Oil Bulletin Euro Benzin95 und Diesel.xlsx"
    validate(need(file.exists(file_path), paste("Datei nicht gefunden:", file_path)))
    
    df <- read_excel(file_path)
    
    df <- df %>%
      drop_na(1) %>%
      mutate(
        raw_val = .[[1]],
        Date = case_when(
          inherits(raw_val, c("POSIXt", "Date", "POSIXct")) ~ as.Date(raw_val),
          is.numeric(raw_val) ~ as.Date(raw_val, origin = "1899-12-30"),
          TRUE ~ dmy(as.character(raw_val))
        ),
        Month = floor_date(Date, "month")
      ) %>%
      filter(!is.na(Date)) %>%
      # Tausendertrennzeichen (') entfernen und in numerisch umwandeln
      mutate(
        Benzin_raw = as.numeric(str_replace_all(as.character(.[[2]]), "'", "")),
        Diesel_raw = as.numeric(str_replace_all(as.character(.[[3]]), "'", ""))
      ) %>%
      group_by(Month) %>%
      summarise(
        Benzin95_1000l = mean(Benzin_raw, na.rm = TRUE),
        Diesel_1000l = mean(Diesel_raw, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        # Umrechnung von 1000l auf 1l
        Benzin95 = Benzin95_1000l / 1000,
        Diesel = Diesel_1000l / 1000,
        Date = Month
      ) %>%
      select(Date, Benzin95, Diesel)
    
    return(df)
  })
  
  # Zusammenführen für Benzin 95
  merged_benzin <- reactive({
    req(data_bfs(), data_eu())
    ch <- data_bfs() %>% select(Date, Schweiz = Benzin95)
    eu <- data_eu() %>% select(Date, EU = Benzin95)
    
    inner_join(ch, eu, by = "Date") %>%
      filter(Date >= as.Date(input$date_range[1]) & Date <= as.Date(input$date_range[2]))
  })
  
  # Zusammenführen für Diesel
  merged_diesel <- reactive({
    req(data_bfs(), data_eu())
    ch <- data_bfs() %>% select(Date, Schweiz = Diesel)
    eu <- data_eu() %>% select(Date, EU = Diesel)
    
    inner_join(ch, eu, by = "Date") %>%
      filter(Date >= as.Date(input$date_range[1]) & Date <= as.Date(input$date_range[2]))
  })
  
  # Plot Benzin 95
  output$plot_benzin <- renderPlot({
    df <- merged_benzin()
    req(nrow(df) > 0)
    
    df_long <- df %>% pivot_longer(cols = c(Schweiz, EU), names_to = "Region", values_to = "Preis")
    
    ggplot(df_long, aes(x = Date, y = Preis, color = Region)) +
      geom_line(linewidth = 1) +
      labs(title = "Monatlicher Preisvergleich: Benzin 95 (Schweiz vs. EU)", 
           y = "Preis pro Liter (CHF / EUR)", x = "Zeitraum") +
      theme_minimal(base_size = 14)
  })
  
  # Plot Diesel
  output$plot_diesel <- renderPlot({
    df <- merged_diesel()
    req(nrow(df) > 0)
    
    df_long <- df %>% pivot_longer(cols = c(Schweiz, EU), names_to = "Region", values_to = "Preis")
    
    ggplot(df_long, aes(x = Date, y = Preis, color = Region)) +
      geom_line(linewidth = 1) +
      labs(title = "Monatlicher Preisvergleich: Diesel (Schweiz vs. EU)", 
           y = "Preis pro Liter (CHF / EUR)", x = "Zeitraum") +
      theme_minimal(base_size = 14)
  })
  
  # Tabellen mit formatiertem Monat (YYYY-MM)
  output$table_benzin <- renderTable({
    merged_benzin() %>% 
      mutate(Date = format(Date, "%Y-%m")) %>% 
      arrange(desc(Date))
  }, digits = 4)
  
  output$table_diesel <- renderTable({
    merged_diesel() %>% 
      mutate(Date = format(Date, "%Y-%m")) %>% 
      arrange(desc(Date))
  }, digits = 4)
}

shinyApp(ui = ui, server = server)