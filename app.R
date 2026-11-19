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
                 tableOutput("table_diesel")),

        tabPanel("Heizöl",
                 br(),
                 plotOutput("plot_heizoel"),
                 br(),
                 h4("Datentabelle Heizöl (Monatlich)"),
                 p("Schweiz: LIK-Durchschnittspreis, Bezugsmenge 3'001 - 6'000 l (CHF/100 l -> CHF/l). ",
                   "EU: Weekly Oil Bulletin, Heating gas oil (EUR/1000 l -> EUR/l)."),
                 tableOutput("table_heizoel")),

        tabPanel("Wechselkurs EUR/CHF",
                 br(),
                 plotOutput("plot_fx"),
                 br(),
                 h4("Datentabelle Wechselkurs (Monatlich)"),
                 p("Monatsdurchschnitt: CHF pro 1 EUR. Gestrichelte Linie = 24.02.2022 (Beginn Ukraine-Krieg)."),
                 tableOutput("table_fx")),

        tabPanel("Benzin 95 in CHF",
                 br(),
                 plotOutput("plot_benzin_chf"),
                 br(),
                 plotOutput("plot_benzin_diff"),
                 br(),
                 h4("Datentabelle Benzin 95 mit Wechselkurs (Monatlich)"),
                 tableOutput("table_benzin_chf")),

        tabPanel("Diesel in CHF",
                 br(),
                 plotOutput("plot_diesel_chf"),
                 br(),
                 plotOutput("plot_diesel_diff"),
                 br(),
                 h4("Datentabelle Diesel mit Wechselkurs (Monatlich)"),
                 tableOutput("table_diesel_chf")),

        tabPanel("Heizöl in CHF",
                 br(),
                 plotOutput("plot_heizoel_chf"),
                 br(),
                 plotOutput("plot_heizoel_diff"),
                 br(),
                 h4("Datentabelle Heizöl mit Wechselkurs (Monatlich)"),
                 tableOutput("table_heizoel_chf")),

        tabPanel("Heizgradtage",
                 br(),
                 plotOutput("plot_hgt"),
                 br(),
                 plotOutput("plot_hgt_saison"),
                 br(),
                 h4("Datentabelle Heizgradtage (Monatlich)"),
                 p("Heizgradtage (HGT) Schweiz, gewichtet (BFE). Je kälter der Monat, desto höher der Wert. ",
                   "Gestrichelte Linie = 24.02.2022 (Beginn Ukraine-Krieg)."),
                 tableOutput("table_hgt"))
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
      select(Date, Benzin95 = 2, Diesel = 3, Heizoel_100l = 4) %>%
      mutate(across(c(Benzin95, Diesel, Heizoel_100l), as.numeric),
             # Heizöl im LIK in CHF pro 100 l -> Umrechnung auf 1 l
             Heizoel = Heizoel_100l / 100) %>%
      select(Date, Benzin95, Diesel, Heizoel)
    
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
        Diesel_raw = as.numeric(str_replace_all(as.character(.[[3]]), "'", "")),
        Heizoel_raw = as.numeric(str_replace_all(as.character(.[[4]]), "'", ""))
      ) %>%
      group_by(Month) %>%
      summarise(
        Benzin95_1000l = mean(Benzin_raw, na.rm = TRUE),
        Diesel_1000l = mean(Diesel_raw, na.rm = TRUE),
        Heizoel_1000l = mean(Heizoel_raw, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        # Umrechnung von 1000l auf 1l
        Benzin95 = Benzin95_1000l / 1000,
        Diesel = Diesel_1000l / 1000,
        Heizoel = Heizoel_1000l / 1000,
        Date = Month
      ) %>%
      # Monate ohne Werte (NaN aus mean) als NA behandeln
      mutate(across(c(Benzin95, Diesel, Heizoel), ~ ifelse(is.nan(.x), NA, .x))) %>%
      select(Date, Benzin95, Diesel, Heizoel)
    
    return(df)
  })
  
  # 3. Wechselkurs einlesen (Spalte 1 = Monat "YYYY-MM", Spalte 2 = CHF pro 1 EUR)
  data_fx <- reactive({
    file_path <- "Euro Exchange.xlsx"
    validate(need(file.exists(file_path), paste("Datei nicht gefunden:", file_path)))

    read_excel(file_path, col_types = "text") %>%
      select(Monat = 1, Kurs = 2) %>%
      mutate(
        Date = as.Date(paste0(Monat, "-01")),
        EUR_CHF = as.numeric(Kurs)
      ) %>%
      filter(!is.na(Date), !is.na(EUR_CHF)) %>%
      select(Date, EUR_CHF)
  })

  # 4. Heizgradtage einlesen (Jahr, Monat, HGT, CDD) - Kühlgradtage (CDD) werden nicht verwendet
  data_hgt <- reactive({
    file_path <- "Heizgradtage und Kühlgradtage.csv"
    validate(need(file.exists(file_path), paste("Datei nicht gefunden:", file_path)))

    read_csv(file_path, show_col_types = FALSE) %>%
      mutate(Date = make_date(Jahr, Monat, 1),
             HGT = as.numeric(HGT)) %>%
      filter(!is.na(Date), !is.na(HGT)) %>%
      select(Date, Jahr, Monat, HGT)
  })

  filtered_hgt <- reactive({
    req(data_hgt())
    data_hgt() %>%
      filter(Date >= as.Date(input$date_range[1]) & Date <= as.Date(input$date_range[2]))
  })

  filtered_fx <- reactive({
    req(data_fx())
    data_fx() %>%
      filter(Date >= as.Date(input$date_range[1]) & Date <= as.Date(input$date_range[2]))
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
  
  # Zusammenführen für Heizöl
  merged_heizoel <- reactive({
    req(data_bfs(), data_eu())
    ch <- data_bfs() %>% select(Date, Schweiz = Heizoel)
    eu <- data_eu() %>% select(Date, EU = Heizoel)

    inner_join(ch, eu, by = "Date") %>%
      filter(!is.na(Schweiz) | !is.na(EU)) %>%
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
  
  # Plot Heizöl
  output$plot_heizoel <- renderPlot({
    df <- merged_heizoel()
    req(nrow(df) > 0)

    df_long <- df %>% pivot_longer(cols = c(Schweiz, EU), names_to = "Region", values_to = "Preis")

    ggplot(df_long, aes(x = Date, y = Preis, color = Region)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      labs(title = "Monatlicher Preisvergleich: Heizöl (Schweiz vs. EU)",
           y = "Preis pro Liter (CHF / EUR)", x = "Zeitraum") +
      theme_minimal(base_size = 14)
  })

  # Plot Wechselkurs
  output$plot_fx <- renderPlot({
    df <- filtered_fx()
    req(nrow(df) > 0)

    ggplot(df, aes(x = Date, y = EUR_CHF)) +
      geom_line(linewidth = 1, color = "steelblue") +
      geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "grey40") +
      labs(title = "Monatlicher Wechselkurs EUR/CHF",
           y = "CHF pro 1 EUR", x = "Zeitraum") +
      theme_minimal(base_size = 14)
  })

  # Plot Heizgradtage: Zeitreihe
  output$plot_hgt <- renderPlot({
    df <- filtered_hgt()
    req(nrow(df) > 0)

    ggplot(df, aes(x = Date, y = HGT)) +
      geom_col(fill = "#1f77b4", width = 25) +
      geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "grey40") +
      labs(title = "Monatliche Heizgradtage Schweiz",
           y = "Heizgradtage (HGT)", x = "Zeitraum") +
      theme_minimal(base_size = 14)
  })

  # Plot Heizgradtage: Saisonverlauf, eine Linie pro Jahr
  output$plot_hgt_saison <- renderPlot({
    df <- filtered_hgt()
    req(nrow(df) > 0)

    ggplot(df, aes(x = Monat, y = HGT, group = Jahr, color = factor(Jahr))) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.5) +
      scale_x_continuous(breaks = 1:12,
                         labels = c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun",
                                    "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")) +
      labs(title = "Saisonverlauf der Heizgradtage nach Jahr",
           y = "Heizgradtage (HGT)", x = "Monat", color = "Jahr") +
      theme_minimal(base_size = 14)
  })

  output$table_hgt <- renderTable({
    filtered_hgt() %>%
      arrange(desc(Date)) %>%
      transmute(Monat = format(Date, "%Y-%m"), Heizgradtage = HGT)
  }, digits = 1)

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

  output$table_heizoel <- renderTable({
    merged_heizoel() %>%
      mutate(Date = format(Date, "%Y-%m")) %>%
      arrange(desc(Date))
  }, digits = 4)

  output$table_fx <- renderTable({
    filtered_fx() %>%
      mutate(Date = format(Date, "%Y-%m")) %>%
      rename(`CHF pro EUR` = EUR_CHF) %>%
      arrange(desc(Date))
  }, digits = 4)

  # ---------------------------------------------------------------------------
  # Vergleich mit Wechselkurs: EU-Preise (EUR) werden mit EUR/CHF in CHF umgerechnet
  # ---------------------------------------------------------------------------

  # Hängt den Wechselkurs an einen Schweiz/EU-Datensatz an und rechnet EU in CHF um
  add_fx <- function(merged_df) {
    merged_df %>%
      inner_join(data_fx(), by = "Date") %>%
      mutate(
        EU_CHF = EU * EUR_CHF,
        Diff_ohne = Schweiz - EU,      # CHF minus EUR (ohne Umrechnung)
        Diff_mit = Schweiz - EU_CHF    # CHF minus CHF (mit Umrechnung)
      )
  }

  merged_benzin_chf  <- reactive({ req(data_fx()); add_fx(merged_benzin()) })
  merged_diesel_chf  <- reactive({ req(data_fx()); add_fx(merged_diesel()) })
  merged_heizoel_chf <- reactive({ req(data_fx()); add_fx(merged_heizoel()) })

  # Preise: Schweiz (CHF), EU original (EUR) und EU umgerechnet (CHF)
  plot_chf <- function(df, produkt) {
    df_long <- df %>%
      select(Date,
             `Schweiz (CHF)` = Schweiz,
             `EU original (EUR)` = EU,
             `EU umgerechnet (CHF)` = EU_CHF) %>%
      pivot_longer(-Date, names_to = "Reihe", values_to = "Preis")

    ggplot(df_long, aes(x = Date, y = Preis, color = Reihe, linetype = Reihe)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "grey40") +
      scale_color_manual(values = c("Schweiz (CHF)" = "#d62728",
                                    "EU original (EUR)" = "#7f7f7f",
                                    "EU umgerechnet (CHF)" = "#1f77b4")) +
      scale_linetype_manual(values = c("Schweiz (CHF)" = "solid",
                                       "EU original (EUR)" = "dotted",
                                       "EU umgerechnet (CHF)" = "solid")) +
      labs(title = paste0("Preisvergleich ", produkt, ": EU in EUR vs. EU in CHF"),
           y = "Preis pro Liter", x = "Zeitraum", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  }

  # Preisdifferenz Schweiz - EU, einmal ohne und einmal mit Umrechnung
  plot_diff <- function(df, produkt) {
    df_long <- df %>%
      select(Date,
             `ohne Umrechnung (CHF - EUR)` = Diff_ohne,
             `mit Umrechnung (CHF - CHF)` = Diff_mit) %>%
      pivot_longer(-Date, names_to = "Differenz", values_to = "Wert")

    ggplot(df_long, aes(x = Date, y = Wert, color = Differenz)) +
      geom_hline(yintercept = 0, color = "grey60") +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed", color = "grey40") +
      scale_color_manual(values = c("ohne Umrechnung (CHF - EUR)" = "#7f7f7f",
                                    "mit Umrechnung (CHF - CHF)" = "#1f77b4")) +
      labs(title = paste0("Preisdifferenz Schweiz - EU: ", produkt),
           y = "Differenz pro Liter", x = "Zeitraum", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  }

  table_chf <- function(df) {
    df %>%
      arrange(desc(Date)) %>%
      transmute(Monat = format(Date, "%Y-%m"),
                `Schweiz (CHF)` = Schweiz,
                `EU (EUR)` = EU,
                `CHF pro EUR` = EUR_CHF,
                `EU (CHF)` = EU_CHF,
                `Diff. ohne Umrechnung` = Diff_ohne,
                `Diff. mit Umrechnung` = Diff_mit)
  }

  output$plot_benzin_chf <- renderPlot({
    df <- merged_benzin_chf(); req(nrow(df) > 0); plot_chf(df, "Benzin 95")
  })
  output$plot_benzin_diff <- renderPlot({
    df <- merged_benzin_chf(); req(nrow(df) > 0); plot_diff(df, "Benzin 95")
  })
  output$table_benzin_chf <- renderTable({ table_chf(merged_benzin_chf()) }, digits = 4)

  output$plot_diesel_chf <- renderPlot({
    df <- merged_diesel_chf(); req(nrow(df) > 0); plot_chf(df, "Diesel")
  })
  output$plot_diesel_diff <- renderPlot({
    df <- merged_diesel_chf(); req(nrow(df) > 0); plot_diff(df, "Diesel")
  })
  output$table_diesel_chf <- renderTable({ table_chf(merged_diesel_chf()) }, digits = 4)

  output$plot_heizoel_chf <- renderPlot({
    df <- merged_heizoel_chf(); req(nrow(df) > 0); plot_chf(df, "Heizöl")
  })
  output$plot_heizoel_diff <- renderPlot({
    df <- merged_heizoel_chf(); req(nrow(df) > 0); plot_diff(df, "Heizöl")
  })
  output$table_heizoel_chf <- renderTable({ table_chf(merged_heizoel_chf()) }, digits = 4)
}

shinyApp(ui = ui, server = server)