############################################################
############################UNCP ###########################
###################Facultad de Economía ####################
############################################################
############## DASHBOARD ECONÓMICO - GAPMINDER #############
############################################################
# Título:
# Transición Demográfica y Economía
# Dashboard de Nashly Marcañaupa
############################################################

# ==========================================================
# 1. INSTALAR PAQUETES
# ==========================================================

paquetes <- c(
  "shiny",
  "shinydashboard",
  "plotly",
  "gapminder",
  "tidyverse",
  "DT",
  "leaflet",
  "ggcorrplot"
)

instalados <- paquetes %in% installed.packages()

if(any(instalados == FALSE)){
  install.packages(paquetes[!instalados])
}

# ==========================================================
# 2. LIBRERÍAS
# ==========================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(gapminder)
library(tidyverse)
library(DT)
library(leaflet)
library(ggcorrplot)

# ==========================================================
# 3. BASE DE DATOS
# ==========================================================

data_base <- gapminder %>%
  mutate(
    gdp_total = gdpPercap * pop,
    continent = as.factor(continent)
  )

# ==========================================================
# 4. COORDENADAS APROXIMADAS PARA MAPA
# ==========================================================

coords <- tibble(
  continent = c("Africa","Americas","Asia","Europe","Oceania"),
  lat = c(1, -15, 34, 54, -25),
  lon = c(20, -70, 100, 15, 135)
)

data_map <- data_base %>%
  left_join(coords, by = "continent")

# ==========================================================
# 5. UI
# ==========================================================

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Dashboard Económico"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Datos",
        tabName = "datos",
        icon = icon("table")
      )
    ),
    
    br(),
    
    selectInput(
      "continent",
      "Continente",
      choices = levels(data_base$continent),
      selected = "Americas"
    ),
    
    selectInput(
      "country",
      "País",
      choices = NULL
    ),
    
    sliderInput(
      "year",
      "Año",
      min = min(data_base$year),
      max = max(data_base$year),
      value = max(data_base$year),
      step = 5,
      sep = ""
    ),
    
    selectInput(
      "indicator_x",
      "Variable X",
      choices = c(
        "Esperanza de Vida" = "lifeExp",
        "Población" = "pop"
      ),
      selected = "lifeExp"
    ),
    
    selectInput(
      "indicator_y",
      "Variable Y",
      choices = c(
        "PIB per cápita" = "gdpPercap",
        "PIB Total" = "gdp_total"
      ),
      selected = "gdpPercap"
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
      
      .content-wrapper {
        background-color: #f4f6f9;
      }
      
      .box {
        border-radius: 10px;
      }
      
      .small-box {
        border-radius: 10px;
      }
      
      "))
    ),
    
    tabItems(
      
      # ======================================================
      # DASHBOARD PRINCIPAL
      # ======================================================
      
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          
          box(
            width = 12,
            title = "Transición Demográfica y Economía - Dashboard de Nashly Marcañaupa",
            status = "primary",
            solidHeader = TRUE,
            h3("Análisis Económico y Demográfico Mundial con Gapminder")
          )
        ),
        
        # ==================================================
        # KPI
        # ==================================================
        
        fluidRow(
          
          valueBoxOutput("box_pop", width = 4),
          valueBoxOutput("box_life", width = 4),
          valueBoxOutput("box_gdp", width = 4)
          
        ),
        
        # ==================================================
        # MAPA + SCATTER
        # ==================================================
        
        fluidRow(
          
          box(
            title = "Mapa Mundial",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            leafletOutput("mapa", height = 400)
          ),
          
          box(
            title = "Relación entre Variables",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("scatterPlot", height = 400)
          )
        ),
        
        # ==================================================
        # SERIES + BARRAS
        # ==================================================
        
        fluidRow(
          
          box(
            title = "Evolución Histórica",
            width = 8,
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("timeSeriesPlot", height = 350)
          ),
          
          box(
            title = "Top 10 Países",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("barPlot", height = 350)
          )
        ),
        
        # ==================================================
        # REGRESIÓN + PROYECCIÓN
        # ==================================================
        
        fluidRow(
          
          box(
            title = "Regresión Lineal",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("regressionPlot", height = 350)
          ),
          
          box(
            title = "Proyección Económica",
            width = 6,
            status = "danger",
            solidHeader = TRUE,
            plotlyOutput("forecastPlot", height = 350)
          )
        ),
        
        # ==================================================
        # CORRELACIÓN
        # ==================================================
        
        fluidRow(
          
          box(
            title = "Matriz de Correlación",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("corrPlot", height = 400)
          )
        )
      ),
      
      # ======================================================
      # TABLA
      # ======================================================
      
      tabItem(
        tabName = "datos",
        
        box(
          title = "Base de Datos Gapminder",
          width = 12,
          DTOutput("tabla")
        )
      )
    )
  )
)

# ==========================================================
# 6. SERVER
# ==========================================================

server <- function(input, output, session){
  
  # ======================================================
  # ACTUALIZAR PAÍSES
  # ======================================================
  
  observe({
    
    countries <- data_base %>%
      filter(continent == input$continent) %>%
      pull(country) %>%
      unique() %>%
      sort()
    
    updateSelectInput(
      session,
      "country",
      choices = countries,
      selected = countries[1]
    )
  })
  
  # ======================================================
  # DATOS REACTIVOS
  # ======================================================
  
  data_year <- reactive({
    
    data_base %>%
      filter(year == input$year)
  })
  
  country_data <- reactive({
    
    req(input$country)
    
    data_base %>%
      filter(country == input$country)
  })
  
  # ======================================================
  # KPI
  # ======================================================
  
  output$box_pop <- renderValueBox({
    
    val <- country_data() %>%
      filter(year == input$year) %>%
      pull(pop)
    
    valueBox(
      paste(round(val/1000000,2),"Mill."),
      "Población",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$box_life <- renderValueBox({
    
    val <- country_data() %>%
      filter(year == input$year) %>%
      pull(lifeExp)
    
    valueBox(
      paste(round(val,1),"años"),
      "Esperanza de Vida",
      icon = icon("heartbeat"),
      color = "green"
    )
  })
  
  output$box_gdp <- renderValueBox({
    
    val <- country_data() %>%
      filter(year == input$year) %>%
      pull(gdpPercap)
    
    valueBox(
      paste("$",round(val,0)),
      "PIB per cápita",
      icon = icon("usd"),
      color = "red"
    )
  })
  
  # ======================================================
  # MAPA
  # ======================================================
  
  output$mapa <- renderLeaflet({
    
    mapa_data <- data_map %>%
      filter(year == input$year)
    
    leaflet(mapa_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~sqrt(pop)/500,
        popup = ~paste(
          "<b>País:</b>", country,
          "<br><b>PIB pc:</b>", round(gdpPercap,0),
          "<br><b>Vida:</b>", round(lifeExp,1)
        ),
        color = "blue",
        fillOpacity = 0.6
      )
  })
  
  # ======================================================
  # SCATTER
  # ======================================================
  
  output$scatterPlot <- renderPlotly({
    
    p <- ggplot(
      data_year(),
      aes(
        x = .data[[input$indicator_x]],
        y = .data[[input$indicator_y]],
        color = continent,
        size = pop,
        text = country
      )
    ) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      geom_smooth(method = "lm", se = FALSE)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ======================================================
  # SERIES TEMPORALES
  # ======================================================
  
  output$timeSeriesPlot <- renderPlotly({
    
    p <- ggplot(
      country_data(),
      aes(
        x = year,
        y = .data[[input$indicator_y]]
      )
    ) +
      geom_line(color = "darkblue", linewidth = 1.2) +
      geom_point(color = "red", size = 2) +
      theme_light()
    
    ggplotly(p)
  })
  
  # ======================================================
  # TOP 10
  # ======================================================
  
  output$barPlot <- renderPlotly({
    
    df <- data_year() %>%
      filter(continent == input$continent) %>%
      arrange(desc(.data[[input$indicator_y]])) %>%
      slice(1:10)
    
    p <- ggplot(
      df,
      aes(
        x = reorder(country, .data[[input$indicator_y]]),
        y = .data[[input$indicator_y]]
      )
    ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ======================================================
  # REGRESIÓN
  # ======================================================
  
  output$regressionPlot <- renderPlotly({
    
    p <- ggplot(
      data_year(),
      aes(
        x = lifeExp,
        y = gdpPercap
      )
    ) +
      geom_point(color = "darkgreen") +
      geom_smooth(method = "lm", color = "red") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ======================================================
  # PROYECCIÓN
  # ======================================================
  
  output$forecastPlot <- renderPlotly({
    
    df <- country_data()
    
    modelo <- lm(gdpPercap ~ year, data = df)
    
    future <- data.frame(
      year = seq(max(df$year), 2030, 5)
    )
    
    future$pred <- predict(modelo, newdata = future)
    
    p <- ggplot() +
      
      geom_line(
        data = df,
        aes(x = year, y = gdpPercap),
        color = "blue",
        linewidth = 1
      ) +
      
      geom_line(
        data = future,
        aes(x = year, y = pred),
        color = "red",
        linewidth = 1.2,
        linetype = "dashed"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ======================================================
  # CORRELACIÓN
  # ======================================================
  
  output$corrPlot <- renderPlot({
    
    corr_data <- data_year() %>%
      select(lifeExp, pop, gdpPercap, gdp_total)
    
    corr <- cor(corr_data)
    
    ggcorrplot(corr,
               lab = TRUE,
               colors = c("red","white","blue"))
  })
  
  # ======================================================
  # TABLA
  # ======================================================
  
  output$tabla <- renderDT({
    
    datatable(
      data_base,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
}

# ==========================================================
# 7. EJECUTAR APP
# ==========================================================

shinyApp(ui = ui, server = server)