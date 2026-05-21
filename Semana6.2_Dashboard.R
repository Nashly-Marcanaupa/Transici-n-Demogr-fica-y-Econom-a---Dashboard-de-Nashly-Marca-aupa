#################################################
###### UNCP ##################
###### Facultad de Economía #####################
# =============================================
# *** Econometría I
# *** Prof. Joel Turco Quinto
# *** Tema  : Dashboard
# *** Semana: 06-1
# =============================================

# =====================================================
# 1. LIMPIAR ENTORNO
# =====================================================

rm(list = ls())

# =====================================================
# 2. INSTALAR PAQUETES (SOLO LA PRIMERA VEZ)
# =====================================================

# install.packages(c(
#   "gapminder",
#   "shiny",
#   "plotly",
#   "tidyverse",
#   "DT",
#   "readxl",
# ))

# =====================================================
# 3. CARGAR LIBRERÍAS
# =====================================================

library(gapminder)
library(shiny)
library(plotly)
library(tidyverse)
library(DT)
library(readxl)
library(ggplot2)

# =====================================================
# 4. EXPLORACIÓN Y LIMPIEZA DE DATOS
# =====================================================

data <- gapminder %>% 
  filter(year >= 1962) %>% 
  mutate(
    gdp_total = gdpPercap * pop,
    continent = as.factor(continent)
  )

# Revisar valores nulos
print(colSums(is.na(data)))

# Revisar estructura
str(data)

# =====================================================
# 5. TRANSFORMACIÓN Y CÁLCULOS
# =====================================================

# PIB per cápita promedio por continente y año
gdp_by_continent <- data %>%
  group_by(continent, year) %>%
  summarise(
    gdpPercap_avg = mean(gdpPercap),
    lifeExp_avg = mean(lifeExp),
    total_pop = sum(pop),
    .groups = "drop"
  )

# Países con mayor PIB per cápita por año
top_gdp <- data %>%
  group_by(year) %>%
  slice_max(order_by = gdpPercap, n = 5) %>%
  ungroup()

# =====================================================
# 6. INTERFAZ DEL DASHBOARD (UI)
# =====================================================

ui <- fluidPage(
  
  titlePanel("Dashboard Gapminder - Econometría I"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "continent",
        "Selecciona Continente:",
        choices = unique(data$continent),
        selected = "Europe"
      ),
      
      sliderInput(
        "year",
        "Año:",
        min = 1952,
        max = 2007,
        value = 2007,
        step = 5,
        sep = "",
        animate = animationOptions(interval = 1500)
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Resumen",
          
          br(),
          
          dataTableOutput("table_summary"),
          
          br(),
          
          plotlyOutput("plot_gdp_life")
        ),
        
        tabPanel(
          "Comparación países",
          
          br(),
          
          plotlyOutput("plot_country")
        ),
        
        tabPanel(
          "Top PIB per cápita",
          
          br(),
          
          dataTableOutput("top_gdp_table")
        )
      )
    )
  )
)

# =====================================================
# 7. SERVER (LÓGICA INTERACTIVA)
# =====================================================

server <- function(input, output) {
  
  # ---------------------------------------------
  # FILTRO REACTIVO
  # ---------------------------------------------
  
  filtered_data <- reactive({
    
    data %>%
      filter(
        continent == input$continent,
        year == input$year
      )
  })
  
  # ---------------------------------------------
  # GRÁFICO PIB VS ESPERANZA DE VIDA
  # ---------------------------------------------
  
  output$plot_gdp_life <- renderPlotly({
    
    plot_ly(
      data = filtered_data(),
      x = ~gdpPercap,
      y = ~lifeExp,
      type = "scatter",
      mode = "markers",
      size = ~pop,
      color = ~country,
      
      marker = list(
        sizemode = "diameter"
      ),
      
      text = ~paste(
        "País:", country,
        "<br>PIB per cápita:", round(gdpPercap, 2),
        "<br>Esperanza de vida:", round(lifeExp, 2),
        "<br>Población:", pop
      ),
      
      hoverinfo = "text"
      
    ) %>%
      
      layout(
        title = "PIB per cápita vs Esperanza de Vida",
        
        xaxis = list(
          title = "PIB per cápita"
        ),
        
        yaxis = list(
          title = "Esperanza de vida"
        )
      )
  })
  
  # ---------------------------------------------
  # TABLA RESUMEN
  # ---------------------------------------------
  
  output$table_summary <- renderDataTable({
    
    filtered_data() %>%
      select(
        country,
        gdpPercap,
        lifeExp,
        pop
      ) %>%
      arrange(desc(gdpPercap))
    
  })
  
  # ---------------------------------------------
  # EVOLUCIÓN TEMPORAL
  # ---------------------------------------------
  
  output$plot_country <- renderPlotly({
    
    plot_data <- data %>%
      filter(continent == input$continent)
    
    gg <- ggplot(
      plot_data,
      aes(
        x = year,
        y = gdpPercap,
        color = country
      )
    ) +
      
      geom_line() +
      
      labs(
        title = paste(
          "PIB per cápita en",
          input$continent
        ),
        
        x = "Año",
        y = "PIB per cápita"
      ) +
      
      theme_economist()
    
    ggplotly(gg)
  })
  
  # ---------------------------------------------
  # TOP PIB
  # ---------------------------------------------
  
  output$top_gdp_table <- renderDataTable({
    
    top_gdp %>%
      filter(year == input$year) %>%
      select(
        country,
        continent,
        year,
        gdpPercap,
        lifeExp,
        pop
      )
  })
}

# =====================================================
# 8. EJECUTAR DASHBOARD
# =====================================================

shinyApp(ui = ui, server = server)