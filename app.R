install.packages("rsconnect")
install.packages(c(
  "shiny", #para crear aplicaciones web interactivas en R.
  "shinydashboard", # Permite crear dashboards profesionales con: Sidebar,KPIs, Cajas, Menús
  "tidyverse", #Conjunto de paquetes para:
  # limpiar datos
  #   filtrar
  # transformar
  # graficar
  "plotly", # Convierte gráficos normales en gráficos interactivos
  "leaflet", # Sirve para crear mapas interactivos.
  "WDI", # Conecta directamente con la API del Banco Mundial
  "DT", # Crea tablas interactivas:búsqueda,filtros,exportación
  "countrycode", # Convierte nombres de países a:continentes,códigos ISO,regiones
  "rnaturalearth", 
  "rnaturalearthdata",
  "sf", 'rsconnect'# Descargan y manejan mapas mundiales.
))


library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(WDI)
library(DT)
library(countrycode)
library(rnaturalearth)
library(sf)
library(rsconnect)
# =========================================
# DESCARGAR DATOS BANCO MUNDIAL
# =========================================
rsconnect::setAccountInfo(name='nils-villalva-250203', token='CD1FB5834E9324F245481576BDA3B5FF', secret='u8IxD923tFhpGcWphHziiKCwfnR8TNeuev4xBRYU')

# Indicadores
# NY.GDP.MKTP.CD = PIB
# SP.POP.TOTL = Población
# SL.UEM.TOTL.ZS = Desempleo
# EN.ATM.CO2E.PC = CO2 per cápita

# =========================================
# DESCARGAR DATOS DEL BANCO MUNDIAL
# =========================================

wb_data <- WDI(
  country = "all",
  indicator = c(
    gdp = "NY.GDP.MKTP.CD",
    population = "SP.POP.TOTL",
    unemployment = "SL.UEM.TOTL.ZS",
    co2_pc = "EN.ATM.CO2E.PC"   # <- CAMBIADO
  ),
  start = 2000,
  end = 2024,
  extra = TRUE
)

# =========================================
# LIMPIAR DATOS
# =========================================

wb_data <- wb_data %>%
  
  # Eliminar agregados regionales
  filter(region != "Aggregates") %>%
  
  # Renombrar columnas
  dplyr::rename(
    country = country,
    iso3 = iso3c,
    continent = region
  )
# =========================================
# MAPA
# =========================================
# Descargar el mapa del mundo
world <- ne_countries(scale = "medium", returnclass = "sf")
# Detalle "medio" y como objeto "espacial moderno"
# =========================================
# UI
# =========================================
# Estructura del dashboard
ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Dashboard del Banco Mundial"
  ),
  
  ## Barra lateral izquierda.
  dashboardSidebar(
    
    ## Menú
    sidebarMenu(
      
      ## Pestaña del menú
      menuItem(
        "Tablero",
        tabName = "dashboard",
        icon = icon("chart-line")
      ),
      
      # FILTRO CONTINENTE
      selectInput(
        "continent",
        "Selecciona Continente",
        choices = sort(unique(wb_data$continent)),
        selected = NULL
      ),
      
      # FILTRO PAIS
      uiOutput("country_ui"),
      
      # FILTRO INDICADOR
      selectInput(
        "indicator",
        "Selecciona Indicador",
        choices = c(
          "PIB" = "gdp",
          "Población" = "population",
          "Desempleo" = "unemployment",
          "CO2 per cápita" = "co2_pc"
        ),
        selected = "gdp"
      )
      
    )
    
  ),   # <- ESTA COMA FALTABA
  
  # =====================================
  # CONTENIDO PRINCIPAL
  # =====================================
  
  dashboardBody(
    
    fluidRow(
      
      valueBoxOutput("kpi1", width = 4),
      valueBoxOutput("kpi2", width = 4),
      valueBoxOutput("kpi3", width = 4)
      
    ),
    
    fluidRow(
      
      tabBox(
        
        title = "Análisis Económico",
        width = 12,
        
        # TAB 1
        tabPanel(
          "Serie Temporal",
          br(),
          plotlyOutput("line_plot", height = 500)
        ),
        
        # TAB 2
        tabPanel(
          "Comparación Países",
          br(),
          plotlyOutput("comparison_plot", height = 700)
        ),
        
        # TAB 3
        tabPanel(
          "Mapa",
          br(),
          leafletOutput("world_map", height = 650)
        ),
        
        # TAB 4
        tabPanel(
          "Tabla Resumen",
          br(),
          DTOutput("summary_table")
        )
        
      )
      
    )
    
  )
  
)
# =========================================
# SERVER
# =========================================
server <- function(input, output, session) {
  
  # =========================================
  # FILTRO PAIS
  # =========================================
  
  output$country_ui <- renderUI({
    
    countries <- wb_data %>%
      filter(continent == input$continent) %>%
      pull(country) %>%
      unique() %>%
      sort()
    
    selectInput(
      "country",
      "Selecciona País",
      choices = countries,
      selected = countries[1]
    )
    
  })
  
  # =========================================
  # DATOS FILTRADOS
  # =========================================
  
  filtered_data <- reactive({
    
    wb_data %>%
      filter(
        continent == input$continent,
        country == input$country
      )
    
  })
  
  # =========================================
  # KPI 1
  # =========================================
  
  output$kpi1 <- renderValueBox({
    
    latest <- filtered_data() %>%
      filter(year == max(year, na.rm = TRUE))
    
    valueBox(
      
      paste0(
        format(
          round(latest[[input$indicator]] / 1e6, 2),
          big.mark = ","
        ),
        " mill."
      ),
      
      "Último Valor",
      
      icon = icon("chart-line"),
      
      color = "blue"
      
    )
    
  })
  
  # =========================================
  # KPI 2
  # =========================================
  
  output$kpi2 <- renderValueBox({
    
    avg_value <- filtered_data() %>%
      summarise(
        avg = mean(.data[[input$indicator]], na.rm = TRUE)
      )
    
    valueBox(
      
      format(
        round(avg_value$avg, 2),
        big.mark = ","
      ),
      
      "Promedio",
      
      icon = icon("calculator"),
      
      color = "green"
      
    )
    
  })
  
  # =========================================
  # KPI 3
  # =========================================
  
  output$kpi3 <- renderValueBox({
    
    growth <- filtered_data() %>%
      arrange(year) %>%
      drop_na(!!sym(input$indicator))
    
    if(nrow(growth) < 2){
      
      return(
        valueBox(
          "No disponible",
          "Crecimiento",
          icon = icon("triangle-exclamation"),
          color = "yellow"
        )
      )
      
    }
    
    last_val <- tail(growth[[input$indicator]], 1)
    
    prev_val <- tail(growth[[input$indicator]], 2)[1]
    
    if(prev_val == 0 || is.na(prev_val)){
      
      return(
        valueBox(
          "No disponible",
          "Crecimiento",
          icon = icon("triangle-exclamation"),
          color = "yellow"
        )
      )
      
    }
    
    growth_rate <- ((last_val - prev_val) / prev_val) * 100
    
    valueBox(
      
      paste0(round(growth_rate, 2), "%"),
      
      "Crecimiento Último Año",
      
      icon = icon("arrow-up"),
      
      color = ifelse(growth_rate >= 0, "green", "red")
      
    )
    
  })
  
  # =========================================
  # GRAFICO LINEAL
  # =========================================
  
  output$line_plot <- renderPlotly({
    
    p <- ggplot(
      
      filtered_data(),
      
      aes(
        x = year,
        y = .data[[input$indicator]]
      )
      
    ) +
      
      geom_line(
        color = "darkgreen",
        linewidth = 1.2
      ) +
      
      geom_point(color = "black") +
      
      theme_minimal() +
      
      labs(
        x = "Año",
        y = input$indicator,
        title = paste(
          input$country,
          "-",
          input$indicator
        )
      )
    
    ggplotly(p)
    
  })
  
  # =========================================
  # COMPARACION ENTRE PAISES
  # =========================================
  
  output$comparison_plot <- renderPlotly({
    
    latest_year <- max(wb_data$year, na.rm = TRUE)
    
    comparison_data <- wb_data %>%
      
      filter(
        continent == input$continent,
        year == latest_year
      ) %>%
      
      select(
        country,
        value = all_of(input$indicator)
      ) %>%
      
      drop_na() %>%
      
      arrange(value)
    
    p <- ggplot(
      
      comparison_data,
      
      aes(
        x = reorder(country, value),
        y = value,
        text = paste(
          "País:", country,
          "<br>Valor:", round(value, 2)
        )
      )
      
    ) +
      
      geom_col(fill = "steelblue") +
      
      coord_flip() +
      
      theme_minimal() +
      
      labs(
        title = paste(
          "Comparación entre Países -",
          latest_year
        ),
        x = "País",
        y = input$indicator
      ) +
      
      theme(
        plot.title = element_text(
          size = 18,
          face = "bold"
        ),
        axis.text.y = element_text(size = 9)
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # =========================================
  # MAPA MUNDIAL
  # =========================================
  
  output$world_map <- renderLeaflet({
    
    latest_year <- max(wb_data$year, na.rm = TRUE)
    
    map_data <- wb_data %>%
      
      filter(year == latest_year) %>%
      
      select(
        iso3,
        value = all_of(input$indicator)
      )
    
    world_map <- world %>%
      
      left_join(
        map_data,
        by = c("iso_a3" = "iso3")
      )
    
    pal <- colorNumeric(
      "YlOrRd",
      domain = world_map$value
    )
    
    leaflet(world_map) %>%
      
      addTiles() %>%
      
      addPolygons(
        
        fillColor = ~pal(value),
        
        weight = 1,
        
        color = "white",
        
        fillOpacity = 0.7,
        
        popup = ~paste(
          "<strong>País:</strong>", name,
          "<br><strong>Valor:</strong>",
          round(value, 2)
        )
        
      ) %>%
      
      addLegend(
        pal = pal,
        values = ~value,
        title = input$indicator
      )
    
  })
  
  # =========================================
  # TABLA RESUMEN
  # =========================================
  
  output$summary_table <- renderDT({
    
    latest_year <- max(wb_data$year, na.rm = TRUE)
    
    summary_data <- wb_data %>%
      
      filter(
        continent == input$continent,
        year == latest_year
      ) %>%
      
      select(
        País = country,
        PIB = gdp,
        Población = population,
        Desempleo = unemployment,
        co2_pc = co2_pc
      ) %>%
      
      drop_na()
    
    datatable(
      
      summary_data,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      
      rownames = FALSE
      
    ) %>%
      
      formatRound(
        columns = c("PIB", "Población"),
        digits = 0,
        mark = ","
      ) %>%
      
      formatRound(
        columns = c("Desempleo", "CO2"),
        digits = 2
      )
    
  })
  
}
# =========================================
# EJECUTAR APP
# =========================================
library(rsconnect)

deployApp(
  appDir = "C:/Users/HP/Desktop/UNCP/UNCP 2026-10/ECONOMETRIA 1/dashboard_bm",
  appPrimaryDoc = "app.R"
)
shinyApp(ui, server)
rsconnect::deployApp()