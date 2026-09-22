library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(viridisLite)

# ---------------------------------------------------------------------------
# DATOS
# ---------------------------------------------------------------------------

allBuffers <- readRDS("allBuffers.rds")

distancias_disponibles <- 1:300

names(allBuffers) <- as.character(distancias_disponibles)


# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  
  # -------------------------------------------------------------------------
  # CSS
  # -------------------------------------------------------------------------
  
  tags$head(
    tags$style(HTML("
      
      html, body {
        width: 100%;
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden;
      }
      
      .container-fluid {
        padding: 0;
      }
      
      /* ------------------------------------------------------------
         MAPA
         ------------------------------------------------------------ */
      
      #mapaBuffers {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100vh !important;
        z-index: 1;
      }
      
      /* ------------------------------------------------------------
         PANEL FLOTANTE
         ------------------------------------------------------------ */
      
      #panelControles {
        position: absolute;
        top: 20px;
        left: 20px;
        width: 330px;
        z-index: 1000;
        
        background-color: rgba(255, 255, 255, 0.95);
        
        padding: 20px;
        
        border-radius: 10px;
        
        box-shadow: 0 2px 15px rgba(0, 0, 0, 0.30);
      }
      
      #panelControles h4 {
        margin-top: 0;
        margin-bottom: 18px;
      }
      
      #panelControles .form-group {
        margin-bottom: 12px;
      }
      
      #panelControles .help-block {
        font-size: 12px;
        color: #666;
      }
      
      #panelControles .btn {
        margin-top: 5px;
      }
      
      /* ------------------------------------------------------------
         ADVERTENCIA
         ------------------------------------------------------------ */
      
      #advertencia {
        margin-top: 5px;
        margin-bottom: 5px;
      }
      
    "))
  ),
  
  
  # -------------------------------------------------------------------------
  # MAPA
  # -------------------------------------------------------------------------
  
  leafletOutput(
    outputId = "mapaBuffers",
    width = "100%",
    height = "100vh"
  ),
  
  
  # -------------------------------------------------------------------------
  # PANEL DE CONTROLES
  # -------------------------------------------------------------------------
  
  absolutePanel(
    
    id = "panelControles",
    
    top = 20,
    left = 20,
    
    fixed = TRUE,
    
    h4("Buffers de distancia a la costa"),
    
    
    # -----------------------------------------------------------------------
    # DISTANCIAS
    # -----------------------------------------------------------------------
    
    textInput(
      inputId = "distancias",
      label = "Distancias a la costa (mn):",
      value = "50, 100, 150, 200",
      placeholder = "Ej.: 50, 100, 150"
    ),
    
    helpText(
      paste0(
        "Valores entre ",
        min(distancias_disponibles),
        " y ",
        max(distancias_disponibles),
        " mn, separados por comas."
      )
    ),
    
    
    # -----------------------------------------------------------------------
    # ADVERTENCIA
    # -----------------------------------------------------------------------
    
    uiOutput("advertencia"),
    
    
    # -----------------------------------------------------------------------
    # MAPA BASE
    # -----------------------------------------------------------------------
    
    selectInput(
      inputId = "mapa_base",
      label = "Mapa base:",
      choices = c(
        "OpenStreetMap" = "osm",
        "Topográfico" = "topo",
        "Satélite" = "satellite"
      ),
      selected = "topo"
    ),
    
    
    br(),
    
    
    # -----------------------------------------------------------------------
    # DESCARGA
    # -----------------------------------------------------------------------
    
    downloadButton(
      outputId = "descargar",
      label = "Descargar buffers (GeoPackage)",
      width = "100%"
    ),
    
    br(),
    br(),
    helpText(
      strong("¿Tienes algún comentario o sugerencia?"),
      "Puedes hacerlo en el repositorio del proyecto:",
      a(
        "GitHub",
        href = "https://github.com/LuisLauM/peruvian_buffers",
        target = "_blank"
      )
    )
  )
)


# ---------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # =========================================================================
  # DISTANCIAS VÁLIDAS
  # =========================================================================
  
  distancias_validas <- reactive({
    
    valores <- suppressWarnings(
      as.numeric(
        trimws(
          strsplit(input$distancias, ",")[[1]]
        )
      )
    )
    
    valores <- valores[!is.na(valores)]
    
    valores <- valores[
      valores %in% distancias_disponibles
    ]
    
    sort(unique(valores))
  })
  
  
  # =========================================================================
  # ADVERTENCIAS
  # =========================================================================
  
  output$advertencia <- renderUI({
    
    ingresadas <- suppressWarnings(
      as.numeric(
        trimws(
          strsplit(input$distancias, ",")[[1]]
        )
      )
    )
    
    ingresadas <- ingresadas[!is.na(ingresadas)]
    
    invalidas <- setdiff(
      ingresadas,
      distancias_disponibles
    )
    
    if (length(invalidas) > 0) {
      
      tags$p(
        style = "color:red;",
        paste0(
          "Valores no disponibles y omitidos: ",
          paste(invalidas, collapse = ", ")
        )
      )
      
    }
  })
  
  
  # =========================================================================
  # BUFFERS SELECCIONADOS
  # =========================================================================
  
  buffers_seleccionados <- reactive({
    
    dists <- distancias_validas()
    
    req(length(dists) > 0)
    
    
    lista_sf <- lapply(
      dists,
      function(d) {
        
        obj <- allBuffers[[as.character(d)]]
        
        obj$distancia_mn <- d
        
        obj
      }
    )
    
    
    do.call(
      rbind,
      lista_sf
    )
  })
  
  
  # =========================================================================
  # PALETA DE COLORES
  # =========================================================================
  
  paleta <- reactive({
    
    dists <- distancias_validas()
    
    colorFactor(
      palette = viridisLite::turbo(length(dists)),
      domain = dists
    )
  })
  
  
  # =========================================================================
  # MAPA INICIAL
  # =========================================================================
  
  output$mapaBuffers <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(
        zoomControl = TRUE
      )
    ) |>
      
      # ---------------------------------------------------------------------
    # OPENSTREETMAP
    # ---------------------------------------------------------------------
    
    addTiles(
      group = "OpenStreetMap"
    ) |>
      
      # ---------------------------------------------------------------------
    # MAPA TOPOGRÁFICO ESRI
    # ---------------------------------------------------------------------
    
    addProviderTiles(
      providers$Esri.WorldTopoMap,
      group = "Topográfico"
    ) |>
      
      # ---------------------------------------------------------------------
    # SATÉLITE ESRI
    # ---------------------------------------------------------------------
    
    addProviderTiles(
      providers$Esri.WorldImagery,
      group = "Satélite"
    ) |>
      
      # ---------------------------------------------------------------------
    # CONTROL DE CAPAS
    # ---------------------------------------------------------------------
    
    addLayersControl(
      baseGroups = c(
        "OpenStreetMap",
        "Topográfico",
        "Satélite"
      ),
      options = layersControlOptions(
        collapsed = TRUE
      )
    ) |>
      
      # ---------------------------------------------------------------------
    # VISTA INICIAL SOBRE PERÚ
    # ---------------------------------------------------------------------
    
    setView(
      lng = -77,
      lat = -12,
      zoom = 5
    )
  })
  
  
  # =========================================================================
  # ACTUALIZAR MAPA CUANDO CAMBIAN LAS DISTANCIAS O MAPA BASE
  # =========================================================================
  
  observe({
    
    dists <- distancias_validas()
    
    proxy <- leafletProxy("mapaBuffers")
    
    
    # -----------------------------------------------------------------------
    # LIMPIAR BUFFERS ANTERIORES
    # -----------------------------------------------------------------------
    
    proxy |>
      clearShapes() |>
      clearControls()
    
    
    # -----------------------------------------------------------------------
    # CAMBIAR MAPA BASE
    # -----------------------------------------------------------------------
    
    if (input$mapa_base == "osm") {
      
      proxy |>
        showGroup("OpenStreetMap") |>
        hideGroup("Topográfico") |>
        hideGroup("Satélite")
      
    }
    
    
    if (input$mapa_base == "topo") {
      
      proxy |>
        hideGroup("OpenStreetMap") |>
        showGroup("Topográfico") |>
        hideGroup("Satélite")
      
    }
    
    
    if (input$mapa_base == "satellite") {
      
      proxy |>
        hideGroup("OpenStreetMap") |>
        hideGroup("Topográfico") |>
        showGroup("Satélite")
      
    }
    
    
    # -----------------------------------------------------------------------
    # SI NO HAY DISTANCIAS VÁLIDAS
    # -----------------------------------------------------------------------
    
    if (length(dists) == 0) {
      
      proxy |>
        setView(
          lng = -77,
          lat = -12,
          zoom = 5
        )
      
      return()
    }
    
    
    # -----------------------------------------------------------------------
    # DATOS
    # -----------------------------------------------------------------------
    
    datos <- buffers_seleccionados() |>
      st_transform(4326)
    
    
    # -----------------------------------------------------------------------
    # PALETA
    # -----------------------------------------------------------------------
    
    pal <- paleta()
    
    
    # -----------------------------------------------------------------------
    # AGREGAR BUFFERS
    # -----------------------------------------------------------------------
    
    proxy |>
      addPolylines(
        data = datos,
        
        color = ~pal(distancia_mn),
        
        weight = 2,
        
        opacity = 0.9,
        
        label = ~paste0(
          distancia_mn,
          " mn"
        ),
        
        highlightOptions = highlightOptions(
          weight = 4,
          bringToFront = TRUE
        )
      ) |>
      
      # ---------------------------------------------------------------------
    # LEYENDA
    # ---------------------------------------------------------------------
    
    addLegend(
      position = "bottomright",
      
      pal = pal,
      
      values = datos$distancia_mn,
      
      title = "Distancia (mn)",
      
      opacity = 0.9
    )
    
    
    # -----------------------------------------------------------------------
    # AJUSTAR EXTENSIÓN DEL MAPA
    # -----------------------------------------------------------------------
    
    bb <- st_bbox(datos)
    
    dx <- (bb["xmax"] - bb["xmin"]) * 0.03
    dy <- (bb["ymax"] - bb["ymin"]) * 0.03
    
    proxy |>
      
      fitBounds(
        lng1 = bb["xmin"] - dx,
        lat1 = bb["ymin"] - dy,
        lng2 = bb["xmax"] + dx,
        lat2 = bb["ymax"] + dy
      )
    
  })
  
  
  # =========================================================================
  # DESCARGA
  # =========================================================================
  
  output$descargar <- downloadHandler(
    
    filename = function() {
      
      dists <- distancias_validas()
      
      paste0(
        "buffers_costa_",
        paste(dists, collapse = "_"),
        ".gpkg"
      )
    },
    
    content = function(file) {
      
      datos <- buffers_seleccionados()
      
      st_write(
        datos,
        file,
        layer = "buffers_costa",
        driver = "GPKG",
        delete_dsn = TRUE,
        quiet = TRUE
      )
    }
  )
  
}


# ---------------------------------------------------------------------------
# EJECUTAR
# ---------------------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)