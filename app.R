library("aws.s3")
library("ncdf4")
library("sp")
library("sf")
library("dplyr")
library("shiny")
library("shinyWidgets")
library("shinydashboard")
library("waiter")
library("leaflet")
library("rlist")
#library("graphics")
#library("fields")
library("raster")
library("RColorBrewer")
#library("purrr")
library("ggplot2")
library("png")
library("patchwork")
library("DT") # librería para las fcs: dataTableOutput() y renderDataTable()
library("tidyr")
library("xts")
#library("dygraphs")
library("viridis")
library("terra")
library("lubridate")
library("plotly")


# ------------------------------------------------------------------------------
# Funciones utilizadas en la app
source("./00_fct.R")
# ------------------------------------------------------------------------------

# Configuracion de UI
ui <- dashboardPage(skin = "green",
  
  dashboardHeader(title = "Productos de las salidas SMN Hi-Res Weather Forecast AWS",
                  titleWidth = 600),
  
  dashboardSidebar(
    sidebarMenu(
      
      use_waiter(),
      
      conditionalPanel(condition = "input.tabs1==1",
                       
                       div(tags$img(src = "logoSMN-75x80.png", width = "75px"), "Servicio Meteorológico Nacional"),
                       
                       dateInput("fecha", "Seleccione una fecha de pronóstico",
                                 value = Sys.Date() - 1,
                                 min = "2022-01-01",
                                 max = Sys.Date()),
                       
                       selectInput("ciclo",
                                   label = "Selección del ciclo de pronóstico",
                                   choices = c("00", "06", "12", "18"),
                                   selected = "00"),
                       
                       selectInput("time",
                                   label = "Selección del dataset (plazo de pronóstico)",
                                   choices = c("24H", "01H"),
                                   selected = "24H"),
                       
                       actionButton("descarga", "DESCARGA DE DATOS",
                                    class = "btn-lg btn-success", icon = icon("sync")),
                       
                       uiOutput("variable"), 
                      
                       textInput("lon", label = "Selección de longitud"),
                       
                       textInput("lat", label = "Selección de latitud"),
                       
                       actionButton("inicio", "Grafica Información Puntual",
                                    class = "btn-lg btn-success")),
      

      conditionalPanel(condition = "input.tabs1==2",
                       
                       uiOutput("tabSelection"))

      ), width = 350
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .box.collapsed-box {
          background-color: transparent !important;
          border: none !important;
        }
        .box.collapsed-box .box-body {
          display: none;
        }
      "))
    ),
    
    fluidPage(
    tabsetPanel(id="tabs1",
      tabPanel("Información puntual", value = 1,
               box(title = "La evolución temporal de pronóstico de la variable es:",
                   collapsible = TRUE, width = 6,
                   plotOutput("plot_output", height = "600px")),
               box(title = "Mapa:",
                   collapsible = TRUE, width = 6,
                   leafletOutput("leaflet_output", height = "600px")),
               box(width = 2, downloadButton("download", "Download.csv")),
               box(title = "Los datos cercanos al punto seleccionado son:",
                   collapsible = TRUE, width = 10,
                   dataTableOutput("var_output"))
               ),
      tabPanel("Productos ITH WRF-DET", value = 2,
               box(title = "Mapas de ITH WRF-DET",
                   collapsible = TRUE, width = 6, height = 1000,
                   imageOutput("image_ith")),
               box(title = "Series Temporales de ITH WRF-DET",
                   collapsible = TRUE, width = 6, height = 500,
                   plotlyOutput("plot_ith"))
               ),
      tabPanel("Productos ITH WRF-ENS (desactualizado)", value = 2,
               box(title = "Mapas de probabilidad de ITH WRF-ENS",
                   collapsible = TRUE, width = 10, height = 500,
                   imageOutput("image_ith_ens")),
               box(title = "Series Temporales de ITH WRF-ENS",
                   collapsible = TRUE, width = 8, height = 800,
                   imageOutput("plot_ith_ens"))
      )
      )
    )
    )
  )



server <- function(input, output, session) {

  
  # el diálogo a mostrar
  query_modal <- modalDialog(
    title = "Mensaje importante",
    HTML("Para visualizar productos de variables meteorológicas o de ITH primero
    hay que hacer una descarga de datos. Para la visualización de productos ITH
    solo se encuentran disponibles al descargar el dataset de 01H. Esto puede tomar un tiempo...<br><br>
    La información del modelo WRF obtenida en este sitio es obtenida de servicios AWS: <br> https://registry.opendata.aws/smn-ar-wrf-dataset/ <br><br>
    La documentación para la explotación de esta información se encuentra en: <br> https://odp-aws-smn.github.io/documentation_wrf_det/ <br><br>
    Cualquier comentario o reporte de error comunicarse a: gdiaz@smn.gob.ar"),
    footer = tagList(actionButton("close", "Cerrar")),
    easyClose = FALSE)
  
  # Muestra el diálogo al iniciar la app...
  showModal(query_modal)
  
  # Quitar diálogo
  observeEvent(input$close, {
    removeModal()
    })
  
  # abre archivos estáticos
  sa <- read_sf("./shp_SA/", "SA")
  logo <- readPNG("./www/logoSMN-75x80.png", native = TRUE)
  
  # UI según selección de dataset
  output$variable <- renderUI({
    
    if (input$time == "01H")
    {opts <- c("PP", "HR2", "T2", "dirViento10", "magViento10", "PSFC",
               "TSLB", "SMOIS")}
    
    if (input$time == "24H")
    {opts <- c("Tmax", "Tmin")}
    
    selectInput("variable", "Seleccione la variable del dataset:", choices = opts)
  })
  
  # UI del 2do TAB
  output$tabSelection <- renderUI({
    
    sidebarMenu(
      selectInput("time_ith", label = "Selección de hora de pronóstico WRF-ITH",
                  choices = paste0("(", sprintf("%02d", seq(1, 72, 1)), ")", " ",
                                   seq(strptime(as.character(paste0(input$fecha, "T", input$ciclo)),
                                                format = "%Y-%m-%dT%H"),
                                       by = "hour",
                                       length.out = 72))),
      
      actionButton("ith_map", "Grafica Mapa ITH-WRF-DET", class = "btn-lg btn-success"),
      
      selectInput("estaciones", "Elija estación", choices = c("Sunchales", "Reconquista", "Ceres")),
    
      actionButton("ith_srs", "Grafica Serie ITH-WRF-DET", class = "btn-lg btn-success")
    )
    
  })
  
  
  
  
  # definición de variables de funciones
  var.1 <- eventReactive(input$inicio, {
    input$variable
   })
  var.2 <- eventReactive(input$inicio, {
    as.numeric(input$lon)
  })
  var.3 <- eventReactive(input$inicio, {
    as.numeric(input$lat)
  })
  
  
  observeEvent(input$descarga, {
    
    # elimina los archivos netcdf descargados
    unlink(Sys.glob("*.nc"))
    
    showNotification("La descarga de archivos se encuentra en proceso", duration = 7)
       
    get.wrf.files(anual = format(input$fecha, "%Y"),
                  mes =format(input$fecha, "%m"),
                  dia = format(input$fecha, "%d"),
                  ciclo = input$ciclo,
                  time = input$time)
    
    showNotification("¡Listo!", duration = NULL)
  })
  
  
  resp0 <- eventReactive(input$inicio, {
    
    # waiter en pantalla
    waiter::Waiter$new(html = spin_square_circle(), id = "plot_output")$show()
    
    folder.data <- "./"
    
    var.netcdf <- load.netcdf.terra(nc.filenames = Sys.glob(paste0(folder.data, "*",
                                                            format(input$fecha, "%Y"),
                                                            format(input$fecha, "%m"),
                                                            format(input$fecha, "%d"), "_*")),
                              variable = var.1())
    
    
    var.netcdf
  })
  
  
  resp <- eventReactive(input$inicio, {
    np.netcdf <- find.nearest.point(data.matrix = resp0()[[1]],
                                    data.lon = resp0()[[2]],
                                    data.lat = resp0()[[3]],
                                    lon = var.2(),
                                    lat = var.3())
    np.netcdf
  })
  
  
  
  # ----------------------------------------------------------------------------
  # OUTPUTS
  
  # OUTPUT TAB 1
  # OUTPUT PLOT METEOROLOGICAL VARIABLE
  source(file = "./01a_plot_tab1.R", local = TRUE)
  
  # OUTPUT LEAFLET LOCATION
  source(file = "./01b_leaflet_tab1.R", local = TRUE)
  
  # OUTPUT DATA TABLE METEOROLOGICAL VARIABLE
  source(file = "./01c_table_tab1.R", local = TRUE)
  # ----------------------------------------------------------------------------
  
  # OUTPUT TAB 2
  # OUTPUT MAP IMAGE ITH
  source(file = "./02a_map_ith_tab2.R", local = TRUE)
  
  # OUTPUT TIME SERIES ITH
  source(file = "./02b_timeseries_ith_tab2.R", local = TRUE)
  # ----------------------------------------------------------------------------
  
  # OUTPUT TAB 3
  # OUTPUT PROBABILITIES MAPS IMAGES & TIMESERIES ITH
  source(file = "./03_map_timeseries_ith_tab3.R", local = TRUE)
  # ----------------------------------------------------------------------------

}

shinyApp(ui = ui, server = server)