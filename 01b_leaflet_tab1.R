res_leaflet <- reactive({
  # waiter en pantalla
  mapa <- draw.map(x = var.2(), y = var.3())
  mapa
})


output$leaflet_output <- renderLeaflet({
  res_leaflet()
})