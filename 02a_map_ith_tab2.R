#resp_ith <- eventReactive(input$ith_map, {
  
#  folder.data <- "./"
  
#  ith.calc <- ith.wrf.det(path.data = folder.data,
#                          anual = format(input$fecha, "%Y"),
#                          mes = format(input$fecha, "%m"),
#                          dia = format(input$fecha, "%d"),
#                          ciclo = as.numeric(input$ciclo))
  
#})


output$image_ith <- renderImage({
  
  req(resp.2())
  
  # waiter en pantalla
  waiter::Waiter$new(html = spin_square_circle(), id = "image_ith")$show()
  
  png(file = paste0("./www/png_aux.png"), width = 850, height = 1000)
  fig <- map.graph(long = resp.2()[[1]],
                   lat = resp.2()[[2]],
                   z = resp.2()[[3]][[as.numeric(substr(input$time_ith, 2, 3))]],
                   shp = sa)
  
  fig <- fig + inset_element(p = logo,
                             left = 0.05, bottom = 0.87,
                             right = 0.20, top = 0.98)
  
  print(fig)
  dev.off()
  
  filename <-  normalizePath(file.path("./www/png_aux.png"))
  
  list(src = filename, alt = paste0("Image number ", input$n))
  
}, deleteFile = FALSE)