resp_ith_srs_2 <- eventReactive(input$ith_srs, {
  
  # waiter en pantalla
  waiter::Waiter$new(html = spin_square_circle(), id = "plot_new_ith")$show()
  
  out.plot <- ith.new.product(ith = resp.2(), station = input$estaciones)
  
  png(file = paste0("./www/series_aux.png"), width = 1000, height = 450)
  print(out.plot)
  dev.off()
})


output$plot_new_ith <- renderImage({
  
  req(resp_ith_srs_2())
  
  # waiter en pantalla
  waiter::Waiter$new(html = spin_square_circle(), id = "plot_new_ith")$show()
  
  filename <-  normalizePath(file.path("./www/series_aux.png"))
  
  list(src = filename, alt = paste0("Image number ", input$n))
  
}, deleteFile = FALSE)