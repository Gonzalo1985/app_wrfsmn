output$image_ith_ens <- renderImage({
  filename <-  normalizePath(file.path("./www/mean_prob_ITH.png"))
  
  list(src = filename, alt = paste0("Image number ", input$n))
  
}, deleteFile = FALSE)

output$plot_ith_ens <- renderImage({
  filename <-  normalizePath(file.path("./www/serieITH_Sunchales.png"))
  
  list(src = filename, alt = paste0("Image number ", input$n))
  
}, deleteFile = FALSE)