plot.params <- eventReactive(input$inicio, {
  
  if (input$time == "01H")
  {
    aux.Date <- as.POSIXct(paste0(input$fecha, input$ciclo), format = "%Y-%m-%d %H", tz = "UTC")
    str.Date <- seq(from = aux.Date, by = "3 hour", length.out = 24)
    by <- seq(from = 1, to = 72, by = 3)
  }
  
  if (input$time == "24H")
  {
    aux.Date <- as.POSIXct(paste0(input$fecha, input$ciclo), format = "%Y-%m-%d %H", tz = "UTC")
    str.Date <- seq(from = aux.Date, by = "24 hour", length.out = 3)
    by <- seq(from = 1, to = 3, by = 1)
  }
  
  list(str.Date = as.character(str.Date), by = by)
  
})




output$plot_output <- renderPlot({
  
  req(resp.1())
  
  # waiter en pantalla
  waiter::Waiter$new(html = spin_square_circle(), id = "plot_output")$show()
  
  par(mar = c(9, 4, 4, 2) + 0.1)
  
  plot(1:(length(resp.1())-2), resp.1()[3:length(resp.1())],
       type = "b", col = "red", xaxt = "n",
       xlab = "", ylab = var.1())
  
  axis(1,
       at = plot.params()$by,
       labels = plot.params()$str.Date,
       las = 2)
  
})