resp_ith_srs <- eventReactive(input$ith_srs, {
  
  folder.data <- "./"
  
  ith.calc <- ith.wrf.det(path.data = folder.data,
                          anual = format(input$fecha, "%Y"),
                          mes = format(input$fecha, "%m"),
                          dia = format(input$fecha, "%d"),
                          ciclo = as.numeric(input$ciclo))
  
  if(input$estaciones == "Sunchales") {lon.s <- -61.53258043 ; lat.s <- -30.95685965}
  if(input$estaciones == "Reconquista") {lon.s <- -59.69317393 ; lat.s <- -29.20502553}
  if(input$estaciones == "Ceres") {lon.s <- -61.93601186 ; lat.s <- -29.87538747}
  
  ith.point <- find.nearest.point(data.matrix = ith.calc[[3]],
                                  data.lon = ith.calc[[1]],
                                  data.lat = ith.calc[[2]],
                                  lon = lon.s,
                                  lat = lat.s)
  
  
  date.srs <- paste0(seq(strptime(as.character(paste0(input$fecha, "T",
                                                      input$ciclo)),
                                  format = "%Y-%m-%dT%H"),
                         by = "hour",
                         length.out = length(ith.point) - 2))
  
  ith.series <- data.frame(Date = date.srs,
                           Data = t(ith.point[3:length(ith.point)]))
  
  
  colnames(ith.series) <- c("Date", "Mod")
  ith.series
})



output$plot_ith <- renderPlotly({
  
  # waiter en pantalla
  #waiter::Waiter$new(html = spin_square_circle(), id = "plot_ith")$show()
  # waiter en pantalla
  waiter::Waiter$new(html = spin_square_circle(), id = "plot_ith")$show()
  
  data.xts <- resp_ith_srs()
  #data.xts <- xts(x = resp_ith_srs()[,c("Mod")], order.by = resp_ith_srs()$Date)
  
  plot_ly(
    data = data.xts,
    x = ~Date,
    y = ~Mod,
    type = 'scatter',
    mode = 'lines+markers'
  ) %>%
    layout(
      title = "Serie Temporal",
      xaxis = list(title = "Fecha y Hora", range = c(min(data.xts$Date), max(data.xts$Date))),
      yaxis = list(title = "Valor")
    )
  
  #fig <- dygraph(data.xts) %>%
  #  dyOptions(pointSize = 4, labelsUTC = TRUE, fillGraph = TRUE, drawGrid = TRUE,
  #            colors = "#D8AE5A") %>%
  #  dyRangeSelector() %>%
  #  dyCrosshair(direction = "vertical") %>%
  #  dyHighlight(highlightCircleSize = 2.5, hideOnMouseOut = FALSE)
  
  #print(fig)
})