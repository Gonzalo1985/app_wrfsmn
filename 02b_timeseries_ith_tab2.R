resp_ith_srs <- eventReactive(input$ith_srs, {
  
  # waiter en pantalla
  waiter::Waiter$new(html = spin_square_circle(), id = "plot_ith")$show()
  
  if(input$estaciones == "Sunchales") {lon.s <- -61.53258043 ; lat.s <- -30.95685965}
  if(input$estaciones == "Reconquista") {lon.s <- -59.69317393 ; lat.s <- -29.20502553}
  if(input$estaciones == "Ceres") {lon.s <- -61.93601186 ; lat.s <- -29.87538747}
  
  ith.point <- find.nearest.point(data.matrix = resp.2()[[3]],
                                  data.lon = resp.2()[[1]],
                                  data.lat = resp.2()[[2]],
                                  lon = lon.s,
                                  lat = lat.s)
  
  date.srs <- paste0(seq(strptime(as.character(paste0(input$fecha, "T",
                                                      input$ciclo)),
                                  format = "%Y-%m-%dT%H"),
                         by = "hour",
                         length.out = length(ith.point) - 2))
  
  ith.series <- data.frame(Date = as.POSIXct(date.srs),
                           Data = t(ith.point[3:length(ith.point)]))
  
  
  if(input$estaciones == "Sunchales")
  {ith.obs <- ith.obs.series(id.station = 87356,
                             initial.day = "2022-01-01",
                             final.day = "2025-01-31",
                             analysis.date = paste0(input$fecha, input$ciclo, ":00:00"))}
  if(input$estaciones == "Reconquista")
  {ith.obs <- ith.obs.series(id.station = 87270,
                             initial.day = "2022-01-01",
                             final.day = "2025-01-31",
                             analysis.date = paste0(input$fecha, input$ciclo, ":00:00"))}
  if(input$estaciones == "Ceres")
  {ith.obs <- ith.obs.series(id.station = 87257,
                             initial.day = "2022-01-01",
                             final.day = "2025-01-31",
                             analysis.date = paste0(input$fecha, input$ciclo, ":00:00"))}
  
  ith.series <- merge(ith.series, ith.obs, by = "Date")
  colnames(ith.series) <- c("Date", "Mod", "Obs")
  ith.series
})



output$plot_ith <- renderDygraph({
  
  # waiter en pantalla
  waiter::Waiter$new(html = spin_square_circle(), id = "plot_ith")$show()
  
  data.xts <- xts(x = resp_ith_srs()[,c("Mod", "Obs")], order.by = resp_ith_srs()$Date)
  
  fig <- dygraph(data.xts) %>%
    dyOptions(pointSize = 4, labelsUTC = TRUE, fillGraph = TRUE, drawGrid = TRUE,
              colors = "#D8AE5A") %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 2.5, hideOnMouseOut = FALSE)
  
  print(fig)
})