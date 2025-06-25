ith.new.product <- function(ith = ith, station = station){
  
  ith.calc <- ith
  
  if(station == "Sunchales")   {lon.s <- -61.53258043 ; lat.s <- -30.95685965}
  if(station == "Reconquista") {lon.s <- -59.69317393 ; lat.s <- -29.20502553}
  if(station == "Ceres")       {lon.s <- -61.93601186 ; lat.s <- -29.87538747}
  
  ith.point <- find.nearest.point(data.matrix = ith.calc[[3]],
                                  data.lon = ith.calc[[1]],
                                  data.lat = ith.calc[[2]],
                                  lon = lon.s,
                                  lat = lat.s)
  
  serie.ITH <- data.frame(Date.Time = unique(time(rast(list.files(pattern = ".nc")))),
                          ITH.WRF = t(ith.point[3:75]))
  # ------------------------------------------------------------------------------
  
  serie.ITH$Date.Time <- as.POSIXct(serie.ITH$Date.Time, tz = "GMT")
  
  
  serie.ITH <- serie.ITH %>%
    mutate(day = as.Date(Date.Time),  # Extraer el día
           hour = hour(Date.Time),  # Extraer la hora
           below.threshold.68 = ifelse(ITH.WRF < 68, 1, 0),
           above.threshold.68 = ifelse(ITH.WRF > 68 & ITH.WRF < 72, 1, 0),
           above.threshold.72 = ifelse(ITH.WRF > 72 & ITH.WRF < 82, 1, 0),
           above.threshold.82 = ifelse(ITH.WRF > 82, 1, 0))  # Indicador si está por encima del umbral
  
  serie.ITH <- serie.ITH %>%
    group_by(day) %>%
    mutate(cumulative.below.68 = cumsum(below.threshold.68),
           cumulative.above.68 = cumsum(above.threshold.68),
           cumulative.above.72 = cumsum(above.threshold.72),
           cumulative.above.82 = cumsum(above.threshold.82))  # Acumulación total por día
  
  # Dash lines para indicar comienzo del día
  lines.plot <- unique(serie.ITH$day)
  day.line <- as.POSIXct(lines.plot, tz = "GMT")
  
  # Condiciones para ubicación de elementos en gráfico según ciclo
  if (serie.ITH$hour[1] == 0) {
    serie.ITH <- serie.ITH[-73,]
    position.th.leg <- max(serie.ITH$Date.Time)
    position.leg <- position.th.leg - as.difftime(2, units = "hours")
    }
  
  if (serie.ITH$hour[1] == 6) {
    position.th.leg <- max(serie.ITH$Date.Time)
    position.leg <- position.th.leg - as.difftime(12, units = "hours")
  }
  
  if (serie.ITH$hour[1] == 12) {
    position.th.leg <- max(serie.ITH$Date.Time)
    position.leg <- position.th.leg - as.difftime(2, units = "hours")
  }
  
  if (serie.ITH$hour[1] == 18) {
    position.th.leg <- max(serie.ITH$Date.Time)
    position.leg <- position.th.leg - as.difftime(2, units = "hours")
  }
  
  
  # Obtener el valor máximo de la acumulación por día
  max.cumulative <- serie.ITH %>%
    group_by(day) %>%
    summarise(max.below.68 = max(cumulative.below.68),
              max.above.68 = max(cumulative.above.68),
              max.above.72 = max(cumulative.above.72),
              max.above.82 = max(cumulative.above.82))
  
  # Combinar la acumulación máxima con los datos originales
  serie.ITH <- serie.ITH %>%
    left_join(max.cumulative, by = "day")
  
  # Categoría según el valor que toma el índice
  serie.ITH$category <- ifelse(serie.ITH$ITH.WRF < 68, "#1ABC9C",
                               ifelse(serie.ITH$ITH.WRF < 72, "#FEF65B",
                                      ifelse(serie.ITH$ITH.WRF < 82, "#FF9932", "#FF5733")))
  
  serie.ITH$category <- factor(serie.ITH$category, 
                               levels = c("#1ABC9C", "#FEF65B", "#FF9932", "#FF5733"))
  

  circles.data <- serie.ITH %>%
    group_by(day) %>%
    summarise(max.below.68 = first(max.below.68),
              max.above.68 = first(max.above.68),
              max.above.72 = first(max.above.72),
              max.above.82 = first(max.above.82),
              time = first(Date.Time),
              time2 = first(Date.Time) + 43600)
  
  circles.data$size.category.below.68 <- cut(circles.data$max.below.68,
                                             breaks = c(0, 10, 15, 18, Inf),
                                             labels = c("00H - 10H", "10H - 15H", "15H - 18H", "18H - 24H"),
                                             right = FALSE)
  
  circles.data$size.category.68 <- cut(circles.data$max.above.68,
                                       breaks = c(0, 10, 15, 18, Inf),
                                       labels = c("00H - 10H", "10H - 15H", "15H - 18H", "18H - 24H"),
                                       right = FALSE)
  
  circles.data$size.category.72 <- cut(circles.data$max.above.72,
                                       breaks = c(0, 10, 15, 18, Inf),
                                       labels = c("00H - 10H", "10H - 15H", "15H - 18H", "18H - 24H"),
                                       right = FALSE)
  
  circles.data$size.category.82 <- cut(circles.data$max.above.82,
                                       breaks = c(0, 10, 15, 18, Inf),
                                       labels = c("00H - 10H", "10H - 15H", "15H - 18H", "18H - 24H"),
                                       right = FALSE)
  
  
  if (length(which(serie.ITH$category == "#1ABC9C")) == 0)
    
    {manual.colors <- c( "#FEF65B", "#FF9932", "#FF5733")} else
    
          {manual.colors <- c("#1ABC9C", "#FEF65B", "#FF9932", "#FF5733")}
 
  
  p <- ggplot() +
    
    # Puntos de serie.ITH con colores según la categoría
    geom_point(data = serie.ITH, aes(x = Date.Time, y = ITH.WRF, fill = category), show.legend = FALSE,
               shape = 21, size = 2.5, stroke = 0.8, color = "black") +
    scale_fill_manual(values = manual.colors, guide = "none") +
    scale_color_manual(values = c("black"), guide = "none") +
    
    # Líneas horizontales
    geom_hline(yintercept = 68, col = "gray") + annotate("text", x = position.th.leg, y = 69, label = "Umbral 68", size = 3, hjust = 1, color = "black") +
    geom_hline(yintercept = 72, col = "gray") + annotate("text", x = position.th.leg, y = 73, label = "Umbral 72", size = 3, hjust = 1, color = "black") +
    geom_hline(yintercept = 82, col = "gray") + annotate("text", x = position.th.leg, y = 83, label = "Umbral 82", size = 3, hjust = 1, color = "black") +
    
    geom_vline(xintercept = as.numeric(day.line), linetype = "dashed", color = "#341304") +
    
    # Puntos con tamaño categórico basado en las horas por debajo del umbral 68
    geom_point(data = circles.data,
               aes(x = time + 10800, y = 47, size = size.category.below.68),
               fill = "#1ABC9C", shape = 21, stroke = 1.0, show.legend = TRUE) +
    
    geom_text(data = circles.data,
              aes(x = time + 10800, y = 47, label = max.below.68), 
              vjust = 0.5, size = 3, color = "black", fontface = "bold") +
    
    geom_text(aes(x = position.leg, y = 47, label = "Hrs < a 68"), 
              vjust = 0.5, size = 3.5, color = "black") +
    
    # Puntos con tamaño categórico basado en las horas sobre el umbral 68
    geom_point(data = circles.data,
               aes(x = time + 10800, y = 50, size = size.category.68),
               fill = "#FEF65B", shape = 21, stroke = 1.0, show.legend = TRUE) +
    
    geom_text(data = circles.data,
              aes(x = time + 10800, y = 50, label = max.above.68), 
              vjust = 0.5, size = 3, color = "black", fontface = "bold") +
    
    geom_text(aes(x = position.leg, y = 50, label = "Hrs > a 68"), 
              vjust = 0.5, size = 3.5, color = "black") +
    
    
    # Puntos con tamaño categórico basado en las horas sobre el umbral 72
    geom_point(data = circles.data,
               aes(x = time + 10800, y = 53, size = size.category.72),
               fill = "#FF9932", shape = 21, stroke = 1.0, show.legend = TRUE) +
    
    geom_text(data = circles.data,
              aes(x = time + 10800, y = 53, label = max.above.72), 
              vjust = 0.5, size = 3, color = "black", fontface = "bold") +
    
    geom_text(aes(x = position.leg, y = 53, label = "Hrs > a 72"), 
              vjust = 0.5, size = 3.5, color = "black") +
    
    
    # Puntos con tamaño categórico basado en las horas sobre el umbral 82
    geom_point(data = circles.data,
               aes(x = time + 10800, y = 56, size = size.category.82),
               fill = "#FF5733", shape = 21, stroke = 1.0, show.legend = TRUE) +
    
    geom_text(data = circles.data,
              aes(x = time + 10800, y = 56, label = max.above.82), 
              vjust = 0.5, size = 3, color = "black", fontface = "bold") +
    
    geom_text(aes(x = position.leg, y = 56, label = "Hrs > a 82"), 
              vjust = 0.5, size = 3.5, color = "black") +
    
    
    # Escala de tamaño manual para las categorías
    scale_size_manual(values = c("00H - 10H" = 7,
                                 "10H - 15H" = 9,
                                 "15H - 18H" = 11,
                                 "18H - 24H" = 13),
                      name = "Horas por día superior a umbral",
                      limits = c("00H - 10H", "10H - 15H", "15H - 18H", "18H - 24H")) +
    
    labs(x = "Fecha", y = "Valor de ITH") +
    
    theme(
      axis.title.x = element_text(size = 14),             # Tamaño del label del eje X
      axis.title.y = element_text(size = 14),             # Tamaño del label del eje Y
      axis.text.x = element_text(size = 12,               # Tamaño del texto de las marcas del eje X
                                 angle = 90, hjust = 1),
      axis.text.y = element_text(size = 12),              # Tamaño del texto de las marcas del eje Y
      legend.title = element_text(size = 12),             # Tamaño del título de la leyenda
      legend.text = element_text(size = 10)               # Tamaño del texto de la leyenda
    ) +
    
    guides(size = guide_legend(override.aes = list(fill = NA, color = "black"))) +
    
    scale_x_datetime(
      breaks = seq(min(serie.ITH$Date.Time), max(serie.ITH$Date.Time), by = "6 hours"), 
      labels = scales::date_format("%Y-%m-%d %H:%M")
    )
    
  return(p)
  
}

