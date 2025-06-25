
#pepe <- s3read_using(FUN = rast, object = wrf.names$Key[4], bucket = "s3://smn-ar-wrf/")


# función wrf.download para descarga de archivos netcdf de AWS
wrf.download <- function(wrf.name = wrf.name){
  save_object(
    object = paste0(wrf.name),
    bucket = "s3://smn-ar-wrf/",
    region = "us-west-2",
    file = substring(wrf.name, 28),
    overwrite = TRUE)
}



# función que obtiene las corridas del WRF de AWS (utiliza función wrf.download)
get.wrf.files <- function(anual = anual, mes = mes, dia = dia, ciclo = ciclo, time = time){
  # nombres de los archivos del Bucket a descargar
  wrf.names <- get_bucket_df(
    bucket = "s3://smn-ar-wrf/",
    prefix = paste0("DATA/WRF/DET/", anual, "/", mes, "/", dia, "/", ciclo),
    max = Inf,
    region = "us-west-2")
  
  # se queda solo con el intervalo indicado 01H o 24H
  wrf.names.rows <- which(grepl(time, wrf.names$Key, fixed = TRUE) == TRUE)
  wrf.names <- wrf.names[wrf.names.rows, ]
  
  # ejecución de la función wrf.download
  #lapply(wrf.names$Key, FUN = wrf.download)
  withProgress(message = 'Descargando datos...', detail = "Comienzo", value = 0, {
    
    for (i in 1:length(wrf.names$Key))
      {
       wrf.download(wrf.names$Key[i])
       incProgress(1/length(wrf.names$Key),
                   detail = paste("archivo", i, "de", length(wrf.names$Key)))
      }
  })
}



# se crea función load.netcdf para carga de archivos netcdf de AWS en environ R
load.netcdf <- function(nc.filenames = nc.filenames, variable = variable){
  nc.aux <- lapply(nc.filenames, FUN = nc_open, verbose = TRUE)
  var.lon <- ncvar_get(nc.aux[[1]], "lon")
  var.lat <- ncvar_get(nc.aux[[1]], "lat")
  var.time <- ncvar_get(nc.aux[[1]], "time")
  
  if (variable == "Tmin") {loop <- seq(from = 2, to = length(nc.aux), by = 1)}
  if (variable == "Tmax") {loop <- seq(from = 1, to = length(nc.aux) - 1, by = 1)}
  if (variable != "Tmax" & variable != "Tmin") {loop <- seq(from = 1, to = length(nc.aux), by = 1)}
  
  var.array <- list()
  for (i in loop){
    var.array[[i]] <- ncvar_get(nc.aux[[i]], variable) # guarda los datos en un array de 3 dimensiones
    nc_close(nc.aux[[i]])
  }
  
  # elimina elemento de lista con NULL
  var.array[sapply(var.array, is.null)] <- NULL
  
  # retorna salida
  return(list(var.array, var.lon, var.lat))
}



# se crea función load.netcdf para carga de archivos netcdf de AWS en environ R
load.netcdf.terra <- function(nc.filenames = nc.filenames, variable = variable){
  terra.aux <- rast(nc.filenames)
  
  position.variable <- which(varnames(terra.aux) == variable)
  terra.aux <- terra.aux[[position.variable]]
  
  terra.aux.4 <- c()
  for (i in 1:nlyr(terra.aux))
    {
     terra.aux.2 <- project(terra.aux[[i]], "+proj=longlat +datum=WGS84", method = "bilinear")
     terra.aux.3 <- crop(terra.aux.2, extent(-75, -53, -55.5, -21))
     terra.aux.4 <- c(terra.aux.4, terra.aux.3)
    }
  
  
  matrix.aux <- list()
  for (i in 1:nlyr(terra.aux))
    {
     matrix.aux[[i]] <- matrix(data = terra.aux.4[[i]],
                               nrow = nrow(terra.aux.4[[i]]),
                               ncol = ncol(terra.aux.4[[i]]))
     
    }
  
  COORDS <- crds(terra.aux.4[[1]])
  
  LONs <- sort(unique(COORDS[, 1])) ; LATs <- sort(unique(COORDS[, 2]))
  print(length(LATs))
  matrix.LONs <- matrix(LONs, nrow = length(LONs), ncol = length(LATs))
  matrix.LATs <- matrix(LATs, nrow = length(LATs), ncol = length(LONs))
  
  #return(terra.aux)
  return(list(matrix.aux, matrix.LONs, matrix.LATs))
}



# se crea función wrf.txt.from.aws para lectura de archivos y busqueda de punto más cercano
#wrf.txt.from.aws <- function(anual = anual, mes = mes, dia = dia, ciclo = ciclo,
#                             time = time, var = var, lon = lon, lat = lat){
find.nearest.point <- function(data.matrix = data.matrix,
                               data.lon = data.lon, data.lat = data.lat,
                               lon = lon, lat = lat){
  
  # creado de data.nc.array con ubicaciones de la grilla
  data.nc.array <- cbind(array(data.lon),
                         array(data.lat))
  
  # agregado por columna al data.nc.array de cada uno de los datos por tiempo
  for (i in 1:length(data.matrix))
    {data.nc.array <- cbind(data.nc.array, array(data.matrix[[i]]))}
  
  # identifica el punto más cercano a los puntos de interés y prepara data.to.write
  data.to.write <- c()
  distances.to.point <- spDists(data.nc.array[, c(1,2)],
                                as.matrix(cbind(lon, lat)),
                                longlat = FALSE)
  nearest.point.row <- which(distances.to.point == min(distances.to.point))
  data.to.write <- rbind(data.to.write, data.nc.array[nearest.point.row,])
  
  # transforma a data frame e identificación de columnas
  data.to.write <- data.to.write %>%
    as.data.frame() %>%
    `colnames<-`(c("lon.cercano", "lat.cercano", seq(1, ncol(data.to.write)-2, 1)))

  return(data.to.write)
}



# se crea función draw.map para graficado de punto en mapa leaflet
draw.map <- function(x = x, y = y){
  
  df <- data.frame(lon = x, lat = y)
  df <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, agr = "constant")
  
  mapa <- leaflet(data = NULL, width = 1300, height = 1600, options = leafletOptions(preferCanvas = TRUE)) %>%
    
    addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG%3A3857@png/{z}/{x}/{-y}.png",
             attribution = "Servicio Meteorológico Nacional", group = "Argenmap IGN") %>%
    
    addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_topo@EPSG%3A3857@png/{z}/{x}/{-y}.png",
             attribution = "Servicio Meteorológico Nacional", group = "Argenmap Topo IGN") %>%
    
    addTiles(urlTemplate = "//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}",
             attribution = "Servicio Meteorológico Nacional", group = "OSM (default)") %>%
  
    addCircles(data = df, weight = 5, group = "Punto de interés") %>%
    
    addLayersControl(baseGroups = c("Argenmap IGN", "Argenmap Topo IGN", "OSM (default)"), 
                     overlayGroups = c("Punto de interés"), 
                     options = layersControlOptions(collapsed = FALSE)) %>%
  
    setView(lat = y[1], lng = x[1], zoom = 8)
  
  return(mapa)
}



# se crea función ith.wrf.det para cálculo del índice
ith.wrf.det <- function(path.data = path.data, anual = anual, mes = mes, dia = dia, ciclo = ciclo){
  wrfs.HR <- load.netcdf(Sys.glob(paste0(path.data, "/*", anual, mes, dia, "_", ciclo, "*.nc")),
                         variable = "HR2")
  wrfs.T2 <- load.netcdf(Sys.glob(paste0(path.data, "/*", anual, mes, dia, "_", ciclo, "*.nc")),
                         variable = "T2")
  
  ith.horario <- list()
  for (t in 1:length(wrfs.HR[[1]]))
    {
     term.1 <- 1.8 * wrfs.T2[[1]][[t]]
     term.2 <- 32
     term.3 <- (0.55 - (0.55*wrfs.HR[[1]][[t]])/100)
     term.4 <- (1.8*wrfs.T2[[1]][[t]]) - 26
     ith.aux <- term.1 + term.2 - (term.3 * term.4)
     ith.horario <- list.append(ith.horario, ith.aux)
    }
  
  return(list(wrfs.T2[[2]], wrfs.T2[[3]], ith.horario))
}



# se crea función map.graph para graficado del índice ITH
map.graph <- function(long = long, lat = lat, z = z, shp = shp)
{
  xyz <- as.matrix(data.frame(x = array(long), y = array(lat), z = array(z)))
  #xyz <- data.frame(x = array(long), y = array(lat), z = array(z))
  
  # se especifica el CRS
  crs.latlon <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  #nrow <- 999
  #ncol <- 1249
  
  nrow <- 500
  ncol <- 625
  
  # se define el extent del dominio
  ext <- extent(xyz[, c("x", "y")])
  
  # se crea raster con extent definido por nrow y ncol
  rast <- raster(ext = ext, nrow = nrow, ncol = ncol, crs = crs.latlon)
  
  # rasterize de los datos
  rast <- rasterize(xyz[, c("x", "y")], rast,
                    xyz[, "z"],
                    fun = mean)
  
  
  test_spdf <- as(rast, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  colnames(test_df) <- c("value", "lon", "lat")
  
  clrs <- c("#228B22", "#00A433", "#00FF7F", "#99E64D", "#CCF2A6", "#FFFFE0",
            "#F6BE00", "#FFA500", "#DE591C", "#FF0000", "#F62217", "#800000")
  
  brks <- c(35, 45, 55, 60, 65, 70, 72, 74, 78, 82, 88, 95)

  lbls <- c("45", "49", "53", "57", "61", "65", "69", "73", "77", "81", "85",
            "89", "93")
  
  fig <- ggplot() +
    geom_tile(data = test_df, aes(x = lon, y = lat, fill = value), alpha = 0.9) + 
    geom_sf(data = shp, fill = NA, color = "grey50", linewidth = 0.25) +
    #geom_contour(data = test_df, aes(x = lon, y = lat, z = value, colour = "isoline 72")) +
    #metR::geom_text_contour(data = test_df, aes(x = lon, y = lat, z = value, colour = "isoline 72")) +
    scale_fill_viridis() +
    coord_equal() +
    coord_sf(xlim = c(-78, -52), ylim = c(-55, -20)) +
    scale_fill_fermenter(breaks = brks,
                         palette = "RdYlGn",
                         limits = c(35, 95),
                         name = paste("Escala ITH"),
                         na.value = NA,
                         guide = guide_coloursteps(even.steps = TRUE,
                                                   show.limits = TRUE,
                                                   barwidth = 2,
                                                   barheight = 20,
                                                   title.position = "top",
                                                   title.vjust = 1, 
                                                   title.hjust = 0.5))
  
  return(fig)
}


# Armado de serie de índice ITH de las observaciones
ith.obs.series <- function(id.station = id.station,
                           initial.day = initial.day,
                           final.day = final.day,
                           analysis.date = analysis.date){
   
   period <- seq.Date(as.Date(initial.day), as.Date(final.day), 1)
   n.months <- length(seq(as.Date(initial.day), as.Date(final.day), by = "1 month"))
   n.years <- unique(year(period))
   
   str <- c()
   for (i in 1:length(n.years)){
     for (j in 1:12){
     s <- paste0(n.years[i], "-",
                 sprintf("%02d", j), "-",
                 sprintf("%02d", seq(1,31,1)))
     str <- c(str, s)
     }
   }
   str <- str[1:which(str == final.day)]
  
   #n.times <- 31 * n.months # 837
   
   ith09 <- lapply(Sys.glob("./OBSith/09z/ith09-*"), FUN = read.table, skip = 1)
   ith12 <- lapply(Sys.glob("./OBSith/12z/ith12-*"), FUN = read.table, skip = 1)
   ith15 <- lapply(Sys.glob("./OBSith/15z/ith15-*"), FUN = read.table, skip = 1)
   ith18 <- lapply(Sys.glob("./OBSith/18z/ith18-*"), FUN = read.table, skip = 1)
   
   pos09 <- lapply(1:n.months, function(i) {which(ith09[[i]]$V2 == id.station)})
   pos12 <- lapply(1:n.months, function(i) {which(ith12[[i]]$V2 == id.station)})
   pos15 <- lapply(1:n.months, function(i) {which(ith15[[i]]$V2 == id.station)})
   pos18 <- lapply(1:n.months, function(i) {which(ith18[[i]]$V2 == id.station)})
   
   ith09 <- lapply(1:n.months, function(i) {ith09[[i]][pos09[[i]],]})
   ith12 <- lapply(1:n.months, function(i) {ith12[[i]][pos12[[i]],]})
   ith15 <- lapply(1:n.months, function(i) {ith15[[i]][pos15[[i]],]})
   ith18 <- lapply(1:n.months, function(i) {ith18[[i]][pos18[[i]],]})
   
   ith09 <- lapply(1:n.months, function(i) {ith09[[i]][,-c(1, 2, 3, 4)]})
   ith12 <- lapply(1:n.months, function(i) {ith12[[i]][,-c(1, 2, 3, 4)]})
   ith15 <- lapply(1:n.months, function(i) {ith15[[i]][,-c(1, 2, 3, 4)]})
   ith18 <- lapply(1:n.months, function(i) {ith18[[i]][,-c(1, 2, 3, 4)]})
   
   ith.station <- cbind(unlist(ith09), unlist(ith12), unlist(ith15), unlist(ith18))

   ith.station.data <- data.frame(ndays = str, ith.station = ith.station)
   
   # crea vector de fechas a eliminar extras que vienen en tablas de datos ITH
   na <- lapply(1:length(n.years), function(i)
     {if(leap_year(n.years[i]) == TRUE)
       {c(paste0(n.years[i], "-", sprintf("%02d", 2), "-", c(30, 31)),
          paste0(n.years[i], "-", sprintf("%02d", 4), "-", 31),
          paste0(n.years[i], "-", sprintf("%02d", 6), "-", 31),
          paste0(n.years[i], "-", sprintf("%02d", 9), "-", 31),
          paste0(n.years[i], "-", 11, "-", 31))
       } else {c(paste0(n.years[i], "-", sprintf("%02d", 2), "-", c(29, 30, 31)),
                 paste0(n.years[i], "-", sprintf("%02d", 4), "-", 31),
                 paste0(n.years[i], "-", sprintf("%02d", 6), "-", 31),
                 paste0(n.years[i], "-", sprintf("%02d", 9), "-", 31),
                 paste0(n.years[i], "-", 11, "-", 31))}
     })
   
   # elimina los datos extras que vienen en las tablas de datos ITH
   ith.station.data <- ith.station.data[-which(ith.station.data$ndays %in% unlist(na)), ]
   
   ith.station.data <- array(t(ith.station.data[,-1]))
   
   d <- sort(rep(period, 4))
   h <- rep(c("09", "12", "15", "18"), length(period)) # en utc

   dh <- paste0(strptime(as.character(paste0(d, "T", h)),
                         format = "%Y-%m-%dT%H"))
   
   obs.ith <- data.frame(Date = dh, Data = ith.station.data)
   
   obs.ith.new <- obs.ith %>%
     mutate(Date = as.POSIXct(obs.ith$Date)) %>%
     complete(Date = seq.POSIXt(min(Date), max(Date), by="hour"))
   
   # faltantes/erroneos convertidos a NA
   obs.ith.new[obs.ith.new < 0] <- NA
   
   # solo los días de la corrida del ciclo
   pos.analysis <- which(obs.ith.new$Date == as.POSIXct(analysis.date))
   pos.analysis <- seq(pos.analysis, pos.analysis + 72, 1)
   
   obs.ith.new <- obs.ith.new[pos.analysis,]
   
   return(obs.ith.new)
}

