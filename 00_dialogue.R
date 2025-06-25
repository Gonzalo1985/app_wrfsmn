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