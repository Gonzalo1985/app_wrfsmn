output$download <- downloadHandler(
  filename = "serie_temporal.csv",
  content = function(file) {
    write.csv(resp(), file, row.names = FALSE)
  }
)

output$var_output <- renderDataTable({
  req(resp())
  t(resp())
})