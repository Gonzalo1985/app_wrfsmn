output$download <- downloadHandler(
  filename = "serie_temporal.csv",
  content = function(file) {
    write.csv(resp.1(), file, row.names = FALSE)
  }
)

output$var_output <- renderDataTable({
  req(resp.1())
  t(resp.1())
})