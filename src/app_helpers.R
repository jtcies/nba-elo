# function to determine if display should be by season or by date range

input_options <- reactive({
  
  if(input$option == "date range") {
    
    dateRangeInput(
      "dates", "Date range",
      start = "1997-10-01",
      end = "2018-10-01",
      format = "mm/dd/yyyy"
    )  
  }
  
  if(input$option == "season") {
    
    numericInpuut(
      "season", "Season",
      value = 2018
      )
  }
})
  
  
  
  

