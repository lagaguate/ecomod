library(shiny)

#default channel is closed
reqChannel = -1

#function to attempt connection
connectAs <- function(DSN, user, pw) {
  require(RODBC)
  connection <- odbcConnect(DSN, user, pw)
  return(connection)
}

#function takes connection info to get data
get.data <- function(channel) {
  if (channel != -1) {
    data <- sqlQuery(channel,"Select * from dual;")
  }
  return(data)
}

shinyServer(function(input, output) {
  v <- reactiveValues(data = NULL)
  
  #whenever login button is clicked, store results and attempt connection
  observeEvent(input$loginButt, {
    v$reqChannel <- connectAs("PTRAN", input$user, input$pw)  
  })
  
  #if not connected, let user know
  output$conn<-renderText({
    if (is.null(v$reqChannel) || v$reqChannel==-1){
      data= "Not logged in"
    }
  })

  #populate table with results of get.data() function
  output$resultTable <- renderDataTable({
    if (is.null(v$reqChannel))return(NULL)
      data=get.data(v$reqChannel)
  })
})