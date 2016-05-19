library(shiny)

shinyUI(fluidPage(

  titlePanel("PTRAN Login Example"),
  
  sidebarLayout(
    sidebarPanel(id="menu1", width=3,
      "Login",
      textInput("user", label = "Username", value = oracle.personal.username),
      passwordInput("pw", label = "Password", value = oracle.personal.password),
      actionButton("loginButt", label = "Login"),
      textOutput("conn")
    ),
    mainPanel(
      dataTableOutput("resultTable")
    )
  )
))