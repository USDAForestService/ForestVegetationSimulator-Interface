library(shiny)

shinyUI(fluidPage(
  verticalLayout(
    h2("Forest Vegetation Simulator (FVS-Online)"),
    p("Set up an FVSOnline project by filling out this form, including the ",
      "selection the FVS variants you might use. You will be sent an Email ",
      "with a link to this project.  Note that your project(s) will be ",
      "removed from this server 60 days after the last access."),
    p("Check your Email spam files if you don't get the Email promptly."),
    tags$style(type="text/css", "#title { width: 500px; }"),
    textInput("title", "Your new project title"),
    textInput("emailnew", "Your Email address"),
    uiOutput("uivariants"),
    actionButton("submitnew","Submit"),
    hr(),
    p("Get a list of existing projects"),
    textInput("emailexist", "Your Email address", ""),
    actionButton("submitexist","Submit"),
    hr()
  )
))

