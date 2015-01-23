library(shiny)
library(RSQLite)

options(shiny.trace = F)  # change to T for reactive tracing (lots of output)

shinyServer(function(input, output, session) {

  observe(if (input$FVSOnline > 0) 
  {
cat ("returnToFVSOnline\n")
    file.copy("fvsOnlineServer.R", "server.R",overwrite=TRUE)
    file.copy("fvsOnlineUI.R",     "ui.R",    overwrite=TRUE)
    unlink("fvsOnlineServer.R")
    unlink("fvsOnlineUI.R")
    output$reload<-renderUI(tags$script("location.reload();"))
  })

})
