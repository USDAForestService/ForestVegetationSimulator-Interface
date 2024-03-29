library(shiny)

options(shiny.trace = F)  # change to T for trace


shinyServer(function(input, output, session) {

  trim <- function (x) if (is.null(x)) "" else gsub("^\\s+|\\s+$","",x)
# pop the sink stack
  while(sink.number()) sink()
  try(sink("FVSPrjBldr.log"))
  cat (date(),"\n")
  cat ("cur dir=",getwd(),"\n")
  source("prjListEmail.R")
  source("uuidgen.R")
  observe({
    if (input$submitnew == 0) return()
    isolate({
      if (nchar(input$title)==0) return()
      emailnew = trim(input$emailnew)
      emaildup = trim(input$emaildup)
      if ((nchar(emailnew)<5 && regexpr("@",emailnew) < 2) || emailnew != emaildup)
      {
        msg = "Email entry error."
        output$actionMsg = renderText(msg)
        return()
      }
      uuid = uuidgen()
      workDir = paste0("/home/shiny/FVSwork/",uuid)
      cat("workDir=",workDir,"\n")
      dir.create(workDir)
#      if (input$version == "production") 
        cat ('library(fvsOL)\nfvsOL(fvsBin="/home/shiny/FVS/bin")\n',file=paste0(workDir,"/app.R"))
#      if (input$version == "development") 
#        cat ('library(fvsOLdev)\nfvsOL(fvsBin="/home/shiny/FVSdev/bin")\n',file=paste0(workDir,"/app.R"))
      # projectId file...
cat("email=",emailnew,"\ntitle=",input$title,"\n")
      cat(file=paste0(workDir,"/projectId.txt"),
          "email=",emailnew,"\ntitle=",input$title,"\n")
      rptFile = tempfile()
      con = file(rptFile,"w")
      link = paste0("https://charcoal2.cnre.vt.edu/FVSwork/",uuid)
      cat (file=con,"To:",emailnew,"\n")
      cat (file=con,"Subject: New FVSOnline project at Virginia Tech\n")
      cat (file=con,"\nHere is a link to the project named: ",input$title,"\n\n")
      cat (file=con,link,"\n\n")
      if (input$version == "development") 
        cat (file=con,"This project uses development versions of the FVS software\n")
      cat (file=con,"Note that this project may be removed",
           "from the system 60 days after the last access.")
      close(con)

      mailCmd = paste('ssmtp -t < ',rptFile)

      system (mailCmd)
      if (nchar(input$title)) 
      {
        msg = paste0('Project: "',input$title,'" created.')
        output$actionMsg = renderText(msg)
      }
      updateTextInput(session=session, inputId="title", value="")
      Sys.sleep(.3)
      unlink(rptFile)
    })
  })

  observe({
    if (input$submitexist==0) return()
    isolate({
      emailexist=trim(input$emailexist)
      if (nchar(emailexist)<5 && regexpr("@",emailexist) < 2) return()
      prjListEmail(emailexist,sendEmail=TRUE)
      updateTextInput(session=session, inputId="emailexist", value="")
    })
  })

})



