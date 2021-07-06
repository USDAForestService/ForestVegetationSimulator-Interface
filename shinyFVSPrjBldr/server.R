library(shiny)

options(shiny.trace = F)  # change to T for trace


shinyServer(function(input, output, session) {
  
  source("prjListEmail.R")
  source("uuidgen.R")
  trim <- function (x) gsub("^\\s+|\\s+$","",x)
# pop the sink stack
  while(sink.number()) sink()
  try(sink("FVSPrjBldr.log"))
  cat (date(),"\n")

  observe({
    if (input$submitnew == 0) return()
    isolate({
      if (nchar(input$title)==0) return()
      emailnew = trim(input$emailnew)
      if (nchar(emailnew)<5 && regexpr("@",emailnew) < 2) return()
      uuid = uuidgen()
      workDir = paste0("/home/shiny/FVSwork/",uuid)
     cat("workDir=",workDir,"\n")
     dir.create(workDir)
     setwd(workDir)
     cat ('library(fvsOL)\nfvsOL(fvsBin="../../FVS/bin")\n',file="app.R")

      # projectId file...
cat("email=",emailnew,"\ntitle=",input$title,"\n")
      cat(file=paste0(workDir,"/projectId.txt"),
          "email=",emailnew,"\ntitle=",input$title,"\n")
      rptFile = tempfile()
      con = file(rptFile,"w")
      link = paste0("https://charcoal2.cnre.vt.edu/FVSwork/",uuid)
      cat (file=con,"Here is a link to the project named:",input$title,"\n\n")
      cat (file=con,link,"\n\n")
      cat (file=con,"Note that this project may be removed",
           "from the system 2 months after the last access.")
      close(con)

      mailCmd = paste('mailx -a "From: FVSOnline"',
       '-a "Subject: New project: ',input$title,'"',
       '-a "Reply-To: Nicholas Crookston <ncrookston.fs@gmail.com>"',
       '-a  --',emailnew,'<',rptFile)      
cat("mailCmd=",mailCmd,"\n")
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


