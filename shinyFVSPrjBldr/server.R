jlibrary(shiny)

options(shiny.trace = F)  # change to T for trace


shinyServer(function(input, output, session) {
  
  source("prjListEmail.R")
  source("uuidgen.R")
  source("settings.R")
  avail = gsub("[.]so$","",dir (fvsBinDir,pattern="[.]so$"))
  keep = intersect(avail,names(pgmList))
  topick = as.list(keep)
  names(topick) = unlist(pgmList[keep])
  output$uivariants <- renderUI(
    checkboxGroupInput("uivariants","Select FVS variants", 
              choices=topick,selected="FVSie",inline=TRUE))

  observe({
    if (input$submitnew == 0) return()
    isolate({
      if (is.null(input$uivariants)) return()
      if (nchar(input$title)==0) return()
      if (nchar(input$emailnew)<5 && regexpr("@",input$emailnew) < 2) return()
      uuid = uuidgen()
      workDir = paste0(fvsWork,uuid)
      dir.create(workDir)
      binDir = paste0(workDir,"/FVSbin/")
      # fvs libraries
      dir.create(binDir)
      cmd = paste0("cp ",paste(paste0(fvsBinDir,
            paste0(input$uivariants,".so"),collapse=" ")," ",binDir))
cat ("fvsbin cmd=",cmd,"\n")
      system (cmd)
      # shiny code, etc
      needed=paste(paste0(fvsOnlineDir,FVSOnlineNeeded),collapse=" ")
      system (paste0("cp -R ",needed," ",workDir))
      # rFVS
      dir.create(tr <- paste0(workDir,"/rFVS"))
      system (paste0("cp -R ",rFVSDir," ",tr))
      
      # projectId file...
      cat(file=paste0(workDir,"/projectId.txt"),
          "email=",input$emailnew,"\ntitle=",input$title,"\n")
      # make link, send email:
      rptFile = tempfile()
      con = file(rptFile,"w")
      link = paste0("http://forest.moscowfsl.wsu.edu/FVSwork/",uuid)
      cat (file=con,"Here is a link to the project named:",input$title,"\n\n")
      cat (file=con,link,"\n\n")
      cat (file=con,"Note that this and all other projects may be removed",
           "from the system 2 months after the last access.")
      close(con)

      mailCmd = paste('mailx -t',input$emailnew,'-a "From: FVSOnline"',
       '-a "Subject: New project: ',input$title,'"',
       '-a "Reply-To: Nicholas Crookston <ncrookston.fs@gmail.com>"',
       '-a "Cc: ncrookston.fs@gmail.com" < ',rptFile)      
      system (mailCmd)
      if (nchar(input$title)) 
      {
        msg = paste0('Project: "',input$title,'" created.')
        output$actionMsg = renderText(msg)
      }
      updateTextInput(session=session, inputId="title", value="")
    })
  })

  observe({
    if (input$submitexist==0) return()
    isolate({
      if (nchar(input$emailexist)<5 && regexpr("@",input$emailexist) < 2) return()
      prjListEmail(input$emailexist,sendEmail=TRUE)
      updateTextInput(session=session, inputId="emailexist", value="")
    })
  })

})


