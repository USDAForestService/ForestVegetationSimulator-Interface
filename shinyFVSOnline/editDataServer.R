library(shiny)
library(RSQLite)
library(shinysky)


# set shiny.trace=T for reactive tracing (lots of output)
options(shiny.maxRequestSize=30*1024^2,shiny.trace = F)

shinyServer(function(input, output, session) {
                                                      
  globals <- new.env()
  globals$tbl <- NULL
  globals$navsOn <- FALSE            
  globals$rowSelOn <- FALSE
  globals$disprows <- 20
  
  dbDrv <- dbDriver("SQLite")
  dbcon <- dbConnect(dbDrv,"FVS_Data.db")
  tbs <- dbListTables(dbcon)
  globals$tbsCTypes <- lapply(tbs,function(x,dbcon) 
    {
      res <- dbSendQuery(dbcon,paste0("Select * from ",x," limit 1"))
      tb <- dbFetch(res)
      dbClearResult(dbcon)
      unlist(lapply(tb,class)) == "character"
    }, dbcon)     
  names(globals$tbsCTypes) = tbs
    
  idx <- grep ("StandInit",tbs)
  if (length(idx) == 0) idx=1 

  updateSelectInput(session=session, inputId="selectdbtabs", choices=tbs, 
    selected=tbs[idx])
  
  observe({                       
cat ("selectdbtabs, input$selectdbtabs=",input$selectdbtabs,
     " input$mode=",input$mode,"\n")
    if (length(input$selectdbtabs)) 
    {
      globals$tblName <- input$selectdbtabs
      fixEmptyTable(dbcon,globals,checkCnt=TRUE)
      checkMinColumnDefs(dbcon)
      globals$tbl <- NULL
      globals$tblCols <- names(globals$tbsCTypes[[globals$tblName]])
      if (length(intersect("Stand_ID",globals$tblCols))) 
      {
        res = dbSendQuery(dbcon,paste0("select distinct Stand_ID from ",
                          globals$tblName))
        globals$sids = dbFetch(res,n=-1)$Stand_ID
        dbClearResult(dbcon)
        if (any(is.na(globals$sids))) globals$sids[is.na(globals$sids)] = ""
        if (length(globals$sids) > 0)
        {
          if (globals$rowSelOn) updateSelectInput(session=session, 
            inputId="rowSelector",choices  = globals$sids) else 
          output$stdSel <- mkStdSel(globals)
        } 
      } else {
        globals$sids <- NULL
        output$stdSel <- renderUI(NULL)
        globals$rowSelOn <- FALSE
      }
      updateSelectInput(session=session, inputId="selectdbvars", 
        choices=as.list(globals$tblCols),selected=globals$tblCols)  
    }
  })              
  

  observe({              
    if (length(input$selectdbvars)) 
    {
cat ("selectdbvars, input$selectdbvars=",input$selectdbvars,"\n")       
      ndr = suppressWarnings(as.numeric(input$disprows))
      if (is.na(ndr) || is.nan(ndr) || ndr < 1 || ndr > 500) ndr = 20 
      globals$disprows <- ndr
      switch(input$mode,                    
        "New rows"= 
        {
          globals$rows <- NULL
          tbl <- as.data.frame(matrix("",ncol=length(input$selectdbvars),
                               nrow=globals$disprows))
          colnames(tbl) <- input$selectdbvars
          output$tbl <- renderHotable(tbl,readOnly=FALSE)
          output$stdSel <- output$navRows <- renderUI(NULL)
          globals$rowSelOn <- globals$navsOn <- FALSE
        },
        Edit = 
        {
          qry <- paste0("select _ROWID_,* from ",globals$tblName)
          qry <- if (length(intersect("Stand_ID",globals$tblCols)) && 
                     length(input$rowSelector))
            paste0(qry," where Stand_ID in (",
                  paste0("'",input$rowSelector,"'",collapse=","),");") else
            paste0(qry,";")                             
          res <- dbSendQuery(dbcon,qry)
          globals$tbl <- dbFetch(res,n=-1)
          dbClearResult(dbcon)
          for (col in 2:ncol(globals$tbl))
            if (class(globals$tbl[[col]]) != "character") 
               globals$tbl[[col]] = as.character(globals$tbl[[col]])
          if (nrow(globals$tbl) == 0) globals$rows = NULL else
          {
            globals$tbl$Delete = "No"
            globals$rows <- c(1,min(nrow(globals$tbl),globals$disprows))
            output$tbl <- renderHotable(globals$tbl[1:min(nrow(globals$tbl),globals$disprows),
              union(c("Delete"),input$selectdbvars),drop=FALSE],
              readOnly=FALSE)
            if (!globals$navsOn) 
            {
              globals$navsOn <- TRUE
              output$navRows <- renderUI(list(
                shiny::actionButton("previousRows","<< previous rows"),
                shiny::actionButton("nextRows","next rows >>"),
                textOutput("rowRng",inline=TRUE)))
            }
            output$rowRng <- renderText(paste0(globals$rows[1]," to ",
                  globals$rows[2]," of ",nrow(globals$tbl)))
            
            if (!globals$rowSelOn && length(globals$sids))
              output$stdSel <- mkStdSel(globals)
          }
        }
      )
    }
  })

  observe({
    if (length(input$nextRows) && input$nextRows > 0) 
    {
      if (is.null(globals$tbl)) return()
      input$disprows
      newBot <- min(globals$rows[2]+globals$disprows,nrow(globals$tbl))
      newTop <- max(newBot-globals$disprows-1,1)
      globals$rows <- c(newTop,newBot)
      output$tbl <- renderHotable(globals$tbl[newTop:newBot,
        union(c("Delete"),isolate(input$selectdbvars)),
          drop=FALSE],readOnly=FALSE)
      output$rowRng <- renderText(paste0(newTop," to ",
          newBot," of ",nrow(globals$tbl)))
    }
  })
  observe({
    if (length(input$previousRows) && input$previousRows > 0) 
    {
      if (is.null(globals$tbl)) return()
      input$disprows
      newTop <- max(globals$rows[1]-globals$disprows,1)
      newBot <- min(newTop+globals$disprows-1,nrow(globals$tbl))
      globals$rows <- c(newTop,newBot)
      output$tbl <- renderHotable(globals$tbl[newTop:newBot,
        union(c("Delete"),isolate(input$selectdbvars)),
          drop=FALSE],readOnly=FALSE)
      output$rowRng <- renderText(paste0(newTop," to ",
          newBot," of ",nrow(globals$tbl)))
    }
  })
      
  
  observe({
    if (input$commitChanges > 0) 
    {
      isolate({
cat ("commitChanges, mode=",input$mode,"len tbl=",length(input$tbl),"\n")
        switch(input$mode,
          "New rows"= 
          {
            inserts <- mkInserts(input$tbl,globals$tblName,
                             globals$tbsCTypes[[globals$tblName]])
lapply(inserts,function (x) cat("ins=",x,"\n"))                             
            if (length(inserts)) 
            {
              dbBegin(dbcon)
              err = FALSE
              for (ins in inserts) 
              {
                res = try(dbSendQuery(dbcon,ins))
                if (class(res) == "try-error") {err=TRUE; break}
              }
              if (err) 
              {
                dbRollback(dbcon) 
                output$actionMsg = renderText(paste0("Error processing: ",ins))
                return()
              } else {
                dbCommit(dbcon)
                output$actionMsg = renderText(paste0(length(inserts)," insert(s) processed."))
              }
              tbl <- as.data.frame(matrix("",
                     ncol=length(input$selectdbvars),nrow=globals$disprows))
              colnames(tbl) <- input$selectdbvars
              output$tbl <- renderHotable(tbl,readOnly=FALSE)
            }
          },
          Edit = 
          {
            dbBegin(dbcon)
            err=FALSE
            ln = length(input$tbl$data)
            nprocess = 0
            if (ln) for (rn in 1:ln)
            {
              row = input$tbl$data[[rn]]
              names(row) = unlist(input$tbl$colHeaders)
              id = globals$tbl[globals$rows[1]:globals$rows[2],"rowid"][rn]
              if (row$Delete != "No") 
              {
                qry = paste0("delete from ",globals$tblName," where _ROWID_ = ",
                             id,";")
cat ("edit del, qry=",qry,"\n")                     
                res = try(dbSendQuery(dbcon,qry))
                if (class(res) == "try-error") {err=TRUE; break}
                nprocess = nprocess+1
                if (!is.null(globals$sids)) globals$sids = NULL
              } else {
                id = globals$tbl[globals$rows[1]:globals$rows[2],"rowid"][rn]
                row = unlist(row)
cat ("edit update, id =",id," row=",row,"\n")
                if (length(row) < 2) next
                row = row[-1]
                org = subset(globals$tbl,rowid == id)
                org = as.character(org[,names(row),drop=TRUE])
                names(org)=names(row)
                neq = vector("logical",length(row))
                for (i in 1:length(row)) neq[i]=!identical(row[i],org[i])
                if (sum(neq) == 0) next
                update = row[neq]
                toquote = globals$tbsCTypes[[globals$tblName]][names(update)]
                if (!is.null(globals$sids) && 
                    !is.na(toquote["Stand_ID"])) globals$sids = NULL
                if (any(toquote))
                {
                  for (toq in names(toquote[toquote]))
                  {
                    update[toq] = gsub("'","''",update[toq])  
                    update[toq] = paste0("'",update[toq],"'")
                  }
                }
                qry = paste0("update ",globals$tblName," set ",
                  paste(paste0(names(update)," = ",update),collapse=", "),
                    " where _ROWID_ = ",id,";")
cat ("edit upd, qry=",qry,"\n")
                res = try(dbSendQuery(dbcon,qry))              
                if (class(res) == "try-error") {err=TRUE; break}
                nprocess = nprocess+1
              }
            }
            if (err) 
            {
              dbRollback(dbcon) 
              output$actionMsg = renderText(paste0("Error processing: ",qry))
              return()
            } else {
              dbCommit(dbcon)
              output$actionMsg = renderText(paste0(nprocess," change(s) processed."))
            }
            fixEmptyTable(dbcon,globals,checkCnt=TRUE)
cat ("after commit, is.null(globals$sids)=",is.null(globals$sids),
     " globals$tblName=",globals$tblName,
     " Stand_ID yes=",length(intersect("Stand_ID",globals$tblCols)),"\n")
            if (is.null(globals$sids) && 
                length(intersect("Stand_ID",globals$tblCols)))
            {
              res = dbSendQuery(dbcon,paste0("select distinct Stand_ID from ",
                                globals$tblName))
              globals$sids = dbFetch(res,n=-1)$Stand_ID
              dbClearResult(dbcon)
              if (any(is.na(globals$sids))) globals$sids[is.na(globals$sids)] = ""
              if (globals$rowSelOn && length(globals$sids)) 
                updateSelectInput(session=session, inputId="rowSelector",
                  choices  = globals$sids) else 
                output$stdSel <- mkStdSel(globals)
            }  

            qry <- paste0("select _ROWID_,* from ",globals$tblName)
            qry <- if (length(intersect("Stand_ID",globals$tblCols)) && 
                       length(input$rowSelector))
              paste0(qry," where Stand_ID in (",
                    paste0("'",input$rowSelector,"'",collapse=","),");") else
              paste0(qry,";") 
            res <- dbSendQuery(dbcon,qry)
            globals$tbl <- dbFetch(res,n=-1)
            dbClearResult(dbcon)
            for (col in 2:ncol(globals$tbl))
              if (class(globals$tbl[[col]]) != "character") 
                 globals$tbl[[col]] = as.character(globals$tbl[[col]])
            if (nrow(globals$tbl) == 0) globals$rows = NULL else 
            {
              globals$tbl$Delete = "No"
              globals$rows <- c(globals$rows[1],
                              min(nrow(globals$tbl),globals$rows[2]))
              output$tbl <- renderHotable(
                globals$tbl[globals$rows[1]:globals$rows[2],
                union(c("Delete"),input$selectdbvars),drop=FALSE],
                readOnly=FALSE)
            }
          }
        )
      })
    }
  })

  ## recoverdb
  observe({  
    if (input$recoverdb == 0) return()
    if (file.exists("FVS_Data.db")) file.remove("FVS_Data.db")
    if (file.exists("FVS_Data.db.backup")) 
        file.rename("FVS_Data.db.backup","FVS_Data.db") else
        file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
    output$reload<-renderUI(tags$script("location.reload();"))
  }) 

  
  observe(if (input$clearTable > 0) 
  {
cat ("clearTable, tbl=",globals$tblName,"\n")
    dbSendQuery(dbcon,paste0("delete from ",globals$tblName))
    globals$navsOn <- FALSE            
    globals$rowSelOn <- FALSE
    globals$sids <- NULL
    output$stdSel <- renderUI(NULL)
    fixEmptyTable(dbcon,globals)
    output$tbl <- renderHotable(
       globals$tbl[,union(c("Delete"),input$selectdbvars),drop=FALSE],
                readOnly=FALSE)
    output$rowRng <- renderText("1 to 1 of 1")
    isolate(if (input$mode=="New rows") updateRadioButtons(session=session, 
      inputId="mode",selected="Edit"))
  })
  
  observe(if (input$FVSOnline > 0) 
  {
cat ("returnToFVSOnline\n")
    checkMinColumnDefs(dbcon)
    if (all(file.exists(c("fvsOnlineServer.R","fvsOnlineUI.R"))))
    {
      unloadNamespace("shinysky")
      file.copy("fvsOnlineServer.R", "server.R",overwrite=TRUE)
      file.copy("fvsOnlineUI.R",     "ui.R",    overwrite=TRUE)
      unlink("fvsOnlineServer.R")
      unlink("fvsOnlineUI.R")
      Sys.sleep(.5)
      output$reload <- renderUI(tags$script("location.reload();"))
    } 
  })

  ## Upload
  observe({  
    if (is.null(input$upload)) return()
    if (regexpr("\\.accdb$",input$upload$name) == 0 && 
        regexpr("\\.mdb$",input$upload$name)   == 0 &&
        regexpr("\\.db$",input$upload$name)    == 0) return()
    dbDisconnect(dbcon)
    if (file.exists("FVS_Data.db"))
    {
      if (file.exists("FVS_Data.db.backup")) file.remove("FVS_Data.db.backup")
      file.rename("FVS_Data.db","FVS_Data.db.backup")
    }
    if (regexpr("\\.db$",input$upload$name) > 1) 
    {
      file.copy(input$upload$datapath,"FVS_Data.db",overwrite = TRUE)
      output$actionMsg = renderText("New FVS_Data.db installed.")
    } else {   
      Sys.sleep (.1)        
      withProgress(session, 
      {  
        setProgress(message = "Create schema", value = 1) 
        curDir=getwd()
        setwd(dirname(input$upload$datapath))
        system (paste0("java -jar '",curDir,"/access2csv.jar' ",
                input$upload$datapath," --schema > schema"))
        setProgress(message = "Process schema", value = 2)
        if (!file.exists("schema") || file.size("schema") == 0) 
        {
          setwd(curDir)      
          setProgress(value = NULL)
          output$actionMsg = renderText("'schema' not created, no data loaded.")
          Sys.sleep (2)
          session$sendCustomMessage(type = "resetFileInputHandler","upload")
          return()
        }
        schema = scan("schema",what="character",sep="\n",quiet=TRUE)
        schema = gsub(" LONG,"," INTEGER,",schema)
        schema = gsub(" DOUBLE,"," REAL,",schema)
        fix = grep (")",schema)        
        schema[fix] = sub (")",");",schema[fix])
        fix = fix - 1
        schema[fix] = sub (",","",schema[fix])
        for (i in 1:length(schema))
        {
          if (substring(schema[i],1,12) == "CREATE TABLE") next
          if (substring(schema[i],1,2)  == ");") next
          items = unlist(strsplit(schema[i]," "))
          items = items[nchar(items)>0]
          schema[i] = paste0(' "',items[1],'" ',items[2])
        }        
        cat (paste0(schema,"\n"),file="schema")
        setProgress(message = "Extract data", value = 3) 
        system (paste0("java -jar '",curDir,"/access2csv.jar' ",
                 input$upload$datapath))  
        setProgress(message = "Import schema to Sqlite3", value = 4) 
        system (paste0 ("sqlite3 ","FVS_Data.db"," < schema"))
        fix = grep ("CREATE TABLE",schema)
        schema = sub ("CREATE TABLE ","",schema[fix])
        schema = sub (" [(]",".csv",schema)
        i = 5
        for (s in schema)
        {
          cat (".separator ,\n",file="schema")
          cat (".import ",s," ",sub(".csv","",s),"\n",file="schema",append=TRUE)
          setProgress(message = paste0("Import ",s), value = i) 
          i = i+1;
          system (paste0("sqlite3 ","FVS_Data.db"," < schema"))
        }
        setProgress(message = "Copy tables", value = i+1) 
        lapply(schema,unlink) 
        file.copy("FVS_Data.db",paste0(curDir),overwrite=TRUE)
        unlink("schema") 
        setwd(curDir)      
        Sys.sleep (.5) 
        setProgress(value = NULL)  
      }, min=1, max=12)
    }
    unlink(input$upload$datapath)
    dbcon <- dbConnect(dbDrv,"FVS_Data.db")
    fixFVSKeywords(dbcon) 
    checkMinColumnDefs(dbcon)
    dbDisconnect(dbcon)
    output$reload<-renderUI(tags$script("location.reload();"))
  }) 

  ## uploadStdTree
  observe({  
    if (is.null(input$uploadStdTree)) return()
    isolate({      
      indat = try(read.csv(file=input$uploadStdTree$datapath,as.is=TRUE))
      if (file.exists(input$uploadStdTree$datapath)) 
               unlink(input$uploadStdTree$datapath)
      if (class(indat) == "try-error" || nrow(indat)==0)
      {
        output$actionMsg = renderText("Input empty, no data loaded.")
        Sys.sleep(1)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      }
      del = apply(indat,1,function (x) 
        {
          x = as.vector(x)
          x[is.na(x)] = ""
          all(x == "")
        })
      indat = indat[!del,,drop=FALSE]
      if (nrow(indat)==0)
      {
        output$actionMsg = renderText("All rows were empty,  no data loaded.")
        Sys.sleep(1)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      }
      cols = na.omit(pmatch(tolower(colnames(indat)),
               tolower(names(globals$tbsCTypes[[globals$tblName]]))))
      if (length(cols) == 0) 
      {
        output$actionMsg = renderText(paste0("No columns match what is defined for ",
               globals$tblName,", no data loaded."))
        Sys.sleep(1)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      }
      kill = attr(cols,"na.action")
      if (length(kill)) indat = indat[,-kill,drop=FALSE]
      types = globals$tbsCTypes[[globals$tblName]][cols]
      req = switch(globals$tblName,
         FVS_StandInit = c("Stand_ID","Variant","Inv_Year"),
         FVS_TreeInit  = c("Stand_ID","Species","DBH"),
         FVS_GroupAddFilesAndKeywords = c("Groups"),
         NULL)
      if (!is.null(req) && !all(req %in% names(types)))
      {
        output$actionMsg = renderText(paste0("Required columns were missing for ",
               globals$tblName,", no data loaded."))
        Sys.sleep(1)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      }
      colnames(indat) = names(types)
      quote = types[types]
      if (length(quote)) for (cn in names(quote)) 
        indat[,cn] = paste0("'",indat[,cn],"'")
      dbBegin(dbcon)
      err = FALSE
      for (i in 1:nrow(indat))
      {
        row = indat[i,,drop=FALSE]
        row = row[,!is.na(row),drop=FALSE]
        qry = paste0("insert into ",globals$tblName," (",
                paste0(colnames(row),collapse=","),
                  ") values (",paste0(row,collapse=","),");")
        res = try(dbSendQuery(dbcon,qry))
        if (class(res) == "try-error") {err=TRUE; break}
      }
      if (err) 
      {
        dbRollback(dbcon) 
        output$actionMsg = renderText(paste0("Error processing: ",qry))
        return()
      } else {
        dbCommit(dbcon)
        output$actionMsg = renderText(paste0(nrow(indat)," rows were inserted into ",
               globals$tblName))
      }
      Sys.sleep(1)
      session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
      dbSendQuery(dbcon,paste0("delete from ",globals$tblName,
        " where Stand_ID = ''"))      
      res = dbSendQuery(dbcon,paste0("select distinct Stand_ID from ",
                        globals$tblName))
      globals$sids = dbFetch(res,n=-1)$Stand_ID
      dbClearResult(dbcon)
      if (any(is.na(globals$sids))) globals$sids[is.na(globals$sids)] = ""
      if (globals$rowSelOn && length(globals$sids)) 
        updateSelectInput(session=session, inputId="rowSelector",
          choices  = as.list(globals$sids), selected=unique(indat[,"Stand_ID"])) else 
        output$stdSel <- mkStdSel(globals)
      
      qry <- paste0("select _ROWID_,* from ",globals$tblName)
      qry <- if (length(input$rowSelector))
        paste0(qry," where Stand_ID in (",
              paste0("'",input$rowSelector,"'",collapse=","),");") else
        paste0(qry,";") 
      res <- dbSendQuery(dbcon,qry)
      globals$tbl <- dbFetch(res,n=-1)
      dbClearResult(dbcon)
      for (col in 2:ncol(globals$tbl))
        if (class(globals$tbl[[col]]) != "character") 
           globals$tbl[[col]] = as.character(globals$tbl[[col]])
      if (nrow(globals$tbl) == 0) globals$rows = NULL else 
      {
        globals$tbl$Delete = "No"
        globals$rows <- c(globals$rows[1],
                        min(nrow(globals$tbl),globals$rows[2]))
        output$tbl <- renderHotable(
          globals$tbl[globals$rows[1]:globals$rows[2],
          union(c("Delete"),input$selectdbvars),drop=FALSE],
          readOnly=FALSE)
      } 
      Sys.sleep(1)
      session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
    })
  }) 


  ## climateFVSUpload
  observe({  
    if (is.null(input$climateFVSUpload)) return()
    progress <- shiny::Progress$new(session,min=1,max=10)
    progress$set(message = "Loading data set",value = 2)    
    if (input$climateFVSUpload$type == "application/zip")
    {
      cmd = paste0("unzip -l -p ",input$climateFVSUpload$datapath,
        " FVSClimAttrs.csv > FVSClimAttrs.csv ")
      system (cmd)
    } else {
      file.copy(input$climateFVSUpload$datapath,"FVSClimAttrs.csv",
        overwrite = TRUE)
    }
    if (!file.exists("FVSClimAttrs.csv")) 
    {
cat ("no FVSClimAttrs.csv file\n")
      output$actionMsg = renderText("FVSClimAttrs.csv not found.")
      progress$set(message = "FVSClimAttrs.csv not found", value = 6)
      Sys.sleep (2)
      session$sendCustomMessage(type = "resetFileInputHandler","climateFVSUpload")
      progress$close()
      return()
    }
cat ("processing FVSClimAttrs.csv\n")
    progress$set(message = "Loading data set (big files take a while)",value = 2) 
    climd = read.csv("FVSClimAttrs.csv",nrows=1)
    climd = read.csv("FVSClimAttrs.csv",colClasses=c(rep("character",2),
        "integer",rep("numeric",ncol(climd)-3)),as.is=TRUE)        
    colnames(climd)[1] <- "Stand_ID"
    unlink("FVSClimAttrs.csv")
    climTab <- dbListTables(dbcon)
    if (!("FVS_ClimAttrs" %in% climTab))
    {
cat ("no current FVS_ClimAttrs\n")
      progress$set(message = "Building FVS_ClimAttrs table",value = 4) 
      dbWriteTable(dbcon,"FVS_ClimAttrs",climd)
      output$actionMsg = renderText("FVSClimAttrs created.")
      rm (climd)
      progress$set(message = "Creating FVS_ClimAttrs index",value = 6)
      dbSendQuery(dbcon,'drop index if exists StdScnIndex')
      dbSendQuery(dbcon,"create index StdScnIndex on FVS_ClimAttrs (Stand_ID, Scenario);")
      progress$set(message = "Done", value = 9)
      Sys.sleep (.5)
      progress$close()
      return()      
    }
cat ("current FVS_ClimAttrs\n")
    if (file.exists("FVSClimAttrs.db")) unlink("FVSClimAttrs.db")
    dbclim <- nect(dbDrv,"FVSClimAttrs.db")
    progress$set(message = "Building temporary FVS_ClimAttrs table",value = 4) 
    dbWriteTable(dbclim,"FVS_ClimAttrs",climd)
    rm (climd)  
    progress$set(message = "Query distinct stands and scenarios",value = 5) 
    distinct = dbGetQuery(dbclim,"select distinct Stand_ID,Scenario from FVS_ClimAttrs")
    dbDisconnect(dbclim)
    progress$set(message = "Cleaning previous climate data as needed",value = 6)    
    dbBegin(dbcon)
    results = apply(distinct,1,function (x,dbcon)
    {
      dbSendQuery(dbcon,paste0('delete from FVS_ClimAttrs where Stand_ID = "',
         x[1],'" and Scenario = "',x[2],'"'))
    }, dbcon)
    dbCommit(dbcon)
    dbSendQuery(dbcon,'drop index if exists StdScnIndex')
    
    dbSendQuery(dbcon,'attach database "FVSClimAttrs.db" as new')
    # get the table:
    progress$set(message = "Inserting new data",value = 8)    
    qur = dbSendQuery(dbcon,'select * from FVS_ClimAttrs')
    oldAttrs = dbFetch(qur,n=1)
    dbClearResult(qur)
    if (nrow(oldAttrs) == 0) 
    {
cat ("simple copy from new, all rows were deleted\n")
      dbSendQuery(dbcon,'drop table FVS_ClimAttrs')
      dbSendQuery(dbcon,'insert into FVS_ClimAttrs select * from new.FVS_ClimAttrs')
    } else {
      qur = dbSendQuery(dbcon,'select * from new.FVS_ClimAttrs')
      newAttrs = dbFetch(qur,n=1)
      dbClearResult(qur)
      if (identical(colnames(oldAttrs),colnames(newAttrs)))
      {
cat ("simple insert from new, all cols are identical\n")
        dbSendQuery(dbcon,'insert into FVS_ClimAttrs select * from new.FVS_ClimAttrs')
      } else {  
cat ("need to match columns, cols are not identical\n")
        oldAttrs=colnames(oldAttrs)[-(1:3)]
        newAttrs=colnames(newAttrs)
        ssid = newAttrs[1:3]
        newAttrs = newAttrs[-(1:3)]
        oldcl=unlist(lapply(oldAttrs,function (x) if (tolower(x) == x) x else NULL))
        newcl=unlist(lapply(newAttrs,function (x) if (tolower(x) == x) x else NULL))
        oldsp=unlist(lapply(oldAttrs,function (x) if (toupper(x) == x) x else NULL))
        newsp=unlist(lapply(newAttrs,function (x) if (toupper(x) == x) x else NULL))
        oldot=setdiff(setdiff(oldAttrs,oldcl),oldsp)   
        newot=setdiff(setdiff(newAttrs,newcl),newsp)   
        bothcl = union(oldcl,newcl)
        bothsp = union(oldsp,newsp)
        bothot = union(oldot,oldot)
        newall = c(bothcl,bothsp,bothot)
        oldmiss= setdiff(newall,oldAttrs)
        newmiss= setdiff(newall,newAttrs)
        newall = c(ssid,newall)
        selnew = paste0(newall,collapse=",")
cat ("length(newmiss)=",length(newmiss)," selnew=",selnew,"\n")
        if (length(newmiss) > 0)  
        {
          dbBegin(dbcon)
          for (mis in newmiss) dbSendQuery(dbcon,
            paste0('alter table new.FVS_ClimAttrs add "',mis,'" real'))
          dbCommit(dbcon)
        }
cat ("length(oldmiss)=",length(oldmiss),"\n")
        if (length(oldmiss) > 0)
        {
          dbSendQuery(dbcon,'alter table FVS_ClimAttrs rename to oldClimAttrs')
          dbBegin(dbcon)
          for (mis in oldmiss) dbSendQuery(dbcon,
            paste0('alter table oldClimAttrs add "',mis,'" real'))
          dbCommit(dbcon)
          dbSendQuery(dbcon,
            paste0('create table FVS_ClimAttrs as select ',selnew,' from oldClimAttrs'))
          dbSendQuery(dbcon,'drop table oldClimAttrs')
        }     
        dbSendQuery(dbcon,
          paste0('insert into FVS_ClimAttrs select ',selnew,' from new.FVS_ClimAttrs'))
      }
    }
    dbSendQuery(dbcon,'detach database new')   
    unlink("FVSClimAttrs.db")
    progress$set(message = "Recreating FVS_ClimAttrs index",value = 9)
    dbSendQuery(dbcon,'drop index if exists StdScnIndex')
    dbSendQuery(dbcon,"create index StdScnIndex on FVS_ClimAttrs (Stand_ID, Scenario);")
    progress$set(message = "Done", value = 10)
    output$actionMsg = renderText("FVSClimAttrs updated.")
    Sys.sleep (2)
    session$sendCustomMessage(type = "resetFileInputHandler","climateFVSUpload")
    progress$close()
  }) 
})


mkInserts <- function (ctbl,dbtblName,dbtbltypes)
{
  ins = list()

  for (row in ctbl$data)
  {
    row <- unlist(row)
    keep <- row != ""
    if (! any(keep)) next
    vars <- unlist(ctbl$colHeaders[keep])
    row <- row[keep]
    needtypes <- dbtbltypes[unlist(ctbl$colHeaders[keep])] 
    if (any(needtypes)) row[needtypes] = paste0("'",row[needtypes],"'")
    ins <- append(ins,
       paste0("insert into ",dbtblName," (",paste0(vars,collapse=","),
              ") values (",paste0(row,collapse=","),");"))
  }
  ins
}

  
mkStdSel <- function (globals)
{
  globals$rowSelOn <- TRUE
  renderUI(selectInput("rowSelector",
    "Select stand(s)", choices  = globals$sids, multiple = TRUE, 
     selectize=FALSE, size=10))
}
      
fixEmptyTable <- function (con,globals,checkCnt=FALSE)
{
  if (checkCnt)
  {
    qry = paste0("select count(*) from ",globals$tblName)
    res = dbSendQuery(con,qry)
    tmp = dbFetch(res,n=-1)
    if (tmp[1,1] > 0) return()
  }
  tmp = dbReadTable(con,globals$tblName)
  for (i in 1:ncol(tmp)) tmp[1,i]=if (class(tmp[1,i]) == "character") "" else NA 
  dbWriteTable(con,globals$tblName,tmp,overwrite=TRUE)
  qry <- paste0("select _ROWID_,* from ",globals$tblName)
  res <- dbSendQuery(con,qry)
  globals$tbl <- dbFetch(res,n=-1)
  globals$rows = c(1,1)
  globals$tbl$Delete = "No"
}

    
checkMinColumnDefs <- function(dbcon)
{
  #this routine may need to be rebuilt. One issue is that the Stand_CN may not be
  # in the TreeInit table. That is not checked in this code.
cat ("in checkMinColumnDefs\n")
  fields = try(dbListFields(dbcon,"FVS_StandInit"))
  # if this is an error, then FVS_StandInit does not exist and this is an error
  # where the standard fixup in this case is to try recovery of the database.
  if (class(fields) == "try-error")
  {
    if (file.exists("FVS_Data.db")) file.remove("FVS_Data.db")
    if (file.exists("FVS_Data.db.backup"))
    {
      file.rename("FVS_Data.db.backup","FVS_Data.db")
      checkMinColumnDefs(dbcon)
    } else {
      file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
      unlink("FVS_Data.db.backup")
    }
    return()
  }
  modStarted = FALSE
  sID = FALSE
  sCN = FALSE
  # make sure groups are defined, if missing set one to "All"
  if (length(grep("Groups",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbcon)}
    dbSendQuery(dbcon,
      "alter table FVS_StandInit add column Groups text not null default 'All'")
  }
  # make sure Stand_ID is defined
  if (length(grep("Stand_ID",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbcon)}
    dbSendQuery(dbcon,
      "alter table FVS_StandInit add column Stand_ID text")      
    sID = TRUE
  }
  # make sure Stand_CN is defined
  if (length(grep("Stand_CN",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbcon)}
    dbSendQuery(dbcon,
      "alter table FVS_StandInit add column Stand_CN text")
    sCN = TRUE
  }
  # make sure Inv_Year is defined
  if (length(grep("Inv_Year",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbcon)}
    year=substring(as.character(Sys.time()),1,4)
    dbSendQuery(dbcon,paste0(
      "alter table FVS_StandInit add column Inv_Year text not null default '",year,"'"))      
  }
  # make sure FVSKeywords is defined
  if (length(grep("FVSKeywords",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbcon)}
    year=substring(as.character(Sys.time()),1,4)
    dbSendQuery(dbcon,
      "alter table FVS_StandInit add column FVSKeywords text")
  }
cat ("in checkMinColumnDefs, modStarted=",modStarted," sID=",sID,
     " sCN=",sCN,"\n")
  if (modStarted)
  {                                             
    dbCommit(dbcon)
    if (sID || sCN) 
    {
      fvsInit = dbReadTable(dbcon,"FVS_StandInit")
      if (nrow(fvsInit))
      {
        if (sID) fvsInit$Stand_ID = 
          if (sCN) paste0("Stand",1:nrow(fvsInit)) else fvsInit$Stand_CN
        if (sCN) fvsInit$Stand_CN = fvsInit$Stand_ID
        dbWriteTable(dbcon,"FVS_StandInit",fvsInit,overwrite=TRUE)
      }
    }
  }
  # check on FVS_GroupAddFilesAndKeywords, if present, assume it is correct
  gtab = try(dbReadTable(dbcon,"FVS_GroupAddFilesAndKeywords"))
  need = class(try) == "try-error"
  if (!need) need = nrow(gtab) == 0 
  if (!need) need = all(is.na(gtab[,"FVSKeywords"]))
  if (!need) need = all(gtab[,"FVSKeywords"] == "")
  if (need)
  {
    dfin = data.frame(Groups = "All All_Stands",Addfiles = "",
      FVSKeywords = paste0("Database\nDSNIn\nFVS_Data.db\nStandSQL\n",
        "SELECT * FROM FVS_StandInit\nWHERE Stand_CN= '%Stand_CN%'\n",
        "EndSQL\nTreeSQL\nSELECT * FROM FVS_TreeInit\n", 
        "WHERE Stand_CN= '%Stand_CN%'\nEndSQL\nEND")    
    )
    dbWriteTable(dbcon,name="FVS_GroupAddFilesAndKeywords",value=dfin,overwrite=TRUE)
  }
}


fixFVSKeywords <- function(dbcon)
{
  tbs <- dbListTables(dbcon)
  for (tb in tbs)
  {
cat ("in fixFVSKeywords, tb=",tb,"\n")
    flds <- dbListFields(dbcon, tb)
    kwdsIdxs <- grep ("keywords",flds,ignore.case = TRUE)
    if (length(kwdsIdxs) == 0) next
    for (kwdname in flds[kwdsIdxs])
    {    
      qry = paste0("select _ROWID_,",kwdname," from ",tb,
        " where ",kwdname," is not null and ",kwdname," != '';")
cat ("qry=",qry,"\n")              
      res <- dbSendQuery(dbcon,qry)
      kwdf <- dbFetch(res, n=-1)
cat ("result nrow=",nrow(kwdf),"\n")      
      dbClearResult(dbcon)
      if (nrow(kwdf))
      {
        for (row in 1:nrow(kwdf))
        {
          if (nchar(kwdf[row,2]) < 2) {kwdf[row,1] = -1; next}
          one <- gsub ("\r","",kwdf[row,2])
          dsnin <- grep ("dsnin",one,ignore.case = TRUE)
          if (length(dsnin) == 0) {kwdf[row,1] = -1; next}
          one <- scan(text=one,sep="\n",what="character",quiet=TRUE)
          dsnin <- grep ("dsnin",one,ignore.case = TRUE)+1
          one[dsnin] <- "FVS_Data.db"
          kwdf[row,2] <- paste0(paste0(one,collapse="\n"),"\n")
        }
        kwdf = subset(kwdf,rowid > 0)
        if (nrow(kwdf) > 0)
        {
          dbBegin(dbcon)
          for (row in 1:nrow(kwdf))
          {
            qut <- if (length(grep("'",kwdf[row,2],fixed=TRUE))) "\"" else "'"
            qry <- paste0("update ",tb," set ",kwdname," = ",qut,
              kwdf[row,2],qut," where _ROWID_ = ",kwdf[row,1],";")
cat ("qry=",qry,"\n")              
             dbSendQuery(dbcon,qry)              
          }
          dbCommit(dbcon)
        }
      }
    }
  }
cat ("exit fixFVSKeywords\n")
}    
