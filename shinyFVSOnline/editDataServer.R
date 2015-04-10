library(shiny)
library(RSQLite)
library(shinysky)


options(shiny.trace = F)  # change to T for reactive tracing (lots of output)

shinyServer(function(input, output, session) {

  globals <- new.env()
  globals$tbl <- NULL
  globals$navsOn <- FALSE
  globals$rowSelOn <- FALSE
  dbDrv <- dbDriver("SQLite")
  con <- dbConnect(dbDrv,"FVS_Data.db")
  tbs <- dbListTables(con)
  globals$tbsCTypes <- lapply(tbs,function(x,con) 
    {
      res <- dbSendQuery(con,paste0("Select * from ",x," limit 1"))
      tb <- dbFetch(res)
      dbClearResult(con)
      unlist(lapply(tb,class)) == "character"
    }, con)     
  names(globals$tbsCTypes) = tbs
    
  idx <- grep ("StandInit",tbs)
  if (length(idx) == 0) idx=1 

  updateSelectInput(session=session, inputId="selectdbtabs", choices=tbs, 
    selected=tbs[idx])      
  
  observe({
cat ("selectdbtabs, input$selectdbtabs=",input$selectdbtabs,"\n")
    if (length(input$selectdbtabs)) 
    {
      globals$tblName <- input$selectdbtabs
      globals$tbl <- NULL
      globals$tblCols <- names(globals$tbsCTypes[[globals$tblName]])
      if (length(intersect("Stand_ID",globals$tblCols))) 
      {
        res = dbSendQuery(con,paste0("select distinct Stand_ID from ",
                          globals$tblName))
        globals$sids = dbFetch(res,n=-1)$Stand_ID
        dbClearResult(con)
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
      switch(input$mode,
        "New rows"= 
        {
          globals$rows <- NULL
          tbl <- as.data.frame(matrix("",ncol=length(input$selectdbvars),
                               nrow=20))
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
          res <- dbSendQuery(con,qry)
          globals$tbl <- dbFetch(res,n=-1)
          dbClearResult(con)
          for (col in 2:ncol(globals$tbl))
            if (class(globals$tbl[[col]]) != "character") 
               globals$tbl[[col]] = as.character(globals$tbl[[col]])
          if (nrow(globals$tbl) == 0) globals$rows = NULL else
          {
            globals$tbl$Delete = "No"
            globals$rows <- c(1,min(nrow(globals$tbl),20))
            output$tbl <- renderHotable(globals$tbl[1:min(nrow(globals$tbl),20),
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
      newBot <- min(globals$rows[2]+20,nrow(globals$tbl))
      newTop <- max(newBot-19,1)
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
      newTop <- max(globals$rows[1]-20,1)
      newBot <- min(newTop+19,nrow(globals$tbl))
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
              dbBegin(con)
              for (ins in inserts) dbSendQuery(con,ins)
              dbCommit(con)
              tbl <- as.data.frame(matrix("",
                     ncol=length(input$selectdbvars),nrow=15))
              colnames(tbl) <- input$selectdbvars
              output$tbl <- renderHotable(tbl,readOnly=FALSE)
            }
          },
          Edit = 
          {
            dbBegin(con)
            ln = length(input$tbl$data)
            if (ln) for (rn in 1:ln)
            {
              row = input$tbl$data[[rn]]
              names(row) = unlist(input$tbl$colHeaders)
              id = globals$tbl[globals$rows[1]:globals$rows[2],"rowid"][rn]
              if (row$Delete != "No") 
              {
                qry = paste0("delete from ",globals$tblName," where _ROWID_ = ",
                             id,";")
cat ("edit, qry=",qry,"\n")                     
                dbSendQuery(con,qry)
                if (!is.null(globals$sids)) globals$sids = NULL
              } else {globals$rows
                id = globals$tbl[globals$rows[1]:globals$rows[2],"rowid"][rn]
                row = unlist(row)
                if (length(row) < 2) next
                row = row[-1]
                org = subset(globals$tbl,rowid == id)
                org = as.character(org[,names(row),drop=TRUE])
                update = row[org != row]
                if (length(update) == 0) next
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
cat ("edit, qry=",qry,"\n")
                dbSendQuery(con,qry)              
              }
            }   
            dbCommit(con)
            if (is.null(globals$sids))
            {
              res = dbSendQuery(con,paste0("select distinct Stand_ID from ",
                                globals$tblName))
              globals$sids = dbFetch(res,n=-1)$Stand_ID
              dbClearResult(con)
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
            res <- dbSendQuery(con,qry)
            globals$tbl <- dbFetch(res,n=-1)
            dbClearResult(con)
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
  
  observe(if (input$FVSOnline > 0) 
  {
cat ("returnToFVSOnline\n")
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
      
    
    
