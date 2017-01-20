library(shiny)
library(rhandsontable)
library(ggplot2)
library(parallel)
library(RSQLite)
library(plyr)

# set shiny.trace=T for reactive tracing (lots of output)
options(shiny.maxRequestSize=1000*1024^2,shiny.trace = FALSE) 

shinyServer(function(input, output, session) {

  if (!interactive()) sink("FVSOnline.log")

  withProgress(session, {  
    setProgress(message = "Start up", 
                detail  = "Loading scripts and settings", value = 1)
 
    source("fvsRunUtilities.R",local=TRUE)
    source("fvsOutUtilities.R",local=TRUE)
    source("componentWins.R",local=TRUE)
    source("mkInputElements.R",local=TRUE)
    
    if (file.exists("localSettings.R")) 
             source("localSettings.R",local=TRUE) else if
       (file.exists("../../FVSOnline/settings.R")) 
             source("../../FVSOnline/settings.R",local=TRUE)
    
    load("prms.RData") 
    globals <- mkglobals(saveOnExit=TRUE)
    dbGlb <- new.env()
    dbGlb$tbl <- NULL
    dbGlb$navsOn <- FALSE            
    dbGlb$rowSelOn <- FALSE
    dbGlb$disprows <- 20
    resetGlobals(globals,NULL,prms)

    setProgress(message = "Start up",value = 2)
    ##load existing runs

    globals$fvsRun <- mkfvsRun()
    if (file.exists("FVS_Runs.RData"))
    {
      load("FVS_Runs.RData")
      
      # old style, convert to new.
      if (!is.null(names(FVS_Runs[[1]])))
      {
        newlist = list()
        for (i in 1:length(FVS_Runs))
        {
          setProgress(message="Converting runs to newest format",detail="",value = i+1)
          rtn = try(loadFromList(globals$fvsRun,FVS_Runs[[i]]))
          if (class(rtn) != "try-error")
          {
            newlist[[globals$fvsRun$uuid]] = globals$fvsRun$title
            attr(newlist[[globals$fvsRun$uuid]],"time") = attr(FVS_Runs[[i]],"time")
            saveFvsRun = globals$fvsRun
            save(file=paste0(globals$fvsRun$uuid,".RData"),saveFvsRun)
          }
        }
        FVS_Runs = reorderFVSRuns(newlist)    
        save (file="FVS_Runs.RData",FVS_Runs)
      }
      globals$FVS_Runs = FVS_Runs
      rm (FVS_Runs)      
    } else {
      resetfvsRun(globals$fvsRun,globals$FVS_Runs)
      globals$FVS_Runs[[globals$fvsRun$uuid]] = globals$fvsRun$title
    } 
    setProgress(message = "Start up",
                detail  = "Loading interface elements", value = 3)
    mkSimCnts(globals$fvsRun,globals$fvsRun$selsim)
    resetGlobals(globals,globals$fvsRun,prms)
    selChoices = names(globals$FVS_Runs)
    names(selChoices) = globals$FVS_Runs
cat ("Setting initial selections, length(selChoices)=",length(selChoices),"\n")
    updateSelectInput(session=session, inputId="runSel", 
        choices=selChoices,selected=selChoices[[1]])
    extns <-  prms$extensions
    extnslist <-  as.list(unlist(lapply(extns,function (x,extns) 
                      getPstring(extns,x), extns)))
    if (exists("fvsOutData")) rm (fvsOutData) 
    fvsOutData <- mkfvsOutData(plotSpecs=list(res=144,height=4,width=6))
    dbDrv <- dbDriver("SQLite")
    dbGlb$dbOcon <- dbConnect(dbDrv,"FVSOut.db")    
    dbSendQuery(dbGlb$dbOcon,'attach ":memory:" as m')
    if (!file.exists("FVS_Data.db")) 
      file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
    
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
    dbSendQuery(dbGlb$dbIcon,'attach ":memory:" as m')
    loadVarData(globals,prms,dbGlb$dbIcon)                                              
    setProgress(value = NULL)          
  }, min=1, max=6)
  
  session$onSessionEnded(function ()
  {                                                
cat ("onSessionEnded, globals$saveOnExit=",globals$saveOnExit,"\n")
    if (exists("dbGlb$dbOcon")) try(dbDisconnect(dbGlb$dbOcon))
    if (exists("dbGlb$dbIcon")) try(dbDisconnect(dbGlb$dbIcon))
    if (!globals$saveOnExit) return()
    saveRun()
    FVS_Runs = globals$FVS_Runs
    save (file="FVS_Runs.RData",FVS_Runs)
    if (file.exists("projectId.txt"))
    {
      prjid = scan("projectId.txt",what="",sep="\n",quiet=TRUE)
      write(file="projectId.txt",prjid)
    }
    if (!interactive()) q(save="no")
  })
  
  initTableGraphTools <- function ()
  {
cat ("initTableGraphTools\n")    
    fvsOutData$dbData = data.frame()
    fvsOutData$runs = character(0)
    fvsOutData$dbVars = character(0)
    fvsOutData$browseVars = character(0)
    fvsOutData$dbSelVars = character(0)
    fvsOutData$browseSelVars = character(0)
    choices = list()               
    updateSelectInput(session,"pivVar",choices=choices,select="")              
    updateSelectInput(session,"hfacet",choices=choices,select="") 
    updateSelectInput(session,"vfacet",choices=choices,select="") 
    updateSelectInput(session,"pltby", choices=choices,select="") 
    updateSelectInput(session,"dispVar",choices=choices,select="")       
    updateSelectInput(session,"xaxis",choices=choices,select="") 
    updateSelectInput(session,"yaxis",choices=choices,select="")
    updateCheckboxGroupInput(session, "browsevars", choices=choices) 
    updateTextInput(session=session, inputId="sqlOutput", label="", value="")
    choices = list("None loaded")
    updateSelectInput(session,"stdtitle",choices=choices,select=NULL)
    updateSelectInput(session,"stdgroups",choices=choices,select=NULL)
    updateSelectInput(session,"stdid",choices=choices,select=NULL)
    updateSelectInput(session,"mgmid",choices=choices,select=NULL)
    updateSelectInput(session,"year",choices=choices,select=NULL)
    updateSelectInput(session,"species",choices=choices,select=NULL)
    updateSelectInput(session,"dbhclass",choices=choices,select=NULL)
    output$table <- renderTable(NULL)
  }

  ## Load
  observe({
    if (input$topPan == "Process Outputs" && input$leftPan == "Load")
    {
cat ("Process Outputs & Load\n")
      initTableGraphTools()
      tbs <- dbListTables(dbGlb$dbOcon)
      if (length(tbs) > 0 && !is.na(match("FVS_Cases",tbs)))
      {
        fvsOutData$dbCases = dbReadTable(dbGlb$dbOcon,"FVS_Cases")
        fvsOutData$runs = unique(fvsOutData$dbCases$KeywordFile)
        times = NULL
        titles = NULL
        for (run in fvsOutData$runs) 
        {
          indx = grep(run,fvsOutData$dbCases$KeywordFile)[1]
          times = c(times,fvsOutData$dbCases$RunDateTime[indx])
          titles = c(titles,fvsOutData$dbCases$RunTitle[indx])
        }
        srt = order(times,decreasing = TRUE)
        fvsOutData$runs = fvsOutData$runs[srt]
        names(fvsOutData$runs) = titles[srt]
      }
      updateSelectInput(session, "runs", choices = fvsOutData$runs, 
        selected=0)
    }
  })

  #sdskldbh and sdskwdbh
  observe({
    if (input$sdskwdbh<1) updateNumericInput(session=session,
                          inputId="sdskwdbh",value=1)
    if (input$sdskwdbh>10) updateNumericInput(session=session,
                           inputId="sdskwdbh",value=10)
    if (input$sdskldbh < 2*input$sdskwdbh) updateNumericInput(session=session,
                           inputId="sdskldbh",value=2*input$sdskwdbh)
cat ("sdskwdbh=",input$sdskwdbh," sdskldbh",input$sdskldbh,"\n")
  })

  ## output run selection
  observe({
    if (input$leftPan != "Load") return()
cat ("runs, run selection (load) input$runs=",input$runs,"\n")
    if (!is.null(input$runs)) # will be a list of run keywordfile names (uuid's)
    {
      tbs <- dbListTables(dbGlb$dbOcon)
cat ("runs, tbs=",tbs,"\n")
      withProgress(session, {  
        i = 1
        setProgress(message = "Output query", 
                    detail  = "Selecting tables", value = i); i = i+1
        # create a m.Cases table that is a list of CaseIDs and MgmtIDs 
        # associated with the selected runs. These two items are used to 
        # filter records selected from selected tables.
        dbSendQuery(dbGlb$dbOcon,"drop table if exists m.Cases")
        inSet=paste0("('",paste(input$runs,collapse="','"),"')")
        dbSendQuery(dbGlb$dbOcon,paste0("create table m.Cases as select CaseID ",
                     "from FVS_Cases where FVS_Cases.KeywordFile in ",inSet))        
        dbBegin(dbGlb$dbOcon)      
        for (tb in tbs) 
        {
cat ("tb=",tb,"\n")
          cnt = 0
          if (tb == "FVS_Cases") next
          else if (tb == "Composite")
            dbSendQuery(dbGlb$dbOcon,"drop table Composite")
          else if (tb == "Composite_East")
            dbSendQuery(dbGlb$dbOcon,"drop table Composite_East")
          else if (tb == "StdStk")
            dbSendQuery(dbGlb$dbOcon,"drop table StdStk")
          else 
          {
            cnt = if ("CaseID" %in% dbListFields(dbGlb$dbOcon,tb))  
              dbGetQuery(dbGlb$dbOcon,paste0("select count(*) from ",
                   "(select distinct CaseID from ",tb," where CaseID in ",
                   "(select CaseID from m.Cases))")) else 1
          }
          if (cnt == 0) tbs = setdiff(tbs,tb)
        }
        source("sqlQueries.R")
        isolate(dbhclassexp <- mkdbhCase(input$sdskwdbh,input$sdskldbh))
        input$bldstdsk # force this section to be reactive to this input     
        if ("FVS_Summary" %in% tbs)
        {
          setProgress(message = "Output query", 
            detail  = "Building composites", value = i); i = i+1
          exqury(dbGlb$dbOcon,Create_Composite)
          tbs = c(tbs,"Composite")
cat ("tbs1=",tbs,"\n")
        }
        if ("FVS_Summary_East" %in% tbs)
        {
          setProgress(message = "Output query", 
            detail  = "Building composites", value = i); i = i+1
          exqury(dbGlb$dbOcon,Create_Composite_East)
          tbs = c(tbs,"Composite_East")
cat ("tbs2=",tbs,"\n")
        }

        if ("FVS_TreeList" %in% tbs)  
        {
          setProgress(message = "Output query", 
            detail  = "Building StdStk from Treelists", value = i); i = i+1
          exqury(dbGlb$dbOcon,Create_m.StdStk,dbhclassexp)
          if ("FVS_CutList" %in% tbs)
          {
            setProgress(message = "Output query", 
              detail  = "Building StdStk from Cutlists", value = i); i = i+1
            exqury(dbGlb$dbOcon,Create_m.HrvStdStk,dbhclassexp)
            setProgress(message = "Output query", 
              detail  = "Joining tables", value = i); i = i+1
            exqury(dbGlb$dbOcon,Create_StdStk,dbhclassexp)
          } else {
             setProgress(message = "Output query", 
              detail  = "Joining tables", value = i); i = i+2
            exqury(dbGlb$dbOcon,Create_StdStkNoHrv,dbhclassexp)
          }
          tbs = c(tbs,"StdStk")     
        }
cat ("tbs3=",tbs,"\n")       
        setProgress(message = "Output query", 
            detail  = "Committing changes", value = i); i = i+1
        dbCommit(dbGlb$dbOcon)
        dbd = lapply(tbs,function(tb,con) dbListFields(con,tb), dbGlb$dbOcon)
        names(dbd) = tbs
        if (!is.null(dbd[["FVS_Summary"]])) dbd$FVS_Summary = c(dbd$FVS_Summary,
            c("TPrdTpa","TPrdTCuFt","TPrdMCuFt","TPrdBdFt"))
        if (!is.null(dbd[["FVS_Summary_East"]])) dbd$FVS_Summary_East = 
            c(dbd$FVS_Summary_East,c("TPrdTpa","TPrdMCuFt","TPrdSCuFt","TPrdSBdFt"))
        if (!is.null(dbd[["Composite"]])) dbd$Composite = c(dbd$Composite,
            c("CmpTPrdTpa","CmpTPrdTCuFt","CmpTPrdMCuFt","CmpTPrdBdFt"))
          
        if (length(dbd)) fvsOutData$dbLoadData <- dbd      
        updateSelectInput(session, "selectdbtables", choices=as.list(tbs),
                    selected= intersect(tbs, 
                    c("FVS_Cases","FVS_Summary","FVS_Summary_East")))
        setProgress(value = NULL)          
      }, min=1, max=6)
    } else
    {
      updateSelectInput(session, "selectdbtables", choices=list(" "))
    }
  })
    
  # selectdbtables
  observe({
cat("selectdbtables\n")    
    if (is.null(input$selectdbtables))
    {
      updateSelectInput(session, "selectdbvars", choices=list(" "))               
    } else         
    {
      tables = input$selectdbtables    
      if (!is.null(fvsOutData$dbLoadData$FVS_Cases)) tables = 
        union("FVS_Cases",tables)
      updateSelectInput(session, "selectdbtables", 
                        choices=as.list(names(fvsOutData$dbLoadData)),
                        selected=tables)
      vars = lapply(tables,function (tb,dbd) paste0(tb,".",dbd[[tb]]), 
        fvsOutData$dbLoadData)
      vars = unlist(vars)
      if (length(vars) == 0) return()
      fvsOutData$dbVars    <- vars
      fvsOutData$dbSelVars <- vars
      updateSelectInput(session=session, "selectdbvars",choices=as.list(vars), 
                        selected=vars)
    }
  })

  # selectdbvars
  observe({
    if (!is.null(input$selectdbvars)) 
    {
cat("selectdbvars input$selectdbvars",input$selectdbvars,"\n")      
      fvsOutData$dbSelVars <- input$selectdbvars
    }
  })
    
  ## Custom Query
  observe({  
    if (input$leftPan == "Custom Query")
    { 
cat("Custom Query\n")        
      initTableGraphTools()
      if (length(globals$customQueries) == 0) 
      {
        if (file.exists("customQueries.RData"))
        {
          load("customQueries.RData")
          globals$customQueries = customQueries
        }
      }
      if (length(globals$customQueries) == 0) 
      {
        updateSelectInput(session=session, inputId="sqlSel", 
                          choices=list(),selected=0)
      } else {
        sels = as.list(as.character(1:length(globals$customQueries)))
        names(sels) = names(globals$customQueries)
        updateSelectInput(session=session, inputId="sqlSel", choices=sels, 
           selected = 0)
      }
      updateTextInput(session=session, inputId="sqlTitle", value="")
      updateTextInput(session=session, inputId="sqlQuery", value="")
      updateTextInput(session=session, inputId="sqlOutput", label="", value="")
      output$table <- renderTable(NULL)
    }
  })
     
  ## sqlRunQuery
  observe({ 
    if (input$sqlRunQuery > 0)
    {
cat ("sqlRunQuery\n")      
      isolate({
        msgtxt = character(0)
        qrys = trim(scan(text=gsub("\n"," ",input$sqlQuery),
                    sep=";",what="",quote="",quiet=TRUE))
        output$table <- renderTable(NULL)        
        iq = 0
        dfrtn = NULL
        for (qry in qrys) 
        {
          if (nchar(qry))
          {
            iq = iq+1
            res = try (dbGetQuery(dbGlb$dbOcon,qry))
            msgtxt = if (class(res) == "data.frame") paste0(msgtxt,
              "query ",iq," returned a data frame with ",nrow(res)," rows and ",
              ncol(res)," cols\n") else  
              if (class(res) == "try-error") paste0(msgtxt,"query ",iq,
                " returned\n",attr(res,"condition"),"\n") else
              paste0(msgtxt,"query ",iq," ran\n")         
            updateTextInput(session=session, inputId="sqlOutput", label="", 
                            value=msgtxt)                          
            if (class(res) == "try-error") break
            if (class(res) == "data.frame")
            {
              for (col in 1:ncol(res)) if (class(res[[col]]) == "character") 
                res[[col]] = as.factor(res[[col]])
              if (!is.null(res$Year)) res$Year = as.factor(res$Year)
              fvsOutData$dbData = res
              fvsOutData$render = res
              fvsOutData$runs = character(0)
              fvsOutData$dbVars = colnames(res)
              fvsOutData$browseVars = colnames(res)
              fvsOutData$dbSelVars = character(0)
              fvsOutData$browseSelVars = colnames(res)
              choices = as.list(c("None",
                colnames(res)[unlist(lapply(res, is.factor))]))
              updateSelectInput(session,"pivVar",choices=choices,selected="None")              
              updateSelectInput(session,"hfacet",choices=choices,selected="None") 
              updateSelectInput(session,"vfacet",choices=choices,selected="None") 
              updateSelectInput(session,"pltby", choices=choices,selected="None") 
              choices = as.list(c("None",
                colnames(res)[!unlist(lapply(res, is.factor))]))              
              updateSelectInput(session,"dispVar",choices=choices,selected="None")
              choices = as.list(colnames(res))              
              updateSelectInput(session,"xaxis",choices=choices,selected=colnames(res)[1]) 
              updateSelectInput(session,"yaxis",choices=choices,selected=colnames(res)[1]) 
              if (input$outputRightPan != "Tables")
                updateSelectInput(session,"outputRightPan",selected="Tables")
              if (nrow(res) > 10000) res = res[1:10000,,drop=FALSE]
              output$table <- renderTable(res)
              return()
            }
          }
        }
      })
    }
  })

## sqlSave      
  observe({ 
    if (input$sqlSave > 0)
    {
cat ("sqlSave\n")      
      isolate({
        if (is.null(input$sqlTitle) || input$sqlTitle == "")
        {
          newTit = paste0("Query ",length(globals$customQueries)+1) 
          updateTextInput(session=session, inputId="sqlTitle", value=newTit)
        } else newTit = input$sqlTitle  
       globals$customQueries[[newTit]] = input$sqlQuery
        customQueries = globals$customQueries
        save(file="customQueries.RData",customQueries)
        if (length(globals$customQueries) == 0) 
        {
          updateSelectInput(session=session, inputId="sqlSel", 
                            choices=list(),selected=0)
        } else {
          sels = as.list(as.character(1:length(globals$customQueries)))
          names(sels) = names(globals$customQueries)
          updateSelectInput(session=session, inputId="sqlSel", choices=sels, 
             selected = 0)
        }
      })
    }
  })
   
  ## sqlSel
  observe({  
cat ("sqlSel input$sqlSel=",input$sqlSel," isnull=",
 is.null(input$sqlSel),"\n")
    updateTextInput(session=session, inputId="sqlTitle", value="") 
    updateTextInput(session=session, inputId="sqlOutput", value="")
    updateTextInput(session=session, inputId="sqlQuery", value="")
    output$table <- renderTable(NULL)
    if (!is.null(input$sqlSel))
    {
      sel = as.numeric(input$sqlSel)
cat ("sqlSel sel=",sel,"\n")
      if (length(globals$customQueries) >= sel) 
      {
        updateTextInput(session=session, inputId="sqlTitle", 
          value=names(globals$customQueries)[sel])
        updateTextInput(session=session, inputId="sqlQuery",
          value=globals$customQueries[[sel]])
      }
    }
  })

  ## sqlDelete
  observe({  
    if (input$sqlDelete > 0)
    {
      isolate ({
cat ("sqlDelete is.null(input$sqlTitle)=",is.null(input$sqlTitle),"\n")
        if (is.null(input$sqlTitle)) return()
        globals$customQueries[[input$sqlTitle]] = NULL
        customQueries = globals$customQueries
        save(file="customQueries.RData",customQueries)
        if (length(customQueries) > 0)
        {
          sels = as.list(as.character(1:length(globals$customQueries)))
          names(sels) = names(globals$customQueries)
        } else sels=list()
        updateSelectInput(session=session, inputId="sqlSel", choices=sels, 
           selected = 0)
        updateTextInput(session=session, inputId="sqlTitle", value="")
        updateTextInput(session=session, inputId="sqlQuery", value="")
        updateTextInput(session=session, inputId="sqlOutput", value="")
      })
    }
  })

  ## sqlNew
  observe({  
    if (input$sqlNew > 0)
    {
cat ("sqlNew\n")
        updateSelectInput(session=session, inputId="sqlSel", selected = 0)
        updateTextInput(session=session, inputId="sqlQuery", value="")
        updateTextInput(session=session, inputId="sqlOutput", value="")
    }
  })

  
  ## Explore
  observe({ 
    if (input$leftPan == "Explore")
    { 
cat ("Explore, length(fvsOutData$dbSelVars)=",length(fvsOutData$dbSelVars),"\n") 
      if (length(fvsOutData$dbSelVars) == 0) 
      {
       initTableGraphTools()
       return()
      }
      withProgress(session, 
      {  
        iprg = 1
        setProgress(message = "Processing variable names", detail="",
                    value = iprg)
        tbs = unique(unlist(lapply(strsplit(fvsOutData$dbSelVars,".",fixed=TRUE),
              function (x) x[1])))
        if (length(tbs) == 0) return()
        cols = unique(unlist(lapply(strsplit(fvsOutData$dbSelVars,".",fixed=TRUE),
              function (x) x[2])))
        if (length(cols) == 0) return()
        dat = list()
        for (tb in tbs)       
        {
          iprg = iprg+1
          setProgress(message = "Processing tables", detail=tb,value = iprg)
          if (tb == "Composite" || tb == "Composite_East") 
          {
            dtab = dbReadTable(dbGlb$dbOcon,tb)
            dtab <- ddply(dtab,.(MgmtID),.fun=function (x) 
                   setupSummary(x,composite=TRUE))
            dtab$Year=as.factor(dtab$Year) 
            dtab$MgmtID=as.factor(dtab$MgmtID) 
            dat = list(Composite = dtab)
          } else {
            dtab = if ("CaseID" %in% dbListFields(dbGlb$dbOcon,tb))
              dbGetQuery(dbGlb$dbOcon,paste0("select * from ",tb,
                   " where CaseID in (select CaseID from m.Cases)")) else
              dbGetQuery(dbGlb$dbOcon,paste0("select * from ",tb))
            # fix the stand and stock table.
            if (tb == "StdStk") 
            {
              fix = grep ("Hrv",colnames(dtab))
              if (length(fix)) for (ifx in fix) dtab[[ifx]] = as.numeric(dtab[[ifx]])
            }
            if (tb == "FVS_Summary" || tb == "FVS_Summary_East") 
            { 
              dtab <- ddply(dtab,.(CaseID),.fun=setupSummary)
              dtab$ForTyp =as.factor(dtab$ForTyp)
              dtab$SizeCls=as.factor(dtab$SizeCls)
              dtab$StkCls =as.factor(dtab$StkCls)
            } else if (tb == "FVS_Cases") dtab$RunTitle=trim(dtab$RunTitle)          
            cls = intersect(c(cols,"srtOrd"),colnames(dtab))
            if (length(cls) > 0) dtab = dtab[,cls,drop=FALSE]       
            for (col in colnames(dtab)) if (is.character(dtab[,col])) 
                dtab[,col] = as.factor(dtab[,col])
            if (!is.null(dtab$Year))    dtab$Year    =as.factor(dtab$Year)        
            if (!is.null(dtab$TreeVal)) dtab$TreeVal =as.factor(dtab$TreeVal)        
            if (!is.null(dtab$PtIndex)) dtab$PtIndex =as.factor(dtab$PtIndex)        
            if (!is.null(dtab$SSCD))    dtab$SSCD    =as.factor(dtab$SSCD) 
            rownames(dtab) = 1:nrow(dtab)
            dat[[tb]] = dtab
          }
        }
cat ("Explore, len(dat)=",length(dat),"\n") 
        mdat = dat$FVS_Cases
        dat$FVS_Cases = NULL
        if (is.null(mdat))  mdat = dat[[1]]
        if (is.null(mdat))
        {
          initTableGraphTools()
          return()
        }

        
        iprg = iprg+1
        setProgress(message = "Merging selected tables", detail  = "", value = iprg)
        if (length(dat) > 0)
        {
          #avoid name conflicts with the TreeList table and others.
          if (!is.null(dat[["FVS_TreeList"]]))
          {
            toren = c("TCuFt", "MCuFt", "BdFt", "PrdLen")
            cols = match(toren,names(dat[["FVS_TreeList"]]))
            names(dat[["FVS_TreeList"]])[cols] = paste0("T.",toren)
          } 
          for (tb in names(dat))
          { 
            mrgVars = intersect(names(mdat),c("CaseID","Year","StandID","Species"))
            mrgVars = intersect(mrgVars,names(dat[[tb]]))
            setProgress(message = "Merging selected tables", 
                        detail  = tb, value = iprg)
cat ("tb=",tb," mrgVars=",mrgVars,"\n")                        
            mdat = merge(mdat,dat[[tb]], by=mrgVars)
          }
          fvsOutData$dbData = mdat
        } else fvsOutData$dbData = mdat #happens when only FVS_Cases is selected  
        iprg = iprg+1
        setProgress(message = "Processing variables", detail=tb,value = iprg)
        mdat = fvsOutData$dbData
        vars = colnames(mdat)
        sby = intersect(c("MgmtID","StandID","Stand_CN","Year","PtIndex",
                  "TreeIndex","Species","DBHClass","RunDateTime"),vars)
        sby = if (length(sby)) 
        {
          cmd = paste0("order(",paste(paste0("mdat$",sby),collapse=","),
               if("srtOrd" %in% vars) ",mdat$srtOrd)" else ")")
          sby = try(eval(parse(text=cmd)))
          if (class(sby) == "try-error") NULL else sby
        } else NULL
        vars = intersect(c("MgmtID","Stand_CN","StandID","Year",
                           "Species","DBHClass"),colnames(mdat))
        vars = c(vars,setdiff(colnames(mdat),vars))
        endvars = intersect(c("SamplingWt","Variant","RunTitle",
                         "Groups","RunDateTime","KeywordFile","CaseID"),vars)
        vars = union(setdiff(vars,endvars),endvars)
        if (!is.null(sby)) mdat = mdat[sby,vars,drop=FALSE]
        mdat$srtOrd = NULL
        vars = colnames(mdat)     
        if (length(vars) == 0) 
        {
          setProgress(value = NULL)  
          return()
        }
        iprg = iprg+1
        setProgress(message = "Loading selection widgets", detail  = "", value = iprg)
        if (is.null(mdat$RunTitle)) 
          updateSelectInput(session, "stdtitle", choices  = list("None loaded"), 
            selected = NULL) else 
          updateSelectInput(session, "stdtitle", 
            choices=as.list(levels(mdat$RunTitle)), selected=levels(mdat$RunTitle))      
        iprg = iprg+1
        setProgress(message = "Loading selection widgets", detail  = "", value = iprg)
        if (is.null(mdat$StandID)) 
          updateSelectInput(session, "stdid", choices  = list("None loaded"), 
            selected = NULL) else 
          {
            if (length(levels(mdat$StandID)) > 500) 
              updateSelectInput(session, "stdid", 
                choices=as.list(paste0("None loaded (",
                length(levels(mdat$StandID))," stands)")), selected=NULL) else
              updateSelectInput(session, "stdid", 
                choices=as.list(levels(mdat$StandID)), selected=levels(mdat$StandID))
          }
        if (is.null(mdat$Groups)) updateSelectInput(session, "stdgroups", 
            choices  = list("None loaded"), selected = NULL) else
          {
            grps = sort(unique(unlist(lapply(levels(mdat$Groups), function (x)
              trim(scan(text=x,what="character",sep=",",quiet=TRUE))))))           
            updateSelectInput(session, "stdgroups",choices=as.list(grps), 
              selected=grps)
          }
        if (is.null(mdat$MgmtID)) updateSelectInput(session, "mgmid", 
            choices  = list("None loaded"), selected = NULL) else 
          updateSelectInput(session, "mgmid",choices=as.list(levels(mdat$MgmtID)), 
            selected=levels(mdat$MgmtID))
        if ("FVS_TreeList" %in% names(dat))
          updateSelectInput(session, "plotType",selected="scatter") else 
          if ("StdStk" %in% names(dat)) 
            updateSelectInput(session, "plotType",selected="bar") else
              updateSelectInput(session, "plotType",selected="line")
        iprg = iprg+1
        setProgress(message = "Loading selection widgets", detail  = "", value = iprg)
        if (is.null(mdat$Year)) updateSelectInput(session, "year", 
            choices  = list("None loaded"), selected = NULL) else 
          {
            sel  = levels(mdat$Year)
            isel = max(1,length(sel) %/% 2)
            sel =  if (length(intersect(c("FVS_TreeList","StdStk"),names(dat)))) 
                   sel[isel] else sel 
            updateSelectInput(session, "year", choices=as.list(levels(mdat$Year)), 
              selected=sel)
          }
        if (is.null(mdat$Species)) updateSelectInput(session, "species", 
            choices  = list("None loaded"), selected = NULL) else
          {
            sel = names(sort(table(mdat$Species),decreasing=TRUE))
            sel = setdiff(sel,"All")
            sel = sel[1:min(length(sel),5)]
            updateSelectInput(session, "species",
              choices=as.list(levels(mdat$Species)), selected=sel)
          }
        if (is.null(mdat$DBHClass)) updateSelectInput(session, "dbhclass", 
            choices  = list("None loaded"), selected = NULL) else
          {
            sel = if ("All" %in% levels(mdat$DBHClass)) "All" else 
              { 
                top = names(sort(table(mdat$DBHClass),decreasing=TRUE))
                top[1:min(length(top),5)]
              }
            updateSelectInput(session, "dbhclass", 
              choices=as.list(levels(mdat$DBHClass)), selected=sel)
          }           
        iprg = iprg+1
        setProgress(message = "Finishing", detail  = "", value = iprg)
        selVars = unlist(lapply(c("StandID","MgmtID","Year","^DBH","^DG$",
          "AGE","CCF","SDI","QMD","TopHt","^BA$","TPA","Species","^Ht$",
          "^HtG$","CuFt$","BdFt$","Total","HrvPA","RunTitle"),
          function (x,vs) 
            {
              hits = unlist(grep(x,vs,ignore.case = TRUE))
              hits = hits[hits>0]
              vs[hits]
            },vars))
        keep = unlist(lapply(selVars,function(x,mdat) !all(is.na(mdat[,x])),mdat))
        selVars = selVars[keep]
        if (length(vars) < 7) selVars = vars
        updateCheckboxGroupInput(session, "browsevars", choices=as.list(vars), 
                                 selected=selVars,inline=TRUE)                               
        fvsOutData$dbData        <- mdat
        fvsOutData$browseVars    <- vars
        fvsOutData$browseSelVars <- selVars
        setProgress(value = NULL)          
      }, min=1, max=10)
    } 
  })
  
  ## renderTable
  renderTable <- function (dat)
  {
cat ("renderTable, is.null=",is.null(dat)," nrow(dat)=",nrow(dat),"\n")
    if (!is.null(dat) && nrow(dat) > 0)
    {
      dat = lapply(dat,function (x) 
        if (is.factor(x)) levels(x)[as.numeric(x)] else x)
      dat = as.data.frame(dat)
    }
    renderRHandsontable(if (is.null(dat) || nrow(dat)==0) NULL else 
              rhandsontable(dat,readOnly=TRUE,useTypes=FALSE,contextMenu=FALSE,
              width="100%",height=700))
  }
         
  observe({
cat("filterRows and/or pivot\n")
    if (is.null(input$browsevars)) return()
    fvsOutData$browseSelVars <- input$browsevars
    dat = if (length(input$stdtitle) || length(input$stdgroups) ||   
              length(input$stdid)    || length(input$mgmid)     || 
              length(input$year)     || length(input$species)   || 
              length(input$dbhclass))                       
      fvsOutData$dbData[filterRows(fvsOutData$dbData, input$stdtitle, input$stdgroups,
          input$stdid, input$mgmid, input$year, input$species, input$dbhclass)
          ,fvsOutData$browseSelVars,drop=FALSE] else 
      fvsOutData$dbData[,fvsOutData$browseSelVars,drop=FALSE]  
    if (!is.null(input$pivVar)  && input$pivVar  != "None" &&
        !is.null(input$dispVar) && input$dispVar != "None")  
          dat = pivot(dat,input$pivVar,input$dispVar)
    fvsOutData$render = dat    
    if (nrow(dat) > 10000) dat = dat[1:10000,,drop=FALSE]
    output$table <- renderTable(dat) 
  })
           
  
  ##browsevars 
  observe({
    if (!is.null(input$browsevars)) 
    {
cat ("browsevars\n")
      fvsOutData$browseSelVars <- input$browsevars  
      cats = unlist(lapply(fvsOutData$dbData,is.factor))
      cats = names(cats)[cats]
      cats = intersect(cats,input$browsevars)
      cont = union("Year",setdiff(input$browsevars,cats))
      
      spiv  = if (length(input$pivVar) && 
                  input$pivVar %in% cats) input$pivVar else "None"
      sdisp = if (length(input$dispVar) && 
                  input$dispVar %in% input$browsevars) 
                                          input$dispVar else "None"
      ccont = c("None",setdiff(input$browsevars,spiv))
      bb = intersect(ccont,cats) # put the factors at the end of the choices
      ccont = c(setdiff(ccont,bb),bb)
      updateSelectInput(session,"pivVar",choices=as.list(c("None",cats)),
                      selected=spiv)    
      updateSelectInput(session,"dispVar",choices=as.list(ccont),
                      selected=sdisp)
      if (input$plotType == "line" || input$plotType == "scatter")
      {
        sel = if ("Year" %in% cont) "Year" else 
                if (length(cont) > 0) cont[1] else NULL
        if (sel=="Year" && input$plotType == "scatter" && length(cont) > 1)
        {
          sel = setdiff(cont,"Year")
          sel = if ("DBH" %in% cont) "DBH" else cont[1]
        }
        updateSelectInput(session, "xaxis",choices=as.list(cont), selected=sel)
      } else {
        sel = if ("Species" %in% cats) "Species" else 
               if (length(cats) > 0) cats[1] else NULL       
        updateSelectInput(session, "xaxis",choices=as.list(cats), selected=sel)
      }
      sel = setdiff(cont,c(sel,"Year"))
      sel = if ("DG" %in% cont && input$plotType == "scatter" ) "DG" else sel[1]
      updateSelectInput(session, "yaxis",choices=as.list(cont),
                      selected=if (length(sel) > 0) sel else NULL)     
      sel = if (length(intersect(cats,"StandID")) > 0) "StandID" else "None"
      updateSelectInput(session, "hfacet",choices=as.list(c("None",cats)),
        selected=sel) 
      sel = if (length(intersect(cats,"MgmtID")) > 0) "MgmtID" else "None"
      updateSelectInput(session, "vfacet",choices=as.list(c("None",cats)),
        selected=sel) 
      sel = if (length(intersect(cats,"Species")) > 0) "Species" else "None"
      updateSelectInput(session, "pltby",choices=as.list(c("None",cats)),
        selected=sel)                                                                   
    }
  })
  
  ## selectdbvars
  observe({
    if (!is.null(input$selectdbvars)) fvsOutData$dbSelVars <- input$selectdbvars
  })

  ## renderPlot
  output$outplot <- renderImage(
  {   
cat ("renderPlot\n")    
    nullPlot <- function ()
    {
      outfile = "nullPlot.png"
      if (!file.exists(outfile))
      {
        png(outfile, width=3, height=2, res=72, units="in", pointsize=12)              
        plot.new()
        text(x=.5,y=.5,"Nothing to graph",col="red")
        dev.off()
      }
      list(src = outfile)
    }

    if (input$leftPan == "Load"  || (length(input$xaxis) == 0 && 
        length(input$yaxis) == 0)) return(nullPlot())

    vf = if (input$vfacet == "None") NULL else input$vfacet
    hf = if (input$hfacet == "None") NULL else input$hfacet
    pb = if (input$pltby  == "None") NULL else input$pltby

    dat = if (input$leftPan == "Custom Query") fvsOutData$dbData else
      droplevels(fvsOutData$dbData[filterRows(fvsOutData$dbData, input$stdtitle, 
          input$stdgroups, input$stdid, input$mgmid, input$year, input$species, 
          input$dbhclass),])
cat ("vf=",vf," hf=",hf," pb=",pb," xaxis=",input$xaxis," yaxis=",input$yaxis,"\n")
    if (!is.null(hf) && (nlevels(dat[,hf]) == 1 || nlevels(dat[,hf]) > 8))
    {
cat ("hf test, nlevels(dat[,hf])=",nlevels(dat[,hf]),"\n")
      updateSelectInput(session=session, inputId="hfacet", selected="None")
      return (nullPlot())
    }
    if (!is.null(vf) && (nlevels(dat[,vf]) == 1 || nlevels(dat[,vf]) > 8))
    {
cat ("vf test hit, nlevels(dat[,vf])=",nlevels(dat[,vf]),"\n")
      updateSelectInput(session=session, inputId="vfacet", selected="None")
      return (nullPlot())
    }
          
    if (!is.null(pb) && pb %in% colnames(dat) && 
         input$pltby %in% c(input$xaxis,input$yaxis,vf,hf))
    {
cat ("pb test hit\n")
      if (input$pltby == input$xaxis)
        updateSelectInput(session=session, inputId="pltby", selected="None")
      else if (input$pltby == input$yaxis)
        updateSelectInput(session=session, inputId="pltby", selected="None")
      else if (input$pltby == vf)
        updateSelectInput(session=session, inputId="vfacet", selected="None")
      else if (input$pltby == hf)
        updateSelectInput(session=session, inputId="hfacet", selected="None")
      return (nullPlot())
    }
    if (!is.null(hf) && hf %in% colnames(dat) && 
        input$hfacet %in% c(input$xaxis,input$yaxis,pb,vf))
    {
cat ("hfacet test hit\n")
      if (!is.null(input$xaxis) && input$hfacet == input$xaxis)
        updateSelectInput(session=session, inputId="xaxis", selected=NULL)
      else if (!is.null(input$yaxis) && input$hfacet == input$yaxis)
        updateSelectInput(session=session, inputId="yaxis", selected=NULL)
      else if (input$hfacet == input$pltby)
        updateSelectInput(session=session, inputId="pltby", selected="None")
      else if (input$hfacet == vf)
        updateSelectInput(session=session, inputId="vfacet", selected="None")
      return (nullPlot())
    }
    if (!is.null(vf) && vf %in% colnames(dat) && 
        input$vfacet %in% c(input$xaxis,input$yaxis,pb,hf))
    {
cat ("vfacet test hit\n")
      if (!is.null(input$xaxis) && input$vfacet == input$xaxis)
        updateSelectInput(session=session, inputId="xaxis", selected=NULL)
      else if (!is.null(input$yaxis) && input$vfacet == input$yaxis)
        updateSelectInput(session=session, inputId="yaxis", selected=NULL)
      else if (input$vfacet == input$pltby)
        updateSelectInput(session=session, inputId="pltby", selected="None")
      else if (input$vfacet == hf)
        updateSelectInput(session=session, inputId="hfacet", selected="None")
      return (nullPlot())
    }   
    nlv  = 1 + (!is.null(pb)) + (!is.null(vf)) + (!is.null(hf))    
    vars = c(input$xaxis, vf, hf, pb, input$yaxis)                                        
    if (input$xaxis == "Year" && input$plotType != "box" && 
        input$plotType != "bar") dat$Year = as.numeric(as.character(dat$Year))
    nd = NULL
    for (v in vars[(nlv+1):length(vars)])
    {
      if (is.na(v) || !v %in% names(dat)) return(nullPlot())
      pd = dat[,c(vars[1:nlv],v),drop=FALSE]
      names(pd)[ncol(pd)] = "Y"
      nd = rbind(nd, data.frame(pd,Legend=v,stringsAsFactors=FALSE))
    }
    nd = na.omit(nd)
    if (length(nd) == 0) return(nullPlot())
    rownames(nd)=1:nrow(nd)
    names(nd)[match(input$xaxis,names(nd))] = "X"
    if (!is.null(vf)) names(nd)[match(vf,names(nd))] = "vfacet"
    if (!is.null(hf)) names(nd)[match(hf,names(nd))] = "hfacet"      
    if (!is.null(pb) && !is.null(nd$Legend)) 
    {
      alv = nlevels(as.factor(nd$Legend))
      nd$Legend = if (alv == 1) paste(pb,nd[,pb],sep=":") else
                                paste(nd$Legend,pb,nd[,pb],sep=":")
    }      
    if (!is.null(nd$vfacet)) nd$vfacet = as.factor(nd$vfacet)
    if (!is.null(nd$hfacet)) nd$hfacet = as.factor(nd$hfacet)
    if (!is.null(nd$Legend)) nd$Legend = as.factor(nd$Legend)
    fg = NULL
    fg = if (!is.null(nd$vfacet) && !is.null(nd$hfacet)) 
         facet_grid(vfacet~hfacet)
    fg = if (is.null(fg)         && !is.null(nd$hfacet)) 
         facet_grid(.~hfacet) else fg
    fg = if (is.null(fg)         && !is.null(nd$vfacet)) 
         facet_grid(vfacet~.) else fg
    if (input$plotType %in% c("bar","box") && is.factor(nd$Y))
    {
      flip = TRUE
      nd$temp = nd$X
      nd$X    = nd$Y
      nd$Y    = nd$temp
      nd$temp = NULL
    } else flip = FALSE
    p = ggplot(data = nd) + fg + labs(
          x=if (nchar(input$xlabel)) input$xlable else input$xaxis, 
          y=if (nchar(input$ylabel)) input$ylabel else input$yaxis, 
          title=input$ptitle)  + 
          theme(text = element_text(size=9),
            panel.background = element_rect(fill="gray95"),
            axis.text = element_text(color="black"))
    if (flip) p = p + coord_flip() 
    colors = if (input$colBW == "B&W") rep(rgb(0,0,0,seq(.5,.9,.05)),5) else
      ggplotColours(n=nlevels(nd$Legend)+1)
    if (!is.null(colors)) p = p + scale_colour_manual(values=colors)
    if (nlevels(nd$Legend)>6) p = p +
      scale_shape_manual(values=1:nlevels(nd$Legend))
    alpha = approxfun(c(50,100,1000),c(1,.7,.4),rule=2)(nrow(nd))    
    size  = approxfun(c(50,100,1000),c(1,.7,.5),rule=2)(nrow(nd))
    if (is.factor(nd$X)) nd$X = as.ordered(nd$X)
    if (is.factor(nd$Y)) nd$Y = as.ordered(nd$Y)
    plt = switch(input$plotType,
      line    = if (input$colBW == "B&W") 
        geom_line  (aes(x=X,y=Y,color=Legend,linetype=Legend)) else
        geom_line  (aes(x=X,y=Y,color=Legend),alpha=.8),
      scatter = if (input$colBW == "B&W") 
        geom_point (aes(x=X,y=Y,shape=Legend,color=Legend),size=size) else
        geom_point (aes(x=X,y=Y,shape=Legend,color=Legend),
                          alpha=alpha,size=size),
      bar     = if (input$colBW == "B&W")
        geom_bar (aes(x=X,y=Y,color=Legend,fill=Legend),
           position="dodge",stat="identity") else
        geom_bar (aes(x=X,y=Y,color=Legend,fill=Legend),alpha=.8,
           position="dodge",stat="identity"),
      box     = if (input$colBW == "B&W") 
        geom_boxplot (aes(x=X,y=Y,color=Legend,linetype=Legend)) else
        geom_boxplot (aes(x=X,y=Y,color=Legend),alpha=alpha)
      )  
    if (input$colBW == "B&W" && input$plotType == "bar") 
      p = p + scale_fill_grey(start=.15, end=.85)
    p = p + theme(text=element_text(size=9))
    if (nlevels(nd$Legend)==1 || nlevels(nd$Legend)>9) p = p + theme(legend.position="none")
    if (input$colBW == "color") p = p + scale_colour_brewer(palette = "Set1")
    outfile = "plot.png" 
    fvsOutData$plotSpecs$res    = as.numeric(input$res)
    fvsOutData$plotSpecs$width  = as.numeric(input$width)
    fvsOutData$plotSpecs$height = as.numeric(input$height)
        
    png(outfile, width=fvsOutData$plotSpecs$width, 
                 height=fvsOutData$plotSpecs$height, units="in", 
                 res=fvsOutData$plotSpecs$res)              
    print(p + plt)
    dev.off()
    list(src = outfile)            
  }, deleteFile = FALSE)
    
  ## Stands tab 
  observe({    
    if (input$topPan == "Runs" || input$rightPan == "Stands") 
    {
cat ("Stands\n")
      updateVarSelection()
    }
  })
  
  updateVarSelection <- function ()
  { 
cat ("in updateVarSelection\n")   
    if (length(globals$fvsRun$FVSpgm) == 0) 
    {
      selVarListUse <- intersect(names(globals$selVarList),
                                 globals$activeVariants)
      selVarListUse <- globals$selVarList[selVarListUse]                                   
      vlst <- as.list (names(selVarListUse))                                
      names(vlst) = selVarListUse        
    } else {
      if (is.null(globals$activeFVS[[globals$fvsRun$FVSpgm]])) vlst <- list() else
      {
        vlst <- as.list(globals$activeFVS[globals$fvsRun$FVSpgm][[1]][[1]][1])
        names(vlst) <- globals$selVarList[[vlst[[1]]]]
      }
    }
    updateSelectInput(session=session, inputId="inVars", choices=vlst,
          selected=if (length(vlst)) vlst[[1]] else NULL)
  } 
  
  ## inVars has changed
  observe({
    if (is.null(input$inVars)) return()
cat ("inVars\n")    
    grps = try(dbGetQuery(dbGlb$dbIcon,paste0('select Stand_ID,Groups from ',
     'FVS_StandInit where lower(variant) like "%',input$inVars,'%"')))
    if (class(grps) == "try-error" || nrow(grps) == 0)
    {
      dbSendQuery(dbGlb$dbIcon,'drop table if exists m.Grps') 
      dbWriteTable(dbGlb$dbIcon,"m.Grps",data.frame(Stand_ID="",Grp=""))
      updateSelectInput(session=session, inputId="inGrps",choices=list())
      updateSelectInput(session=session, inputId="inStds",list())
    } else {
      dd = apply(grps,1,function (x)
        { 
          gr=unlist(strsplit(x[2]," "))
          st=rep(x[1],length(gr))
          attributes(st) = NULL
          attributes(gr) = NULL
          list(st,gr)    
        })
      dd = lapply(dd,function(x) matrix(unlist(x),ncol=2))
      dd = do.call(rbind,dd)
      colnames(dd) = c("Stand_ID","Grp")
      dd = as.data.frame(dd)
      dbSendQuery(dbGlb$dbIcon,'drop table if exists m.Grps') 
      dbWriteTable(dbGlb$dbIcon,"m.Grps",dd)
      selGrp = dbGetQuery(dbGlb$dbIcon,
        'select distinct Grp from m.Grps order by Grp')[,1]
      updateSelectInput(session=session, inputId="inGrps", 
              choices=as.list(selGrp))
      updateSelectInput(session=session, inputId="inStds", 
           choices=list())
      output$stdSelMsg <- output$stdSampTools <- renderUI(NULL)
    }
  })

  ## inGrps or inAnyAll has changed
  observe({
    if (input$topPan == "Runs" || input$rightPan == "Stands")
    {
cat ("inGrps\n")     
      if (is.null(input$inGrps))          
      {
        output$stdSelMsg <- output$stdSampTools <- renderUI(NULL)
        updateSelectInput(session=session, inputId="inStds", 
           choices=list())
      } else {
        dbSendQuery(dbGlb$dbIcon,'drop table if exists m.SGrps') 
        dbWriteTable(dbGlb$dbIcon,"m.SGrps",data.frame(SelGrps = input$inGrps))
        stds = try(dbGetQuery(dbGlb$dbIcon,paste0('select Stand_ID from m.Grps ',
             'where Grp in (select SelGrps from m.SGrps)')))
        if (class(stds) == "try-error") return()                                                             
cat ("inGrps, nrow(stds)=",nrow(stds),"\n")
        stds = stds[,1]
        stds = if (input$inAnyAll == "Any") unique(stds) else
        {
          stdCnts = table(stds) 
          stds = names(stdCnts[stdCnts == length(input$inGrps)])                                                                                                                       
        }                                                                                                
        nstds = length(stds)
        msg = paste0(length(stds)," Stand(s) in ",length(input$inGrps)," Group(s)<br>")
        output$stdSelMsg <- renderUI(HTML(msg))
        if (length(stds) > 220)  stds = c(stds[1:200],
          paste0("<< Display 201 to ",min(400,length(stds))," of ",length(stds)," >>"))
        updateSelectInput(session=session, inputId="inStds", 
             choices=as.list(stds))
        output$stdSampTools <- if (length(stds)) 
        {         
          output$stdSampTools <- renderUI(list(
            myInlineTextInput("smpSize", "Sample size ", max(1,floor(.5*nstds))),
            myRadioGroup("smpWithReplace","Sample with replacement",c("No","Yes")),
            actionButton("inAddSample","Select a sample of stands from selected groups")))
        } else renderUI(NULL)
      }
    }
  })
  ## inStds has changed
  observe({
    if (length(input$inStds) != 1) return()
    prts = unlist(strsplit(input$inStds[1]," "))
    if (prts[1] != "<<") return()
    stds = try(dbGetQuery(dbGlb$dbIcon,paste0('select Stand_ID from m.Grps ',
         'where Grp in (select SelGrps from m.SGrps)')))
    if (class(stds) == "try-error") return()
    stds = stds[,1]
    stds = if (isolate(input$inAnyAll) == "Any") unique(stds) else
    {
      stdCnts = table(stds) 
      stds = stdCnts[stdCnts == length(input$inGrps)]                                                                                                                           
    }                                                                                                
cat ("inStds, length(stds)=",length(stds),"\n")
    if (length(stds) < 220) return() 
    nprts = as.numeric(prts[c(3,5,7)])
cat ("nprts=",nprts,"\n")
    up = nprts[c(1,2)] - 200
    if (up[2]-up[1] < 200) up[2] = min(up[1]+200,length(stds))
    upM = if (up[1] > 0) paste0("<< Display ",up[1]," to ",
      min(up[2],length(stds))," of ",length(stds)," >>") else NULL
    dn = nprts[c(1,2)] + 200
    if (dn[2]-dn[1] < 200) dn[2] = min(dn[1]+200,length(stds))
    dn[2] = min(dn[2],length(stds))
    dnM = if (dn[1] <= length(stds)) paste0("<< Display ",dn[1]," to ",
      dn[2]," of ",length(stds)," >>") else NULL
    stds = c(upM,stds[nprts[1]:nprts[2]],dnM)
cat ("inStds upM=",upM," dnM=",dnM,"\n")    
    updateSelectInput(session=session, inputId="inStds", 
         choices=as.list(stds))   
  })
        

  ## New run    
  observe({
    if (input$newRun > 0)
    {
      resetfvsRun(globals$fvsRun,globals$FVS_Runs)
      globals$fvsRun$title <- paste0("Run ",length(globals$FVS_Runs)+1)
      resetGlobals(globals,NULL,prms)
      loadVarData(globals,prms,dbGlb$dbIcon)
      updateTextInput(session=session, inputId="title", label="", 
                      value=globals$fvsRun$title) 
      updateTextInput(session=session, inputId="defMgmtID",
                     value=globals$fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="simCont", 
          choices=list(), selected=NULL)
      output$contCnts <- renderUI(HTML(paste0("<b>Contents</b><br>",
        length(globals$fvsRun$stands)," stand(s)<br>",
        length(globals$fvsRun$grps)," group(s)")))
      updateSelectInput(session=session, inputId="addCategories", 
          choices=list(), selected=NULL)
      updateSelectInput(session=session, inputId="addComponents", 
          choices=list(), selected=NULL)
      updateTextInput(session=session, inputId="startyr", 
                      value=globals$fvsRun$startyr)
      updateTextInput(session=session, inputId="endyr", 
                      value=globals$fvsRun$endyr)
      updateTextInput(session=session, inputId="cyclelen", 
                      value=globals$fvsRun$cyclelen)
      updateTextInput(session=session, inputId="cycleat", 
                      value=globals$fvsRun$cycleat)
      output$runProgress <- renderUI(NULL)
      updateSelectInput(session=session, inputId="cmdSet", 
                        selected="Management")
      updateSelectInput(session=session, inputId="runScript", 
                        selected="fvsRun")
      isolate ({
        if (!is.null(input$inVars)) 
        {
          selVarListUse <- intersect(globals$activeVariants,names(globals$selVarList))
          if (length(selVarListUse)) 
          {
            selVarListUse <- globals$selVarList[selVarListUse]
            vlst <- as.list (names(selVarListUse))
            names(vlst) = selVarListUse
            updateSelectInput(session=session, inputId="inVars", NULL, 
                          vlst, vlst[[1]])
          } else updateSelectInput(session=session, inputId="inVars", NULL, list())
          updateSelectInput(session=session, inputId="inGrps", NULL, NULL)
          updateSelectInput(session=session, inputId="inStds", NULL, NULL)
          if (input$rightPan != "Stands")
          {
            updateTabsetPanel(session=session, inputId="rightPan", 
               selected="Stands")
          }
        }
      })
    }
  })    
      

  ## Duplicate run
  observe({
    if (input$dupRun > 0)
    {
      if (length(globals$FVS_Runs) == 0) return()
      saveRun()
      globals$fvsRun$title <- paste0("Run ",length(globals$FVS_Runs)+1)
      globals$fvsRun$uuid  <- uuidgen()
      globals$fvsRun$defMgmtID = sprintf("A%3.3d",length(globals$FVS_Runs)+1)
      globals$FVS_Runs[[globals$fvsRun$uuid]] = globals$fvsRun$title
      FVS_Runs = globals$FVS_Runs
      save (file="FVS_Runs.RData",FVS_Runs)
      resetGlobals(globals,NULL,prms)
      updateTextInput(session=session, inputId="title", label="", 
                      value=globals$fvsRun$title) 
      updateTextInput(session=session, inputId="defMgmtID",
                      value=globals$fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="cmdSet", 
                        selected="Management")
    }
  })    


  ## Reload or Run Selection   
  observe({
    if (input$reload > 0 || !is.null(input$runSel))
    {
cat ("reload or run selection, runSel=",input$runSel," lensim=",
length(globals$fvsRun$simcnts)," globals$currentQuickPlot=",globals$currentQuickPlot,"\n")      
      if (length(globals$currentQuickPlot) &&
          globals$currentQuickPlot != input$runSel)
      {
cat("setting uiRunPlot to NULL\n")        
        output$uiRunPlot <- output$uiErrorScan <- renderUI(NULL)
        globals$currentQuickPlot = character(0)
      }
      progress <- shiny::Progress$new(session,min=1,max=5)
      progress$set(message = "Loading selected run",value = 1)
      resetGlobals(globals,NULL,prms)
      sel = match (input$runSel,names(globals$FVS_Runs)) 
      if (is.na(sel)) sel = 1
      fn=paste0(names(globals$FVS_Runs)[sel],".RData")
      if (file.exists(fn)) 
      {
        ret = try (load(file=fn))  # maybe the file has been corrupted
        if (class(ret) == "try-error")
        {
          cat ("error loading",fn,"\n")
          unlink(fn)
          globals$FVS_Runs = globals$FVS_Runs[-sel]
          saveFvsRun=globals$fvsRun
          if (length(globals$FVS_Runs) == 0) unlink("FVS_Runs.RData") else
          {
            FVS_Runs = globals$FVS_Runs
            save (file="FVS_Runs.RData",FVS_Runs)
          }
        } 
        globals$fvsRun = saveFvsRun
      }
      resetGlobals(globals,globals$fvsRun,prms)
      mkSimCnts(globals$fvsRun,globals$fvsRun$selsim)
      output$uiCustomRunOps = renderUI(NULL)    
cat ("reloaded globals$fvsRun$title=",globals$fvsRun$title," uuid=",globals$fvsRun$uuid,"\n")      
cat ("reloaded globals$fvsRun$runScript=",globals$fvsRun$runScript,"\n")
      if (length(globals$fvsRun$uiCustomRunOps)) lapply(names(globals$fvsRun$uiCustomRunOps), function (x,y)
cat ("globals$fvsRun$uiCustomRunOps$",x,"=",y[[x]],"\n",sep=""),globals$fvsRun$uiCustomRunOps) else
cat ("globals$fvsRun$uiCustomRunOps is empty\n")

      isolate({
        if (input$rightPan != "Run" && length(globals$fvsRun$simcnts)>0)
        {
          if (input$rightPan != "Components" && length(globals$fvsRun$simcnts)>0)
          {
            updateTabsetPanel(session=session, inputId="rightPan", 
               selected="Components")
          }
          if (input$rightPan != "Stands" && length(globals$fvsRun$simcnts)==0)
          {
            updateTabsetPanel(session=session, inputId="rightPan", 
               selected="Stands")
          }
        }
      })
      progress$set(message = paste0("Setting values for run ", globals$fvsRun$title),
            value = 2)
      updateCheckboxGroupInput(session=session, inputId="autoOut",
        selected=globals$fvsRun$autoOut)
      updateTextInput(session=session, inputId="title", value=globals$fvsRun$title)
      updateTextInput(session=session, inputId="defMgmtID",
                      value=globals$fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="addComponents", 
          choices=list(" "), selected=NULL)
      updateTextInput(session=session, inputId="startyr",  
                      value=globals$fvsRun$startyr)
      updateTextInput(session=session, inputId="endyr",    
                      value=globals$fvsRun$endyr)
      updateTextInput(session=session, inputId="cyclelen", 
                      value=globals$fvsRun$cyclelen)
      updateTextInput(session=session, inputId="cycleat",  
                      value=globals$fvsRun$cycleat)
      progress$set(message = paste0("Setting simulation contents for run ", 
        globals$fvsRun$title),value = 3)
      updateSelectInput(session=session, inputId="simCont", 
        choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
      updateVarSelection()
      output$contCnts <- renderUI(HTML(paste0("<b>Contents</b><br>",
        length(globals$fvsRun$stands)," stand(s)<br>",
        length(globals$fvsRun$grps)," group(s)")))
      # if the update causes a change in the runscript selection, then
      # customRunOps will get called automatically. If it is the same
      # script then it needs to be called here to update/set the settings.
      progress$set(message = "Setting custom run options ",value = 4)
      isolate ({callCustom = globals$fvsRun$runScript == input$runScript})
      updateSelectInput(session=session, inputId="runScript", 
          selected=globals$fvsRun$runScript)
      if (callCustom) customRunOps()
      progress$close()
    }
  })

  ##autoOut
  observe(globals$fvsRun$autoOut<-as.list(input$autoOut))
  
  ## Save saveRun  
  observe({
    if (input$saveRun > 0)                  
    {
cat ("saveRun\n")
      saveRun()
      selChoices = names(globals$FVS_Runs) 
      names(selChoices) = globals$FVS_Runs
      updateSelectInput(session=session, inputId="runSel", 
          choices=selChoices,selected=selChoices[[1]])
      FVS_Runs = globals$FVS_Runs
      save (FVS_Runs,file="FVS_Runs.RData")
    } 
  })

  ## inAdd:    Add Selected Stands
  observe({
    if (input$inAdd > 0) 
    {
cat ("input$inAdd=",input$inAdd,"\n")
      addStandsToRun(session,input,output,selType="inAdd",globals,dbGlb)
    }
  })  
  ## inAddGrp: Add all stands in selected groups
  observe({
    if (input$inAddGrp > 0) 
    {
cat (" input$inAddGrp=",input$inAddGrp,"\n")
      addStandsToRun(session,input,output,selType="inAddGrp",globals,dbGlb)
    }
  })  
  ## inAddGrp: Add all stands in selected groups
  observe({
    if (length(input$inAddSample) &&  input$inAddSample > 0) 
    {
cat (" input$inAddGrp=",input$inAddGrp,"\n")
      addStandsToRun(session,input,output,selType="inAddSample",globals,dbGlb)
    }
  })  
  
  ## run element selection
  observe({
    if (length(input$simCont) == 0) return()
cat ("run element selection\n")
    if (all(input$simCont == globals$fvsRun$selsim)) return()
    mkSimCnts(globals$fvsRun,input$simCont[[1]])
    updateSelectInput(session=session, inputId="simCont", 
         choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
  })
  
  ## findStand (set run element to item if found.
  observe({
    if (input$searchNext== 0) return()      
    isolate ({
cat ("searchNext: string=",input$searchString,"\n")
      if (nchar(input$searchString) == 0) return()
    elt = findStand(globals,search=input$searchString)
cat ("elt=",elt,"\n")
    if (is.null(elt)) return()
    mkSimCnts(globals$fvsRun,elt,globals$foundStand)
    updateSelectInput(session=session, inputId="simCont", 
         choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
  })})

  ## Edit  
  observe({
    if (input$editSel == 0) return()      
    isolate ({
      globals$currentEditCmp <- globals$NULLfvsCmp
      updateSelectInput(session=session, inputId="addComponents", selected=0)
      if (length(input$simCont) == 0) return()
      toed = input$simCont[1]
      # find component
      cmp = findCmp(globals$fvsRun,toed)
      if (is.null(cmp)) return()
      globals$currentEditCmp = cmp
cat ("Edit, cmp$kwdName=",cmp$kwdName,"\n")
      if (exists(cmp$kwdName)) #if a function exists, use it.
      {
        eltList <- eval(parse(text=paste0(cmp$kwdName,
          "(globals$currentEditCmp$title,prms,globals$fvsRun,globals)")))
        if (is.null(eltList)) return(NULL)
        eltList <- eltList[[1]]
      } else {
        pk <- match (cmp$kwdName,names(prms))
        if (is.na(pk)) # FreeForm Edit, used if pk does not match a parms.
        {
          varsDef  = " "
          varsName = " "
          for (elt in prms[["evmon.variables"]])
          {
            atl = attr(elt,"atlist")
            # the appliesTo list will have two tokens if the extension is part of the list
            if (length(atl) > 1 && length(intersect(atl,globals$activeExtens)) == 0) next
            varsDef <- c(varsDef,paste0(elt,": ",attr(elt,"pstring")))
            attributes(elt) <- NULL
            varsName <- c(varsName,elt)
          }
          indx = sort(varsName,index.return=TRUE)$ix
          varsName = as.list(varsName[indx])
          names(varsName) = varsDef[indx]
          funcDef  = " "
          funcName = " "
          for (elt in prms[["evmon.functions"]])
          {
            atl = attr(elt,"atlist")
            # the appliesTo list will have two tokens if the extension is part of the list
            if (length(atl) > 1 && length(intersect(atl,globals$activeExtens)) == 0) next
            funcDef <- c(funcDef,paste0(elt,": ",attr(elt,"pstring")))
            attributes(elt) <- NULL
            funcName <- c(funcName,elt)
          }
          indx = sort(funcName,index.return=TRUE)$ix
          funcName = as.list(funcName[indx])
          names(funcName) = funcDef[indx]    
          eltList <- list(
            tags$style(type="label/css", "#cmdTitle{display: inline;}"),
            myInlineTextInput("cmdTitle","Component title", 
              value=globals$currentEditCmp$title,size=40),          
            tags$style(type="text/css", 
              "#freeEditCols{font-family:monospace;font-size:90%;width:95%;}"), 
            tags$p(id="freeEditCols", 
                   HTML(paste0("&nbsp;",paste0("....+....",1:8,collapse="")))),
            tags$style(type="text/css", 
              "#freeEdit{font-family:monospace;font-size:90%;width:95%;}"), 
            tags$textarea(id="freeEdit", rows=10, 
                          globals$currentEditCmp$kwds),
            myInlineListButton ("freeOps","Math:",list(
               " "=" ",
               "+ Simple addition"="+",
               "- Subtraction or change sign"="-",
               "* Multiplication"="*",
               "/ Division"="/",
               "** Exponentiate, X**Y is X raised to the power Y"="**",
               "EQ Logical Equal"="EQ",
               "NE Logical Not Equal"="NE",
               "LT Logical Less than"="LT",
               "LE Logical Less than or equal"="LE",
               "GT Logical Greater than"="GT",
               "GE Logical Greater than or equal"="GE",
               "AND Logical AND"="AND",
               "OR Logical OR"="OR",
               "NOT Logical NOT"="NOT",
               "ABS() Absolute value, ABS(-3) is 3."="ABS()",
               "ALog() Natural logarithm (base e)"="ALog()",
               "ALog10() Common logarithm (base 10)"="ALog10()",
               "ArcCos() Arc cosine (argument in radians)"="ArcCos()",
               "ArcSin() Arc sine (argument in radians)"="ArcSin()",
               "ArcTan() Arc tangent (argument in radians)"="ArcTan()",
               "Cos() Cosine (argument in radians)"="Cos()",
               "Exp() e raised to power"="Exp()",
               "Frac() Fractional part of a number, Frac(3.4) is .4"="Frac()",
               "Int() Integer part of a number, Int(3.4) is 3"="Int()",
               "Max() Maximum value of the arguments, Max(5,3,-1,10,2) is 10"="Max()",
               "Min() Minimum value of the arguments, Min(5,3,-1,10,2) is -1"="Min()",
               "Mod() Remainder of first argument divided by the second"="Mod()",
               "Sin() Sine (argument in radians)"="Sin()",
               "Sqrt() Square root"="Sqrt()",
               "Tan() Tangent (argument in radians)"="Tan()"), 0),
             myInlineListButton ("freeVars","Variables:",varsName),
             mkSelSpecies("freeSpecies",prms,"Species codes:",fpvs=-1,
                  choices=NULL,globals$activeVariants[1]),
             myInlineListButton ("freeFuncs","FVS Functions:",funcName),
             uiOutput("fvsFuncRender")
          )
        } else {        # Launch general purpose builder when pk matches a parms.        
          pkeys <- prms[[pk]]
          eltList <- mkeltList(pkeys,prms,globals,globals$fvsRun,cmp$atag=="c")     
          eltList <- append(eltList,list(
            myInlineTextInput("cmdTitle","Component title: ", 
                      value=globals$currentEditCmp$title,size=40)),after=0)          
        }
      }      
      eltList <- append(eltList,list(
        h4(paste0('Edit: "',globals$currentEditCmp$title),'"')),after=0)
      output$cmdBuild <- renderUI(eltList)
      output$fvsFuncRender <- renderUI (NULL)
      if (input$rightPan != "Components")
      {
        updateTabsetPanel(session=session, inputId="rightPan", 
          selected="Components")
      }
    })
  })

  # install callback functionality for the freeEdit text area to get start and end
  # selection poistions.
  #TODO: suppress the call once it's done the first time
  observe({
    if (length(input$freeEdit)) 
    {
#cat("call sendcustomMessage\n")          
      session$sendCustomMessage(type="getStart", "freeEdit")
    }
  })
  observe({
    if (length(input$freeSpecies) && nchar(input$freeSpecies)) isolate({
      if (length(input$freeEdit) == 0) return()
      insertStrinIntoFreeEdit(input$freeSpecies)
    })
  })
  observe({
    if (length(input$freeVars) && nchar(input$freeVars)) isolate({
      if (length(input$freeEdit) == 0) return()
      insertStrinIntoFreeEdit(input$freeVars)
    })
  })
  observe({
    if (length(input$freeOps) && nchar(input$freeOps)) isolate({
      if (length(input$freeEdit) == 0) return()
      insertStrinIntoFreeEdit(input$freeOps)
    })
  })  
  observe({
    if (length(input$freeFuncs) && nchar(input$freeFuncs)) isolate({
      if (length(input$freeEdit) == 0) return()
      pkeys = prms[[paste0("evmon.function.",input$freeFuncs)]] 
      if (is.null(pkeys)) return()
      eltList <- mkeltList(pkeys,prms,globals,globals$fvsRun)
      eltList <- append(eltList,list(
        actionButton("fvsFuncInsert","Insert function"),
        actionButton("fvsFuncCancel","Cancel function"),h6()))
      output$fvsFuncRender <- renderUI(eltList)
    })
  })  
  observe({  #fvsFuncCancel
    if (length(input$fvsFuncCancel) && input$fvsFuncCancel) 
    {
      output$fvsFuncRender <- renderUI (NULL)
      updateSelectInput(session=session, inputId="freeFuncs",selected=1)
    }
  })
  observe({  #fvsFuncInsert
    if (length(input$fvsFuncInsert) && input$fvsFuncInsert) 
    isolate({
      pkeys = prms[[paste0("evmon.function.",input$freeFuncs)]]
      ansFrm = getPstring(pkeys,"answerForm",globals$activeVariants[1])
      reopn = NULL
      fn = 0
      repeat
      {
        fn = fn+1
        pkey = paste0("f",fn)
        fps = getPstring(pkeys,pkey,globals$activeVariants[1])
        if (is.null(fps)) break
        instr = input[[pkey]]
        reopn = c(reopn,as.character(if (is.null(instr)) " " else instr))
        names(reopn)[fn] = pkey
      }
      string = mkKeyWrd(ansFrm,reopn,pkeys,globals$activeVariants[1])      
      insertStrinIntoFreeEdit(string)
    })
  })   
  insertStrinIntoFreeEdit <- function(string)
  {
    if (is.null(string) || nchar(string) == 0 || string == " ") return()
    isolate({
      if (length(input$selectionStart)) 
      {
        start = input$selectionStart
        end   = input$selectionEnd
      } else { start=0;end=0 } 
      len   = nchar(input$freeEdit)
cat ("insertStrinIntoFreeEdit string=",string," start=",start," end=",end," len=",len,"\n")
      if (nchar(string) == 0) return()
      if (start == end && end == len) {         # prepend 
        updateTextInput(session, "freeEdit", value = paste0(input$freeEdit,string))
      } else if (start == 0 && end == start) {  # append
        updateTextInput(session, "freeEdit", value = paste0(string,input$freeEdit))
      } else if (end >= start) {                # insert/replace
        str = input$freeEdit
        updateTextInput(session, "freeEdit", value = 
          paste0(substring(input$freeEdit,1,max(1,start)),string,
                 substring(input$freeEdit,min(end+1,len))))
      }
      updateSelectInput(session=session, inputId="freeOps", selected=1)
      updateSelectInput(session=session, inputId="freeVars",selected=1)
      updateSelectInput(session=session, inputId="freeSpecies",selected=1)
      updateSelectInput(session=session, inputId="freeFuncs",selected=1)
      output$fvsFuncRender <- renderUI (NULL)
      session$sendCustomMessage(type="refocus", "freeEdit")
    })
  }

 
  ## Cut  
  observe({
    if (input$cutCmp == 0) return()
    isolate ({
cat ("Cut length(input$simCont) = ",length(input$simCont),"\n")     
      if (length(input$simCont) == 0) return
      if (moveToPaste(input$simCont[1],globals,globals$fvsRun))
      {   
        globals$foundStand=0L 
        updateRepsTags(globals) 
        mkSimCnts(globals$fvsRun) 
        updateSelectInput(session=session, inputId="simCont", 
          choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
        output$contCnts <- renderUI(HTML(paste0("<b>Contents</b><br>",
          length(globals$fvsRun$stands)," stand(s)<br>",
          length(globals$fvsRun$grps)," group(s)")))
        updateSelectInput(session=session, inputId="selpaste", 
          choices=globals$pastelistShadow,
          selected=if (length(globals$pastelistShadow)) 
              globals$pastelistShadow[[1]] else 0)
      }
    })
  })

  
  ## Copy  
  observe({
    if (input$copyCmp == 0) return()
    isolate ({
      toCpy = findCmp(globals$fvsRun,input$simCont[1])
      if (is.null(toCpy)) return()
      toCpy = mkfvsCmp(kwds=toCpy$kwds,kwdName=toCpy$kwdName,
              exten=toCpy$exten,variant=toCpy$variant,uuid=uuidgen(),
              atag=toCpy$atag,title=toCpy$title,reopn=toCpy$reopn)
      globals$pastelist <- append(globals$pastelist,toCpy,after=0)     
      globals$pastelistShadow <- append(globals$pastelistShadow,toCpy$uuid,after=0)
      names(globals$pastelistShadow)[1] = toCpy$title
      updateSelectInput(session=session, inputId="selpaste", 
          choices=globals$pastelistShadow,
          selected=if (length(globals$pastelistShadow)) 
            globals$pastelistShadow[[1]] else 0)
    })
  })


  ## Paste
  observe({
    if (input$paste == 0) return()
    isolate ({
      if (length(input$simCont) == 0) return() 
      if (length(input$selpaste) == 0) return()
      if (nchar(input$selpaste) == 0) return()
      pidx = findIdx (globals$pastelist, input$selpaste)
      if (is.null(pidx)) return()
      topaste = globals$pastelist[[pidx]]
      if (class(topaste) != "fvsCmp") return()
      topaste = mkfvsCmp(kwds=topaste$kwds,kwdName=topaste$kwdName,
              exten=topaste$exten,variant=topaste$variant,uuid=uuidgen(),
              atag=topaste$atag,title=topaste$title,reopn=topaste$reopn)
      idx = pasteComponent(globals$fvsRun,input$simCont[1],topaste)
      if (!is.null(idx))
      { 
        mkSimCnts(globals$fvsRun)   
        updateSelectInput(session=session, inputId="simCont", 
           choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
        output$contCnts <- renderUI(HTML(paste0("<b>Contents</b><br>",
          length(globals$fvsRun$stands)," stand(s)<br>",
          length(globals$fvsRun$grps)," group(s)")))
      }
      globals$foundStand=0L 
    })
  })


  # Convert to freeform
  observe({
    if (input$mkfree == 0) return()
    isolate ({
      globals$currentEditCmp <- globals$NULLfvsCmp
      updateSelectInput(session=session, inputId="addComponents", selected = 0)
      if (length(input$simCont) == 0) return
      toed = input$simCont[1]
      # find component
      cmp = findCmp(globals$fvsRun,toed)
      if (is.null(cmp)) return()
      if (substring(cmp$kwdName,1,10) == "Freeform: ") return()
      if (substring(cmp$kwdName,1,6) != "From: ")
      {
        kwPname = cmp$kwdName
        pkeys = prms[[kwPname]]
        if (!is.null(pkeys))
        {
          ansFrm = getPstring(pkeys,"parmsForm",globals$activeVariants[1])
          if (is.null(ansFrm)) ansFrm = getPstring(pkeys,"answerForm",globals$activeVariants[1])
          if (is.null(ansFrm)) 
          {
            kw = unlist(strsplit(kwPname,".",fixed=TRUE))
            kw = kw[length(kw)]
            ansFrm = paste0(substr(paste0(kw,"         "),1,10),
                     "!1,10!!2,10!!3,10!!4,10!!5,10!!6,10!!7,10!")
          }
          if (is.null(ansFrm)) return()
          if (cmp$atag != "c") cmp$kwds = mkKeyWrd(ansFrm,cmp$reopn,pkeys,globals$activeVariants[1])
        }
        cmp$kwdName = paste0("Freeform: ",cmp$kwdName)
        cmp$title = paste0("Freeform: ",cmp$title)
        cmp$reopn = character(0)
        mkSimCnts(globals$fvsRun,toed)   
        updateSelectInput(session=session, inputId="simCont", 
             choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
        output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      }
    })
  })


  ## Command Set (radio button).
  observe({
cat ("command set (radio), input$cmdSet=",input$cmdSet,
     " input$simCont=",length(input$simCont),"\n")
    output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
    if (length(input$simCont) == 0) return(NULL)
    switch (input$cmdSet,
      "Management" = 
      {
        if (length(globals$mgmtsel) == 0) globals$mgmtsel <- 
          mkcatsel(prms,"mgmtCategories",globals)
        updateSelectInput(session=session, inputId="addCategories", 
          label="Categories", choices=mkpair(globals$mgmtsel), selected = 0)
        updateSelectInput(session=session, inputId="addComponents", 
          label="Components", choices=list(" "), selected = 0)
      },
      "Modifiers"  = 
      {
        if (length(globals$mmodsel) == 0) globals$mmodsel <- 
          mkcatsel(prms,"selectModelModifiers",globals)
        updateSelectInput(session=session, inputId="addCategories", 
          label="Categories", choices=mkpair(globals$mmodsel), selected = 0)
        updateSelectInput(session=session, inputId="addComponents", 
          label="Components", choices=list(" "), selected = 0)
      },
      "Outputs"    = 
      {
        if (length(globals$moutsel) == 0) globals$moutsel <- 
          mkcatsel(prms,"selectModelOutputs",globals)
        updateSelectInput(session=session, inputId="addCategories", 
          label="Categories", choices=mkpair(globals$moutsel), selected = 0)
        updateSelectInput(session=session, inputId="addComponents", 
          label="Components", choices=list(" "), selected = 0)
      },
      "Keywords"   = 
      {
        if (length(globals$extnsel) == 0) mkextkwd(prms,globals)
        updateSelectInput(session=session, inputId="addCategories", 
           label="Extensions", choices=globals$extnsel, selected = 0)
        updateSelectInput(session=session, inputId="addComponents", 
           label="Keywords", choices=list(" "), selected = 0)
      },
      "Your components"   = 
      {
        if (length(globals$customCmps) == 0)
        {
          if (file.exists("FVS_kcps.RData"))
          {
            load("FVS_kcps.RData")
            globals$customCmps = customCmps
          }
        }
        if (length(globals$customCmps) == 0) 
        {
           updateSelectInput(session=session, inputId="addCategories", 
           label="None exist", choices=list(" "), selected = 0)
           return()
        }
        sels = as.list(as.character(1:length(globals$customCmps)))
        names(sels) = names(globals$customCmps)
        updateSelectInput(session=session, inputId="addCategories", 
           label="Your components", choices=sels, selected = 0)
        updateSelectInput(session=session, inputId="addComponents", 
           label="", choices=list(" "), selected = 0)
      },
      NULL)   
  })

  
  # Category selection, load "Components" (or if "Custom components", insert item).
  observe({ 
    if (length(input$addCategories) == 0 || 
        nchar(input$addCategories) == 0 ||
        length(input$simCont) == 0) 
    { 
cat ("Category selection direct return\n")
      updateSelectInput(session=session, inputId="addComponents", selected = 0)
      output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      return(NULL)
    }
    isolate ({ 
cat ("Category selection input$cmdSet=",input$cmdSet," input$addCategories=",input$addCategories,"\n")
    switch (input$cmdSet,
      "Management" =  updateSelectInput(session=session, inputId="addComponents", 
           label="Components", selected = 0, 
           choices=globals$mgmtsel[[as.numeric(input$addCategories)]]),
      "Modifiers"  = updateSelectInput(session=session, inputId="addComponents", 
           label="Components", selected = 0, 
           choices=globals$mmodsel[[as.numeric(input$addCategories)]]),
      "Outputs"    = updateSelectInput(session=session, inputId="addComponents", 
           label="Components", selected = 0, 
           choices=globals$moutsel[[as.numeric(input$addCategories)]]),
      "Keywords"   = updateSelectInput(session=session, inputId="addComponents", 
           label="Keywords", selected = 0, 
           choices=globals$kwdsel[[input$addCategories]]),
      "Your components" = 
        {
          output$cmdBuild <- renderUI(
            if (is.null(input$addCategories) || 
                        input$addCategories == "") NULL else
            {
              sel = as.numeric(input$addCategories)
              titnam = paste0("Custom: ",names(globals$customCmps)[sel])
              globals$currentEditCmp = mkfvsCmp(
                kwds = globals$customCmps[[sel]], 
                kwdName = titnam, title = titnam, 
                exten="base",variant=globals$activeVariants[1],
                uuid=uuidgen(), atag="k", reopn="pasteOnSave")
              list(
                h4(globals$currentEditCmp$title),
                tags$style(type="text/css", "#cmdTitle{display: inline;}"),
                textInput("cmdTitle","Component title", value=globals$currentEditCmp$title),
                tags$style(type="text/css", 
                  "#freeEditCols{font-family:monospace;font-size:90%;width:95%;}"), 
                tags$p(id="freeEditCols", 
                   HTML(paste0("&nbsp;",paste0("....+....",1:8,collapse="")))),
                tags$style(type="text/css", 
                  "#freeEdit{font-family:monospace;font-size:90%;width:95%;}"), 
                tags$textarea(id="freeEdit",rows=15,globals$currentEditCmp$kwds)
              )
            })
       },
       NULL)  
    }) 
  })
               
  observe({ 
    # render the command specification (on command selection).
    if (length(input$addComponents) == 0 || 
        nchar(input$addComponents) == 0 || 
        length(input$simCont) == 0 )
    { 
      output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      return(NULL)
    }
    globals$currentEditCmp <- globals$NULLfvsCmp
    globals$currentCndPkey <- "0"   
    globals$currentCmdPkey <- input$addComponents
    isolate ({
cat ("command selection, input$addComponents=",input$addComponents,
     " input$cmdSet=",input$cmdSet,
     " input$addCategories=",input$addCategories,
     "\nglobals$currentCndPkey=",globals$currentCndPkey,
     " globals$currentCmdPkey=",globals$currentCmdPkey,"\n")
       if (is.null(input$addCategories)) return()
       title = switch (input$cmdSet,
        "Management" = globals$mgmtsel[[as.numeric(input$addCategories)]],
        "Modifiers"  = globals$mmodsel[[as.numeric(input$addCategories)]],
        "Outputs"    = globals$moutsel[[as.numeric(input$addCategories)]],
        "Keywords"   = globals$kwdsel[[input$addCategories]],
        NULL)
      titIndx = match(input$addComponents,title)
      if (!is.null(title)) 
      {
        titIndx = match(input$addComponents,title)
        if (!is.na(titIndx)) title = names(title)[titIndx]
      }
cat ("title=",title," titIndx=",titIndx,"\n")      
      if (is.na(indx <- suppressWarnings(as.integer(globals$currentCmdPkey))))
      { 
        globals$winBuildFunction <- globals$currentCmdPkey
        globals$currentCmdPkey <- character(0)
cat ("function name=",globals$winBuildFunction,"\n")
        ans = eval(parse(text=paste0(globals$winBuildFunction,
          "(title,prms,globals$fvsRun,globals)")))
        if (is.null(ans)) return(NULL)
        output$cmdBuild     <- renderUI (if (length(ans[[1]])) ans[[1]] else NULL)
        output$cmdBuildDesc <- renderUI (if (length(ans[[2]])) ans[[2]] else NULL)
      } else {
        globals$winBuildFunction <- character(0)
        pkeys <- prms[[indx]]
        eltList <- mkeltList(pkeys,prms,globals,globals$fvsRun)
        if (!is.null(title)) eltList <- 
          append(eltList,list(
            tags$style(type="text/css", "#cmdTitle{display: inline;}"),
            myInlineTextInput("cmdTitle","Component title ", value=title,size=40)),
            after=0)
        output$cmdBuild <- renderUI (if (length(eltList)) eltList else NULL)
        des <- getPstring(pkeys,"description",globals$activeVariants[1])
        output$cmdBuildDesc <- renderUI (if (!is.null(des) && nchar(des) > 0)
          HTML(paste0("<br>Description:<br>",gsub("\n","<br>",des))) else NULL)
      }
    })          
  })

  # schedule box toggled.
  observe({  
    if (length(input$schedbox) == 0) return()
cat("input$schedbox=",input$schedbox,"\n")
    if (input$schedbox == 1) 
    {
      updateTextInput(session, globals$schedBoxPkey, 
        label = "Year or cycle number: ", 
        value = globals$schedBoxYrLastUsed) 
      output$conditions <- renderUI(NULL)
    } else if (input$schedbox == 2) 
    {
      updateTextInput(session, globals$schedBoxPkey, 
        label = "Number of years after condition is found true: ", value = "0") 
      cndlist = unlist(prms$conditions_list)
      names(cndlist) = unlist(lapply(prms$conditions_list,attr,"pstring"))
      cndlist = as.list(cndlist)
cat("globals$currentCmdPkey=",globals$currentCmdPkey,"\n")      
      n = suppressWarnings(as.numeric(globals$currentCmdPkey))    
      default =  getPstring(prms[[if (is.na(n)) globals$currentCmdPkey else n]],
        "defaultCondition",globals$activeVariants[1])
      if (is.null(default)) default="cycle1"
      output$conditions <- renderUI(list(
        selectInput("condList", "Create a condition", cndlist, 
          selected = default, multiple = FALSE, selectize = FALSE),
        uiOutput("condElts")
      ))
    } else 
    {
      globals$currentCndPkey <- "0"
      updateTextInput(session, globals$schedBoxPkey, 
        label = "Number of years after condition is found true ", value = "0") 
      output$conditions <- renderUI(
        selectInput("condList","Existing conditions", globals$existingCmps, 
          selected = NULL, multiple = FALSE, selectize = FALSE))
    }
  })

  observe({  
    # schedule by condition condition selection
    if (length(input$schedbox) == 0) return()
    if (length(input$condList) == 0) return()
cat("make condElts, input$condList=",input$condList,"\n")    
    output$condElts <- renderUI(if (input$condList == "none") NULL else
      {
        cnpkey <- paste0("condition.",input$condList)
        idx <- match(cnpkey,names(prms))
        globals$currentCndPkey <- if (is.na(idx)) "0" else as.character(idx)
        ui = if (globals$currentCndPkey == "0") NULL else
        {
          eltList <- mkeltList(prms[[as.numeric(globals$currentCndPkey)]],prms,
                              globals,globals$fvsRun,cndflag=TRUE)
          if (length(eltList) == 0) NULL else eltList
        }
        if (!is.null(ui))
        {
          title = getPstring(prms$conditions_list,input$condList)
          if (!is.null(title)) ui <- 
            append(ui,list(myInlineTextInput("cndTitle","Condition title", 
              value=title, size=40)),after=1)
        }
        ui
      })
  })

  observe({  
    # command Cancel
    if (input$cmdCancel == 0) return()
    globals$currentEditCmp <- globals$NULLfvsCmp
    globals$schedBoxPkey <- character(0)
    output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
    updateSelectInput(session=session, inputId="addCategories", selected=0)
    updateSelectInput(session=session, inputId="addComponents", selected=0)
  })

  observe({  
    # command Save in run 
    if (input$cmdSaveInRun == 0) return()
    isolate ({      
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp) &&
         globals$currentCndPkey == "0" && globals$currentCmdPkey == "0") return()
      if (length(globals$currentEditCmp$reopn) && 
                 globals$currentEditCmp$reopn == "pasteOnSave") 
      {
        globals$currentEditCmp$reopn = character(0)
        globals$currentEditCmp$kwds = input$freeEdit
        if (!is.null(input$cmdTitle) && nchar(input$cmdTitle)) 
          globals$currentEditCmp$title = input$cmdTitle
        idx = pasteComponent(globals$fvsRun,input$simCont[1],globals$currentEditCmp)
        if (!is.null(idx))
        { 
          mkSimCnts(globals$fvsRun)   
          updateSelectInput(session=session, inputId="simCont", 
             choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
        }
        globals$currentEditCmp <- globals$NULLfvsCmp
        output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
        updateSelectInput(session=session, inputId="addCategories", selected=0)
        updateSelectInput(session=session, inputId="addComponents", selected=0)
        return()
      }   
      if (globals$currentCndPkey != "0")
      {
        kwPname = names(prms)[as.numeric(globals$currentCndPkey)]
        pkeys = prms[[as.numeric(globals$currentCndPkey)]]
        ansFrm = getPstring(pkeys,"answerForm",globals$activeVariants[1])
        if (is.null(ansFrm)) ansFrm = 
          getPstring(pkeys,"parmsForm",globals$activeVariants[1])
        reopn = NULL
        fn = 0
        repeat
        {
          fn = fn+1
          pkey = paste0("f",fn)
          fps = getPstring(pkeys,pkey,globals$activeVariants[1])
          if (is.null(fps)) break
          instr = input[[paste0("cnd.",pkey)]]
          reopn = c(reopn,as.character(if (is.null(instr)) " " else instr))
          names(reopn)[fn] = pkey
        } 
        instr = input[["waitYears"]]
        reopn = c(reopn,as.character(if (is.null(instr)) " " else instr))
        names(reopn)[length(names(reopn))] = "waitYears"
        kwds = sprintf("%-10s%10s\n","If",if (is.null(instr)) " " else instr)
        kwds = paste0(kwds,mkKeyWrd(ansFrm,reopn,pkeys,globals$activeVariants[1]),
               "\nThen")
cat ("Save with if/then, kwds=",kwds," reopn=",reopn,"\n") 
         newcnd = mkfvsCmp(uuid=uuidgen(),atag="c",exten="base",
                   kwdName=kwPname,title=input$cndTitle,kwds=kwds,reopn=reopn)
      } else newcnd = NULL
      # make or edit a keyword. This section is used for both 
      # building a keyword and editing a keyword or a condition. 
      
      # if this is true, then we are building a new component
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp))
      {
        if (length(globals$winBuildFunction))
        {
          kwPname = globals$winBuildFunction
          pkeys = character(0)
        } else {
          kwPname = names(prms)[as.numeric(globals$currentCmdPkey)]
          pkeys = prms[[as.numeric(globals$currentCmdPkey)]]
        }
        oReopn  = character(0) 
cat ("Building a component: kwPname=",kwPname,"\n")
      } else { # we are editing the component
        kwPname = globals$currentEditCmp$kwdName
        oReopn  = globals$currentEditCmp$reopn
cat ("Editing a component: kwPname=",kwPname," oReopn=",oReopn,"\n")
        pkeys = prms[[kwPname]]
        if (is.null(pkeys) && length(oReopn) == 0) #this is freeform...
        {
cat ("Editing as freeform\n")
          globals$currentEditCmp$kwds = input$freeEdit
          globals$currentEditCmp$reopn = character(0)
          globals$currentEditCmp$title = input$cmdTitle
          mkSimCnts(globals$fvsRun,input$simCont[[1]])
          updateSelectInput(session=session, inputId="simCont", 
             choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
          updateSelectInput(session=session,inputId="addComponents",selected = 0)
          output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
          return()
        }  
      }
      # building/editing a keyword from a custom window.    
      if (length(pkeys) == 0 && nchar(kwPname)) 
      {
        # try to find a function that can make the keywords
        fn = paste0(kwPname,".mkKeyWrd")
        ans = if (exists(fn)) eval(parse(text=paste0(fn,"(input)"))) else NULL
        if (is.null(ans)) return()
        ex = ans$ex
        kwds = ans$kwds
        reopn = ans$reopn
      } else {
        ansFrm = getPstring(pkeys,"answerForm",globals$activeVariants[1])
        if (is.null(ansFrm)) 
        { 
cat ("kwPname=",kwPname,"\n")
          kw = unlist(strsplit(kwPname,".",fixed=TRUE))
          kw = kw[length(kw)]
          ansFrm = paste0(substr(paste0(kw,"         "),1,10),
                   "!1,10!!2,10!!3,10!!4,10!!5,10!!6,10!!7,10!")
        }
        reopn = NULL
        fn = 0
        repeat
        {
          fn = fn+1
          pkey = paste0("f",fn)
          fps = getPstring(pkeys,pkey,globals$activeVariants[1])
          if (is.null(fps)) break
          instr = input[[pkey]]
          reopn = c(reopn,as.character(if (is.null(instr)) " " else instr))
          names(reopn)[fn] = pkey
        }
        if ("waitYears" %in% names(oReopn))
        {
          instr = input[["waitYears"]]
          if (!is.null(instr))
          {
            reopn = c(reopn,as.character(if (is.null(instr)) " " else instr))
            names(reopn)[length(names(reopn))] = "waitYears"
            kwds = sprintf("%-10s%10s\n","If",if (is.null(instr)) " " else instr)
            kwds = paste0(kwds,mkKeyWrd(ansFrm,reopn,pkeys,globals$activeVariants[1]),
                   "\nThen")
          }
        } else kwds = mkKeyWrd(ansFrm,reopn,pkeys,globals$activeVariants[1]) 
      }
cat ("Save, kwds=",kwds,"\n")      
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp))
      {
cat ("class(input$addCategories)=",class(input$addCategories)," and=",input$addCategories,"\n")
        ex = if (input$cmdSet != "Keywords") "base" else
               unlist(strsplit(input$addCategories,":",fixed=TRUE))[1] 
        newcmp = mkfvsCmp(uuid=uuidgen(),atag="k",kwds=kwds,exten=ex,
             variant=globals$activeVariants[1],kwdName=kwPname,
             title=input$cmdTitle,reopn=reopn)
        # find the attachment point. 
        sel = if (length(globals$schedBoxPkey) &&
              input$schedbox == 3) input$condList else input$simCont[[1]]
        grp = findIdx(globals$fvsRun$grps,sel)
        std = if (is.null(grp)) findIdx(globals$fvsRun$stands,sel) else NULL
        cmp = NULL
        if (is.null(grp) && is.null(std)) 
        {
          for (grp in 1:length(globals$fvsRun$grps))
          {
            cmp = findIdx(globals$fvsRun$grps[[grp]]$cmps,sel)
            if (!is.null(cmp)) break
          }
          if (is.null(cmp)) grp = NULL
          if (is.null(grp)) for (std in 1:length(globals$fvsRun$stands))
          {
            cmp = findIdx(globals$fvsRun$stands[[std]]$cmps,sel)
            if (!is.null(cmp)) break
          }
        }
        if (length(globals$schedBoxPkey) && input$schedbox == 3) 
        {
          #tag the component as being linked to the condition.
          newcmp$atag = sel
          #adjust insert point.
          if (is.null(std)) for (i in (cmp+1):length(globals$fvsRun$grps[[grp]]$cmps))
          {
            if (i > length(globals$fvsRun$grps[[grp]]$cmps)) break
            if (globals$fvsRun$grps[[grp]]$cmps[[i]]$atag == sel) cmp = i
          } else for (i in (cmp+1):length(globals$fvsRun$stands[[std]]$cmps))
          {
            if (i > length(globals$fvsRun$stands[[std]]$cmps)) break
            if (globals$fvsRun$stands[[std]]$cmps[[i]]$atag == sel) cmp = i
          }
        }
        # save schedBoxYrLastUsed
        if (length(globals$schedBoxPkey) && input$schedbox == 1 &&
            length(input[[globals$schedBoxPkey]])) globals$schedBoxYrLastUsed <- 
              input[[globals$schedBoxPkey]]
        # if there is a newcnd, then attach it first.
        if (!is.null(newcnd))
        {
          newcmp$atag = newcnd$uuid
          if (is.null(grp)) 
          { 
            globals$fvsRun$stands[[std]]$cmps <- if (is.null(cmp))  
                append(globals$fvsRun$stands[[std]]$cmps, newcnd) else
                append(globals$fvsRun$stands[[std]]$cmps, newcnd, after=cmp)
          } else
          { 
            globals$fvsRun$grps[[grp]]$cmps <- if (is.null(cmp))  
                append(globals$fvsRun$grps[[grp]]$cmps, newcnd) else
                append(globals$fvsRun$grps[[grp]]$cmps, newcnd, after=cmp)
          }
          if (!is.null(cmp)) cmp <- cmp+1
        } 
        # attach the new component
        if (is.null(grp)) 
        {
          globals$fvsRun$stands[[std]]$cmps <- if (is.null(cmp))  
              append(globals$fvsRun$stands[[std]]$cmps, newcmp) else
              append(globals$fvsRun$stands[[std]]$cmps, newcmp, after=cmp)
        } else { 
          globals$fvsRun$grps[[grp]]$cmps <- if (is.null(cmp))  
              append(globals$fvsRun$grps[[grp]]$cmps, newcmp) else
              append(globals$fvsRun$grps[[grp]]$cmps, newcmp, after=cmp)
        }
      } else {
        globals$currentEditCmp$kwds=kwds
        globals$currentEditCmp$title=input$cmdTitle
cat ("saving, kwds=",kwds," title=",input$cmdTitle," reopn=",reopn,"\n")       
        globals$currentEditCmp$reopn=if (is.null(reopn)) character(0) else reopn
        globals$currentEditCmp=globals$NULLfvsCmp
      }
      mkSimCnts(globals$fvsRun,input$simCont[[1]])
      updateSelectInput(session=session, inputId="simCont", 
         choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
      updateSelectInput(session=session, inputId="addComponents", selected = 0)
      output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      globals$schedBoxPkey <- character(0)
    })
  })

  ## time
  observe(globals$fvsRun$startyr  <- input$startyr)
  observe(globals$fvsRun$endyr    <- input$endyr)
  observe(globals$fvsRun$cyclelen <- input$cyclelen)
  observe(globals$fvsRun$cycleat  <- input$cycleat)

  ## Save and Run
  observe({  
    if (input$saveandrun == 0) return()
      
    isolate ({
      if (length(globals$fvsRun$stands) > 0) 
      {
        progress <- shiny::Progress$new(session,min=1,
                           max=length(globals$fvsRun$stands)+10)
        progress$set(message = "Run preparation: ", 
          detail = "Saving FVS Runs", value = 1)         
        saveRun()
cat("Nulling uiRunPlot at Save and Run\n")
        output$uiRunPlot <- output$uiErrorScan <- renderUI(NULL)
        globals$currentQuickPlot = character(0)
        selChoices = names(globals$FVS_Runs)
        names(selChoices) = globals$FVS_Runs
        updateSelectInput(session=session, inputId="runSel", 
            choices=selChoices,selected=selChoices[[1]])
        FVS_Runs = globals$FVS_Runs
        save (FVS_Runs,file="FVS_Runs.RData")
        killIfRunning(globals$fvsRun$uuid)
        progress$set(message = "Run preparation: ", 
          detail = "Deleting old ouputs", value = 2)         
        removeFVSRunFiles(globals$fvsRun$uuid)
        updateSelectInput(session=session, inputId="bkgRuns", 
                          choices=getBkgRunList(),selected=0)
        progress$set(message = "Run preparation: ", 
          detail = "Write .key file and prepare program", value = 3)
        writeKeyFile(globals$fvsRun,dbGlb$dbIcon,prms)
        dir.create(globals$fvsRun$uuid)
        if (!exists("rFVSDir")) rFVSDir = "rFVS/R"
        if (!file.exists(rFVSDir)) rFVSDir = "rFVS/R"
        if (!file.exists(rFVSDir)) return()
        binDir = if (file.exists("FVSbin")) "FVSbin" else fvsBinDir
cat ("runwaitback=",input$runwaitback,"\n")
        if (input$runwaitback!="Wait for run")
        {
          runScript = paste0(globals$fvsRun$uuid,".rscript")
          rs = file(runScript,open="wt")
          cat ('options(echo=TRUE)\nlibrary(methods)\nlibrary(RSQLite)\n',file=rs)
          cat ('pid = Sys.getpid()\n',file=rs)
          cmd = paste0('unlink("',globals$fvsRun$uuid,'.db")')
          cat (cmd,"\n",file=rs)
          cmd = paste0("title = '",globals$fvsRun$title,"'")
          cat (cmd,"\n",file=rs)                   
          cmd = paste0("nstands = ",length(globals$fvsRun$stands))
          cat (cmd,"\n",file=rs)          
          cmd = paste0("for (rf in dir('",rFVSDir,
             "')) source(paste0('",rFVSDir,"','/',rf))")
          cat (cmd,"\n",file=rs)
          cmd = paste0("fvsLoad('",
             globals$fvsRun$FVSpgm,"',bin='",binDir,"')")
          cat (cmd,"\n",file=rs)
          if (globals$fvsRun$runScript != "fvsRun")
          {
            cmd = paste0("source('customRun_",globals$fvsRun$runScript,".R')")
            cat (cmd,"\n",file=rs)
            cat ("runOps = ",deparse(globals$fvsRun$uiCustomRunOps),"\n",file=rs)        
          }
          cmd = paste0('fvsSetCmdLine("--keywordfile=',globals$fvsRun$uuid,'.key")')
          cat (cmd,"\n",file=rs)
          runCmd = if (globals$fvsRun$runScript == "fvsRun") "fvsRun()" else
               paste0(globals$fvsRun$runScript,"(runOps)")
          pidfile = paste0(globals$fvsRun$uuid,".pidStatus")
          cmd = 'cat (pid,"Starting title=",title,"\n")'
          cat (cmd,"\n",file=rs)       
          cmd = paste0('cat (pid,"Starting title=",title,file="',pidfile,'")')
          cat (cmd,"\n",file=rs)       
          cmd = paste0('for (istand in 1:nstands)\n{\n',
                       '  cat (pid,"Running",istand,"of",nstands," title=",title,"\n")\n',
                       '  cat (pid,"Running",istand,"of",nstands," title=",title,"\n",file="',pidfile,
                       '")\n',
                       '  rtn = ',runCmd,'\nfvsRun()\n}')
          cat (cmd,"\n",file=rs)
          cat ('source("fvsRunUtilities.R")\n',file=rs)
          cmd = paste0('dbDrv = dbDriver("SQLite")\n',
                       'dbOcon = dbConnect(dbDrv,"FVSOut.db")')
          cat (cmd,"\n",file=rs)
          cmd = paste0('cat (pid,"Adding results to output database; title=",title,"\n")')
          cat (cmd,"\n",file=rs)
          cmd = paste0('cat (pid,"Adding results to output database; title=",title,"\n",file="',pidfile,
                       '")')
          cat (cmd,"\n",file=rs)
          cmd = paste0('addNewRun2DB("',globals$fvsRun$uuid,'",dbOcon)')
          cat (cmd,"\n",file=rs)
          cat ("dbDisconnect(dbOcon)\n",file=rs)
          cmd = paste0("unlink('",pidfile,"')")
          cat (cmd,"\n",file=rs)
          progress$set(message = "Run starting in background", 
              detail = "", value = 4)
          unlink(paste0(globals$fvsRun$uuid,".db"))
          close (rs)
          cmd = paste0("Rscript --no-restore --no-save --no-init-file ",runScript,
                       " > ",runScript,".Rout")
          if (.Platform$OS.type == "unix") cmd = paste0("nohup ",cmd)
cat ("cmd=",cmd,"\n")
          system (cmd,wait=FALSE) 
          Sys.sleep(1)
          updateSelectInput(session=session, inputId="bkgRuns", 
                          choices=getBkgRunList(),selected=0)
          progress$close()
          return()
        }
        fvschild = makePSOCKcluster(1)
        cmd = paste0("clusterEvalQ(fvschild,for (rf in dir('",rFVSDir,
          "')) source(paste0('",rFVSDir,"','/',rf)))")
cat ("load rFVS cmd=",cmd,"\n")          
        rtn = try(eval(parse(text=cmd)))
        if (class(rtn) == "try-error") return()
        cmd = paste0("clusterEvalQ(fvschild,fvsLoad('",
             globals$fvsRun$FVSpgm,"',bin='",binDir,"'))")
cat ("load FVSpgm cmd=",cmd,"\n")          
        rtn = try(eval(parse(text=cmd)))
        if (class(rtn) == "try-error") return()          
        # if not using the default run script, load the one requested.
    
        if (globals$fvsRun$runScript != "fvsRun")
        {
          cmd = paste0("clusterEvalQ(fvschild,",
               "source('customRun_",globals$fvsRun$runScript,".R'))")
cat ("run script load cmd=",cmd,"\n")
          rtn = try(eval(parse(text=cmd)))
          if (class(rtn) == "try-error") return()        
          runOps <<- if (is.null(globals$fvsRun$uiCustomRunOps)) list() else 
            globals$fvsRun$uiCustomRunOps
          rtn = try(clusterExport(fvschild,list("runOps"))) 
          if (class(rtn) == "try-error") return()
        }
        cmd = paste0("clusterEvalQ(fvschild,",
              'fvsSetCmdLine("--keywordfile=',globals$fvsRun$uuid,'.key"))')
cat ("load run cmd=",cmd,"\n")
        rtn = try(eval(parse(text=cmd))) 
        if (class(rtn) == "try-error") return()
        #on exit of the reactive context
        on.exit({          
          progress$close()
cat ("exiting, stop fvschild\n")          
          try(stopCluster(fvschild))
        }) 
        #####
cat ("at for start\n") 
        allSum = list()
        for (i in 1:length(globals$fvsRun$stands))
        {
          detail = paste0("Stand ",i," StandId=",globals$fvsRun$stands[[i]][["sid"]])          
          progress$set(message = "FVS running", detail = detail, value = i+4) 
          rtn = if (globals$fvsRun$runScript != "fvsRun")
            {
              cmd = paste0("clusterEvalQ(fvschild,",globals$fvsRun$runScript,"(runOps))")
cat ("custom run cmd=",cmd,"\n")              
              try(eval(parse(text=cmd)))
            } else try(clusterEvalQ(fvschild,fvsRun()))
          if (class(rtn) == "try-error")
          { 
            cat ("run try error\n")
            break
          }
          if (rtn != 0) break          
          ids = clusterEvalQ(fvschild,fvsGetStandIDs())[[1]]
          rn = paste0("SId=",ids["standid"],";MId=",ids["mgmtid"])
cat ("rn=",rn,"\n")
          allSum[[rn]] = clusterEvalQ(fvschild,
                         fvsSetupSummary(fvsGetSummary()))[[1]]
        }
        if (rtn == 0) clusterEvalQ(fvschild,fvsRun())        
        stopCluster(fvschild)
        progress$set(message = "Scanning output for errors", detail = "", 
                    value = length(globals$fvsRun$stands)+4)
        errScan = try(errorScan(paste0(globals$fvsRun$uuid,".out")))
        if (class(errScan) == "try-error") errScan = 
          "Error scan failed likely due to invalid multibyte strings in output"
        output$uiErrorScan <- renderUI(list(
          h5("FVS output error scan"),
          tags$style(type="text/css", paste0("#errorScan { overflow:auto; ",
             "height:150px; font-family:monospace; font-size:90%;}")),
          HTML(paste(errScan,"<br>"))))
        if (length(dir(globals$fvsRun$uuid)) == 0) file.remove(globals$fvsRun$uuid)
        progress$set(message = if (length(allSum) == length(globals$fvsRun$stands))
                    "FVS finished" else
                    "FVS run failed", detail = "", 
                    value = length(globals$fvsRun$stands)+5)
        Sys.sleep(.1)       
cat ("length(allSum)=",length(allSum),"\n")
        if (length(allSum) == 0) {Sys.sleep(.4); return()}
        progress$set(message = "FVS finished",  
                     detail = "Merging output to master database",
                     value = length(globals$fvsRun$stands)+6)
        res = addNewRun2DB(globals$fvsRun$uuid,dbGlb$dbOcon)
cat ("addNewRun2DB res=",res,"\n")
        unlink(paste0(globals$fvsRun$uuid,".db"))
        X = vector("numeric",0)
        hfacet = vector("character",0)
        Y = vector("numeric",0)
        Legend=vector("character",0)
        progress$set(message = "Building plot", detail = "", 
                     value = length(globals$fvsRun$stands)+6)
        for (i in 1:length(allSum)) 
        {
          X = c(X,rep(allSum[[i]][,"Year"],2))
          hfacet = c(hfacet,rep(names(allSum)[i],nrow(allSum[[i]])*2))
          Y = c(Y,c(allSum[[i]][,"TCuFt"],allSum[[i]][,"TPrdTCuFt"]))
          Legend=c(Legend,c(rep("TCuFt",nrow(allSum[[i]])),
                                  rep("TPrdTCuFt",nrow(allSum[[i]]))))
        }
        toplot = data.frame(X = X, hfacet=as.factor(hfacet), Y=Y, 
                  Legend=as.factor(Legend))
        width = max(4,nlevels(toplot$hfacet)*2)
        height = 2.5
        plt = if (nlevels(toplot$hfacet) < 5)
          ggplot(data = toplot) + facet_grid(.~hfacet) + 
            geom_line (aes(x=X,y=Y,color=Legend,linetype=Legend)) +
            labs(x="Year", y="Total cubic volume per acre") + 
            theme(text = element_text(size=9), legend.position="none",
                  panel.background = element_rect(fill="gray95"),
                  axis.text = element_text(color="black"))  else
        {
          width = 3
          toplot$Legend = as.factor(paste0(toplot$Legend,toplot$hfacet))
          ggplot(data = toplot) +  
            geom_line (aes(x=X,y=Y,color=Legend,alpha=.5)) + 
            labs(x="Year", y="Total cubic volume per acre") + 
               theme(text = element_text(size=9), legend.position="none",
                     panel.background = element_rect(fill="gray95"),
                     axis.text = element_text(color="black"))
        }
        png("quick.png", width=width, height=height, units="in", res=144)
        print(plt)
        dev.off()
        output$uiRunPlot <- renderUI(
                plotOutput("runPlot",width="100%",height=paste0((height+1)*144,"px")))
        output$runPlot <- renderImage(list(src="quick.png", width=(width+1)*144, 
                height=(height+1)*144), deleteFile=TRUE)
cat ("setting currentQuickPlot, input$runSel=",input$runSel,"\n")
        globals$currentQuickPlot = globals$fvsRun$uuid
      }
    })
  })

## bkgKill  
  observe({  
    if (input$bkgKill == 0) return()
    isolate ({
      if (!is.null(input$bkgRuns))
      {
        uuid=sub(".pidStatus","",input$bkgRuns)
        killIfRunning(uuid)
        removeFVSRunFiles(uuid)
      }
      updateSelectInput(session=session, inputId="bkgRuns", 
                      choices=getBkgRunList(),selected=0)
    })
  })
  
## bkgRefresh
  observe({  
    if (input$bkgRefresh == 0) return()
    updateSelectInput(session=session, inputId="bkgRuns", 
                      choices=getBkgRunList(),selected=0)
  })
  
  ## Download handlers
  output$dlFVSDatadb <- downloadHandler(filename="FVS_Data.db",
       content = function (tf = tempfile()) file.copy("FVS_Data.db",tf))
  output$dlFVSOutdb <- downloadHandler(filename="FVSOut.db",
       content = function (tf = tempfile()) file.copy("FVSOut.db",tf))
  ## Download dlRenderData
  output$dlRenderData <- downloadHandler(filename="table.csv",
      content=function (tf = tempfile())
      {
        if (nrow(fvsOutData$render) > 0)
          write.csv(fvsOutData$render,file=tf,row.names=FALSE) else 
          cat (file=tf,'"No data"\n')
      }, contentType="text")
  ## DownLoad
  output$dlFVSRunout <- downloadHandler(filename=function ()
      paste0(globals$fvsRun$title,"_FVSoutput.txt"),
      content=function (tf = tempfile())
      {
        sfile = paste0(input$runSel,".out")
        if (file.exists(sfile)) file.copy(sfile,tf) else
          cat (file=tf,"Output not yet created.\n")
      }, contentType="text")
  ## Download keywords
  output$dlFVSRunkey <- downloadHandler(filename=function ()
      paste0(globals$fvsRun$title,"_FVSkeywords.txt"),
      content=function (tf = tempfile())
      {
        sfile = paste0(input$runSel,".key")
        if (file.exists(sfile)) file.copy(sfile,tf) else
          cat (file=tf,"Keywords not yet created.\n")
      }, contentType="text")
  ## Download FVSData.zip  
  output$dlFVSRunZip <- downloadHandler(filename="FVSData.zip",	
     content = function (tf = tempfile())	
         {
           tempDir = paste0(dirname(tf),"/tozip")
           dir.create(tempDir)
           for (ele in input$dlZipSet)
           {
             switch (ele,
               outdb = file.copy(from="FVSOut.db",
                                 to=paste0(tempDir,"/FVSOut.db")),
               key   = {
                 from=paste0(input$runSel,".key")
                 if (file.exists(from)) file.copy(from=from,
                   to=paste0(tempDir,"/",globals$fvsRun$title,"_FVSkeywords.txt"))
               },
               out   = {
                 from=paste0(input$runSel,".out")
                 if (file.exists(from)) file.copy(from=from,
                   to=paste0(tempDir,"/",globals$fvsRun$title,"_FVSoutput.txt"))
               },
               subdir= {
                 from=input$runSel
                 if (dir.exists(from)) 
                 {
                   to = paste0(tempDir,"/",globals$fvsRun$title,"_SVS/")
                   dir.create (to)
                   file.copy(from=from,to=to,recursive = TRUE)
                   file.copy(from=paste0(from,"_index.svs"),to=to)
                 }
               },              
               FVS_Data = file.copy(from="FVS_Data.db",
                                    to=paste0(tempDir,"/FVS_Data.db")),
               FVS_Runs = file.copy(from="FVS_Runs.RData",
                                    to=paste0(tempDir,"/FVS_Runs.RData")),
               customSQL = file.copy(from="customQueries.RData",
                                    to=paste0(tempDir,"/customQueries.RData")),
               FVS_kcps = file.copy(from="FVS_kcps.RData",
                                    to=paste0(tempDir,"/FVS_kcps.RData"))
           )}
           curdir = getwd()
           setwd(tempDir)
           zip(tf,dir())
           unlink(tempDir,recursive = TRUE)
           setwd(curdir)
         }, contentType="application/zip")
      
  ## cpReload
  observe({      
    if (input$rightPan == "Build Components")
    {
      if (length(globals$customCmps) == 0) 
      {
        if (file.exists("FVS_kcps.RData"))
        {
          load("FVS_kcps.RData")
          globals$customCmps = customCmps
        }
      }
      if (length(globals$customCmps) == 0) 
      {
        updateSelectInput(session=session, inputId="kcpSel", 
                          choices=list(),selected=0)
      } else
      {
        sels = as.list(as.character(1:length(globals$customCmps)))
        names(sels) = names(globals$customCmps)
        updateSelectInput(session=session, inputId="kcpSel", choices=sels, 
           selected = 0)
      }
      updateTextInput(session=session, inputId="kcpTitle", value="")
      updateTextInput(session=session, inputId="kcpEdit", value="") 
    }
  })


  ## kcpSel
  observe({  
    if (is.null(input$kcpSel))
    { 
      updateTextInput(session=session, inputId="kcpTitle", value="") 
    } else
    {
      sel = as.numeric(input$kcpSel)
cat ("kcpSel called, sel=",sel,"\n")
      if (length(globals$customCmps) >= sel) 
      {
        updateTextInput(session=session, inputId="kcpTitle", 
          value=names(globals$customCmps)[sel])
        updateTextInput(session=session, inputId="kcpEdit",
          value=globals$customCmps[[sel]])
      } else
      {
        updateTextInput(session=session, inputId="kcpTitle", value="")
        updateTextInput(session=session, inputId="kcpEdit", value="")
      }
    }
cat ("kcpSel called, input$kcpSel=",input$kcpSel," isnull=",
    is.null(input$kcpSel),"\n")
  })

  ## kcpSaveCmps
  observe({  
    if (input$kcpSaveCmps > 0)
    {
      isolate ({
cat ("kcpSaveCmps called, kcpTitle=",input$kcpTitle," isnull=",
     is.null(input$kcpTitle),"\n")
        if (is.null(input$kcpTitle) || input$kcpTitle == "")
        {
          newTit = paste0("Component ",length(globals$customCmps)+1) 
          updateTextInput(session=session, inputId="kcpTitle", value=newTit)
        } else newTit = input$kcpTitle
        globals$customCmps[[newTit]] = input$kcpEdit
        customCmps = globals$customCmps
        save(file="FVS_kcps.RData",customCmps)
        sels = as.list(as.character(1:length(globals$customCmps)))
        names(sels) = names(globals$customCmps)
        updateSelectInput(session=session, inputId="kcpSel", choices=sels,
           selected = as.character(match(newTit,names(sels))))
      })
    }
  })

  ## kcpDelete
  observe({  
    if (input$kcpDelete > 0)
    {
      isolate ({
        if (is.null(input$kcpTitle)) return()
        globals$customCmps[[input$kcpTitle]] = NULL
        customCmps = globals$customCmps
        save(file="FVS_kcps.RData",customCmps)
        if (length(customCmps) > 0)
        {
          sels = as.list(as.character(1:length(globals$customCmps)))
          names(sels) = names(globals$customCmps)
        } else sels=list()
        updateSelectInput(session=session, inputId="kcpSel", choices=sels, 
           selected = 0)
        updateTextInput(session=session, inputId="kcpTitle", value="")
        updateTextInput(session=session, inputId="kcpEdit", value="")
      })
cat ("kcpDelete called, input$kcpDelete=",input$kcpDelete," isnull=",
      is.null(input$kcpDelete),"\n")
    }
  })

  ## kcpNew
  observe({  
    if (input$kcpNew > 0)
    {
      isolate ({
        updateSelectInput(session=session, inputId="kcpSel", selected = 0)
        updateTextInput(session=session, inputId="kcpEdit", value="")
      })
cat ("kcpNew called, input$kcpNew=",input$kcpNew,"\n")
    }
  })

  ## kcpUpload
  observe({  
    if (is.null(input$kcpUpload)) return()
    data=scan(file=input$kcpUpload$datapath,sep="\n",what="",quiet=TRUE)
    if (length(data)==0) return()
    isolate ({
      addnl = TRUE
      if (is.null(input$kcpTitle) || input$kcpTitle == "")
      {
        addnl = FALSE
        updateTextInput(session=session, inputId="kcpTitle", value=
          paste("From:",input$kcpUpload$name))
      }
      updateTextInput(session=session, inputId="kcpEdit", value=
          paste0(input$kcpEdit,
            paste(if (addnl) "\n* From:" else "* From:",
                  input$kcpUpload$name,"\n"),paste(data,collapse="\n")))
    })
  })

  ## kcpAppend
  observe({  
    if (input$kcpAppend > 0)
    {
      isolate ({
        topaste = findCmp(globals$fvsRun,input$simCont[1])
        if (is.null(topaste)) return()
        updateTextInput(session=session, inputId="kcpEdit", value=
          paste0(input$kcpEdit,"* ",topaste$title,"\n",topaste$kwds,"\n"))
        session$sendCustomMessage(type="refocus", "kcpEdit")
      })
    }
  })

  ## Tools, related to FVSRefresh
  observe({    
    if (input$topPan == "Tools") 
    {
cat ("Tools hit\n")       
      if (!exists("fvsBinDir")) return()
      if (!file.exists(fvsBinDir)) return()
      if (!exists("pgmList")) return()
      pgmFlip = as.list(names(pgmList))
      names(pgmFlip) = paste0(names(pgmList),": ",unlist(pgmList))
      shlibsufx <- if (.Platform$OS.type == "windows") "[.]dll$" else "[.]so$"
      haveFVS <- dir("FVSbin",pattern=shlibsufx) 
      haveFVSp <- sub(shlibsufx,"",haveFVS)
      avalFVS <- dir(fvsBinDir,pattern=shlibsufx)
      avalFVSp <- sub(shlibsufx,"",avalFVS)
cat ("avalFVSp=",avalFVSp,"\n")       
      updateSelectInput(session=session, inputId="FVSprograms", 
        choices=pgmFlip,selected=haveFVSp)
    } 
  })
  

  ## FVSRefresh
  observe({  
    if (input$FVSRefresh == 0) return()               
cat ("FVSRefresh\n")
    isolate({
      if (length(input$FVSprograms) == 0) return()
      if (!exists("fvsBinDir")) fvsBinDir="FVSbin/"
      if (!file.exists(fvsBinDir))
      {
        session$sendCustomMessage(type="infomessage",
          message="FVS programs can not be refreshed on this system.")
        rm (fvsBinDir)
      } else
      {
        shlibsufx <- if (.Platform$OS.type == "windows") ".dll" else ".so"
        i = 0
        for (pgm in input$FVSprograms)
        {
          rtn=file.copy(from=paste0(fvsBinDir,"/",pgm,shlibsufx),to="FVSbin")
          if (rtn) i = i+1
        }
        session$sendCustomMessage(type="infomessage",
          message=paste0(i," of ",length(input$FVSprograms),
            " selected FVS programs refreshed."))
        if (i) output$locReload<-renderUI(tags$script("location.reload();"))
      } 
    })
  })

  ## deleteRun  
  observe({
    if(input$deleteRun > 0)
    {
      isolate({
        tit = globals$fvsRun$title
        session$sendCustomMessage(type = "dialogContentUpdate",
          message = list(id = "deleteRunDlg", 
            message = paste0('Delete run "',
            globals$fvsRun$title,'" (and all related outputs)?')))
      })
    }
  })
  observe({
    if (input$deleteRunDlgBtn > 0)
    {
      isolate({
cat ("delete run",globals$fvsRun$title," uuid=",globals$fvsRun$uuid," runSel=",input$runSel,
 "lenRuns=",length(globals$FVS_Runs),"\n")
        globals$FVS_Runs[[globals$fvsRun$uuid]] = NULL
        killIfRunning(globals$fvsRun$uuid)
        removeFVSRunFiles(globals$fvsRun$uuid,all=TRUE)
        deleteRelatedDBRows(globals$fvsRun$uuid,dbGlb$dbOcon)
        if (length(globals$FVS_Runs) == 0) unlink("FVS_Runs.RData") else
        {
          FVS_Runs = globals$FVS_Runs
          save (file="FVS_Runs.RData",FVS_Runs)
        }
        if (file.exists("projectId.txt"))
        {
          prjid = scan("projectId.txt",what="",sep="\n",quiet=TRUE)
          write(file="projectId.txt",prjid)
        } 
        globals$saveOnExit = FALSE
        output$locReload<-renderUI(tags$script("location.reload();"))
      })
    }
  })

  
  ## deleteAllRuns
  observe({
    if(input$deleteAllRuns > 0)
    {
      session$sendCustomMessage(type = "dialogContentUpdate",
        message = list(id = "deleteAllRunsDlg",
                  message = "Delete all runs and outputs?"))
    }
  })
  observe({  
    if (input$deleteAllRunsDlgBtn == 0) return()
    isolate({
cat ("delete all runs\n")
      rmfiles=dir(pattern="[.]pidStatus$")      
      for (tokill in rmfiles) killIfRunning(sub(".pidStatus","",tokill))
      unlink("FVSOut.db")
      unlink("FVS_Runs.RData")
      for (uuid in names(globals$FVS_Runs)) removeFVSRunFiles(uuid,all=TRUE)
      globals$saveOnExit = FALSE
      output$locReload<-renderUI(tags$script("location.reload();"))
    })  
  })
  
  ## interfaceRefresh
  observe({
    if(input$interfaceRefresh > 0)
    {
      session$sendCustomMessage(type = "dialogContentUpdate",
        message = list(id = "interfaceRefreshDlg",
                  message = "Are you sure?"))
    }
  })
  observe({  
    if (input$interfaceRefreshDlgBtn == 0) return()
cat ("interfaceRefreshDlgBtn\n") 
    if (.Platform$OS.type == "windows") return()
## TODO: set this up so that it works "ONLocal" and with windows.
    if (file.exists("../../FVSOnline/settings.R")) 
             source("../../FVSOnline/settings.R") 
    if (!exists("fvsOnlineDir")) return()
    # shiny code, etc
    needed=paste(paste0(fvsOnlineDir,FVSOnlineNeeded),collapse=" ")  
    system (paste0("cp -R ",needed," ."))
    output$locReload<-renderUI(tags$script("location.reload();"))
  }) 
  
  ## restoreYesterday 
  observe({
    if(length(input$restoreYesterday) && input$restoreYesterday > 0)
    {
      session$sendCustomMessage(type = "dialogContentUpdate",
        message = list(id = "restoreYesterdayDlg",
                  message = "Are you sure?"))
    }
  })
  observe({  
    if (length(input$restoreYesterdayDlgBtn) && 
        input$restoreYesterdayDlgBtn == 0) return()
cat ("restoreYesterdayDlgBtn\n")
    if (.Platform$OS.type == "windows") return()
## TODO: Set this up to work "Onlocal" and with windows      
    if (!exists("fvsWorkBackup")) return()
    # recover everything from backup
    cdir=getwd()
    backup = sub(fvsWork,fvsWorkBackup,cdir)
cat ("backup=",backup,"\n")    
    if (file.exists(backup)) 
    {
      system (paste0("rm -r ",cdir,"/*"))
      system (paste0("cp -R -p ",backup,"/* ."))
      globals$saveOnExit=FALSE
    }   
    output$locReload<-renderUI(tags$script("location.reload();"))
  }) 

  ## rpRestart
  observe({
    if (input$rpRestart == 0) return()
    getRptFile(TRUE)
    isolate({
      if (nchar(input$rpTitle)) appendToReport(paste0("# ",input$rpTitle))
      appendToReport(paste0("### ",
        format(Sys.time(),"%a %b %d %X %Z %Y")))
    })
  })  
  ## rpBldDwnLd
  output$rpBldDwnLd <- downloadHandler(filename="FVSReport.docx",
       content =  function (tf = tempfile()) generateReport(tf)) 
  ## rpTableAdd
  observe({  
    if (input$rpTableAdd == 0) return()
    appendToReport(fvsOutData$render)
  }) 
  ## rpPlotAdd
  observe(if (input$rpPlotAdd > 0) appendPlotToReport())
  
  ##topHelp
  observe({
    if (input$topPan == "Help")
    {
      fn = "fvsOnlineHelp.html"
      help = readChar(fn, file.info(fn)$size)     
      output$uiHelpText <- renderUI(HTML(help))
    }
  })
  
  ##### dataEditor upload code  
  observe({
    if(input$topPan == "Input Database")
    {
      updateTabsetPanel(session=session, inputId="inputDBPan", 
        selected="Replace existing database")
      output$replaceActionMsg <- renderText("")
    }
  })
  observe({
    if(input$inputDBPan == "Replace existing database") 
    {
cat ("Replace existing database\n")
      output$replaceActionMsg <- renderText("")
    }
  })
  
  initNewInputDB <- function ()
  {
    updateSelectInput(session=session, inputId="editSelDBtabs", choices=list()) 
    updateSelectInput(session=session, inputId="editSelDBvars", choices=list()) 
    updateSelectInput(session=session, inputId="inVars", choices=list()) 
    updateSelectInput(session=session, inputId="Groups", choices=list()) 
    updateSelectInput(session=session, inputId="Stands", choices=list()) 
    output$tbl <- renderRHandsontable(NULL)
    output$stdSel <- output$navRows <- renderUI(NULL)
    dbGlb$rows <- NULL
    dbGlb$rowSelOn <- dbGlb$navsOn <- FALSE
  }
  
  ## installTrainDB
  observe({  
    if (input$installTrainDB == 0) return()
    dbDisconnect(dbGlb$dbIcon)
    if (file.exists("FVS_Data.db")) file.remove("FVS_Data.db")
    file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
    output$replaceActionMsg <- renderText("Training database installed")
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
    dbSendQuery(dbGlb$dbIcon,'attach ":memory:" as m')
    initNewInputDB()
    loadVarData(globals,prms,dbGlb$dbIcon)                                              
  }) 
  ## installEmptyDB
  observe({  
    if (input$installEmptyDB == 0) return()
    dbDisconnect(dbGlb$dbIcon)
    if (file.exists("FVS_Data.db")) file.remove("FVS_Data.db")
    file.copy("FVS_Data.db.empty","FVS_Data.db",overwrite=TRUE)
    output$replaceActionMsg <- renderText("Empty database installed")
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
    dbSendQuery(dbGlb$dbIcon,'attach ":memory:" as m')
    initNewInputDB()
    loadVarData(globals,prms,dbGlb$dbIcon)                                              
  }) 
  ## Upload new database
  observe({  
    if (is.null(input$uploadNewDB)) return()   
    if (regexpr("\\.accdb$",input$uploadNewDB$name) == 0 && 
        regexpr("\\.mdb$",input$uploadNewDB$name)   == 0 &&
        regexpr("\\.db$",input$uploadNewDB$name)    == 0) return()
    dbDisconnect(dbGlb$dbIcon)
    unlink ("FVS_Data.db")
    if (regexpr("\\.db$",input$uploadNewDB$name) > 1) 
    {
      file.copy(input$uploadNewDB$datapath,"FVS_Data.db",overwrite = TRUE)
    } else {   
      progress <- shiny::Progress$new(session,min=1,max=12)
      progress$set(message = "Create schema", value = 1)
      source("dbNamesAndTypes.R")
      curDir=getwd()
      progress$set(message = "Process schema", value = 2)

      setwd(dirname(input$uploadNewDB$datapath))
cat("curDir=",curDir," input dir=",getwd(),"\n") 
      cmd = if (.Platform$OS.type == "windows") 
         shQuote(paste0("java -jar ",curDir,"/access2csv.jar ",
              basename(input$uploadNewDB$datapath)," --schema"),type="cmd2") else
         paste0("java -jar '",curDir,"/access2csv.jar' ",
              basename(input$uploadNewDB$datapath)," --schema")
cat ("cmd=",cmd,"\n")
      schema = system(cmd,intern = TRUE)
      if (!exists("schema") || length(schema) < 2) 
      {
        setwd(curDir) 
        progress$close()     
        output$actionMsg = renderText("'schema' not created, no data loaded.")
        session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
        return()
      }
      fix = grep (")",schema)        
      schema[fix] = sub (")",");",schema[fix])
      fix = fix - 1
      schema[fix] = sub (",","",schema[fix])       
      for (i in 1:length(schema))
      {
        # add a "n" to identifiers that starts with a numeric char.
        t1 = scan(text=schema[i],what="character",quiet=TRUE)
        cn = if (toupper(t1[1]) == "CREATE" && toupper(t1[2]) == "TABLE") 3 else 1
        t1 = t1[cn]
        t1 = if (toupper(t1[1]) == "CREATE" && toupper(t1[2]) == "TABLE") t1[3] else t1[1]
        c1 = substr(t1,1,1)
        n1 = suppressWarnings(as.numeric(c1))
        if (!is.na(n1)) schema[i] = sub(c1,paste0("n",c1),schema[i])      
        if (substring(schema[i],1,12) == "CREATE TABLE") 
        {
          tblName = scan(text=schema[i],what="character",sep=" ",quiet=TRUE)[3]
          cknt = switch(toupper(tblName),
            "FVS_STANDINIT" = standNT,
            "FVS_TREEINIT"  = treeNT,
            NULL)
          next
        }
        if (substring(schema[i],1,2)  == ");") next
        items = unlist(strsplit(schema[i]," "))
        items = items[nchar(items)>0]
        ckntrow = match(toupper(items[1]),toupper(cknt[,1]))
        if (!is.na(ckntrow)) 
        {
          items[1] = cknt[ckntrow,1]
          comma = nchar(items[2])
          comma = substring(items[2],comma,comma)
          items[2] = cknt[ckntrow,2] 
          if (comma == ",") items[2] = paste0(items[2],",")
        }
        schema[i] = paste0(' "',items[1],'" ',items[2])
      }        
      schema = gsub(" LONG,"," INTEGER,",schema)
      schema = gsub(" DOUBLE,"," REAL,",schema)
      schema = gsub(" MEMO,"," TEXT,",schema)
      schema = gsub(" FLOAT,"," REAL,",schema)
      schema = gsub(" MEMO,"," TEXT,",schema)
      schema = gsub(" SHORT_DATE_TIME,"," TEXT,",schema)
      cat (paste0(schema,"\n"),file="schema")
      progress$set(message = "Extract data", value = 3) 
      
      cmd = if (.Platform$OS.type == "windows") 
         shQuote(paste0("java -jar ",curDir,"/access2csv.jar ",
              basename(input$uploadNewDB$datapath)),type="cmd2") else
         paste0("java -jar '",curDir,"/access2csv.jar' ",
               basename(input$uploadNewDB$datapath))
cat ("cmd=",cmd,"\n")       
      system(cmd)  
      progress$set(message = "Import schema to Sqlite3", value = 4) 
      system (paste0 ("sqlite3 ","FVS_Data.db"," < schema"))
      fix = grep ("CREATE TABLE",schema)
      schema = sub ("CREATE TABLE ","",schema[fix])
      schema = sub (" [(]",".csv",schema)
      i = 5
      for (s in schema)
      {
        cat (".separator ,\n",file="schema")
        # if file does not exist, then maybe a "n" was added to the name above.
        if (!file.exists(s)) file.rename(from=substr(s,2,999),to=s)
        cat (".import ",s," ",sub(".csv","",s),"\n",file="schema",append=TRUE)
        progress$set(message = paste0("Import ",s), value = i) 
        i = i+1;
        cmd = paste0("sqlite3 ","FVS_Data.db"," < schema")
cat ("cmd=",cmd,"\n")
        system (cmd)
      }
      progress$set(message = "Copy tables", value = i+1) 
      lapply(schema,unlink) 
      file.copy("FVS_Data.db",paste0(curDir),overwrite=TRUE)
      unlink("schema") 
      setwd(curDir)      
      Sys.sleep (.5) 
      progress$close()  
    }
    unlink(input$uploadNewDB$datapath)
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
    dbSendQuery(dbGlb$dbIcon,'attach ":memory:" as m')
    progress <- shiny::Progress$new(session,min=1,max=3)
    progress$set(message = "Checking database query keywords", value = 1)
    source("editDataUtilities.R")
    fixFVSKeywords(dbGlb,progress) 
    checkMinColumnDefs(dbGlb,progress)
    Sys.sleep (.5) 
    progress$close()    
    loadVarData(globals,prms,dbGlb$dbIcon)                                              
    output$replaceActionMsg <- renderText("Uploaded database installed")
    session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
    initNewInputDB()
  }) 
  observe({
    if(input$inputDBPan == "Upload and insert new rows (.csv)") 
    {
cat ("Upload new rows\n")
      source("editDataUtilities.R")
      tbs <- dbListTables(dbGlb$dbIcon)
      dbGlb$tbsCTypes <- lapply(tbs,function(x,dbIcon) 
        {
          tb <- dbGetQuery(dbIcon,paste0("PRAGMA table_info('",x,"')"))
          tbtypes = toupper(tb[,"type"])
          res = vector("logical",length(tbtypes))
          res[grep ("INT",tbtypes)] = TRUE
          res[grep ("FLOAT",tbtypes)] = TRUE
          res[grep ("REAL",tbtypes)] = TRUE
          names(res) = tb[,"name"]                                               
          res[] = !res
        }, dbGlb$dbIcon)     
      names(dbGlb$tbsCTypes) = tbs 
      if (length(tbs))
      {
        idx <- grep ("FVS_Climattrs",tbs)
        if (length(idx)) tbs = tbs[-ids]
        idx <- grep ("StandInit",tbs)
        if (length(idx) == 0) idx=1     
        updateSelectInput(session=session, inputId="uploadSelDBtabs", choices=tbs, 
          selected=tbs[idx])
      } else updateSelectInput(session=session, inputId="uploadSelDBtabs",  
               choices=list())
      output$uploadActionMsg <- renderText(if (length(tbs)) "" else 
        "No tables in existing database. Use 'Replace existing' to install a new one.")        
      initNewInputDB()
    }
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
        output$uploadActionMsg = renderText("Input empty, no data loaded.")
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
        output$uploadActionMsg = renderText("All rows were empty,  no data loaded.")
        Sys.sleep(1)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      }
      cols = na.omit(pmatch(tolower(colnames(indat)),
               tolower(names(dbGlb$tbsCTypes[[dbGlb$tblName]]))))
      if (length(cols) == 0) 
      {
        output$uploadActionMsg = renderText(paste0("No columns match what is defined for ",
               dbGlb$tblName,", no data loaded."))
        Sys.sleep(1)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      }
      kill = attr(cols,"na.action")
      if (length(kill)) indat = indat[,-kill,drop=FALSE]
      types = dbGlb$tbsCTypes[[dbGlb$tblName]][cols]
      req = switch(dbGlb$tblName,
         FVS_StandInit = c("Stand_ID","Variant","Inv_Year"),
         FVS_TreeInit  = c("Stand_ID","Species","DBH"),
         FVS_GroupAddFilesAndKeywords = c("Groups"),
         NULL)
      if (!is.null(req) && !all(req %in% names(types)))
      {
        output$uploadActionMsg = renderText(paste0("Required columns were missing for ",
               dbGlb$tblName,", no data loaded."))
        Sys.sleep(1)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      }
      colnames(indat) = names(types)
      quote = types[types]
      if (length(quote)) for (cn in names(quote)) 
        indat[,cn] = paste0("'",indat[,cn],"'")
      dbBegin(dbGlb$dbIcon)
      err = FALSE
      for (i in 1:nrow(indat))
      {
        row = indat[i,,drop=FALSE]
        row = row[,!is.na(row),drop=FALSE]
        qry = paste0("insert into ",dbGlb$tblName," (",
                paste0(colnames(row),collapse=","),
                  ") values (",paste0(row,collapse=","),");")
        res = try(dbSendQuery(dbGlb$dbIcon,qry))
        if (class(res) == "try-error") {err=TRUE; break}
      }
      if (err) 
      {
        dbRollback(dbGlb$dbIcon) 
        output$uploadActionMsg = renderText(paste0("Error processing: ",qry))
        return()
      } else {
        dbCommit(dbGlb$dbIcon)
        output$uploadActionMsg = renderText(paste0(nrow(indat)," rows were inserted into ",
               dbGlb$tblName))
        loadVarData(globals,prms,dbGlb$dbIcon)                                              
      }
      Sys.sleep(1)
      session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
      dbSendQuery(dbGlb$dbIcon,paste0("delete from ",dbGlb$tblName,
        " where Stand_ID = ''"))      
      res = dbSendQuery(dbGlb$dbIcon,paste0("select distinct Stand_ID from ",
                        dbGlb$tblName))
      dbGlb$sids = dbFetch(res,n=-1)$Stand_ID
      dbClearResult(dbGlb$dbIcon)
      if (any(is.na(dbGlb$sids))) dbGlb$sids[is.na(dbGlb$sids)] = ""
      if (dbGlb$rowSelOn && length(dbGlb$sids)) 
        updateSelectInput(session=session, inputId="rowSelector",
          choices  = as.list(dbGlb$sids), selected=unique(indat[,"Stand_ID"])) else 
        output$stdSel <- mkStdSel(dbGlb)
      
      qry <- paste0("select _ROWID_,* from ",dbGlb$tblName)
      qry <- if (length(input$rowSelector))
        paste0(qry," where Stand_ID in (",
              paste0("'",input$rowSelector,"'",collapse=","),");") else
        paste0(qry,";") 
      dbGlb$tbl <- dbGetQuery(dbGlb$dbIcon,qry)
      rownames(dbGlb$tbl) = dbGlb$tbl$rowid
      for (col in 2:ncol(dbGlb$tbl))
        if (class(dbGlb$tbl[[col]]) != "character") 
           dbGlb$tbl[[col]] = as.character(dbGlb$tbl[[col]])
      if (nrow(dbGlb$tbl) == 0) dbGlb$rows = NULL else 
      {
        dbGlb$tbl$Delete = FALSE
        dbGlb$rows <- c(dbGlb$rows[1],
                        min(nrow(dbGlb$tbl),dbGlb$rows[2]))
        output$tbl <- renderRHandsontable(rhandsontable(
          dbGlb$tbl[dbGlb$rows[1]:dbGlb$rows[2],
          union(c("Delete"),input$selectdbvars),drop=FALSE],
          readOnly=FALSE,useTypes=TRUE,contextMenu=FALSE))
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
      output$uploadActionMsg = renderText("FVSClimAttrs.csv not found.")
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
    climTab <- dbListTables(dbGlb$dbIcon)
    if (!("FVS_ClimAttrs" %in% climTab))
    {
cat ("no current FVS_ClimAttrs\n")
      progress$set(message = "Building FVS_ClimAttrs table",value = 4) 
      dbWriteTable(dbGlb$dbIcon,"FVS_ClimAttrs",climd)
      output$actionMsg = renderText("FVSClimAttrs created.")
      rm (climd)
      progress$set(message = "Creating FVS_ClimAttrs index",value = 6)
      dbSendQuery(dbGlb$dbIcon,'drop index if exists StdScnIndex')
      dbSendQuery(dbGlb$dbIcon,"create index StdScnIndex on FVS_ClimAttrs (Stand_ID, Scenario);")
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
    dbBegin(dbGlb$dbIcon)
    results = apply(distinct,1,function (x,dbIcon)
    {
      dbSendQuery(dbIcon,paste0('delete from FVS_ClimAttrs where Stand_ID = "',
         x[1],'" and Scenario = "',x[2],'"'))
    }, dbGlb$dbIcon)
    dbCommit(dbGlb$dbIcon)
    dbSendQuery(dbGlb$dbIcon,'drop index if exists StdScnIndex')
    
    dbSendQuery(dbGlb$dbIcon,'attach database "FVSClimAttrs.db" as new')
    # get the table:
    progress$set(message = "Inserting new data",value = 8)    
    qur = dbSendQuery(dbGlb$dbIcon,'select * from FVS_ClimAttrs')
    oldAttrs = dbFetch(qur,n=1)
    dbClearResult(qur)
    if (nrow(oldAttrs) == 0) 
    {
cat ("simple copy from new, all rows were deleted\n")
      dbSendQuery(dbGlb$dbIcon,'drop table FVS_ClimAttrs')
      dbSendQuery(dbGlb$dbIcon,'insert into FVS_ClimAttrs select * from new.FVS_ClimAttrs')
    } else {
      qur = dbSendQuery(dbGlb$dbIcon,'select * from new.FVS_ClimAttrs')
      newAttrs = dbFetch(qur,n=1)
      dbClearResult(qur)
      if (identical(colnames(oldAttrs),colnames(newAttrs)))
      {
cat ("simple insert from new, all cols are identical\n")
        dbSendQuery(dbGlb$dbIcon,'insert into FVS_ClimAttrs select * from new.FVS_ClimAttrs')
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
          dbBegin(dbGlb$dbIcon)
          for (mis in newmiss) dbSendQuery(dbGlb$dbIcon,
            paste0('alter table new.FVS_ClimAttrs add "',mis,'" real'))
          dbCommit(dbGlb$dbIcon)
        }
cat ("length(oldmiss)=",length(oldmiss),"\n")
        if (length(oldmiss) > 0)
        {
          dbSendQuery(dbGlb$dbIcon,'alter table FVS_ClimAttrs rename to oldClimAttrs')
          dbBegin(dbGlb$dbIcon)
          for (mis in oldmiss) dbSendQuery(dbGlb$dbIcon,
            paste0('alter table oldClimAttrs add "',mis,'" real'))
          dbCommit(dbGlb$dbIcon)
          dbSendQuery(dbGlb$dbIcon,
            paste0('create table FVS_ClimAttrs as select ',selnew,' from oldClimAttrs'))
          dbSendQuery(dbGlb$dbIcon,'drop table oldClimAttrs')
        }     
        dbSendQuery(dbGlb$dbIcon,
          paste0('insert into FVS_ClimAttrs select ',selnew,' from new.FVS_ClimAttrs'))
      }
    }
    dbSendQuery(dbGlb$dbIcon,'detach database new')   
    unlink("FVSClimAttrs.db")
    progress$set(message = "Recreating FVS_ClimAttrs index",value = 9)
    dbSendQuery(dbGlb$dbIcon,'drop index if exists StdScnIndex')
    dbSendQuery(dbGlb$dbIcon,"create index StdScnIndex on FVS_ClimAttrs (Stand_ID, Scenario);")
    progress$set(message = "Done", value = 10)
    output$uploadActionMsg = renderText("FVSClimAttrs updated.")
    Sys.sleep (2)
    session$sendCustomMessage(type = "resetFileInputHandler","climateFVSUpload")
    progress$close()
  })  
  
  observe({
    if(input$inputDBPan == "View and edit existing tables") 
    {
cat ("dataEditor\n")
      source("editDataUtilities.R")
      tbs <- dbListTables(dbGlb$dbIcon)
      dbGlb$tbsCTypes <- lapply(tbs,function(x,dbIcon) 
        {
          tb <- dbGetQuery(dbIcon,paste0("PRAGMA table_info('",x,"')"))
          tbtypes = toupper(tb[,"type"])
          res = vector("logical",length(tbtypes))
          res[grep ("INT",tbtypes)] = TRUE
          res[grep ("FLOAT",tbtypes)] = TRUE
          res[grep ("REAL",tbtypes)] = TRUE
          names(res) = tb[,"name"]                                               
          res[] = !res
        }, dbGlb$dbIcon)     
      names(dbGlb$tbsCTypes) = tbs       
      idx <- grep ("StandInit",tbs)
      if (length(idx) == 0) idx=1     
      updateSelectInput(session=session, inputId="editSelDBtabs", choices=tbs, 
        selected=tbs[idx]) 
    }
  })                                                                                 

  observe({                      
cat ("editSelDBtabs, input$editSelDBtabs=",input$editSelDBtabs,
     " input$mode=",input$mode,"\n")
    if (length(input$editSelDBtabs)) 
    {         
      dbGlb$tblName <- input$editSelDBtabs
      fixEmptyTable(dbGlb)                                
      checkMinColumnDefs(dbGlb)
      dbGlb$tbl <- NULL                                           
      dbGlb$tblCols <- names(dbGlb$tbsCTypes[[dbGlb$tblName]])
      if (length(grep("Stand_ID",dbGlb$tblCols,ignore.case=TRUE))) 
      {
        dbGlb$sids = dbGetQuery(dbGlb$dbIcon,
          paste0("select distinct Stand_ID from ",dbGlb$tblName))$Stand_ID
        if (any(is.na(dbGlb$sids))) dbGlb$sids[is.na(dbGlb$sids)] = ""
        if (length(dbGlb$sids) > 0)                                           
        {
          if (dbGlb$rowSelOn) updateSelectInput(session=session, 
            inputId="rowSelector",choices  = dbGlb$sids) else 
          output$stdSel <- mkStdSel(dbGlb)
        } 
      } else {
        dbGlb$sids <- NULL                                                  
        output$stdSel <- renderUI(NULL)                                         
        dbGlb$rowSelOn <- FALSE
      }   
      updateSelectInput(session=session, inputId="editSelDBvars", 
        choices=as.list(dbGlb$tblCols),selected=dbGlb$tblCols)  
    }
  })              
  
  observe({              
    if (length(input$editSelDBvars)) 
    {
cat ("editSelDBvars, input$editSelDBvars=",input$editSelDBvars,"\n")       
      ndr = suppressWarnings(as.numeric(input$disprows))
      if (is.na(ndr) || is.nan(ndr) || ndr < 1 || ndr > 500) ndr = 20 
      dbGlb$disprows <- ndr
      switch(input$mode,                    
        "New rows"= 
        {
          dbGlb$rows <- NULL
          tbl <- as.data.frame(matrix("",ncol=length(input$editSelDBvars),
                               nrow=dbGlb$disprows))
          colnames(tbl) <- input$editSelDBvars
          output$tbl <- renderRHandsontable(rhandsontable(tbl,
            readOnly=FALSE,useTypes=TRUE,contextMenu=FALSE,width="100%"))
          output$stdSel <- output$navRows <- renderUI(NULL)
          dbGlb$rowSelOn <- dbGlb$navsOn <- FALSE
        },
        Edit = 
        {
          qry <- paste0("select _ROWID_,* from ",dbGlb$tblName)
          qry <- if (length(intersect("Stand_ID",dbGlb$tblCols)) && 
                     length(input$rowSelector))
            paste0(qry," where Stand_ID in (",
                  paste0("'",input$rowSelector,"'",collapse=","),");") else
            paste0(qry,";")                             
          dbGlb$tbl <- dbGetQuery(dbGlb$dbIcon,qry)
          rownames(dbGlb$tbl) = dbGlb$tbl$rowid
          for (col in 2:ncol(dbGlb$tbl))
            if (class(dbGlb$tbl[[col]]) != "character") 
               dbGlb$tbl[[col]] = as.character(dbGlb$tbl[[col]])
          if (nrow(dbGlb$tbl) == 0) dbGlb$rows = NULL else
          {
            dbGlb$tbl$Delete = FALSE
            dbGlb$rows <- c(1,min(nrow(dbGlb$tbl),dbGlb$disprows))
            output$tbl <- renderRHandsontable(
              rhandsontable(dbGlb$tbl[1:min(nrow(dbGlb$tbl),dbGlb$disprows),
                union(c("Delete"),input$editSelDBvars),drop=FALSE],
                 readOnly=FALSE,useTypes=TRUE,contextMenu=FALSE))
            if (!dbGlb$navsOn) 
            {
              dbGlb$navsOn <- TRUE
              output$navRows <- renderUI(list(
                actionButton("previousRows","<< previous rows"),
                actionButton("nextRows","next rows >>"),
                textOutput("rowRng",inline=TRUE)))
            }
            output$rowRng <- renderText(paste0(dbGlb$rows[1]," to ",
                  dbGlb$rows[2]," of ",nrow(dbGlb$tbl)))            
            if (!dbGlb$rowSelOn && length(dbGlb$sids))
              output$stdSel <- mkStdSel(dbGlb)
          }
        }
      )
    }
  })
  observe({
    if (length(input$nextRows) && input$nextRows > 0) 
    {
      if (is.null(dbGlb$tbl)) return()
      input$disprows
      newBot <- min(dbGlb$rows[2]+dbGlb$disprows,nrow(dbGlb$tbl))
      newTop <- max(newBot-dbGlb$disprows-1,1)
      dbGlb$rows <- c(newTop,newBot)
      output$tbl <- renderRHandsontable(rhandsontable(dbGlb$tbl[newTop:newBot,
        union(c("Delete"),isolate(input$editSelDBvars)),
          drop=FALSE],readOnly=FALSE,useTypes=TRUE,contextMenu=FALSE))
      output$rowRng <- renderText(paste0(newTop," to ",
          newBot," of ",nrow(dbGlb$tbl)))
    }
  })
  observe({
    if (length(input$previousRows) && input$previousRows > 0) 
    {
      if (is.null(dbGlb$tbl)) return()
      input$disprows
      newTop <- max(dbGlb$rows[1]-dbGlb$disprows,1)
      newBot <- min(newTop+dbGlb$disprows-1,nrow(dbGlb$tbl))
      dbGlb$rows <- c(newTop,newBot)
      output$tbl <- renderRHandsontable(rhandsontable(dbGlb$tbl[newTop:newBot,
        union(c("Delete"),isolate(input$editSelDBvars)),
          drop=FALSE],readOnly=FALSE,useTypes=TRUE,contextMenu=FALSE))
      output$rowRng <- renderText(paste0(newTop," to ",
          newBot," of ",nrow(dbGlb$tbl)))
    }
  })

  # commitChanges
  observe({
    if (input$commitChanges > 0) 
    {             
      isolate({
cat ("commitChanges, mode=",input$mode,"len tbl=",length(input$tbl),"\n")
        inputTbl = matrix(unlist(input$tbl$params$data),
                   ncol=length(input$tbl$params$columns),byrow=TRUE)
        inputTbl[inputTbl=="NA"] = NA
        colnames(inputTbl) = unlist(input$tbl$params$colHeaders)
        rownames(inputTbl) = unlist(input$tbl$params$rowHeaders)
        switch(input$mode,
          "New rows"= 
          {
            inserts <- mkInserts(inputTbl,dbGlb$tblName,
                                 dbGlb$tbsCTypes[[dbGlb$tblName]])
lapply(inserts,function (x) cat("ins=",x,"\n"))                             
            if (length(inserts)) 
            {
              dbBegin(dbGlb$dbIcon)
              err = FALSE
              for (ins in inserts) 
              {
                res = try(dbSendQuery(dbGlb$dbIcon,ins))
                if (class(res) == "try-error") {err=TRUE; break}
              }
              if (err) 
              {
                dbRollback(dbGlb$dbIcon) 
                output$actionMsg = renderText(paste0("Error processing: ",ins))
                return()
              } else {
                dbCommit(dbGlb$dbIcon)
                output$actionMsg = renderText(paste0(length(inserts)," insert(s) processed."))
              }
              tbl <- as.data.frame(matrix("",
                     ncol=length(input$editSelDBvars),nrow=dbGlb$disprows))
              colnames(tbl) <- input$editSelDBvars
              output$tbl <- renderRHandsontable(rhandsontable(tbl,
                readOnly=FALSE,useTypes=TRUE,contextMenu=FALSE))
            }
          },
          Edit = 
          {
            err=FALSE
            nrows = nrow(inputTbl)
            nprocess = 0
            dbBegin(dbGlb$dbIcon)
            if (nrows) for (rn in 1:nrows)
            {
              row = inputTbl[rn,]
              id = rownames(inputTbl)[rn]
              if (row["Delete"] == "TRUE") 
              {
                qry = paste0("delete from ",dbGlb$tblName," where _ROWID_ = ",
                             id)
cat ("edit del, qry=",qry,"\n")                     
                res = try(dbSendQuery(dbGlb$dbIcon,qry))
                if (class(res) == "try-error") {err=TRUE; break}
                nprocess = nprocess+1
                if (!is.null(dbGlb$sids)) dbGlb$sids = NULL
              } else {              
                row = inputTbl[rn,]              
                if (length(row) < 2) next
                row = row[-1]
                row[is.na(row)] = ""
                row[row == "NA"] = ""
                org = subset(dbGlb$tbl,rowid == id)
                org = as.character(org[,names(row),drop=TRUE])
                org[is.na(org)] = ""
                org[org=="character(0)"] = ""
                org[org == "NA"] = ""
                names(org)=names(row)
                neq = vector("logical",length(row))
                for (i in 1:length(row)) neq[i]=!identical(row[i],org[i])              
                if (sum(neq) == 0) next
                update = row[neq]
                toquote = dbGlb$tbsCTypes[[dbGlb$tblName]][names(update)]
                if (!is.null(dbGlb$sids) && 
                    !is.na(toquote["Stand_ID"])) dbGlb$sids = NULL
                if (any(toquote))
                {
                  for (toq in names(toquote[toquote]))
                  {
                    update[toq] = if (update[toq]=="") "NULL" else
                      paste0("'",gsub("'","''",update[toq]),"'")
                  }
                }
                update[update==""] = "NULL"
                qry = paste0("update ",dbGlb$tblName," set ",
                  paste(paste0(names(update)," = ",update),collapse=", "),
                    " where _ROWID_ = ",id)
cat ("edit upd, qry=",qry,"\n")
                res = try(dbSendQuery(dbGlb$dbIcon,qry))              
                if (class(res) == "try-error") {err=TRUE; break}
                nprocess = nprocess+1
              }
            }
            if (err) 
            {
              dbRollback(dbGlb$dbIcon) 
              output$actionMsg = renderText(paste0("Error processing: ",qry))
              return()
            } else {
              dbCommit(dbGlb$dbIcon)
              output$actionMsg = renderText(paste0(nprocess," change(s) processed."))
            }
            fixEmptyTable(dbGlb)
cat ("after commit, is.null(dbGlb$sids)=",is.null(dbGlb$sids),
     " dbGlb$tblName=",dbGlb$tblName,
     " Stand_ID yes=",length(intersect("Stand_ID",dbGlb$tblCols)),"\n")
            if (is.null(dbGlb$sids) && 
                length(intersect("Stand_ID",dbGlb$tblCols)))
            {
              res = dbSendQuery(dbGlb$dbIcon,paste0("select distinct Stand_ID from ",
                                dbGlb$tblName))
              dbGlb$sids = dbFetch(res,n=-1)$Stand_ID
              dbClearResult(dbGlb$dbIcon)
              if (any(is.na(dbGlb$sids))) dbGlb$sids[is.na(dbGlb$sids)] = ""
              if (dbGlb$rowSelOn && length(dbGlb$sids)) 
                updateSelectInput(session=session, inputId="rowSelector",
                  choices  = dbGlb$sids) else 
                output$stdSel <- mkStdSel(dbGlb)
            }  

            qry <- paste0("select _ROWID_,* from ",dbGlb$tblName)
            qry <- if (length(intersect("Stand_ID",dbGlb$tblCols)) && 
                       length(input$rowSelector))
              paste0(qry," where Stand_ID in (",
                    paste0("'",input$rowSelector,"'",collapse=","),");") else
              paste0(qry,";") 
            res <- dbSendQuery(dbGlb$dbIcon,qry)
            dbGlb$tbl <- dbFetch(res,n=-1)
            rownames(dbGlb$tbl) = dbGlb$tbl$rowid
            dbClearResult(dbGlb$dbIcon)
            for (col in 2:ncol(dbGlb$tbl))
              if (class(dbGlb$tbl[[col]]) != "character") 
                 dbGlb$tbl[[col]] = as.character(dbGlb$tbl[[col]])
            if (nrow(dbGlb$tbl) == 0) dbGlb$rows = NULL else 
            {
              dbGlb$tbl$Delete = FALSE
              dbGlb$rows <- c(dbGlb$rows[1],
                              min(nrow(dbGlb$tbl),dbGlb$rows[2]))
              output$tbl <- renderRHandsontable(rhandsontable(
                dbGlb$tbl[dbGlb$rows[1]:dbGlb$rows[2],
                union(c("Delete"),input$editSelDBvars),drop=FALSE],
                readOnly=FALSE,useTypes=TRUE,contextMenu=FALSE))
            }
          }
        )
      })
    }
  })



  
  observe(if (input$clearTable > 0) 
  {
cat ("clearTable, tbl=",dbGlb$tblName,"\n")
    dbSendQuery(dbGlb$dbIcon,paste0("delete from ",dbGlb$tblName))
    dbGlb$navsOn <- FALSE            
    dbGlb$rowSelOn <- FALSE
    dbGlb$sids <- NULL
    output$stdSel <- renderUI(NULL)
    tmp = as.data.frame(lapply(dbGlb$tbsCTypes[[dbGlb$tblName]],
      function (x) vector(if (x) "character" else "numeric",1)),
      stringsAsFactors=FALSE)
    tmp[1,] = NA
    dbWriteTable(dbGlb$dbIcon,dbGlb$tblName,tmp,overwrite=TRUE)
    qry <- paste0("select _ROWID_,* from ",dbGlb$tblName)
    dbGlb$tbl <- dbGetQuery(dbGlb$dbIcon,qry)
    rownames(dbGlb$tbl) = dbGlb$tbl$rowid
    dbGlb$tbl$Delete = FALSE
    output$tbl <- renderRHandsontable(rhandsontable(
       dbGlb$tbl[,union(c("Delete"),input$selectdbvars),drop=FALSE],
                readOnly=FALSE,useTypes=TRUE,contextMenu=FALSE))
    output$rowRng <- renderText("1 to 1 of 1")
    isolate(if (input$mode=="New rows") updateRadioButtons(session=session, 
      inputId="mode",selected="Edit"))
  })
  

  #runScript selection
  observe({
    if (length(input$runScript)) customRunOps()
  })

  customRunOps <- function ()
  {
    isolate({
      if (length(input$runScript) == 0)
      {
cat ("in customRunOps runScript is empty\n")
        return()
      }  
cat ("in customRunOps runScript: ",input$runScript,"\n")
      globals$fvsRun$runScript = input$runScript
      output$uiCustomRunOps = renderUI(NULL)    
      if (input$runScript != "fvsRun")
      {
        rtn = try(source(paste0("customRun_",globals$fvsRun$runScript,".R")))
        if (class(rtn) == "try-error") return()
        uiF = try(eval(parse(text=paste0(sub("fvsRun","ui",globals$fvsRun$runScript)))))
        if (class(uiF) != "function") return()
        output$uiCustomRunOps = renderUI(uiF(globals$fvsRun))
      } else {
        globals$fvsRun$uiCustomRunOps = list()
      }
if (length(globals$fvsRun$uiCustomRunOps)) lapply(names(globals$fvsRun$uiCustomRunOps), function (x,y)
cat ("globals$fvsRun$uiCustomRunOps$",x,"=",y[[x]],"\n",sep=""),globals$fvsRun$uiCustomRunOps) else
cat ("globals$fvsRun$uiCustomRunOps is empty\n")
    })
  }

  
  saveRun <- function() 
  {
    isolate({
cat ("in saveRun\n") 
      if (input$title == "")
      {
        if (globals$fvsRun$title == "") globals$fvsRun$title = paste("Run",length(globals$FVS_Runs))
        updateTextInput(session=session, inputId="title", value=globals$fvsRun$title)
      } 
      globals$fvsRun$title = input$title
      globals$fvsRun$defMgmtID = input$defMgmtID
      globals$fvsRun$runScript = if (length(input$runScript)) input$runScript else "fvsRun"
      if (globals$fvsRun$runScript == "fvsRun") globals$fvsRun$uiCustomRunOps = list() else
      {
        for (item in names(globals$fvsRun$uiCustomRunOps))
          globals$fvsRun$uiCustomRunOps[[item]] = input[[item]]
      }
      globals$FVS_Runs[[globals$fvsRun$uuid]] = globals$fvsRun$title
      attr(globals$FVS_Runs[[globals$fvsRun$uuid]],"time") = as.integer(Sys.time())
      saveFvsRun = globals$fvsRun
      save(file=paste0(globals$fvsRun$uuid,".RData"),saveFvsRun)
      globals$FVS_Runs = reorderFVSRuns(globals$FVS_Runs)
cat ("leaving saveRun\n") 
    }) 
  }
   
})

