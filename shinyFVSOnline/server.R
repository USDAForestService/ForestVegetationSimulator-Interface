library(shiny)
library(rhandsontable)
library(ggplot2)
library(parallel)
library(RSQLite)

# set shiny.trace=T for reactive tracing (lots of output)
options(shiny.maxRequestSize=1000*1024^2,shiny.trace = FALSE) 

shinyServer(function(input, output, session) {

  #sink("FVSOnline.log")

  source("fvsRunUtilities.R",local=TRUE)
  source("fvsOutUtilities.R",local=TRUE)
  source("componentWins.R",local=TRUE)
  source("mkInputElements.R",local=TRUE)

  if (file.exists("localSettings.R")) 
           source("localSettings.R",local=TRUE) else if
     (file.exists("../../FVSOnline/settings.R")) 
           source("../../FVSOnline/settings.R",local=TRUE)
  
  load("prms.RData") 
  globals <- mkglobals(saveOnExit=TRUE,autoPanNav=TRUE,oldInAdd=0)
  resetGlobals(globals,NULL,prms)

  ##load existing runs
  fvsRun <- mkfvsRun()
  trys = 1
  repeat
  {
    if (file.exists("FVS_Runs.RData"))
    {
cat ("FVS_Runs.RData exists, trys =",trys,"\n")      
      load("FVS_Runs.RData")
      globals$FVS_Runs = FVS_Runs
      rm (FVS_Runs)
      trycall <- try(loadFromList(fvsRun,globals$FVS_Runs[[1]]))
      if (class(trycall)=="try-error")
      {
cat ("FVS_Runs try error...deleting one\n")      
        globals$FVS_Runs[[1]] = NULL
        if (length(globals$FVS_Runs)) 
        {
          FVS_Runs = globals$FVS_Runs
          save (FVS_Runs,file="FVS_Runs.RData")
        } else file.remove("FVS_Runs.RData")
        resetfvsRun(fvsRun,globals$FVS_Runs)
        trys = trys+1
        if (trys > 10) file.remove("FVS_Runs.RData")
        next
      } else break
    } else 
    {
cat ("FVS_Runs does not exist, resetting\n")      
      globals$FVS_Runs = list()
      resetfvsRun(fvsRun,globals$FVS_Runs)
      globals$FVS_Runs[[fvsRun$uuid]] = asList(fvsRun)
      break
    }
  } 
  mkSimCnts(fvsRun,fvsRun$selsim)
  resetGlobals(globals,fvsRun,prms)
  selChoices = names(globals$FVS_Runs) 
  names(selChoices) = unlist(lapply(globals$FVS_Runs,function (x) x$title))
cat ("Setting initial selections, length(selChoices)=",length(selChoices),"\n")
  updateSelectInput(session=session, inputId="runSel", 
      choices=selChoices,selected=selChoices[[1]])
  updateTextInput(session=session, inputId="title",
      value=if (fvsRun$title == "") fvsRun$uuid else fvsRun$title)
  updateTextInput(session=session, inputId="defMgmtID",value=fvsRun$defMgmtID)
  updateSelectInput(session=session, inputId="simCont", choices=fvsRun$simcnts, 
      selected=fvsRun$selsim)      
  updateTabsetPanel(session=session, inputId="rightPan", 
    selected=if (length(fvsRun$simcnts)) "Components" else "Stands")
  extns <-  prms$extensions
  extnslist <-  as.list(unlist(lapply(extns,function (x,extns) 
                    getPstring(extns,x), extns)))
  extn <- extnslist[globals$activeExtens]
  if (length(fvsRun$stands) > 0) updateSelectInput(session=session,
      inputId="addCategories","Extension",choices=paste0(names(extn),": ",extn),
       selected=extn[["base"]]) 
  updateTextInput(session=session, inputId="startyr",  value=fvsRun$startyr)
  updateTextInput(session=session, inputId="endyr",    value=fvsRun$endyr)
  updateTextInput(session=session, inputId="cyclelen", value=fvsRun$cyclelen)
  updateTextInput(session=session, inputId="cycleat",  value=fvsRun$cycleat)

  if (exists("fvsOutData")) rm (fvsOutData) 
  fvsOutData <- mkfvsOutData(plotSpecs=list(res=144,height=4,width=6))
  dbDrv <- dbDriver("SQLite")
  dbcon <- dbConnect(dbDrv,"FVSOut.db")    
  dbSendQuery(dbcon,'attach ":memory:" as m')
  if (!file.exists("FVS_Data.db"))
  {
    if (file.exists("FVS_Data.db.backup")) 
        file.rename("FVS_Data.db.backup","FVS_Data.db") else
        file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
  }
  dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
  dbSendQuery(dbIcon,'attach ":memory:" as m')
  loadVarData(globals,prms,dbIcon)
  
  session$onSessionEnded(function ()
  {                                                
cat ("onSessionEnded, globals$saveOnExit=",globals$saveOnExit,"\n")
    if (exists("dbcon"))  try(dbDisconnect(dbcon))
    if (exists("dbIcon")) try(dbDisconnect(dbIcon))
    if (!globals$saveOnExit) return()
    saveRun()
    FVS_Runs = globals$FVS_Runs
    save (FVS_Runs,file="FVS_Runs.RData")
    if (file.exists("projectId.txt"))
    {
      prjid = scan("projectId.txt",what="",sep="\n",quiet=TRUE)
      write(file="projectId.txt",prjid)
    }                                          
  })

  ## leftPan automatic panel navigation 
  #when autoPanNav==FALSE, this logic is active. When TRUE, this logic
  #should be skipped. This switch is used as a semaphore, it is set true
  #when another part of the code causes a navigation process to start and 
  #thereby keeps an unending loop of changed navigation from occuring.
  observe({
    input$leftPan
isolate({
  cat ("autoPanNav=",globals$autoPanNav,
    " HIT input$leftPan=",input$leftPan," input$rightPan=",input$rightPan,"\n")
})
    if (globals$autoPanNav)
    {
      globals$autoPanNav = FALSE
      return()
    }
    isolate({
      switch(input$leftPan,
        "Runs" = 
        {
          if (!(input$rightPan %in% c("Components","Stands","Time","Run")))
          {
            globals$autoPanNav = TRUE
            updateTabsetPanel(session=session, inputId="rightPan", 
              selected=if (length(fvsRun$simcnts)) "Components" else "Stands")
          }
        },
        "Load Output" = 
        {
          if (! input$rightPan %in% c("Tables","Graphs","Reports"))
          {
            globals$autoPanNav = TRUE
            updateTabsetPanel(session=session, inputId="rightPan", 
              selected="Tables")
          }
        },
        "Explore Output" = 
        {
          if (! input$rightPan %in% c("Tables","Graphs","Reports"))
          {
            globals$autoPanNav = TRUE
            updateTabsetPanel(session=session, inputId="rightPan", 
              selected="Tables")
          }
          if (length(fvsOutData$dbLoadData) == 0)
          {
            globals$autoPanNav = TRUE
            updateTabsetPanel(session=session, inputId="leftPan", 
              selected="Load Output")
          }
        }
      )
    })
  })
  ## rightPan automatic panel navigation 
  observe({
    input$rightPan
    isolate({
      cat ("autoPanNav=",globals$autoPanNav,
        " input$leftPan=",input$leftPan," HIT input$rightPan=",input$rightPan,"\n")
    })
    if (globals$autoPanNav)
    {
      globals$autoPanNav = FALSE
      return()
    }
    isolate({
      if (input$rightPan %in% c("Components","Stands","Time","Run"))
      {
        if (input$leftPan != "Runs")
        {
          globals$autoPanNav = TRUE
          updateTabsetPanel(session=session, inputId="leftPan", 
              selected="Runs")
        }
      }
      if (input$rightPan %in% c("Tables","Graphs","Reports"))
      {
        if (input$leftPan == "Runs")
        {
          globals$autoPanNav = TRUE
          updateTabsetPanel(session=session, inputId="leftPan", 
              selected=if (length(fvsOutData$dbLoadData))
                       "Explore Output" else "Load Output")
        }
      }
      if (input$rightPan == "Run")
      {
        output$uiRunPlot <- output$uiErrorScan <- renderUI(NULL)
        updateSelectInput(session=session, inputId="bkgRuns", 
                          choices=getBkgRunList(),selected=0)
        globals$currentQuickPlot = character(0)
      }
    })
  })

  ## Load Output
  observe({
    if (input$leftPan == "Load Output")
    {
cat ("Load Output\n")
      fvsOutData$runs = character(0)
      fvsOutData$dbVars = character(0)
      fvsOutData$browseVars = character(0)
      fvsOutData$dbSelVars = character(0)
      fvsOutData$browseSelVars = character(0)

      tbs <- dbListTables(dbcon)
      if (length(tbs) > 0 && !is.na(match("FVS_Cases",tbs)))
      {
        fvsOutData$dbCases = dbReadTable(dbcon,"FVS_Cases")
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
    if (input$leftPan != "Load Output") return()
cat ("runs, run selection (load) input$runs=",input$runs,"\n")
    if (!is.null(input$runs)) # will be a list of run keywordfile names (uuid's)
    {
      tbs <- dbListTables(dbcon)
cat ("runs, tbs=",tbs,"\n")
      withProgress(session, {  
        i = 1
        setProgress(message = "Output query", 
                    detail  = "Selecting tables", value = i); i = i+1
        # create a m.Cases table that is a list of CaseIDs and MgmtIDs 
        # associated with the selected runs. These two items are used to 
        # filter records selected from selected tables.
        dbSendQuery(dbcon,"drop table if exists m.Cases")
        inSet=paste0("('",paste(input$runs,collapse="','"),"')")
        dbSendQuery(dbcon,paste0("create table m.Cases as select CaseID ",
                     "from FVS_Cases where FVS_Cases.KeywordFile in ",inSet))        
        dbBegin(dbcon)      
        for (tb in tbs) 
        {
cat ("tb=",tb,"\n")
          cnt = 0
          if (tb == "FVS_Cases") next
          else if (tb == "Composite")
            dbSendQuery(dbcon,"drop table Composite")
          else if (tb == "Composite_East")
            dbSendQuery(dbcon,"drop table Composite_East")
          else if (tb == "StdStk")
            dbSendQuery(dbcon,"drop table StdStk")
          else cnt = dbGetQuery(dbcon,paste0("select count(*) from ",
                   "(select distinct CaseID from ",tb," where CaseID in ",
                   "(select CaseID from m.Cases))"))
          if (cnt == 0) tbs = setdiff(tbs,tb)
        }
        source("sqlQueries.R")
        isolate(dbhclassexp <- mkdbhCase(input$sdskwdbh,input$sdskldbh))
        input$stdskreb # force this section to be reactive to this input
        if ("FVS_Summary" %in% tbs)
        {
          setProgress(message = "Output query", 
            detail  = "Building composites", value = i); i = i+1
          exqury(dbcon,Create_Composite)
          tbs = c(tbs,"Composite")
cat ("tbs1=",tbs,"\n")
        }
        if ("FVS_Summary_East" %in% tbs)
        {
          setProgress(message = "Output query", 
            detail  = "Building composites", value = i); i = i+1
          exqury(dbcon,Create_Composite_East)
          tbs = c(tbs,"Composite_East")
cat ("tbs2=",tbs,"\n")
        }

        if ("FVS_TreeList" %in% tbs)  
        {
          setProgress(message = "Output query", 
            detail  = "Building StdStk from Treelists", value = i); i = i+1
          exqury(dbcon,Create_m.StdStk,dbhclassexp)
          if ("FVS_CutList" %in% tbs)
          {
            setProgress(message = "Output query", 
              detail  = "Building StdStk from Cutlists", value = i); i = i+1
            exqury(dbcon,Create_m.HrvStdStk,dbhclassexp)
            setProgress(message = "Output query", 
              detail  = "Joining tables", value = i); i = i+1
            exqury(dbcon,Create_StdStk,dbhclassexp)
          } else {
             setProgress(message = "Output query", 
              detail  = "Joining tables", value = i); i = i+2
            exqury(dbcon,Create_StdStkNoHrv,dbhclassexp)
          }
          tbs = c(tbs,"StdStk")
        }
        
        setProgress(message = "Output query", 
            detail  = "Committing changes", value = i); i = i+1
        dbCommit(dbcon)
        dbd = lapply(tbs,function(tb,con) dbListFields(con,tb), dbcon)
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
      vars = lapply(tables,function (tb,dbd) paste0(tb,":",dbd[[tb]]), 
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

  ## Explore Output
  observe({ 
    if (input$leftPan == "Explore Output")
    { 
cat ("Explore Output\n")      
      if (length(fvsOutData$dbSelVars) == 0) return()
      tbs = unique(unlist(lapply(strsplit(fvsOutData$dbSelVars,":"),
            function (x) x[1])))
      if (length(tbs) == 0) return()
      cols = unique(unlist(lapply(strsplit(fvsOutData$dbSelVars,":"),
            function (x) x[2])))
      if (length(cols) == 0) return()
      dat = list()
      for (tb in tbs) 
      {
        if (tb == "Composite" || tb == "Composite_East") 
        {
          dtab = dbReadTable(dbcon,tb)
          dtab = by(dtab,as.factor(dtab$MgmtID),FUN=function (x) 
                 setupSummary(x,composite=TRUE))
          dtab = do.call("rbind",dtab)
          dtab$Year=as.factor(dtab$Year) 
          dtab$MgmtID=as.factor(dtab$MgmtID) 
          dat = list(Composite = dtab)
        } else {
          dtab = dbGetQuery(dbcon,paste0("select * from ",tb,
                 " where CaseID in (select CaseID from m.Cases)"))
          if (tb == "FVS_Summary" || tb == "FVS_Summary_East") 
          { 
            dtab = by(dtab,as.factor(dtab$CaseID),FUN=function (x) 
                      setupSummary(x))
            dtab = do.call("rbind",dtab)
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
        updateSelectInput(session, "stdtitle",choices = list("None loaded"), 
          selected = NULL)
        updateSelectInput(session, "stdid",   choices = list("None loaded"), 
          selected = NULL)
        updateSelectInput(session, "mgmid",   choices = list("None loaded"),
          selected = NULL)
        updateSelectInput(session, "year",    choices = list("None loaded"),
          selected = NULL)
        updateSelectInput(session, "species", choices = list("None loaded"),
          selected = NULL)
        updateSelectInput(session, "dbhclass",choices = list("None loaded"),
          selected = NULL)
        return()
      }
      # this merges the data on all columns      
      if (length(dat) > 0)
      {
        withProgress(session, 
        {  
          i = 1
          for (tb in names(dat))
          {
            setProgress(message = "Merging selected tables", 
                        detail  = tb, value = i); i = i+1
            mdat = merge(mdat,dat[[tb]],all=TRUE)
          }
          fvsOutData$dbData = mdat
          setProgress(value = NULL)          
        }, min=1, max=length(dat))
      } else fvsOutData$dbData = mdat #happens when only FVS_Cases is selected  
      
      mdat = fvsOutData$dbData
      vars = colnames(mdat)
      sby = intersect(c("MgmtID","StandID","Stand_CN","Year","PtIndex",
                "TreeIndex","Species","DBHClass","RunDateTime"),vars) 
      cmd = paste0("order(",paste(paste0("mdat$",sby),collapse=","),
             if("srtOrd" %in% vars) ",mdat$srtOrd)" else ")")
      sby = eval(parse(text=cmd))
      vars = intersect(c("MgmtID","Stand_CN","StandID","Year",
                         "Species","DBHClass"),colnames(mdat))
      vars = c(vars,setdiff(colnames(mdat),vars))
      endvars = intersect(c("SamplingWt","Variant","RunTitle",
                       "Groups","RunDateTime","KeywordFile","CaseID"),vars)
      vars = union(setdiff(vars,endvars),endvars)
      mdat = mdat[sby,vars,drop=FALSE]
      mdat$srtOrd = NULL
      vars = colnames(mdat)     
      if (length(vars) == 0) return()
      if (is.null(mdat$RunTitle)) 
        updateSelectInput(session, "stdtitle", choices  = list("None loaded"), 
          selected = NULL) else 
        updateSelectInput(session, "stdtitle", 
          choices=as.list(levels(mdat$RunTitle)), selected=levels(mdat$RunTitle))      
      if (is.null(mdat$StandID)) 
        updateSelectInput(session, "stdid", choices  = list("None loaded"), 
          selected = NULL) else 
        updateSelectInput(session, "stdid", 
          choices=as.list(levels(mdat$StandID)), selected=levels(mdat$StandID))
      if (is.null(mdat$MgmtID)) updateSelectInput(session, "mgmid", 
          choices  = list("None loaded"), selected = NULL) else 
        updateSelectInput(session, "mgmid",choices=as.list(levels(mdat$MgmtID)), 
          selected=levels(mdat$MgmtID))
      if ("FVS_TreeList" %in% names(dat))
        updateSelectInput(session, "plotType",selected="scatter") else 
        if ("StdStk" %in% names(dat)) 
          updateSelectInput(session, "plotType",selected="bar") else
            updateSelectInput(session, "plotType",selected="line")
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
      selVars = unlist(lapply(c("StandID","MgmtID","Year","^DBH","^DG$",
        "QMD","TopHt","^BA$","TPA","Species","^Ht$","^HtG$","CuFt$","Total"),
        function (x,vs) 
          {
            hits = unlist(grep(x,vs,ignore.case = TRUE))
            hits = hits[hits>0]
            vs[hits]
          },vars))
      keep = unlist(lapply(selVars,function(x,mdat) !all(is.na(mdat[,x])),mdat))
      selVars = selVars[keep]
      updateCheckboxGroupInput(session, "browsevars", choices=as.list(vars), 
                               selected=selVars,inline=TRUE)
      fvsOutData$dbData        <- mdat
      fvsOutData$browseVars    <- vars
      fvsOutData$browseSelVars <- selVars
    } 
  })
  
  ## renderTable
  output$table <- renderRHandsontable(
  {
cat("renderTable\n")
    rhandsontable(data.frame())

    if (input$leftPan == "Load Output") return(rhandsontable(data.frame())) 
    if (length(input$selectdbvars) == 0) return(rhandsontable(data.frame())) 
cat("renderTable continued\n")
    if (length(input$browsevars) == 0)
    {
      fvsOutData$browseSelVars <- character(0)
      return (NULL)
    }
    fvsOutData$browseSelVars <- input$browsevars
    {
      dat = if (length(input$browsevars) > 0) 
      {
        dat = fvsOutData$dbData[filterRows(fvsOutData$dbData, input$stdtitle, 
          input$stdid, input$mgmid, input$year, input$species, input$dbhclass)
          ,,drop=FALSE]
cat ("nrow fvsOutData$dbData=",nrow(fvsOutData$dbData),
     " nrow dat=",nrow(dat),"\n")
        sel=match(input$browsevars,colnames(dat))
        dat = dat[,sel,drop=FALSE]
        if (!is.null(input$pivVar)  && input$pivVar  != "None" &&
            !is.null(input$dispVar) && input$dispVar != "None") dat = 
              pivot(dat,input$pivVar,input$dispVar)
        fvsOutData$render = dat
        if (nrow(dat) > 3000) dat[1:3000,,drop=FALSE] else dat
      } else NULL
      if (is.null(dat)) rhandsontable(data.frame()) else
        rhandsontable(dat,readOnly=TRUE,useTypes=FALSE,contextMenu=FALSE)
    }
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
      
      spiv  = if (input$pivVar  %in% cats) input$pivVar  else "None"
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
    if (input$leftPan == "Load Output"  || 
        length(input$selectdbvars) == 0 || (length(input$xaxis) == 0 && 
        length(input$yaxis) == 0)) return(nullPlot())

    vf = if (input$vfacet == "None") NULL else input$vfacet
    hf = if (input$hfacet == "None") NULL else input$hfacet
    pb = if (input$pltby  == "None") NULL else input$pltby

    dat = droplevels(fvsOutData$dbData[filterRows(fvsOutData$dbData, input$stdtitle, 
          input$stdid, input$mgmid, input$year, input$species, input$dbhclass),])
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
        updateSelectInput(session=session, inputId="xaxis", selected=NULL)
      else if (input$pltby == input$yaxis)
        updateSelectInput(session=session, inputId="yaxis", selected=NULL)
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
    p = ggplot(data = nd) + fg + labs(x=input$xlabel, y=input$ylabel, 
          title=input$ptitle)  + 
          theme(text = element_text(size=9),
            panel.background = element_rect(fill="gray95"),
            axis.text = element_text(color="black"))
    colors = if (input$colBW == "B&W") rep(rgb(0,0,0,seq(.5,.9,.05)),5) else
      ggplotColours(n=nlevels(nd$Legend)+1)
    if (!is.null(colors)) p = p + scale_colour_manual(values=colors)
    if (nlevels(nd$Legend)>6) p = p +
      scale_shape_manual(values=1:nlevels(nd$Legend))
    alpha = approxfun(c(50,100,1000),c(1,.7,.4),rule=2)(nrow(nd))    
    size  = approxfun(c(50,100,1000),c(1,.7,.5),rule=2)(nrow(nd))    
    plt = switch(input$plotType,
      line    = if (input$colBW == "B&W") 
        geom_line    (aes(x=X,y=Y,color=Legend,linetype=Legend)) else
        geom_line    (aes(x=X,y=Y,color=Legend),alpha=.8),
      scatter = if (input$colBW == "B&W") 
        geom_point   (aes(x=X,y=Y,shape=Legend,color=Legend),size=size) else
        geom_point   (aes(x=X,y=Y,shape=Legend,color=Legend),
                          alpha=alpha,size=size),
      bar     = if (input$colBW == "B&W") 
        geom_bar     (aes(x=X,y=Y,color=Legend,fill=Legend),
           position="dodge",stat="identity") else
        geom_bar     (aes(x=X,y=Y,color=Legend,fill=Legend),alpha=.8,
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
    if (input$rightPan == "Stands") 
    {
      if (length(fvsRun$FVSpgm) == 0) 
      {
        selVarListUse <- intersect(names(globals$selVarList),
                                   globals$activeVariants)
        selVarListUse <- globals$selVarList[selVarListUse]                                   
        vlst <- as.list (names(selVarListUse))
        names(vlst) = selVarListUse
      } else {
        if (is.null(globals$activeFVS[[fvsRun$FVSpgm]])) vlst <- list() else
        {
          vlst <- as.list(globals$activeFVS[fvsRun$FVSpgm][[1]][[1]][1])
          names(vlst) <- globals$selVarList[[vlst[[1]]]]
        }
      }
      updateSelectInput(session=session, inputId="inVars", choices=vlst,
            selected=if (length(vlst)) vlst[[1]] else NULL)
    } 
  })
  
  ## inVars has changed
  observe({    
    if (input$rightPan != "Stands") return()
    if (is.null(input$inVars)) return()
    dbQ = try(dbSendQuery(dbIcon,paste0('select Stand_ID,Groups from ',
     'FVS_StandInit where lower(variant) like "%',input$inVars,'%"')))
    if (class(dbQ) == "try-error") return()
    grps = dbFetch(dbQ,n=-1)
    if (nrow(grps) == 0) return()
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
    dbSendQuery(dbIcon,'drop table if exists m.Grps') 
    dbWriteTable(dbIcon,"m.Grps",dd)
    dbQ = dbSendQuery(dbIcon,'select distinct Grp from m.Grps order by Grp')
    selGrp <- dbFetch(dbQ,n=-1)[,1]
    updateSelectInput(session=session, inputId="inGrps", 
            choices=as.list(selGrp))
    updateSelectInput(session=session, inputId="inStds", 
         choices=list())
    output$stdSelMsg <- renderUI(NULL)
  })

  ## inGrps has changed
  observe({
    if (input$rightPan != "Stands") return()
    
    if (is.null(input$inGrps))
    {
      output$stdSelMsg <- renderUI(NULL)
      updateSelectInput(session=session, inputId="inStds", 
         choices=list())
      return()
    }
    dbSendQuery(dbIcon,'drop table if exists m.SGrps') 
    dbWriteTable(dbIcon,"m.SGrps",data.frame(SelGrps = input$inGrps))
    dbQ = try(dbSendQuery(dbIcon,paste0('select distinct Stand_ID from m.Grps ',
         'where Grp in (select SelGrps from m.SGrps)')))
    if (class(dbQ) == "try-error") return()
    stds = dbFetch(dbQ,n=-1)[,1]
    msg = paste0(length(stds)," Stands in ",length(input$inGrps)," Group(s)<br>")
    output$stdSelMsg <- renderUI(HTML(msg))
    if (length(stds) > 120)  stds = c(stds[1:100],
      paste0("<< Display 101 to ",min(200,length(stds))," of ",length(stds)," >>"))
    updateSelectInput(session=session, inputId="inStds", 
         choices=as.list(stds))   
  })
  ## inStds has changed
  observe({
    if (length(input$inStds) != 1) return()
    prts = unlist(strsplit(input$inStds[1]," "))
    if (prts[1] != "<<") return()
    dbQ = try(dbSendQuery(dbIcon,paste0('select distinct Stand_ID from m.Grps ',
         'where Grp in (select SelGrps from m.SGrps)')))
    if (class(dbQ) == "try-error") return()
    stds = dbFetch(dbQ,n=-1)[,1]
    if (length(stds) < 120) return() 
    nprts = as.numeric(prts[c(3,5,7)])
cat ("nprts=",nprts,"\n")
    up = nprts[c(1,2)] - 100
    if (up[2]-up[1] < 100) up[2] = min(up[1]+100,length(stds))
    upM = if (up[1] > 0) paste0("<< Display ",up[1]," to ",
      min(up[2],length(stds))," of ",length(stds)," >>") else NULL
    dn = nprts[c(1,2)] + 100
    if (dn[2]-dn[1] < 100) dn[2] = min(dn[1]+100,length(stds))
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
      resetfvsRun(fvsRun,globals$FVS_Runs)
      fvsRun$title <- paste0("Run ",length(globals$FVS_Runs)+1)
      resetGlobals(globals,NULL,prms)
      loadVarData(globals,prms,dbIcon)
      updateTextInput(session=session, inputId="title", label="", 
                      value=fvsRun$title) 
      updateTextInput(session=session, inputId="defMgmtID",
                     value=fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="simCont", 
          choices=list(), selected=NULL)
      updateSelectInput(session=session, inputId="addCategories", 
          choices=list(), selected=NULL)
      updateSelectInput(session=session, inputId="addComponents", 
          choices=list(), selected=NULL)
      updateTextInput(session=session, inputId="startyr", 
                      value=fvsRun$startyr)
      updateTextInput(session=session, inputId="endyr", 
                      value=fvsRun$endyr)
      updateTextInput(session=session, inputId="cyclelen", 
                      value=fvsRun$cyclelen)
      updateTextInput(session=session, inputId="cycleat", 
                      value=fvsRun$cycleat)
      output$runProgress <- renderUI(NULL)
      updateSelectInput(session=session, inputId="cmdSet", 
                        selected="Management")
      updateSelectInput(session=session, inputId="runScript", 
                        selected="fvsRun")
      isolate ({
        if (!is.null(input$inVars)) 
        {
          selVarListUse <- globals$selVarList[globals$activeVariants]
          vlst <- as.list (names(selVarListUse))
          names(vlst) = selVarListUse
          updateSelectInput(session=session, inputId="inVars", NULL, 
                            vlst, vlst[[1]])
          updateSelectInput(session=session, inputId="inGrps", NULL, NULL)
          updateSelectInput(session=session, inputId="inStds", NULL, NULL)
          if (input$rightPan != "Stands")
          {
            globals$autoPanNav = TRUE
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
      globals$FVS_Runs[[fvsRun$uuid]] = asList(fvsRun)
      fvsRun$title <- paste0("Run ",length(globals$FVS_Runs)+1)
      fvsRun$uuid  <- uuidgen()
      fvsRun$defMgmtID = sprintf("A%3.3d",length(globals$FVS_Runs)+1)
      globals$FVS_Runs[[fvsRun$uuid]] = asList(fvsRun)
      FVS_Runs = globals$FVS_Runs
      save (FVS_Runs,file="FVS_Runs.RData")
      resetGlobals(globals,NULL,prms)
      updateTextInput(session=session, inputId="title", label="", 
                      value=fvsRun$title) 
      updateTextInput(session=session, inputId="defMgmtID",
                      value=fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="cmdSet", 
                        selected="Management")
    }
  })    


  ## Reload or Run Selection   
  observe({
    if (input$reload > 0 || !is.null(input$runSel))
    {
cat ("reload or run selection, runSel=",input$runSel," lensim=",
length(fvsRun$simcnts)," globals$currentQuickPlot=",globals$currentQuickPlot,"\n")      
      if (length(globals$currentQuickPlot) &&
          globals$currentQuickPlot != input$runSel)
      {
cat("setting uiRunPlot to NULL\n")        
        output$uiRunPlot <- output$uiErrorScan <- renderUI(NULL)
        globals$currentQuickPlot = character(0)
      }
      resetGlobals(globals,NULL,prms)
      loadFromList(fvsRun,globals$FVS_Runs[[input$runSel]])
      resetGlobals(globals,fvsRun,prms)
      mkSimCnts(fvsRun,fvsRun$selsim)
      output$uiCustomRunOps = renderUI(NULL)    
cat ("reloaded fvsRun$title=",fvsRun$title," uuid=",fvsRun$uuid,"\n")      
cat ("reloaded fvsRun$runScript=",fvsRun$runScript,"\n")
      if (length(fvsRun$uiCustomRunOps)) lapply(names(fvsRun$uiCustomRunOps), function (x,y)
cat ("fvsRun$uiCustomRunOps$",x,"=",y[[x]],"\n",sep=""),fvsRun$uiCustomRunOps) else
cat ("fvsRun$uiCustomRunOps is empty\n")

      isolate({
        if (input$rightPan != "Run" && length(fvsRun$simcnts)>0)
        {
          if (input$rightPan != "Components" && length(fvsRun$simcnts)>0)
          {
            globals$autoPanNav = TRUE
            updateTabsetPanel(session=session, inputId="rightPan", 
               selected="Components")
          }
          if (input$rightPan != "Stands" && length(fvsRun$simcnts)==0)
          {
            globals$autoPanNav = TRUE
            updateTabsetPanel(session=session, inputId="rightPan", 
               selected="Stands")
          }
        }
      })
      updateCheckboxGroupInput(session=session, inputId="autoOut",
        selected=fvsRun$autoOut)
      updateTextInput(session=session, inputId="title", value=fvsRun$title)
      updateTextInput(session=session, inputId="defMgmtID",
                      value=fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="simCont", 
        choices=fvsRun$simcnts, selected=fvsRun$selsim)
      updateSelectInput(session=session, inputId="simCont", 
        choices=fvsRun$simcnts, selected=fvsRun$selsim)
      updateSelectInput(session=session, inputId="addComponents", 
          choices=list(" "), selected=NULL)
      updateTextInput(session=session, inputId="startyr",  
                      value=fvsRun$startyr)
      updateTextInput(session=session, inputId="endyr",    
                      value=fvsRun$endyr)
      updateTextInput(session=session, inputId="cyclelen", 
                      value=fvsRun$cyclelen)
      updateTextInput(session=session, inputId="cycleat",  
                      value=fvsRun$cycleat)
      # if the update causes a change in the runscript selection, then
      # customRunOps will get called automatically. If it is the same
      # script then it needs to be called here to update/set the settings.
      isolate ({callCustom = fvsRun$runScript == input$runScript})
      updateSelectInput(session=session, inputId="runScript", 
          selected=fvsRun$runScript)
      if (callCustom) customRunOps()
    }
  })

  ##autoOut
  observe(fvsRun$autoOut<-as.list(input$autoOut))
  
  ## Save saveRun  
  observe({
    if (input$saveRun > 0)                  
    {
cat ("saveRun\n")
      saveRun()
      selChoices = names(globals$FVS_Runs) 
      names(selChoices) = unlist(lapply(globals$FVS_Runs,function (x) x$title))
      updateSelectInput(session=session, inputId="runSel", 
          choices=selChoices,selected=selChoices[[1]])
      FVS_Runs = globals$FVS_Runs
      save (FVS_Runs,file="FVS_Runs.RData")
    } 
  })

  ## inAdd:    Add Selected Stands
  ## inAddGrp: Add all stands in selected groups
  observe({
    if (input$inAdd > 0 || input$inAddGrp > 0) 
    {
cat ("input$inAdd=",input$inAdd," input$inAddGrp=",input$inAddGrp,
     " globals$oldInAdd=",globals$oldInAdd,"\n")
      isolate ({
        if (length(input$inStds)+length(input$inGrps) == 0) return()
          
        v <- scan(text=input$inVars,what=" ",sep=" ",quiet=TRUE)
        for (i in 1:length(globals$activeFVS))
        {
          if (globals$activeFVS[[i]][1] == v) 
          {
            fvsRun$FVSpgm <- names(globals$activeFVS[i])[1]
            break
          }
        }            
        resetGlobals(globals,fvsRun,prms) 
        extn <- extnslist[globals$activeExtens]
        updateSelectInput(session,"addCategories","Extension",
                paste0(names(extn),": ",extn),
                selected=extn[["base"]]) 
        selVarListUse <- globals$selVarList[globals$activeVariants]
        vlst <- as.list (names(selVarListUse))
        names(vlst) = selVarListUse
        updateSelectInput(session=session, inputId="inVars", NULL, 
                          vlst, vlst[[1]])
        fvsRun$startyr <- format(Sys.time(), "%Y")
        curstartyr = as.numeric(fvsRun$startyr)

        fields = dbListFields(dbIcon,"FVS_StandInit")
        
        fields = intersect(toupper(fields),toupper(c("Stand_ID","Stand_CN","Groups",
                 "Inv_Year","FVSKeywords")))        
        # use if inAdd was hit (test on old value of oldInAdd)
        dbQ = NULL
        if (input$inAdd > globals$oldInAdd)
        {
          dbSendQuery(dbIcon,'drop table if exists m.Stds') 
          if (length(input$inStds))
          {
            dbWriteTable(dbIcon,"m.Stds",data.frame(SelStds = input$inStds))
            dbQ = try(dbSendQuery(dbIcon,
              paste0('select ',paste0(fields,collapse=","),' from FVS_StandInit ',
                'where Stand_ID in (select SelStds from m.Stds)')))
          } 
        } else {
          # use if inAddGrp
          dbQ = try(dbSendQuery(dbIcon,
            paste0('select ',paste0(fields,collapse=","),' from FVS_StandInit ',
              'where Stand_ID in (select distinct Stand_ID from m.Grps ',
                  'where Grp in (select SelGrps from m.SGrps))')))
        }
        globals$oldInAdd=as.numeric(input$inAdd)
        if (is.null(dbQ) || class(dbQ) == "try-error") return()
        fvsInit = dbFetch(dbQ,n=-1)
        if (nrow(fvsInit) == 0) return()
        names(fvsInit) = toupper(names(fvsInit))
        maxMsgs = (nrow(fvsInit) %/% 10) + 2
        progress <- shiny::Progress$new(session,min=1,max=maxMsgs)
        msgVal = 1
        progress$set(message = paste0("Loading ",nrow(fvsInit)," stands "), 
            value = msgVal)
        for (row in 1:nrow(fvsInit))  # the selectInput list
        {
          if (row %% 10 == 0) 
          {
            msgVal = msgVal+1
            progress$set(value = msgVal)
          }
          sid = fvsInit[row,"STAND_ID"]
          newstd <- mkfvsStd(sid=sid,uuid=uuidgen())
          addkeys <- fvsInit[row,"FVSKEYWORDS"]
          if (!is.null(addkeys) && !is.na(addkeys) && nchar(addkeys)) 
            newstd$cmps[[1]] <- mkfvsCmp(kwds=addkeys,uuid=uuidgen(),
                     exten="base", atag="k",kwdName="From: FVS_StandInit",
                     title="From: FVS_StandInit")
          grps <- if (!is.null(fvsInit$GROUPS))
             scan(text=fvsInit[row,"GROUPS"],
                  what=" ",quiet=TRUE) else c("All All_Stands")
          requ <- unlist(grps[grep("^All",grps)])
          grps <- sort(union(intersect(input$inGrps,grps),requ))
          have <- unlist(lapply(fvsRun$grps,function(x) 
                  if (x$grp != "") x$grp else NULL))
          need <- setdiff(grps, have)
          for (grp in need) 
          {
            newgrp <- mkfvsGrp(grp=grp,uuid=uuidgen())
            grprow <- if (!is.null(globals$inData$FVS_GroupAddFilesAndKeywords)) 
              grep(grp,globals$inData$FVS_GroupAddFilesAndKeywords[,"GROUPS"],
                   fixed=TRUE) else c()   
            if (length(grprow) > 0) 
            {
              addkeys <- globals$inData$
                         FVS_GroupAddFilesAndKeywords[grprow,"FVSKEYWORDS"]
              if (!is.null(addkeys)) newgrp$cmps[[1]] <- 
                 mkfvsCmp(kwds=addkeys,uuid=uuidgen(),atag="k",exten="base",
                          kwdName="From: FVS_GroupAddFilesAndKeywords",
                            title="From: FVS_GroupAddFilesAndKeywords")
            }
            fvsRun$grps <- append(fvsRun$grps,newgrp)
          }
          invyr <- as.numeric(fvsInit[row,"INV_YEAR"])
          if (invyr > curstartyr) 
          {
            curstartyr <- invyr
            fvsRun$startyr <- as.character(curstartyr)
          }
          newstd$invyr <- as.character(invyr)
          have <- unlist(lapply(fvsRun$grps,function(x) 
                  if (x$grp != "") x$grp else NULL))
          newstd$grps <- fvsRun$grps[sort(match(grps,have))]
          fvsRun$stands <- append(fvsRun$stands,newstd)
        }
        fvsRun$endyr <- as.character(as.numeric(fvsRun$startyr) +
                          as.numeric(getPstring(prms$timing,"simLength",
                                     globals$activeVariants[1])))
        fvsRun$cyclelen <- as.character(getPstring(
                                     prms$timing,"cycleLength",
                                     globals$activeVariants[1]))
        updateTextInput(session=session, inputId="startyr",  
                        value=fvsRun$startyr)
        updateTextInput(session=session, inputId="endyr",    
                        value=fvsRun$endyr)
        updateTextInput(session=session, inputId="cyclelen", 
                        value=fvsRun$cyclelen)
        msgVal = msgVal+1
        progress$set(detail="Updating reps tags",value = msgVal)
        stds <- unlist(lapply(fvsRun$stands,function(x) x$sid))
        cnts <- table(stds)
        for (cn in 1:length(cnts)) 
        {
          cnt <- cnts[cn]
          reps <- grep(names(cnt),stds,fixed=TRUE)
          if (length(reps) > 1)
          {
            i <- 1
            for (r in reps) 
            {
              fvsRun$stands[[r]]$rep <- i
              i <- i+1
            }
          }
        }
        msgVal = msgVal+1
        progress$set(detail="Loading contents listbox",value = msgVal)
        mkSimCnts(fvsRun)
        updateSelectInput(session=session, inputId="simCont", 
          choices=fvsRun$simcnts, selected=fvsRun$selsim)
        progress$close()
      })
    }
  })
  
  ## run element selection
  observe({
    if (length(input$simCont) == 0) return()
cat ("run element selection\n")
    if (all(input$simCont == fvsRun$selsim)) return()
    mkSimCnts(fvsRun,input$simCont[[1]])
    updateSelectInput(session=session, inputId="simCont", 
         choices=fvsRun$simcnts, selected=fvsRun$selsim)
  })

  ## Edit  
  observe({
    if (input$editSel == 0) return()      
    isolate ({
      globals$currentEditCmp <- globals$NULLfvsCmp
      updateSelectInput(session=session, inputId="addComponents", selected=0)
      if (length(input$simCont) == 0) return()
      toed = input$simCont[1]
      # find component
      cmp = findCmp(fvsRun,toed)
      if (is.null(cmp)) return()
      globals$currentEditCmp = cmp
cat ("Edit, cmp$kwdName=",cmp$kwdName,"\n")
      if (exists(cmp$kwdName)) #if a function exists, use it.
      {
        eltList <- eval(parse(text=paste0(cmp$kwdName,
          "(globals$currentEditCmp$title,prms,fvsRun,globals)")))
        if (is.null(eltList)) return(NULL)
        eltList <- eltList[[1]]
      } else {
        pk <- match (cmp$kwdName,names(prms))
        if (is.na(pk)) # FreeForm Edit, used if pk does not match a parms.
        {
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
            tags$textarea(id="freeEdit", rows=15, 
                          globals$currentEditCmp$kwds)
          )
        } else {        # Launch general purpose builder when pk matches a parms.        
          pkeys <- prms[[pk]]
          eltList <- mkeltList(pkeys,prms,globals,fvsRun)     
          eltList <- append(eltList,list(
            myInlineTextInput("cmdTitle","Component title: ", 
                      value=globals$currentEditCmp$title,size=40)),after=0)          
        }
      }
      
      eltList <- append(eltList,list(
        h4(paste0('Edit: "',globals$currentEditCmp$title),'"')),after=0)
      output$cmdBuild <- renderUI(eltList)

      if (input$rightPan != "Components")
      {
        globals$autoPanNav = TRUE
        updateTabsetPanel(session=session, inputId="rightPan", 
          selected="Components")
      }
    })
  })

  ## Cut  
  observe({
    if (input$cutCmp == 0) return()
    isolate ({
      if (length(input$simCont) == 0) return
      if (moveToPaste(input$simCont[1],globals,fvsRun))
      { 
        mkSimCnts(fvsRun) 
        updateSelectInput(session=session, inputId="simCont", 
          choices=fvsRun$simcnts, selected=fvsRun$selsim)
        updateSelectInput(session=session, inputId="selpaste", 
          choices=globals$pastelistShadow,
          selected=if (length(globals$pastelistShadow)) 
              globals$pastelistShadow[[length(globals$pastelistShadow)]] else 0)
      }
    })
  })

  
  ## Copy  
  observe({
    if (input$copyCmp == 0) return()
    isolate ({
      toCpy = findCmp(fvsRun,input$simCont[1])
      if (is.null(toCpy)) return()
      toCpy = mkfvsCmp(kwds=toCpy$kwds,kwdName=toCpy$kwdName,
              exten=toCpy$exten,variant=toCpy$variant,uuid=uuidgen(),
              atag=toCpy$atag,title=toCpy$title,reopn=toCpy$reopn)
      globals$pastelist <- append(globals$pastelist,toCpy)
      globals$pastelistShadow <- append(globals$pastelistShadow,toCpy$uuid)
      names(globals$pastelistShadow)[length(globals$pastelistShadow)] =  
          toCpy$kwdName
      updateSelectInput(session=session, inputId="selpaste", 
          choices=globals$pastelistShadow,
          selected=if (length(globals$pastelistShadow)) 
            globals$pastelistShadow[[length(globals$pastelistShadow)]] else 0)
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
      idx = pasteComponent(fvsRun,input$simCont[1],topaste)
      if (!is.null(idx))
      { 
        mkSimCnts(fvsRun)   
        updateSelectInput(session=session, inputId="simCont", 
           choices=fvsRun$simcnts, selected=fvsRun$selsim)
      }
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
      cmp = findCmp(fvsRun,toed)
      if (is.null(cmp)) return()
      if (substring(cmp$kwdName,1,10) == "Freeform: ") return()
      if (substring(cmp$kwdName,1,6) != "From: ")
      { 
        kwPname = cmp$kwdName
        pkeys = prms[[kwPname]]
        if (!is.null(pkeys))
        {
          ansFrm = getPstring(pkeys,"parmsForm",globals$activeVariants[1])
          cmp$kwds = mkKeyWrd(ansFrm,cmp$reopn,pkeys,globals$activeVariants[1])
        }
        cmp$kwdName = paste0("Freeform: ",cmp$kwdName)
        cmp$title = paste0("Freeform: ",cmp$title)
        cmp$reopn = character(0)
        mkSimCnts(fvsRun,toed)   
        updateSelectInput(session=session, inputId="simCont", 
             choices=fvsRun$simcnts, selected=fvsRun$selsim)
        output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      }
    })
  })


  ## Command Set (radio button).
  observe({
cat ("command set (radio), input$cmdSet=",input$cmdSet,
  " input$simCont=",length(input$simCont),"\n")
    if (length(input$simCont) == 0) return(NULL)
    output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
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
      updateSelectInput(session=session, inputId="addComponents", selected = 0)
      output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      return(NULL)
    }
    cmd = input$cmdSet
    isolate ({ 
    switch (cmd,
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
          "(title,prms,fvsRun,globals)")))
        if (is.null(ans)) return(NULL)
        output$cmdBuild     <- renderUI (if (length(ans[[1]])) ans[[1]] else NULL)
        output$cmdBuildDesc <- renderUI (if (length(ans[[2]])) ans[[2]] else NULL)
      } else {
        globals$winBuildFunction <- character(0)
        pkeys <- prms[[indx]]
        eltList <- mkeltList(pkeys,prms,globals,fvsRun)
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
                              globals,fvsRun,cndflag=TRUE)
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
    # command Save 
    if (input$cmdSave == 0) return()
    isolate ({
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp) &&
         globals$currentCndPkey == "0" && globals$currentCmdPkey == "0") return()
      if (length(globals$currentEditCmp$reopn) && 
                 globals$currentEditCmp$reopn == "pasteOnSave") 
      {
cat ("input$freeEdit=",input$freeEdit,"\n")        
        globals$currentEditCmp$reopn = character(0)
        globals$currentEditCmp$kwds = input$freeEdit
        if (!is.null(input$cmdTitle) && nchar(input$cmdTitle)) 
          globals$currentEditCmp$title = input$cmdTitle
        idx = pasteComponent(fvsRun,input$simCont[1],globals$currentEditCmp)
        if (!is.null(idx))
        { 
          mkSimCnts(fvsRun)   
          updateSelectInput(session=session, inputId="simCont", 
             choices=fvsRun$simcnts, selected=fvsRun$selsim)
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
        waityrs = getPstring(pkeys,"waitYears",globals$activeVariants[1])
        ansFrm = getPstring(pkeys,"answerForm",globals$activeVariants[1])
        if (is.null(ansFrm)) ansFrm = 
          getPstring(pkeys,"parmsForm",globals$activeVariants[1])
        reopn = NULL
        f = 0
        repeat
        {
          f = f+1
          pkey = paste0("f",f)
          fps = getPstring(pkeys,pkey,globals$activeVariants[1])
          if (is.null(fps)) break
          instr = input[[paste0("cnd.",pkey)]]
          reopn = c(reopn,as.character(if (is.null(instr)) " " else instr))
          names(reopn)[f] = pkey
        } 
        kwds = if (is.null(waityrs)) "If\n" else 
               paste0("If           ",waityrs,"\n")
        kwds = paste0(kwds,mkKeyWrd(ansFrm,reopn,pkeys,globals$activeVariants[1]),
               "\nThen")
cat ("Save with if/then, kwds=",kwds,"\n") 
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
      } else { # we are editing the component
        kwPname = globals$currentEditCmp$kwdName
        pkeys = prms[[kwPname]]
        if (is.null(pkeys) && !is.null(input$freeEdit)) #this is freeform...
        {
          globals$currentEditCmp$kwds = input$freeEdit
          globals$currentEditCmp$title = input$cmdTitle
          mkSimCnts(fvsRun,input$simCont[[1]])
          updateSelectInput(session=session, inputId="simCont", 
             choices=fvsRun$simcnts, selected=fvsRun$selsim)
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
        if (is.null(ansFrm)) ansFrm = 
          getPstring(pkeys,"parmsForm",globals$activeVariants[1])
        if (is.null(ansFrm)) 
        {
          kw = unlist(strsplit(kwPname,".",fixed=TRUE))[3]
          ansFrm = substr(paste0(kw,"         "),1,10)
          ansFrm = paste0(ansFrm,
                   "!1,10!!2,10!!3,10!!4,10!!5,10!!6,10!!7,10!")
        }
        reopn = NULL
        f = 0
        repeat
        {
          f = f+1
          pkey = paste0("f",f)
          fps = getPstring(pkeys,pkey,globals$activeVariants[1])
          if (is.null(fps)) break
          instr = input[[pkey]]
          reopn = c(reopn,as.character(if (is.null(instr)) " " else instr))
          names(reopn)[f] = pkey
        }
        ex = if (input$cmdSet != "Keywords") "base" else
               unlist(strsplit(input$addCategories,":"))[1] 
        kwds = mkKeyWrd(ansFrm,reopn,pkeys,globals$activeVariants[1]) 
      }
cat ("Save, kwds=",kwds,"\n")      
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp))
      {
        newcmp = mkfvsCmp(uuid=uuidgen(),atag="k",kwds=kwds,exten=ex,
             variant=globals$activeVariants[1],kwdName=kwPname,
             title=input$cmdTitle,reopn=reopn)
        # find the attachment point. 
        sel = if (length(globals$schedBoxPkey) &&
              input$schedbox == 3) input$condList else input$simCont[[1]]
        grp = findIdx(fvsRun$grps,sel)
        std = if (is.null(grp)) findIdx(fvsRun$stands,sel) else NULL
        cmp = NULL
        if (is.null(grp) && is.null(std)) 
        {
          for (grp in 1:length(fvsRun$grps))
          {
            cmp = findIdx(fvsRun$grps[[grp]]$cmps,sel)
            if (!is.null(cmp)) break
          }
          if (is.null(cmp)) grp = NULL
          if (is.null(grp)) for (std in 1:length(fvsRun$stands))
          {
            cmp = findIdx(fvsRun$stands[[std]]$cmps,sel)
            if (!is.null(cmp)) break
          }
        }
        if (length(globals$schedBoxPkey) && input$schedbox == 3) 
        {
          #tag the component as being linked to the condition.
          newcmp$atag = sel
          #adjust insert point.
          if (is.null(std)) for (i in (cmp+1):length(fvsRun$grps[[grp]]$cmps))
          {
            if (i > length(fvsRun$grps[[grp]]$cmps)) break
            if (fvsRun$grps[[grp]]$cmps[[i]]$atag == sel) cmp = i
          } else for (i in (cmp+1):length(fvsRun$stands[[std]]$cmps))
          {
            if (i > length(fvsRun$stands[[std]]$cmps)) break
            if (fvsRun$stands[[std]]$cmps[[i]]$atag == sel) cmp = i
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
            fvsRun$stands[[std]]$cmps <- if (is.null(cmp))  
                append(fvsRun$stands[[std]]$cmps, newcnd) else
                append(fvsRun$stands[[std]]$cmps, newcnd, after=cmp)
          } else
          { 
            fvsRun$grps[[grp]]$cmps <- if (is.null(cmp))  
                append(fvsRun$grps[[grp]]$cmps, newcnd) else
                append(fvsRun$grps[[grp]]$cmps, newcnd, after=cmp)
          }
          if (!is.null(cmp)) cmp <- cmp+1
        } 
        # attach the new component
        if (is.null(grp)) 
        {
          fvsRun$stands[[std]]$cmps <- if (is.null(cmp))  
              append(fvsRun$stands[[std]]$cmps, newcmp) else
              append(fvsRun$stands[[std]]$cmps, newcmp, after=cmp)
        } else { 
          fvsRun$grps[[grp]]$cmps <- if (is.null(cmp))  
              append(fvsRun$grps[[grp]]$cmps, newcmp) else
              append(fvsRun$grps[[grp]]$cmps, newcmp, after=cmp)
        }
      } else {
        globals$currentEditCmp$kwds=kwds
        globals$currentEditCmp$title=input$cmdTitle
cat ("saving, kwds=",kwds," title=",input$cmdTitle," reopn=",reopn,"\n")       
        globals$currentEditCmp$reopn=if (is.null(reopn)) character(0) else reopn
        globals$currentEditCmp=globals$NULLfvsCmp
      }
      mkSimCnts(fvsRun,input$simCont[[1]])
      updateSelectInput(session=session, inputId="simCont", 
         choices=fvsRun$simcnts, selected=fvsRun$selsim)
      updateSelectInput(session=session, inputId="addComponents", selected = 0)
      output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      globals$schedBoxPkey <- character(0)
    })
  })

  ## time
  observe(fvsRun$startyr  <- input$startyr)
  observe(fvsRun$endyr    <- input$endyr)
  observe(fvsRun$cyclelen <- input$cyclelen)
  observe(fvsRun$cycleat  <- input$cycleat)

  ## Save and Run
  observe({  
    if (input$saveandrun == 0) return()
    isolate ({
      if (exists("fvsRun")) if (length(fvsRun$stands) > 0) 
      {
        saveRun()
cat("Nulling uiRunPlot at Save and Run\n")
        output$uiRunPlot <- output$uiErrorScan <- renderUI(NULL)
        globals$currentQuickPlot = character(0)
        selChoices = names(globals$FVS_Runs) 
        names(selChoices) = unlist(lapply(globals$FVS_Runs,function (x) x$title))
        updateSelectInput(session=session, inputId="runSel", 
            choices=selChoices,selected=selChoices[[1]])
        FVS_Runs = globals$FVS_Runs
        progress <- shiny::Progress$new(session,min=1,
                           max=length(fvsRun$stands)+6)
        progress$set(message = "Run preparation: ", 
          detail = "Saving FVS Runs, deleting old ouputs", value = 1)         
        save (FVS_Runs,file="FVS_Runs.RData")
        killIfRunning(fvsRun$uuid)
        removeFVSRunFiles(fvsRun$uuid)
        updateSelectInput(session=session, inputId="bkgRuns", 
                          choices=getBkgRunList(),selected=0)
        progress$set(message = "Run preparation: ", 
          detail = "Write .key file and prepare program", value = 2)
        writeKeyFile(fvsRun,dbIcon,prms)
        dir.create(fvsRun$uuid)
        if (!exists("rFVSDir")) rFVSDir = "rFVS/R/"
        if (!file.exists(rFVSDir)) rFVSDir = "rFVS/R/"
        if (!file.exists(rFVSDir)) return()
        binDir = if (file.exists("FVSbin/")) "FVSbin/" else fvsBinDir
cat ("runwaitback=",input$runwaitback,"\n")
        if (input$runwaitback!="Wait for run")
        {
          runScript = paste0(fvsRun$uuid,".rscript")
          rs = file(runScript,open="wt")
          cat ('options(echo=TRUE)\nlibrary(methods)\nlibrary(RSQLite)\n',file=rs)
          cat ('pid = Sys.getpid()\n',file=rs)
          cmd = paste0('unlink("',fvsRun$uuid,'.db")')
          cat (cmd,"\n",file=rs)
          cmd = paste0("title = '",fvsRun$title,"'")
          cat (cmd,"\n",file=rs)                   
          cmd = paste0("nstands = ",length(fvsRun$stands))
          cat (cmd,"\n",file=rs)          
          cmd = paste0("for (rf in dir('",rFVSDir,
             "')) source(paste0('",rFVSDir,"',rf))")
          cat (cmd,"\n",file=rs)
          cmd = paste0("fvsLoad('",
             fvsRun$FVSpgm,"',bin='",binDir,"')")
          cat (cmd,"\n",file=rs)
          if (fvsRun$runScript != "fvsRun")
          {
            cmd = paste0("source('customRun_",fvsRun$runScript,".R')")
            cat (cmd,"\n",file=rs)
            cat ("runOps = ",deparse(fvsRun$uiCustomRunOps),"\n",file=rs)        
          }
          cmd = paste0('fvsSetCmdLine("--keywordfile=',fvsRun$uuid,'.key")')
          cat (cmd,"\n",file=rs)
          runCmd = if (fvsRun$runScript == "fvsRun") "fvsRun()" else
               paste0(fvsRun$runScript,"(runOps)")
          pidfile = paste0(fvsRun$uuid,".pidStatus")
          cmd = 'cat (pid,"Starting title=",title,"\n")'
          cat (cmd,"\n",file=rs)       
          cmd = paste0('cat (pid,"Starting title=",title,file="',pidfile,'")')
          cat (cmd,"\n",file=rs)       
          cmd = paste0('for (istand in 1:nstands)\n{\n',
                       '  cat (pid,"Running",istand,"of",nstands," title=",title,"\n")\n',
                       '  cat (pid,"Running",istand,"of",nstands," title=",title,"\n",file="',pidfile,
                       '")\n',
                       '  rtn = ',runCmd,'\n}')
          cat (cmd,"\n",file=rs)
          cat ('source("fvsRunUtilities.R")\n',file=rs)
          cmd = paste0('dbDrv = dbDriver("SQLite")\n',
                       'dbcon = dbConnect(dbDrv,"FVSOut.db")')
          cat (cmd,"\n",file=rs)
          cmd = paste0('cat (pid,"Adding results to output database; title=",title,"\n")')
          cat (cmd,"\n",file=rs)
          cmd = paste0('cat (pid,"Adding results to output database; title=",title,"\n",file="',pidfile,
                       '")')
          cat (cmd,"\n",file=rs)
          cmd = paste0('addNewRun2DB("',fvsRun$uuid,'",dbcon)')
          cat (cmd,"\n",file=rs)
          cat ("dbDisconnect(dbcon)\n",file=rs)
          cmd = paste0("unlink('",pidfile,"')")
          cat (cmd,"\n",file=rs)
          progress$set(message = "Run starting in background", 
              detail = "", value = 5)
          unlink(paste0(fvsRun$uuid,".db"))
          close (rs)
          cmd = paste0("Rscript --no-restore --no-save --no-init-file ",runScript,
                       " > ",runScript,".Rout")
          if (.Platform$OS.type == "unix") cmd = paste0("nohup ",cmd)
cat ("cmd=",cmd,"\n")
          system (cmd,wait=FALSE) 
          Sys.sleep(2)
          updateSelectInput(session=session, inputId="bkgRuns", 
                          choices=getBkgRunList(),selected=0)
          progress$close()
          return()
        }
        fvschild = makePSOCKcluster(1)
        cmd = paste0("clusterEvalQ(fvschild,for (rf in dir('",rFVSDir,
          "')) source(paste0('",rFVSDir,"',rf)))")
cat ("load rFVS cmd=",cmd,"\n")          
        rtn = try(eval(parse(text=cmd)))
        if (class(rtn) == "try-error") return()
        cmd = paste0("clusterEvalQ(fvschild,fvsLoad('",
             fvsRun$FVSpgm,"',bin='",binDir,"'))")
cat ("load FVSpgm cmd=",cmd,"\n")          
        rtn = try(eval(parse(text=cmd)))
        if (class(rtn) == "try-error") return()          
        # if not using the default run script, load the one requested.
    
        if (fvsRun$runScript != "fvsRun")
        {
          cmd = paste0("clusterEvalQ(fvschild,",
               "source('customRun_",fvsRun$runScript,".R'))")
cat ("run script load cmd=",cmd,"\n")
          rtn = try(eval(parse(text=cmd)))
          if (class(rtn) == "try-error") return()        
          runOps <<- if (is.null(fvsRun$uiCustomRunOps)) list() else 
            fvsRun$uiCustomRunOps
          rtn = try(clusterExport(fvschild,list("runOps"))) 
          if (class(rtn) == "try-error") return()
        }
        cmd = paste0("clusterEvalQ(fvschild,",
              'fvsSetCmdLine("--keywordfile=',fvsRun$uuid,'.key"))')
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
        for (i in 1:length(fvsRun$stands))
        {
          detail = paste0("Stand ",i," StandId=",fvsRun$stands[[i]][["sid"]])          
          progress$set(message = "FVS running", detail = detail, value = i+2) 
          rtn = if (fvsRun$runScript != "fvsRun")
            {
              cmd = paste0("clusterEvalQ(fvschild,",fvsRun$runScript,"(runOps))")
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
                    value = length(fvsRun$stands)+3)
        errScan = try(errorScan(paste0(fvsRun$uuid,".out")))
        if (class(errScan) == "try-error") errScan = 
          "Error scan failed likely due to invalid multibyte strings in output"
        output$uiErrorScan <- renderUI(list(
          h5("FVS output error scan"),
          tags$style(type="text/css", paste0("#errorScan { overflow:auto; ",
             "height:150px; font-family:monospace; font-size:90%;}")),
          HTML(paste(errScan,"<br>"))))
        if (length(dir(fvsRun$uuid)) == 0) file.remove(fvsRun$uuid)
        progress$set(message = if (length(allSum) == length(fvsRun$stands))
                    "FVS finished" else
                    "FVS run failed", detail = "", 
                    value = length(fvsRun$stands)+4)
        Sys.sleep(.1)       
cat ("length(allSum)=",length(allSum),"\n")
        if (length(allSum) == 0) {Sys.sleep(.4); return()}
        progress$set(message = "FVS finished",  
                     detail = "Merging output to master database",
                     value = length(fvsRun$stands)+5)
        res = addNewRun2DB(fvsRun$uuid,dbcon)
cat ("addNewRun2DB res=",res,"\n")
        unlink(paste0(fvsRun$uuid,".db"))
        X = vector("numeric",0)
        hfacet = vector("character",0)
        Y = vector("numeric",0)
        Legend=vector("character",0)
        progress$set(message = "Building plot", detail = "", 
                     value = length(fvsRun$stands)+6)
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
        globals$currentQuickPlot = fvsRun$uuid
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
  ## Download output
  output$dlFVSRunout <- downloadHandler(filename=function ()
      paste0(globals$FVS_Runs[[isolate(input$runSel)]]$title,"_FVSoutput.txt"),
      content=function (tf = tempfile())
      {
        sfile = paste0(input$runSel,".out")
        if (file.exists(sfile)) file.copy(sfile,tf) else
          cat (file=tf,"Output not yet created.\n")
      }, contentType="text")
  ## Download keywords
  output$dlFVSRunkey <- downloadHandler(filename=function ()
      paste0(globals$FVS_Runs[[isolate(input$runSel)]]$title,"_FVSkeywords.txt"),
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
           opref = globals$FVS_Runs[[input$runSel]]$title
           for (ele in input$dlZipSet)
           {
             switch (ele,
               outdb = file.copy(from="FVSOut.db",
                                 to=paste0(tempDir,"/FVSOut.db")),
               key   = {
                 from=paste0(input$runSel,".key")
                 if (file.exists(from)) file.copy(from=from,
                   to=paste0(tempDir,"/",opref,"_FVSkeywords.txt"))
               },
               out   = {
                 from=paste0(input$runSel,".out")
                 if (file.exists(from)) file.copy(from=from,
                   to=paste0(tempDir,"/",opref,"_FVSoutput.txt"))
               },
               subdir= {
                 from=input$runSel
                 if (dir.exists(from)) 
                 {
                   to = paste0(tempDir,"/",opref,"_SVS/")
                   dir.create (to)
                   file.copy(from=from,to=to,recursive = TRUE)
                   file.copy(from=paste0(from,"_index.svs"),to=to)
                 }
               },              
               FVS_Data = file.copy(from="FVS_Data.db",
                                    to=paste0(tempDir,"/FVS_Data.db")),
               FVS_Runs = file.copy(from="FVS_Runs.RData",
                                    to=paste0(tempDir,"/FVS_Runs.RData")),
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

  ## kcpSave
  observe({  
    if (input$kcpSave > 0)
    {
      isolate ({
cat ("kcpSave called, kcpTitle=",input$kcpTitle," isnull=",
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
  

  ## Tools, related to FVSRefresh
  observe({    
    if (input$rightPan == "Tools") 
    {
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
          rtn=file.copy(from=paste0(fvsBinDir,pgm,shlibsufx),to="FVSbin")
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
      if (is.null(input$runSel) || 
          is.null(globals$FVS_Runs[[input$runSel]])) return()
      isolate({
        tit = globals$FVS_Runs[[input$runSel]]$title
        session$sendCustomMessage(type = "dialogContentUpdate",
          message = list(id = "deleteRunDlg", 
            message = paste0('Delete run: "',tit,'." Are you sure?')))
      })
    }
  })
  observe({
    if (input$deleteRunDlgBtn > 0)
    {
      isolate({
cat ("delete run",fvsRun$title," uuid=",fvsRun$uuid," runSel=",input$runSel,
 "lenRuns=",length(globals$FVS_Runs),"\n")                           
        if (is.null(input$runSel) || length(globals$FVS_Runs) == 0 || 
            is.null(globals$FVS_Runs[[input$runSel]])) return() 
        globals$FVS_Runs[[input$runSel]] = NULL
        killIfRunning(input$runSel)
        removeFVSRunFiles(input$runSel)
        deleteRelatedDBRows(input$runSel,dbcon)
        FVS_Runs = globals$FVS_Runs
        save (FVS_Runs,file="FVS_Runs.RData")
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
                  message = "Delete all runs! Are you sure?"))
    }
  })
  observe({  
    if (input$deleteAllRunsDlgBtn == 0) return()
    isolate({
cat ("delete all runs\n")
      rmfiles=dir(pattern="[.]pidStatus$")      
      for (tokill in rmfiles) killIfRunning(sub(".pidStatus","",tokill))
      file.remove("FVSOut.db")
      file.remove("FVS_Runs.RData")
      rmfiles=dir(pattern="[.]key$")      
      rmfiles=gsub ("[.]key$","*",rmfiles)
      if (length(rmfiles)) unlink(rmfiles,recursive=TRUE)
      rmfiles=dir(pattern="[.]out$")      
      rmfiles=gsub ("[.]out$","*",rmfiles)
      if (length(rmfiles)) unlink(rmfiles,recursive=TRUE)
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
    if (input$topHelp > 0)
    { 
      source("topHelp.R")     
      output$uiHelpText <- renderUI(
        list(tags$style(type="text/css", 
             "#uiHelpText { overflow:auto; height:150px;}"),
             HTML(topHelp)))
      output$uiHelpClose <- renderUI(actionButton("helpClose","Close Help"))
    }
  })
  #helpClose
  observe({
    if (length(input$helpClose) && input$helpClose > 0)
    {
      output$uiHelpText <- renderUI(NULL)
      output$uiHelpClose <- renderUI(NULL)
    }
  })
  #feedBack
  observe({
    if (input$feedBack > 0)
    {
      output$uiHelpText <- renderUI(list(
        h5("Enter your comments and suggestions"),
        tags$style(type="text/css",paste0("#comments {overflow:auto;",
          "height:150px; width:95%; background-color: #eef8ff;}")),
        tags$textarea(id="comments", rows=10)
      ))
      output$uiHelpClose <- renderUI(list(
        tags$style(type="text/css","#submitFeedback {background-color: #eef8ff;}"),
        tags$style(type="text/css","#cancelFeedback {background-color: #eef8ff;}"),
        actionButton("submitFeedback","Submit"),
        actionButton("cancelFeedback","Cancel")
      )) 
    }
  })
  #submitFeedback
  observe({
    if (length(input$submitFeedback) && input$submitFeedback > 0)
    {
      if (nchar(input$comments) > 2)
        cat ("Date: ",date(),"\n",input$comments,"\n",file="userComments.txt",
          append=TRUE)     
      output$uiHelpText <- renderUI(NULL)
      output$uiHelpClose <- renderUI(NULL)     
    }
  })
  #cancelFeedback
  observe({
    if (length(input$cancelFeedback) && input$cancelFeedback > 0)
    {
      output$uiHelpText <- renderUI(NULL)
      output$uiHelpClose <- renderUI(NULL)     
    }
  })
  
  #dataEditor
  observe(if (input$launchDataEditor > 0 || input$inDataEdit > 0) 
  {
cat ("dataEditor\n")
    file.copy("server.R","fvsOnlineServer.R", overwrite=TRUE)
    file.copy("ui.R",    "fvsOnlineUI.R",     overwrite=TRUE)
    file.copy("editDataServer.R","server.R",  overwrite=TRUE)
    file.copy("editDataUI.R",    "ui.R",      overwrite=TRUE)   
    output$locReload<-renderUI(tags$script("location.reload();"))
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
      fvsRun$runScript = input$runScript
      output$uiCustomRunOps = renderUI(NULL)    
      if (input$runScript != "fvsRun")
      {
        rtn = try(source(paste0("customRun_",fvsRun$runScript,".R")))
        if (class(rtn) == "try-error") return()
        uiF = try(eval(parse(text=paste0(sub("fvsRun","ui",fvsRun$runScript)))))
        if (class(uiF) != "function") return()
        output$uiCustomRunOps = renderUI(uiF(fvsRun))
      } else {
        fvsRun$uiCustomRunOps = list()
      }
if (length(fvsRun$uiCustomRunOps)) lapply(names(fvsRun$uiCustomRunOps), function (x,y)
cat ("fvsRun$uiCustomRunOps$",x,"=",y[[x]],"\n",sep=""),fvsRun$uiCustomRunOps) else
cat ("fvsRun$uiCustomRunOps is empty\n")
    })
  }

  
  saveRun <- function() 
  {
    isolate({
cat ("in saveRun\n")    
      if (input$title == "")
      {
        fvsRun$title = fvsRun$uuid
        updateTextInput(session=session, inputId="title", value=fvsRun$title)
      } else fvsRun$title = input$title
      fvsRun$title = input$title
      fvsRun$defMgmtID = input$defMgmtID
      fvsRun$runScript = if (length(input$runScript)) input$runScript else "fvsRun"
      if (fvsRun$runScript == "fvsRun") fvsRun$uiCustomRunOps = list() else
      {
        for (item in names(fvsRun$uiCustomRunOps))
        {
          fvsRun$uiCustomRunOps[[item]] = input[[item]]
cat("during saveRun, item=",item," val=",fvsRun$uiCustomRunOps[[item]],"\n")    
        }
      }
      globals$FVS_Runs[[fvsRun$uuid]] = asList(fvsRun)
      globals$FVS_Runs = reorderFVSRuns(globals$FVS_Runs)    
    }) 
  }
   
})

