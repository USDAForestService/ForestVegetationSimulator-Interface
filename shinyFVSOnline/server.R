library(shiny)
library(ggplot2)
library(parallel)
library(RSQLite)

# set shiny.trace=T for reactive tracing (lots of output)
options(shiny.maxRequestSize=30*1024^2,shiny.trace = F) 

shinyServer(function(input, output, session) {

  source("mkInputElements.R")
  source("fvsRunUtilities.R")
  source("fvsOutUtilities.R")
  
  load("prms.RData") 

  globals <- mkglobals(saveOnExit=TRUE,autoPanNav=TRUE)
  loadInvData(globals,prms)
  resetGlobals(globals,NULL,prms)

  ##load existing runs
  fvsRun <- mkfvsRun()
  repeat
  {
    if (file.exists("FVS_Runs.RData"))
    {
      load("FVS_Runs.RData")
      globals$FVS_Runs = FVS_Runs
      rm (FVS_Runs)
      trycall <- try(loadFromList(fvsRun,globals$FVS_Runs[[1]]))
      if (class(trycall)=="try-error")
      {
        globals$FVS_Runs[[1]] = NULL
        if (length(globals$FVS_Runs)) 
        {
          FVS_Runs = globals$FVS_Runs
          save (FVS_Runs,file="FVS_Runs.RData")
        } else file.remove("FVS_Runs.RData")
        resetfvsRun(fvsRun,globals$FVS_Runs)
        next
      } else break
    } else 
    {
      globals$FVS_Runs = list()
      resetfvsRun(fvsRun,globals$FVS_Runs)
      break
    }
  }
  mkSimCnts(fvsRun,fvsRun$selsim)
  resetGlobals(globals,fvsRun,prms)
  selChoices = names(globals$FVS_Runs) 
  names(selChoices) = unlist(lapply(globals$FVS_Runs,function (x) x$title))
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

  session$onSessionEnded(function ()
  {
cat ("onSessionEnded, globals$saveOnExit=",globals$saveOnExit,"\n")
    if (exists("dbcon")) try(dbDisconnect(dbcon))
    if (!globals$saveOnExit) return()
    saveRun()
    FVS_Runs = globals$FVS_Runs
    save (FVS_Runs,file="FVS_Runs.RData")
    if (file.exists("projectId.txt"))
    {
      prjid = scan("projectId.txt",what="",sep="\n",quiet=TRUE)
      write(file="projectId.txt",prjid)
    }
#    stopApp()
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
        output$uiRunPlot <- output$uiErrorScan <- renderUI(NULL)
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
cat ("runs, run selection (load)\n")
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
          cnt = 0
          if (tb == "FVS_Cases") next
          else if (tb == "Composite")
            dbSendQuery(dbcon,"drop table Composite")
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
        }
#### add code for when FVS_Summary_East is present
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
#when the processing of the eastern summary is fixed up, need this code will be needed.
#        if (!is.null(dbd[["FVS_Summary_East"]])) dbd$FVS_Summary_East = 
#            c(dbd$FVS_Summary_East,c("TPrdTpa","TPrdMTCuFt","TPrdSCuFt","TPrdSBdFt"))
#this code will need to be modified for eastern composites
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
        if (tb == "Composite") 
        {
          dtab = dbReadTable(dbcon,"Composite")
          dtab = by(dtab,as.factor(dtab$MgmtID),FUN=function (x) 
                 setupSummary(x,composite=TRUE))
          dtab = do.call("rbind",dtab)
          dtab$Year=as.factor(dtab$Year) 
          dtab$MgmtID=as.factor(dtab$MgmtID) 
          dat = list(Composite = dtab)
          break
        } else{
          dtab = dbGetQuery(dbcon,paste0("select * from ",tb,
                 " where CaseID in (select CaseID from m.Cases)"))
          if (tb == "FVS_Summary") 
          { 
            dtab = by(dtab,as.factor(dtab$CaseID),FUN=function (x) 
                      setupSummary(x))
            dtab = do.call("rbind",dtab)
            dtab$ForTyp =as.factor(dtab$ForTyp)
            dtab$SizeCls=as.factor(dtab$SizeCls)
            dtab$StkCls =as.factor(dtab$StkCls)
          }
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
      if (is.null(mdat$Year)) updateSelectInput(session, "year", 
          choices  = list("None loaded"), selected = NULL) else 
        updateSelectInput(session, "year", choices=as.list(levels(mdat$Year)), 
          selected=levels(mdat$Year))
      if (is.null(mdat$Species)) updateSelectInput(session, "species", 
          choices  = list("None loaded"), selected = NULL) else 
        updateSelectInput(session, "species",
          choices=as.list(levels(mdat$Species)), selected=levels(mdat$Species))
      if (is.null(mdat$DBHClass)) updateSelectInput(session, "dbhclass", 
          choices  = list("None loaded"), selected = NULL) else 
        updateSelectInput(session, "dbhclass", 
          choices=as.list(levels(mdat$DBHClass)), selected=levels(mdat$DBHClass))
      selVars = unlist(lapply(c("StandID","MgmtID","Year","^DBH","Species",
        "^Ht$","TCuFt","Total","SCuFt"),function (x,vs) 
        {
          hits = unlist(grep(x,vs,ignore.case = TRUE))
          hits = hits[hits>0]
          vs[hits]
        },vars))      
      updateCheckboxGroupInput(session, "browsevars", choices=as.list(vars), 
                               selected=selVars,inline=TRUE)
      fvsOutData$dbData        <- mdat
      fvsOutData$browseVars    <- vars
      fvsOutData$browseSelVars <- selVars
    } 
  })
  
  ## renderTable
  output$table <- renderTable(
  {
cat("renderTable\n")
    if (input$leftPan == "Load Output") return(NULL) 
    if (length(input$selectdbvars) == 0) return(NULL) 
cat("renderTable continued\n")
    if (length(input$browsevars) == 0)
    {
      fvsOutData$browseSelVars <- character(0)
      return (NULL)
    }
    fvsOutData$browseSelVars <- input$browsevars
    {
      if (length(input$browsevars) > 0) 
      {
        dat = fvsOutData$dbData[filterRows(fvsOutData$dbData, input$stdtitle, 
          input$stdid, input$mgmid, input$year, input$species, input$dbhclass)
          ,,drop=FALSE]
cat ("nrow fvsOutData$dbData=",nrow(fvsOutData$dbData),
     " nrow dat=",nrow(dat),"\n")
        sel=match(input$browsevars,colnames(dat))
cat ("match=",sel,"\n")
        dat = dat[,sel,drop=FALSE]
        if (!is.null(input$pivVar)  && input$pivVar  != "None" &&
            !is.null(input$dispVar) && input$dispVar != "None") dat = 
              pivot(dat,input$pivVar,input$dispVar)
       fvsOutData$render = dat
        if (nrow(dat) > 3000) dat[1:3000,,drop=FALSE] else dat
      } else NULL
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

      sel = if (length(intersect(cont,"Year")) > 0) "Year" else 
            if (length(cont) > 0) cont[1] else NULL
      if (sel=="Year" && input$plotType == "scatter" && length(cont) > 1) 
          sel = setdiff(cont,"Year")[1]
      updateSelectInput(session, "xaxis",choices=
             if (input$plotType == "line" || input$plotType == "scatter") 
               as.list(cont) else as.list(cats), selected=sel)
      sel = setdiff(cont,c(sel,"Year"))
      if (length(sel) > 0 && input$plotType != "line" && 
        input$plotType != "scatter") sel = sel[1]
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
      png(outfile, width=3, height=2, res=72, units="in", pointsize=12)              
      plot.new()
      text(x=.5,y=.5,"Nothing to graph",col="red")
      dev.off()
      list(src = outfile)
    }
    if (input$leftPan == "Load Output"  || 
        length(input$selectdbvars) == 0 || (length(input$xaxis) == 0 && 
        length(input$yaxis) == 0)) return(nullPlot())

    vf = if (input$vfacet == "None") NULL else input$vfacet
    if (!is.null(vf) && (input$xaxis  == vf || input$yaxis == vf))  vf = NULL
    hf = if (input$hfacet == "None") NULL else input$hfacet
    if (!is.null(hf) && (input$xaxis  == hf || input$yaxis == hf))  hf = NULL
    pb = if (input$pltby  == "None") NULL else input$pltby
    if (!is.null(pb) && (input$xaxis  == pb || input$yaxis == pb))  pb = NULL

    dat = fvsOutData$dbData[filterRows(fvsOutData$dbData, input$stdtitle, 
          input$stdid, input$mgmid, input$year, input$species, input$dbhclass),]
    if (!is.null(vf) && nlevels(dat[,vf]) < 2) vf=NULL
    if (!is.null(hf) && nlevels(dat[,hf]) < 2) hf=NULL
    if (!is.null(pb) && nlevels(dat[,pb]) < 2) pb=NULL

    nlv  = 1 + (!is.null(pb)) + (!is.null(vf)) + (!is.null(hf))
    
    vars = c(input$xaxis, vf, hf, pb, input$yaxis)
                                         
    if (input$xaxis == "Year" && input$plotType != "box" && 
        input$plotType != "bar") dat$Year = as.numeric(as.character(dat$Year))

    nd = NULL  
    for (v in vars[(nlv+1):length(vars)])
    {
      if (is.na(v)) return(nullPlot())
      pd = dat[,c(vars[1:nlv],v),drop=FALSE]
      names(pd)[ncol(pd)] = "Y"
      nd = rbind(nd, data.frame(pd,Attribute=v,stringsAsFactors=FALSE))
    }
    nd = na.omit(nd)
    names(nd)[match(input$xaxis,names(nd))] = "X"
    if (!is.null(vf)) names(nd)[match(vf,names(nd))] = "vfacet"
    if (!is.null(hf)) names(nd)[match(hf,names(nd))] = "hfacet"      
    if (!is.null(pb) && !is.null(nd$Attribute)) 
    {
      alv = nlevels(as.factor(nd$Attribute))
      nd$Attribute = if (alv == 1) paste(pb,nd[,pb],sep=":") else
                                   paste(nd$Attribute,pb,nd[,pb],sep=":")
    }
      
    if (!is.null(nd$vfacet))    nd$vfacet    = as.factor(nd$vfacet)
    if (!is.null(nd$hfacet))    nd$hfacet    = as.factor(nd$hfacet)
    if (!is.null(nd$Attribute)) nd$Attribute = as.factor(nd$Attribute)

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
      ggplotColours(n=nlevels(nd$Attribute)+1)
    if (!is.null(colors)) p = p + scale_colour_manual(values=colors)
    plt = switch(input$plotType,
      line    = if (input$colBW == "B&W") 
        geom_line    (aes(x=X,y=Y,color=Attribute,linetype=Attribute)) else
        geom_line    (aes(x=X,y=Y,color=Attribute)),
      scatter = if (input$colBW == "B&W") 
        geom_point   (aes(x=X,y=Y,shape=Attribute,color=Attribute)) else
        geom_point   (aes(x=X,y=Y,shape=Attribute,color=Attribute)),
      bar     = if (input$colBW == "B&W") 
        geom_bar     (aes(x=X,y=Y,color=Attribute,fill=Attribute),
           position="dodge",stat="identity") else
        geom_bar     (aes(x=X,y=Y,color=Attribute,fill=Attribute),
           position="dodge",stat="identity"),
      box     = if (input$colBW == "B&W") 
        geom_boxplot (aes(x=X,y=Y,color=Attribute,linetype=Attribute)) else
        geom_boxplot (aes(x=X,y=Y,color=Attribute))
      )
    if (input$colBW == "B&W" && input$plotType == "bar") 
      p = p + scale_fill_grey(start=.15, end=.85)
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
        vlst <- as.list(globals$activeFVS[fvsRun$FVSpgm][[1]][[1]][1])
        names(vlst) <- globals$selVarList[[vlst[[1]]]]
      }
      updateSelectInput(session=session, inputId="inVars", choices=vlst,
            selected=vlst[[1]])
    } 
  })
  
  ## inVars has changed
  observe({    
    if (input$rightPan != "Stands") return()
    if (is.null(input$inVars)) return() 
    if (is.null(globals$inData$FVS_StandInit$VARIANT)) return()
    vars <- unlist(lapply(input$inVars, function(x) 
      scan(text=x,what=" ",quiet=TRUE)[1]))
    hits <- vector("logical",nrow(globals$inData$FVS_StandInit))
    for (i in 1:length(hits))
    {
      v <- tolower(scan(text=globals$inData$FVS_StandInit$VARIANT[i],
                            what=" ",quiet=TRUE))
      hits[i] <- !all (is.na(match(v,vars))) 
    }   
    selGrpList <- as.list(sort(unique(unlist(lapply (
        globals$inData$FVS_StandInit[hits,"GROUPS"], 
            function (x) scan(text=x,what=" ",quiet=TRUE))))))
    updateSelectInput(session=session, inputId="inGrps", choices=selGrpList,
            selected=if (length(selGrpList)> 0) selGrpList[[1]] else NULL)
  })

  ## inGrps has changed
  observe({
    if (input$rightPan != "Stands") return()
    if (is.null(input$inGrps)) return()
    if (is.null(globals$inData$FVS_StandInit$VARIANT)) return()
    if (is.null(globals$inData$FVS_StandInit$GROUPS)) return()
    grps <- input$inGrps
    # inVars will not be NULL if inGrps is not NULL
    var  <- scan(text=input$inVars,what=" ",quiet=TRUE)[1]
    hits <- vector("logical",nrow(globals$inData$FVS_StandInit))
    for (i in 1:length(hits))
    {
      v <- scan(text=tolower(globals$inData$FVS_StandInit$VARIANT[i]),
                      what=" ",quiet=TRUE)
      g <- if (!is.null(globals$inData$FVS_StandInit$GROUPS))
              scan(text=globals$inData$FVS_StandInit$GROUPS[i],
                   what=" ",quiet=TRUE) else c("All","All_Stands")
      hits[i] <- !all (is.na(match(v,var))) && !all (is.na(match(g,grps))) 
    }   
    updateSelectInput(session=session, inputId="inStds", 
         choices=globals$selStdList[hits], selected=globals$selStdList[hits])
  })

  ## New run    
  observe({
    if (input$newRun > 0)
    {
      resetfvsRun(fvsRun,globals$FVS_Runs)
      fvsRun$title <- paste0("Run ",length(globals$FVS_Runs)+1)
      resetGlobals(globals,NULL,prms)
      loadInvData(globals,prms)
      updateTextInput(session=session, inputId="title", label="", 
                      value=fvsRun$title) 
      updateTextInput(session=session, inputId="defMgmtID",
                     value=fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="simCont", 
          choices=list(" "), selected=NULL)
      updateSelectInput(session=session, inputId="addCategories", 
          choices=list(" "), selected=NULL)
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
      if (is.null(input$runSel)) return
cat ("reload or run selection, runSel=",input$runSel," lensim=",
  length(fvsRun$simcnts),"\n")      
      resetGlobals(globals,NULL,prms)
      loadFromList(fvsRun,globals$FVS_Runs[[input$runSel]])
      resetGlobals(globals,fvsRun,prms)
      mkSimCnts(fvsRun,fvsRun$selsim)
      output$uiRunPlot = renderUI(NULL)
      output$uiCustomRunOps = renderUI(NULL)    
cat ("reloaded: fvsRun$title=",fvsRun$title," uuid=",fvsRun$uuid,"\n")      
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


  ## inAdd: Add Selected Stands
  observe({
    if (input$inAdd > 0) 
    {
      isolate ({
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
        for (toadd in input$inStds)  # the selectInput list
        {
          newstd <- mkfvsStd(sid=toadd,uuid=uuidgen())
          row <- match(toadd,globals$inData$FVS_StandInit$STAND_ID)
          if (length(row) > 1) row = row[1]
          addkeys <- globals$inData$FVS_StandInit[row,"FVSKEYWORDS"]
          if (!is.null(addkeys) && !is.na(addkeys) && nchar(addkeys)) 
            newstd$cmps[[1]] <- mkfvsCmp(kwds=addkeys,uuid=uuidgen(),
                     exten="base", atag="k",kwdName="From: FVS_StandInit",
                     title="From: FVS_StandInit")
          grps <- if (!is.null(globals$inData$FVS_StandInit$GROUPS))
             scan(text=globals$inData$FVS_StandInit[row,"GROUPS"],
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
                          kwdName="From: FVS_StandInit",
                            title="From: FVS_StandInit")
            }
            fvsRun$grps <- append(fvsRun$grps,newgrp)
          }
          invyr <- as.numeric(globals$inData$FVS_StandInit[row,"INV_YEAR"])
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
        mkSimCnts(fvsRun)
        updateSelectInput(session=session, inputId="simCont", 
          choices=fvsRun$simcnts, selected=fvsRun$selsim)
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
      updateSelectInput(session=session, inputId="addComponents", selected = 0)
      if (length(input$simCont) == 0) return()
      toed = input$simCont[1]
      # find component
      cmp = findCmp(fvsRun,toed)
      if (is.null(cmp)) return()
      globals$currentEditCmp = cmp
      pk = match (cmp$kwdName,names(prms))
      output$cmdBuild <- renderUI( if (is.na(pk))
      {
        list(
          h4(paste0('Edit: "',globals$currentEditCmp$title),'"'),
          textInput("cmdTitle","", value=globals$currentEditCmp$title),          
          tags$style(type="text/css", 
            "#freeEditCols{font-family:monospace;font-size:90%;width:95%;}"), 
          tags$p(id="freeEditCols", 
                 HTML(paste0("&nbsp;",paste0("....+....",1:8,collapse="")))),
          tags$style(type="text/css", 
            "#freeEdit{font-family:monospace;font-size:90%;width:95%;}"), 
          tags$textarea(id="freeEdit", rows=15, 
                        globals$currentEditCmp$kwds)
        )
      } else
      {
        pkeys <- prms[[pk]]
        eltList <- mkeltList(pkeys,prms,globals,fvsRun)
        eltList <- append(eltList,list(
          h4(paste0('Edit: "',globals$currentEditCmp$title),'"'),
          textInput("cmdTitle","", value=globals$currentEditCmp$title)),after=0)          
      })      
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
cat ("category selection, cmd=",cmd,"input$addCategories=",input$addCategories,"\n")
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
                textInput("cmdTitle","", value=globals$currentEditCmp$title),
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
    globals$currentCndPkey <- 0
    globals$currentCmdPkey <- as.numeric(input$addComponents)
    if (is.na(globals$currentCmdPkey)) return(NULL)
    isolate ({
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
      pkeys <- prms[[globals$currentCmdPkey]]
      eltList <- mkeltList(pkeys,prms,globals,fvsRun)
      if (!is.null(title)) eltList <- 
        append(eltList,textInput("cmdTitle","", value=title),after=0)
      output$cmdBuild <- renderUI (if (length(eltList)) eltList else NULL)
      des <- getPstring(pkeys,"description",globals$activeVariants[1])
      output$cmdBuildDesc <- renderUI (if (!is.null(des) && nchar(des) > 0)
        HTML(paste0("<br>Description:<br>",gsub("\n","<br>",des))) else NULL)
    })
  })

  # schedule box toggled.
  observe({  
    if (length(input$schedbox) == 0) return()
cat("input$schedbox=",input$schedbox,"\n")
    if (input$schedbox == 1) 
    {
      updateTextInput(session, globals$schedBoxPkey, 
        label = "Year or cycle number", 
        value = globals$schedBoxYrLastUsed) 
      output$conditions <- renderUI(NULL)
    } else if (input$schedbox == 2) 
    {
      updateTextInput(session, globals$schedBoxPkey, 
        label = "Number of years after condition is found true", value = "0") 
      cndlist = unlist(prms$conditions_list)
      names(cndlist) = unlist(lapply(prms$conditions_list,attr,"pstring"))
      cndlist = as.list(cndlist)
      default =  getPstring(prms[[globals$currentCmdPkey]],
        "defaultCondition",globals$activeVariants[1])
      if (is.null(default)) default="cycle1"
      output$conditions <- renderUI(list(
        selectInput("condList", "Create a condition", cndlist, 
          selected = default, multiple = FALSE, selectize = FALSE),
        uiOutput("condElts")
      ))
    } else 
    {
      globals$currentCndPkey <- 0
      updateTextInput(session, globals$schedBoxPkey, 
        label = "Number of years after condition is found true", value = "0") 
      output$conditions <- renderUI(list(
        selectInput("condList","Existing conditions", globals$existingCmps, 
          selected = NULL, multiple = FALSE, selectize = FALSE),
        h5("Settings for the action (e.g. keyword(s)):")))
    }
  })

  observe({  
    # schedule by condition condition selection
    if (length(input$schedbox) == 0) return()
    if (length(input$condList) == 0) return() 
    output$condElts <- renderUI(if (input$condList == "none") NULL else
      {
        cnpkey <- paste0("condition.",input$condList)
        idx <- match(cnpkey,names(prms))
        globals$currentCndPkey <- if (is.na(idx)) 0 else idx
        ui = if (globals$currentCndPkey == 0) NULL else
        {
          eltList <- mkeltList(prms[[globals$currentCndPkey]],prms,
                              globals,fvsRun,cndflag=TRUE)
          if (length(eltList) == 0) NULL else eltList
        }
        if (!is.null(ui))
        {
          title = getPstring(prms$conditions_list,input$condList)
          if (!is.null(title)) ui <- 
            append(ui,textInput("cndTitle","", value=title),after=1)
        }
        ui
      })
  })

  observe({  
    # command Cancel
    if (input$cmdCancel == 0) return()
    globals$currentEditCmp <- globals$NULLfvsCmp
    output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
    updateSelectInput(session=session, inputId="addComponents", selected = 0)
  })

  observe({  
    # command Save 
    if (input$cmdSave == 0) return()
    isolate ({
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp) &&
         globals$currentCndPkey == 0 && globals$currentCmdPkey == 0) return()

      if (length(globals$currentEditCmp$reopn) && 
                 globals$currentEditCmp$reopn == "pasteOnSave") 
      {
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
      if (globals$currentCndPkey > 0)
      {
        kwPname = names(prms)[globals$currentCndPkey]
        pkeys = prms[[globals$currentCndPkey]]
        waityrs = getPstring(pkeys,"waitYears",globals$activeVariants[1])
        # use parmsForm if it is available
        ansFrm = getPstring(pkeys,"parmsForm",globals$activeVariants[1])
        if (is.null(ansFrm)) ansFrm = 
         getPstring(pkeys,"answerForm",globals$activeVariants[1])
        inp = NULL
        f = 0
        repeat
        {
          f = f+1
          pkey = paste0("f",f)
          fps = getPstring(pkeys,pkey,globals$activeVariants[1])
          if (is.null(fps)) break
          inp = c(inp,as.character(input[[paste0("cnd.",pkey)]]))
          names(inp)[length(inp)] = pkey
        } 
        kwds = if (is.null(waityrs)) "If\n" else 
               paste0("If           ",waityrs,"\n")
        kwds = paste0(kwds,mkKeyWrd(ansFrm,inp,pkeys,globals$activeVariants[1]),
               "\nThen")
        newcnd = mkfvsCmp(uuid=uuidgen(),atag="c",exten="base",
                   kwdName=kwPname,title=input$cndTitle,kwds=kwds,reopn=inp)
      } else newcnd = NULL
      # make or edit a keyword. This section is used for both 
      # building a keyword and editing a keyword or a condition.
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp))
      {
        kwPname = names(prms)[globals$currentCmdPkey]
        pkeys = prms[[globals$currentCmdPkey]]
      } else
      {
        kwPname = globals$currentEditCmp$kwdName
        pkeys = prms[[kwPname]]
        if (is.null(pkeys)) #this is freeform...
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
      # always use parmsForm if it is available
      ansFrm = getPstring(pkeys,"parmsForm",globals$activeVariants[1])
      if (is.null(ansFrm)) ansFrm = 
         getPstring(pkeys,"answerForm",globals$activeVariants[1])
      if (is.null(ansFrm)) 
      {
        kw = unlist(strsplit(kwPname,".",fixed=TRUE))[3]
        ansFrm = substr(paste0(kw,"         "),1,10)
        ansFrm = paste0(ansFrm,
                 "!1,10!!2,10!!3,10!!4,10!!5,10!!6,10!!7,10!")
      }
      inp = NULL
      f = 0
      repeat
      {
        f = f+1
        pkey = paste0("f",f)
        fps = getPstring(pkeys,pkey,globals$activeVariants[1])
        if (is.null(fps)) break
        inp = c(inp,as.character(input[[pkey]]))
        names(inp)[length(inp)] = pkey
      }
      ex = if (input$cmdSet != "Keywords") "base" else
             unlist(strsplit(input$addCategories,":"))[1] 
      kwds = mkKeyWrd(ansFrm,inp,pkeys,globals$activeVariants[1])
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp))
      {
        newcmp = mkfvsCmp(uuid=uuidgen(),atag="k",kwds=kwds,exten=ex,
             variant=globals$activeVariants[1],kwdName=kwPname,
             title=input$cmdTitle,reopn=inp)
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
        } else
        { 
          fvsRun$grps[[grp]]$cmps <- if (is.null(cmp))  
              append(fvsRun$grps[[grp]]$cmps, newcmp) else
              append(fvsRun$grps[[grp]]$cmps, newcmp, after=cmp)
        }
      } else
      {
        globals$currentEditCmp$kwds=kwds
        globals$currentEditCmp$title=input$cmdTitle
        globals$currentEditCmp$reopn=if (is.null(inp)) character(0) else inp
        globals$currentEditCmp=globals$NULLfvsCmp
      }
      mkSimCnts(fvsRun,input$simCont[[1]])
      updateSelectInput(session=session, inputId="simCont", 
         choices=fvsRun$simcnts, selected=fvsRun$selsim)
      updateSelectInput(session=session, inputId="addComponents", selected = 0)
      output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
    })
  })

  ## time
  observe(fvsRun$startyr  <- input$startyr)
  observe(fvsRun$endyr    <- input$endyr)
  observe(fvsRun$cyclelen <- input$cyclelen)
  observe(fvsRun$cycleat  <- input$cycleat)

  ## saveandrun
  observe({  
    if (input$saveandrun == 0) return()

    isolate ({
      if (exists("fvsRun")) if (length(fvsRun$stands) > 0) 
      {
        output$uiRunPlot = renderUI(NULL)
        saveRun()
        selChoices = names(globals$FVS_Runs) 
        names(selChoices) = unlist(lapply(globals$FVS_Runs,function (x) x$title))
        updateSelectInput(session=session, inputId="runSel", 
            choices=selChoices,selected=selChoices[[1]])
        FVS_Runs = globals$FVS_Runs
        progress <- shiny::Progress$new(session,min=1,
                           max=length(fvsRun$stands)+5)
        progress$set(message = "Run preparation: ", 
          detail = "Deleting obsolete output data", value = 1)         
        save (FVS_Runs,file="FVS_Runs.RData")
        removeFVSRunFiles(fvsRun$uuid)
        deleteRelatedDBRows(fvsRun$uuid,dbcon)
        progress$set(message = "Run preparation: ", 
          detail = "Write .key file and load program", value = 2)         
        writeKeyFile(fvsRun,globals$inData$FVS_StandInit,prms)
         dir.create(fvsRun$uuid)
        fvschild = makePSOCKcluster(1)
        rtn = try(clusterEvalQ(fvschild,
              for (rf in dir("rFVS/R")) source(paste0("rFVS/R/",rf))))
        if (class(rtn) == "try-error") return()
        rtn = try(eval(parse(text=paste0("clusterEvalQ(fvschild,fvsLoad('",
             fvsRun$FVSpgm,"',bin='./FVSbin'))"))) )
        if (class(rtn) == "try-error") return()          
        # if not using the default run script, load the one requested.
        if (fvsRun$runScript != "fvsRun")
        {
          rtn = try(eval(parse(text=paste0("clusterEvalQ(fvschild,",
               "source('customRun_",fvsRun$runScript,".R'))"))))
          if (class(rtn) == "try-error") return()
          runOps <<- if (is.null(fvsRun$uiCustomRunOps)) list() else 
            fvsRun$uiCustomRunOps
          rtn = try(clusterExport(fvschild,list("runOps"))) 
          if (class(rtn) == "try-error") return()
        }
        rtn = try(eval(parse(text=paste0("clusterEvalQ(fvschild,",
              'fvsSetCmdLine("--keywordfile=',fvsRun$uuid,'.key"))')))) 
        if (class(rtn) == "try-error") return()
        on.exit(progress$close()) #on exit of the reactive context
cat ("at for start\n")          
        allSum = list()
        for (i in 1:length(fvsRun$stands))
        {
          detail = paste0("Stand ",i," StandId=",fvsRun$stands[[i]][["sid"]])          
          progress$set(message = "FVS running", detail = detail, value = i+2) 
          rtn = if (fvsRun$runScript != "fvsRun")
             try(eval(parse(text=paste0("clusterEvalQ(fvschild,",
                            fvsRun$runScript,"(runOps))")))) else
             try(clusterEvalQ(fvschild,fvsRun()))
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
        output$uiErrorScan <- renderUI(list(
          h5("FVS output error scan"),
          tags$style(type="text/css", paste0("#errorScan { overflow:auto; ",
             "height:150px; font-family:monospace; font-size:90%;}")),
          HTML(paste(errorScan(paste0(fvsRun$uuid,".out")),"<br>"))))
        if (length(dir(fvsRun$uuid)) == 0) file.remove(fvsRun$uuid)
        progress$set(message = if (length(allSum) == length(fvsRun$stands))
                    "FVS finished" else
                    "FVS run failed", detail = "", 
                    value = length(fvsRun$stands)+4)
        Sys.sleep(.4)
cat ("length(allSum)=",length(allSum),"\n")
        if (length(allSum) == 0) return()

        X = vector("numeric",0)
        hfacet = vector("character",0)
        Y = vector("numeric",0)
        Attribute=vector("character",0)
        progress$set(message = "Building plot", detail = "", 
                     value = length(fvsRun$stands)+5)                             
        for (i in 1:length(allSum)) 
        {
          X = c(X,rep(allSum[[i]][,"Year"],2))
          hfacet = c(hfacet,rep(names(allSum)[i],nrow(allSum[[i]])*2))
          Y = c(Y,c(allSum[[i]][,"TCuFt"],allSum[[i]][,"TPrdTCuFt"]))
          Attribute=c(Attribute,c(rep("TCuFt",nrow(allSum[[i]])),
                                  rep("TPrdTCuFt",nrow(allSum[[i]]))))
        }
        toplot = data.frame(X = X, hfacet=as.factor(hfacet), Y=Y, 
                  Attribute=as.factor(Attribute))
        width = max(4,nlevels(toplot$hfacet)*2)
        height = 2.5
        plt = if (nlevels(toplot$hfacet) < 5)
          ggplot(data = toplot) + facet_grid(.~hfacet) + 
            geom_line (aes(x=X,y=Y,color=Attribute,linetype=Attribute)) +
            labs(x="Year", y="Total cubic volume per acre") + 
            theme(text = element_text(size=9), legend.position="none",
                  panel.background = element_rect(fill="gray95"),
                  axis.text = element_text(color="black"))  else
        {
          width = 3
          toplot$Attribute = as.factor(paste0(toplot$Attribute,toplot$hfacet))
          ggplot(data = toplot) +  
            geom_line (aes(x=X,y=Y,color=Attribute)) + 
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
      }
    })
  })


  ## Upload
  observe({  
    if (is.null(input$upload)) return()
    if (file.exists("FVS_Data.db"))
    {
      if (file.exists("FVS_Data.db.backup")) file.remove("FVS_Data.db.backup")
      file.rename("FVS_Data.db","FVS_Data.db.backup")
    }
    if (regexpr("\\.db$",input$upload$name) > 1) 
    {
      file.copy(input$upload$datapath,"FVS_Data.db",overwrite = TRUE)
    } else
    {   
      Sys.sleep (.1)        
      withProgress(session, 
      {  
        setProgress(message = "Create schema", value = 1) 
        curDir=getwd()
        setwd(dirname(input$upload$datapath))
        system (paste0("java -jar ",curDir,"/access2csv.jar ",
                input$upload$datapath," --schema > schema"))
        setProgress(message = "Process schema", value = 2) 
        schema = scan("schema",what="character",sep="\n",quiet=TRUE)
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
        system (paste0("java -jar ",curDir,"/access2csv.jar ",
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
        setProgress(message = "Done", value = 10) 
        lapply(schema,unlink) 
        file.copy("FVS_Data.db",paste0(curDir),overwrite=TRUE)
        unlink("schema") 
        Sys.sleep (.5) 
        setwd(curDir)      
        setProgress(value = NULL)  
      }, min=1, max=12)
    }
    unlink(input$upload$datapath)
    fixFVSKeywords() 
    output$reload<-renderUI(tags$script("location.reload();"))
  }) 

  ## recoverdb
  observe({  
    if (input$recoverdb == 0) return()
    if (file.exists("FVS_Data.db")) file.remove("FVS_Data.db")
    if (file.exists("FVS_Data.db.backup")) 
        file.rename("FVS_Data.db.backup","FVS_Data.db") else
        file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
  }) 


  ## Download handlers
  output$dlFVSDatadb <- downloadHandler(filename="FVS_Data.db",
       content = function (tf = tempfile()) file.copy("FVS_Data.db",tf))
  output$dlFVSOutdb <- downloadHandler(filename="FVSOut.db",
       content = function (tf = tempfile()) file.copy("FVSOut.db",tf))
  output$dlFVSRunout <- downloadHandler(filename=
       paste0(if(nchar(fvsRun$title)) fvsRun$title else "fvsRun",".out"),
       content = 
         {
           isolate ({
             function (tf = tempfile()) 
               file.copy(paste0(input$runSel,".out"),tf)
           })
         })
  output$dlFVSRunkey <- downloadHandler(filename=
       paste0(if(nchar(fvsRun$title)) fvsRun$title else "fvsRun",".key"),
       content = 
         {
           isolate ({
             function (tf = tempfile()) 
               file.copy(paste0(input$runSel,".key"),tf)
           })
         })
  output$dlFVSRunZip <- downloadHandler(filename="FVSData.zip",
       content = function (tf = tempfile)
         {
           isolate ({
             thelist = sub("current",input$runSel,input$dlZipSet)
             thelist = thelist[unlist(lapply(thelist,file.exists))]
             if (length(thelist)) 
             { 
               zip(tf,thelist)
               file.rename(paste0(tf,".zip"),tf)
             }
           })
         })


  ## cpReload
  observe({  
    if (input$rightPan == "Build Components" || input$kcpReload > 0)
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
      # pgmList is defined in settings.R, if it is not found in one place, 
      # look in the current directory. 
      if (file.exists("../../FVSOnline/settings.R")) 
               source("../../FVSOnline/settings.R") else if 
             (file.exists("localSettings.R")) source("localSettings.R")
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
      if (file.exists("../../FVSOnline/settings.R")) 
               source("../../FVSOnline/settings.R") else if
          (file.exists("localSettings.R")) source("localSettings.R")
      if (!exists("fvsBinDir")) fvsBinDir="~/open-fvs/branches/DBSrefactor/bin/"
      if (!file.exists(fvsBinDir))
      {
        session$sendCustomMessage(type="infomessage",
          message="FVS programs can not be refreshed on this system.")
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
        if (i) output$reload<-renderUI(tags$script("location.reload();"))
      } 
    })
  })

  ## deleteRun  
  observe({
    if(input$deleteRun > 0)
    {
      if (is.null(globals$FVS_Runs[[input$runSel]])) return()
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
        if (length(globals$FVS_Runs) == 0 || 
            is.null(globals$FVS_Runs[[input$runSel]])) return() 
        globals$FVS_Runs[[input$runSel]] = NULL
        removeFVSRunFiles(input$runSel)
        deleteRelatedDBRows(input$runSel,dbcon)
        resetfvsRun(fvsRun,globals$FVS_Runs)
        if (length(globals$FVS_Runs) == 0)
        {
          updateSelectInput(session=session, inputId="runSel", 
            choices=NULL,selected=NULL)
        } else
        {
          selChoices = names(globals$FVS_Runs) 
          names(selChoices) = unlist(lapply(globals$FVS_Runs,
                                        function (x) x$title))
          updateSelectInput(session=session, inputId="runSel", 
              choices=selChoices,selected=selChoices[[1]])
        }
        resetGlobals(globals,NULL,prms)
        loadFromList(fvsRun,globals$FVS_Runs[[1]])
        mkSimCnts(fvsRun,fvsRun$selsim)        
        isolate({
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
        })
        updateTextInput(session=session, inputId="title", value=fvsRun$title)
        updateTextInput(session=session, inputId="defMgmtID",
                      value=fvsRun$defMgmtID)
        updateSelectInput(session=session, inputId="simCont", 
          choices=fvsRun$simcnts, selected=fvsRun$selsim)
        updateTextInput(session=session, inputId="startyr",  
                        value=fvsRun$startyr)
        updateTextInput(session=session, inputId="endyr",    
                        value=fvsRun$endyr)
        updateTextInput(session=session, inputId="cyclelen", 
                        value=fvsRun$cyclelen)
        updateTextInput(session=session, inputId="cycleat",  
                        value=fvsRun$cycleat)
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
      file.remove("FVSOut.db")
      file.remove("FVS_Runs.RData")
      rmfiles=dir(pattern="[.]key$")      
      rmfiles=gsub ("[.]key$","*",rmfiles)
      if (length(rmfiles)) unlink(rmfiles,recursive=TRUE)
      rmfiles=dir(pattern="[.]out$")      
      rmfiles=gsub ("[.]out$","*",rmfiles)
      if (length(rmfiles)) unlink(rmfiles,recursive=TRUE)
      globals$saveOnExit = FALSE
      output$reload<-renderUI(tags$script("location.reload();"))
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
    output$reload<-renderUI(tags$script("location.reload();"))
  }) 
  
  ## restoreYesterday 
  observe({
    if(input$interfaceRefresh > 0)
    {
      session$sendCustomMessage(type = "dialogContentUpdate",
        message = list(id = "restoreYesterdayDlg",
                  message = "Are you sure?"))
    }
  })
  observe({  
    if (input$restoreYesterdayDlgBtn == 0) return()
cat ("restoreYesterdayDlgBtn\n") 
    if (file.exists("../../FVSOnline/settings.R")) 
             source("../../FVSOnline/settings.R") 
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
    output$reload<-renderUI(tags$script("location.reload();"))
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
  
  #dataEditor
  observe(if (input$launchDataEditor > 0) 
  {
cat ("dataEditor\n")
    file.copy("server.R","fvsOnlineServer.R", overwrite=TRUE)
    file.copy("ui.R",    "fvsOnlineUI.R",     overwrite=TRUE)
    file.copy("editDataServer.R","server.R",  overwrite=TRUE)
    file.copy("editDataUI.R",    "ui.R",      overwrite=TRUE)   
    output$reload<-renderUI(tags$script("location.reload();"))
  })


  #runScript selection
  observe({
    if (length(input$runScript)) customRunOps()
  })

  customRunOps <- function ()
  {
    isolate({
cat ("in customRunOps\n")    
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

