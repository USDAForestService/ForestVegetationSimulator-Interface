library(shiny)
library(rhandsontable)
library(ggplot2)
library(parallel)
library(RSQLite)
library(plyr)
library(colourpicker)
library(rgl)
library(leaflet)
#library(rgdal) #loaded when it is needed
library(openxlsx)              

# set shiny.trace=T for reactive tracing (lots of output)
options(shiny.maxRequestSize=1000*1024^2,shiny.trace = FALSE,
        rgl.inShiny=TRUE) 

shinyServer(function(input, output, session) {

  if (!interactive()) 
  {
    if (file.exists("FVSOnline.log")) 
    {
      unlink("FVSOnline.older.log")
      file.rename("FVSOnline.log","FVSOnline.older.log")
    }
    #make sure the sink stack is empty
    while (sink.number()) sink()
    sink("FVSOnline.log")
  }
cat ("FVSOnline/OnLocal interface server start.\n")
       
  withProgress(session, {  
    setProgress(message = "Start up", 
                detail  = "Loading scripts and settings", value = 1)
 
    source("fvsRunUtilities.R",local=TRUE)
    source("fvsOutUtilities.R",local=TRUE)
    source("componentWins.R",local=TRUE)
    source("mkInputElements.R",local=TRUE)
    source("editDataUtilities.R",local=TRUE)
    
    isLocal <- function () Sys.getenv('SHINY_PORT') == ""

    if (file.exists("localSettings.R")) source("localSettings.R",local=TRUE) 
    if (!isLocal() && file.exists("../../FVSOnline/settings.R")) source("../../FVSOnline/settings.R",local=TRUE)
    # cbbPalette is used in the graphics
    cbbPalette <- c("#D55E00", "#56B4E9", "#009E73", "#0072B2", "#E69F00", "#CC79A7")   
    load("prms.RData") 
    globals <- mkGlobals(saveOnExit=TRUE,reloadAppIsSet=0)
    dbGlb <- new.env()
    dbGlb$tbl <- NULL
    dbGlb$navsOn <- FALSE            
    dbGlb$rowSelOn <- FALSE
    dbGlb$disprows <- 20
    resetGlobals(globals,NULL,prms)
    setProgress(message = "Start up",value = 2)
    globals$fvsRun <- mkfvsRun()
    if (!file.exists("FVS_Runs.RData"))
    {
      resetfvsRun(globals$fvsRun,globals$FVS_Runs)
      globals$FVS_Runs[[globals$fvsRun$uuid]] = globals$fvsRun$title
      attr(globals$FVS_Runs[[globals$fvsRun$uuid]],"time") = as.integer(Sys.time())
      saveFvsRun = globals$fvsRun
      save(file=paste0(globals$fvsRun$uuid,".RData"),saveFvsRun)
      FVS_Runs = globals$FVS_Runs
      save (file="FVS_Runs.RData",FVS_Runs)
    }
    if (file.exists("FVS_Runs.RData"))
    {
      load("FVS_Runs.RData")
      # make sure there are .RData files for each of these and if not, then
      # delete them from the list of runs. This code does some testing in hopes
      # of clearing up some startup-after failure problems.
      notok = c()
      if (length(FVS_Runs)) for (i in 1:length(FVS_Runs))
      {
        rn = names(FVS_Runs)[i]
        run = FVS_Runs[[i]]
        ok =  !is.null(rn) && !is.null(run) && nchar(rn) && nchar(run) && rn != run && 
              !is.null(attributes(run)$time) && file.exists(paste0(rn,".RData"))
        if (!ok) notok = c(notok,i)
      }
      if (!is.null(attr(FVS_Runs,"stdstkParms")))
      {
        val = as.numeric(attr(FVS_Runs,"stdstkParms")$sdskwdbh)
        if (!is.na(val)) updateNumericInput(session=session,inputId="sdskwdbh",
           value=val)
        val = as.numeric(attr(FVS_Runs,"stdstkParms")$sdskldbh)
        if (!is.na(val)) updateNumericInput(session=session, inputId="sdskldbh",
           value=val)
      }
cat ("length(FVS_Runs)=",length(FVS_Runs)," length(notok)=",length(notok)," notok=",notok,"\n")
      if (length(FVS_Runs) == length(notok))
      {
        unlink("FVS_Runs.RData")
        resetfvsRun(globals$fvsRun,globals$FVS_Runs)
        globals$FVS_Runs[[globals$fvsRun$uuid]] = globals$fvsRun$title
      } else if (length(notok)) FVS_Runs = FVS_Runs[-notok] 
      globals$FVS_Runs = FVS_Runs
      rm (FVS_Runs)      
    } else {
cat ("serious start up error\n") 
      return()
    }
    setProgress(message = "Start up",
                detail  = "Loading interface elements", value = 3)
    tit=NULL
    if (!file.exists("projectId.txt"))
      cat("title= ",basename(getwd()),"\n",file="projectId.txt")
    prjid = scan("projectId.txt",what="",sep="\n",quiet=TRUE)
    tit=prjid[grep("^title",prjid)]
    tit=trim(unlist(strsplit(tit,split="=",fixed=TRUE))[2])
    email=prjid[grep("^email",prjid)]
    email=trim(unlist(strsplit(email,split="=",fixed=TRUE))[2])
    tstring = paste0("Project title: <b>",tit,"</b>",
           if (length(email)) paste0("<br>Email: <b>",email,"</b>") else "",
           "<br>Last accessed: <b>",
           format(file.info(getwd())[1,"mtime"],"%a %b %d %H:%M:%S %Y"),"</b>")
    cat ("tstring=",tstring,"\n")    
    output$projectTitle = renderText(HTML(paste0("<p>",tstring,"<p/>")))            
    updateTextInput(session=session, inputId="rpTitle", 
      value=paste0("Custom report",if (length(tit)) 
            paste0(" for project: ",tit) else "")) 
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
    if (!file.exists("FVS_Data.db")) 
      file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
    
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
    loadVarData(globals,prms,dbGlb$dbIcon)                                              
    setProgress(value = NULL)          
  }, min=1, max=6)
  
  observe({
cat ("protocol: ", session$clientData$url_protocol, "\n",
     "hostname: ", session$clientData$url_hostname, "\n",
     "pathname: ", session$clientData$url_pathname, "\n",
     "port: ",     session$clientData$url_port,     "\n",
     "search: ",   session$clientData$url_search,   "\n")
    globals$hostname = session$clientData$url_hostname
cat("signalClosing=",input$signalClosing,"\n")
    if (!is.null(input$signalClosing) && input$signalClosing==1 &&
        globals$reloadAppIsSet == 0 && globals$hostname == "127.0.0.1") 
    {
cat ("sending closeWindow\n")
      session$sendCustomMessage(type = "closeWindow"," ")
    }
  })
  
  session$onSessionEnded(function ()
  { 
cat ("onSessionEnded, globals$saveOnExit=",globals$saveOnExit,
     " interactive()=",interactive(),"\n",
     "globals$reloadAppIsSet=",globals$reloadAppIsSet,
     " globals$hostname=",globals$hostname,"\n")
    if (exists("dbOcon",envir=dbGlb,inherit=FALSE)) try(dbDisconnect(dbGlb$dbOcon))
    if (exists("dbIcon",envir=dbGlb,inherit=FALSE)) try(dbDisconnect(dbGlb$dbIcon))
    if (globals$saveOnExit) 
    {
      saveRun()
      FVS_Runs = globals$FVS_Runs
      attr(FVS_Runs,"stdstkParms") = isolate(list("sdskwdbh"=input$sdskwdbh,
                                                  "sdskldbh"=input$sdskldbh))  
      save (file="FVS_Runs.RData",FVS_Runs)
      if (file.exists("projectId.txt"))
      {
        prjid = scan("projectId.txt",what="",sep="\n",quiet=TRUE)
        write(file="projectId.txt",prjid)
      }
    }
    if (globals$reloadAppIsSet == 0 && globals$hostname == "127.0.0.1")
    {
      stopApp()
    } 
    globals$reloadAppIsSet == 0
    if (isLocal()){
      file.copy(paste0("C:/FVSOnlocal/",basename(getwd()),"/projectId.txt"),
                "C:/FVSOnlocal/lastAccessedProject.txt",overwrite=TRUE)
    }
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

cat ("getwd= ",getwd(),"\n")
  
  ## Load
  observe({
    if (input$topPan == "View Outputs" && input$leftPan == "Load")
    {
cat ("View Outputs & Load\n")
      initTableGraphTools()
      tbs <- dbGetQuery(dbGlb$dbOcon,"select name from sqlite_master where type='table';")[,1]      
      if (length(tbs) > 0 && !is.na(match("FVS_Cases",tbs)))
      {
        runsdf = dbGetQuery(dbGlb$dbOcon,
          paste0("Select RunTitle,KeywordFile from FVS_Cases group by KeywordFile ",
                 "having min(RunDateTime) order by RunDateTime desc;"))
        fvsOutData$runs = runsdf$KeywordFile
        names(fvsOutData$runs) = runsdf$RunTitle
      }
      updateSelectInput(session, "runs", choices = fvsOutData$runs, 
        selected=0)
    }
  })

  ## output run selection
  observe({
    if (input$leftPan != "Load") return()
cat ("runs, run selection (load) input$runs=",input$runs,"\n")
    if (!is.null(input$runs)) # will be a list of run keywordfile names (uuid's)
    {
      tbs <- dbGetQuery(dbGlb$dbOcon,"select name from sqlite_master where type='table';")[,1]
cat ("runs, tbs=",tbs,"\n")
      if (length(tbs) == 0) 
      {
        updateSelectInput(session, "selectdbtables", choices=list())
        return()
      }
      withProgress(session, {  
        i = 1
        setProgress(message = "Output query", 
                    detail  = "Selecting tables", value = i); i = i+1
        # set an exclusive lock on the database
        dbExecute(dbGlb$dbOcon,"PRAGMA locking_mode = EXCLUSIVE")
        trycnt=0
        while (TRUE)
        {
          trycnt=trycnt+1
          setProgress(message = "Getting exclusive lock", 
                      detail  = paste0("Number of attempts=",trycnt," of 1000"))
          if (trycnt > 1000) 
          {
            dbExecute(dbGlb$dbOcon,"PRAGMA locking_mode = NORMAL")
            setProgress(value = NULL)
            return()
          }
cat ("try to get exclusive lock, trycnt=",trycnt,"\n");
          rtn <- try(dbExecute(dbGlb$dbOcon,"create table dummy (dummy int)"))
          if (class(rtn) != "try-error") break;
          Sys.sleep (10)
        } 
        dbExecute(dbGlb$dbOcon,"drop table if exists dummy")
        # create a temp.Cases table that is a list of CaseIDs 
        # associated with the selected runs. These two items are used to 
        # filter records selected from selected tables.
        dbExecute(dbGlb$dbOcon,"drop table if exists temp.Cases")
        inSet=paste0("('",paste(input$runs,collapse="','"),"')")
        dbExecute(dbGlb$dbOcon,paste0("create table temp.Cases as select CaseID ",
                     "from FVS_Cases where FVS_Cases.KeywordFile in ",inSet)) 
        for (tb in tbs) 
        {
cat ("tb=",tb,"\n")
          cnt = 0
          if (tb == "FVS_Cases") next
          if (tb %in% c("CmpSummary","CmpSummary_East",
              "CmpSummary2","CmpSummary2_East","StdStk","CmpStdStk",
              "StdStk_East","CmpStdStk_East","CmpMetaData","CmpCompute"))
              dbExecute(dbGlb$dbOcon,paste0("drop table ",tb))
          else 
          {
            cnt = if ("CaseID" %in% dbListFields(dbGlb$dbOcon,tb))  
              dbGetQuery(dbGlb$dbOcon,paste0("select count(*) from ",
                   "(select distinct CaseID from ",tb," where CaseID in ",
                   "(select CaseID from temp.Cases))")) else 1
          }
          if (cnt == 0) tbs = setdiff(tbs,tb)
        }
        source("sqlQueries.R")
        dbExecute(dbGlb$dbOcon, "drop table if exists CmpMetaData;")
        ncases = dbGetQuery(dbGlb$dbOcon, "select count(*) from temp.Cases;")[1,1]
        if (ncases > 1) exqury(dbGlb$dbOcon,Create_CmpMetaData)
        isolate(dbhclassexp <- mkdbhCase(input$sdskwdbh,input$sdskldbh))
        input$bldstdsk # force this section to be reactive to this input     
        if ("FVS_Summary" %in% tbs && ncases > 1)
        {
          setProgress(message = "Output query", 
            detail  = "Building CmpSummary", value = i); i = i+1
          exqury(dbGlb$dbOcon,Create_CmpSummary)
          tbs = c(tbs,"CmpSummary")
cat ("tbs1=",tbs,"\n")
        }
        if ("FVS_Summary_East" %in% tbs && ncases > 1)
        {
          setProgress(message = "Output query", 
            detail  = "Building CmpSummary_East", value = i); i = i+1
          exqury(dbGlb$dbOcon,Create_CmpSummary_East)
          tbs = c(tbs,"CmpSummary_East")
cat ("tbs2=",tbs,"\n")
        }
        if ("FVS_Summary2" %in% tbs && ncases > 1)
        {
          setProgress(message = "Output query", 
            detail  = "Building CmpSummary2", value = i); i = i+1
          exqury(dbGlb$dbOcon,Create_CmpSummary2)
          tbs = c(tbs,"CmpSummary2")
cat ("tbs1=",tbs,"\n")
        }
        if ("FVS_Summary2_East" %in% tbs && ncases > 1)
        {
          setProgress(message = "Output query", 
            detail  = "Building CmpSummary2_East", value = i); i = i+1
          exqury(dbGlb$dbOcon,Create_CmpSummary2_East)
          tbs = c(tbs,"CmpSummary2_East")
cat ("tbs2=",tbs,"\n")
        }
        if ("FVS_Compute" %in% tbs && ncases > 1)
        {
          setProgress(message = "Output query", 
            detail  = "Building CmpCompute", value = i); i = i+1
          cmp = dbGetQuery(dbGlb$dbOcon,
            "select * from FVS_Compute limit 0")
          sumExpressions = paste0(
            lapply(setdiff(colnames(cmp),c("CaseID","StandID","Year")),
              function (var) paste0("round(sum(",var,
                "*SamplingWT)/sum(SamplingWt),2) as Cmp",var)),collapse=",")
          exqury(dbGlb$dbOcon,Create_CmpCompute,subExpression=sumExpressions)
          cmp = dbGetQuery(dbGlb$dbOcon,"Select * from CmpCompute;")
          keep = apply(cmp,2,function (x) !(all(is.na(x))))
          if (!all(keep)) 
          {
            cmp = cmp[,keep]
            dbWriteTable(dbGlb$dbOcon,"CmpCompute",cmp,overwrite=TRUE)
          }
          tbs = c(tbs,"CmpCompute")    
cat ("tbs3=",tbs,"\n")
        }
        tlprocs = c("tlwest"="FVS_TreeList" %in% tbs, "tleast"="FVS_TreeList_East" %in% tbs)
        tlprocs = names(tlprocs)[tlprocs]
        chtoEast = function(cmd)
        {
          cmd = gsub("BdFt", "SBdFt",cmd,fixed=TRUE)
          cmd = gsub("TCuFt","SCuFt",cmd,fixed=TRUE)
          cmd = gsub("FVS_TreeList","FVS_TreeList_East",cmd,fixed=TRUE)
          gsub("FVS_CutList","FVS_CutList_East",cmd,fixed=TRUE)
        }
        for (tlp in tlprocs)          
        {
          if (tlp == "tlwest")
          {
            C_StdStkDBHSp  = Create_StdStkDBHSp
            C_HrvStdStk    = Create_HrvStdStk
            C_StdStk1Hrv   = Create_StdStk1Hrv
            C_StdStk1NoHrv = Create_StdStk1NoHrv
            C_StdStkFinal  = Create_StdStkFinal
            C_CmpStdStk    = Create_CmpStdStk
            detail = "Building StdStk from tree lists"
            stdstk = "StdStk"
            clname = "FVS_CutList"
          } else {
            C_StdStkDBHSp  = chtoEast(Create_StdStkDBHSp )
            C_HrvStdStk    = chtoEast(Create_HrvStdStk   )
            C_StdStk1Hrv   = chtoEast(Create_StdStk1Hrv  )
            C_StdStk1NoHrv = chtoEast(Create_StdStk1NoHrv)
            C_StdStkFinal  = chtoEast(Create_StdStkFinal )
            C_StdStkFinal  = gsub(" StdStk"," StdStk_East",C_StdStkFinal)
            C_CmpStdStk    = chtoEast(Create_CmpStdStk   )
            C_CmpStdStk    = gsub(" CmpStdStk"," CmpStdStk_East",C_CmpStdStk)
            C_CmpStdStk    = gsub(" StdStk "," StdStk_East ",C_CmpStdStk)
            detail = "Building StdStk_East from tree lists"
            stdstk = "StdStk_East"
            clname = "FVS_CutList_East"
          }
          setProgress(message = "Output query", 
            detail  = detail, value = i); i = i+1
          exqury(dbGlb$dbOcon,C_StdStkDBHSp,subExpression=dbhclassexp)
          if (clname %in% tbs)
          {
            setProgress(message = "Output query", 
              detail  = detail, value = i); i = i+1
            exqury(dbGlb$dbOcon,C_HrvStdStk,subExpression=dbhclassexp)
            setProgress(message = "Output query", 
              detail  = "Joining tables", value = i); i = i+1
            exqury(dbGlb$dbOcon,C_StdStk1Hrv,subExpression=dbhclassexp)
          } else {
             setProgress(message = "Output query", 
              detail  = "Joining tables", value = i); i = i+2
            exqury(dbGlb$dbOcon,C_StdStk1NoHrv,subExpression=dbhclassexp)
          }
          exqury(dbGlb$dbOcon,C_StdStkFinal)
          tbs = c(tbs,stdstk) 
          if (ncases > 1) 
          {
            exqury(dbGlb$dbOcon,C_CmpStdStk)
            tbs = c(tbs,paste0("Cmp",stdstk))
          }
        }
        if (all(Create_View_DWN_Required %in% tbs)) 
        {
          exqury(dbGlb$dbOcon,Create_View_DWN)
          tbs = c(tbs,"View_DWN")
        }
        dbExecute(dbGlb$dbOcon,"PRAGMA locking_mode = NORMAL")
cat ("tbs4=",tbs,"\n")       
        setProgress(message = "Output query", 
            detail  = "Committing changes", value = i); i = i+1
        dbd = lapply(tbs,function(tb,con) dbListFields(con,tb), dbGlb$dbOcon)
        names(dbd) = tbs
        if (!is.null(dbd[["FVS_Summary"]])) dbd$FVS_Summary = c(dbd$FVS_Summary,
            c("TPrdTpa","TPrdTCuFt","TPrdMCuFt","TPrdBdFt"))
        if (!is.null(dbd[["FVS_Summary_East"]])) dbd$FVS_Summary_East = 
            c(dbd$FVS_Summary_East,c("TPrdTpa","TPrdMCuFt","TPrdSCuFt","TPrdSBdFt"))
        if (!is.null(dbd[["CmpSummary"]])) dbd$CmpSummary = c(dbd$CmpSummary,
            c("CmpTPrdTpa","CmpTPrdTCuFt","CmpTPrdMCuFt","CmpTPrdBdFt"))
        if (!is.null(dbd[["CmpSummary_East"]])) dbd$CmpSummary = c(dbd$CmpSummary_East,
            c("CmpTPrdTpa","CmpTPrdTCuFt","CmpTPrdMCuFt","CmpTPrdBdFt"))
          
        if (length(dbd)) fvsOutData$dbLoadData <- dbd
        tbs = sort(tbs)
        sel = intersect(tbs, c("FVS_Summary2","FVS_Summary2_East")) #not both!
        if (length(sel)==0) sel = intersect(tbs, c("FVS_Summary","FVS_Summary_East")) #not both!
        if (length(sel)>1) sel = sel[1]
        updateSelectInput(session, "selectdbtables", choices=as.list(tbs),
                          selected=sel)
        setProgress(value = NULL) 
      }, min=1, max=6)
    } else
    {
      updateSelectInput(session, "selectdbtables", choices=list())
    }
  })
    
  # selectdbtables
  observe({
cat("selectdbtables\n")    
    if (is.null(input$selectdbtables))
    {
      updateSelectInput(session, "selectdbvars", choices=list())               
    } else  {
      tables = input$selectdbtables 
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
        # remove the /* */ comments and newline chars
        qrys = trim(gsub("\n"," ",gsub("/\\*.*\\*/"," ",input$sqlQuery)))
        qrys = scan(text=qrys,sep=";",what="",quote="",quiet=TRUE)
        output$table <- renderTable(NULL)        
        iq = 0
        dfrtn = NULL
        for (qry in qrys) 
        {
          if (nchar(qry))
          {
            iq = iq+1
cat ("sqlRunQuery, qry=",qry,"\n")
            res = try (dbGetQuery(dbGlb$dbOcon,qry))
            msgtxt = if (class(res) == "data.frame" && ncol(res) && nrow(res)) 
              paste0(msgtxt,"query ",iq," returned a data frame with ",nrow(res),
                    " rows and ",ncol(res)," cols\n") else  
              if (class(res) == "try-error") paste0(msgtxt,"query ",iq,
                " returned\n",attr(res,"condition"),"\n") else
                paste0(msgtxt,"query ",iq," ran\n")         
            updateTextInput(session=session, inputId="sqlOutput", label="", 
                            value=msgtxt)                          
            if (class(res) == "try-error") break
            if (class(res) == "data.frame" && ncol(res) && nrow(res))
            {
              for (col in 1:ncol(res)) if (class(res[[col]]) == "character") 
                res[[col]] = factor(res[[col]],unique(res[[col]]))
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
             selected = match(newTit,names(globals$customQueries)))
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
        tbgroup=c("CmpMetaData"="0","CmpSummary"=1, "CmpSummary_East"=1, 
          "CmpSummary2"=1, "CmpSummary2_East"=1,
          "CmpCompute"=1, "CmpStdStk"=1, "StdStk"=3, 
          "CmpStdStk_East"=1, "StdStk_East"=3, "FVS_ATRTList"=8,
          "FVS_Cases"=2, "FVS_Climate"=4, "FVS_Compute"=2, "FVS_CutList"=8,
          "FVS_EconHarvestValue"=2, "FVS_EconSummary"=2, "FVS_BurnReport"=2,
          "FVS_CanProfile"=5, "FVS_Carbon"=2, "FVS_SnagDet"=6, "FVS_Down_Wood_Cov"=2,
          "FVS_Down_Wood_Vol"=2, "FVS_Consumption"=2, "FVS_Hrv_Carbon"=2,
          "FVS_Mortality"=2, "FVS_PotFire_East"=2, "FVS_PotFire"=2, "FVS_SnagSum"=2,
          "FVS_Fuels"=2, "FVS_DM_Spp_Sum"=7, "FVS_DM_Stnd_Sum"=2, "FVS_DM_Sz_Sum"=2,
          "FVS_RD_Sum"=2, "FVS_RD_Det"=2, "FVS_RD_Beetle"=2, "FVS_StrClass"=2,
          "FVS_Summary_East"=2, "FVS_Summary"=2, "FVS_TreeList"=8,"FVS_ATRTList"=8,
          "FVS_CutList"=8,"FVS_TreeList_East"=8,"FVS_ATRTList_East"=8,
          "FVS_CutList_East"=8)
        tbg = tbgroup[tbs]
        arena = is.na(tbg)
        if (any(arena))
        {
          tbg[arena] = 3
          names(tbg)[arena] = tbs[arena]
        }
        if (max(tbg) > 1 && ! ("FVS_Cases" %in% tbs)) tbg = c("FVS_Cases"=2,tbg)
        dat=NULL
        for (tb in names(sort(tbg)))       
        {
cat ("tb=",tb," len(dat)=",length(dat),"\n")
          iprg = iprg+1
          setProgress(message = "Processing tables", detail=tb,value = iprg)
          if (tb %in% c("CmpSummary","CmpSummary_East","CmpSummary2","CmpSummary2_East")) 
          {            
            dtab <- dbReadTable(dbGlb$dbOcon,tb)
            if (tb %in% c("CmpSummary","CmpSummary_East")) 
              dtab <- ddply(dtab,.(MgmtID),.fun=function (x) 
                      setupSummary(x,composite=TRUE)) else
              dtab$RmvCode <- as.factor(dtab$RmvCode)   
            dtab$Year <- as.factor(dtab$Year) 
            dtab$MgmtID <- as.factor(dtab$MgmtID) 
            dat[[tb]] <- dtab
          } else {
            dtab = if ("CaseID" %in% dbListFields(dbGlb$dbOcon,tb))
              dbGetQuery(dbGlb$dbOcon,paste0("select * from ",tb,
                   " where CaseID in (select CaseID from temp.Cases)")) else
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
            } else if (tb == "FVS_Summary2" || tb == "FVS_Summary2_East") 
            { 
              dtab$ForTyp =as.factor(dtab$ForTyp)
              dtab$SizeCls=as.factor(dtab$SizeCls)
              dtab$StkCls =as.factor(dtab$StkCls)
              dtab$RmvCode=as.factor(dtab$RmvCode) 
            } else if (tb == "FVS_Cases") dtab$RunTitle=trim(dtab$RunTitle)
            cls = intersect(c(cols,"StandID","MgmtID","RunTitle","srtOrd"),colnames(dtab))
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
        if (length(dat) == 0)
        {
          initTableGraphTools()
          return()
        }
        iprg = iprg+1
        setProgress(message = "Merging selected tables", detail  = "", value = iprg)
        inch = 0
        mdat = NULL
        for (tb in names(dat))      
        {
          #avoid name conflicts with the TreeList table and others.
          if (tb %in% c("FVS_TreeList","FVS_ATRTList","FVS_CutList",
                   "FVS_TreeList_East","FVS_ATRTList_East","FVS_CutList_East"))
          {
            toren = c("TCuFt","MCuFt","BdFt","PrdLen","SCuFt","SBdFt")
            cols = match(toren,names(dat[[tb]]))
            names(dat[[tb]])[cols] = paste0(if (inch==0) "T." else paste0("T",inch,".",toren))
            inch = inch+1
          }
cat ("tb=",tb," is.null(mdat)=",is.null(mdat),"\n") 
          if (is.null(mdat)) mdat = dat[[tb]] else
          {
             mrgVars = intersect(names(mdat),c("CaseID","Year","StandID","MgmtID"))
             mrgVars = intersect(mrgVars,names(dat[[tb]]))
             setProgress(message = "Merging selected tables", 
                         detail  = tb, value = iprg)
cat ("tb=",tb," mrgVars=",mrgVars,"\n")
             mdat = merge(mdat,dat[[tb]], by=mrgVars)
          }
          fvsOutData$dbData = mdat
        } 
        iprg = iprg+1                                      
        setProgress(message = "Processing variables", detail=tb,value = iprg)
        mdat = fvsOutData$dbData
        vars = colnames(mdat)
        sby = intersect(vars,c("MgmtID","StandID","Stand_CN","Year","PtIndex",
           "TreeIndex","Species","DBHClass"))
        sby = if (length(sby)) 
        {
          cmd = paste0("order(",paste(paste0("mdat$",sby),collapse=","),
               if("srtOrd" %in% vars) ",mdat$srtOrd)" else ")")
cat ("cmd=",cmd,"\n")
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
        if (length(intersect(c("FVS_TreeList","FVS_ATRTList","FVS_CutList",
                "FVS_TreeList_East","FVS_ATRTList_East","FVS_CutList_East"),names(dat))))
          updateSelectInput(session, "plotType",selected="scat") else 
          if (length(intersect(c("StdStk","CmpStdStk","StdStk_East","CmpStdStk_East"),names(dat)))) 
            updateSelectInput(session, "plotType",selected="bar") else
              updateSelectInput(session, "plotType",selected="line")
        iprg = iprg+1
        setProgress(message = "Loading selection widgets", detail  = "", value = iprg)
        if (is.null(mdat$Year)) updateSelectInput(session, "year", 
            choices  = list("None loaded"), selected = NULL) else 
          {
            sel  = levels(mdat$Year)
            isel = max(1,length(sel) %/% 2)
            sel =  if (length(intersect(c("FVS_TreeList","FVS_ATRTList","FVS_CutList",
                "FVS_TreeList_East","FVS_ATRTList_East","FVS_CutList_East",
                "StdStk","StdStk_East","CmpStdStk","CmpStdStk_East"),names(dat)))) 
                sel[isel] else sel 
            updateSelectInput(session, "year", choices=as.list(levels(mdat$Year)), 
              selected=sel)
          }
        if (is.null(mdat$Species)) updateSelectInput(session, "species", 
            choices  = list("None loaded"), selected = NULL) else
            updateSelectInput(session, "species",
              choices=as.list(levels(mdat$Species)), selected=setdiff(mdat$Species,"All"))
        if (is.null(mdat$DBHClass)) updateSelectInput(session, "dbhclass", 
            choices  = list("None loaded"), selected = NULL) else
          {
            sel = if ("All" %in% levels(mdat$DBHClass)) "All" else mdat$DBHClass
            updateSelectInput(session, "dbhclass", 
              choices=as.list(levels(mdat$DBHClass)), selected=sel)
          }           
        iprg = iprg+1
        setProgress(message = "Finishing", detail  = "", value = iprg)
        selVars = unlist(lapply(c("StandID","MgmtID","Year","^DBH","^DG$",
          "AGE","CCF","SDI","QMD","TopHt","BA$","TPA","Species","^Ht$",
          "^HtG$","CuFt$","BdFt$","Total","HrvPA","RunTitle","Groups","^MY"),
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
      for (i in 1:ncol(dat)) 
        if (class(dat[[i]]) == "numeric") dat[[i]] = round(dat[[i]],3)
    }
    renderRHandsontable(if (is.null(dat) || nrow(dat)==0) NULL else 
              rhandsontable(dat,readOnly=TRUE,useTypes=FALSE,contextMenu=FALSE,
              width="100%",height=700))
  }
         
  observe({
    if (is.null(input$browsevars)) return()
cat("filterRows and/or pivot\n")
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
           
  
  ##browsevars/plotType 
  observe({
    if (!is.null(input$browsevars) && !is.null(input$plotType)) 
    {
cat ("browsevars/plotType\n")
      fvsOutData$browseSelVars <- input$browsevars  
      cats = unlist(lapply(fvsOutData$dbData,is.factor))
      cats = names(cats)[cats]
      cats = intersect(cats,input$browsevars)
      cont = union("Year",setdiff(input$browsevars,cats))   
      spiv  = if (length(input$pivVar) && 
                input$pivVar %in% cats) input$pivVar else "None"
      sdisp = if (length(input$dispVar) && 
                input$dispVar %in% input$browsevars) input$dispVar else "None"
      ccont = c("None",setdiff(input$browsevars,spiv))
      bb = intersect(ccont,cats) # put the factors at the end of the choices
      ccont = c(setdiff(ccont,bb),bb)
      updateSelectInput(session,"pivVar",choices=as.list(c("None",cats)),
                      selected=spiv)    
      updateSelectInput(session,"dispVar",choices=as.list(ccont),
                      selected=sdisp)       
      isolate({
        curX = input$xaxis
        curY = input$yaxis
        if (input$plotType=="line") {
          selx = if (is.null(curX)) "Year" else curX
          selx = if (selx %in% cont) selx else 
                 if (length(cont) > 0) cont[1] else NULL
          updateSelectInput(session, "xaxis",choices=as.list(cont), selected=selx)
          sel = if (is.null(curY)) "BA" else curY
          sel = if (sel %in% cont) sel else 
                if (length(cont) > 0) cont[1] else NULL
          if (sel == selx && length(cont) > 1) 
          {
            sel = grep("BA",cont)[1]
            sel = if (is.na(sel)) cont[2] else cont[sel]
          }
          updateSelectInput(session, "yaxis",choices=as.list(cont), selected=sel)
        } else if (input$plotType == "scat") {
          sel = if (is.null(curX)) "DBH" else curX
          sel = if (sel %in% cont) sel else 
                if (length(cont) > 0) cont[1] else NULL
          updateSelectInput(session, "xaxis",choices=as.list(cont), selected=sel)
          sel = if (is.null(curY)) "DG" else curY
          sel = if (sel %in% cont) sel else 
                if (length(cont) > 0) cont[1] else NULL
          updateSelectInput(session, "yaxis",choices=as.list(cont), selected=sel)
        } else if (input$plotType %in% c("box","bar")) {
          def = if ("Species" %in% cats) "Species" else NULL
          if (!is.null(def) && "Year" %in% cats) "Year" else cats[1]
          sel = if (!is.null(curX) && curX %in% cats) curX else def
          updateSelectInput(session, "xaxis",choices=as.list(cats), selected=sel)
          sel = if (!is.null(curX) && curX %in% cont) curX else cont[1]
          if (sel=="Year" && length(cont) > 1) sel = cont[2]
          updateSelectInput(session, "yaxis",choices=as.list(cont), selected=sel)
        } else if (input$plotType=="DMD") {
          updateRadioButtons(session=session,inputId="XUnits",selected="QMD")
          updateRadioButtons(session=session,inputId="YUnits",selected="Tpa")          
          updateRadioButtons(session=session,inputId="YTrans",selected="log10")
          updateRadioButtons(session=session,inputId="XTrans",selected="log10")          
          updateSelectInput(session, "xaxis",choices=as.list(cont), selected="QMD")
          updateSelectInput(session, "yaxis",choices=as.list(cont), selected="Tpa")
        } else if (input$plotType=="StkCht") {
          updateSelectInput(session, "xaxis",choices=as.list(cont), selected="Tpa")
          updateSelectInput(session, "yaxis",choices=as.list(cont), selected="BA")
        }
        updateSliderInput(session, "transparency",  
          value = if(input$plotType == "scat") .3 else 0.)
        if (input$plotType!="DMD")
        {
          updateRadioButtons(session=session,inputId="YTrans",selected="identity")
          updateRadioButtons(session=session,inputId="XTrans",selected="identity") 
        }
        sel = if (length(intersect(cats,"StandID")) > 0) "StandID" else "None"
        updateSelectInput(session=session, inputId="hfacet",choices=as.list(c("None",cats)),
          selected=sel) 
        sel = if (length(intersect(cats,"MgmtID")) > 0) "MgmtID" else "None"
        updateSelectInput(session=session, inputId="vfacet",choices=as.list(c("None",cats)),
          selected=sel) 
        sel = if (length(intersect(cats,"Species")) > 0) "Species" else "None"
        updateSelectInput(session=session, inputId="pltby",choices=as.list(c("None",cats)),
          selected=sel)
      })
    }
  })   
  ## selectdbvars
  observe({
    if (!is.null(input$selectdbvars)) fvsOutData$dbSelVars <- input$selectdbvars
  })
  ## yaxis, xaxis regarding the Y- and XUnits for DMD
  observe({
    if (!is.null(input$yaxis) && input$yaxis %in% c("Tpa","QMD")) 
      updateRadioButtons(session=session,inputId="YUnits",  
       selected=input$yaxis)
    if (!is.null(input$xaxis) && input$xaxis %in% c("Tpa","QMD")) 
      updateRadioButtons(session=session,inputId="XUnits",                
       selected=input$xaxis)
  })
  ## Set a tool to "None" if the same level is selected by another tool (doesn't 
  ## apply to axes selection
  observe({
    if (is.null(input$pltby) || input$pltby  == "None") return()
    isolate({
      if (all(!c(is.null(input$pltby),is.null(input$xaxis),is.null(input$pltby),
                 is.null(input$yaxis))) && 
         (input$pltby == input$xaxis || input$pltby == input$yaxis))
      {
        updateSelectInput(session=session, inputId="pltby", selected="None")
        return()
      }
      if (input$pltby == input$vfacet)
        updateSelectInput(session=session, inputId="vfacet", selected="None")
      if (input$pltby == input$hfacet)
        updateSelectInput(session=session, inputId="hfacet", selected="None")
  }) }) 
  observe({
    if (is.null(input$vfacet) || input$vfacet  == "None") return()
    isolate({
      if (input$vfacet == input$xaxis || input$vfacet == input$yaxis) 
      {
        updateSelectInput(session=session, inputId="vfacet", selected="None")
        return()
      }
      if (input$vfacet == input$pltby)
        updateSelectInput(session=session, inputId="pltby", selected="None")       
      if (input$vfacet == input$hfacet)
        updateSelectInput(session=session, inputId="hfacet", selected="None")
  }) }) 
  observe({
    if (is.null(input$hfacet) || input$hfacet  == "None") return()                 
    isolate({
      if (input$hfacet == input$xaxis || input$hfacet == input$yaxis) 
      {
        updateSelectInput(session=session, inputId="hfacet", selected="None")
        return()
      }
      if (input$hfacet == input$pltby)
        updateSelectInput(session=session, inputId="pltby", selected="None")
      if (input$hfacet == input$vfacet)
        updateSelectInput(session=session, inputId="vfacet", selected="None")
  }) }) 
  
  
  
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
      output$plotMessage=renderText("Pick different variables, change plot type, or change facet settings.")
      list(src = outfile)
    }
    autorecycle <- function(a,n)
    {
      if (length(a)<n) 
      {
        add = n%/%length(a)
        if (add) a = rep(a,add)
        add = n%%length(a)
        if (add) a = c(a,a[1:add])                                      
      }
      a[1:n]
    } 
    if (input$leftPan == "Load"  || (length(input$xaxis) == 0 && 
        length(input$yaxis) == 0)) return(nullPlot())
    output$plotMessage=renderText(NULL)

    vf = if (input$vfacet == "None") NULL else input$vfacet
    hf = if (input$hfacet == "None") NULL else input$hfacet
    pb = if (input$pltby  == "None") NULL else input$pltby

    dat = if (input$leftPan == "Custom Query") fvsOutData$dbData else         
      droplevels(fvsOutData$dbData[filterRows(fvsOutData$dbData, input$stdtitle, 
          input$stdgroups, input$stdid, input$mgmid, input$year, input$species, 
          input$dbhclass),])
    if (!is.null(pb) && pb=="Groups" && length(input$stdgroups) && length(levels(dat$Groups)))
    {
      for (il in 1:length(levels(dat$Groups)))
      {
        levs = trim(unlist(strsplit(levels(dat$Groups)[il],",")))
        newl = paste0(intersect(levs,input$stdgroups),collapse=", ")
        levels(dat$Groups)[il] = newl
      }      
    }

cat ("vf=",vf," hf=",hf," pb=",pb," xaxis=",input$xaxis," yaxis=",input$yaxis,"\n")
    if (is.null(input$xaxis) || is.null(input$yaxis)) return (nullPlot())
    if (!is.null(hf) && nlevels(dat[,hf]) > 8)
    {
cat ("hf test, nlevels(dat[,hf])=",nlevels(dat[,hf]),"\n")
      updateSelectInput(session=session, inputId="hfacet", selected="None")
      return (nullPlot())
    }
    if (!is.null(vf) && nlevels(dat[,vf]) > 8)
    {
cat ("vf test hit, nlevels(dat[,vf])=",nlevels(dat[,vf]),"\n")
      updateSelectInput(session=session, inputId="vfacet", selected="None")
      return (nullPlot())
    }         
    nlv  = 1 + (!is.null(pb)) + (!is.null(vf)) + (!is.null(hf))    
    vars = c(input$xaxis, vf, hf, pb, input$yaxis)                                        
    if (input$xaxis == "Year" && isolate(input$plotType) != "box" && 
        isolate(input$plotType) != "bar") dat$Year = as.numeric(as.character(dat$Year))
    nd = NULL
    for (v in vars[(nlv+1):length(vars)])
    {
      if (is.na(v) || !v %in% names(dat)) return(nullPlot())
      pd = dat[,c(vars[1:nlv],v),drop=FALSE]
      names(pd)[ncol(pd)] = "Y"
      nd = rbind(nd, data.frame(pd,Legend=v,stringsAsFactors=FALSE))
    }
    hrvFlag = NULL
    if (isolate(input$plotType) %in% c("line","DMD","StkCht"))
    {
      if (is.null(dat[["RmvCode"]]))
      {                
        rtpa = grep ("RTpa",names(dat))[1]
        if (!is.null(dat$Year) && !is.null(rtpa) && !is.na(rtpa) && nrow(dat)>1) 
        {
          hrvFlag = vector(mode="logical",length=nrow(pd))
          i = 0
          while (i < nrow(dat)-1) {
            i = i+1;
            if (dat$Year[i]==dat$Year[i+1] && dat[i+1,rtpa]>0)
            {
              hrvFlag[i]=TRUE
              i=i+1
            }
          }
        } 
      } else hrvFlag = dat[["RmvCode"]] == 1
    }
    nd = na.omit(nd)
    omits = as.numeric(attr(nd,"na.action"))
    if (length(nd) == 0) return(nullPlot())
    if (length(omits)) hrvFlag = hrvFlag[-omits]
    rownames(nd)=1:nrow(nd)
    names(nd)[match(input$xaxis,names(nd))] = "X"
    if (!is.null(vf)) names(nd)[match(vf,names(nd))] = "vfacet"
    if (!is.null(hf)) names(nd)[match(hf,names(nd))] = "hfacet" 
    legendTitle = "Legend"
    if (!is.null(pb) && !is.null(nd$Legend)) 
    {
      legendTitle = pb
      nd$Legend = if (nlevels(as.factor(nd$Legend)) == 1)
        nd[,pb] else paste(nd$Legend,nd[,pb],sep=":")
    }
    # attempt to reduce/modify the by variable set
    bys = setdiff(names(nd),c("X","Y",pb,"hfacet","vfacet"))
cat ("before summing logic, plotType=",input$plotType," names(nd)=",
     names(nd)," nrow(nd)=",nrow(nd)," bys=",bys,"\n")
    if (input$plotType %in% c("line","box","bar")) for (byy in bys)
    {
      tt=table(nd[,c("X",byy)])
cat ("byy=",byy," ncol(tt)=",tt,"\n")
      if (ncol(tt))
      {
        dd=by(data=nd,INDICES=list(nd$X,nd[[byy]]),FUN=function (x)
          {
            an=x[1,,drop=FALSE]
            an$Y=sum(x$Y)
            an
          })
        nd = do.call(rbind,dd)
      }     
cat ("after summing logic names(nd)=",names(nd)," nrow(nd)=",nrow(nd)," bys=",bys,"\n")
    }
    if (!is.null(nd$vfacet)) nd$vfacet = ordered(nd$vfacet, levels=sort(unique(nd$vfacet)))
    if (!is.null(nd$hfacet)) nd$hfacet = ordered(nd$hfacet, levels=sort(unique(nd$hfacet)))
    if (!is.null(nd$Legend)) nd$Legend = ordered(nd$Legend, levels=sort(unique(nd$Legend)))
    fg = NULL
    fg = if (!is.null(nd$vfacet) && !is.null(nd$hfacet)) 
         facet_grid(vfacet~hfacet)
    fg = if (is.null(fg)         && !is.null(nd$hfacet)) 
         facet_grid(.~hfacet) else fg
    fg = if (is.null(fg)         && !is.null(nd$vfacet)) 
         facet_grid(vfacet~.) else fg
    p = ggplot(data=nd) + fg + labs(
          x=if (nchar(input$xlabel)) input$xlabel else input$xaxis, 
          y=if (nchar(input$ylabel)) input$ylabel else input$yaxis, 
          title=input$ptitle)  + 
            theme(text = element_text(size=9),
            panel.background = element_rect(fill="gray95"),
            axis.text = element_text(color="black"))
    colors = if (input$colBW == "B&W") 
      unlist(lapply(seq(0,.3,.05),function (x) rgb(x,x,x))) else
        {
          if (is.null(input$color1)) cbbPalette else
             c(input$color1,input$color2,input$color3,input$color4,input$color5,
               input$color6)
        }
    colors = autorecycle(colors,nlevels(nd$Legend))
    alpha = if (is.null(input$transparency)) .7 else (1-input$transparency)
cat ("nlevels=",nlevels(nd$Legend)," colors=",colors,"\n")
    p = p + theme(axis.text.x = element_text(angle = as.numeric(input$XlabRot), 
      hjust = if(input$XlabRot=="45") 1 else .5))
    p = p + theme(axis.text.y = element_text(angle = as.numeric(input$YlabRot), 
      hjust = if(input$YlabRot!="0") .5 else 1))
    p = p + scale_colour_manual(values=colors)
    p = p + scale_fill_manual(values=colors)
    p = p + scale_shape_manual(values=1:nlevels(nd$Legend))
cat ("input$XTrans=",input$XTrans," input$YTrans=",input$YTrans,"\n")
    xmin = as.numeric(input$XLimMin)
    xmax = as.numeric(input$XLimMax)
    xlim = if (!is.na(xmin) && !is.na(xmax) && xmin < xmax) c(xmin, xmax) else NULL
    ymin = as.numeric(input$YLimMin)
    ymax = as.numeric(input$YLimMax)
    ylim = if (!is.na(ymin) && !is.na(ymax) && ymin < ymax) c(ymin, ymax) else NULL    
cat("ylim=",ylim," xlim=",xlim,"\n")
    ymaxlim = NA
    xmaxlim = NA
    DMDguideLines = NULL
    if (isolate(input$plotType) == "DMD")
    {
      sdis=input$SDIvals
      for (xx in c(" ","\n","\t",",",";")) sdis = if (is.null(sdis)) 
        NULL else unlist(strsplit(sdis,split=xx))
      if (!is.null(sdis))
      {
        maxSDI = max(na.omit(as.numeric(sdis)))
        if (maxSDI == -Inf) {maxSDI=700; sdis = c(sdis,as.character(maxSDI))}
        sdisn = NULL
        for (xx in sdis)
        {
          li = nchar(xx)
          nv = if (li>1 && substr(xx,li,li)=="%") 
            as.numeric(substr(xx,1,li-1))*.01*maxSDI else as.numeric(xx)
          sdisn = c(sdisn,nv)
        }          
cat("sdisn=",sdisn,"\nXUnits=",input$XUnits," YUnits=",input$YUnits,"\n")
        seqTpa = seq(5,3000,length.out=50)
        seqQMD = seq(1,80,length.out=50)
        for (SDI in sdisn)
        {
          xseq = if (input$XUnits=="Tpa") seqTpa else seqQMD
          yseq = if (input$YUnits=="Tpa") 
                   if (input$XUnits=="Tpa") seqTpa else 
                     # Tpa = f(QMD,SDI)  
                     SDI / (seqQMD/10)^1.605 else
                   if (input$XUnits=="QMD") seqQMD else 
                     # QMD = f(Tpa,SDI)
                     exp(log(SDI/seqTpa) / 1.605)*10
          lineData = data.frame(xseq=xseq,yseq=yseq)[! yseq > Inf,]
          ymaxlim = range(c(ymaxlim,lineData$yseq),na.rm=TRUE)
          xmaxlim = range(c(xmaxlim,lineData$xseq),na.rm=TRUE)
          DMDguideLines[[as.character(SDI)]] = lineData
cat("SDI=",SDI," ymaxlim=",ymaxlim," xmaxlim=",xmaxlim,"\n")
        }
      }
    }
    StkChtguideLines = NULL
    if (isolate(input$plotType) == "StkCht")
    {
      sdis=input$StkChtvals
      for (xx in c(" ","\n","\t",",",";")) sdis = if (is.null(sdis)) 
        NULL else unlist(strsplit(sdis,split=xx))
      if (length(sdis))
      {
        sdis = unlist(lapply(sdis,function(x) if(substr(x,nchar(x),nchar(x)) == "%")
          x else paste0(x,"%")))
        for (i in 1:length(sdis)) 
        yptsba  = c(70.2,80.9,89.5,96.5,102.5,107.5,111.9,115.7,119.0,121.8,
                    124.4,126.6,128.9)
        xptstpa = c(1430,928,657,492,383,308,253,212,180,155,135,119,105)
        seqTpa = seq(10,max(2000,nd$X),length.out=100)
        seqBA = 161.47029555*exp(-.02275259*(seqTpa^.5)) #found using nls()       
        ymaxlim = range(seqBA)
        xmaxlim = range(seqTpa)
        StkChtguideLines = list()
        for (PCT in sdis)
        {
          pct = as.numeric(gsub("%","",PCT))*.01
          lineData = data.frame(xseq=seqTpa*pct,yseq=seqBA*pct)
          StkChtguideLines[[as.character(PCT)]] = lineData
          ymaxlim = range(c(ymaxlim,lineData$yseq),na.rm=TRUE)
          xmaxlim = range(c(xmaxlim,lineData$xseq),na.rm=TRUE)
        }
        pcts = as.numeric(gsub("%","",sdis))*.01
        pm = min(pcts)
        px = max(pcts)
        StkChtrng = data.frame(X=c(xptstpa[1]*pm,xptstpa[1]*px,xptstpa*px,rev(xptstpa)*pm),
                               Y=c(yptsba[1]*pm,yptsba[1]*px,yptsba*px,rev(yptsba)*pm))
      }
    }
    ### end DMD...except for adding annotations, see below.
    if (is.factor(nd$X)) nd$X = as.ordered(nd$X)
    if (is.factor(nd$Y)) nd$Y = as.ordered(nd$Y)
    pltp = isolate(input$plotType) 
    if (pltp %in% c("DMD","StkCht")) pltp = "path"
cat ("pltp=",pltp," input$colBW=",input$colBW," hrvFlag is null=",is.null(hrvFlag),"\n")
    brks = function (x,log=FALSE) 
    {
      b = range(x,na.rm=TRUE)
      if (log) {
        b = pretty (log10(b), n = 4, min.n = 1)
        b = ifelse(b<=.1,.1,b)
        b = floor(10**b[!duplicated(b)])
        xx = 10**floor(log10(b))
        ceiling((b/xx))*xx
      } else pretty(b, n=4, min.n = 1)
    }
    if (!is.factor(nd$X))
    {
      rngx=range(if (!is.null(xlim)) xlim else range(c(nd$X,xmaxlim),na.rm=TRUE))
      if(input$XTrans == "log10")
      {
        brkx=brks(rngx,log=TRUE)
        rngx=ifelse(rngx<=.01,.01,rngx)
        p = p + scale_x_log10(breaks=brkx,limits=rngx)
      } else {
        brkx=brks(rngx)
        if (! (pltp %in% c("bar","box"))) p = p + scale_x_continuous(breaks=brkx,limits=rngx)
      }
cat("xlim=",xlim," rngx=",rngx," brkx=",brkx,"\n")
    }
    if (!is.factor(nd$Y)) 
    {
      rngy=range(if (!is.null(ylim)) ylim else range(c(nd$Y,ymaxlim),na.rm=TRUE))
      if(input$YTrans == "log10")
      {
        brky=brks(rngy,log=TRUE)
        rngy=ifelse(rngy<.01,.01,rngy)
        p = p + scale_y_log10(breaks=brky,limits=rngy)
      } else {
        brky=brks(rngy)
        if (! (pltp %in% c("bar","box"))) p = p + scale_y_continuous(breaks=brky,limits=rngy)
      }
cat("ylim=",ylim," rngy=",rngy," brky=",brky,"\n")
    }
    # add the guidelines and annotation here (now that we know the range limits of x and y
    if (!is.null(DMDguideLines)) 
    {
      pltorder = sort(as.numeric(names(DMDguideLines)),decreasing=TRUE,index.return=TRUE)$ix
      for (linetype in 1:length(pltorder)) 
      {
        SDI = names(DMDguideLines)[pltorder[linetype]]
        p = p + geom_line(aes(x=xseq,y=yseq),show.legend=FALSE,alpha=.4,
          linetype=linetype,data=DMDguideLines[[SDI]])
      }
      sq = seq(.95,0,-.05)
      sq = if (input$YTrans=="log10") 10^(log10(rngy[2])*sq) else rngy[2]*sq
      sq = sq[1:min(length(sq),length(pltorder))]
      xs = if (input$XTrans=="log10") 10^(log10(rngx[2])*c(.75,.9)) else rngx[2]*c(.75,.9)
      guidedf = do.call(rbind,lapply(sq,function(y) data.frame(ys=y,xs=xs)))
      guidedf$SDI=unlist(lapply(names(DMDguideLines)[pltorder], function(x) c(x,x)))
      linetype = 0
      for (idrow in seq(1,nrow(guidedf)-1,2)) 
      {
        linetype = linetype+1
        p = p + annotate(geom="text",hjust="left",
          label=paste0(guidedf$SDI[idrow]),size=2,y=guidedf$ys[idrow],x=guidedf$xs[idrow+1]) +
        annotate("segment",y=guidedf$ys[idrow],yend=guidedf$ys[idrow+1],linetype=linetype,
                           x=guidedf$xs[idrow],xend=guidedf$xs[idrow+1],alpha=.4) 
      }
    }
    if (!is.null(StkChtguideLines)) 
    {
      linetype = 1
      for (PCT in rev(names(StkChtguideLines))) 
      {
        linetype = linetype+1
        p = p + geom_line(aes(x=xseq,y=yseq),show.legend=FALSE,alpha=.4,
          linetype=if (PCT == "100%") 1 else linetype,data=StkChtguideLines[[PCT]])
      }
      sq = seq(.95,0,-.05)
      sq = if (input$YTrans=="log10") 10^(log10(rngy[2])*sq) else rngy[2]*sq
      sq = sq[1:min(length(sq),length(names(StkChtguideLines)))]
      xs = if (input$XTrans=="log10") 10^(log10(rngx[2])*c(.75,.9)) else rngx[2]*c(.75,.9)
      guidedf = do.call(rbind,lapply(sq,function(y) data.frame(ys=y,xs=xs)))
      guidedf$PCT=unlist(lapply(rev(names(StkChtguideLines)), function (x) c(x,x)))
      linetype = 1
      for (idrow in seq(1,nrow(guidedf)-1,2))
      {
        linetype = linetype+1
        p = p + annotate(geom="text",hjust="left",
          label=paste0(guidedf$PCT[idrow]),size=2,y=guidedf$ys[idrow],x=guidedf$xs[idrow+1]) +
        annotate("segment",y=guidedf$ys[idrow],yend=guidedf$ys[idrow+1],alpha=.4,
                           x=guidedf$xs[idrow],xend=guidedf$xs[idrow+1],
                           linetype=if (guidedf$PCT[idrow] == "100%") 1 else linetype) 
      }
      p = p + geom_polygon(aes(x=X,y=Y), data = StkChtrng, color="Gray", alpha=.3, 
                          show.legend = FALSE)
    }    
    size  = approxfun(c(50,100,1000),c(1,.7,.5),rule=2)(nrow(nd))

    if (is.factor(nd$X))      nd$X = as.ordered(nd$X)
    if (is.factor(nd$Y))      nd$Y = as.ordered(nd$Y)
    pltp = isolate(input$plotType) 
    if (pltp %in% c("DMD","StkCht")) pltp = "path"
cat ("pltp=",pltp," input$colBW=",input$colBW," hrvFlag is null=",is.null(hrvFlag),"\n")
    p = p + switch(pltp,
      line    = if (input$colBW == "B&W") 
        geom_line  (aes(x=X,y=Y,linetype=Legend),alpha=alpha) else
        geom_line  (aes(x=X,y=Y,color=Legend),alpha=alpha),
      path   = if (input$colBW == "B&W") 
        geom_path  (aes(x=X,y=Y,linetype=Legend),alpha=alpha,    
           arrow=grid::arrow(angle=20,length=unit(6,"pt"),
           ends="last",type="closed")) else
        geom_path  (aes(x=X,y=Y,color=Legend),alpha=alpha,
           arrow=grid::arrow(angle=20,length=unit(6,"pt"),
           ends="last",type="closed")),
      scat = 
        geom_point (aes(x=X,y=Y,color=Legend,shape=Legend),size=size,alpha=alpha),
      bar     = if (input$colBW == "B&W") 
        geom_col (aes(x=X,y=Y,fill=Legend),color="black",size=.2,alpha=alpha,
           position=input$barPlace) else
        geom_col (aes(x=X,y=Y,fill=Legend),color="transparent",size=.1,alpha=alpha,
           position=input$barPlace),
      box     = if (input$colBW == "B&W") 
        geom_boxplot (aes(x=X,y=Y,linetype=Legend),color="black",size=.6,alpha=alpha) else     
        geom_boxplot (aes(x=X,y=Y,color=Legend),linetype=1,size=.6,alpha=alpha) 
      )
    if (!is.null(hrvFlag) && any(hrvFlag)) p = p +
      if (input$colBW == "B&W") 
        geom_point(aes(x=X,y=Y), shape=82,  #the letter R is code 82
          data = nd[hrvFlag,], alpha=alpha, show.legend = FALSE) else
        geom_point(aes(x=X,y=Y,color=Legend), shape=82,  #the letter R is code 82
          data = nd[hrvFlag,], alpha=alpha, show.legend = FALSE)
    if (input$colBW == "B&W" && pltp == "bar") 
        p = p + scale_fill_grey(start=.15, end=.85) 
    p = p + theme(text=element_text(size=9),plot.title = element_text(hjust = 0.5))
    p = p + switch(pltp,
      line    = if (input$colBW == "B&W") 
        guides(linetype=guide_legend(override.aes = list(alpha=1,size=.8),
               title=legendTitle)) else
        guides(colour=guide_legend(override.aes = list(alpha=1,size=.8),
               title = legendTitle)),
      path   = if (input$colBW == "B&W") 
        guides(linetype=guide_legend(override.aes=list(alpha=1,size=.8), 
           arrow=grid::arrow(angle=20,length=unit(5,"pt"),ends="last",type="closed"),
           title=legendTitle)) else
        guides(colour=guide_legend(override.aes=list(alpha=1,size=.8), 
           arrow=grid::arrow(angle=20,length=unit(5,"pt"),ends="last",type="closed"),
           title=legendTitle)),
      scat = 
        guides(shape=guide_legend(override.aes = list(color=colors,alpha=1,size=1),
               title = legendTitle),color="none"),
      bar     = 
        guides(fill=guide_legend(override.aes = list(alpha=.9,size=.6),
               title = legendTitle, keywidth = .8, keyheight = .8)),
      box     = if (input$colBW == "B&W") 
        guides(linetype=guide_legend(override.aes = list(alpha=.8,size=.5),
               title = legendTitle, keywidth = .8, keyheight = .8)) else 
        guides(color=guide_legend(override.aes = list(alpha=.8,size=.5),
               title = legendTitle, keywidth = .8, keyheight = .8))) 
    if (nlevels(nd$Legend)==1 || nlevels(nd$Legend)>30) 
    { 
      p = p + theme(legend.position="none")
      if (nlevels(nd$Legend)>30) output$plotMessage=renderText("Over 30 legend items, legend not drawn.")
    } else p = p + theme(legend.position=input$legendPlace)
    outfile = "plot.png" 
    fvsOutData$plotSpecs$res    = as.numeric(if (is.null(input$res)) 150 else input$res)
    fvsOutData$plotSpecs$width  = as.numeric(input$width)
    fvsOutData$plotSpecs$height = as.numeric(input$height)        
    png(outfile, width=fvsOutData$plotSpecs$width, 
                 height=fvsOutData$plotSpecs$height, units="in", 
                 res=fvsOutData$plotSpecs$res)              
    print(p)
    dev.off()
    list(src = outfile)            
  }, deleteFile = FALSE)
    
  ## Stands tab 
  observe({    
    if (input$topPan == "Runs" || input$rightPan == "Stands") 
    {
cat ("Stands\n")     
      initNewInputDB()
      loadStandTableData(globals, dbGlb$dbIcon)
      updateStandTableSelection()
      loadVarData(globals,prms,dbGlb$dbIcon)                                              
      updateVarSelection()
    }
  })
  
  updateStandTableSelection <- function ()
  {
    cat ("in updateStandTableSelection\n")   
    updateSelectInput(session=session, inputId="inTabs", choices=globals$selStandTableList,
    selected=if (length(globals$selStandTableList)) globals$selStandTableList[[1]] else NULL)
  }
  
  ## inTabs has changed
  observe({
    if (is.null(input$inTabs)) return()
    reloadStandSelection()
cat ("inTabs\n")
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
    reloadStandSelection()
cat ("inVars\n")
  })

  reloadStandSelection <- function ()
  {
cat ("in reloadStandSelection\n")
    stdInit = getTableName(dbGlb$dbIcon,"FVS_StandInit")
    plotInit = getTableName(dbGlb$dbIcon,"FVS_PlotInit")
    stdInit_cond = getTableName(dbGlb$dbIcon,"FVS_StandInit_Cond")
    stdInit_plot = getTableName(dbGlb$dbIcon,"FVS_StandInit_Plot")
    plotInit_plot = getTableName(dbGlb$dbIcon,"FVS_PlotInit_Plot")
    grps = if (input$inTabs =="FVS_StandInit" && !is.null(input$inTabs)) {
      try(dbGetQuery(dbGlb$dbIcon,paste0('select Stand_ID,Groups from ',
        stdInit,' where lower(variant) like "%',input$inVars,'%"')))
      }else if(input$inTabs =="FVS_PlotInit" && !is.null(input$inTabs)){
        try(dbGetQuery(dbGlb$dbIcon,paste0('select StandPlot_ID,Groups from ',
        plotInit,' where lower(variant) like "%',input$inVars,'%"')))
      }else if(input$inTabs =="FVS_StandInit_Cond" && !is.null(input$inTabs)){
        try(dbGetQuery(dbGlb$dbIcon,paste0('select Stand_ID,Groups from ',
        stdInit_cond,' where lower(variant) like "%',input$inVars,'%"')))
      }else if(input$inTabs =="FVS_StandInit_Plot" && !is.null(input$inTabs)){
        try(dbGetQuery(dbGlb$dbIcon,paste0('select Stand_ID,Groups from ',
        stdInit_plot,' where lower(variant) like "%',input$inVars,'%"')))
      }else if(input$inTabs =="FVS_PlotInit_Plot" && !is.null(input$inTabs)){
        try(dbGetQuery(dbGlb$dbIcon,paste0('select StandPlot_ID,Groups from ',
        plotInit_plot,' where lower(variant) like "%',input$inVars,'%"')))
      }else NULL
      
    if (class(grps) == "try-error" || is.null(grps) || nrow(grps) == 0)
    {
      dbExecute(dbGlb$dbIcon,"drop table if exists temp.Grps")
      dbWriteTable(dbGlb$dbIcon,DBI::SQL("temp.Grps"),data.frame(Stand_ID="",Grp=""))
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
      if (input$inTabs =="FVS_StandInit" || input$inTabs =="FVS_StandInit_Cond"||
          input$inTabs =="FVS_StandInit_Plot"){
        colnames(dd) = c("Stand_ID","Grp")
      }else colnames(dd) = c("StandPlot_ID","Grp")
      dd = as.data.frame(dd)
      dbExecute(dbGlb$dbIcon,"drop table if exists temp.Grps")
      dbWriteTable(dbGlb$dbIcon,DBI::SQL("temp.Grps"),dd)
      selGrp = dbGetQuery(dbGlb$dbIcon,
        'select distinct Grp from temp.Grps order by Grp')[,1]
      updateSelectInput(session=session, inputId="inGrps", 
              choices=as.list(selGrp))
      updateSelectInput(session=session, inputId="inStds", 
              choices=list())
      output$stdSelMsg <- renderUI(NULL)
    }
  }

  ## inGrps, inAnyAll, or inStdFindBut has changed
  observe({
    if (input$topPan == "Runs" || input$rightPan == "Stands")
    {
cat ("inGrps inAnyAll inStdFindBut\n")
      # insure reactivity to inStdFindBut
      input$inStdFindBut    
      if (is.null(input$inGrps))          
      {
        output$stdSelMsg <- renderUI(NULL)
        updateSelectInput(session=session, inputId="inStds", 
           choices=list())
      } else {  
         dbExecute(dbGlb$dbIcon,"drop table if exists temp.SGrps")
         dbWriteTable(dbGlb$dbIcon,DBI::SQL("temp.SGrps"),
           data.frame(SelGrps = input$inGrps))
         if ((input$inTabs =="FVS_StandInit" && !is.null(input$inTabs)) ||
             (input$inTabs =="FVS_StandInit_Cond" && !is.null(input$inTabs))||
             (input$inTabs =="FVS_StandInit_Plot" && !is.null(input$inTabs)))
         {
           stds = try(dbGetQuery(dbGlb$dbIcon,paste0('select Stand_ID from temp.Grps ',
                      'where Grp in (select SelGrps from temp.SGrps)')))
         }else  stds = try(dbGetQuery(dbGlb$dbIcon,paste0('select StandPlot_ID from temp.Grps ',
                       'where Grp in (select SelGrps from temp.SGrps)')))
        if (class(stds) == "try-error") return()                                                             
cat ("inGrps, nrow(stds)=",nrow(stds),"\n")
        globals$selStds = stds[,1]
        globals$selStds = if (input$inAnyAll == "Any") unique(globals$selStds) else
        {
          stdCnts = table(globals$selStds) 
          names(stdCnts[stdCnts == length(input$inGrps)])                                                                                                                       
        }                                                                                                
        isolate({
cat ("input$inStdFind=",input$inStdFind,"\n")
          srchStr = input$inStdFind 
          if (length(globals$selStds) && nchar(srchStr)) globals$selStds = 
            globals$selStds[grep(srchStr,globals$selStds)]
        })          
        nstds = length(globals$selStds)
        msg = paste0(length(globals$selStds)," Stand(s) in ",length(input$inGrps)," Group(s)")
        if (nchar(srchStr)) msg = paste0(msg," and matching search string ",srchStr)
        msg = paste0(msg,"<br>")
        output$stdSelMsg <- renderUI(HTML(msg))
        stds = if (length(globals$selStds) <= 220)  globals$selStds else
          c(globals$selStds[1:200],paste0("<< Display 201 to ",
          min(400,length(globals$selStds ))," of ",length(globals$selStds )," >>"))
        updateSelectInput(session=session, inputId="inStds", 
             choices=as.list(stds))
      }
    }
  })
  ## inStds has changed
  observe({
cat ("inStds, length(input$inStds)=",length(input$inStds),"\n")
    if (length(input$inStds) != 1) return()
    prts = unlist(strsplit(input$inStds[1]," "))
    if (prts[1] != "<<") return()
    nprts = as.numeric(prts[c(3,5,7)])
cat ("inStds, nprts=",nprts,"\n")
    up = nprts[c(1,2)] - 200
    if (up[2]-up[1] < 200) up[2] = min(up[1]+200,length(globals$selStds))
    upM = if (up[1] > 0) paste0("<< Display ",up[1]," to ",
      min(up[2],length(globals$selStds))," of ",
      length(globals$selStds)," >>") else NULL
    dn = nprts[c(1,2)] + 200
    if (dn[2]-dn[1] < 200) dn[2] = min(dn[1]+200,length(globals$selStds))
    dn[2] = min(dn[2],length(globals$selStds))
    dnM = if (dn[1] <= length(globals$selStds)) paste0("<< Display ",dn[1]," to ",
      dn[2]," of ",length(globals$selStds)," >>") else NULL
    stds = c(upM,globals$selStds[nprts[1]:nprts[2]],dnM)
cat ("inStds upM=",upM," dnM=",dnM,"\n")    
    updateSelectInput(session=session, inputId="inStds", 
         choices=as.list(stds))   
  })
  
  ## Save saveRun  
  observe({
    if (input$saveRun > 0 || ((input$installNewDBDlgBtn > 0 || input$installTrainDBDlgBtn > 0) && 
                              length(globals$fvsRun$simcnts)))                  
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

  ## New run    
  observe({
    if (input$newRun > 0 || input$installNewDBDlgBtn > 0 || input$addNewDBDlgBtn > 0 || 
        input$installTrainDBDlgBtn > 0 || input$installEmptyDBDlgBtn > 0 )
    {
      if (input$installNewDBDlgBtn > 0 || input$addNewDBDlgBtn > 0 || input$installTrainDBDlgBtn > 0) {
        updateTabsetPanel(session=session,inputId="topPan",selected="Runs") 
        updateTabsetPanel(session=session,inputId="rightPan",selected="Stands") 
        if(!length(globals$fvsRun$simcnts)) return()
      }
      if (input$installEmptyDBDlgBtn > 0){
        updateTabsetPanel(session=session,inputId="topPan",selected="Upload Data")
        updateTabsetPanel(session=session,inputId="inputDBPan",selected="View and edit existing tables")
        if(!length(globals$fvsRun$simcnts)) return()
      }
      resetfvsRun(globals$fvsRun,globals$FVS_Runs)
      globals$fvsRun$title <- paste0("Run ",length(globals$FVS_Runs)+1)
      resetGlobals(globals,NULL,prms)
      if (length(globals$GenGrp)) globals$GenGrp <- list()
      if (length(globals$GrpNum)) globals$GrpNum <- as.numeric()
      loadVarData(globals,prms,dbGlb$dbIcon)
      updateTextInput(session=session, inputId="title", label="Run title",
                      value=globals$fvsRun$title)
      updateTextInput(session=session, inputId="defMgmtID",
                      value=globals$fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="simCont", 
          choices=list(), selected=NULL)
      output$contCnts <- renderUI(HTML(paste0("<b>Contents</b><br>",
        length(globals$fvsRun$stands)," stand(s)<br>",
        length(globals$fvsRun$grps)," group(s)")))
      updateSelectInput(session=session, inputId="addMgmtCats", 
          choices=list(), selected=NULL)
      updateSelectInput(session=session, inputId="addMgmtCmps", 
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
      updateSelectInput(session=session, inputId="compTabSet", 
                        selected="Management")
      updateSelectInput(session=session, inputId="runScript", 
                        selected="fvsRun")
      isolate ({
        if (!is.null(input$inVars) && !is.null(input$inTabs))
        {
          loadStandTableData(globals, dbGlb$dbIcon)
          updateSelectInput(session=session, inputId="inTabs", choices=globals$selStandTableList,
          selected=if (length(globals$selStandTableList)) globals$selStandTableList[[1]] else NULL)          
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
      globals$changeind <- 0
      output$contChange <- renderUI("Run")
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
      updateTextInput(session=session, inputId="title", label="Run title", 
                      value=globals$fvsRun$title) 
      updateTextInput(session=session, inputId="defMgmtID",
                      value=globals$fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="compTabSet", 
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
      ret = try (load(file=fn))  # maybe the file has been corrupted or does not exist
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
        globals$saveOnExit = TRUE
        globals$reloadAppIsSet=1
        session$reload()
      } 
      globals$fvsRun = saveFvsRun
      if (length(saveFvsRun$stands)) for (i in 1:length(saveFvsRun$stands))
      {
        if (length(saveFvsRun$stands[[i]]$grps) > 0)
          for (j in 1:length(saveFvsRun$stands[[i]]$grps))
          { 
            if (length(saveFvsRun$stands[[i]]$grps[[j]]$cmps) > 0)
              for (k in 1:length(saveFvsRun$stands[[i]]$grps[[j]]$cmps))
              {
                test <- saveFvsRun$grps[[j]]$cmps[[k]]$kwds
                spgtest <- grep("^SpGroup",saveFvsRun$grps[[j]]$cmps[[k]]$kwds)
                cntr <- 0
                spgname <- list()
                if (length(spgtest)){
                  cntr<-cntr+1
                  spgname[cntr] <- trim(unlist(strsplit(strsplit(test, split = "\n")[[1]][1],
                  split=" "))[length(unlist(strsplit(strsplit(test, split = "\n")[[1]][1],split=" ")))])

                  if(!length(globals$GrpNum)){
                    globals$GrpNum[1] <- 1
                  }else
                  globals$GrpNum[(length(globals$GrpNum)+1)] <- length(globals$GrpNum)+1
                  
                  spgname[1] <- gsub(" ","", spgname[1])
                  tmpk <- match(spgname[1], globals$GenGrp)
                  if (!is.na(tmpk)){
                    globals$GrpNum <- globals$GrpNum[-length(globals$GrpNum)]
                  }else globals$GenGrp[length(globals$GrpNum)]<-spgname
                }
                if (length(grep("FVS_StandInit",saveFvsRun$grps[[j]]$cmps[[k]]$kwds))){
                  globals$selStandTableList[[1]] <- "FVS_StandInit"}
                if (length(grep("FVS_PlotInit",saveFvsRun$grps[[j]]$cmps[[k]]$kwds))){
                  globals$selStandTableList[[1]] <- "FVS_PlotInit"}
                if (length(grep("FVS_StandInit_Cond",saveFvsRun$grps[[j]]$cmps[[k]]$kwds))){
                  globals$selStandTableList[[1]] <- "FVS_StandInit_Cond"}
                if (length(grep("FVS_StandInit_Plot",saveFvsRun$grps[[j]]$cmps[[k]]$kwds))){
                  globals$selStandTableList[[1]] <- "FVS_StandInit_Plot"}
                if (length(grep("FVS_PlotInit_Plot",saveFvsRun$grps[[j]]$cmps[[k]]$kwds))){
                  globals$selStandTableList[[1]] <- "FVS_PlotInit_Plot"}
              }
          }
        updateSelectInput(session=session, inputId="inTabs", choices=globals$selStandTableList[[1]],
        selected=if (length(globals$selStandTableList)) globals$selStandTableList[[1]] else NULL)
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
        if ((globals$changeind==0 && !length(globals$currentQuickPlot)) && length(globals$fvsRun$simcnts)>0)
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
      updateSelectInput(session=session, inputId="addMgmtCmps", 
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
  
#session$sendCustomMessage(type = "infomessage",
#        message = paste0("WARNING: the ",globals$fvsRun$origDBname," database associated with ", 
#                  globals$fvsRun$title," is no longer active due to another database having been uploaded. Import the ",
#                  globals$fvsRun$origDBname," database before attempting to re-run ",globals$fvsRun$title," ."))

  ##autoOut
   observe({
    if(!length(input$simCont)|| length(globals$fvsRun$autoOut)==length(input$autoOut)) return()
    globals$fvsRun$autoOut<-as.list(input$autoOut)
    updateCheckboxGroupInput(session=session, inputId="autoOut", selected=globals$fvsRun$autoOut)
    globals$changeind <- 1
    output$contChange <- renderText({
      HTML(paste0("<b>","*Run*","</b>"))
    })
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
  ## inStdFindBut: Find and select stands in the stand list that match the search string
  observe({
    if (input$inStdFindBut > 0) 
    {
cat ("input$inStdFindBut=",input$inStdFindBut,"\n")
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
      if (length(input$simCont) == 0) return()
      toed = input$simCont[1]
      # find component
      cmp = findCmp(globals$fvsRun,toed)
      if (is.null(cmp)) return()
      globals$currentEditCmp = cmp
cat ("Edit, cmp$kwdName=",cmp$kwdName,"\n")
      eltList = NULL
      if (length(cmp$kwdName) && nchar(cmp$kwdName))
      { 
        if (exists(cmp$kwdName)) #if a function exists, use it.
        {
          eltList <- eval(parse(text=paste0(cmp$kwdName,
            "(globals$currentEditCmp$title,prms,globals$fvsRun,globals)")))
          if (is.null(eltList)) return(NULL)
          eltList <- eltList[[1]]
        } else {
          pk <- match (cmp$kwdName,names(prms))
          if (!is.na(pk)) # FreeForm Edit, used if pk does not match a parms.
          {        # Launch general purpose builder when pk matches a parms.        
            pkeys <- prms[[pk]]
            eltList <- mkeltList(pkeys,prms,globals,globals$fvsRun,cmp$atag=="c")     
            eltList <- append(eltList,list(
              myInlineTextInput("cmdTitle","Component title: ", 
                        value=globals$currentEditCmp$title,size=40)),after=0)          
          }
        }
      }
      if (is.null(eltList)) eltList <- 
          mkFreeformEltList(globals,prms,globals$currentEditCmp$title,
                            globals$currentEditCmp$kwds)
      eltList <- append(eltList,list(
        h4(paste0('Edit: "',globals$currentEditCmp$title),'"')),after=0)        
      eltList <- append(eltList,list(
           tags$style(type="text/css", "#cmdCancel {color:red;}"),
           actionButton("cmdCancel","Cancel"),
           tags$style(type="text/css", "#cmdSaveInRun {color:green;}"),
           actionButton("cmdSaveInRun","Save in run")))
      output$cmdBuild <- renderUI(eltList)
      output$fvsFuncRender <- renderUI (NULL)
      if (input$rightPan != "Components") updateTabsetPanel(session=session, 
        inputId="rightPan",selected="Components")
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
      eltList <- mkeltList(pkeys,prms,globals,globals$fvsRun,funcflag=TRUE)
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
        pkey = paste0("func.f",fn)
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
        spgkeep <- 0
        for (i in 1:length(globals$pastelist)){
          if (length(grep("^SpGroup",globals$pastelist[i]$kwds)))
            spgkeep <- spgkeep+1
        }
        if (!length(spgkeep))updateReps(globals) 
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
      globals$changeind <- 1
      output$contChange <- renderText({
        HTML(paste0("<b>","*Run*","</b>"))
      })
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
      globals$changeind <- 1
      output$contChange <- renderText({
        HTML(paste0("<b>","*Run*","</b>"))
      })
    })
  })


  # Convert to freeform
  observe({
    if (input$mkfree == 0) return()
    isolate ({
      globals$currentEditCmp <- globals$NULLfvsCmp
      updateSelectInput(session=session, inputId="addMgmtCmps", selected = 0)
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


  ## Command Set.
  observe({
cat ("compTabSet, input$compTabSet=",input$compTabSet,
     " input$simCont=",length(input$simCont),"\n")
    if (length(input$simCont) == 0) return(NULL)
    switch (input$compTabSet,
      "Management" = 
      {
        if (length(globals$mgmtsel) == 0) globals$mgmtsel <- 
          mkcatsel(prms,"mgmtCategories",globals)
        updateSelectInput(session=session, inputId="addMgmtCats", 
          choices=mkpair(globals$mgmtsel), selected = 0)
        updateSelectInput(session=session, inputId="addMgmtCmps", 
          choices=list())
        output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      },                                                     
      "Modifiers"  = 
      {
        if (length(globals$mmodsel) == 0) globals$mmodsel <- 
          mkcatsel(prms,"selectModelModifiers",globals)
        updateSelectInput(session=session, inputId="addModCats", 
          choices=mkpair(globals$mmodsel), selected = 0)
        updateSelectInput(session=session, inputId="addModCmps", 
          choices=list())
        output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      },
      "Event Monitor"= 
      {
        if (length(globals$moutsel) == 0) 
        {
          globals$moutsel <- mkcatsel(prms,"selectModelOutputs",globals)
          # get rid of the first entry about adding the values to the database.
          globals$moutsel[["Event Monitor (EM) Compute Variables"]]=
            globals$moutsel[["Event Monitor (EM) Compute Variables"]][-1]
        }
        updateSelectInput(session=session, inputId="addEvCmps",
          selected = 0,choices=globals$moutsel[["Event Monitor (EM) Compute Variables"]])
        output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      },
      "Economic"= 
      {
        renderComponent("ecn")
      },
      "Keywords"   = 
      {
        if (length(globals$extnsel) == 0) mkextkwd(prms,globals)
         updateSelectInput(session=session, inputId="addKeyExt", 
            label="Extensions", choices=globals$extnsel, selected = 0)
         updateSelectInput(session=session, inputId="addKeyWds", 
            label="Keywords", choices=list())
        output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      },
      "Addfile"   = 
      {
        customCmps = NULL
        if (length(globals$customCmps) == 0 && file.exists("FVS_kcps.RData")) 
        {
          load("FVS_kcps.RData")
          globals$customCmps = customCmps
        }
        if (!is.null(customCmps)) updateSelectInput(session=session,
          inputId="kcpSel",choices=as.list(names(customCmps)), 
          selected=names(customCmps)[1]) 
      },
      NULL)   
  })  
  ## addMgmtCats
  observe({
    if (is.null(input$addMgmtCats)) return()
    if (length(globals$mgmtsel) == 0) globals$mgmtsel <- 
          mkcatsel(prms,"mgmtCategories",globals)
    updateSelectInput(session=session, inputId="addMgmtCmps", selected = 0, 
      choices=globals$mgmtsel[[as.numeric(input$addMgmtCats)]])     
    output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
  })
  ## addModCats
  observe({
    if (is.null(input$addModCats)) return()
        if (length(globals$mmodsel) == 0) globals$mmodsel <- 
          mkcatsel(prms,"selectModelModifiers",globals)
    updateSelectInput(session=session, inputId="addModCmps", selected = 0, 
          choices=globals$mmodsel[[as.numeric(input$addModCats)]])
    output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
  })
  ## addKeyExt
  observe({
    if (is.null(input$addKeyExt)) return()
    updateSelectInput(session=session, inputId="addKeyWds", selected = 0, 
          choices=globals$kwdsel[[input$addKeyExt]])
    output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
  })
  ## addMgmtCmps
  observe({
    if (length(input$addMgmtCmps) && 
        nchar(input$addMgmtCmps)) renderComponent("mgt")
  })
  ## addModCmps
  observe({
    if (length(input$addModCmps) && 
        nchar(input$addModCmps)) renderComponent("mod") 
  })
  ## addKeyWds
  observe({
    if (length(input$addKeyWds) && 
        nchar(input$addKeyWds)) renderComponent("key")
  })
  ## addEvent
  observe({
    if (length(input$addEvCmps) && 
        nchar(input$addEvCmps)) renderComponent("evn")
  })
                
  renderComponent <- function(inCode="default")
  { 
cat ("renderComponent, inCode=",inCode,"\n")
    isolate ({
      globals$currentEditCmp <- globals$NULLfvsCmp
      globals$currentCndPkey <- "0"
      switch (as.character(inCode),
        "mgt" =
        {
          if (is.null(input$addMgmtCats)) return(NULL)
          titIndx = try(match(input$addMgmtCmps,
                              globals$mgmtsel[[as.numeric(input$addMgmtCats)]]))
          if (class(titIndx)=="try-error") return(NULL)
          title = names(globals$mgmtsel[[as.numeric(input$addMgmtCats)]])[titIndx]
          globals$currentCmdPkey = globals$mgmtsel[[as.numeric(input$addMgmtCats)]][titIndx]
        },
        "mod" = 
        {
          titIndx = try(match(input$addModCmps,
                        globals$mmodsel[[as.numeric(input$addModCats)]]))
          if (class(titIndx)=="try-error") return(NULL)
          title = names(globals$mmodsel[[as.numeric(input$addModCats)]])[titIndx]
          globals$currentCmdPkey = globals$mmodsel[[as.numeric(input$addModCats)]][titIndx]
        },
        "key" = 
        {
          titIndx =  try(match(input$addKeyWds,
                         globals$kwdsel[[input$addKeyExt]]))
          if (class(titIndx)=="try-error") return(NULL)
          title = names(globals$kwdsel[[input$addKeyExt]])[titIndx] 
          globals$currentCmdPkey = globals$kwdsel[[input$addKeyExt]][titIndx]
        },
        "evn" = 
        {
          titIndx =  try(match(input$addEvCmps,
                         globals$moutsel[["Event Monitor (EM) Compute Variables"]]))
          if (class(titIndx)=="try-error") return(NULL)
          title = names(globals$moutsel[[7]])[titIndx] 
          globals$currentCmdPkey = globals$moutsel[["Event Monitor (EM) Compute Variables"]][titIndx]
        },
        "ecn" =
        {
          title = "Economic analysis"
          globals$currentCmdPkey = "Econ_reports econ"
        },
        return(NULL)
      ) 
cat   ("globals$currentCmdPkey=",globals$currentCmdPkey,"\n")
      cmdp = scan(text=globals$currentCmdPkey,what="character",sep=" ",quiet=TRUE)[1]
      funName = paste0(cmdp,".Win")
      if (!exists(funName)) funName = cmdp
      if (!exists(funName)) funName = NULL
      if (!is.null(funName)) 
      { 
        globals$winBuildFunction <- funName
cat   ("function name=",globals$winBuildFunction,"\n")
          ans = eval(parse(text=paste0(globals$winBuildFunction,
            "(title,prms,globals$fvsRun,globals)")))
          if (is.null(ans)) return(NULL)
          ans[[1]] <- append(ans[[1]],list(
             tags$style(type="text/css", "#cmdCancel {color:red;}"),
             actionButton("cmdCancel","Cancel"),
             tags$style(type="text/css", "#cmdSaveInRun {color:green;}"),
             actionButton("cmdSaveInRun","Save in run")))
          output$cmdBuild     <- renderUI (if (length(ans[[1]])) ans[[1]] else NULL)
          output$cmdBuildDesc <- renderUI (if (length(ans[[2]])) ans[[2]] else NULL)
      } else { 
        globals$winBuildFunction <- character(0)
        indx = match(cmdp,names(prms))
        if (is.na(indx)) return()
        pkeys <- prms[[indx]]
        eltList <- mkeltList(pkeys,prms,globals,globals$fvsRun)
        if (!is.null(title)) eltList <- 
          append(eltList,list(
            tags$style(type="text/css", "#cmdTitle{display: inline;}"),
            myInlineTextInput("cmdTitle","Component title ", value=title,size=40)),
            after=0)
        eltList <- append(eltList,list(
           tags$style(type="text/css", "#cmdCancel {color:red;}"),
           actionButton("cmdCancel","Cancel"),
           tags$style(type="text/css", "#cmdSaveInRun {color:green;}"),
           actionButton("cmdSaveInRun","Save in run")))
        output$cmdBuild <- renderUI (if (length(eltList)) eltList else NULL)
        des <- getPstring(pkeys,"description",globals$activeVariants[1])
        output$cmdBuildDesc <- renderUI (if (!is.null(des) && nchar(des) > 0)
          HTML(paste0("<br>Description:<br>",gsub("\n","<br>",des))) else NULL)
      }
    })          
  }

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
      if (length(globals$toggleind)) globals$currentCndPkey <- "0"
    } else if (input$schedbox == 2) 
    {
      updateTextInput(session, globals$schedBoxPkey, 
        label = "Number of years after condition is found true: ", value = "0") 
      cndlist = unlist(prms$conditions_list)
      names(cndlist) = unlist(lapply(prms$conditions_list,attr,"pstring"))
      cndlist = as.list(cndlist)
      globals$toggleind <- "1"
cat("globals$currentCmdPkey=",globals$currentCmdPkey,"\n")
      if (length(globals$currentCmdPkey))
      {
        n = suppressWarnings(as.numeric(globals$currentCmdPkey))   
        default =  getPstring(prms[[if (is.na(n)) globals$currentCmdPkey else n]],
          "defaultCondition",globals$activeVariants[1])
        if (is.null(default)) default="cycle1"
      } else default = "cycle1"
      output$conditions <- renderUI(list(
        selectInput("condList", "Create a condition", cndlist, 
          selected = default, multiple = FALSE, selectize = FALSE),
        uiOutput("condElts")
      ))
    } else {
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
    if (length(globals$toggleind) && input$schedbox == 1)return()
cat("make condElts, input$condList=",input$condList,"\n")    
    output$condElts <- renderUI(if (input$condList == "none") NULL else
      {
        cnpkey <- paste0("condition.",input$condList)
        idx <- match(cnpkey,names(prms))
        globals$currentCndPkey <- if (is.na(idx)) "0" else cnpkey
        ui = if (globals$currentCndPkey == "0") NULL else
        {
          eltList <- mkeltList(prms[[globals$currentCndPkey]],prms,
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
    if (length(input$cmdCancel) && input$cmdCancel == 0) return()
    closeCmp()
  })
  
  closeCmp <- function ()
  {
    globals$currentEditCmp <- globals$NULLfvsCmp
    globals$schedBoxPkey <- character(0)
    updateSelectInput(session=session, inputId="addMgmtCmps", selected = 0)     
    updateSelectInput(session=session, inputId="addModCmps", selected = 0)     
    updateSelectInput(session=session, inputId="addKeyWds", selected = 0)     
    updateSelectInput(session=session, inputId="addEvCmps",selected = 0)
    output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
  }

  observe({  
    # command Save in run 
    if (length(input$cmdSaveInRun) && input$cmdSaveInRun == 0) return()
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
        closeCmp()
        return()
      }
      if (globals$currentCndPkey != "0")
      {
        kwPname = globals$currentCndPkey
        pkeys = prms[[kwPname]]
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
          kwPname = scan(text=globals$currentCmdPkey,what="character",sep=" ",quiet=TRUE)
          pkeys = prms[[kwPname[1]]]
        }
        oReopn  = character(0) 
cat ("Building a component: kwPname=",kwPname,"\n")
      } else { # we are editing the component
        kwPname = globals$currentEditCmp$kwdName
        oReopn  = globals$currentEditCmp$reopn
cat ("Editing a component: kwPname=",kwPname," oReopn=",oReopn,"\n")
        pkeys = if (length(kwPname)) prms[[kwPname]] else NULL
        if (is.null(pkeys) && length(oReopn) == 0) #this is freeform...
        {
cat ("Editing as freeform\n")
          globals$currentEditCmp$kwds = input$freeEdit
          globals$currentEditCmp$reopn = character(0)
          globals$currentEditCmp$title = input$cmdTitle
          mkSimCnts(globals$fvsRun,input$simCont[[1]])
          updateSelectInput(session=session, inputId="simCont", 
             choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
          closeCmp()
          return()
        }  
      }
      # building/editing a keyword from a custom window.    
      if (length(pkeys) == 0 && nchar(kwPname)) 
      {
        # try to find a function that can make the keywords
        fn = paste0(kwPname,".mkKeyWrd")
        ans = if (exists(fn)) eval(parse(text=paste0(fn,"(input,output)"))) else NULL
        if (is.null(ans)) return()
        ex = ans$ex
        kwds = ans$kwds
        reopn = ans$reopn
      } else { 
        # build from prms entry
        ansFrm = getPstring(pkeys,"answerForm",globals$activeVariants[1])
        if (is.null(ansFrm)) 
        { 
cat ("kwPname=",kwPname,"\n")
          kw = kwPname[1]
          kw = unlist(strsplit(kw,".",fixed=TRUE))
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
          instr =  if (length(globals$currentEditCmp$atag) && 
                              globals$currentEditCmp$atag=="c") 
                  input[[paste0("cnd.",pkey)]] else input[[pkey]]            
          if (fps == "scheduleBox" && !is.null(input$schedbox) && 
              input$schedbox == "1" && 
              !identical(newcnd,globals$NULLfvsCmp)) newcnd = NULL
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
        gensps <- grep("SpGroup", kwds)
        if(length(gensps)) { cntr <- 0
        if(!length(globals$GrpNum)){
          globals$GrpNum[1] <- 1
        }else
        globals$GrpNum[(length(globals$GrpNum)+1)] <- length(globals$GrpNum)+1
        grlist <- list()
        for (spg in 1:length(reopn))if(try(reopn[spg])!=" ")
        {
          cntr<-cntr+1
          grlist[cntr]<-reopn[spg]
        }

# prevent duplicate SpGroup names due to editing & saving non-name changes
        grlist[1] <- gsub(" ","", grlist[1])
        tmpk <- match(grlist[1], globals$GenGrp)
          if (is.na(tmpk) && !length(globals$currentEditCmp$kwds)){
          globals$GenGrp[length(globals$GrpNum)]<-grlist
          }
          if (is.na(tmpk) && length(globals$currentEditCmp$kwds)){
            globals$GrpNum <- globals$GrpNum[-length(globals$GrpNum)]
            globals$GenGrp <- globals$GenGrp[-length(globals$GenGrp)]
            globals$GenGrp[length(globals$GrpNum)]<-grlist
          }
          if (!is.na(tmpk) && length(globals$currentEditCmp$kwds)){
            globals$GrpNum <- globals$GrpNum[-length(globals$GrpNum)]
          }
        } 
      }
cat ("Save, kwds=",kwds,"\n")      
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp))
      {
        ex = if (length(kwPname) == 2) kwPname[2] else "base"
cat ("building component kwPname=",kwPname,"\n")
        newcmp = mkfvsCmp(uuid=uuidgen(),atag="k",kwds=kwds,exten=ex,
             variant=globals$activeVariants[1],kwdName=kwPname[1],
             title=input$cmdTitle,reopn=if (is.null(reopn)) character(0) else reopn)
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
      globals$changeind <- 1
      output$contChange <- renderText({
        HTML(paste0("<b>","*Run*","</b>"))
      })
      closeCmp()
      globals$schedBoxPkey <- character(0)
    })
  })

  observe({
    cat ("changeind=",globals$changeind,"\n")
    if (globals$changeind == 0){
      output$contChange <- renderUI("Run")
      output$srtYr <-renderUI({
        HTML(paste0("<b>",input$startyr,"</b>"))
      })
      output$eYr <-renderUI({
        HTML(paste0("<b>",input$endyr,"</b>"))
      })
      output$cyLen <-renderUI({
        HTML(paste0("<b>",input$cyclelen,"</b>"))
      })
      output$cyAt <-renderUI({
        HTML(paste0("<b>",input$cycleat,"</b>"))
      })
    }
  })
  
  ## time--start year
  observe({
    if(!length(input$simCont) || globals$fvsRun$startyr==input$startyr) return()
    globals$fvsRun$startyr  <- input$startyr
    updateTextInput(session=session, inputId="startyr", value= input$startyr)
    globals$changeind <- 1
    output$contChange <- renderText({
      HTML(paste0("<b>","*Run*","</b>"))
    })
  })
  ## time--end year
  observe({
    if(!length(input$simCont) || globals$fvsRun$endyr==input$endyr) return()
    globals$fvsRun$endyr <- input$endyr
    updateTextInput(session=session, inputId="endyr", value= input$endyr)
    globals$changeind <- 1
    output$contChange <- renderText({
      HTML(paste0("<b>","*Run*","</b>"))
    })
  })
  ## time--cycle length
  observe({
    if(!length(input$simCont) || globals$fvsRun$cyclelen==input$cyclelen) return()
    globals$fvsRun$cyclelen <- input$cyclelen
    updateTextInput(session=session, inputId="cyclelen", value= input$cyclelen)
    globals$changeind <- 1
    output$contChange <- renderText({
      HTML(paste0("<b>","*Run*","</b>"))
    })
  }) 
  ## time--cycle breaks
  observe({
    if(!length(input$simCont) || (length(globals$fvsRun$cycleat) && 
                                  length(input$cycleat) && globals$fvsRun$cycleat==input$cycleat)) return()
    globals$fvsRun$cycleat  <- input$cycleat
    globals$changeind <- 1
    output$contChange <- renderText({
      HTML(paste0("<b>","*Run*","</b>"))
    })
  })

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
        # if rerunning a run that is currently selected in the "View Outputs",
        # then clear those tools.
        if (globals$fvsRun$uuid %in% input$runs) initTableGraphTools()
        progress$set(message = "Run preparation: ", 
          detail = "Deleting old ouputs", value = 2)         
        removeFVSRunFiles(globals$fvsRun$uuid)
        updateSelectInput(session=session, inputId="bkgRuns", 
                          choices=getBkgRunList(),selected=0)
        progress$set(message = "Run preparation: ", 
          detail = "Write .key file and prepare program", value = 3) 
        writeKeyFile(globals$fvsRun,dbGlb$dbIcon,prms,newSum=!("FVS_Summary" %in% 
          dbGetQuery(dbGlb$dbOcon,"select name from sqlite_master where type='table';")[[1]]))
        if(globals$timeissue==1){
          progress$close()
          isolate(updateTabsetPanel(session=session,inputId="rightPan",selected="Time"))
        }
        if (!file.exists(paste0(globals$fvsRun$uuid,".key")))
        {
cat ("keyword file was not created.\n")
          progress$set(message = "Error: Keyword file was not created.",
                      detail = "", value = 3) 
          Sys.sleep(3)
          progress$close()     
          return()
        }          
        dir.create(globals$fvsRun$uuid)
        locrFVSDir = if (isLocal() && exists("rFVSDir") && 
                         !is.null(rFVSDir)) rFVSDir else "rFVS/R"
        if (!file.exists(locrFVSDir)) return()
        locbinDir = if (isLocal() && exists("fvsBinDir") && 
                        !is.null(fvsBinDir)) fvsBinDir else "FVSbin"
        if (!file.exists(locbinDir)) return()
cat ("runwaitback=",input$runwaitback,"\n")
        if (input$runwaitback!="Wait for run")
        {
          runScript = paste0(globals$fvsRun$uuid,".rscript")
          rs = file(runScript,open="wt")
          cat (paste0('setwd("',getwd(),'")\n'),file=rs)
          cat ('options(echo=TRUE)\nlibrary(methods)\nlibrary(RSQLite)\n',file=rs)
          cat ('pid = Sys.getpid()\n',file=rs)
          cmd = paste0('unlink("',globals$fvsRun$uuid,'.db")')
          cat (cmd,"\n",file=rs)
          cmd = paste0("title = '",globals$fvsRun$title,"'")
          cat (cmd,"\n",file=rs)                   
          cmd = paste0("nstands = ",length(globals$fvsRun$stands))
          cat (cmd,"\n",file=rs)          
          cmd = paste0("for (rf in dir('",locrFVSDir,
             "')) source(paste0('",locrFVSDir,"','/',rf))")
          cat (cmd,"\n",file=rs)
          cmd = paste0("fvsLoad('",
             globals$fvsRun$FVSpgm,"',bin='",locbinDir,"')")
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
          rscript = if (exists("RscriptLocation")) RscriptLocation else "Rscript"
          cmd = paste0(rscript," --no-restore --no-save --no-init-file ",runScript,
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
        cmd = paste0("clusterEvalQ(fvschild,for (rf in dir('",locrFVSDir,
          "')) source(paste0('",locrFVSDir,"','/',rf)))")
cat ("load rFVS cmd=",cmd,"\n")          
        rtn = try(eval(parse(text=cmd)))
        if (class(rtn) == "try-error") return()
        cmd = paste0("clusterEvalQ(fvschild,fvsLoad('",
             globals$fvsRun$FVSpgm,"',bin='",locbinDir,"'))")
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
            } else {
cat ("running normal run cmd\n")
              try(clusterEvalQ(fvschild,fvsRun()))
            }
          if (class(rtn) == "try-error")
          { 
            cat ("run try error\n")
            break
          }
cat ("rtn class for stand i=",i," is ",class(rtn),"\n")
          if (class(rtn) != "try-error") rtn = rtn[[1]]
          if (rtn != 0) break          
          ids = try(clusterEvalQ(fvschild,fvsGetStandIDs()))
          if (class(ids) == "try-error") break
          ids = ids[[1]]
          rn = paste0("SId=",ids["standid"],";MId=",ids["mgmtid"])
cat ("rn=",rn,"\n")
          rtn = try(clusterEvalQ(fvschild,fvsSetupSummary(fvsGetSummary())))
          if (class(rtn) == "try-error") break
          allSum[[i]] = rtn[[1]]
          names(allSum)[i] = rn
        }
cat ("rtn,class=",class(rtn),"\n")
        try(clusterEvalQ(fvschild,fvsRun()))        
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
        height = 2
        plt = if (nlevels(toplot$hfacet) < 5)
          ggplot(data = toplot) + facet_grid(.~hfacet) + 
            geom_line (aes(x=X,y=Y,color=Legend,linetype=Legend)) +
            labs(x="Year", y="Total cubic volume per acre") + 
            theme(text = element_text(size=9), legend.position="none",
                  panel.background = element_rect(fill="gray95"),
                  axis.text = element_text(color="black"))  else
        {
          width = 2.5
          toplot$Legend = as.factor(paste0(toplot$Legend,toplot$hfacet))
          ggplot(data = toplot) +  
            geom_line (aes(x=X,y=Y,color=Legend,alpha=.5)) + 
            labs(x="Year", y="Total cubic volume per acre") + 
               theme(text = element_text(size=9), legend.position="none",
                     panel.background = element_rect(fill="gray95"),
                     axis.text = element_text(color="black"))
        }
        png("quick.png", width=width, height=height, units="in", res=150)
        print(plt)
        dev.off()
        output$uiRunPlot <- renderUI(
                plotOutput("runPlot",width="100%",height=paste0((height+1)*144,"px")))
        output$runPlot <- renderImage(list(src="quick.png", width=(width+1)*144, 
                height=(height+1)*144), deleteFile=TRUE)
cat ("setting currentQuickPlot, input$runSel=",input$runSel,"\n")
        globals$currentQuickPlot = globals$fvsRun$uuid
        globals$changeind <- 0
        output$contChange <- renderUI("Run")
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
  output$dlFVSOutxlsx <- downloadHandler(
       filename=function () paste0(globals$fvsRun$title,"_FVSoutput.xlsx"),
       content = function (tf = paste0(tempfile(),".xlsx"))
       {
         runuuid = globals$fvsRun$uuid
         if (is.null(runuuid)) return()
         tabs = dbGetQuery(dbGlb$dbOcon,"select name from sqlite_master where type='table';")[,1]
         if (!("FVS_Cases" %in% tabs)) return()
         cases = dbGetQuery(dbGlb$dbOcon,paste0("select CaseID from FVS_Cases ",
             "where KeywordFile = '",globals$fvsRun$uuid,"';"))
         if (nrow(cases) == 0) return()
cat ("download run as xlsx, ncases=",nrow(cases),"\n")
         tmp = paste0("tmp",gsub("-","",runuuid),Sys.getpid(),"genoutput")
         dbExecute(dbGlb$dbOcon,paste0("attach database ':memory:' as ",tmp))
         casesToGet = paste0(tmp,".casesToGet")
         dbWriteTable(dbGlb$dbOcon,name=DBI::SQL(casesToGet),value=cases,overwirte=TRUE)
         out = list()
         cmpYes = if ("CmpMetaData" %in% tabs) 
         { 
           meta = try(dbReadTable(dbGlb$dbOcon,"CmpMetaData"))
           class(meta) == "data.frame" && meta$KeywordFile == runuuid
         } 
         for (tab in tabs)
         {
           qry = if (cmpYes && substr(tab,1,3) == "Cmp")
             paste0("select * from ",tab," limit 1048576;") else
             paste0("select * from ",tab," where ",tab,".CaseID in",
                    " (select CaseID from ",casesToGet,") limit 1048576;")
          dat = try(dbGetQuery(dbGlb$dbOcon,qry))
          if (class(dat) == "try-error") next
          if (nrow(dat) == 0) next
          out[[tab]] = dat
cat ("qry=",qry," class(dat)=",class(dat),"\n")
         }
         dbExecute(dbGlb$dbOcon,paste0("detach database ",tmp,";"))
         if (length(out)) write.xlsx(file=tf,out)
       }, contentType=NULL)
  ## Download dlRenderData 
  output$dlRenderData <- downloadHandler(
      filename=function() paste0("table",isolate(input$dlRDType)),
      content=function (tf = tempfile())
      {
        if (isolate(input$dlRDType) == ".csv")
        {
          if (nrow(fvsOutData$render) > 0)
            write.csv(fvsOutData$render,file=tf,row.names=FALSE) else 
            cat (file=tf,'"No data"\n')
        } else {
          if (nrow(fvsOutData$render) > 0)
          {
            if (nrow(fvsOutData$render) > 1048576) 
              write.xlsx(fvsOutData$render[1:1048576,],file=tf,colNames = TRUE) else 
              write.xlsx(fvsOutData$render,file=tf,colNames = TRUE) 
          } else write.xlsx(file=tf)
        }          
      }, contentType=NULL)
  ## dlPrjBackup
  output$dlPrjBackup <- downloadHandler(filename=function ()
      isolate({
        if (file.exists(input$pickBackup)) input$pickBackup else "NoBackup.txt"
      }),  
      content=function (tf = tempfile())
      {
        sfile = input$pickBackup
        if (file.exists(sfile)) file.copy(sfile,tf) else
          cat (file=tf,"Backup does not exist.\n")
      }, contentType="zip")
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
           if (dir.exists(tempDir)) lapply(paste0(tempDir,"/",dir(tempDir)),unlink) else
               dir.create(tempDir)
           for (ele in input$dlZipSet)
           {
             switch (ele,
               outdb = {
                 from="FVSOut.db"
                 to=paste0(tempDir,"/FVSOut.db")
                 if (file.exists(from)) file.copy(from=from,to=to) else
                   cat (file=to,"Output database does not exist.\n")
               },
               key   = {
                 from=paste0(input$runSel,".key")
                 to=paste0(tempDir,"/",globals$fvsRun$title,"_FVSkeywords.txt") 
                 if (file.exists(from)) file.copy(from=from,to=to) else
                   cat (file=to,"Keyword file not yet created.\n")                   
               },
               out   = {
                 from=paste0(input$runSel,".out")
                 to=paste0(tempDir,"/",globals$fvsRun$title,"_FVSoutput.txt")
                 if (file.exists(from)) file.copy(from=from,to=to) else
                   cat (file=to,"Output not yet created.\n")
               },
               subdir= {
                 from=input$runSel
                 if (dir.exists(from)) 
                 {
                   to = paste0(tempDir,"/",globals$fvsRun$title,"_SVS/")
                   dir.create (to)
                   file.copy(from=from,to=to,recursive = TRUE)
                   file.copy(from=paste0(from,"_index.svs"),to=to)
                 } else cat(file=paste0(tempDir,"/emptySVS.txt"),"No SVS files exist.\n")
               },              
               FVS_Data = file.copy(from="FVS_Data.db",
                                    to=paste0(tempDir,"/FVS_Data.db")),
               FVS_Runs = {
                 if (file.exists("FVS_Runs.RData")) 
                   file.copy(from="FVS_Runs.RData",to=paste0(tempDir,"/FVS_Runs.RData"))
                 rdm=paste0(tempDir,"/ReadMe.txt")
                 cat (file=rdm,append=TRUE,"Run UUIDs and titles as of:",date(),"\n")
                 for (kn in names(globals$FVS_Runs))
                 {
                   cat (file=rdm,append=TRUE,"UUID:",kn," Title:",globals$FVS_Runs[[kn]],"\n")
                   fn = paste0(kn,".RData")
                   if (file.exists(fn)) file.copy(from=fn,to=paste0(tempDir,"/",fn))
                 }
               },
               customSQL = if (file.exists("customQueries.RData")) 
                 file.copy(from="customQueries.RData",to=paste0(tempDir,"/customQueries.RData")),
               FVS_kcps = if (file.exists("FVS_kcps.RData"))
                 file.copy(from="FVS_kcps.RData",to=paste0(tempDir,"/FVS_kcps.RData"))
           )}
           curdir = getwd()
           setwd(tempDir)
           zip(tf,dir())
           unlink(tempDir,recursive = TRUE)
           setwd(curdir)
         }, contentType="application/zip") 
  
  ## kcpSel
  observe({
    if (length(input$kcpSel) == 0) return()
cat ("kcpSel called, input$kcpSel=",input$kcpSel,"\n")
    if (is.null(input$kcpSel))
    { 
      updateTextInput(session=session, inputId="kcpTitle",value="") 
      updateTextInput(session=session, inputId="kcpEdit",value="")
    } else {
      sel = match(trim(input$kcpSel),trim(names(globals$customCmps)))
      updateTextInput(session=session, inputId="kcpTitle",
        value=names(globals$customCmps)[sel])
      updateTextInput(session=session, inputId="kcpEdit",
        value=globals$customCmps[[sel]])
    }
  })

  ## kcpSaveCmps
  observe({  
    if (length(input$kcpSaveCmps) && input$kcpSaveCmps > 0)
    {
      isolate ({
cat ("kcpSaveCmps called, kcpTitle=",input$kcpTitle," isnull=",
is.null(input$kcpTitle),"\n")
        if (nchar(input$kcpTitle) == 0)
        {
          newTit = paste0("Component ",length(globals$customCmps)+1) 
          updateTextInput(session=session, inputId="kcpTitle", value=newTit)
        } else newTit = trim(input$kcpTitle)
        globals$customCmps[[newTit]] = input$kcpEdit
        customCmps = globals$customCmps
        topaste = findCmp(globals$fvsRun,input$simCont[1])
        if (is.null(topaste)) return()
        if(length(grep("^--> Kwd",names(globals$kcpAppendConts[length(globals$kcpAppendConts)])))){
        updateTextInput(session=session, inputId="kcpEdit", value=
          paste0(customCmps,"ENDIF\n"))
          customCmps <-as.list(paste0(customCmps,"ENDIF\n"))
          names(customCmps) <- names(globals$customCmps)
          globals$customCmps = customCmps
          }
        save(file="FVS_kcps.RData",customCmps)
        updateSelectInput(session=session, inputId="kcpSel",
           choices=names(globals$customCmps),
           selected=newTit)
      })
      mkSimCnts(globals$fvsRun,input$simCont[[1]])
      updateSelectInput(session=session, inputId="simCont",
         choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
      closeCmp()
      
    }
  })
  
  ## kcpSaveInRun
  observe({  
    if (length(input$kcpSaveInRun) && input$kcpSaveInRun > 0)
    {
      isolate ({
cat ("kcpSaveInRun\n")
        if (nchar(input$kcpTitle) == 0)
        {
          newTit = paste0("Freeform: Component ",length(globals$customCmps)+1) 
          updateTextInput(session=session, inputId="kcpTitle", value=newTit)
        } else newTit = paste0("Addfile: ",trim(input$kcpTitle))
        newcmp = mkfvsCmp(uuid=uuidgen(),atag="k",kwds=input$kcpEdit,exten="base",
             variant=globals$activeVariants[1],kwdName=character(0),
             title=newTit,reopn=character(0))
        # find the attachment point. 
        sel = input$simCont[[1]]
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
        mkSimCnts(globals$fvsRun,input$simCont[[1]])
        updateSelectInput(session=session, inputId="simCont", 
           choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
        closeCmp()
        globals$schedBoxPkey <- character(0)
      })
    }
  })


  ## kcpDelete
  observe({  
    if (length(input$kcpDelete) && input$kcpDelete > 0)
    {
      isolate ({
cat ("kcpDelete, input$kcpSel=",input$kcpSel,"\n")
        sel = na.omit(match(trim(input$kcpSel),trim(names(globals$customCmps))))
        if (length(sel)) globals$customCmps[[sel[1]]] = NULL
        if (length(globals$customCmps)) 
        {
          customCmps = globals$customCmps
          save(file="FVS_kcps.RData",customCmps)
          updateSelectInput(session=session, inputId="kcpSel", choices=names(customCmps))
        } else {
          customCmps=NULL
          unlink("FVS_kcps.RData")
          updateSelectInput(session=session, inputId="kcpSel", choices=list())
        }
      })
    }
  })

  ## kcpNew
  observe({  
    if (length(input$kcpNew) && input$kcpNew > 0)
    {
      isolate ({
        updateSelectInput(session=session, inputId="kcpSel", selected = 0)
        updateTextInput(session=session, inputId="kcpTitle", value="")
        updateTextInput(session=session, inputId="kcpEdit", value="")
        globals$kcpAppendConts <- list()
      })
cat ("kcpNew called, input$kcpNew=",input$kcpNew,"\n")
    }
  })

  ## kcpUpload
  observe({  
    if (is.null(input$kcpUpload)) return()
    data=scan(file=input$kcpUpload$datapath,sep="\n",what="",quiet=TRUE)
    if (input$kcpUpload$name=="FVS_kcps.RData") data <- data[4:length(data)]
    if (length(data)==0) return()
    isolate ({
      addnl = TRUE
      if (is.null(input$kcpTitle) || nchar(input$kcpTitle) == 0)
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
    if (length(input$kcpAppend) && input$kcpAppend > 0)
    {
      isolate ({
        topaste = findCmp(globals$fvsRun,input$simCont[1])
        if (is.null(topaste)) return()
        if (nchar(input$kcpTitle) == 0) 
          updateTextInput(session=session, inputId="kcpTitle", 
            value=topaste$title)
        updateTextInput(session=session, inputId="kcpEdit", value=
          paste0(input$kcpEdit,"* ",topaste$title,"\n",topaste$kwds,"\n"))
        session$sendCustomMessage(type="refocus", "kcpEdit")
        indx <- match(input$simCont,globals$fvsRun$simcnts)
        if (!length(globals$kcpAppendConts)){
          globals$kcpAppendConts[1] <- globals$fvsRun$simcnts[indx]
          names(globals$kcpAppendConts)[1] <- names(globals$fvsRun$simcnts)[indx]
        }else
          globals$kcpAppendConts[(length(globals$kcpAppendConts)+1)] <- globals$fvsRun$simcnts[indx]
          names(globals$kcpAppendConts)[length(globals$kcpAppendConts)] <- names(globals$fvsRun$simcnts)[indx]
          # first conditional added
        if (length(grep("^-> Cnd",names(globals$kcpAppendConts[length(globals$kcpAppendConts)]))) &&
            (!length(globals$opencond) || globals$opencond==0)){
          globals$opencond <- 1
          globals$condKeyCntr <- 0
        }
        # first conditional keyword added
        if (length(grep("^--> Kwd",names(globals$kcpAppendConts[length(globals$kcpAppendConts)])))){
          globals$condKeyCntr <- globals$condKeyCntr + 1
          }
        if (length(grep("^-> Cnd",names(globals$kcpAppendConts[length(globals$kcpAppendConts)]))) &&
            (length(globals$condKeyCntr) && globals$condKeyCntr > 0)){
          globals$opencond <- 0
          globals$condKeyCntr <- 0
          updateTextInput(session=session, inputId="kcpEdit", value=
            paste0(input$kcpEdit,"ENDIF\n","* ",topaste$title,"\n",topaste$kwds,"\n"))
          }
        if (length(grep("^-> Kwd",names(globals$kcpAppendConts[length(globals$kcpAppendConts)])))&&
            (length(globals$condKeyCntr) && globals$condKeyCntr > 0)){
          globals$opencond <- 0
          globals$condKeyCntr <- 0
          updateTextInput(session=session, inputId="kcpEdit", value=
            paste0(input$kcpEdit,"ENDIF\n","* ",topaste$title,"\n",topaste$kwds,"\n"))
        }  
      })
    }
  })
  
  observe({
    if (input$topPan == "SVS3d(alpha)")
    {
cat ("SVS3d hit\n")
      allRuns = names(globals$FVS_Runs)
      names(allRuns) = globals$FVS_Runs
      runChoices = list()
      for (has in names(allRuns))
      {
        fn = paste0(allRuns[[has]],"_index.svs")
        if (file.exists(fn)) runChoices[[has]] = allRuns[[has]]
      }
      updateSelectInput(session=session, inputId="SVSRunList1", 
        choices=runChoices,selected=0)
      updateSelectInput(session=session, inputId="SVSRunList2", 
        choices=runChoices,selected=0)   
      updateSelectInput(session=session, inputId="SVSImgList1", choices=list(),
        selected=0) 
      updateSelectInput(session=session, inputId="SVSImgList2", choices=list(),
        selected=0)
      output$SVSImg1 = renderRglwidget(NULL)
      output$SVSImg2 = renderRglwidget(NULL)
    }
   })
  observe({
    if (length(input$SVSRunList1))
    {
cat ("SVS3d input$SVSRunList1=",input$SVSRunList1,"\n")
      fn = paste0(input$SVSRunList1,"_index.svs")
      if (!file.exists(fn)) return()
      index = read.table(file=fn,as.is=TRUE)
      choices = as.list(index[,2])
      names(choices) = index[,1]
      updateSelectInput(session=session, inputId="SVSImgList1", choices=choices, 
                        selected = 0)
      output$SVSImg1 = renderRglwidget(NULL)
    }
  })
  observe({
    if (length(input$SVSRunList2))
    {
cat ("SVS3d input$SVSRunList2=",input$SVSRunList2,"\n")
      fn = paste0(input$SVSRunList2,"_index.svs")
      if (!file.exists(fn)) return()
      index = read.table(file=fn,as.is=TRUE)
      choices = as.list(index[,2])
      names(choices) = index[,1]
      updateSelectInput(session=session, inputId="SVSImgList2", choices=choices, 
                        selected = 0)
      output$SVSImg2 = renderRglwidget(NULL)
    }
  })
  
  renderSVSImage <- function (id,imgfile)
  {    
    for (dd in rgl.dev.list()) try(rgl.close())
    open3d(useNULL=TRUE) 
    rgl.viewpoint(theta = 1, phi = -45, fov = 30, zoom = .8, interactive = TRUE)
 ##   rgl.viewpoint(theta = 0, phi = -45, fov = 30, zoom = .75, interactive = TRUE)
    source("svsTree.R",local=TRUE)
    load("treeforms.RData")    
    svs = scan(file=paste0(imgfile),what="character",sep="\n",quiet=TRUE)
    treeform = tolower(unlist(strsplit(unlist(strsplit(svs[2],
               split=" ",fixed=TRUE))[2],split=".",fixed=TRUE))[1])
    treeform = treeforms[[treeform]]
    rcirc = grep ("^#CIRCLE",svs)
    if (length(rcirc)) 
    {
      args = as.numeric(scan(text=svs[rcirc],what="character",quiet=TRUE)[2:4])
cat ("args=",args,"\n")
      circle3D(x0=args[1],y0=args[2],r=args[3],alpha=0.5)
    } else {return()}  # what if it is not a circle???????  
    fireline = grep("^#FIRE_LINE",svs)
cat ("length(fireline)=",length(fireline),"\n")
    if (length(fireline))
    {
      fl = as.numeric(scan(text=substring(svs[fireline],11),what="numeric",quiet=TRUE))
      xx = seq(0,args[3]*2,length.out=length(fl))
      r = sqrt(((xx-args[3])^2) + ((fl-args[3])^2))
      k = r<=args[3]
      if (sum(k)>1)
      {
        nn=500
        fl = fl[k]
        xx = xx[k]
        fireline = matrix(c(xx,fl,rep(0,length(fl))),ncol=3,byrow=FALSE)
        lines3d(fireline,col="red",lwd=4,add=TRUE)
        fls  = approx(xx,fl,rule=2,n=nn)
        fls$y = jitter(fls$y,amount=5)
        fls$z = runif(nn)*3
        fls = matrix(c(fls$x,fls$y,fls$z),ncol=3,byrow=FALSE)
        fls = t(apply(fls,1,function (x) c(x[1]-x[3],x[2],0,x[1],x[2],x[3]*3,
                                           x[1]+x[3],x[2],0)))
        verts = NULL
        for (row in 1:nrow(fls)) 
        {
          tlt=runif(1)*40
          rot=runif(1)*360
          mat = matrix(fls[row,],ncol=3,byrow=TRUE)
          xs = max(mat[,1])-(diff(range(mat[,1]))*.5)
          ys = max(mat[,2])-(diff(range(mat[,2]))*.5)
          zs = max(mat[,3])-(diff(range(mat[,3]))*.5)
          mat[,1] = mat[,1]-xs
          mat[,2] = mat[,2]-ys
          mat[,3] = mat[,3]-zs
          mat = matRotat(mat,tlt,tlt,rot)
          mat[,1] = mat[,1]+xs
          mat[,2] = mat[,2]+ys
          mat[,3] = mat[,3]+zs
          mat[,3] = ifelse(mat[,3]<0,0,mat[,3])
          verts = rbind(verts,mat) 
        } 
        triangles3d(verts,col="red")
      }
    }
    rpols = grep("^RANGEPOLE",svs)
    if (length(rpols))
    {
      poles = c()
      for (line in rpols)
      {
        pole = as.numeric(scan(text=svs[line],what="character",quiet=TRUE)[c(21,22,7)])
        poles = c(poles,c(pole[1:2],0,pole))
      } 
      poles = matrix(poles,ncol=3,byrow=TRUE)
      segments3d(poles,col="red",lwd=4,add=TRUE)
      par3d(ignoreExtent=TRUE) #just use the plot and range poles to define the extent.
    }
    calls = 0
    progress <- shiny::Progress$new(session,min=1,max=length(svs)+4)
    flames = grep("^@flame.eob",svs)
cat("N flames=",length(flames),"\n")
    if (length(flames))
    {
      calls = calls+1
      progress$set(message = "Generate flames",value = calls)
      allv = NULL
      nflsm = 5
      tmp=NULL
      for (fl in svs[flames])
      {
        fdat = as.numeric(scan(text=substring(fl,30),what="numeric",quiet=TRUE))
        # ht,tilt,rotation,width,x,y,z
        fdat = fdat[c(1,2,3,5,15,16,17)]
        names(fdat)=c("ht","tlt","rot","wid","x","y","z")
        tmp=rbind(tmp,fdat[c("x","y","z")])
        hw=fdat["wid"]*.5
        hwr=rnorm(nflsm,hw,.5)
        hwr=ifelse(hwr<(hw*.1),hw*.1,hw)
        ht=fdat["ht"]
        htr=rnorm(nflsm,ht,1)
        htr=ifelse(ht<(htr*.1),ht*.1,ht)
        tlt=runif(nflsm)*2*fdat["tlt"]
        fbr=rnorm(nflsm,fdat["z"],.5)
        fbr=ifelse(fbr<0,0,fbr)
        rot=runif(nflsm)*360
        for (i in 1:nflsm)
        {
#cat ("i=",i," hw=",hwr[i]," ht=",htr[i]," tlt=",tlt[i]," rot=",rot[i],"\n")
          verts = cbind(x=c(-hwr[i],hwr[i],0),
                        y=c(0,0,0),
                        z=c(0,0,htr[i]))
          verts = matRotat(verts,xa=tlt[i],ya=tlt[i],za=rot[i])
          verts[,1]=verts[,1]+fdat["x"]
          verts[,2]=verts[,2]+fdat["y"]
          verts[,3]=verts[,3]+rnorm(1,fbr[i],1)
          allv = rbind(allv,verts)
        }
      }
      triangles3d(allv[,1],allv[,2],allv[,3],col=c("yellow","red")) 
      svs = svs[-flames]
    }
cat("Residual length of svs=",length(svs),"\n")
    drawnTrees = list()
    trees = list()
    for (line in svs)
    {
      calls = calls+1
      progress$set(message = "Generate trees",value = calls)
      c1 = substr(line,1,1) 
      if (c1 == "#" || c1 == ";") next                                                      
      tree = scan(text=line,what="character",quiet=TRUE)
      sp = tree[1]                                                                            
      if (sp == "RANGEPOLE") next
      tree=tree[-1]
      tree = as.numeric(tree)
      names(tree) = c("TrNum","TrCl","CrCl","Stus","DBH","Ht","Lang",
                      "Fang","Edia","Crd1","Cr1","CrD2","Cr2","CrD3","Cr3",
                      "CrD4","Cr4","Ex","Mk","Xloc","Yloc","Z")
      tree = as.list(tree[c(2,3,4,5,6,7,8,10,11,20,21)])
      tree$sp = sp
      ll = matrix(c(tree$Xloc,tree$Yloc,0),nrow=1)
      tree$Xloc = ll[1,1]
      tree$Yloc = ll[1,2]
      drawn = svsTree(tree,treeform)
      if (!is.null(drawn)) drawnTrees[[length(drawnTrees)+1]] = drawn
####TESTING     if (calls > 60) break
    }
    progress$set(message = "Display trees",value = length(svs)+1) 
    displayTrees(drawnTrees)
    progress$set(message = "Sending image to browser",value = length(svs)+2) 
    output[[id]] <- renderRglwidget(rglwidget(scene3d()))
    Sys.sleep(1)
    progress$close()
  }

  observe({
    if (length(input$SVSImgList1))
    {
cat ("SVS3d input$SVSImgList1=",input$SVSImgList1,"\n") 
      if (!file.exists(input$SVSImgList1)) return()
      renderSVSImage('SVSImg1',input$SVSImgList1)
    }
  })
  observe({
    if (length(input$SVSImgList2))
    {
cat ("SVS3d input$SVSImgList2=",input$SVSImgList2,"\n") 
      if (!file.exists(input$SVSImgList2)) return()
      renderSVSImage('SVSImg2',input$SVSImgList2)
    }
  })
 
  observe({
    if (input$topPan == "Maps(alpha)")
    {
cat ("Maps hit\n")
      require(rgdal) 
      allRuns = names(globals$FVS_Runs)
      names(allRuns) = globals$FVS_Runs
      updateSelectInput(session=session, inputId="mapDsRunList", 
        choices=allRuns,selected=0)
      updateSelectInput(session=session, inputId="mapDsTable", choices=list(),
        selected=0)   
      updateSelectInput(session=session, inputId="mapDsVar", choices=list(),
        selected=0) 
      updateSelectInput(session=session, inputId="MapYear", choices=list(),
        selected=0)
      output$leafletMap = renderLeaflet(NULL)
      output$leafletMessage=renderText(NULL)
    }
   })
  observe({
    if (length(input$mapDsRunList))
    {
cat ("mapDsRunList input$mapDsRunList=",input$mapDsRunList,"\n") 
      cases = dbGetQuery(dbGlb$dbOcon,
          paste0("select CaseID from FVS_Cases where KeywordFile = '",
                 input$mapDsRunList,"'"))
      dbExecute(dbGlb$dbOcon,"drop table if exists temp.mapsCases")
      dbWriteTable(dbGlb$dbOcon,DBI::SQL("temp.mapsCases"),data.frame(cases))
      tabs = setdiff(dbGetQuery(dbGlb$dbOcon,"select name from sqlite_master where type='table';")[,1],
                     c("CmpSummary","FVS_Cases","CmpSummary_East"))
      tables = list()
      for (tab in tabs)
      {
        tb <- dbGetQuery(dbGlb$dbOcon,paste0("PRAGMA table_info('",tab,"')"))
        if (length(intersect(c("caseid","standid","year"),tolower(tb$name))) != 3) next
        cnt = try(dbGetQuery(dbGlb$dbOcon,paste0("select count(*) from ",tab,
                 " where CaseID in (select CaseID from temp.mapsCases) limit 1")))
        if (class(cnt) == "try-error") next
        if (cnt[1,1]) tables=append(tables,tab)
      }
      if (length(tables)) names(tables) = tables
      updateSelectInput(session=session, inputId="mapDsTable", choices=tables,
        selected=0)   
      updateSelectInput(session=session, inputId="mapDsVar", choices=list(),
        selected=0) 
      output$leafletMap = renderLeaflet(NULL)
    }
  })
  observe({
    if (length(input$mapDsTable))
    {
cat ("mapDsRunList input$mapDsTable=",input$mapDsTable,"\n")
      vars = setdiff(dbListFields(dbGlb$dbOcon,input$mapDsTable),
         c("CaseID","StandID","Year"))
      vars = as.list(vars)
      names(vars) = vars
      updateSelectInput(session=session, inputId="mapDsVar", choices=vars,
        selected=0) 
      output$leafletMap = renderLeaflet(NULL)
    }
  })
  observe({
    if (length(input$mapDsVar))
    {
      require(rgdal) 
cat ("mapDsRunList input$mapDsTable=",isolate(input$mapDsTable),
     " input$mapDsVar=",input$mapDsVar," input$mapDsType=",input$mapDsType,"\n")
      if (!exists("SpatialData",envir=dbGlb,inherit=FALSE) && 
          file.exists("SpatialData.RData")) load("SpatialData.RData",envir=dbGlb)
      if (!exists("SpatialData",envir=dbGlb,inherit=FALSE)) 
      {
cat ("mapDsRunList trying to use StandInit data\n")
        inInit = getTableName(dbGlb$dbIcon,"FVS_StandInit")
        dbGlb$SpatialData = try(dbGetQuery(dbGlb$dbIcon, 
           paste0("select Stand_ID,Latitude,Longitude from ", inInit)))
        if (class(dbGlb$SpatialData)!="try-error")
        {
          idxLng = grep("Longitude",names(dbGlb$SpatialData),ignore.case=TRUE)
          idxLat = grep("Latitude",names(dbGlb$SpatialData),ignore.case=TRUE)
cat ("mapDsRunList idxLng=",idxLng," idxLat=",idxLat," names=",names(dbGlb$SpatialData),"\n")
          if (length(idxLng) && length(idxLat))
          {
            dbGlb$SpatialData[,idxLng] = as.numeric(dbGlb$SpatialData[,idxLng])
            dbGlb$SpatialData[,idxLat] = as.numeric(dbGlb$SpatialData[,idxLat])
            dbGlb$SpatialData = na.omit(dbGlb$SpatialData)
          } else dbGlb$SpatialData = NULL
          if (is.null(dbGlb$SpatialData) || nrow(dbGlb$SpatialData) == 0)
          {
cat ("mapDsRunList trying PlotInit\n")
            inInit = getTableName(dbGlb$dbIcon,"FVS_PlotInit")
            dbGlb$SpatialData = try(dbGetQuery(dbGlb$dbIcon, 
              paste0("select Stand_ID,avg(Latitude) as Latitude, ",
                     "avg(Longitude) as Longitude from ",inInit," group by Stand_ID;")))
            if (class(dbGlb$SpatialData)!="try-error")
            {
              dbGlb$SpatialData$Longitude = as.numeric(dbGlb$SpatialData$Longitude)
              dbGlb$SpatialData$Latitude  = as.numeric(dbGlb$SpatialData$Latitude)
              dbGlb$SpatialData = na.omit(dbGlb$SpatialData)
              if (nrow(dbGlb$SpatialData) > 0) dbGlb$SpatialData = subset(dbGlb$SpatialData, Latitude != 0 & Longitude != 0)
              if (nrow(dbGlb$SpatialData) == 0) dbGlb$SpatialData = NULL
            }
          }
        }
        if (class(dbGlb$SpatialData) != "data.frame")
        {
          dbGlb$SpatialData = NULL
          output$leafletMessage=renderText("Spatial data needs to be loaded")
          return() 
        } else {
cat ("mapDsRunList names(dbGlb$SpatialData)=",names(dbGlb$SpatialData)," class(dbGlb$SpatialData)=",class(dbGlb$SpatialData),"\n")
          idxLng = grep("Longitude",names(dbGlb$SpatialData),ignore.case=TRUE)
          idxLat = grep("Latitude",names(dbGlb$SpatialData),ignore.case=TRUE)
          idxID  = grep("Stand_ID",names(dbGlb$SpatialData),ignore.case=TRUE)
cat (" idxLng=",idxLng," idxLat=",idxLat," idxID=",idxID,"\n")
          dbGlb$SpatialData[,idxLng] = ifelse(dbGlb$SpatialData[,idxLng]>0, 
                  -dbGlb$SpatialData[,idxLng], dbGlb$SpatialData[,idxLng])
          names(dbGlb$SpatialData)[c(idxID,idxLng,idxLat)] = c("Stand_ID","Longitude","Latitude")
          coordinates(dbGlb$SpatialData) <- ~Longitude+Latitude
          proj4string(dbGlb$SpatialData) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")         
          attr(dbGlb$SpatialData,"MatchesStandID") = names(dbGlb$SpatialData)[idxID]
          output$leafletMessage=renderText(paste0("Spatial data are taken from ",inInit))
        }
      }
      matchVar = attr(dbGlb$SpatialData,"MatchesStandID")
cat ("matchVar=",matchVar,"\n")
      dispData = dbGetQuery(dbGlb$dbOcon,paste0("select * from ",
                isolate(input$mapDsTable)," where CaseID in (select CaseID from temp.mapsCases)"))
      dispData = dispData[,-1]
      keys = setdiff(colnames(dispData),c("StandID","Year"))
      extra = NULL
      for (var in keys) 
      {
        if (class(dispData[,var]) == "character") 
        {
          x = suppressWarnings(as.numeric(dispData[,var]))
          if (!any(is.na(x))) dispData[,var] = x
        }
      }
      extra = NULL
      if (any(table(dispData[,c("StandID","Year")]) > 1)) for (var in keys) 
      {
        if (class(dispData[,var]) == "character") 
        {
          extra = c(extra,var)
          if (all(table(dispData[,c("StandID","Year",extra)]) == 1)) break
        }
      }
      extra = setdiff(extra,input$mapDsVar)
      if (length(extra) > 1) extra = extra[1]
      if (class(dispData[,input$mapDsVar]) == "numeric") dispData[,input$mapDsVar] = 
          format(dispData[,input$mapDsVar],digits=3,scientific=FALSE)
      dispData = dispData[,c("StandID","Year",extra,input$mapDsVar)]
      subset=match(unique(dispData$StandID),dbGlb$SpatialData@data[,matchVar])
      subset=na.omit(subset)
      if (length(subset) == 0) 
      {
        output$leafletMessage=renderText("No StandIDs match polygons")
        return()
      }
      output$textOutput=renderText(paste0(length(subset)," StandIds match polygons"))
      polys = spTransform(dbGlb$SpatialData[subset,],CRS("+init=epsg:4326"))
      uids = unique(dispData$StandID)
      progress <- shiny::Progress$new(session,min=1,max=length(uids))
      labs  = lapply(uids, function (sid)
        {
          tab = subset(dispData,StandID == sid)[,-1]
          progress$set(message = paste0("Preparing ",sid), value = parent.frame()$i)  # <-too tricky, need another approach
          if (input$mapDsType == "table" || any(is.na(as.numeric(tab[,input$mapDsVar]))))
          {
            HTML(paste0("<p style=LINE-HEIGHT:1>StandID=",sid,"<br>",
               paste(names(tab),collapse=" "),"<br>",paste0(apply(tab,1,function (x) 
               paste0(paste0(x,collapse=" "))),collapse="<br>"),"</p>",collapse=""))
          } else {
            tab = subset(dispData,StandID == sid)[,-1]          
            tab[,input$mapDsVar] = as.numeric(tab[,input$mapDsVar]) 
            pfile=paste0("www/s",sid,".png")
cat ("pfile=",pfile," nrow=",nrow(tab)," sid=",sid,"\n")
            png(file=pfile,height=1.7,width=2.3,units="in",res=100) 
            p = ggplot(tab, if (!is.null(extra))
                 aes_string(x="Year",y=input$mapDsVar,linetype=extra) else
                 aes_string(x="Year",y=input$mapDsVar)) +
                 geom_line()+geom_point()+
                 ggtitle(sid)+theme(text = element_text(size=8))
            print(p)
            dev.off()
            pfile=paste0("s",sid,".png")
            HTML(paste0('<img src="',pfile,'?',as.character(as.numeric(Sys.time())),
              '" alt="',sid,'" style="width:229px;height:170px;">'))
         }  
      })
      progress$close()
      map = leaflet(data=polys) %>% addTiles() %>%
              addTiles(urlTemplate = 
                paste0("https://mts1.google.com/vt/lyrs=",input$mapDsProvider,
                       "&hl=en&src=app&x={x}&y={y}&z={z}&s=G"),attribution = 'Google')
      if (class(polys) == "SpatialPointsDataFrame")         
        map = map %>% addCircleMarkers(radius = 6, color="red", 
                        stroke = FALSE, fillOpacity = 0.5, label=labs)  else
        map = map %>% addPolygons(color = "red", weight = 2, smoothFactor = 0.1,
              opacity = .3, fillOpacity = 0.1, label=labs,   
              highlightOptions = c(weight = 5, color = "#666", dashArray = NULL,
                fillOpacity = 0.3, opacity = .6, bringToFront = TRUE))
      output$leafletMap = renderLeaflet(map)
    }
  })


  ## Tools, related to FVSRefresh
  observe({    
    if (input$topPan == "Tools") 
    {
cat ("Tools hit\n") 
      if (exists("fvsBinDir") && !is.null(fvsBinDir) && file.exists(fvsBinDir) &&
          exists("pgmList")) 
      {
        pgmFlip = as.list(names(pgmList))
        names(pgmFlip) = paste0(names(pgmList),": ",unlist(pgmList))
        shlibsufx <- if (.Platform$OS.type == "windows") "[.]dll$" else "[.]so$"
        haveFVS <- dir("FVSbin",pattern=shlibsufx) 
        haveFVSp <- sub(shlibsufx,"",haveFVS)
        avalFVS <- dir(fvsBinDir,pattern=shlibsufx)
        avalFVSp <- sub(shlibsufx,"",avalFVS)
cat ("avalFVSp=",avalFVSp,"\n")  
      } else {
        pgmFlip = list("Refresh not supported on this system.")
        haveFVSp = NULL
      }
      updateSelectInput(session=session, inputId="FVSprograms", 
        choices=pgmFlip,selected=haveFVSp)
      backups = dir (pattern="ProjectBackup")
      if (length(backups)) 
      {
        backups = sort(backups,decreasing=TRUE)
        names(backups) = backups 
      } else backups=list()
      updateSelectInput(session=session, inputId="pickBackup", 
        choices = backups, selected="")
    } 
  })
  
  ## FVSRefresh
  observe({  
    if (input$FVSRefresh == 0) return()               
cat ("FVSRefresh\n")
    isolate({
      if (length(input$FVSprograms) == 0) return()
      shlibsufx <- if (.Platform$OS.type == "windows") ".dll" else ".so"
      i = 0
      if (exists("fvsBinDir") && !is.null(fvsBinDir) && file.exists(fvsBinDir))
      {
        for (pgm in input$FVSprograms)
        {
          frm = paste0(fvsBinDir,pgm,shlibsufx)
          tto = paste0("FVSbin/",pgm,shlibsufx)
cat ("copy frm=",frm," tto=",tto,"\n")
          rtn <- try(file.copy(from=frm,to=tto,overwrite = TRUE))
          if (class(rtn) != "try-error" && rtn) i = i+1
        } 
      } else if (exists("fvsBinURL") && !is.null(fvsBinURL)) 
      {
        for (pgm in input$FVSprograms)
        {
          pgmd=paste0(fvsBinURL,"/", pgm,".zip")
          rtn <- try(download.file(pgmd,paste0(fvsBinDir,"/", pgm,".zip")))
          if (class(rtn) != "try-error")
          {
            unzip(paste0(fvsBinDir,"/", pgm,".zip"), exdir=paste0(fvsBinDir))
            if (file.exists(paste0(fvsBinDir,"/", pgm,shlibsufx))) i = i+1
            unlink (paste0(fvsBinDir,"/", pgm,".zip"))
          }
        }
      } else {
        session$sendCustomMessage(type="infomessage",
                                  message="FVS programs can not be refreshed on this system.")
        return()
      }
      session$sendCustomMessage(type="infomessage",
              message=paste0(i," of ",length(input$FVSprograms),
                              " selected FVS programs refreshed."))                                         
      if (i) 
      {
        globals$reloadAppIsSet=1
        session$reload()
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
cat ("delete run",globals$fvsRun$title," uuid=",globals$fvsRun$uuid,
     " runSel=",input$runSel,"lenRuns=",length(globals$FVS_Runs),"\n")
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
        globals$reloadAppIsSet=1
        session$reload()       
      })
    }
  })
  
  ## deleteAllOutputs
  observe({
    if(input$deleteAllOutputs > 0)
    {
      session$sendCustomMessage(type = "dialogContentUpdate",
        message = list(id = "deleteAllOutputsDlg",
                  message = "Delete all outputs?"))
    }
  })
  observe({  
    if (input$deleteAllOutputsDlgBtn == 0) return()
    isolate({
cat ("delete all outputs\n")
      dbGlb$dbOcon <- dbDisconnect(dbGlb$dbOcon )    
      unlink("FVSOut.db")
      for (uuid in names(globals$FVS_Runs)) removeFVSRunFiles(uuid)
      dbGlb$dbOcon <- dbConnect(dbDriver("SQLite"),"FVSOut.db")    
    })  
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
cat ("delete all runs and outputs\n")
      rmfiles=dir(pattern="[.]pidStatus$")      
      for (tokill in rmfiles) killIfRunning(sub(".pidStatus","",tokill))
      dbGlb$dbOcon <- dbDisconnect(dbGlb$dbOcon)
      unlink("FVSOut.db")
      unlink("FVS_Runs.RData")
      for (uuid in names(globals$FVS_Runs)) removeFVSRunFiles(uuid,all=TRUE)
      dbGlb$dbOcon <- dbConnect(dbDriver("SQLite"),"FVSOut.db")
      globals$saveOnExit = FALSE
      globals$reloadAppIsSet=1
      session$reload()
    })  
  })
  
  ## interfaceRefresh
  observe({
    if(input$interfaceRefresh > 0)
    {
      if (exists("fvsOnlineDir") && !is.null(fvsOnlineDir) && file.exists(fvsOnlineDir) &&
        .Platform$OS.type != "windows") 
      {
        session$sendCustomMessage(type = "dialogContentUpdate",
          message = list(id = "interfaceRefreshDlg",
                    message = "Are you sure?"))
      } else {
        session$sendCustomMessage(type="infomessage",
                message="The interface can not be refreshed on this system.")
      }
    }
  })
  observe({  
    if (input$interfaceRefreshDlgBtn == 0) return()
cat ("interfaceRefreshDlgBtn\n") 
## TODO: set this up so that it works "ONlocal" and with windows.
    if (exists("fvsOnlineDir") && !is.null(fvsOnlineDir) && file.exists(fvsOnlineDir) &&
        .Platform$OS.type != "windows") 
    {
      # shiny code, etc
      needed=paste(paste0(fvsOnlineDir,FVSOnlineNeeded),collapse=" ") 
      cmd = paste0("cp -R ",needed," .")
cat ("interfaceRefreshDlgBtn, cmd = ",cmd,"\n")           
      system (cmd)
      globals$reloadAppIsSet=1
      session$reload()
    } else {
      session$sendCustomMessage(type="infomessage",
              message="The interface can not be refreshed on this system.")
    }
  }) 

  ## delZipBackup
  observe({
    if(input$delZipBackup > 0)
    {
      fl = isolate(input$pickBackup)
      if (is.null(fl)) return()
      if (file.exists(fl))
      {
        unlink(fl)
        backups = dir (pattern="ProjectBackup")
        if (length(backups)) 
        {
          backups = sort(backups,decreasing=TRUE)
          names(backups) = backups 
        } else backups=list()
        updateSelectInput(session=session, inputId="pickBackup", 
          choices = backups, selected="")
      }
    }
  })    
  
  
  ## mkZipBackup
  observe({
    if(input$mkZipBackup > 0)
    {
      zfile=paste0("ProjectBackup_",format(Sys.time(),"%Y-%d-%m_%H_%M_%S"),".zip")
      flst=dir()
      del = grep("^ProjectBackup",flst)
      if (length(del)) flst = flst[-del]
      del = grep ("^FVSbin",flst)
      if (length(del)) flst = flst[-del]
      shlibsufx = if (.Platform$OS.type == "windows") "[.]dll$" else "[.]so$"
      fvsPgms = dir("FVSbin",pattern=shlibsufx)
      fvsPgms = paste0("FVSbin/",fvsPgms)
      flst = c(flst,fvsPgms)
      progress <- shiny::Progress$new(session,min=1,max=length(flst))
      for (i in 1:length(flst))
      {
        x = flst[i]
        progress$set(message = paste0("Adding ",x," to ",zfile), value = i)
        zip(zfile,x)
      }
      Sys.sleep(.2)
      progress$close()
      backups = dir (pattern="ProjectBackup")
      if (length(backups)) 
      {
        backups = sort(backups,decreasing=TRUE)
        names(backups) = backups 
      } else backups=list()
      updateSelectInput(session=session, inputId="pickBackup", 
        choices = backups, selected="")
    }
  })    
  
  ## restorePrjBackup 
  observe({
    if(length(input$restorePrjBackup) && input$restorePrjBackup > 0)
    {
      session$sendCustomMessage(type = "dialogContentUpdate",
        message = list(id = "restorePrjBackupDlg",
                  message = "Are you sure?"))
    }
  })
  observe({  
    if (length(input$restorePrjBackupDlgBtn) && 
        input$restorePrjBackupDlgBtn > 0) 
    {
      isolate({
        fvsWorkBackup = input$pickBackup 
cat ("restorePrjBackupDlgBtn fvsWorkBackup=",fvsWorkBackup,"\n")    
        if (file.exists(fvsWorkBackup)) 
        {
          unzip (fvsWorkBackup)
          globals$reloadAppIsSet=1
          session$reload()
        }
      })
    }
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
  observe(if (input$rpPlotAdd > 0) 
    appendPlotToReport(width =fvsOutData$plotSpecs$width,
                       height=fvsOutData$plotSpecs$width))

  xlsx2html <- function(tab=NULL,xlsxfile="databaseDescription.xlsx")
  {
    if (!file.exists(xlsxfile) || is.null(tab)) return(NULL)
    sheets = getSheetNames(xlsxfile)
    if (tab %in% sheets)
    {
      sdat = read.xlsx(xlsxFile=xlsxfile,sheet=tab)
      html = paste0("<b>",tab,"</b>")
      html = paste0(html,'<p><TABLE border="1"><TR><TH>', 
             paste0(colnames(sdat),collapse="</TH><TH>"),"</TH></TR>\n")
      for (i in 1:nrow(sdat)) html = paste0(html,"<TR><TD>",paste0(as.character(sdat[i,]),
          collapse="</TD><TD>"),"</TD></TR>\n")
      html = paste0(html,"</TABLE><br>")
      return (html)
    } else return (NULL)
  } 
  ##topHelp
  observe({
    if (input$topPan == "Help")
    {
      progress <- shiny::Progress$new(session,min=1,max=12)
      progress$set(message = "Loading Help File", value = 2)
      fn = "fvsOnlineHelp.html"
      help = readChar(fn, file.info(fn)$size) 
      xlsxfile="databaseDescription.xlsx"
      progress$set(message = "Loading Help File", value = 5)
      tabs = try(read.xlsx(xlsxFile=xlsxfile,sheet="OutputTableDescriptions"))
      if (class(tabs)!="try-error")
      {
        morehtml=xlsx2html(tab="OutputTableDescriptions")
        for (tab in tabs$Table) morehtml=paste0(morehtml,xlsx2html(tab=tab))  
        if (!is.null(morehtml)) help = sub(x=help,fixed=TRUE,
                pattern="**OUTPUTHTML**",replacement=morehtml)
      }
      progress$set(message = "Loading Table Descriptions", value = 8)
      tabs = try(read.xlsx(xlsxFile=xlsxfile,sheet="InputTableDescriptions"))
      if (class(tabs)!="try-error")                                                         
      {
        morehtml=xlsx2html(tab="InputTableDescriptions")
        for (tab in tabs$Table) morehtml=paste0(morehtml,xlsx2html(tab=tab))            
        if (!is.null(morehtml)) help = sub(x=help,fixed=TRUE,
                pattern="**INPUTHTML**",replacement=morehtml)
      }
      output$uiHelpText <- renderUI(HTML(help))
      progress$close()
    }
  })
  
  ##tabDescSel
  observe({
    tab = input$tabDescSel
cat ("tabDescSel, tab=",tab,"\n")
    html = NULL
    if (!is.null(tab) && nchar(tab)>0 && file.exists("databaseDescription.xlsx"))
    {
      sheets = getSheetNames("databaseDescription.xlsx")
      if ("OutputTableDescriptions" %in% sheets)
      {
        tabs = read.xlsx(xlsxFile="databaseDescription.xlsx",sheet="OutputTableDescriptions")
        row = match(tab,tabs[,1])
        html = paste0("<b>",tab,"</b> ",tabs[row,2])
        if (tab %in% sheets) 
        {
          sdat = read.xlsx(xlsxFile="databaseDescription.xlsx",sheet=tab)[,c(1,4)]
          html = paste0(html,'<p><TABLE border="1"><TR><TH>', 
                   paste0(colnames(sdat),collapse="</TH><TH>"),"</TH></TR>\n")
          for (i in 1:nrow(sdat))
            html = paste0(html,"<TR><TD>",paste0(as.character(sdat[i,]),
                     collapse="</TD><TD>"),"</TD></TR>\n")
          html = paste0(html,"</TABLE>")
        }
      }
    }
    output$tabDesc <- renderUI(HTML(html))
  })

  
  ##### data upload code  
  observe({
    if(input$topPan == "Import Data")
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
    session$sendCustomMessage(type = "dialogContentUpdate",
            message = list(id = "installTrainDBDlg",
            message = "Training database installed. Do you want to create a new run now?"))
    # output$replaceActionMsg <- renderText("Regional Training database installed")
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
    initNewInputDB()
    loadVarData(globals,prms,dbGlb$dbIcon)                                              
  }) 
  ## installEmptyDB
  observe({  
    if (input$installEmptyDB == 0) return()
    dbDisconnect(dbGlb$dbIcon)
    if (file.exists("FVS_Data.db")) file.remove("FVS_Data.db")
    file.copy("FVS_Data.db.empty","FVS_Data.db",overwrite=TRUE)
    session$sendCustomMessage(type = "dialogContentUpdate",
            message = list(id = "installEmptyDBDlg",
            message = "Empty database installed. Do you want to start inputting data?"))
    # output$replaceActionMsg <- renderText("Empty database installed")
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
    initNewInputDB()
    loadVarData(globals,prms,dbGlb$dbIcon)                                              
  }) 
  ## Upload new database
  observe({
    if (is.null(input$uploadNewDB)) return()
    fext = tools::file_ext(basename(input$uploadNewDB$name))
cat ("fext=",fext,"\n")
    session$sendCustomMessage(type="jsCode",
                          list(code= "$('#input$installNewDB').prop('disabled',true)"))
    session$sendCustomMessage(type="jsCode",
                          list(code= "$('#input$addNewDB').prop('disabled',true)"))
    if (! (fext %in% c("accdb","mdb","db","sqlite","xlsx","zip"))) 
    {
      output$replaceActionMsg  = renderText("Uploaded file is not suitable database types described in Step 1.")
      unlink(input$uploadNewDB$datapath)
      return()
    } else {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#installNewDB').prop('disabled',true)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#addNewDB').prop('disabled',true)"))
    }
    fdir = dirname(input$uploadNewDB$datapath)
    if (fext == "zip") 
    {
      unzip(input$uploadNewDB$datapath, junkpaths = TRUE, exdir = fdir)
      unlink(input$uploadNewDB$datapath)
      fname = dir(dirname(input$uploadNewDB$datapath))
      if (length(fname)>1) 
      {
        output$replaceActionMsg = renderText(".zip contains more than one file.")
        lapply (dir(dirname(input$uploadNewDB$datapath),full.names=TRUE),unlink)
        return()
      } else if (length(fname) == 0) {
        output$actionMsg = renderText(".zip was empty.")
        return()
      } 
      fext = tools::file_ext(fname)
      if (! (fext %in% c("accdb","mdb","db","sqlite","xlsx"))) 
      {
        output$replaceActionMsg = renderText(".zip did not contain one of the suitable file types described in Step 1.")
        lapply (dir(dirname(input$uploadNewDB$datapath),full.names=TRUE),unlink)
        return()
      }
    } else fname = basename(input$uploadNewDB$datapath)
cat ("fext=",fext," fname=",fname," fdir=",fdir,"\n")
    standNT = try(read.xlsx(xlsxFile="databaseDescription.xlsx",sheet="FVS_StandInit"))
    standNT = if (class(standNT) == "try-error") NULL else apply(standNT[,c(1,3)],2,toupper)
    treeNT = try(read.xlsx(xlsxFile="databaseDescription.xlsx",sheet="FVS_TreeInit"))
    treeNT = if (class(treeNT) == "tre-error") NULL else apply(treeNT[,c(1,3)],2,toupper)
    plotNT = try(read.xlsx(xlsxFile="databaseDescription.xlsx",sheet="FVS_PlotInit"))
    plotNT = if (class(treeNT) == "tre-error") NULL else apply(plotNT[,c(1,3)],2,toupper)
    curDir=getwd()
    setwd(fdir)
    if (fext %in% c("accdb","mdb"))
    {      
      progress <- shiny::Progress$new(session,min=1,max=12)
      progress$set(message = "Process schema", value = 2)
cat("curDir=",curDir," input dir=",getwd(),"\n") 
      cmd = if (.Platform$OS.type == "windows") 
         shQuote(paste0("java -jar ",curDir,"/access2csv.jar ",
              fname," --schema"),type="cmd2") else
         paste0("java -jar '",curDir,"/access2csv.jar' ",
              fname," --schema")
cat ("cmd=",cmd,"\n")
      schema = system(cmd,intern = TRUE)
      if (!exists("schema") || length(schema) < 2) 
      {
        setwd(curDir) 
        progress$close()     
        output$replaceActionMsg = renderText("'schema' not created, no data loaded.")
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
          cknt = NULL
          if      (length(grep("FVS_STANDINIT",tblName,ignore.case=TRUE))) cknt = standNT
          else if (length(grep("FVS_STANDINIT_COND",tblName,ignore.case=TRUE))) cknt = standNT
          else if (length(grep("FVS_STANDINIT_PLOT",tblName,ignore.case=TRUE))) cknt = standNT
          else if (length(grep("FVS_TREEINIT", tblName,ignore.case=TRUE))) cknt = treeNT
          else if (length(grep("FVS_TREEINIT_COND", tblName,ignore.case=TRUE))) cknt = treeNT
          else if (length(grep("FVS_TREEINIT_PLOT", tblName,ignore.case=TRUE))) cknt = treeNT
          else if (length(grep("FVS_PLOTINIT", tblName,ignore.case=TRUE))) cknt = plotNT
          else if (length(grep("FVS_PLOTINIT_PLOT", tblName,ignore.case=TRUE))) cknt = plotNT
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
         shQuote(paste0("java -jar ",curDir,"/access2csv.jar ",fname),type="cmd2") else
         paste0("java -jar '",curDir,"/access2csv.jar' ", fname)
cat ("cmd=",cmd,"\n")       
      system(cmd)  
      progress$set(message = "Import schema to Sqlite3", value = 4) 
      cmd = paste0("sqlite3 ","FVS_Data.db < schema")
cat ("cmd=",cmd,"\n")
      if (.Platform$OS.type == "windows") shell(cmd) else system(cmd)
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
cat ("s=",s," cmd=",cmd,"; ")
        if (.Platform$OS.type == "windows") shell(cmd) else system(cmd)
cat ("cmd done.\n")
      }
      dbo = dbConnect(dbDrv,"FVS_Data.db")
    } else if (fext == "xlsx") {
      sheets = getSheetNames(fname)
      progress <- shiny::Progress$new(session,min=1,max=length(sheets)+3)
      i = 0
      normNames = c("FVS_GroupAddFilesAndKeywords","FVS_PlotInit",                
                    "FVS_StandInit","FVS_TreeInit")
      dbo = dbConnect(dbDrv,"FVS_Data.db")
      for (sheet in sheets)
      {
        i = i+1
cat ("sheet = ",sheet," i=",i,"\n")
        progress$set(message = paste0("Processing sheet ",i," name=",sheet), value=i)
        sdat = read.xlsx(xlsxFile=fname,sheet=sheet)
        im = grep(sheet,normNames,ignore.case=TRUE)
        if (!is.na(im)) sheet = normNames[im]
        NT = switch(sheet,"FVS_StandInit"=standNT,"FVS_TreeInit"=treeNT,
                          "FVS_PlotInit"=plotNT,NULL) 
        if (!is.null(NT))
        {
          std = pmatch(toupper(names(sdat)),NT[,1])        
          for (icol in 1:length(sdat))
          {
            if (!is.na(std[icol])) sdat[,icol] = 
              switch(NT[std[icol],2],
              "TEXT"    = as.character(sdat[,icol]),
              "REAL"    = as.numeric  (sdat[,icol]),
              "INTEGER" = as.integer  (sdat[,icol]))
          }
        }
        dbWriteTable(conn=dbo,name=sheet,value=sdat)
      }
    } else {
      i = 0
      file.rename(from=fname,to="FVS_Data.db")
      dbo = dbConnect(dbDrv,"FVS_Data.db")
      progress <- shiny::Progress$new(session,min=1,max=3)
    }
    tabs = dbGetQuery(dbo,"select name from sqlite_master where type='table';")[,1]
    # get rid of "NRIS_" part of names if any
    for (tab in tabs)
    {
cat("loaded table=",tab,"\n")      
      nn = sub("NRIS_","",tab)
      if (nchar(nn) && nn != tab) dbExecute(dbo,paste0("alter table ",tab," rename to ",nn))
    }
    tabs = dbGetQuery(dbo,"select name from sqlite_master where type='table';")[,1]
    rowCnts = unlist(lapply(tabs,function (x) dbGetQuery(dbo,
      paste0("select count(*) as ",x," from ",x,";"))))
    msg = lapply(names(rowCnts),function(x) paste0(x," (",rowCnts[x]," rows)"))
    msg = paste0("Uploaded data: ",paste0(msg,collapse="; "))
    session$sendCustomMessage(type = "infomessage", message = msg)
    # output$replaceActionMsg = renderText(msg)
    dbGlb$newFVSData = tempfile()
    file.copy(from="FVS_Data.db",to=dbGlb$newFVSData,overwrite=TRUE)
    dbDisconnect(dbo)
    session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#installNewDB').prop('disabled',false)"))
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#addNewDB').prop('disabled',false)"))
    setwd(curDir)
    progress$close()    
  })
  ## installNewDB
  observe({
    if (input$installNewDB == 0) return()
    if (is.null(dbGlb$newFVSData)) return()
    dbDisconnect(dbGlb$dbIcon)
    file.copy(dbGlb$newFVSData,"FVS_Data.db",overwrite=TRUE)
    unlink(dbGlb$newFVSData)
    dbGlb$newFVSData=NULL
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
    tabs = dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[,1]
    progress <- shiny::Progress$new(session,min=1,max=length(tabs)+1)
    i = 0
    for (tb in tabs)
    {
      i = i+1
      progress$set(message = paste0("Setting up index for table ",tb), value=i)
      if (tolower(tb) == "fvs_climattr") 
      {
        rtn = try(dbExecute(dbGlb$dbIcon,"drop index if exists StdScnIndex"))
        if (class(try)!="try-error") 
        {
          qry = "create index StdScnIndex on FVS_ClimAttrs (Stand_ID, Scenario);"
cat ("index creation, qry=",qry,"\n")
          try(dbExecute(dbGlb$dbIcon,qry))
        }
      }else if (tolower(tb) == "fvs_standinit_cond" || tolower(tb) == "fvs_treeinit_cond")
        {
        tbidx = grep(tb,c("FVS_StandInit_Cond","FVS_TreeInit_Cond"),ignore.case=TRUE)                 
        if (length(tbidx))
        {
          tbinx = paste0("idx",tb)
          rtn = try(dbExecute(dbGlb$dbIcon,paste0("drop index if exists ",tbinx)))
          if (class(try)!="try-error") 
          {
            qry = paste0("create index ",tbinx," on ",tb," (Stand_ID);")
            cat ("index creation, qry=",qry,"\n")
            try(dbExecute(dbGlb$dbIcon,qry))
          }
        }
      }else if (tolower(tb) == "fvs_standinit_plot")
      {
        tbidx = grep(tb,c("FVS_StandInit_Plot","FVS_TreeInit_Plot"),ignore.case=TRUE)                 
        if (length(tbidx))
        {
          tbinx = paste0("idx",tb)
          rtn = try(dbExecute(dbGlb$dbIcon,paste0("drop index if exists ",tbinx)))
          if (class(try)!="try-error") 
          {
            qry = paste0("create index ",tbinx," on ",tb," (Stand_ID);")
            cat ("index creation, qry=",qry,"\n")
            try(dbExecute(dbGlb$dbIcon,qry))
          }
        }
      }else if (tolower(tb) == "fvs_plotinit_plot")
      {
        tbidx = grep(tb,c("FVS_PlotInit_Plot"),
                     ignore.case=TRUE)                 
        if (length(tbidx))
        {
          tbinx = paste0("idx",tb)
          rtn = try(dbExecute(dbGlb$dbIcon,paste0("drop index if exists ",tbinx)))
          if (class(try)!="try-error") 
          {
            qry = paste0("create index ",tbinx," on ",tb," (StandPlot_ID);")
            cat ("index creation, qry=",qry,"\n")
            try(dbExecute(dbGlb$dbIcon,qry))
          }
        }
      }
      else if (tolower(tb) == "fvs_treeinit_plot")
      {
        tbidx = grep(tb,c("FVS_TreeInit_Plot"),
                     ignore.case=TRUE)                 
        if (length(tbidx))
        {
          tbinx = paste0("idx",tb)
          rtn = try(dbExecute(dbGlb$dbIcon,paste0("drop index if exists ",tbinx)))
          if (class(try)!="try-error") 
          {
            qry = paste0("create index ",tbinx," on ",tb," (StandPlot_ID);")
            cat ("index creation, qry=",qry,"\n")
            try(dbExecute(dbGlb$dbIcon,qry))
          }
        }
      }else if (tolower(tb) == "fvs_plotinit")
      {
        tbidx = grep(tb,c("FVS_PlotInit","FVS_TreeInit"),
                     ignore.case=TRUE)                 
        if (length(tbidx))
        {
          tbinx = paste0("idx",tb)
          rtn = try(dbExecute(dbGlb$dbIcon,paste0("drop index if exists ",tbinx)))
          if (class(try)!="try-error") 
          {
            qry = paste0("create index ",tbinx," on ",tb," (StandPlot_ID);")
            cat ("index creation, qry=",qry,"\n")
            try(dbExecute(dbGlb$dbIcon,qry))
          }
        }
      }else {
        tbidx = grep(tb,c("FVS_StandInit","FVS_TreeInit"),
                     ignore.case=TRUE)                 
        if (length(tbidx))
        {
          tbinx = paste0("idx",tb)
          rtn = try(dbExecute(dbGlb$dbIcon,paste0("drop index if exists ",tbinx)))
          if (class(try)!="try-error") 
          {
            qry = paste0("create index ",tbinx," on ",tb," (Stand_ID);")
cat ("index creation, qry=",qry,"\n")
            try(dbExecute(dbGlb$dbIcon,qry))
          }
        }
      }
    }
    progress$set(message = "Checking database query keywords", value = i+1)
    fixFVSKeywords(dbGlb,progress) 
    msg = checkMinColumnDefs(dbGlb,progress)
    session$sendCustomMessage(type = "dialogContentUpdate",
            message = list(id = "installNewDBDlg",
            message = paste0(msg,". Do you want to create a new run now?")))
    # output$replaceActionMsg <- renderText(msg)
    loadVarData(globals,prms,dbGlb$dbIcon)
    initNewInputDB()
    progress$close()
  }) 
  ## addNewDB
  observe({  
    if (input$addNewDB == 0) return()
    if (is.null(dbGlb$newFVSData)) return() 
    # set an exclusive lock on the database
    dbExecute(dbGlb$dbIcon,"PRAGMA locking_mode = EXCLUSIVE")
    trycnt=0
    while (TRUE)
    {
      trycnt=trycnt+1
      if (trycnt > 1000) 
      {
        dbExecute(dbGlb$dbIcon,"PRAGMA locking_mode = NORMAL")
        return()
      }
cat ("try to get exclusive lock on input database, trycnt=",trycnt,"\n");
      rtn <- try(dbExecute(dbGlb$dbIcon,"create table dummy (dummy int)"))
      if (class(rtn) != "try-error") break;
      Sys.sleep (10)
    } 
    dbExecute(dbGlb$dbIcon,"drop table if exists dummy")    
    oldInds = dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='index';")[,1]
    for (idx in oldInds) dbExecute(dbGlb$dbIcon,paste0("drop index if exists ",idx,";"))
    oldtabs = dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[,1]
    dbo = dbConnect(dbDrv,dbGlb$newFVSData)
    newtabs = dbGetQuery(dbo,"select name from sqlite_master where type='table';")[,1]
    progress <- shiny::Progress$new(session,min=1,max=length(newtabs)*2+1)
    i = 0
    dbDisconnect(dbo)
    attach = try(dbExecute(dbGlb$dbIcon,paste0("attach `",dbGlb$newFVSData,"` as addnew;")))
    if (class(attach) == "try-error")
    {
      output$replaceActionMsg <- renderText("New data could not be loaded")
      unlink(dbGlb$newFVSData)
      dbGlb$newFVSData=NULL
    }
    justNew = setdiff(newtabs,oldtabs)
    dbBegin(dbGlb$dbIcon)
    for (tab in justNew) 
    {
      i=i+1
      progress$set(message = paste0("Loading ",tab), value = i)
      dbExecute(dbGlb$dbIcon,paste0("insert into ",tab," select * from addnew.",tab))
    }
    newtabs = setdiff(newtabs,justNew)
    for (tab in newtabs)
    {
      i=i+1
      progress$set(message = paste0("Loading ",tab), value = i)
      if (tolower(tab) %in% c("fvs_standinit","fvs_treeInit","fvs_plotInit"))
      {
        dbExecute(dbGlb$dbIcon,paste0("delete from ",tab," where Stand_ID in ",
                    "(select Stand_ID from addnew.",tab,")"))
      } else if (tolower(tab) == "fvs_groupaddfilesandkeywords") {
        dbExecute(dbGlb$dbIcon,paste0("delete from ",tab," where Groups in ",
                    " (select Groups from addnew.",tab,")"))
      }
      # homogenize table structure and then do the insert from ...
      newTdef = dbGetQuery(dbGlb$dbIcon,paste0("pragma addnew.table_info(",tab,")"))
      trgTdef = dbGetQuery(dbGlb$dbIcon,paste0("pragma        table_info(",tab,")"))
      newTdef$lcname = tolower(newTdef$name)
      trgTdef$lcname = tolower(trgTdef$name)
      missingInTrg = setdiff(newTdef$lcname,trgTdef$lcname)
      missingIndx  = match(missingInTrg,newTdef$lcname)
      if (length(missingIndx) && !any(is.na(missingIndx)))
      {
        for (i in missingIndx)
        {
          qry = paste0("alter table ",tab," add column ",newTdef$name[i],
                " ",newTdef$type[i],";")
cat ("homogenize qry=",qry,"\n")
          dbExecute(dbGlb$dbIcon,qry)
        }
      }
      alln = paste0(newTdef$name,collapse=",")
      qry = paste0("insert into ",tab," (",alln,") select ",alln,
                   " from addnew.",tab,";") 
cat ("homogenize qry=",qry,"\n")
      dbExecute(dbGlb$dbIcon,qry)
    }
    dbCommit(dbGlb$dbIcon)
    dbExecute(dbGlb$dbIcon,paste0("detach addnew;"))
    unlink(dbGlb$newFVSData)
    dbGlb$newFVSData=NULL
    tabs = dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[,1]
    i = i+1
    progress$set(message = "Setting up indices", value=i)
    for (tb in tabs)
    {
      i = i+1
      progress$set(message = paste0("Setting up index for table ",tb), value=i)
      if (tolower(tb) == "fvs_climttr") 
      {
        dbExecute(dbGlb$dbIcon,"drop index if exists StdScnIndex")
        dbExecute(dbGlb$dbIcon,"create index StdScnIndex on FVS_ClimAttrs (Stand_ID, Scenario);")
      } else if (tolower(tb) != "fvs_groupaddfilesandkeywords") {
        tbinx = paste0("idx",tb)
        dbExecute(dbGlb$dbIcon,paste0("drop index if exists ",tbinx))
        dbExecute(dbGlb$dbIcon,paste0("create index ",tbinx," on ",tb," (Stand_ID);"))
      }
    }
    dbExecute(dbGlb$dbIcon,"PRAGMA locking_mode = NORMAL")
    rowCnts = unlist(lapply(tabs,function (x) dbGetQuery(dbGlb$dbIcon,
      paste0("select count(*) as ",x," from ",x,";"))))
    msg = lapply(names(rowCnts),function(x) paste0(x," (",rowCnts[x]," rows)"))
    msg = paste0("New database: ",paste0(msg,collapse="; "))
    session$sendCustomMessage(type = "dialogContentUpdate",
            message = list(id = "installNewDBDlg",
            message = paste0(msg,". Do you want to create a new run now?")))
    # output$replaceActionMsg <- renderText(msg)
    loadVarData(globals,prms,dbGlb$dbIcon) 
    initNewInputDB()
    progress$close()
  }) 
  observe({
    if(input$inputDBPan == "Upload and add new rows to existing tables (.csv)") 
    {
cat ("Upload new rows\n")
      tbs <- dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[,1]
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
      output$replaceActionMsg <- renderText(if (length(tbs)) "" else 
        "No tables in existing database. Use 'Replace existing' to install a new one.")        
      initNewInputDB()
    }
  })
  ## uploadStdTree
  observe({  
    if (is.null(input$uploadStdTree)) return()
    isolate({ 
      indat = try(read.csv(file=input$uploadStdTree$datapath,as.is=TRUE))
      unlink(input$uploadStdTree$datapath)
      if (class(indat) == "try-error" || is.null(indat) || nrow(indat)==0)
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
      cols = na.omit(charmatch(tolower(colnames(indat)),
             tolower(names(dbGlb$tbsCTypes[[input$uploadSelDBtabs]]))))
      if (length(cols) == 0) 
      {
        output$uploadActionMsg = renderText(paste0("No columns match what is defined for ",
               input$uploadSelDBtabs,", no data loaded."))
        Sys.sleep(1)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      }
      addCols = attr(cols,"na.action")
cat ("addCols=",addCols,"\n")
      if (length(addCols))
      {
        for (icol in addCols)
        {
          dtyp = switch(class(indat[,icol]),
                 "integer" = "int",
                 "numeric" = "real",
                 "character")
          newVar = names(indat)[icol]
          qry = paste0("alter table ",input$uploadSelDBtabs," add column ",
                newVar," ",dtyp,";")
cat ("add column qry=",qry,"\n")
          added = try(dbExecute(dbGlb$dbIcon,qry))
          if (class(added) != "try-error")
          {
            v = dtyp == "character"
            names(v) = newVar
            dbGlb$tbsCTypes[[input$uploadSelDBtabs]] = c(dbGlb$tbsCTypes[[input$uploadSelDBtabs]],v)
          }
        }
      }
      cols = na.omit(charmatch(tolower(colnames(indat)),
             tolower(names(dbGlb$tbsCTypes[[input$uploadSelDBtabs]]))))
      types = dbGlb$tbsCTypes[[input$uploadSelDBtabs]][cols]
      req = switch(tolower(input$uploadSelDBtabs),
         fvs_standinit = c("Stand_ID","Variant","Inv_Year"),
         fvs_treeinit  = c("Stand_ID","Species","DBH"),
         fvs_groupaddfilesandkeywords = c("Groups"),
         NULL)
      if (!is.null(req) && !all(req %in% names(types)))
      {
        output$uploadActionMsg = renderText(paste0("Required columns were missing for ",
               input$uploadSelDBtabs,", no data loaded."))
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      }
      quote = types[types]   
      if (length(quote)) for (cn in names(quote)) 
      {
        if (class(indat[,cn]) != "character") indat[,cn] = as.character(indat[,cn])
      } 
      for (cn in colnames(indat))  
        if (class(indat[,cn]) == "character") indat[,cn] = paste0("'",indat[,cn],"'")

      dbBegin(dbGlb$dbIcon)
      err = FALSE
      insertCount = 0
      for (i in 1:nrow(indat))
      {
        row = indat[i,,drop=FALSE]
        row = row[,!is.na(row),drop=FALSE]
        if (ncol(row) == 0) next
        row = row[,row != "'NA'",drop=FALSE]
        if (ncol(row) == 0) next
        qry = paste0("insert into ",input$uploadSelDBtabs," (",
                paste0(colnames(row),collapse=","),
                  ") values (",paste0(row[1,],collapse=","),");")
cat ("insert qry=",qry,"\n")
        res = try(dbExecute(dbGlb$dbIcon,qry))
        if (class(res) == "try-error") {err=TRUE; break} else insertCount = insertCount+1
      }
      if (err) 
      {
        dbRollback(dbGlb$dbIcon) 
        output$uploadActionMsg = renderText(paste0("Error processing: ",qry))
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      } else {
cat ("insertCount=",insertCount,"\n")
        if (insertCount)
        {
          dbCommit(dbGlb$dbIcon)
          output$uploadActionMsg = renderText(paste0(insertCount," row(s) inserted into ",
                 isolate(input$uploadSelDBtabs)))
        }
        rm (indat)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        loadVarData(globals,prms,dbGlb$dbIcon)                                              
      }
      Sys.sleep(1)
      session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
      # this section removes records that have missing values for standID or group
      keyCol = NULL
      if (length(grep("standinit",input$uploadSelDBtabs,ignore.case=TRUE)) ||
          length(grep("treeinit", input$uploadSelDBtabs,ignore.case=TRUE)) ||
          length(grep("plotinit", input$uploadSelDBtabs,ignore.case=TRUE)))
               keyCol = "Stand_ID"
      if (length(grep("GroupAddFilesAndKeywords",input$uploadSelDBtabs,
                 ignore.case=TRUE))) keyCol = "Groups"
      if (!is.null(keyCol))
      {
        # the key column must not be null, if it is delete the rows.
        try(dbExecute(dbGlb$dbIcon,paste0("delete from ",
            input$uploadSelDBtabs," where ",keyCol," is null")))
        # update the stand selector list if it exists and if we are not doing groups
        if (keyCol != "Groups")
        {
          dbGlb$sids = dbGetQuery(dbGlb$dbIcon,paste0("select distinct Stand_ID from ",
                                  input$uploadSelDBtabs))[,1]
          if (any(is.na(dbGlb$sids))) dbGlb$sids[is.na(dbGlb$sids)] = ""
          if (dbGlb$rowSelOn && length(dbGlb$sids)) 
            updateSelectInput(session=session, inputId="rowSelector",
              choices  = as.list(dbGlb$sids), selected=unique(indat[,"Stand_ID"])) else 
            output$stdSel <- mkStdSel(dbGlb)
          
          qry <- paste0("select _ROWID_,* from ",input$uploadSelDBtabs)
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
        }
      }
      session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
      initNewInputDB()
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
    climTab <- dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[,1]
    if (!("FVS_ClimAttrs" %in% climTab))
    {
cat ("no current FVS_ClimAttrs\n")
      progress$set(message = "Building FVS_ClimAttrs table",value = 4) 
      dbWriteTable(dbGlb$dbIcon,"FVS_ClimAttrs",climd)
      output$actionMsg = renderText("FVSClimAttrs created.")
      rm (climd)
      progress$set(message = "Creating FVS_ClimAttrs index",value = 6)
      dbExecute(dbGlb$dbIcon,'drop index if exists StdScnIndex')
      dbExecute(dbGlb$dbIcon,"create index StdScnIndex on FVS_ClimAttrs (Stand_ID, Scenario);")
      progress$set(message = "Done", value = 9)
      Sys.sleep (.5)
      progress$close()
      return()      
    }
cat ("current FVS_ClimAttrs\n")
    if (file.exists("FVSClimAttrs.db")) unlink("FVSClimAttrs.db")
    dbclim <- dbConnect(dbDrv,"FVSClimAttrs.db")
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
      dbExecute(dbIcon,paste0('delete from FVS_ClimAttrs where Stand_ID = "',
         x[1],'" and Scenario = "',x[2],'"'))
    }, dbGlb$dbIcon)
    dbCommit(dbGlb$dbIcon)
    dbExecute(dbGlb$dbIcon,'drop index if exists StdScnIndex')
    
    dbExecute(dbGlb$dbIcon,'attach database "FVSClimAttrs.db" as new')
    # get the table:
    progress$set(message = "Inserting new data",value = 8)    
    oldAttrs = dbGetQuery(dbGlb$dbIcon,'select * from FVS_ClimAttrs limit 1;')
    if (nrow(oldAttrs) == 0) 
    {
cat ("simple copy from new, all rows were deleted\n")
      dbExecute(dbGlb$dbIcon,'drop table FVS_ClimAttrs')
      dbExecute(dbGlb$dbIcon,'insert into FVS_ClimAttrs select * from new.FVS_ClimAttrs')
    } else {
      newAttrs = dbGetQuery(dbGlb$dbIcon,'select * from new.FVS_ClimAttrs limit 1;')
      if (identical(colnames(oldAttrs),colnames(newAttrs)))
      {
cat ("simple insert from new, all cols are identical\n")
        dbExecute(dbGlb$dbIcon,'insert into FVS_ClimAttrs select * from new.FVS_ClimAttrs')
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
          for (mis in newmiss) dbExecute(dbGlb$dbIcon,
            paste0('alter table new.FVS_ClimAttrs add "',mis,'" real'))
          dbCommit(dbGlb$dbIcon)
        }
cat ("length(oldmiss)=",length(oldmiss),"\n")
        if (length(oldmiss) > 0)
        {
          dbExecute(dbGlb$dbIcon,'alter table FVS_ClimAttrs rename to oldClimAttrs')
          dbBegin(dbGlb$dbIcon)
          for (mis in oldmiss) dbExecute(dbGlb$dbIcon,
            paste0('alter table oldClimAttrs add "',mis,'" real'))
          dbCommit(dbGlb$dbIcon)
          dbExecute(dbGlb$dbIcon,
            paste0('create table FVS_ClimAttrs as select ',selnew,' from oldClimAttrs'))
          dbExecute(dbGlb$dbIcon,'drop table oldClimAttrs')
        }     
        dbExecute(dbGlb$dbIcon,
          paste0('insert into FVS_ClimAttrs select ',selnew,' from new.FVS_ClimAttrs'))
      }
    }
    dbExecute(dbGlb$dbIcon,'detach database new')   
    unlink("FVSClimAttrs.db")
    progress$set(message = "Recreating FVS_ClimAttrs index",value = 9)
    dbExecute(dbGlb$dbIcon,'drop index if exists StdScnIndex')
    dbExecute(dbGlb$dbIcon,"create index StdScnIndex on FVS_ClimAttrs (Stand_ID, Scenario);")
    progress$set(message = "Done", value = 10)
    output$uploadActionMsg = renderText("FVSClimAttrs updated.")
    Sys.sleep (2)
    session$sendCustomMessage(type = "resetFileInputHandler","climateFVSUpload")
    progress$close()
  })  
  
  observe({
    if(input$inputDBPan == "View and edit existing tables") 
    {
cat ("dataEditor View and edit existing tables\n")
      tbs <- dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[,1]
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
      idx <- grep ("StandInit",tbs,ignore.case=TRUE)
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
          paste0("select distinct Stand_ID from ",dbGlb$tblName))[,1]
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
      html=NULL
      tabs = try(read.xlsx(xlsxFile="databaseDescription.xlsx",sheet="InputTableDescriptions"))
      if (class(tabs) != "try-error")
      {
        row = charmatch(toupper(input$editSelDBtabs),toupper(tabs[,1]))
        if (!is.na(row))
        {
          tab = tabs[row,1]
          html = paste0("<b>",tab,"</b> ",tabs[row,2])
          sdat = try(read.xlsx(xlsxFile="databaseDescription.xlsx",sheet=tab))
          if (class(sdat) != "try-error")
          {
            html = paste0(html,'<p><TABLE border="1"><TR><TH>', 
                     paste0(colnames(sdat),collapse="</TH><TH>"),"</TH></TR>\n")
            for (i in 1:nrow(sdat))
              html = paste0(html,"<TR><TD>",paste0(as.character(sdat[i,]),
                       collapse="</TD><TD>"),"</TD></TR>\n")
            html = paste0(html,"</TABLE>")
          }
        }
      }
      output$inputTabDesc <- renderUI(HTML(html))
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
          stdSearch = trim(input$editStandSearch)
          if (nchar(stdSearch)>0) 
          {
            keep = try(grep (stdSearch,dbGlb$tbl$Stand_ID))
            if (class(keep) != "try-error" && length(keep)) dbGlb$tbl = dbGlb$tbl[keep,]
          }
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
        dd = lapply(input$tbl$params$data,function (jj) 
                    lapply(jj,function(x) if (is.null(x)) NA else x))
        inputTbl = matrix(unlist(dd),
                   ncol=length(input$tbl$params$columns),byrow=TRUE)
        inputTbl[inputTbl=="NA"] = NA
        colnames(inputTbl) = unlist(input$tbl$params$colHeaders)
        rownames(inputTbl) = unlist(input$tbl$params$rowHeaders)
        switch(input$mode,
          "New rows"= 
          {
            inserts <- mkInserts(inputTbl,dbGlb$tblName,
                                 dbGlb$tbsCTypes[[dbGlb$tblName]])                                                          
            if (length(inserts)) 
            {
              dbBegin(dbGlb$dbIcon)
              err = FALSE
              for (ins in inserts) 
              {
                res = try(dbExecute(dbGlb$dbIcon,ins))
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
                res = try(dbExecute(dbGlb$dbIcon,qry))
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
                res = try(dbExecute(dbGlb$dbIcon,qry))              
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
              dbGlb$sids = dbGetQuery(dbGlb$dbIcon,paste0("select distinct Stand_ID from ",
                                dbGlb$tblName))[,1]
              if (any(is.na(dbGlb$sids))) dbGlb$sids[is.na(dbGlb$sids)] = ""
              if (dbGlb$rowSelOn && length(dbGlb$sids)) 
                updateSelectInput(session=session, inputId="rowSelector",
                  choices  = dbGlb$sids) else 
                output$stdSel <- mkStdSel(dbGlb)
            }  

            qry <- paste0("select _ROWID_,* from ",dbGlb$tblName)
            qry <- if (length(grep("Stand_ID",dbGlb$tblCols)) && 
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
    reloadStandSelection()
  })

  
  observe(if (input$clearTable > 0) 
  {
cat ("clearTable, tbl=",dbGlb$tblName,"\n")
    dbExecute(dbGlb$dbIcon,paste0("delete from ",dbGlb$tblName))
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

##### Map data mapUpload
   observe({
    if(input$inputDBPan == "Map data") 
    {
cat ("Map data hit.\n")
      require(rgdal) 
      progress <- shiny::Progress$new(session,min=1,max=3)
      progress$set(message = "Preparing projection library",value = 2)
      updateSelectInput(session=session, inputId="mapUpIDMatch",choices=list())
      if (!exists("prjs",envir=dbGlb,inherit=FALSE)) 
      {
        dbGlb$prjs = make_EPSG()
        delList = c("Unknown","deprecated","Unable to","Unspecified","Paris","China",
        "Oslo","NZGD","Kalianpur","Hartebeesth","ELD79","Sierra Le","Locodjo","ETRS89",
        "Xian 1980","Italy","GDM2000","KKJ ","Karbala","North Pole","LGD2006","JAD2","GDA94",
        "HTRS96","Bermuda","Pitcairn","Cuba ","Kertau","Portug","Brunei","Jakarta","Abidjan",
        "Chile","Russia","Japan","Israel","Nahrwan","Fiji","Viti L","PRS92","MAGNA-","Banglade",
        "Minna","poraloko","Sahara","Zanderij","MGI","Ain el","Afgooye","Barbados","Carthage",
        "Luzon","Maroc","Massawa","Schwarzeck","Tanana","Timbalai","OSNI","Irish","Trinidad",
        "Voirol","Yoff","Belge ","Tokyo","British","Amersfoort","Lao ","Yemen ","Brazil",
        "Indian","Indonesia","Garoua","Fahud","Egypt","Deir ez","Corrego","Cape /","Hong Kong",
        "Bogota","Camacupa","Beijing","Batavia","Aratu","Adindan","Pulkovo","Lisbon","Hanoi",
        "Macedonia","Cayman","Arctic","Europe","Krovak","Panama","Sibun G","Ocotepeque",
        "Peru","DRUKREF","TUREF","Korea","Spain","Congo","Katanga","Manoca","LKS9","Tahiti",
        "Argentina","Iraq","Slovenia","Naparima","Mauritania","Maupiti","Martinique","Estonian",
        "Qatar","Doulas","Easter","Qornoq","Rassad","Miquelon","Segara","Tahhaa","Singapore")
        dbGlb$prjs <- dbGlb$prjs[!is.na(dbGlb$prjs[,3]),]
        for (del in delList)
        {
          tod = grep(del,dbGlb$prjs[,2],ignore.case=TRUE)
# cat ("del=",del," len=",length(tod)," nrow=",nrow(dbGlb$prjs),"\n")
          if (length(tod)) dbGlb$prjs = dbGlb$prjs[-tod,]
        }         
      }
      dbGlb$prjs = dbGlb$prjs[order(dbGlb$prjs[,2]),]
      grp = c(grep ("NAD",dbGlb$prjs[,2],fixed=TRUE),grep("WGS",dbGlb$prjs[,2],fixed=TRUE))
      dbGlb$prjs = rbind(dbGlb$prjs[grp,],dbGlb$prjs[-grp,])
      updateSelectInput(session=session, inputId="mapUpLayers", choices=list(),
                        selected=0)
      epsg = as.character(1:nrow(dbGlb$prjs))
      names(epsg) = paste0("epsg:",dbGlb$prjs$code," ",dbGlb$prjs$note)
      updateSelectInput(session=session, inputId="mapUpSelectEPSG", choices=epsg,
                        selected=0)
      updateTextInput(session=session, inputId="mapUpProjection",value="")
      output$mapActionMsg = renderText(" ")
      progress$close()
    }
   })
   observe({
    if(is.null(input$mapUpload)) return()
    {
cat ("mapUpload\n")
      progress <- shiny::Progress$new(session,min=1,max=3)
      if (file.exists(input$mapUpload$datapath))
      {
        fileEnding = tolower(tools::file_ext(basename(input$mapUpload$datapath)))
cat ("mapUpload, filename=",input$mapUpload$datapath," ending=",fileEnding,"\n")
        if (fileEnding != "zip") 
        {
          output$mapActionMsg = renderText(paste0("Upload a .zip file"))
          progress$close()
          return()
        }
        mapDir = paste0(dirname(input$mapUpload$datapath),"/mapData")
        unlink(mapDir,recursive=TRUE)
        dir.create(mapDir)
        file.copy(from=input$mapUpload$datapath,to=mapDir)
        zipName = basename(input$mapUpload$datapath)
        unlink(input$mapUpload$datapath)
        progress$set(message = "Unzipping",value = 1)
        curdir = getwd()
        setwd(mapDir)
        unzip(zipName)
        unlink(zipName)
        if (length(dir(mapDir)) > 1) mapDir = dirname(mapDir)
        progress$set(message = "Getting layers",value = 2)
        if (length(dir(mapDir)) > 1) mapDir = dirname(mapDir)
        setwd(mapDir)
        lyrs = try(ogrListLayers(dir(mapDir)))
cat ("mapUpload, class(lyrs)=",class(lyrs),"\n")
        setwd(curdir)                                
        if (class(lyrs) == "try-error" || length(lyrs) == 0)
        {
          output$mapActionMsg = renderText("Can not find layers in data")
          progress$close()
          return()
        }
        attributes(lyrs) = NULL
        lyrs = as.list(lyrs)
        names(lyrs) = unlist(lyrs)
        if (length(lyrs) > 1) 
        {
          lyr = grep ("poly",names(lyrs),ignore.case=TRUE)
          if (length(lyr) == 0 || any(is.na(lyr))) lyr = 1
          if (length(lyr) > 1) lyr = lyr[which.min(nchar(names(lyrs)[lyr]))]
          lyr = names(lyrs)[lyr]
        } else lyr = lyrs[1]
        lyr = unlist(lyr)                
        updateSelectInput(session=session, inputId="mapUpLayers", choices=lyrs,
                          selected=lyr)
        progress$close() 
      }
    }
   })
   observe({
      if (is.null(input$mapUpLayers)) return()
      datadir = dirname(isolate(input$mapUpload$datapath))
      if (!dir.exists(datadir)) return()
      curdir = getwd()
      setwd(datadir)
cat ("input$mapUpLayers =",input$mapUpLayers,"\n")
      datadir = dir()
      if (length(dir(datadir)) == 1) setwd(datadir)
      progress <- shiny::Progress$new(session,min=1,max=3)
      progress$set(message = paste0("Loading map: ",datadir," Layer: ",input$mapUpLayers),value=2)
      txtoutput = capture.output(dbGlb$spd <- try(readOGR(dir(),input$mapUpLayers)))
      setwd(curdir)
      if (class(dbGlb$spd) == "try-error")
      {
        output$mapActionMsg = renderText(paste0("Map read error: ",dbGlb$spd))
        progress$close()
        setwd(curdir)
        return()
      }
      for (col in colnames(dbGlb$spd@data))
      {
        if (is.factor(dbGlb$spd@data[,col])) 
          dbGlb$spd@data[,col]=levels(dbGlb$spd@data[,col])[as.numeric(dbGlb$spd@data[,col])]
      }
      txtoutput = paste0(txtoutput,collapse="\n")
      output$mapActionMsg = renderText(txtoutput)
      progress$set(message = txtoutput,value=3)
      choices = as.list(names(dbGlb$spd@data))
      names(choices) = choices
      stdInit = getTableName(dbGlb$dbIcon,"FVS_StandInit")
      ids = try(dbGetQuery(dbGlb$dbIcon,paste0('select Stand_ID from ',stdInit)))
cat ("length(ids)=",length(ids),"\n")
      if (class(ids) == "try-error" || nrow(ids) == 0)
      {
        selected = grep("ID",names(dbGlb$spd@data),ignore.case=TRUE)[1]
        selected = if (is.na(selected)) 0 else names(dbGlb$spd@data)[selected]
      } else {
        ids = unlist(ids)
        cnts = NULL
        for (col in colnames(dbGlb$spd@data))
          cnts = c(cnts,length(na.omit(match(ids,dbGlb$spd@data[,col]))))
        cnts = cnts/length(ids)*100
        choices = paste0(choices," ",format(cnts,digits=3),"%")
        selected = choices[which.max(cnts)]
      }
cat ("input$mapUpLayers, number of layers (choices)=",length(choices)," selected=",selected,"\n")
      updateSelectInput(session=session, inputId="mapUpIDMatch",
          choices=choices,selected=selected)
      prj = proj4string(dbGlb$spd)
      if (!is.na(prj)) 
      {
        updateTextInput(session=session, inputId="mapUpProjection",value=prj)
        i = grep (prj,dbGlb$prjs$prj4,fixed=TRUE)
        if(length(i) && !is.na(i))
          updateSelectInput(session=session, inputId="mapUpSelectEPSG",selected=i)
      }
      progress$close()
   })
   observe({
     if(length(input$mapUpSelectEPSG))
       updateTextInput(session=session, inputId="mapUpProjection",
            value=dbGlb$prjs[as.numeric(input$mapUpSelectEPSG),"prj4"])
   })
   observe({
    if(input$mapUpSetPrj > 0)
    {
      if (!exists("spd",envir=dbGlb,inherit=FALSE)) 
      {
        output$mapActionMsg = renderText("No map, upload one")
        return()
      }
      prjstring = trim(isolate(input$mapUpProjection))
      if (nchar(prjstring) == 0) 
      {
        output$mapActionMsg = renderText("proj4 string is empty")
        return()
      }
      prj = try(CRS(prjstring))
      if (class(prj) == "try-error") 
      {
        output$mapActionMsg = renderText("proj4 string is not valid")
      } else {
        proj4string(dbGlb$spd) = prjstring
        output$mapActionMsg = renderText("proj4 set/reset")
      }
    }
   })
   observe({
    if(input$mapUpSave > 0)
    {
      if (!exists("spd",envir=dbGlb,inherit=FALSE)) 
      {
        output$mapActionMsg = renderText("No map to save.")
        return()
      }
      stdInit = getTableName(dbGlb$dbIcon,"FVS_StandInit")
      ids = try(dbGetQuery(dbGlb$dbIcon,paste0('select Stand_ID from ',stdInit)))
      ids = unlist(ids)
      names(ids) = NULL
      matID = unlist(strsplit(input$mapUpIDMatch," "))[1]
      keep=na.omit(match(ids,dbGlb$spd@data[,matID]))
      if (length(keep))
      {
        SpatialData = dbGlb$spd[keep,]
        rm (spd,envir=dbGlb)    
        attr(SpatialData,"MatchesStandID") =  matID
        save (SpatialData,file="SpatialData.RData")
        dbGlb$SpatialData = SpatialData
        output$mapActionMsg = renderText(paste0("Map saved for this project, StandID match=",
                              matID,", Number of objects kept=",nrow(SpatialData@data)))
      } else {
        output$mapActionMsg = renderText(paste0("Map not saved, no objects match Stand_ID"))
      }
    }
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

  ## Projects hit
  observe({    
    if (input$toolsPan == "Manage projects") 
    {
cat ("Projects hit\n")
    dirs = list.dirs("..",recursive=FALSE)
    selChoices = list()
    for (dir in dirs)
    {
      if (file.exists(paste0(dir,"/server.R")) && 
          file.exists(paste0(dir,"/ui.R"))     &&
          file.exists(paste0(dir,"/projectId.txt"))) selChoices = append(selChoices,dir)
    }
    names(selChoices) = gsub("../","",selChoices)
    sel = match(basename(getwd()),names(selChoices))
    sel = if (is.na(sel)) NULL else selChoices[[sel]]
    updateSelectInput(session=session, inputId="PrjSelect", 
        choices=selChoices,selected=sel)
    }
  })

 
  ## Make New Project (PrjNew)
  observe(if (length(input$PrjNew) && input$PrjNew > 0) 
  {
    isolate({
      progress <- shiny::Progress$new(session,min=1,max=12)
      progress$set(message = "Saving current run",value = 1)
      saveRun()
      curdir = getwd()
      basedir = basename(curdir)
      setwd("../")
      fn = if (nchar(input$PrjNewTitle)) input$PrjNewTitle else basedir
      fn = mkFileNameUnique(fn)
      dir.create(fn)
      if (!file.exists(fn)) 
      {
        setwd(curdir)
        updateTextInput(inputId=PrjNewTitle,session=session,value="")
        return()
      }
      progress$set(message = "Copying project files",value = 5)
      filesToCopy = paste0(curdir,"/",dir(curdir))
      del = grep(".log$",filesToCopy)
      if (length(del)) filesToCopy = filesToCopy[-del]
      del = grep(pattern=".db$",filesToCopy)
      if (length(del)) filesToCopy = filesToCopy[-del]
      del = grep(pattern=".pidStatus$",filesToCopy)
      if (length(del)) filesToCopy = filesToCopy[-del]
      del = grep(pattern="FVS_Runs.RData",filesToCopy)
      if (length(del)) filesToCopy = filesToCopy[-del]
      del = grep(pattern=".key$",filesToCopy)
      if (length(del)) filesToCopy = filesToCopy[-del]
      del = grep(pattern=".out$",filesToCopy)
      if (length(del)) filesToCopy = filesToCopy[-del]
      file.copy(from=filesToCopy,to=fn,recursive=TRUE)
      setwd(fn)     
      file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
      unlink("projectId.txt")
      cat ("title= ",fn,"\n",file="projectId.txt")
      progress$set(message = "Saving new prohect",value = 9)
      for (uuid in names(globals$FVS_Runs)) removeFVSRunFiles(uuid,all=TRUE)
      if (exists("dbOcon",envir=dbGlb,inherit=FALSE)) try(dbDisconnect(dbGlb$dbOcon))
      if (exists("dbIcon",envir=dbGlb,inherit=FALSE)) try(dbDisconnect(dbGlb$dbIcon))
      globals$saveOnExit = FALSE
      globals$reloadAppIsSet=1
      progress$close()
      session$reload()                        
    })
  })    

  observe(if (length(input$PrjSwitch) && input$PrjSwitch > 0) 
  {
cat("PrjSwitch to=",input$PrjSelect,"\n")
    isolate({
      if (dir.exists(input$PrjSelect))
      {
        saveRun()        
        if (exists("dbOcon",envir=dbGlb,inherit=FALSE)) try(dbDisconnect(dbGlb$dbOcon))
        if (exists("dbIcon",envir=dbGlb,inherit=FALSE)) try(dbDisconnect(dbGlb$dbIcon))
        setwd(input$PrjSelect)
        if (isLocal()){
          file.copy(paste0("C:/FVSOnlocal/",basename(input$PrjSelect),"/projectId.txt"),
                    "C:/FVSOnlocal/lastAccessedProject.txt",overwrite=TRUE)
        }
        globals$saveOnExit = FALSE
        globals$reloadAppIsSet=1
        session$reload()
      }
    })
  })
  
                                      
  saveRun <- function() 
  {
    isolate({
      saveTheRun = nchar(trim(input$title)) > 1
      if (saveTheRun) 
      {
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
      }
      # remove excess images that maybe created in Maps.
      delList = dir ("www",pattern="^s.*png$",full.names=TRUE)
      if (length(delList)) lapply(delList,function(x) unlink(x))  
cat ("leaving saveRun, saveTheRun=",saveTheRun,"\n") 
    }) 
  }
   
})

