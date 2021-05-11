# $Id: server.R 3417 2021-03-03 22:36:27Z mshettles521 $

# The top of this file contains several objects loaded into the .GlobalEnv
# prior to the shinyApp call.


#' Run fvsOL (FVS OnLine/OnLocal). 
#'
#' @param prjDir the name of the directory containing an fvsOL project.
#' @param runUUID the uuid of the run that should be opened when the system starts, 
#'   if NULL or not found in the list of runs, it is ignored.
#' @param fvsBin the name of the directory containing the FVS load libraries for the platform
#' @param shiny.trace turns on tracing for shiny, see shiny documentation
#' @param logToConsole controls if the log is output to the console or the log file,
#'   the default set by the interactive() function.
#' @return the shiny app.
#' @export
fvsOL <- function (prjDir=NULL,runUUID=NULL,fvsBin=NULL,shiny.trace=FALSE,
                   logToConsole=interactive())                                         
{
  if (!is.null(prjDir) && dir.exists(prjDir)) setwd(prjDir)
  if (is.null(fvsBin) || !dir.exists(fvsBin)) 
  {
    if (dir.exists("FVSbin")) fvsBin="FVSbin" else stop("fvsBin must be set")
  }
  fvsBin <<- fvsBin
  runUUID <<- runUUID
  logToConsole <<- logToConsole
  
  cat ("FVSOnline/OnLocal function fvsOL started.\n")
  
  addResourcePath("colourpicker-lib/js", 
    system.file("www/shared/colourpicker/js", package="colourpicker"))
  addResourcePath("colourpicker-lib/css", 
    system.file("www/shared/colourpicker/css",package="colourpicker"))
  addResourcePath("colourpicker-binding", 
    system.file("srcjs",package="colourpicker"))
  addResourcePath("FVSlogo.png", 
    system.file("extdata","www/FVSlogo.png",package="fvsOL"))
  addResourcePath("message-handler.js", 
    system.file("extdata","www/message-handler.js",package="fvsOL"))
  if (!dir.exists ("www")) dir.create("www")
  addResourcePath("www",file.path(".","www"))
  
  # set shiny.trace=TRUE for reactive tracing 
  options(shiny.maxRequestSize=10000*1024^2,shiny.trace=shiny.trace,
          rgl.inShiny=TRUE,rgl.useNULL=TRUE)                          
        
  data (prms)
  data (treeforms)

  cat ("Starting shinyApp.\n")
   
  shinyApp(FVSOnlineUI, FVSOnlineServer, options=list(launch.browser=TRUE))
}


mkfvsStd <- setRefClass("fvsStd",
  fields = list(sid = "character", rep = "numeric", repwt = "numeric", 
   invyr = "character", grps = "list", cmps = "list",uuid="character"))
                                       
mkfvsGrp <- setRefClass("fvsGrp",
  fields = list(grp = "character", cmps = "list", uuid="character"))
                          
mkfvsCmp <- setRefClass("fvsCmp",
  fields = list(kwds = "character", kwdName = "character", exten="character",
    title="character", variant="character",uuid="character", atag="character", 
    reopn="character"))
# atag is always "c" if the component is a condition, "k" if it is a keyword
# component that is not attached to a specific component. If it is longer than 1
# character it is the uuid of the related condition

mkfvsRun <- setRefClass("fvsRun", 
  fields = list(stands = "list", grps = "list", simcnts = "list",
    selsim = "list", FVSpgm = "character", title = "character", 
    startyr = "character", endyr = "character", cyclelen = "character",
    cycleat = "character", refreshDB = "character", uuid="character",
    defMgmtID = "character", autoOut = "list", runScript = "character" ,
    uiCustomRunOps = "list", startDisp = "character"))

mkfvsOutData <- 
  setRefClass("fvsOutData", 
    fields = list(dbLoadData = "list", dbData = "data.frame", 
      dbVars = "character", browseVars = "character", 
      dbSelVars = "character", browseSelVars = "character",
      runs = "character", plotSpecs = "list", 
      render = "data.frame"))

mkGlobals <- setRefClass("globals", 
  fields = list(activeFVS = "list", activeVariants = "character", 
    activeExtens = "character", schedBoxYrLastUsed = "character",
    extnsel = "character", kwdsel = "list", mgmtsel = "list",
    mevsel = "list", mmodsel = "list", pastelist = "list",fvsBin="character",
    pastelistShadow = "list", inData = "list", FVS_Runs = "list",
    customCmps = "list", selStds = "character", currentCmdDefs="character",
    schedBoxPkey = "character", currentCmdPkey = "character",GrpNum="numeric",
    currentCndPkey = "character", winBuildFunction = "character",GenGrp="list", 
    existingCmps = "list",currentQuickPlot = "character", 
    currentEditCmp = "fvsCmp", NULLfvsCmp = "fvsCmp", saveOnExit= "logical",
    customQueries = "list", fvsRun = "fvsRun", foundStand="integer", 
    reloadAppIsSet = "numeric", hostname= "character", toggleind="character",
    selStandTableList = "list",kcpAppendConts = "list",opencond="numeric",
    condKeyCntr="numeric",prevDBname="list",changeind="numeric",timeissue="numeric",
    lastRunVar="character",gFreeze="logical",importItems="list",
    settingChoices="list",exploreChoices="list",simLvl="list",stdLvl="list",
    specLvl="list",dClsLvl="list",htClsLvl="list",treeLvl="list",tbsFinal="list",
    selRuns = "character", selUuids = "character",selAllVars="logical",
    explorePass="numeric")) 

isLocal <- function () Sys.getenv('SHINY_PORT') == ""

# cbbPalette is used in the graphics
cbbPalette <- c("#FF0000","#009E73","#0072B2","#E69F00","#CC79A7","#0000FF",
                "#D55E00","#8F7800","#D608FA","#009100","#CF2C73","#00989D",
                "#00FF00","#BAF508","#202020","#6B6B6A","#56B4E9","#20D920")

extnslist <- list(
  "Base FVS system"="base",
  "Cover Model"="cover",
  "Full Establishment Model"="estb",
  "Partial Establishment Model"="strp",
  "Database Extension"="dbs",
  "Economic Analysis Extension"="econ",
  "Dwarf Mistletoe Impact Model"="mist",
  "ORGANON in FVS"="organon",
  "Fire and Fuels Extension"="fire",
  "Climate-FVS Extension"="climate",
  "WRD (Annosus Root Disease)"="ardwrd3",
  "WRD (Armillaria Root Disease)"="armwrd3",
  "WRD (Laminated Root Rot)"="phewrd3")

pgmList <- list(                                
  FVSak = "Southeast AK - Coastal BC",
  FVSbm = "Blue Mountains, Oregon",
  FVSca = "Inland CA, Southern Cascades",
  FVSci = "Central ID",
  FVScr = "Central Rockies",       
  FVSec = "East Cascades, Washington",
  FVSem = "Eastern Montana",
  FVSie = "Inland Empire",
  FVSnc = "Klammath Mountains, Northern CA",
  FVSoc = "ORGANON Southwest",
  FVSop = "ORGANON Pacific Northwest",
  FVSso = "South Central OR N CA",
  FVStt = "Tetons, Wyoming",
  FVSut = "Utah",
  FVSwc = "West Cascades",
  FVSpn = "Pacific Northwest Coast",
  FVSws = "Western Sierra Nevada, CA",
  FVScs = "Central States",
  FVSkt = "Kootenai/Kaniksu/Tally LK, ID - MT",
  FVSls = "Lake States",
  FVSne = "Northeast",
  FVSsn = "Southern") 
                          
options(rgl.useNULL=TRUE)

trim <- function (x) gsub("^\\s+|\\s+$","",x)

defaultRun <- list("Default useful for all FVS variants"="fvsRun")

# used in Tools, dlZipSet
zipList <- list(  
  "FVSProject data base (Runs, Custom components (kcp), Custom queries, GraphSettings)" = "fvsProjdb",
  "Output data base for for all runs"  = "outdb",  
  "Keyword file for current run" = "key",  
  "FVS output file for current run" = "out",  
  "SVS output files for current run" = "subdir",  
  "Input data base FVS_Data.db" = "FVS_Data", 
  "Spatial data (SpatialData.RData)" = "SpatialData")  
selZip <- unlist(zipList[1:4])  

# if "runScripts.R" exists in the project directory, then use it, otherwise load
# the version that is part of the package software.
rsf <- "runScripts.R"
if (file.exists(rsf)) source(rsf) else source(system.file("extdata", rsf, package = "fvsOL"))
runScripts <- if (exists("customRunScripts") && length(customRunScripts)) 
               append(x=customRunScripts,after=0,defaultRun) else defaultRun
 
customRunElements = list(                  
  selectInput("runScript",
              "Select run script (normally, use the default)",
              choices=runScripts,
              selected="fvsRun",multiple=FALSE,selectize=FALSE),
  uiOutput("uiCustomRunOps"))
                                                        
FVSOnlineServer <- function(input, output, session) 
{  
cat ("FVSOnline/OnLocal interface server start\n")

  # set serverDate to be the release date using packageVersion
  serverDate=as.character(packageVersion("fvsOL"))
  serverDate=unlist(strsplit(serverDate,".",fixed=TRUE))
  for (i in 2:3) if (nchar(serverDate[i])==1) serverDate[i]=paste0("0",serverDate[i])
  serverDate=paste0(serverDate,collapse="")

cat ("ServerDate=",serverDate,"\n")

  if (!logToConsole) 
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
                                                     
cat ("FVSOnline/OnLocal interface server start, serverDate=",serverDate,"\n")
  
  withProgress(session,
  {  
    setProgress(message = "Start up", 
                detail  = "Loading scripts and settings", value = 1)
                
    globals <- mkGlobals$new(saveOnExit=TRUE,reloadAppIsSet=0,
               gFreeze=FALSE,fvsBin=fvsBin)
    dbGlb <- new.env()
    dbGlb$tbl <- NULL
    dbGlb$navsOn <- FALSE            
    dbGlb$rowSelOn <- FALSE
    dbGlb$disprows <- 20
    if (file.exists("projectIsLocked.txt"))  
    {
cat ("Project is locked.\n")
      output$appLocked<-renderUI(HTML(paste0('<h4 style="color:#FF0000">',
        'Warning: This project may already be opened.</h4>',
        '<h5>Insure the project is not opened in another window.</h5>',
        '<button id="clearLock" type="button" class="btn btn-default ',
        'action-button">Clear this message and proceed</button>',
        '&nbsp;&nbsp;&nbsp;&nbsp;<button id="exitNow" type="button" ',
        'class="btn btn-default action-button">Exit now</button><h3></h3>')))
    } else cat (file="projectIsLocked.txt",date(),"\n") 
    setProgress(message = "Start up",value = 2)
    
    nruns = mkFVSProjectDB()
    dbGlb$prjDB = connectFVSProjectDB()

    if (nruns==0)
    {   
      globals$fvsRun <- mkfvsRun$new(uuid=uuidgen(),title="Run 1")
      resetGlobals(globals,FALSE)
      storeFVSRun(dbGlb$prjDB,globals$fvsRun)
    } 
    globals$FVS_Runs = getFVSRuns(dbGlb$prjDB)
    #update a couple of list buttons with the list of tables             
    xlsxFile=system.file("extdata", "databaseDescription.xlsx", package = "fvsOL")
    if (file.exists(xlsxFile))
    {
      if ("OutputTableDescriptions" %in% getSheetNames(xlsxFile))
        tabs = read.xlsx(xlsxFile=xlsxFile,sheet="OutputTableDescriptions")[,1]
      tableList <- as.list(sort(c("",tabs)))
      updateSelectInput(session=session, inputId="tabDescSel2",choices=tableList,
          select=tableList[[1]])
      updateSelectInput(session=session, inputId="tabDescSel",,choices=tableList,
          select=tableList[[1]])
    }        

    setProgress(message = "Start up",
                detail  = "Loading interface elements", value = 3)
    output$serverDate=renderText(HTML(paste0("Release date<br>",serverDate,"<br>",
        if (isLocal()) "Local" else "Online"," configuration"))) 
    tit=NULL
    pfexists = file.exists("projectId.txt")
    if (!pfexists || (pfexists && file.size("projectId.txt") < 2))
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
    mkSimCnts(globals$fvsRun,sels=globals$fvsRun$selsim)
    resetGlobals(globals,TRUE)
    selChoices = globals$FVS_Runs
cat ("Setting initial selections, length(selChoices)=",length(selChoices),"\n")
    runUUID = if (!is.null(runUUID) && runUUID %in% selChoices) runUUID else selChoices[[1]]
    updateSelectInput(session=session, inputId="runSel", 
        choices=selChoices,selected=runUUID)
    updateTextInput(session=session, inputId="title", value=names(selChoices[1]))
    if (exists("fvsOutData")) rm (fvsOutData) 
    fvsOutData <- mkfvsOutData$new(plotSpecs=list(res=144,height=4,width=6))

    dbDrv <- dbDriver("SQLite")
    dbGlb$dbOcon <- dbConnect(dbDrv,"FVSOut.db")  
    
    loadObject(dbGlb$prjDB,"stdstkParms")  
    if (exists("stdstkParms"))
    {
      val = stdstkParms$sdskwdbh
      if (!is.na(val)) updateNumericInput(session=session,inputId="sdskwdbh",
         value=val)
      val = stdstkParms$sdskldbh
      if (!is.na(val)) updateNumericInput(session=session, inputId="sdskldbh",
         value=val)
    }
          
    # the default SpatialData is distributed with the package, install it if it
    # is not in the project directory.
    if (!file.exists("FVS_Data.db"))
    {
      frm=system.file("extdata", "FVS_Data.db.default", package = "fvsOL")
      file.copy(frm,"FVS_Data.db",overwrite=TRUE)
      frm=system.file("extdata", "SpatialData.RData.default", package = "fvsOL")
      file.copy(frm,"SpatialData.RData",overwrite=TRUE)
    }
    globals$changeind <- 0
    output$contChange <- renderUI("Run")    
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
    
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
    if (length(globals$importItems))
    {
      if (attr(globals$importItems,"temp")) unlink(attr(globals$importItems,"dir"),recursive = TRUE)
      globals$importItems=list()
    }    

    if (globals$saveOnExit) 
    {
      saveRun(input,session)
      FVS_Runs = globals$FVS_Runs
      stdstkParms = isolate(list("sdskwdbh"=input$sdskwdbh,
                                 "sdskldbh"=input$sdskldbh)) 
      storeOrUpdateObject(dbGlb$prjDB,stdstkParms)
      prjIdTxt = "projectId.txt"
      if (file.exists(prjIdTxt))  # this is done to update the modification time.
      {
        prjid = scan(prjIdTxt,what="character",sep="\n",quiet=TRUE)
        write(file=prjIdTxt,prjid)
      }
    }
    unlink ("projectIsLocked.txt")  
    # remove excess images that may be created in Maps.
    delList = dir ("www",pattern="*png$",full.names=TRUE)
    if (length(delList)) lapply(delList,function(x) unlink(x))  
    #note: the stopApp function returns to the R process that called shinyApp()
    if (globals$reloadAppIsSet == 0) stopApp()
    globals$reloadAppIsSet == 0
  })
  
  ## clearLock, exitNow
  observe({
    if (!is.null(input$clearLock) && input$clearLock==0)
    {
      withProgress(session, {  
        for (i in 1:5)
        {
          setProgress(message = "5 second delay  ", 
                detail  = paste(i,"of 5"), value = i)
          Sys.sleep(1)
        }
        setProgress(value = NULL)          
      }, min=1, max=10)
    }
  })
  observe({
    if (!is.null(input$exitNow) && input$exitNow>0)
    {
      globals$saveOnExit=FALSE
      session$sendCustomMessage(type = "closeWindow"," ")
    }
  })
  observe({
    if (!is.null(input$clearLock) && input$clearLock>0)
    {
      output$appLocked<-NULL
      # remake the lock file.
      cat (file="projectIsLocked.txt",date(),"\n")
    }
  })
      
  ## Load
  observe({
    if (input$topPan == "View Outputs" && input$leftPan == "Load")
    {
      globals$selAllVars=FALSE
cat ("View Outputs & Load\n")
      initTableGraphTools(globals,session,output,fvsOutData)
      tbs <- myListTables(dbGlb$dbOcon)     
      if (length(tbs) > 0 && !is.na(match("FVS_Cases",tbs)))
      {
        runsdf = dbGetQuery(dbGlb$dbOcon,
          paste0("Select RunTitle,KeywordFile from FVS_Cases group by KeywordFile ",
                 "having min(RunDateTime) order by RunDateTime desc;"))
        fvsOutData$runs = runsdf$KeywordFile
        names(fvsOutData$runs) = runsdf$RunTitle
      }
      tableList <- list()
      dbd=system.file("extdata", "databaseDescription.xlsx", package = "fvsOL")
      if (file.exists(dbd))
      {
        if ("OutputTableDescriptions" %in% getSheetNames(dbd))
          tabs = read.xlsx(xlsxFile=dbd,sheet="OutputTableDescriptions")[,1]
        tableList <- as.list(sort(c("",tabs)))
      }

      updateSelectInput(session, "tabDescSel2", choices = tableList, selected=1)
      updateSelectInput(session, "runs", choices = fvsOutData$runs, selected=0)
    }
  })

    ## runs output run selection
  observe({
    if (input$leftPan != "Load") return()
cat ("runs, run selection (load) input$runs=",input$runs,"\n")
    if (!is.null(input$runs)) # will be a list of run keywordfile names (uuid's)
    {
      tbs <- myListTables(dbGlb$dbOcon)
cat ("tbs related to the run",tbs,"\n")
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
            myListTables(dbGlb$dbOcon) #any query will cause the locking mode to become active
            setProgress(value = NULL)
            return()
          }
cat ("try to get exclusive lock, trycnt=",trycnt,"\n");
          rtn <- try(dbExecute(dbGlb$dbOcon,"create table dummy (dummy int)"))
          if (class(rtn) != "try-error") break;
          Sys.sleep (10)
        }
cat ("have exclusive lock\n")
        dbExecute(dbGlb$dbOcon,"drop table if exists dummy")
        # create a temp.Cases table that is a list of CaseIDs 
        # associated with the selected runs. These two items are used to 
        # filter records selected from selected tables.
        qry = paste0("create table temp.Cases as select _RowID_,CaseID ",
                     "from FVS_Cases where FVS_Cases.KeywordFile in ",
                     paste0("('",paste(input$runs,collapse="','"),"')"))
cat("qry=",qry,"\n")
        dbExecute(dbGlb$dbOcon,"drop table if exists temp.Cases")
        rtn = dbExecute(dbGlb$dbOcon,qry)
cat("rtn from create temp.Cases=",rtn,"\n")
        for (tb in tbs) 
        {                     
cat ("tb=",tb,"\n")                             
          cnt = 0
          if (tb == "FVS_Cases") next
          if (tb %in% c("CmpSummary","CmpSummary_East",                 
              "CmpSummary2","CmpSummary2_East","StdStk","CmpStdStk",
              "StdStk_East","CmpStdStk_East","CmpMetaData","CmpCompute"))
          {
cat ("drop tb=",tb,"\n")
             dbExecute(dbGlb$dbOcon,paste0("drop table if exists ",tb))
          } else {
            qry = paste0("select count(*) from ",
                   "(select CaseID from ",tb," where ",tb,".CaseID in ",
                   "(select CaseID from temp.Cases))")
cat("qry=",qry,"\n")
            cnt = if ("CaseID" %in% dbListFields(dbGlb$dbOcon,tb))  
              dbGetQuery(dbGlb$dbOcon,qry) else -1
            cnt = if (class(cnt)=="data.frame") cnt[1,1] else -1
cat ("tb=",tb," cnt=",cnt,"\n")
          }
          if (cnt == 0) tbs = setdiff(tbs,tb)
        }
        source(system.file("extdata", "sqlQueries.R", package = "fvsOL"))
        ncases = dbGetQuery(dbGlb$dbOcon, "select count(*) from temp.Cases;")[1,1]
cat ("ncases=",ncases,"\n")
        if (ncases > 1) if (!exqury(dbGlb$dbOcon,Create_CmpMetaData)) return()
        isolate(dbhclassexp <- mkdbhCase(input$sdskwdbh,input$sdskldbh))
        input$bldstdsk # force this section to be reactive to changing "bldstdsk"   
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
cat ("tbs3=",tbs,"\n")
        }
        if ("FVS_Summary2_East" %in% tbs && ncases > 1)
        {
          setProgress(message = "Output query", 
            detail  = "Building CmpSummary2_East", value = i); i = i+1
          exqury(dbGlb$dbOcon,Create_CmpSummary2_East)
          tbs = c(tbs,"CmpSummary2_East")
cat ("tbs4=",tbs,"\n")
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
cat ("tbs5=",tbs,"\n")
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
          exqury(dbGlb$dbOcon,C_StdStkDBHSp,subExpression=dbhclassexp,
                 asSpecies=paste0("Species",input$spCodes))
          if (clname %in% tbs)
          {
            setProgress(message = "Output query", 
              detail  = detail, value = i); i = i+1
            exqury(dbGlb$dbOcon,C_HrvStdStk,subExpression=dbhclassexp,
                 asSpecies=paste0("Species",input$spCodes))
            setProgress(message = "Output query", 
              detail  = "Joining tables", value = i); i = i+1
            exqury(dbGlb$dbOcon,C_StdStk1Hrv,subExpression=dbhclassexp,
                 asSpecies=paste0("Species",input$spCodes))
          } else {
             setProgress(message = "Output query", 
              detail  = "Joining tables", value = i); i = i+2
            exqury(dbGlb$dbOcon,C_StdStk1NoHrv,subExpression=dbhclassexp,
                 asSpecies=paste0("Species",input$spCodes))
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
cat ("tbs6=",tbs,"\n")       
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
        sel = intersect(tbs, c("FVS_Summary2","FVS_Summary2_East")) #not both!
        if (length(sel)==0) sel = intersect(tbs, c("FVS_Summary","FVS_Summary_East")) #not both!
        if (length(sel)>1) sel = sel[1]
        # rearrange the table list so be organized by levels (i.e., tree level, stand level)
        globals$simLvl <- list("CmpCompute","CmpStdStk","CmpStdStk_East","CmpSummary2","CmpSummary2_East",
                               "CmpMetaData")
        globals$stdLvl <- list("FVS_Climate","FVS_Compute","FVS_EconSummary","FVS_BurnReport","FVS_Carbon",
                       "FVS_Down_Wood_Cov","FVS_Down_Wood_Vol","FVS_Consumption","FVS_Hrv_Carbon",
                       "FVS_PotFire","FVS_PotFire_Cond","FVS_PotFire_East","FVS_SnagSum","FVS_Fuels","FVS_DM_Spp_Sum",
                       "FVS_DM_Stnd_Sum","FVS_Regen_Sprouts","FVS_Regen_SitePrep","FVS_Regen_HabType",
                       "FVS_Regen_Tally","FVS_Regen_Ingrow","FVS_RD_Sum","FVS_RD_Det","FVS_RD_Beetle",
                       "FVS_Stats_Stand","FVS_StrClass","FVS_Summary2","FVS_Summary2_East","FVS_Summary",
                       "FVS_Summary_East","View_DWN")
        globals$specLvl <- list("FVS_CalibStats","FVS_EconHarvestValue","FVS_Stats_Species")
        globals$dClsLvl <- list("StdStk","StdStk_East","FVS_Mortality","FVS_DM_Sz_Sum")
        globals$htClsLvl <- list("FVS_CanProfile")
        globals$treeLvl <- list("FVS_ATRTList","FVS_CutList","FVS_SnagDet","FVS_TreeList",
                                "FVS_TreeList_East","FVS_CutList_East","FVS_ATRTList_East")
        globals$tbsFinal <- list("FVS_Cases")
        tbsFinal <- globals$tbsFinal
        if (any(tbs %in% globals$simLvl)) {
          tbsFinal <- c(tbsFinal,"-----Composite tables-----")
          simLvlIdx <- subset(match(globals$simLvl,tbs),match(globals$simLvl,tbs) != "NA")
          tbsFinal <- c(tbsFinal,sort(tbs[simLvlIdx]))
        }
        if (any(tbs %in% globals$stdLvl)) {
          tbsFinal = c(tbsFinal,"-----Stand-level tables-----")
          stdLvlIdx <- subset(match(globals$stdLvl,tbs),match(globals$stdLvl,tbs) != "NA")
          tbsFinal <- c(tbsFinal,sort(tbs[stdLvlIdx]))
        }
        if (any(tbs %in% globals$specLvl)) {
          tbsFinal = c(tbsFinal,"-----Species-level tables-----")
          specLvlIdx <- subset(match(globals$specLvl,tbs),match(globals$specLvl,tbs) != "NA")
          tbsFinal <- c(tbsFinal,sort(tbs[specLvlIdx]))
        }
        if (any(tbs %in% globals$dClsLvl)) {
          tbsFinal = c(tbsFinal,"-----Diameter-class tables-----")
          dClsLvlIdx <- subset(match(globals$dClsLvl,tbs),match(globals$dClsLvl,tbs) != "NA")
          tbsFinal <- c(tbsFinal,sort(tbs[dClsLvlIdx]))
        }
        if (any(tbs %in% globals$htClsLvl)) {
          tbsFinal = c(tbsFinal,"-----Height-class tables-----")
          htClsLvlIdx <- subset(match(globals$htClsLvl,tbs),match(globals$htClsLvl,tbs) != "NA")
          tbsFinal <- c(tbsFinal,sort(tbs[htClsLvlIdx]))
        }
        if (any(tbs %in% globals$treeLvl)) {
          tbsFinal = c(tbsFinal,"-----Tree-level tables-----")
          treeLvlIdx <- subset(match(globals$treeLvl,tbs),match(globals$treeLvl,tbs) != "NA")
          tbsFinal <- c(tbsFinal,sort(tbs[treeLvlIdx]))
        }
        globals$tbsFinal <- tbsFinal
        updateSelectInput(session, "selectdbtables", choices=as.list(tbsFinal),
                          selected="FVS_Cases")
                          
        setProgress(value = NULL)
      }, min=1, max=6)
    } else
    {
      updateSelectInput(session, "selectdbtables", choices=list())
    }
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
  
  # selectdbtables
  observe({
cat("selectdbtables\n")    
    if (is.null(input$selectdbtables) ||(length(input$selectdbtables)==1 
                          && length(grep("-----",input$selectdbtables))))
    {
      updateSelectInput(session, "selectdbvars", choices=list())               
    } else  {
      tables = input$selectdbtables 
      if(length(grep("-----",tables))) tables <- setdiff(tables,tables[grep("-----",tables)])
      # Logic to restrict combining tables from different levels (e.g., tree with stand-level).
      # Throw up warning, then have first table selection in level that threw error remain selected
      while(length(tables)>1)
      {
        if(length(tables)==2 && tables %in% "FVS_Cases") break
        if(length(tables)==2 && (tables[1] %in% "CmpCompute" && tables[2] %in% "CmpSummary2")) break
        if(length(tables)==2 && (tables[1] %in% "CmpCompute" && tables[2] %in% "CmpSummary2_East")) break
        '%notin%' = Negate('%in%')
        if (any(tables %in% globals$simLvl)) {
          session$sendCustomMessage(type = "infomessage",
              message = paste0("This composite table combination in not allowed"))
          tables <- tables[1]
          updateSelectInput(session, "selectdbtables", choices=as.list(globals$tbsFinal),
                          selected=tables)
        }
        if (any(tables %in% globals$stdLvl) && any(tables %notin% globals$stdLvl)) {
          session$sendCustomMessage(type = "infomessage",
              message = paste0("Stand-level tables can only be combined with other stand-level tables"))
          tables <- tables[1]
          updateSelectInput(session, "selectdbtables", choices=as.list(globals$tbsFinal),
                          selected=tables)
        }
        if (any(tables %in% globals$specLvl) && any(tables %notin% globals$specLvl)) {
          session$sendCustomMessage(type = "infomessage",
               message = paste0("Species-level tables can only be combined with other species-level tables"))
          tables <- tables[1]
          updateSelectInput(session, "selectdbtables", choices=as.list(globals$tbsFinal),
                          selected=tables)
        }
        # DBH-class tables cannot be combined with any other table
        if (any(tables %in% globals$dClsLvl)) {
          session$sendCustomMessage(type = "infomessage",
              message = paste0("DBH-class tables cannot be combined with any other tables"))
          tables <- tables[1]
          updateSelectInput(session, "selectdbtables", choices=as.list(globals$tbsFinal),
                          selected=tables)
        }
        # HT-class tables cannot be combined with any other table
        if (any(tables %in% globals$htClsLvl)) {
          session$sendCustomMessage(type = "infomessage",
              message = paste0("HT-class tables cannot be combined with any other tables"))
          tables <- tables[1]
          updateSelectInput(session, "selectdbtables", choices=as.list(globals$tbsFinal),
                          selected=tables)
        }
        # tree-level tables cannot be combined with any other table
        if (any(tables %in% globals$treeLvl)) {
          session$sendCustomMessage(type = "infomessage",
              message = paste0("Tree-level tables cannot be combined with any other tables"))
          tables <- tables[1]
          updateSelectInput(session, "selectdbtables", choices=as.list(globals$tbsFinal),
                          selected=tables)
        }
        break
      }
      vars = lapply(tables,function (tb,dbd) paste0(tb,".",dbd[[tb]]),fvsOutData$dbLoadData)
      vars = unlist(vars)

      if (length(vars) == 0) return()
      fvsOutData$dbVars    <- vars
      fvsOutData$dbSelVars <- vars     
      updateSelectInput(session=session, "selectdbvars",choices=as.list(vars), 
                        selected=vars)
      output$tbSel <-renderUI({
        HTML(input$selectdbtables)
      })
    }
  })

  # selectdbvars
  observe({
cat("selectdbvars\n")    
    if (!is.null(input$selectdbvars)) 
    {
      # if CaseID is part of the variable set, make sure it is selected at least once
      selidxCaseID=grep("CaseID",input$selectdbvars)
      if (!length(selidxCaseID)) 
      {
        idxCaseID=grep("CaseID",fvsOutData$dbVars)
        if (length(idxCaseID)) 
        {
          selvars=union(fvsOutData$dbVars[idxCaseID[1]],input$selectdbvars)
          updateSelectInput(session=session, "selectdbvars",choices=as.list(fvsOutData$dbVars), 
                            selected=selvars)
        }
      }
      fvsOutData$dbSelVars <- input$selectdbvars
cat ("input$selectdbvars=",input$selectdbvars,"\n") 
    }
  })
    
  ## Custom Query
  observe({  
    if (input$leftPan == "Custom Query")
    {
cat("Custom Query\n")
      initTableGraphTools(globals,session,output,fvsOutData)
      if (length(globals$customQueries) == 0) 
      {
        loadObject(dbGlb$prjDB,"customQueries")
        if (exists("customQueries")) globals$customQueries=customQueries
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
        qrys = trim(gsub("\n"," ",removeComment(input$sqlQuery)," ",input$sqlQuery))
        qrys = scan(text=qrys,sep=";",what="",quote="",quiet=TRUE)
        qrys = qrys[nchar(qrys)>0]
        output$table <- renderTable(NULL)        
        iq = 0
        dfrtn = NULL
        # attempt to attach the input database is attached as "input"
        attInput = if (!dbGlb$dbIcon@dbname %in% dbGetQuery(dbGlb$dbOcon,"PRAGMA database_list")$file) 
          try(dbExecute(dbGlb$dbOcon,paste0("attach database '",dbGlb$dbIcon@dbname,
                    "' as input"))) else NULL             
        for (qry in qrys)                  
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
            globals$settingChoices[["pivVar"]] = choices
            globals$settingChoices[["hfacet"]] = choices
            globals$settingChoices[["vfacet"]] = choices
            globals$settingChoices[["pltby"]] = choices
            choices = as.list(c("None",
              colnames(res)[!unlist(lapply(res, is.factor))])) 
            globals$settingChoices[["dispVar"]] = choices
            updateSelectInput(session,"dispVar",choices=choices,selected="None")
            choices = as.list(colnames(res))              
            globals$settingChoices[["xaxis"]] = choices
            globals$settingChoices[["yaxis"]] = choices
            updateSelectInput(session,"xaxis",choices=choices,selected=colnames(res)[1]) 
            updateSelectInput(session,"yaxis",choices=choices,selected=colnames(res)[1]) 
            if (input$outputRightPan != "Tables")
              updateSelectInput(session,"outputRightPan",selected="Tables")
            tableDisplayLimit = 5000
            if (nrow(res) > tableDisplayLimit) 
            {
              msg=paste0("Table display limit exceeded. ",
                tableDisplayLimit," of ",nrow(res)," displayed. Use Download table",
                " to download all rows.")
              output$tableLimitMsg<-renderText(msg)
              res = res[1:tableDisplayLimit,,drop=FALSE] 
            } else output$tableLimitMsg<-NULL
            output$table <- renderTable(res)
            break
          }        
        }       
        if (!is.null(attInput)) try(dbExecute(dbGlb$dbOcon,"detach database 'input'"))     
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
        storeOrUpdateObject(dbGlb$prjDB,customQueries)
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
        storeOrUpdateObject(dbGlb$prjDB,customQueries)
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
       initTableGraphTools(globals,session,output,fvsOutData)
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
            } else if (tb == "FVS_Summary" || tb == "FVS_Summary_East") 
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
            # fix the species column.
            spcd=paste0("Species",input$spCodes) 
            if (spcd %in% names(dtab))
            {
              if (is.null(dtab$Species)) dtab$Species=dtab[,spcd] else 
              {
                na=is.na(dtab$Species)
                dtab$Species = as.character(dtab$Species)
                dtab$Species[na] = as.character(dtab[na,spcd])
                dtab$Species = as.factor(dtab$Species)
              }
            }
            dat[[tb]] = dtab
          }
        }
cat ("Explore, len(dat)=",length(dat),"\n") 
        if (length(dat) == 0)
        {
          initTableGraphTools(globals,session,output,fvsOutData)
          return()
        }
        iprg = iprg+1
        setProgress(message = "Merging selected tables", detail  = "", value = iprg)
        inch = 0
        mdat = NULL
        for (tb in names(dat))      
        {
cat ("tb=",tb," is.null(mdat)=",is.null(mdat),"\n") 
          if (is.null(mdat)) mdat = dat[[tb]] else
          {
             mrgVars = intersect(names(mdat),c("CaseID","Year","StandID","MgmtID"))
             mrgVars = intersect(mrgVars,names(dat[[tb]]))
             setProgress(message = "Merging selected tables", 
                         detail  = tb, value = iprg)
cat ("tb=",tb," mrgVars=",mrgVars,"\n")
             merged = merge(mdat,dat[[tb]], by=mrgVars)
             mdat = if (nrow(merged)) merged else
             {
               common = intersect(names(mdat),names(dat[[tb]]))
               unique = setdiff(names(dat[[tb]]),c(common,mrgVars))
               nd=matrix(data=NA,ncol=length(unique),nrow=nrow(mdat))
               colnames(nd)=unique
               mdat=cbind(mdat,nd)
               common = intersect(names(mdat),names(dat[[tb]]))
               unique = setdiff(names(mdat),c(common,mrgVars,"MgmtID","RunTitle"))
               nd=matrix(data=NA,ncol=length(unique),nrow=nrow(dat[[tb]]))
               colnames(nd)=unique
               nd = data.frame(nd)
               idr=match(as.character(dat[[tb]]$CaseID),as.character(dat$FVS_Cases$CaseID))            
               nd=cbind(dat$FVS_Cases[idr,c("MgmtID","RunTitle")],nd)
               dat[[tb]]=cbind(dat[[tb]],nd)
               rbind(mdat,dat[[tb]])
             }
          }
        }
        if (!is.null(mdat$CaseID))
        {
          mdat=merge(mdat,dbGetQuery(dbGlb$dbOcon,"select * from temp.Cases"),by="CaseID")
          mdat=mdat[order(mdat$rowid,1:nrow(mdat)),]
          mdat$rowid=NULL
        }
        fvsOutData$dbData = mdat
        iprg = iprg+1
        # do rep assignments
        setProgress(message = "Setting stand reps", detail  = "", value = iprg)
        newSid = as.character(fvsOutData$dbData$StandID)
        icid = as.integer(fvsOutData$dbData$CaseID)
        imid = as.integer(fvsOutData$dbData$MgmtID)
        isid = as.integer(fvsOutData$dbData$StandID)+as.integer(imid*1000000)
        sidch = FALSE
        for (id in unique(isid))
        {
          nq = unique(icid[isid==id])
          if (length(nq)==1) next
          mq = unique(imid[isid==id])
          sidch = TRUE
          rep = 0
          for (iq in nq) 
          {
            rep = rep+1
            chng = icid==iq
            newSid[chng] = sprintf("%s r%03i",newSid[chng],rep)
          }
        }
        if (sidch) fvsOutData$dbData$StandID = as.factor(newSid)
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
        {
          cho = "None loaded"
          updateSelectInput(session,"stdid",choices =list(cho),selected = NULL) 
        } else {
          cho = levels(mdat$StandID)
          sel = cho
          if (length(cho) > 5000) 
          {
            cho = paste0("None loaded (",length(cho)," stands)")
            sel = NULL
          }
          updateSelectInput(session,"stdid",choices=as.list(cho),selected=sel)
        }
        globals$exploreChoices$stdid = cho
        if (is.null(mdat$Groups)) 
        {
          cho = "None loaded"
          updateSelectInput(session,"stdgroups",choices=as.list(cho),selected = NULL) 
        } else {
          cho = sort(unique(unlist(lapply(levels(mdat$Groups), function (x)
                trim(scan(text=x,what="character",sep=",",quiet=TRUE))))))           
          updateSelectInput(session, "stdgroups",choices=as.list(cho),selected=cho)
        }
        globals$exploreChoices$stdgroups = cho
        if (is.null(mdat$MgmtID)) 
        {
          cho = "None loaded"         
          updateSelectInput(session,"mgmid",choices=as.list(cho),selected=0) 
        } else {
          cho = levels(mdat$MgmtID)
          updateSelectInput(session, "mgmid",choices=as.list(cho),selected=cho)
        }
        globals$exploreChoices$mgmid = cho
        if (length(intersect(c("FVS_TreeList","FVS_ATRTList","FVS_CutList",
                "FVS_TreeList_East","FVS_ATRTList_East","FVS_CutList_East"),names(dat))))
          updateSelectInput(session, "plotType",selected="scat") else 
          if (length(intersect(c("StdStk","CmpStdStk","StdStk_East","CmpStdStk_East"),names(dat)))) 
            updateSelectInput(session, "plotType",selected="bar") else
              updateSelectInput(session, "plotType",selected="line")
        iprg = iprg+1
        setProgress(message = "Loading selection widgets", detail  = "", value = iprg)
        if (is.null(mdat$Year)) 
        {
          cho = "None loaded"       
          updateSelectInput(session,"year",choices=as.list(cho),selected = NULL) 
        } else {
          cho  = levels(mdat$Year)
          isel = max(1,length(cho) %/% 2)
          sel =  if (length(intersect(c("FVS_TreeList","FVS_ATRTList","FVS_CutList",
              "FVS_TreeList_East","FVS_ATRTList_East","FVS_CutList_East",
              "StdStk","StdStk_East","CmpStdStk","CmpStdStk_East"),names(dat)))) 
              cho[isel] else cho 
          updateSelectInput(session, "year", choices=as.list(cho), selected=sel)
        }        
        globals$exploreChoices$year = cho
        if (is.null(mdat$Species)) 
        {
          cho = "None loaded"       
          updateSelectInput(session, "species", choices  = list(cho), selected = NULL) 
        } else {
          cho = levels(mdat$Species)
          updateSelectInput(session, "species",
             choices=as.list(cho), selected=setdiff(cho,"All"))
        }
        globals$exploreChoices$species = cho        
        if (is.null(mdat$DBHClass)) 
        {
          cho = "None loaded"
          updateSelectInput(session,"dbhclass",choices=list(cho),selected = NULL) 
        } else {
          cho = levels(mdat$DBHClass)
          sel = if ("All" %in%  cho) "All" else cho
          updateSelectInput(session, "dbhclass", choices=as.list(cho), selected=sel)
        }           
        globals$exploreChoices$dbhclass = cho        
        iprg = iprg+1
        setProgress(message = "Finishing", detail  = "", value = iprg)
        fvsOutData$dbData <- mdat
        vars <- c("Select all",vars)
        fvsOutData$browseVars <- vars
        varsList <- as.list(vars)
        vars = setdiff(vars,c("Select all","Stand_CN","KeywordFile",
           "SamplingWt","Variant","Version", 
           "RV", "RunDateTime"))
        fvsOutData$browseSelVars <- vars
        updateCheckboxGroupInput(session, "browsevars", choices=varsList, 
                                 selected=vars,inline=TRUE)                               
        setProgress(value = NULL)          
      }, min=1, max=12)
    } 
  })
 
  ## renderTable
  renderTable <- function (dat)
  {
cat ("renderTable, is.null=",is.null(dat)," nrow(dat)=",nrow(dat),"\n")
    if (!is.null(dat) && ncol(dat)==0){
      renderRHandsontable(NULL)
      return() 
    }
    if (!is.null(dat) && nrow(dat) > 0)
    {                                 
      dat = lapply(dat,function (x) 
        if (is.factor(x)) levels(x)[as.numeric(x)] else x)
      dat = as.data.frame(dat)
      for (i in 1:ncol(dat)) 
        if (class(dat[[i]]) == "numeric") dat[[i]] = round(dat[[i]],3)
    }
    if(length(grep("X_",names(dat)))){
      idxs <- grep("X_",names(dat))
      for(i in 1:length(grep("X_",names(dat)))){
        names(dat)[idxs[i]] <- sub('.', '', names(dat)[idxs[i]])
      }
    }
    renderRHandsontable(if (is.null(dat) || nrow(dat)==0) NULL else 
              rhandsontable(dat,readOnly=TRUE,useTypes=FALSE,contextMenu=FALSE,
              width="100%",height=700))
  }
         
  observe({
    if (is.null(input$browsevars)) return()
cat("filterRows and/or pivot\n")
    if(fvsOutData$browseVars[1]==input$browsevars[1]){
        fvsOutData$browseSelVars <- fvsOutData$browseVars[-1]
        updateCheckboxGroupInput(session, "browsevars", choices=as.list(fvsOutData$browseVars), 
                                 selected=fvsOutData$browseVars,inline=TRUE)
        globals$selAllVars = TRUE
      if(length(input$browsevars)==(length(fvsOutData$browseVars)-1) && globals$selAllVars){
        fvsOutData$browseSelVars <- input$browsevars[-1]
        updateCheckboxGroupInput(session, "browsevars", choices=as.list(fvsOutData$browseVars), 
                                 selected=fvsOutData$browseSelVars,inline=TRUE)
        globals$selAllVars = FALSE
      }
    }else if (fvsOutData$browseVars[1]!=input$browsevars[1] && globals$selAllVars){
      fvsOutData$browseSelVars <- character()
      updateCheckboxGroupInput(session, "browsevars", choices=as.list(fvsOutData$browseVars),
                                 selected=fvsOutData$browseSelVars,inline=TRUE)
      globals$selAllVars = FALSE
    }else fvsOutData$browseSelVars <- input$browsevars
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
    tableDisplayLimit = 5000
    if (nrow(dat) > tableDisplayLimit) 
    {
      msg=paste0("Table display limit exceeded. ",
        tableDisplayLimit," of ",nrow(dat)," displayed. Use .o. table",
        " to download all rows.")
      output$tableLimitMsg<-renderText(msg)
      dat = dat[1:tableDisplayLimit,,drop=FALSE] 
    } else output$tableLimitMsg<-NULL
    output$table <- renderTable(dat) 
  })               
           
  ##Graphs
  observe({                 
    if (input$leftPan == "Explore" && input$outputRightPan == "Graphs")
    {
cat ("Graphs pan hit\n")
      # update color pallet
      for (i in 1:length(cbbPalette))
        updateColourInput(session=session,inputId=paste0("color",i),value=cbbPalette[i])
      loadObject(dbGlb$prjDB,"GraphSettings") 
      if (!exists("GraphSettings")) GraphSettings=list("None"=list())
      updateSelectInput(session=session, inputId="OPsettings", choices=names(GraphSettings),
         selected="None")
      updateTextInput(session=session, "OPname", value = "")
      output$OPmessage=NULL
    }
  })
  
  ##OPsettings
  observe({
    if (!is.null(input$OPsettings))
    {
      input$OPredo
      isolate({
cat ("OPsettings hit, OPsettings=",input$OPsettings,"\n")
        loadObject(dbGlb$prjDB,"GraphSettings")
        if (!exists("GraphSettings") || 
            length(GraphSettings[[input$OPsettings]])<1 ||
            input$OPsettings == "None")
        {                  
          output$OPmessage=NULL
          updateTextInput(session=session, "OPname", value = "") 
        } else {
          updateTextInput(session=session, "OPname", value = input$OPsettings)
          if (all(unlist(GraphSettings[[input$OPsettings]][["selectdbtables"]]) %in%
                   input$selectdbtables) &&
              all(unlist(GraphSettings[[input$OPsettings]][["dbvars"]]) %in% 
                  input$selectdbvars)) 
          {
            output$OPmessage=NULL 
            msg = setGraphSettings(session,globals,GraphSettings[[input$OPsettings]])
cat ("msg=",msg,"\n")
            if (! is.null(msg)) output$OPmessage=
              renderUI(HTML(paste0('<p style="color:darkred">',
                "Warning(s): <br>",paste0(msg,collapse="<br>"),"</p>")))  
          } else output$OPmessage=renderUI(HTML(paste0('<p style="color:darkred">',
              "Error: The data needed for this setting was not selected ",
              "when you picked data to load.<br>Table(s) needed: ",
              paste0(GraphSettings[[input$OPsettings]][["selectdbtables"]],
              collapse=", "),"</p>")))
        } 
      })
    }
  })      
  observe({
    if (input$OPsave > 0) 
    {
      output$OPmessage=NULL
      isolate({
cat ("OPsave hit, OPname=",input$OPname,"\n")
        loadObject(dbGlb$prjDB,"GraphSettings")
        if (!exists("GraphSettings")) 
        {
          GraphSettings=list("None"=list())
          attr(GraphSettings[[1]],"setTime")=.Machine$integer.max
        }
        if (nchar(input$OPname)==0) 
        {
          setName=paste0("Setting ",length(GraphSettings)+1)
          updateTextInput(session=session,inputId="OPname",value=setName)
        } else setName=input$OPname      
        GraphSettings[[setName]]=getGraphSettings(input)
        attr(GraphSettings[[setName]],"setTime")=as.integer(Sys.time())
        GraphSettings <- GraphSettings[order(unlist(lapply(GraphSettings,
          function(x) attr(x,"setTime"))),decreasing = TRUE)]
        storeOrUpdateObject(dbGlb$prjDB,"GraphSettings")        
        updateSelectInput(session=session, inputId="OPsettings", choices=
          names(GraphSettings),selected=setName)
      })
    }
  })
  observe({
    if (input$OPdel > 0) 
    {                             
      isolate({
cat("OPdel hit, input$OPname=",input$OPname,"\n")
        output$OPmessage=NULL
        loadObject(dbGlb$prjDB,"GraphSettings")
        if (!exists("GraphSettings")) return() 
        if (input$OPname == "None") return()
        if (is.null(GraphSettings[[input$OPname]])) return()
        GraphSettings[[input$OPname]] = NULL
        if (length(GraphSettings)==0) 
        {
          updateSelectInput(session=session, inputId="OPsettings", choices=list())
          removeObject(dbGlb$prjDB,"GraphSettings")
        } else {
          updateSelectInput(session=session, inputId="OPsettings", choices=
            names(GraphSettings),selected="None")
          storeOrUpdateObject(dbGlb$prjDB,"GraphSettings")
        }
        updateTextInput(session=session, "OPname", value = "") 
      })
    }
  })

  ##browsevars/plotType 
  observe({
    if (!is.null(input$browsevars) && !is.null(input$plotType)) 
    {
cat ("browsevars/plotType, input$plotType=",input$plotType," globals$gFreeze=",globals$gFreeze,"\n")
      fvsOutData$browseSelVars <- input$browsevars  
      cats = unlist(lapply(fvsOutData$dbData,is.factor))
      cats = names(cats)[cats]
      cats = intersect(cats,input$browsevars)
      cont = union("Year",setdiff(input$browsevars,cats))
      if(length(cont) > 1 && cont[2]=="Select all") cont <- cont[-2]
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
      if (globals$gFreeze) return()
      isolate({
        curX = input$xaxis
        curY = input$yaxis
        if (input$plotType=="line") {
          selx = if (is.null(curX)) "Year" else curX
          selx = if (selx %in% cont) selx else 
                 if (length(cont) > 0) cont[1] else NULL
          globals$settingChoices[["xaxis"]] = as.list(cont)
          updateSelectInput(session, "xaxis",choices=globals$settingChoices[["xaxis"]], selected=selx)
          sel = if (is.null(curY)) "BA" else curY
          sel = if (sel %in% cont) sel else 
                if (length(cont) > 0) cont[1] else NULL
          if (sel == selx && length(cont) > 1) 
          {
            sel = grep("BA",cont)[1]
            sel = if (is.na(sel)) cont[2] else cont[sel]
          }
          globals$settingChoices[["yaxis"]] = as.list(cont)
          updateSelectInput(session, "yaxis",choices=globals$settingChoices[["yaxis"]], selected=sel)
        } else if (input$plotType == "scat") {
          sel = if (is.null(curX)) "DBH" else curX
          sel = if (sel %in% cont) sel else 
                if (length(cont) > 0) cont[1] else NULL
          updateSelectInput(session, "xaxis",choices=as.list(cont), selected=sel)
          sel = if (is.null(curY)) "DG" else curY
          sel = if (sel %in% cont) sel else 
                if (length(cont) > 0) cont[1] else NULL
          globals$settingChoices[["yaxis"]] = as.list(cont)
          updateSelectInput(session, "yaxis",choices=globals$settingChoices[["yaxis"]], selected=sel)
        } else if (input$plotType == "bar") {
          def = if ("Species" %in% cats) "Species" else NULL
          def = if (is.null(def) && "Year" %in% cats) "Year" else cats[1]
          sel = if (!is.null(curX) && curX %in% cats) curX else def
          globals$settingChoices[["xaxis"]] = as.list(cats)
          updateSelectInput(session, "xaxis",choices=globals$settingChoices[["xaxis"]], selected=sel)
          sel = if (!is.null(curX) && curX %in% cont) curX else cont[1]
          if (sel=="Year" && length(cont) > 1) sel = cont[2]
          globals$settingChoices[["yaxis"]] = as.list(cont)
          updateSelectInput(session, "yaxis",choices=globals$settingChoices[["yaxis"]], selected=sel)
        } else if (input$plotType == "box") {
          def = if ("Species" %in% cats) "Species" else NULL
          def = if (is.null(def) && "Year" %in% cats) "Year" else cats[1]
          sel = if (!is.null(curX) && curX %in% cats) curX else def
          globals$settingChoices[["xaxis"]] = as.list(cats)
          updateSelectInput(session, "xaxis",choices=globals$settingChoices[["xaxis"]], selected=sel)
          sel = if (!is.null(curX) && curX %in% cont) curX else cont[1]
          if (sel=="Year" && length(cont) > 1) sel = cont[2]
          globals$settingChoices[["yaxis"]] = as.list(cont)
          updateSelectInput(session, "yaxis",choices=globals$settingChoices[["yaxis"]], selected=sel)
        } else if (input$plotType=="DMD") {
          updateRadioButtons(session=session,inputId="XUnits",selected="QMD")
          updateRadioButtons(session=session,inputId="YUnits",selected="Tpa")          
          updateRadioButtons(session=session,inputId="YTrans",selected="log10")
          updateRadioButtons(session=session,inputId="XTrans",selected="log10")
          globals$settingChoices[["xaxis"]] = as.list(cont)
          updateSelectInput(session, "xaxis",choices=globals$settingChoices[["xaxis"]], selected="QMD")
          globals$settingChoices[["yaxis"]] = as.list(cont)
          updateSelectInput(session, "yaxis",choices=globals$settingChoices[["yaxis"]], selected="Tpa")
        } else if (input$plotType=="StkCht") {
          globals$settingChoices[["xaxis"]] = as.list(cont)
          updateSelectInput(session, "xaxis",choices=globals$settingChoices[["xaxis"]], selected="Tpa")
          globals$settingChoices[["yaxis"]] = as.list(cont)
          updateSelectInput(session, "yaxis",choices=globals$settingChoices[["yaxis"]], selected="BA")
        }
        updateSliderInput(session, "transparency",  
          value = if(input$plotType == "scat") .3 else 0.)
        if (input$plotType!="DMD")
        {
          updateRadioButtons(session=session,inputId="YTrans",selected="identity")
          updateRadioButtons(session=session,inputId="XTrans",selected="identity") 
        }
        sel = if ("StandID" %in% cats && input$plotType != "box") "StandID" else "None"
        updateSelectInput(session=session, inputId="hfacet",choices=as.list(c("None",cats)),
          selected=sel) 
        sel = if ("MgmtID" %in% cats && input$plotType != "box") "MgmtID" else "None"
        updateSelectInput(session=session, inputId="vfacet",choices=as.list(c("None",cats)),
          selected=sel) 
        sel = if ("Species" %in% cats && input$plotType != "box") "Species" else "None"
        updateSelectInput(session=session, inputId="pltby",choices=as.list(c("None",cats)),
          selected=sel)
cat ("end of browsevars/plotType\n")
      })
    }
  })   

  ## yaxis, xaxis regarding the Y- and XUnits for DMD
  observe({
    if (globals$gFreeze) return()
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
    if (is.null(input$pltby) || input$pltby  == "None" || globals$gFreeze) return()
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
cat ("vfacet change, globals$gFreeze=",globals$gFreeze,"\n")
    if (is.null(input$vfacet) || input$vfacet  == "None" || globals$gFreeze) return()
    isolate({
      if (!is.null(input$xaxis) && !is.null(input$yaxis) &&
          (input$vfacet == input$xaxis || input$vfacet == input$yaxis))
      {
        updateSelectInput(session=session, inputId="vfacet", selected="None")
        return()
      }
      if (!is.null(input$pltby) && input$vfacet == input$pltby)
        updateSelectInput(session=session, inputId="pltby", selected="None")       
      if (input$vfacet == input$hfacet)
        updateSelectInput(session=session, inputId="hfacet", selected="None")
  }) }) 
  observe({
cat ("hfacet change, globals$gFreeze=",globals$gFreeze,"\n")
    if (is.null(input$hfacet) || input$hfacet  == "None" || globals$gFreeze) return()                 
    isolate({
      if (!is.null(input$xaxis) && !is.null(input$yaxis) &&
          (input$hfacet == input$xaxis || input$hfacet == input$yaxis))
      {
        updateSelectInput(session=session, inputId="hfacet", selected="None")
        return()
      }
      if (!is.null(input$pltby) && input$hfacet == input$pltby)
        updateSelectInput(session=session, inputId="pltby", selected="None")
      if (input$hfacet == input$vfacet)
        updateSelectInput(session=session, inputId="vfacet", selected="None")
  }) })   
  
  ## renderPlot
  output$outplot <- renderImage(
  {
cat ("renderPlot\n")
    output$plotMessage=NULL
    nullPlot <- function (msg="Select different data, variables, plot type, or facet settings.")
    {
      outfile = "www/nullPlot.png"
      if (!file.exists(outfile))
      {
        CairoPNG(outfile, width=3, height=2, res=72, units="in", pointsize=12)              
        plot.new()
        text(x=.5,y=.5,"Nothing to graph",col="red")
        dev.off()
      }
      output$plotMessage=renderText(msg)
      list(src = outfile)
    }
    if (input$leftPan == "Load"  || (length(input$xaxis) == 0 && 
        length(input$yaxis) == 0)) return(nullPlot())
    output$plotMessage=renderText(NULL)

    vf = if (input$vfacet == "None") NULL else input$vfacet
    hf = if (input$hfacet == "None") NULL else input$hfacet
    pb = if (input$pltby  == "None") NULL else input$pltby
    needVars = unique(c(vf,hf,pb,input$xaxis,input$yaxis))
    dat = if (input$leftPan == "Custom Query") fvsOutData$dbData else         
      droplevels(fvsOutData$dbData[filterRows(fvsOutData$dbData, input$stdtitle, 
          input$stdgroups, input$stdid, input$mgmid, input$year, input$species, 
          input$dbhclass),])
    if (nrow(dat)==0) return(nullPlot("No observations using these selections"))
    # fix DBHClass if it is in the data.
    if (!is.null(dat$DBHClass))
    { 
      mlv=setdiff(input$dbhclass,levels(dat$DBHClass))
      if (length(mlv))
      {
        # this bit makes sure CaseID is first
        byset=c("CaseID",setdiff(names(dat)[unlist(lapply(dat,is.factor))],
                        c("CaseID","MgmtID","StandID","DBHClass","RunTitle")))
        newrows = ddply(dat,byset,function(x) x[1,])
        newrows[,!unlist(lapply(dat,is.factor))]=0
        newrows$DBHClass=as.character(newrows$DBHClass)
        dat$DBHClass=as.character(dat$DBHClass)
        for (lms in mlv) 
        {
          newrows$DBHClass = lms
          dat=rbind(dat,newrows)
        }
        dat$DBHClass=factor(as.character(dat$DBHClass))
        cmd=paste0("idx=with(dat,order(",paste0(c(byset,"DBHClass"),collapse=","),"))")
        eval(parse(text=cmd))
        dat = dat[idx,]
      }
    } # end of DBHClass fixup
    if (!is.null(pb) && pb=="Groups" && length(input$stdgroups) && length(levels(dat$Groups)))
    {
      for (il in 1:length(levels(dat$Groups)))
      {
        levs = trim(unlist(strsplit(levels(dat$Groups)[il],",")))
        newl = paste0(intersect(levs,input$stdgroups),collapse=", ")
        levels(dat$Groups)[il] = newl
      }      
    }
    if (length(setdiff(needVars,names(dat)))) return(nullPlot())
cat ("vf=",vf," hf=",hf," pb=",pb," xaxis=",input$xaxis," yaxis=",input$yaxis,"\n")
    if (is.null(input$xaxis) || is.null(input$yaxis)) return (nullPlot("Select both X- and Y-axes"))
    if (!is.null(hf) && nlevels(dat[,hf]) > 9)
    {
cat ("hf test, nlevels(dat[,hf])=",nlevels(dat[,hf]),"\n")
      return (nullPlot(paste0("Number of horizontal facets= ",nlevels(dat[,hf]),"> 9")))
    }
    if (!is.null(vf) && nlevels(dat[,vf]) > 9)
    {
cat ("vf test hit, nlevels(dat[,vf])=",nlevels(dat[,vf]),"\n")
      return (nullPlot(paste0("Number of vertical facets= ",nlevels(dat[,vf]),"> 9")))
    }
    chk = if ("RunTitle" %in% c(input$xaxis, vf, hf, pb, input$yaxis)) 
          c("RunTitle","StandID","Year") else c("MgmtID","StandID","Year") 
    if (input$plotType != "box") for (v in chk) 
    { 
      if (input$plotType %in% c("line","scat","DMD","StkCht") && v=="Year") next
      if (v %in% names(dat) && nlevels(dat[[v]]) > 1 && 
          ! (v %in% c(input$xaxis, vf, hf, pb, input$yaxis)))
          
          
        return(nullPlot(paste0("Variable '",v,"' has ",nlevels(dat[[v]])," levels and ",
                               " therefore must be an axis, plot-by code, or a facet.")))
    }
    pltp = input$plotType
    if (input$xaxis == "Year" && !(pltp %in% c("bar","box"))) dat$Year = as.numeric(as.character(dat$Year))
    nlv  = 1 + (!is.null(pb)) + (!is.null(vf)) + (!is.null(hf))    
    vars = c(input$xaxis, vf, hf, pb, input$yaxis)                                        
    nd = NULL
    sumOnSpecies = !"Species"  %in% vars && "Species"  %in% names(dat) && 
                    nlevels(dat$Species)>1 
    sumOnDBHClass= !"DBHClass" %in% vars && "DBHClass" %in% names(dat) && 
                    nlevels(dat$DBHClass)>1   
    for (v in vars[(nlv+1):length(vars)])
    {
      if (is.na(v) || !v %in% names(dat)) return(nullPlot())
      pd = dat[,c(vars[1:nlv],v),drop=FALSE]
      names(pd)[ncol(pd)] = "Y"
      if (sumOnSpecies) pd = cbind(pd,Species =dat$Species)
      if (sumOnDBHClass)pd = cbind(pd,DBHClass=dat$DBHClass)
      nd = rbind(nd, data.frame(pd,Legend=v,stringsAsFactors=FALSE))
    }
cat("sumOnSpecies=",sumOnSpecies," sumOnDBHClass=",sumOnDBHClass,"\n") 
    if (sumOnSpecies) 
    {
      nd=subset(nd,Species!="All")
      nd$Species="Sum"
    }
    if (sumOnDBHClass) 
    {
      nd=subset(nd,DBHClass!="All")
      nd$DBHClass="Sum"
    }
    if (sumOnSpecies||sumOnDBHClass) 
    {
      nd=ddply(nd,setdiff(names(nd),"Y"),.fun=function (x) sum(x$Y))
      names(nd)[ncol(nd)]="Y"
    }
    if (nlevels(nd[[input$xaxis]])>7 && max(nchar(levels(nd[[input$xaxis]]))) > 6 && 
        isolate(input$XlabRot) == "0" && !globals$gFreeze) 
      updateSelectInput(session=session,inputId="XlabRot",selected="90")
    hrvFlag = NULL
    if (input$plotType %in% c("line","DMD","StkCht"))
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
    if (input$plotType %in% c("line","DMD","StkCht") &&
        length(unique(nd$X)) < 2) return(nullPlot(
          "Selected plot type requires more than 1 unique value on the X-axis"))    
    if (!is.null(nd$vfacet)) nd$vfacet = ordered(nd$vfacet, levels=sort(unique(nd$vfacet)))
    if (!is.null(nd$hfacet)) nd$hfacet = ordered(nd$hfacet, levels=sort(unique(nd$hfacet)))
    if (!is.null(nd$Legend)) nd$Legend = ordered(nd$Legend, levels=sort(unique(nd$Legend)))
    fg = if (!is.null(nd$vfacet) && !is.null(nd$hfacet)) facet_grid(vfacet~hfacet) else NULL
    if (input$facetWrap == "Off")
    {
      fg = if (is.null(fg) && !is.null(nd$hfacet)) facet_grid(.~hfacet) else fg
      fg = if (is.null(fg) && !is.null(nd$vfacet)) facet_grid(vfacet~.) else fg
    } else {
      fg = if (is.null(fg) && !is.null(nd$hfacet)) 
           facet_wrap(~hfacet,ncol=ceiling(sqrt(nlevels(nd$hfacet))),strip.position="top") else fg
      fg = if (is.null(fg) && !is.null(nd$vfacet)) 
           facet_wrap(~vfacet,ncol=ceiling(sqrt(nlevels(nd$vfacet))),strip.position="right") else fg
    }
    p = ggplot(data=nd) + fg + labs(
          x=if (nchar(input$xlabel)) input$xlabel else input$xaxis, 
          y=if (nchar(input$ylabel)) input$ylabel else input$yaxis, 
          title=input$ptitle)  + 
            theme(text = element_text(size=9),
            panel.background = element_rect(fill="gray95"),
            axis.text = element_text(color="black"))
    if (!is.null(fg)) p = p + 
      theme(strip.text.x = element_text(margin = margin(.025, .01, .025, .01, "in"))) +
      theme(strip.text.y = element_text(margin = margin(.025, .01, .025, .01, "in")))
    colors = if (input$colBW == "B&W") 
      unlist(lapply(seq(0,.3,.05),function (x) rgb(x,x,x))) else
        {
          if (is.null(input$color1)) cbbPalette else
            c(input$color1,input$color2,input$color3,input$color4, input$color5, input$color6,
              input$color7,input$color8,input$color9,input$color10,input$color11,input$color12,
              input$color13,input$color14,input$color15,input$color16,input$color17,input$color18)
        }
    colors = autorecycle(colors,nlevels(nd$Legend))
    linetypes = autorecycle(c("solid","dashed","dotted","dotdash","longdash","twodash"),
                             nlevels(nd$Legend))
    alpha = if (is.null(input$transparency)) .7 else (1-input$transparency)
cat ("Legend nlevels=",nlevels(nd$Legend)," colors=",colors,"\n")
    p = p + theme(axis.text.x = element_text(angle = as.numeric(input$XlabRot), 
      hjust = if(input$XlabRot=="0") .5 else 1))
    p = p + theme(axis.text.y = element_text(angle = as.numeric(input$YlabRot), 
      hjust = if(input$YlabRot!="0") .5 else 1))
    p = p + scale_colour_manual(values=colors)
    p = p + scale_fill_manual(values=colors)
    p = p + scale_shape_manual(values=1:nlevels(nd$Legend))
    scale_linetype_manual(values=linetypes)
    p = p + scale_linetype_manual(values=1:nlevels(nd$Legend)) 
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
    if (input$plotType == "DMD")
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
    if (input$plotType == "StkCht")
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
        if (! (pltp %in% c("bar","box"))) p = p + scale_x_continuous(breaks=brkx,
          limits=rngx,guide=guide_axis(check.overlap = TRUE))
      }
cat("xlim=",xlim," rngx=",rngx," brkx=",brkx,"\n")
    } else p = p + scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

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
        if (! (pltp %in% c("bar","box"))) p = p + scale_y_continuous(breaks=brky,
          limits=rngy,guide = guide_axis(check.overlap = TRUE))
      }
cat("ylim=",ylim," rngy=",rngy," brky=",brky,"\n")
    } else p = p + scale_y_discrete(guide = guide_axis(check.overlap = TRUE))
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
 
    if (is.factor(nd$X)) nd$X = as.ordered(nd$X)
    if (is.factor(nd$Y)) nd$Y = as.ordered(nd$Y)
    pltp = input$plotType 
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
    outfile = "www/plot.png" 
    fvsOutData$plotSpecs$res    = as.numeric(if (is.null(input$res)) 150 else input$res)
    fvsOutData$plotSpecs$width  = as.numeric(input$width)
    fvsOutData$plotSpecs$height = as.numeric(input$height)        
    CairoPNG(outfile, width=fvsOutData$plotSpecs$width, 
                      height=fvsOutData$plotSpecs$height, units="in", 
                      res=fvsOutData$plotSpecs$res)              
    print(p)
    dev.off()
    globals$gFreeze = FALSE
    list(src = outfile) 
  }, deleteFile = FALSE)

  observe(  
    if (input$copyplot > 0)
    {
cat ("copyToClipboard copyplot\n")
      session$sendCustomMessage(type="copyEltToClipboard", "outplot") 
    }
  )
  
  ## Stands tab 
  observe({    
    if (input$topPan == "Runs" || input$rightPan == "Stands") 
    {
cat ("Stands\n")     
      initNewInputDB(session,output,dbGlb)
      loadStandTableData(globals, dbGlb$dbIcon)
      updateStandTableSelection(session,input,globals)
      loadVarData(globals,input,dbGlb$dbIcon)                                              
      updateVarSelection(globals,session,input)
    }
  })
    
  ## inTabs has changed
  observe({
    if (is.null(input$inTabs)) return()
    reloadStandSelection(session,input)
cat ("inTabs\n")
  })
  
  
  ## inVars has changed
  observe({
    if (is.null(input$inVars)) return()
    globals$activeVariants = input$inVars
    globals$activeExtens = c("base",globals$activeFVS[[paste0("FVS",globals$activeVariants)]][-1])    
    reloadStandSelection(session,input)
cat ("inVars globals$activeVariants=",globals$activeVariants,
     " globals$activeExtens=",globals$activeExtens," \n")
  })

  reloadStandSelection <- function (session,input)
  isolate({
cat ("in reloadStandSelection\n")
    if (is.null(input$inTabs) || is.null(input$inVars)) return()
    sid = if (input$inTabs %in% c("FVS_PlotInit","FVS_PlotInit_Plot")) "StandPlot_ID" else "Stand_ID"      
    grps = try(dbGetQuery(dbGlb$dbIcon,paste0('select ',sid,",Groups from ",input$inTabs,
              ' where lower(variant) like "%',tolower(input$inVars),'%"')))
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
      colnames(dd) = c(if (input$inTabs %in% c("FVS_PlotInit","FVS_PlotInit_Plot"))
                           "StandPlot_ID" else "Stand_ID","Grp")      
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
  })

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
         dbWriteTable(dbGlb$dbIcon,DBI::SQL("temp.SGrps"),data.frame(SelGrps = input$inGrps))
         sid = if (input$inTabs %in% c("FVS_PlotInit","FVS_PlotInit_Plot"))
               "StandPlot_ID" else "Stand_ID"
         stds = try(dbGetQuery(dbGlb$dbIcon,paste0('select ',sid,' from temp.Grps ',
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
    if (input$saveRun > 0)
    {
cat ("saveRun\n")
      saveRun(input,session)
      updateSelectInput(session=session, inputId="runSel", 
                        choices=globals$FVS_Runs,selected=globals$FVS_Runs[[1]])
    } 
  })

  ## New run    
  observe({
    if (input$newRun > 0)
    {
      saveRun(input,session)
      resetfvsRun(globals$fvsRun,globals$FVS_Runs)
      globals$fvsRun$title <- nextRunName(names(globals$FVS_Runs))
      storeFVSRun(dbGlb$prjDB,globals$fvsRun)
      globals$FVS_Runs = getFVSRuns(dbGlb$prjDB)
cat("New run, calling resetGlobals\n")
      resetGlobals(globals,TRUE)
      if (length(globals$GenGrp)) globals$GenGrp <- list()
      if (length(globals$GrpNum)) globals$GrpNum <- numeric(0)
      updateTextInput(session=session, inputId="title", value=globals$fvsRun$title)
cat ("in new run, globals$fvsRun$defMgmtID=",globals$fvsRun$defMgmtID,"\n")
      updateTextInput(session=session, inputId="defMgmtID",
                      value=globals$fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="simCont",choices=list())
      output$contCnts <- renderUI(HTML(paste0("<b>Contents</b><br>",
        length(globals$fvsRun$stands)," stand(s)<br>",
        length(globals$fvsRun$grps)," group(s)")))
      updateSelectInput(session=session, inputId="addMgmtCats",choices=list())
      updateSelectInput(session=session, inputId="addMgmtCmps",choices=list())
      updateTextInput(session=session, inputId="startyr", 
                      value=globals$fvsRun$startyr)
      updateTextInput(session=session, inputId="endyr", 
                      value=globals$fvsRun$endyr)
      updateTextInput(session=session, inputId="cyclelen", 
                      value=globals$fvsRun$cyclelen)
      updateTextInput(session=session, inputId="cycleat", 
                      value=globals$fvsRun$cycleat)
      updateTextInput(session=session, inputId="inReps",value="1") 
      updateTextInput(session=session, inputId="inRwts",value="1") 
      output$runProgress <- renderUI(NULL)
      updateSelectInput(session=session, inputId="rightPan", 
                        selected="Stands")     
      updateSelectInput(session=session, inputId="compTabSet", 
                        selected="Management")
      updateSelectInput(session=session, inputId="runScript", 
                        selected="fvsRun")
      updateCheckboxGroupInput(session=session, "autoOut", choices=list(
                        "Tree lists (FVS_Treelist, FVS_CutList (StdStk-stand and stock))"="autoTreelists",
                        "Carbon and fuels (FVS_Carbon, FVS_Consumption, FVS_Hrv_Carbon, FVS_Fuels)"="autoCarbon",
                        "Fire and mortality (FVS_Potfire, FVS_BurnReport, FVS_Mortality)"="autoFire",
                        "Snags and down wood (FVS_SnagSum, FVS_Down_Wood_Cov, FVS_Down_Wood_Vol)"="autoDead",
                        "FFE canopy profile (FVS_CanProfile)"="autoCanProfile",          
                        "FFE detailed snag (FVS_SnagDet)"="autoSnagDet",  
                        "Stand structure (FVS_StrClass)"="autoStrClass",
                        "Calibration stats (FVS_CalibStats)"="autoCalibStats",  
                        "Climate-FVS (FVS_Climate)"="autoClimate",                                    
                        "Economics (FVS_EconSummary, FVS_EconHarvestValue)"="autoEcon",
                        "Mistletoe detail by tree size (FVS_DM_Sz_Sum)"="autoDM_Sz_Sum",  
                        "Western Root Disease summary (FVS_RD_Sum)"="autoRD_Sum",  
                        "Western Root Disease details (FVS_RD_Det)"="autoRD_Det",  
                        "Western Root Disease bark beetles (FVS_RD_Beetle)"="autoRD_Beetle",
                        "Inventory Statistics (FVS_Stats_Species, FVS_Stats_Stand)"="autoInvStats",
                        "Regeneration (All Variants: FVS_Regen_Sprouts, FVS_Regen_SitePrep, FVS_Regen_Tally. 
                         AK, EM, KT, IE, and CI variants also get: FVS_Regen_HabType, FVS_Regen_Ingrowth)"="autoRegen",
                        "Produce all standard FVS text outputs (otherwise some are suppressed)"="autoDelOTab"  
                        ), selected=list())   
      isolate ({
        loadStandTableData(globals, dbGlb$dbIcon)
        updateSelectInput(session=session, inputId="inTabs", choices=globals$selStandTableList,
          selected=if (length(globals$selStandTableList)) globals$selStandTableList[[1]] else NULL)          
        updateSelectInput(session=session, inputId="inGrps", NULL, NULL)
        updateSelectInput(session=session, inputId="inStds", NULL, NULL)
        updateTabsetPanel(session=session, inputId="rightPan",selected="Stands")
        loadVarData(globals,input,dbGlb$dbIcon)
        updateVarSelection(globals,session,input)
      })
      updateSelectInput(session=session, inputId="runSel", 
                        choices=globals$FVS_Runs,selected=globals$fvsRun$uuid)
      globals$changeind <- 0
      output$contChange <- renderUI("Run")
    }
  })    
      

  ## Duplicate run
  observe({
    if (input$dupRun > 0)
    {
      if (length(globals$FVS_Runs) == 0) return()
      saveRun(input,session)
      globals$fvsRun$title <- mkNameUnique(globals$fvsRun$title,names(globals$FVS_Runs))
      globals$fvsRun$uuid  <- uuidgen()
      globals$fvsRun$defMgmtID <- nextMgmtID(length(globals$FVS_Runs))
      storeFVSRun(dbGlb$prjDB,globals$fvsRun)
      globals$FVS_Runs=getFVSRuns(dbGlb$prjDB)
      updateTextInput(session=session, inputId="title", label="Run title", 
                      value=globals$fvsRun$title) 
      updateTextInput(session=session, inputId="defMgmtID",
                      value=globals$fvsRun$defMgmtID)
      updateSelectInput(session=session, inputId="compTabSet", 
                        selected="Management")
      updateSelectInput(session=session, inputId="compTabSet", 
                        selected="Management") 
      updateSelectInput(session=session, inputId="runSel", 
                        choices=globals$FVS_Runs,selected=globals$fvsRun$uuid)
      globals$changeind <- 0
      output$contChange <- renderUI("Run")
    }
  })    

  updateAutoOut <- function(session,autoOut)
  {
cat ("updateAutoOut called\n")
     if (is.null(names(autoOut))) # block is for backward compatibility, after 2021 it can be deleted.
     { 
       updateCheckboxGroupInput(session=session, inputId="autoOut",
          selected=autoOut)
       if ("autoSVS" %in% unlist(autoOut)) updateCheckboxGroupInput(session=session, 
          inputId="autoSVS",selected="autoSVS")
     } else {
       updateCheckboxGroupInput(session=session, inputId="autoOut",
          selected=autoOut[["autoOut"]])
       updateCheckboxGroupInput(session=session,inputId="autoSVS",selected=autoOut[["svsOut"]][["svs"]])
       updateRadioButtons(session=session,inputId="svsPlotShape",selected=autoOut[["svsOut"]][["shape"]])
       updateNumericInput(session=session,inputId="svsNFire",value=as.numeric(autoOut[["svsOut"]][["nfire"]])) 
     }
  }
 
  ## Reload or Run Selection   
  observe({
    if (input$reload > 0 || !is.null(input$runSel))
    isolate({
      if (length(globals$fvsRun$uuid) && input$runSel != globals$fvsRun$uuid) saveRun(input,session)
cat ("reload or run selection, runSel=",input$runSel," lensim=",
length(globals$fvsRun$simcnts)," globals$currentQuickPlot=",globals$currentQuickPlot,"\n")      
      if (length(globals$currentQuickPlot) &&
          globals$currentQuickPlot != input$runSel)
      {
cat("setting uiRunPlot to NULL\n")        
        output$uiRunPlot <- output$uiErrorScan <- renderUI(NULL)
        globals$currentQuickPlot = character(0)        
      }
      output$titleBuild <-output$condBuild <- output$cmdBuild <- output$cmdBuildDesc <- output$fvsFuncRender <- renderUI (NULL)
      progress <- shiny::Progress$new(session,min=1,max=5)
      progress$set(message = "Loading selected run",value = 1)
      resetGlobals(globals,FALSE)
      sel = match (input$runSel,globals$FVS_Runs) 
      if (is.na(sel)) sel = 1 
      saveFvsRun = loadFVSRun(dbGlb$prjDB,globals$FVS_Runs[sel])
      # make sure the saved object has the correct class. This will fix load errors from old projects
      if (! identical(attributes(class(saveFvsRun)),attributes(class(globals$fvsRun))))
        attributes(class(saveFvsRun)) = attributes(class(globals$fvsRun))
      globals$fvsRun = saveFvsRun

      if (length(globals$fvsRun$stands)) for (i in 1:length(globals$fvsRun$stands))
      {
        if (length(globals$fvsRun$stands[[i]]$grps) > 0) 
          for (j in 1:length(globals$fvsRun$stands[[i]]$grps))
          { 
            if (length(globals$fvsRun$stands[[i]]$grps[[j]]$cmps) > 0) 
              for (k in 1:length(globals$fvsRun$stands[[i]]$grps[[j]]$cmps))
              {
                test <- globals$fvsRun$stands[[i]]$grps[[j]]$cmps[[k]]$kwds
                spgtest <- grep("^SpGroup",test)
                cntr <- 0                                                                                   
                spgname <- list()
                if (length(spgtest))
                {
                  cntr<-cntr+1
                  spgname[cntr] <- trim(unlist(strsplit(strsplit(test, split = "\n")[[1]][1],
                  split=" "))[length(unlist(strsplit(strsplit(test, split = "\n")[[1]][1],split=" ")))])
                  if(!length(globals$GrpNum)) globals$GrpNum[1] <- 1 else
                  globals$GrpNum[(length(globals$GrpNum)+1)] <- length(globals$GrpNum)+1
                  
                  spgname[1] <- gsub(" ","", spgname[1])
                  tmpk <- match(spgname[1], globals$GenGrp)
                  if (!is.na(tmpk)) 
                  {
                    globals$GrpNum <- globals$GrpNum[-length(globals$GrpNum)]
                  } else globals$GenGrp[length(globals$GrpNum)]<-spgname
                }
              }
          } 
      }
      resetGlobals(globals,TRUE)
      tmp = unlist(globals$activeFVS[globals$fvsRun$FVSpgm])
      globals$lastRunVar = if (length(tmp) && !is.null(tmp)) tmp[1] else 
        if (length(globals$fvsRun$FVSpgm) && nchar(globals$fvsRun$FVSpgm)>4) 
          substring(globals$fvsRun$FVSpgm,4) else character(0)         
      mkSimCnts(globals$fvsRun,sels=globals$fvsRun$selsim,
        justGrps=isolate(input$simContType)=="Just groups")
      output$uiCustomRunOps = renderUI(NULL)    
cat ("reloaded globals$fvsRun$title=",globals$fvsRun$title," uuid=",globals$fvsRun$uuid,"\n")      
cat ("reloaded globals$fvsRun$runScript=",globals$fvsRun$runScript,"\n")
      if (length(globals$fvsRun$uiCustomRunOps)) lapply(names(globals$fvsRun$uiCustomRunOps), function (x,y)
cat ("globals$fvsRun$uiCustomRunOps$",x,"=",y[[x]],"\n",sep=""),globals$fvsRun$uiCustomRunOps) else
cat ("globals$fvsRun$uiCustomRunOps is empty\n")
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
      progress$set(message = paste0("Setting values for run ", globals$fvsRun$title),
            value = 2)
      updateAutoOut(session, globals$fvsRun$autoOut)
      updateTextInput(session=session, inputId="title", value=globals$fvsRun$title)
cat ("in Reload, globals$fvsRun$defMgmtID=",globals$fvsRun$defMgmtID,"\n")
      updateTextInput(session=session, inputId="defMgmtID",
                      value=globals$fvsRun$defMgmtID)
      for (id in c("addMgmtCats","addMgmtCmps","addModCats","addModCmps",
                   "addEvCmps","addKeyExt","addKeyWds"))
         updateSelectInput(session=session, inputId=id,selected=0)
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
      loadVarData(globals,input,dbGlb$dbIcon)
      updateVarSelection(globals,session,input)
      output$contCnts <- renderUI(HTML(paste0("<b>Contents</b><br>",
        length(globals$fvsRun$stands)," stand(s)<br>",
        length(globals$fvsRun$grps)," group(s)")))
      updateStandTableSelection(session,input,globals)
      loadVarData(globals,input,dbGlb$dbIcon)                                              
      updateVarSelection(globals,session,input)
      # if the update causes a change in the runscript selection, then
      # customRunOps will get called automatically. If it is the same
      # script then it needs to be called here to update/set the settings.
      progress$set(message = "Setting custom run options ",value = 4)
      callCustom = length(globals$fvsRun$runScript) && 
                   globals$fvsRun$runScript == input$runScript
      updateSelectInput(session=session, inputId="runScript", 
          selected=globals$fvsRun$runScript)
      if (callCustom) customRunOps()
      progress$close()
    })
  })
  
  ##autoOut
  observe({
    if(length(input$autoOut) || length(input$autoSVS))
    {
      out<-list(svsOut=list(svs=input$autoSVS,shape=input$svsPlotShape,nfire=input$svsNFire),
                autoOut=as.list(input$autoOut))
      if (identical(out,globals$fvsRun$autoOut)) return()
      globals$fvsRun$autoOut <- out
      updateAutoOut(session, globals$fvsRun$autoOut)
      globals$changeind <- 1
      output$contChange <- renderText(HTML("<b>*Run*</b>"))
    }
  })

  ## inAdd:    Add Selected Stands
  observe({
    if (input$inAdd > 0) 
    {
cat ("input$inAdd=",input$inAdd,"\n")
      addStandsToRun(session,input,output,selType="inAdd",globals,dbGlb)
      updateVarSelection(globals,session,input)
    }
  })  
  ## inAddGrp: Add all stands in selected groups
  observe({
    if (input$inAddGrp > 0) 
    {
cat (" input$inAddGrp=",input$inAddGrp,"\n")
      addStandsToRun(session,input,output,selType="inAddGrp",globals,dbGlb)
      updateVarSelection(globals,session,input)
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
    mkSimCnts(globals$fvsRun,sels=input$simCont[[1]],justGrps=isolate(input$simContType=="Just groups"))
    updateSelectInput(session=session, inputId="simCont", 
         choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
  })
  
  ## findStand (set run element to item if found)
  observe({
    if (input$searchNext== 0) return()      
    isolate ({
cat ("searchNext: string=",input$searchString,"\n")
      if (nchar(input$searchString) == 0) return()
    elt = findStand(globals,search=input$searchString)
cat ("elt=",elt,"\n")
    if (is.null(elt)) return()
    if (input$simContType=="Just groups") updateRadioButtons(session=session, 
       inputId="simContType", selected="Full run")
    mkSimCnts(globals$fvsRun,sels=elt,justGrps=input$simContType=="Just groups")
    updateSelectInput(session=session, inputId="simCont", 
         choices=globals$fvsRun$simcnts, selected=elt)
  })})

  ## Edit  
  observe({
    if (input$editSel == 0) return()      
    isolate ({
      output$titleBuild <-output$condBuild <- output$cmdBuild <- output$cmdBuildDesc <- output$fvsFuncRender <- renderUI (NULL)   
      globals$currentEditCmp <- globals$NULLfvsCmp
      if (length(input$simCont) == 0) return()
      toed = input$simCont[1]
      # find component
      cmp = findCmp(globals$fvsRun,toed)
      if (is.null(cmp)) return()
      globals$currentEditCmp = cmp
      if (length(cmp$kwdName) == 0) cmp$kwdName="freeEdit"
cat ("Edit, cmp$kwdName=",cmp$kwdName,"\n")
      eltList = NULL
      if (cmp$kwdName=="freeEdit"){
        eltList <- mkFreeformEltList(globals,input,prms,cmp$title,cmp$kwds)
        if(cmp$atag=="c"){
        rtn <- list(h5(),div(myInlineTextInput("cmdTitle","Condition title ", 
                             value=globals$currentEditCmp$title,size=40)),h5())
        }else 
        rtn <- list(h5(),div(myInlineTextInput("cmdTitle","Component title ", 
                             value=globals$currentEditCmp$title,size=40)),h5())
        if(length(globals$currentEditCmp$title)) rtn <- append(rtn,list(
              h4(paste0('Edit: "',globals$currentEditCmp$title),'"')),after=0)  
        output$titleBuild <- renderUI(rtn)
      } else
      {
        if (length(cmp$kwdName) && nchar(cmp$kwdName))
        { 
          if (exists(cmp$kwdName)) #if a function exists, use it.
          {
            eltList <- eval(parse(text=paste0(cmp$kwdName,
              "(globals$currentEditCmp$title,prms,globals,input,output)")))
            if (is.null(eltList)) return(NULL)
            eltList <- eltList[[1]]
            rtn <- list(h5(),div(myInlineTextInput("cmdTitle","Component title ", 
                                 value=globals$currentEditCmp$title,size=40)),h5())
            if(length(globals$currentEditCmp$title)) rtn <- append(rtn,list(
                  h4(paste0('Edit: "',globals$currentEditCmp$title),'"')),after=0)  
            output$titleBuild <- renderUI(rtn)
          } else {
            pk <- match (cmp$kwdName,names(prms))
            if (!is.na(pk)) # FreeForm Edit, used if pk does not match a parms.
            {        # Launch general purpose builder when pk matches a parms.        
              pkeys <- prms[[pk]]
              eltList <- mkeltList(pkeys,prms,globals,input,output,
                         cmp$atag=="c",FALSE,globals$currentEditCmp$title)     
            }
          }
        }
      }   
      if (is.null(eltList)){
        rtn <- list(h5(),div(myInlineTextInput("cmdTitle","Component title ", 
                             value=globals$currentEditCmp$title,size=40)),h5())
        rtn <- append(rtn,list(
        h4(paste0('Edit: "',globals$currentEditCmp$title),'"')),after=0)  
        output$titleBuild <- renderUI(rtn)
        eltList <- mkFreeformEltList(globals,input,prms,globals$currentEditCmp$title,
                            globals$currentEditCmp$kwds)
      }
      eltList <- append(eltList,list(
           tags$style(type="text/css", "#cmdCancel {color:red;}"),
           actionButton("cmdCancel","Cancel"),
           tags$style(type="text/css", "#cmdSaveInRun {color:green;}"),
           actionButton("cmdSaveInRun","Save in run")))
      output$cmdBuild <- renderUI(eltList)
      output$cmdBuildDesc <- output$fvsFuncRender <- renderUI (NULL)
      if (input$rightPan != "Components") {
        updateTabsetPanel(session=session,
        inputId="rightPan",selected="Components")
        updateSelectInput(session=session,
        inputId="compTabSet", selected="Management")
      }
      if (input$rightPan == "Components" && input$compTabSet !="Management") {
        updateSelectInput(session=session,
        inputId="compTabSet", selected="Management")
      output$cmdBuildDesc <- output$fvsFuncRender <- renderUI (NULL)
      }
      # for (id in c("addMgmtCats","addMgmtCmps","addModCats","addModCmps",
      #              "addEvCmps","addKeyExt","addKeyWds"))
      #    updateSelectInput(session=session, inputId=id, selected=0)
    })
  })
  # install callback functionality for the textarea that has the focus 
  # to get start and end selection poistions.
  observe({
    if (length(input$focusedElement) && 
        input$focusedElement %in% c("freeEdit","condDisp"))
      session$sendCustomMessage(type="getStartEnd", input$focusedElement)    
  }) 
  observe({
    if (length(input$freeSpecies) && nchar(input$freeSpecies)) 
      insertStringIntoFocusedTextarea(input,input$focusedElement,input$freeSpecies)
  })
  observe({
    if (length(input$freeVars) && nchar(input$freeVars)) 
      insertStringIntoFocusedTextarea(input,input$focusedElement,input$freeVars)
  })
  observe({
    if (length(input$freeOps) && nchar(input$freeOps))
      insertStringIntoFocusedTextarea(input,input$focusedElement,input$freeOps)
  })  
  observe({
    if (length(input$freeFuncs) && nchar(input$freeFuncs)) isolate({
      pkeys = prms[[paste0("evmon.function.",input$freeFuncs)]] 
      if (is.null(pkeys)) return()
      eltList <- mkeltList(pkeys,prms,globals,input,output,funcflag=TRUE)
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
      insertStringIntoFocusedTextarea(input,input$focusedElement,string)
    })
  }) 

  insertStringIntoFocusedTextarea <- function(input,textarea,string)
  {
    isolate({
      if (is.null(textarea)) textarea="freeEdit"
      if (!is.null(string) && nchar(trim(string)) > 0) 
      {
        if (length(input$selectionStart)) 
        {              
          start = input$selectionStart
          end   = input$selectionEnd
        } else { start=0;end=0 }
        len = nchar(input[[textarea]])
cat ("insertStringIntoFocusedTextarea textarea=",textarea," string=",string," start=",start," end=",end," len=",len,"\n")
        if (nchar(string) == 0) return()
        if (start == end && end == len) {         # prepend 
          updateTextInput(session, textarea, value = paste0(input[[textarea]],string))
        } else if (start == 0 && end == start) {  # append
          updateTextInput(session, textarea, value = paste0(string,input[[textarea]]))
        } else if (end >= start) {                # insert/replace
          str = input[[textarea]]
          updateTextInput(session, textarea, value = 
            paste0(substring(input[[textarea]],1,max(1,start)),string,
                   substring(input[[textarea]],min(end+1,len))))
        }
      }
      updateSelectInput(session=session, inputId="freeOps", selected=1)
      updateSelectInput(session=session, inputId="freeVars",selected=1)
      updateSelectInput(session=session, inputId="freeSpecies",selected=1)
      updateSelectInput(session=session, inputId="freeFuncs",selected=1)
      output$fvsFuncRender <- renderUI (NULL)
      session$sendCustomMessage(type="refocus", textarea) 
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
        updateReps(globals)
        mkSimCnts(globals$fvsRun,justGrps=input$simContType=="Just groups") 
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
      output$titleBuild <-output$condBuild <- output$cmdBuild <- output$cmdBuildDesc <- output$fvsFuncRender <- renderUI (NULL)   
      output$contChange <- renderText(HTML("<b>*Run*</b>"))
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
cat("paste, pidx=",pidx,"\n")
      if (is.null(pidx)) return()
      topaste = globals$pastelist[[pidx]]
      if (length(grep("^SpGroup",topaste$kwds)))
      {
cat("paste, SpGroup hit\n")
        cntr <- 0
        if(!length(globals$GrpNum)) globals$GrpNum[1] <- 1 else
          globals$GrpNum[(length(globals$GrpNum)+1)] <- length(globals$GrpNum)+1
        globals$GenGrp[length(globals$GrpNum)] <- topaste$reopn[[1]]
      }
cat("paste, class(topaste)=",class(topaste),"\n")
      if (class(topaste) != "fvsCmp") return()
      topaste = mkfvsCmp(kwds=topaste$kwds,kwdName=topaste$kwdName,
              exten=topaste$exten,variant=topaste$variant,uuid=uuidgen(),
              atag=topaste$atag,title=topaste$title,reopn=topaste$reopn)
      idx = pasteComponent(globals$fvsRun,input$simCont[1],topaste)
      if (!is.null(idx))
      {
        mkSimCnts(globals$fvsRun,justGrps=input$simContType=="Just groups")   
        updateSelectInput(session=session, inputId="simCont", 
           choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
        output$contCnts <- renderUI(HTML(paste0("<b>Contents</b><br>",
          length(globals$fvsRun$stands)," stand(s)<br>",
          length(globals$fvsRun$grps)," group(s)")))
      }
      globals$foundStand=0L 
      globals$changeind <- 1
      output$contChange <- renderText(HTML("<b>*Run*</b>"))
    })
  })
   
  
  # Change to freeform
  observe({
    if (input$mkfree == 0) return()
    isolate ({
      globals$currentEditCmp <- globals$NULLfvsCmp
      updateSelectInput(session=session, inputId="addMgmtCmps", selected = 0)
      if (length(input$simCont) == 0) return ()
      toed = input$simCont[1]
      cmp = findCmp(globals$fvsRun,toed)
      cmp$kwdName="freeEdit"
      cmp$title=paste("Freeform: ",cmp$title)
      cmp$reopn=character(0)
      mkSimCnts(globals$fvsRun,sels=toed,justGrps=input$simContType=="Just groups")
      updateSelectInput(session=session, inputId="simCont", 
           choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
      output$titleBuild <-output$condBuild <- output$cmdBuild <- 
        output$cmdBuildDesc <- output$fvsFuncRender <- renderUI (NULL)    
    })                    
  })

  ## Command Set.
  observe({
cat ("compTabSet, input$compTabSet=",input$compTabSet,
     " input$simCont=",length(input$simCont),"\n")
    if(!length(globals$currentEditCmp$kwds) || input$compTabSet !="Management")
    {
      output$titleBuild <-output$condBuild <- output$cmdBuild <- 
        output$cmdBuildDesc <- output$fvsFuncRender <- renderUI (NULL)   
    } 
    if (length(globals$fvsRun$FVSpgm) == 0) return(NULL)
    if (! globals$fvsRun$FVSpgm %in% names(globals$activeFVS)) return(NULL)
    switch (input$compTabSet,
      "Management" = 
      {                                                        
        if (length(globals$mgmtsel) == 0) globals$mgmtsel <- mkMgmtCats(globals)
        updateSelectInput(session=session, inputId="addMgmtCats", 
          choices=mkpair(globals$mgmtsel), selected = 0)
        updateSelectInput(session=session, inputId="addMgmtCmps", 
          choices=list())
      },                                                     
      "Modifiers"  = 
      {
        if (length(globals$mmodsel) == 0) globals$mmodsel <- mkModMCats(globals)
        updateSelectInput(session=session, inputId="addModCats", 
          choices=mkpair(globals$mmodsel), selected = 0)
        updateSelectInput(session=session, inputId="addModCmps", 
          choices=list())
      },
      "Event Monitor"= 
      {                      
        if (length(globals$mevsel) == 0) globals$mevsel <- mkEvMonCats(globals)
        updateSelectInput(session=session, inputId="addEvCmps",
            selected = 0,choices=mkpair(globals$mevsel[[1]]))
      },
      "Economic"= 
      {
        renderComponent(input,output,"ecn")
      },
      "Keywords"   = 
      {
        if (length(globals$extnsel) == 0) mkextkwd(prms,globals)
         updateSelectInput(session=session, inputId="addKeyExt", 
            label="Extensions", choices=globals$extnsel, selected = 0)
         updateSelectInput(session=session, inputId="addKeyWds", 
            label="Keywords", choices=list())
      },
      "Editor"   =                                                   
      {
        if(length(globals$currentEditCmp$kwds) > 0) closeCmp()
        loadObject(dbGlb$prjDB,"customCmps")
        globals$customCmps = if (exists("customCmps")) customCmps else list()
        if (length(globals$customCmps)) updateSelectInput(session=session,
        inputId="kcpSel",choices=as.list(names(customCmps)), 
        selected=names(customCmps)[1]) else
        updateSelectInput(session=session,inputId="kcpSel",choices=list())
        
        eltList <- mkFreeformEltList(globals,input,prms,globals$currentEditCmp$title,
                            globals$currentEditCmp$kwds)
        output$condBuild <- renderUI(NULL)
        output$cmdBuild <-renderUI(eltList)
        output$fvsFuncRender <- renderUI (NULL)
        output$cmdBuildDesc <- renderUI(paste0("Description: This Editor menu allows you to", 
          " utilize the advanced features of the freeform text format for creating custom",
          " component sets by directly adding & editing keyword records and Event Monitor",
          " functions. You can upload an existing keyword component file (.kcp), or keyword",
          " component archive (FVS_kcps.Rdata) and then save it into the Run Contents window",
          " on the left (Save in run), and also save it in the component collection (Save in",
          " component collection). You can also create your own component sets by appending",
          " items from the Run Contents on the left (Append selected component from run)", 
          " and then saving them into your component collection (Save in component collection).",
          " Finally, you can download a text file of your component set (Download(KCP))."))
      },
      NULL)   
  })
  
  observe({
    if (length(input$kcpEdit)) 
    {
      session$sendCustomMessage(type="getStartEnd", "kcpEdit")
    }
  })
  observe({
    if (length(input$freeSpeciesKCP) && nchar(input$freeSpeciesKCP)) isolate({
      if (length(input$kcpEdit) == 0) return()
      insertStrinIntokcpEdit(input,input$freeSpeciesKCP)
    })
  })
  observe({
    if (length(input$freeVarsKCP) && nchar(input$freeVarsKCP)) isolate({
      if (length(input$kcpEdit) == 0) return()
      insertStrinIntokcpEdit(input,input$freeVarsKCP)
    })
  })
  observe({
    if (length(input$freeOpsKCP) && nchar(input$freeOpsKCP)) isolate({
      if (length(input$kcpEdit) == 0) return()
      insertStrinIntokcpEdit(input,input$freeOpsKCP)
    })
  })  
  observe({
    if (length(input$freeFuncsKCP) && nchar(input$freeFuncsKCP)) isolate({
      if (length(input$kcpEdit) == 0) return()
      pkeys = prms[[paste0("evmon.function.",input$freeFuncsKCP)]] 
      if (is.null(pkeys)) return()
      eltList <- mkeltList(pkeys,prms,globals,input,output,funcflag=TRUE)
      eltList <- append(eltList,list(
        actionButton("fvsFuncInsertKCP","Insert function"),
        actionButton("fvsFuncCancelKCP","Cancel function"),h6()))
      output$fvsFuncRender <- renderUI(eltList)
    })
  })  
  observe({  #fvsFuncCancelKCP
    if (length(input$fvsFuncCancelKCP) && input$fvsFuncCancelKCP) 
    {
      output$fvsFuncRender <- renderUI (NULL)
      updateSelectInput(session=session, inputId="freeFuncsKCP",selected=1)
    }
  })
  observe({  #fvsFuncInsert
    if (length(input$fvsFuncInsertKCP) && input$fvsFuncInsertKCP)
    isolate({
      pkeys = prms[[paste0("evmon.function.",input$freeFuncsKCP)]]
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
      insertStrinIntokcpEdit(input,string)
    })
  }) 
  
  insertStrinIntokcpEdit <- function(input,string)
  {
    if (is.null(string) || nchar(string) == 0 || string == " ") return()
    isolate({
      if (length(input$selectionStart)) 
      {
        start = input$selectionStart
        end   = input$selectionEnd
      } else { start=0;end=0 } 
      len   = nchar(input$kcpEdit)
cat ("insertStrinIntokcpEdit string=",string," start=",start," end=",end," len=",len,"\n")
      if (nchar(string) == 0) return()
      if (start == end && end == len) {         # prepend 
        updateTextInput(session, "kcpEdit", value = paste0(input$kcpEdit,string))
      } else if (start == 0 && end == start) {  # append
        updateTextInput(session, "kcpEdit", value = paste0(string,input$kcpEdit))
      } else if (end >= start) {                # insert/replace
        str = input$kcpEdit
        updateTextInput(session, "kcpEdit", value = 
          paste0(substring(input$kcpEdit,1,max(1,start)),string,
                 substring(input$kcpEdit,min(end+1,len))))
      }
      updateSelectInput(session=session, inputId="freeOpsKCP", selected=1)
      updateSelectInput(session=session, inputId="freeVarsKCP",selected=1)
      updateSelectInput(session=session, inputId="freeSpeciesKCP",selected=1)
      updateSelectInput(session=session, inputId="freeFuncsKCP",selected=1)
      output$fvsFuncRender <- renderUI (NULL)
    })
  }
  
  ## addMgmtCats
  observe({
    if (is.null(input$addMgmtCats)) return()
    if (length(globals$mgmtsel)==0) globals$mgmtsel<-mkMgmtCats(globals)
    updateSelectInput(session=session, inputId="addMgmtCmps", selected = 0, 
      choices=globals$mgmtsel[[as.numeric(input$addMgmtCats)]])
    output$titleBuild <-output$condBuild <- output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
  })
  ## addModCats
  observe({
    if (is.null(input$addModCats)) return()
    if (length(globals$mmodsel) == 0) globals$mmodsel <- mkModMCats(globals)
    updateSelectInput(session=session, inputId="addModCmps", selected = 0, 
          choices=globals$mmodsel[[as.numeric(input$addModCats)]])
    output$titleBuild <-output$condBuild <- output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
  })
  ## addKeyExt
  observe({
    if (is.null(input$addKeyExt))
      updateSelectInput(session=session, inputId="addKeyWds", selected = 0, 
          choices=NULL) else
      {
        if (length(globals$mevsel) == 0) globals$mevsel <- mkEvMonCats(globals)
        updateSelectInput(session=session, inputId="addKeyWds", selected = 0, 
          choices=globals$kwdsel[[input$addKeyExt]])
      }
    output$titleBuild <-output$condBuild <- output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
  })
  ## addMgmtCmps
  observe({
    if (length(input$addMgmtCmps) && 
        nchar(input$addMgmtCmps)) renderComponent(input,output,"mgt")
  })
  ## addModCmps
  observe({
    if (length(input$addModCmps) && 
        nchar(input$addModCmps)) renderComponent(input,output,"mod") 
  })
  ## addKeyWds
  observe({
    if (length(input$addKeyWds) && 
        nchar(input$addKeyWds)) renderComponent(input,output,"key")
  })
  ## addEvent
  observe({
    if (length(input$addEvCmps) && 
        nchar(input$addEvCmps)) renderComponent(input,output,"evn")
  })
                
  renderComponent <- function(input,output,inCode="default")
  {
cat ("renderComponent, inCode=",inCode,"\n")
    isolate ({
      output$titleBuild <-output$condBuild <- output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
      globals$currentEditCmp <- globals$NULLfvsCmp
      globals$currentCndPkey <- character(0)
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
          globals$currentCmdPkey=globals$mevsel[[1]][as.numeric(input$addEvCmps)]
          title = names(globals$currentCmdPkey)
        },
        "ecn" =
        {
          title = "Economic analysis"
          globals$currentCmdPkey = "econ Econ_reports"
        },
        return(NULL)
      ) 
cat ("globals$currentCmdPkey=",globals$currentCmdPkey," title=",title,"\n")
      cmdp = scan(text=globals$currentCmdPkey,what="character",sep=" ",quiet=TRUE)
      if(length(cmdp)>1)cmdp <- cmdp[2] else cmdp <- cmdp[1]
      # the cmdp can be a function name, or a ".Win" can be appended to form a 
      # function name.  If a function does not exist, then try finding a prms entry.
      if (exists(cmdp)) funName = cmdp 
      funName = paste0(cmdp,".Win")
      if (!exists(funName)) funName = cmdp
      if (!exists(funName)) funName = NULL
cat ("funName=",funName,"\n")
      if (!is.null(funName)) 
      { 
        globals$winBuildFunction <- funName
          ans = eval(parse(text=paste0(globals$winBuildFunction,
            "(title,prms,globals,input,output)")))
          if (is.null(ans)) return(NULL)
          ans[[1]] <- append(ans[[1]],list(
             tags$style(type="text/css", "#cmdCancel {color:red;}"),
             actionButton("cmdCancel","Cancel"),
             tags$style(type="text/css", "#cmdSaveInRun {color:green;}"),
             actionButton("cmdSaveInRun","Save in run")))
          if (length(grep("freeEdit",ans[[1]]))==0) ans[[1]] <- append(ans[[1]],
            list(tags$style(type="text/css","#cmdChgToFree {color:black}"),
                 actionButton("cmdChgToFree","Change to freeform")))
          rtn <- list(h5(),div(myInlineTextInput("cmdTitle","Component title ", value=title,size=40)),h5())
          output$titleBuild <- renderUI(rtn)
          output$cmdBuild     <- renderUI (if (length(ans[[1]])) ans[[1]] else NULL)
          output$cmdBuildDesc <- renderUI (if (length(ans[[2]])) ans[[2]] else NULL)
      } else { 
        globals$winBuildFunction <- character(0)
        indx = match(cmdp,names(prms))
        if (is.na(indx)) return()
        pkeys <- prms[[indx]]
        eltList <- try(mkeltList(pkeys,prms,globals,input,output,FALSE,FALSE,title))
        if (class(eltList)=="try-error")
        {
          output$cmdBuildDesc = renderUI (HTML(paste0(
            '<br>Error:<br>Programming for "',title,'" is incorrect.<br>')))
          return()
        }
        eltList <- append(eltList,list(
           tags$style(type="text/css", "#cmdCancel {color:red;}"),
           actionButton("cmdCancel","Cancel"),
           tags$style(type="text/css", "#cmdSaveInRun {color:green;}"),
           actionButton("cmdSaveInRun","Save in run"),
           actionButton("cmdChgToFree","Change to freeform")))
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
      if (length(globals$toggleind)) globals$currentCndPkey <- character(0)
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
        uiOutput("condElts")))
    } else {
      globals$currentCndPkey <- character(0)
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
    if (length(globals$toggleind) && input$schedbox == 1) return()
cat("make condElts, input$condList=",input$condList,"\n") 
    if (input$condList == "none") output$condElts <- renderUI(NULL) else
    {
      cnpkey <- paste0("condition.",input$condList)
      idx <- match(cnpkey,names(prms))
      globals$currentCndPkey <- if (is.na(idx)) character(0) else cnpkey 
      ui = if (identical(globals$currentCndPkey,character(0))) NULL else
      {
        eltList <- mkeltList(prms[[globals$currentCndPkey]],prms,
                             globals,input,output,cndflag=TRUE)
        if (length(eltList) == 0) NULL else eltList
      }
      if (!is.null(ui))
      {                               
        title = getPstring(prms$conditions_list,input$condList)
        if (!is.null(title)) 
        {
          ui <- append(ui,list(myInlineTextInput("condTitle","Condition title", 
                      value=title, size=40)),after=1)
          output$condElts <- renderUI(ui)
        }
      }
    }
  })
  
  
  observe({       
    if (length(input$cmdChgToFree) == 0 || input$cmdChgToFree==0) return()
    isolate({
cat ("cmdChgToFree=",input$cmdChgToFree,"\n") 
       # process the condition first...if there is one.
      if (length(globals$toggleind)>0 && length(globals$currentCndPkey) &&
                     !is.null(input$schedbox) && input$schedbox == 2) 
       {
cat ("cmdChgToFree processing condition\n") 
         kwds = mkCondKeyWrd(globals,prms,input)
         attr(kwds$kwds,"keywords") = "condDisp"
         globals$currentCndPkey=kwds$kwds
         updateTextInput(session, "condTitle",value=paste0("Freeform: ",input$condTitle))
         condUI <- list(myInlineTextInput("condTitle","Condition title", 
              value=paste0("Freeform: ",input$condTitle), size=40),
              tags$style(type="text/css", 
                "#condDisp{font-family:monospace;font-size:90%;height:1in;width:100%;cursor:auto;}"), 
              tags$script('$(document).ready(function(){ $("textarea").on("focus", function(e){ Shiny.setInputValue("focusedElement", e.target.id);}); }); '),
              tags$textarea(id="condDisp",kwds$kwds),
              myInlineTextInput("cmdTitle","Component title", 
              value=paste0("Freeform: ",input$cmdTitle), size=40))
         output$titleBuild <- renderUI(NULL)
       } else {
         titleUI <- list(h5(),div(myInlineTextInput("cmdTitle","Component title ", paste0("Freeform: ",input$cmdTitle),size=40)),h5())
         output$titleBuild <- renderUI(titleUI)
         condUI <- NULL
       }
cat ("cmdChgToFree processing component\n") 
       if (length(globals$winBuildFunction))
       {
         kwPname = globals$winBuildFunction
         pkeys = character(0)
       } else {
         kwPname = scan(text=globals$currentCmdPkey,what="character",sep=" ",quiet=TRUE)
         pkeys = if (length(kwPname)>1) prms[[kwPname[2]]] else prms[[kwPname[1]]]
       }
       kwds = buildKeywords(character(0),pkeys,kwPname,globals)
       attr(kwds$kwds,"keywords") = "freeEdit"
       attr(kwds$kwds,"extension") = kwds$ex
       globals$currentCmdPkey = kwds$kwds
       globals$winBuildFunction = character(0)
       cmdUI <- mkFreeformEltList(globals,input,prms,paste0("Freeform: ",input$cmdTitle),
                                  kwds$kwds)
       cmdUI <- append(cmdUI,list(
                tags$style(type="text/css", "#cmdCancel {color:red;}"),
                actionButton("cmdCancel","Cancel"),
                tags$style(type="text/css", "#cmdSaveInRun {color:green;}"),
                actionButton("cmdSaveInRun","Save in run")))
       output$condBuild <- renderUI(condUI)
       output$cmdBuild <- renderUI(cmdUI)
       output$cmdBuildDesc <- renderUI(NULL)
       session$sendCustomMessage(type="refocus", "freeEdit")
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
    output$titleBuild <-output$condBuild <- output$cmdBuild <- output$cmdBuildDesc <- renderUI (NULL)
  }
  
  mkCondKeyWrd <- function (globals,prms,input)
  {
    kwPname = globals$currentCndPkey
cat ("mkCondKeyWrd, kwPname=",kwPname,"\n")
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
    reopn = c(reopn,as.character(if (is.null(instr)) character(0) else instr))
    names(reopn)[length(names(reopn))] = "waitYears"
    kwds = sprintf("%-10s%10s\n","If",if (is.null(instr)) " " else instr)
    kwds = paste0(kwds,mkKeyWrd(ansFrm,reopn,pkeys,globals$activeVariants[1]),
           "\nThen")
    list(reopn=reopn,kwds=kwds)
  }
  
  buildKeywords <- function(oReopn,pkeys,kwPname,globals)
  {
cat ("in buildKeywords, oReopn=",oReopn," kwPname=",kwPname,"\n")
    if (length(pkeys) == 0 && nchar(kwPname)) 
    {
      # try to find a function that can make the keywords
      fn = paste0(kwPname,".mkKeyWrd")
      ans = if (exists(fn)) eval(parse(text=paste0(fn,"(input,output)"))) else NULL
    } else { 
      # build from prms entry
      ansFrm = getPstring(pkeys,"answerForm",globals$activeVariants[1])
      if (is.null(ansFrm)) 
      { 
        kw = if (length(kwPname) > 1) kwPname[2] else kwPname[1]
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
        reopn = c(reopn,as.character(if (is.null(instr)) " " else instr))
        names(reopn)[fn] = pkey
      }       
      kwds = if ("waitYears" %in% names(oReopn))
      {
        instr = input[["waitYears"]]
        if (!is.null(instr))
        { 
          reopn = c(reopn,as.character(if (is.null(instr)) " " else instr))
          names(reopn)[length(names(reopn))] = "waitYears"
          kwds = sprintf("%-10s%10s\n","If",if (is.null(instr)) " " else instr)
          paste0(kwds,mkKeyWrd(ansFrm,reopn,pkeys,globals$activeVariants[1]),
                 "\nThen")
        }
      } else mkKeyWrd(ansFrm,reopn,pkeys,globals$activeVariants[1])
      ans = list(ex=if (length(kwPname) > 1) kwPname[1] else if (length(grep("keyword.",kwPname))) gsub("[.].*","",gsub("keyword.","",kwPname))
        else "base", kwds=kwds,reopn=reopn)
      if (length(kwPname) > 1 && length(grep("keyword.",kwPname))){
        kwd <- gsub("[.].*","",gsub("keyword.","",kwPname))
        if(kwd[2]=="estbstrp"){
          ans[1] <- if(length(grep("strp",globals$activeExtens))) "strp" else "estb"
        }
      }
    }                        
    ans
  }

  observe({  
    # Save in run                             
    if (length(input$cmdSaveInRun) && input$cmdSaveInRun == 0) return() 
    isolate ({
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp) &&
          identical(globals$currentCndPkey,character(0))  && 
          identical(globals$currentCmdPkey,character(0))) return()
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
          mkSimCnts(globals$fvsRun,justGrps=input$simContType=="Just groups")   
          updateSelectInput(session=session, inputId="simCont", 
             choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
        }
        globals$currentEditCmp <- globals$NULLfvsCmp
        closeCmp()                      
        return()
      }     
      if (identical(globals$currentCndPkey,character(0))) newcnd = NULL else 
      if (is.null(attr(globals$currentCndPkey,"keywords")))
      { 
        kwds = mkCondKeyWrd(globals,prms,input)
        newcnd = mkfvsCmp(uuid=uuidgen(),atag="c",exten="base",
                 kwdName=globals$currentCndPkey,title=input$condTitle,
                 kwds=kwds$kwds,reopn=kwds$reopn)
      } else {
        newcnd = mkfvsCmp(uuid=uuidgen(),atag="c",
                 exten="base",kwdName="freeEdit",title=input$condTitle,
                 kwds=if (attr(globals$currentCndPkey,"keywords")=="condDisp") 
                      input$condDisp else input$freeForm,
                 reopn=character(0))
      }
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
          if (!is.null(attr(globals$currentCmdPkey,"keywords"))) 
          {
            kwPname = attr(globals$currentCmdPkey,"keywords") 
            pkeys=NULL
          } else {
            kwPname = scan(text=globals$currentCmdPkey,what="character",sep=" ",quiet=TRUE)
            pkeys = if (length(kwPname)>1) prms[[kwPname[2]]] else prms[[kwPname[1]]]
          }
        }
        oReopn  = character(0) 
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
          mkSimCnts(globals$fvsRun,sels=input$simCont[[1]],
                    justGrps=input$simContType=="Just groups")
          updateSelectInput(session=session, inputId="simCont", 
             choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
          globals$changeind <- 1
          output$contChange <- renderText(HTML("<b>*Run*</b>"))
          closeCmp()
          return()
        }  
      }
cat ("Building a component: kwPname=",kwPname,"\n")
      ans = if (kwPname=="freeEdit") list(ex=attr(globals$currentCmdPkey,"extension"),
          reopn=NULL,kwds=input$freeEdit) else buildKeywords(oReopn,pkeys, kwPname,globals)
      gensps <- grep("SpGroup", ans$kwds)
      if(length(gensps)) 
      { 
        cntr <- 0
        if(!length(globals$GrpNum)) globals$GrpNum[1] <- 1 else
        globals$GrpNum[(length(globals$GrpNum)+1)] <- length(globals$GrpNum)+1
        grlist <- list()
        for (spg in 1:length(ans$reopn)) if(try(ans$reopn[spg])!=" ")
        {
          cntr<-cntr+1
          grlist[cntr]<-ans$reopn[spg]
        }
        # prevent duplicate SpGroup names due to editing & saving non-name changes
        grlist[1] <- gsub(" ","", grlist[1])
        tmpk <- match(grlist[1], globals$GenGrp)
        if (is.na(tmpk) && !length(globals$currentEditCmp$kwds)) 
          globals$GenGrp[length(globals$GrpNum)]<-grlist
        if (is.na(tmpk) && length(globals$currentEditCmp$kwds))
        {
          globals$GrpNum <- globals$GrpNum[-length(globals$GrpNum)]
          globals$GenGrp <- globals$GenGrp[-length(globals$GenGrp)]
          globals$GenGrp[length(globals$GrpNum)]<-grlist
        }
        if (!is.na(tmpk) && length(globals$currentEditCmp$kwds))
          globals$GrpNum <- globals$GrpNum[-length(globals$GrpNum)]
      } 
      if (identical(globals$currentEditCmp,globals$NULLfvsCmp))
      {
        newcmp = mkfvsCmp(uuid=uuidgen(),atag="k",kwds=ans$kwds,exten=ans$ex,
             variant=globals$activeVariants[1],kwdName= if (length(kwPname)>1) kwPname[2] else kwPname[1],
             title=input$cmdTitle,
             reopn=if (is.null(ans$reopn)) character(0) else ans$reopn)
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
          } else { 
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
        globals$currentEditCmp$kwds=ans$kwds
        globals$currentEditCmp$title=input$cmdTitle
cat ("saving, kwds=",ans$kwds," title=",input$cmdTitle," reopn=",ans$reopn,"\n")       
         globals$currentEditCmp$reopn=if (is.null(ans$reopn)) character(0) else ans$reopn
         globals$currentEditCmp=globals$NULLfvsCmp
      }      
      mkSimCnts(globals$fvsRun,sels=input$simCont[[1]],justGrps=input$simContType=="Just groups")
      updateSelectInput(session=session, inputId="simCont", 
         choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
      globals$changeind <- 1
      output$contChange <- renderText(HTML("<b>*Run*</b>"))
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
    if(!length(input$simCont) || !length(globals$fvsRun$startyr) || 
                           globals$fvsRun$startyr==input$startyr) return()
    globals$fvsRun$startyr  <- input$startyr
    globals$changeind <- 1
    output$contChange <- renderText(HTML("<b>*Run*</b>"))
  })
  ## time--end year
  observe({
    if(!length(input$simCont) || !length(globals$fvsRun$endyr) || 
                                globals$fvsRun$endyr==input$endyr) return()
    globals$fvsRun$endyr <- input$endyr
    globals$changeind <- 1
    output$contChange <- renderText(HTML("<b>*Run*</b>"))
  })
  ## time--cycle length
  observe({
    if(!length(input$simCont) || !length(globals$fvsRun$cyclelen) || globals$fvsRun$cyclelen==input$cyclelen) return()
    globals$fvsRun$cyclelen <- input$cyclelen
    globals$changeind <- 1
    output$contChange <- renderText(HTML("<b>*Run*</b>"))
  }) 
  ## time--cycle breaks
  observe({
    if(!length(input$simCont) || (length(globals$fvsRun$cycleat) && 
       length(input$cycleat) && globals$fvsRun$cycleat==input$cycleat)) return()
    globals$fvsRun$cycleat  <- input$cycleat
    globals$changeind <- 1
    output$contChange <- renderText(HTML("<b>*Run*</b>"))
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
        saveRun(input,session)
cat("Nulling uiRunPlot at Save and Run\n")
        output$uiRunPlot <- output$uiErrorScan <- renderUI(NULL)
        globals$currentQuickPlot = character(0)                                  
        updateSelectInput(session=session, inputId="runSel", 
            choices=globals$FVS_Runs,selected=globals$FVS_Runs[[1]]) 
        killIfRunning(globals$fvsRun$uuid)
        # if rerunning a run that is currently selected in the "View Outputs",
        # then clear those tools.
        if (globals$fvsRun$uuid %in% input$runs) initTableGraphTools(globals,session,output,fvsOutData)
        progress$set(message = "Run preparation: ", 
          detail = "Deleting old ouputs", value = 2)         
        removeFVSRunFiles(globals$fvsRun$uuid)
        updateSelectInput(session=session, inputId="bkgRuns", 
                          choices=getBkgRunList(),selected=0)
        progress$set(message = "Run preparation: ", 
          detail = "Write .key file and prepare program", value = 3)
        newSum = !("FVS_Summary" %in% try(myListTables(dbGlb$dbOcon)))
        msg=writeKeyFile(globals,input,dbGlb$dbIcon,newSum=newSum)
        if(globals$timeissue==1){
          progress$close()
          updateTabsetPanel(session=session,inputId="rightPan",selected="Time")
        }
        fc = paste0(globals$fvsRun$uuid,".key")
        if (!file.exists(fc))
        {
          if(msg=="Run data query returned no data to run."){
cat ("Run data query returned no data to run.\n")  
          progress$set(message = "Error: Keyword file was not created. Try re-importing
                       the inventory database associated with this run.",
                      detail = msg, value = 3) 
          Sys.sleep(10)
          progress$close()     
          return()  
          } else {
cat ("keyword file was not created.\n")
          progress$set(message = "Error: Keyword file was not created.",
                      detail = msg, value = 3) 
          Sys.sleep(5)
          progress$close()     
          return()
          }
        }
        dir.create(globals$fvsRun$uuid)
        if (!dir.exists(globals$fvsBin)) 
        {
          progress$set(message = paste0("Error: ",globals$fvsBin," does not exist."),
                      detail = "", value = 3) 
          Sys.sleep(5)
          progress$close()     
          return()
        }
cat ("runwaitback=",input$runwaitback,"\n")
        if (input$runwaitback!="Wait for run")
        {
          runScript = paste0(globals$fvsRun$uuid,".rscript")
          rs = file(runScript,open="wt")
          cat (paste0('setwd("',getwd(),'")\n'),file=rs)
          cat ('options(echo=TRUE)\nlibrary(fvsOL)\nlibrary(methods)\n',file=rs)
          cat ('pid = Sys.getpid()\n',file=rs)
          cmd = paste0('unlink("',globals$fvsRun$uuid,'.db")')
          cat (cmd,"\n",file=rs)
          cmd = paste0("title = '",globals$fvsRun$title,"'")
          cat (cmd,"\n",file=rs)                   
          cmd = paste0("nstands = ",length(globals$fvsRun$stands))
          cat (cmd,"\n",file=rs)          
          cmd = paste0("fvsLoad('",
             globals$fvsRun$FVSpgm,"',bin='",globals$fvsBin,"')")
          cat (cmd,"\n",file=rs)
          if (globals$fvsRun$runScript != "fvsRun")
          {
            # if the custom run script exists in the project dir, use it, otherwise
            # look in the system extdata directory to find it in the package
            cmdfil=paste0("customRun_",globals$fvsRun$runScript,".R")
            if (!file.exists(cmdfil)) cmdfil=system.file("extdata", cmdfil, package = "fvsOL")
            if (file.exists(cmdfil)) 
            {
              cmd=paste0("source('",cmdfil,"')") 
              cat (cmd,"\n",file=rs)
              cat ("runOps = ",deparse(globals$fvsRun$uiCustomRunOps),"\n",file=rs) 
            }
          }
          foo = foo = paste0(globals$fvsRun$uuid,".key")
          cmd = paste0('fvsSetCmdLine("--keywordfile=',foo,'")')
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
          cmd = paste0('dbOcon = dbConnect(drv = dbDriver("SQLite"),"FVSOut.db")')
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
        #on exit of the reactive context
        on.exit({          
          progress$close()
cat ("exiting, stop fvschild\n")          
          try(stopCluster(fvschild))
        }) 
        clusterEvalQ(fvschild,library(rFVS))
        cmd = paste0("clusterEvalQ(fvschild,fvsLoad('",
             globals$fvsRun$FVSpgm,"',bin='",globals$fvsBin,"'))")
cat ("load FVSpgm cmd=",cmd,"\n")          
        rtn = try(eval(parse(text=cmd)))
        if (class(rtn) == "try-error") return()          
        # if not using the default run script, load the one requested.    
        if (globals$fvsRun$runScript != "fvsRun")
        {
          rsFn = paste0("customRun_",globals$fvsRun$runScript,".R")
          if (!file.exists(rsFn)) rsFn = system.file("extdata", rsFn, package = "fvsOL")
          if (!file.exists(rsFn)) return()
          cmd = paste0("clusterEvalQ(fvschild,source('",rsFn,"'))")
cat ("run script load cmd=",cmd,"\n")
          rtn = try(eval(parse(text=cmd)))
          if (class(rtn) == "try-error") return()
          runOps <- if (is.null(globals$fvsRun$uiCustomRunOps)) list() else 
            globals$fvsRun$uiCustomRunOps
          rtn = try(clusterExport(fvschild,list("runOps"),envir=environment())) 
          if (class(rtn) == "try-error") return()
        }
        foo = paste0(globals$fvsRun$uuid,".key")
        cmd = paste0("clusterEvalQ(fvschild,",'fvsSetCmdLine("--keywordfile=',foo,'"))')
cat ("load run cmd=",cmd,"\n")
        rtn = try(eval(parse(text=cmd))) 
        if (class(rtn) == "try-error") return()
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
cat ("rtn class for stand i=",i," is ",class(rtn),"\n")
          if (class(rtn) == "try-error")
          { 
            cat ("run try error\n")
            return()
          }
          rtn = rtn[[1]]
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
        outf=paste0(globals$fvsRun$uuid,".out")
        errScan = try(errorScan(outf))
        if (class(errScan) == "try-error") errScan = 
          "Error scan failed likely due to invalid multibyte strings in output"
        output$uiErrorScan <- renderUI(list(
          h6(paste0("Run made with: ",globals$fvsRun$FVSpgm)," ",attr(errScan,"pgmRV")),
          h5("FVS error scan: "),
          tags$style(type="text/css", paste0("#errorScan { overflow:auto; ",
             "height:150px; font-family:monospace; font-size:90%;}")),
          HTML(paste(errScan,"<br>"))))
        if (length(dir(globals$fvsRun$uuid)) == 0) 
          unlink(globals$fvsRun$uuid,recursive = TRUE, force = TRUE)
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
        unlink(paste0(globals$fvsRun$uuid,".db"))
        progress$set(message = "Building plot", detail = "", 
                     value = length(globals$fvsRun$stands)+6)
        modn = names(allSum)
        toch = unique(modn)
        if (length(toch) != length(modn)) 
        {
          for (chg in toch)
          {
            chrr = chg == modn
            if ((nch <- sum(chrr)) < 2) next
            chg = unlist(strsplit(chg,";"))
            modn[chrr] = sprintf("%s r%03i;%s",chg[1],1:nch,chg[2])
          }
          names(allSum) = modn
        }
        X <- Y <- Stand <- NULL
        for (i in 1:length(allSum)) 
        { 
          X = c(X,allSum[[i]][,"Year"])
          Y = c(Y,allSum[[i]][,"TCuFt"])
          ltag = gsub(x=names(allSum)[i],pattern=";.*$",replacement="")
          ltag = gsub(x=ltag,pattern="^SId=",replacement="")
          Stand=c(Stand,c(rep(ltag,nrow(allSum[[i]]))))
        }
        toplot = data.frame(X = X, Y=Y, Stand=as.factor(Stand))
        toMany = nlevels(toplot$Stand) > 9
        colors = autorecycle(cbbPalette,nlevels(toplot$Stand))
        volType = if (substring(globals$fvsRun$FVSpgm,4) %in% c("cs","ls","ne","sn"))
           "Merchantable" else "Total"
        plt = ggplot(data = toplot) + scale_colour_manual(values=colors) +
            geom_line (aes(x=X,y=Y,color=Stand)) +
            labs(x="Year", y=paste0(volType," cubic volume per acre")) + 
            theme(text = element_text(size=6), 
              legend.position=if (toMany) "none" else "right",
              axis.text = element_text(color="black")) 
        width=if (toMany) 3 else 4
        height=2.5
        CairoPNG("www/quick.png", width=width, height=height, units="in", res=150)
        print(plt)
        dev.off()
        output$uiRunPlot <- renderUI(
                plotOutput("runPlot",width="100%",height=paste0((height+1)*144,"px")))
        output$runPlot <- renderImage(list(src="www/quick.png", width=(width+1)*144, 
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
         # limit the number of rows exported to Excel to 1,048,576
         excelRowLimit=1048576
         runuuid = globals$fvsRun$uuid
         if (is.null(runuuid)) return()
         tabs = myListTables(dbGlb$dbOcon)
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
           qry = if (!is.null(cmpYes) && cmpYes && substr(tab,1,3) == "Cmp")
             paste0("select * from ",tab," limit ",excelRowLimit,";") else
             paste0("select * from ",tab," where ",tab,".CaseID in",
                    " (select CaseID from ",casesToGet,") limit ",excelRowLimit,";")
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
        excelRowLimit=1048576
        if (isolate(input$dlRDType) == ".csv")
        {
          if (nrow(fvsOutData$render) > 0)
            write.csv(fvsOutData$render,file=tf,row.names=FALSE) else 
            cat (file=tf,'"No data"\n')
        } else {
          if (nrow(fvsOutData$render) > 0)
          {
            if (nrow(fvsOutData$render) > excelRowLimit) 
              write.xlsx(fvsOutData$render[1:excelRowLimit,],file=tf,colNames = TRUE) else 
              write.xlsx(fvsOutData$render,file=tf,colNames = TRUE) 
          } else write.xlsx(file=tf)
        }          
      }, contentType=NULL)
  ## dlPrjBackup
  output$dlPrjBackup <- downloadHandler(filename=function ()
      isolate({
        bckupPick <- input$pickBackup
        if (file.exists(bckupPick)) bckupPick else "NoBackup.txt"
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
        if (file.exists(sfile))
        {
          file.copy(sfile,tf)
          # use perl to change line endings, ignore if an error is detected
          if (!isLocal()) try(system(paste0("perl -pi -e 's/\\n/\\r\\n/' ",tf)))
        } else cat (file=tf,"Output not yet created.\n")
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
  
  ## Download FVSProjectData.zip 
  output$dlFVSRunZip <- downloadHandler(filename="FVSProjectData.zip",
     content = function (tf = tempfile())
         {
           tempDir = paste0(dirname(tf),"/tozip")
           if (dir.exists(tempDir)) lapply(paste0(tempDir,"/",dir(tempDir)),unlink) else
               dir.create(tempDir)
           spatdat = "SpatialData.RData" 
           for (ele in input$dlZipSet)
           {
cat ("building download, ele=",ele,"\n")
             switch (ele,
               outdb = {
                 from="FVSOut.db"
                 to=file.path(tempDir,from)
                 if (file.exists(from)) file.copy(from=from,to=to) else
                   cat (file=to,"Output database does not exist.\n")
               },
               key   = {
                 from=paste0(input$runSel,".key")
                 to=file.path(tempDir,paste0(globals$fvsRun$title,"_FVSkeywords.txt")) 
                 if (file.exists(from)) file.copy(from=from,to=to)                   
               },
               out   = {
                 from=paste0(input$runSel,".out")
                 to=paste0(tempDir,"/",globals$fvsRun$title,"_FVSoutput.txt")
                 if (file.exists(from)) file.copy(from=from,to=to) 
               },
               subdir= {
                 from=input$runSel
                 if (dir.exists(from)) 
                 {
                   to = file.path(tempDir,paste0(globals$fvsRun$title,"_SVS"))
                   dir.create (to)
                   file.copy(from=from,to=to,recursive = TRUE)
                   file.copy(from=paste0(from,"_index.svs"),to=to)
                 } 
               },              
               FVS_Data = file.copy(from="FVS_Data.db" ,
                                    to=file.path(tempDir,"FVS_Data.db")),
               fvsProjdb = {
                 rdat="FVSProject.db"
                 if (file.exists(rdat)) file.copy(from=rdat,to=file.path(tempDir,rdat))
               },
               SpatialData = {
                 spatdat = "SpatialData.RData" 
               if (file.exists(spatdat)) file.copy(from=spatdat,
                                     to=file.path(tempDir,spatdat))
               }
                                   
           )}
           curdir = getwd()
           setwd(tempDir)
           zipr(tf,dir())
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
        skip <- strsplit(as.character(customCmps),"\n")[[1]][length(strsplit(as.character(customCmps),"\n")[[1]])]=="ENDIF"
        if(length(grep("^--> Kwd",names(globals$kcpAppendConts[length(globals$kcpAppendConts)]))) && !skip)
        {
          updateTextInput(session=session, inputId="kcpEdit", value=
            paste0(customCmps,"EndIf\n"))
          customCmps <-as.list(paste0(customCmps,"EndIf\n"))
          names(customCmps) <- names(globals$customCmps)
          globals$customCmps = customCmps
        }
        storeOrUpdateObject(dbGlb$prjDB,customCmps)
        updateSelectInput(session=session, inputId="kcpSel",
           choices=names(globals$customCmps),
           selected=newTit)
        mkSimCnts(globals$fvsRun,sels=input$simCont[[1]],justGrps=input$simContType=="Just groups")
        updateSelectInput(session=session, inputId="simCont",
           choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
      })
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
          newTit = paste0("Editor: Component ",length(globals$customCmps)+1) 
          updateTextInput(session=session, inputId="kcpTitle", value=newTit)
        } else newTit = paste0("Editor: ",trim(input$kcpTitle))
        newcmp = mkfvsCmp(uuid=uuidgen(),atag="k",kwds=input$kcpEdit,exten="base",
             variant=globals$activeVariants[1],kwdName="FreeEdit",
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
        mkSimCnts(globals$fvsRun,sels=input$simCont[[1]],justGrps=input$simContType=="Just groups")
        updateSelectInput(session=session, inputId="simCont", 
           choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
        globals$changeind <- 1
        output$contChange <- renderText(HTML("<b>*Run*</b>"))
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
          storeOrUpdateObject(dbGlb$prjDB,customCmps)
          updateSelectInput(session=session, inputId="kcpSel", choices=names(customCmps))
        } else {
          customCmps=NULL
          removeObject(dbGlb$prjDB,"customCmps")
          updateSelectInput(session=session, inputId="kcpSel", choices=list())
          updateTextInput(session=session, inputId="kcpTitle", value="")
          updateTextInput(session=session, inputId="kcpEdit", value="")
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
      if (length(globals$customCmps) == 0 && input$kcpUpload$name=="FVS_kcps.RData")
      {
        load(input$kcpUpload$datapath)
        globals$customCmps = customCmps
        addnl = FALSE
      }
      if (length(globals$customCmps) && !is.null(globals$customCmps)){
        updateSelectInput(session=session,inputId="kcpSel",choices=as.list(names(globals$customCmps)),
                          selected=names(globals$customCmps)[1])
      }
      updateTextInput(session=session, inputId="kcpTitle", value=
                        paste("From:",input$kcpUpload$name))
      if(addnl){
        updateTextInput(session=session, inputId="kcpEdit", value=
                          paste(data,collapse="\n"))
      } else {
        updateTextInput(session=session, inputId="kcpEdit", value=globals$customCmps[1])
        save(file="FVS_kcps.RData",customCmps) 
      }
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
  
  ## Download KCP
  output$kcpDownload <- downloadHandler(filename=function ()
      paste0(input$kcpSel,".kcp"),
      content=function (tf = tempfile())
      {
        write(input$kcpEdit,tf)
      }, contentType="text")

  observe({
    if (input$topPan == "SVS3d")
    {
cat ("SVS3d hit\n")
      allRuns = globals$FVS_Runs
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
      output$SVSqImg1Top  = renderUI(NULL)
      output$SVSqImg1Side = renderUI(NULL)
      output$SVSImg1      = renderRglwidget(NULL)
      output$SVSqImg2Top  = renderUI(NULL)
      output$SVSqImg2Side = renderUI(NULL)
      output$SVSImg2      = renderRglwidget(NULL)
    }
  })
               
  mkSVSchoices <- function(svsRun)
  {
    fn = paste0(svsRun,"_index.svs")
    if (!file.exists(fn)) return(list())
    index = read.table(file=fn,as.is=TRUE) 
    dups = duplicated(index[,1])
    if (any(dups))
    {
      dupTab = table(index[dups,1])
      for (id in names(dupTab))
      {
        idxs = grep(id,index[,1],fixed=TRUE)
        rep = 1
        for (idx in idxs) 
        {
          index[idx,1] = sub("Year",paste0("rep ",rep," Year"),index[idx,1])
          rep = rep+1
        }
      }
    }
    choices = as.list(index[,2])
    names(choices) = index[,1]
    choices
  }
            
  observe({
    if (length(input$SVSRunList1))
    {
cat ("SVS3d input$SVSRunList1=",input$SVSRunList1,"\n")
      choices = mkSVSchoices(input$SVSRunList1)
      updateSelectInput(session=session, inputId="SVSImgList1", choices=choices, 
                        selected = 0)
#      output$SVSImg1Top  = renderUI(NULL)
#      output$SVSImg1Side = renderUI(NULL)
      output$SVSImg1     = renderRglwidget(NULL)
    }
  })
  observe({
    if (length(input$SVSRunList2))
    {
cat ("SVS3d input$SVSRunList2=",input$SVSRunList2,"\n")
      choices = mkSVSchoices(input$SVSRunList2)
      updateSelectInput(session=session, inputId="SVSImgList2", choices=choices, 
                        selected = 0)
#      output$SVSImg2Top  = renderUI(NULL)
#      output$SVSImg2Side = renderUI(NULL)
      output$SVSImg2     = renderRglwidget(NULL)
    }
  })
  observe(  
    if (input$svsCopy1 > 0)
    {
      session$sendCustomMessage(type="copyWebGLSnapshotToClipboard", "SVSImg1") 
    }
  )
  observe(  
    if (input$svsCopy2 > 0)
    {
      session$sendCustomMessage(type="copyWebGLSnapshotToClipboard", "SVSImg2") 
    }
  )                    
  svsImage <- reactiveVal("")
  renderSVSImage <- function (id,imgfile,subplots=TRUE,downTrees=TRUE,
                    fireLine=TRUE,rangePoles=TRUE,plotColor="gray")
  {
cat ("renderSVSImage, subplots=",subplots," downTrees=",downTrees,
     " fireLine=",fireLine," rangePoles=",rangePoles,"\n")
    for (dd in rgl.dev.list()) try(rgl.close())
    open3d(useNULL=TRUE) 
    svs = scan(file=paste0(imgfile),what="character",sep="\n",quiet=TRUE)
    treeform = tolower(unlist(strsplit(unlist(strsplit(svs[2],
               split=" ",fixed=TRUE))[2],split=".",fixed=TRUE))[1])
    if (! (treeform %in% names(treeforms))) 
    {
      output[[id]] <- NULL
      return()
    }
    treeform = treeforms[[treeform]]
    rcirc = grep ("^#CIRCLE",svs)
    if (length(rcirc)) 
    {
      rgl.viewpoint(theta = 1, phi = -45, fov = 30, zoom = .8, interactive = TRUE)
      args = as.numeric(scan(text=svs[rcirc[1]],what="character",quiet=TRUE)[2:4])
cat ("args=",args,"\n")
      plotDef = circle3D(x0=args[1],y0=args[2],r=args[3],col=plotColor,alpha=0.7)
      if (subplots && length(rcirc)>1)
      {
        for (cir in rcirc[2:length(rcirc)]) 
        {
          ca = as.numeric(scan(text=svs[cir],what="character",quiet=TRUE)[2:4])
          circle3D(x0=ca[1],y0=ca[2],r=ca[3],alpha=1,fill=FALSE,col="black")
        }
      }
      pltshp=1
    } else { # assume square, look for arguments of the rectangle.
      rgl.viewpoint(theta = 1, phi = -45, fov = 30, zoom = .9, interactive = TRUE)
      rect = grep ("^#RECTANGLE",svs)
      if (length(rect)) 
      {
        args = as.numeric(scan(text=svs[rect],what="character",quiet=TRUE)[4])
        plotDef = matrix(c(0,0,0,0,args,0,args,args,0,args,0,0,0,0,0),ncol=3,byrow=TRUE)
        polygon3d(plotDef,col=plotColor,alpha=0.7)
      }
      pltshp=0
    }                
    if (subplots)
    {
      subplts = grep("^#LINE",svs)
      if (length(subplts))
      {
         crds = as.numeric(scan(text=substring(svs[subplts],6),what="character",quiet=TRUE))
         crds = cbind(matrix(crds,ncol=2,byrow=TRUE),0)
         segments3d(crds,col="black",add=TRUE)
      }
    }
    rpols = grep("^RANGEPOLE",svs)
    if (length(rpols))
    {
      if (rangePoles)
      {
        poles = c()
        for (line in rpols)
        {
          pole = as.numeric(scan(text=svs[line],what="character",quiet=TRUE)[c(21,22,7)])
          poles = c(poles,c(pole[1:2],0,pole))
        } 
        poles = matrix(poles,ncol=3,byrow=TRUE)
        segments3d(poles,col="red",lwd=4,add=TRUE)
      }
      svs=svs[-rpols]
    }
    par3d(ignoreExtent=TRUE) #just use the plot and range poles to define the extent.
    calls = 0
    frlineS = grep("^#FIRE_LINE",svs)
cat ("length(frlineS)=",length(frlineS),"fireLine=",fireLine,"\n")
    if (length(frlineS))
    {
      if (fireLine)
      {
        fl = as.numeric(scan(text=substring(svs[frlineS],11),what="numeric",quiet=TRUE))
        frline=NULL
        if (pltshp)
        {
          xx = seq(0,args[3]*2,length.out=length(fl))
          r = sqrt(((xx-args[3])^2) + ((fl-args[3])^2))
          k = r<=args[3]
          if (any(k))
          {
            frline = matrix(c(xx[k],fl[k],rep(0,sum(k))),ncol=3,byrow=FALSE)
            frline = frline[nrow(frline):1,]
            kep1=which.min(((plotDef[,1]-frline[1,1])**2)+((plotDef[,2]-frline[1,2])**2))
            kep2=which.min(((plotDef[,1]-frline[nrow(frline),1])**2)+((plotDef[,2]-frline[nrow(frline),2])**2))
            frline[1,]=plotDef[kep1,]
            frline[nrow(frline),]=plotDef[kep2,]
            brnReg = rbind(frline[2:(nrow(frline)-1),],plotDef[kep2:nrow(plotDef),])            
            if (kep1<(nrow(plotDef)/2)) brnReg = rbind(plotDef[1:kep1,],brnReg)     
            polygon3d(brnReg,col="black",alpha=0.5)
          }
        } else {
          frline = matrix(c(seq(0,args[1],length.out=length(fl)),
                          fl,rep(0,length(fl))),ncol=3,byrow=FALSE)
          brnReg = rbind(plotDef[1,],frline,plotDef[4:5,])
          polygon3d(brnReg,col="black",alpha=0.5)
        }
        if (!is.null(frline))
        {
          lines3d(frline,col="red",lwd=4,add=TRUE)
          nn=500
          fls  = approx(frline[,1],frline[,2],rule=2,n=nn)
          fls$z = runif(nn)*3
          fls$y = jitter(fls$y,amount=5)
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
      svs = svs[-frlineS]
    }
    progress <- shiny::Progress$new(session,min=1,max=length(svs)+4)
    flames = grep("^@flame.eob",svs)
cat("N flames=",length(flames)," fireLine=",fireLine,"\n")
    if (length(flames))
    {
      if (fireLine)
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
      }
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
      if (!downTrees && tree[9]!="0") next
      sp = tree[1]                                                                            
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
####TESTING if (calls > 60) break
    }
    progress$set(message = "Display trees",value = length(svs)+1) 
    displayTrees(drawnTrees)
    progress$set(message = "Sending image to browser",value = length(svs)+2) 
    output[[id]] <- renderRglwidget(rglwidget(scene3d()))
    progress$close()
    svsImage(id)  # will be used in the quick images logic (maybe)
  }
  observe({
cat ("svsImage()=",svsImage(),"\n")
    if (svsImage()=="") return()
# come back to this for the quick images, need to put them in the ui.R as well.
#    Sys.sleep(.1)
#    session$sendCustomMessage(type="makeTopSideImages", 
#      c(svsImage(),paste0(svsImage(),"Top"),paste0(svsImage(),"Side")))
    svsImage("")
  })

  observe({
    if (length(input$SVSImgList1))
    {
cat ("SVS3d SVSImgList1=",input$SVSImgList1," SVSdraw1=",input$SVSdraw1,"\n")
      fn=input$SVSImgList1
      if (!file.exists(fn)) return()
      renderSVSImage('SVSImg1',fn,
        subplots="subplots" %in% input$SVSdraw1,downTrees="downTrees" %in% input$SVSdraw1,
        fireLine="fireLine" %in% input$SVSdraw1,rangePoles="rangePoles" %in% input$SVSdraw1,
        plotColor=input$svsPlotColor1)
    }
  })
  observe({
    if (length(input$SVSImgList2))
    {
cat ("SVS3d SVSImgList2=",input$SVSImgList2," SVSdraw1=",input$SVSdraw2,"\n") 
      fn=input$SVSImgList2
      if (!file.exists(fn)) return()
      renderSVSImage('SVSImg2',fn,
        subplots="subplots" %in% input$SVSdraw2,downTrees="downTrees" %in% input$SVSdraw2,
        fireLine="fireLine" %in% input$SVSdraw2,rangePoles="rangePoles" %in% input$SVSdraw2,
        plotColor=input$svsPlotColor2)
    }
  })
  ## Maps processing
  observe({
    if (input$topPan == "Maps")
    {
cat ("Maps hit\n")
      require(rgdal) 
      theRuns = try(dbGetQuery(dbGlb$dbOcon,
                paste0("select distinct RunTitle, KeywordFile from FVS_Cases",
                       " order by RunDateTime desc")))
      if (class(theRuns)!="try-error" && nrow(theRuns)>0) 
      {
        allRuns=theRuns[,2]
        names(allRuns)=theRuns[,1]
        updateSelectInput(session=session, inputId="mapDsRunList", 
          choices=allRuns) 
      } else updateSelectInput(session=session, inputId="mapDsRunList",choices=list()) 
      updateSelectInput(session=session, inputId="mapDsTable", choices=list())
      updateSelectInput(session=session, inputId="mapDsVar", choices=list())
      updateSelectInput(session=session, inputId="MapYear", choices=list())
      output$leafletMap = renderLeaflet(NULL)
      output$leafletMessage=renderText(NULL)
    }
   })
  observe({
    if (length(input$mapDsRunList) && input$topPan == "Maps")
    {
cat ("mapDsRunList input$mapDsRunList=",input$mapDsRunList,"\n") 
      cases = try(dbGetQuery(dbGlb$dbOcon,
                         paste0("select CaseID,StandID from FVS_Cases where KeywordFile = '",
                                input$mapDsRunList,"'")))
      if (class(cases)=="try-error") return()
      # if there are reps (same stand more than once), just use the first rep, ignore the others
      cases = cases[!duplicated(cases$StandID),]
      dbExecute(dbGlb$dbOcon,"drop table if exists temp.mapsCases")
      dbWriteTable(dbGlb$dbOcon,DBI::SQL("temp.mapsCases"),cases[,1,drop=FALSE])
      tabs = setdiff(myListTables(dbGlb$dbOcon),
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
      sps = na.omit(match(c("SpeciesFVS","SpeciesPLANTS","SpeciesFIA"),vars))
      if (length(sps)==3) vars = vars[-sps]
      vars = vars[! vars == "Characteristic"]
      vars = as.list(vars)
      names(vars) = vars
      updateSelectInput(session=session, inputId="mapDsVar", choices=vars,
                        selected=0) 
      output$leafletMap = renderLeaflet(NULL)
    }
  })
  observe({
    if (length(input$mapDsVar) && !is.na(match(input$mapDsVar,setdiff(
      dbListFields(dbGlb$dbOcon,input$mapDsTable), c("CaseID","StandID","Year")))))
    {
cat ("mapDsRunList input$mapDsTable=",isolate(input$mapDsTable),
     " input$mapDsVar=",input$mapDsVar," input$mapDsType=",input$mapDsType,"\n")     
      # prepare display data
      dispData = try(dbGetQuery(dbGlb$dbOcon,paste0("select * from ",
                   isolate(input$mapDsTable),
                   " where CaseID in (select CaseID from temp.mapsCases)")))
      if (class(dispData)=="try-error" || nrow(dispData)==0) return()
      dispData = dispData[,-1] #remove CaseID
      # if species is a variable, pick the one to display and ditch the others
      sps = na.omit(match(c("SpeciesFVS","SpeciesPLANTS","SpeciesFIA"),names(dispData)))
      if (length(sps)==3) 
      {
        spk = match(paste0("Species",input$spCodes),names(dispData))
        names(dispData)[spk]="Species"
        dispData = dispData[,-sps[sps!=spk]]
        spk = "Species"
      } else { spk = NULL }
      keys = setdiff(colnames(dispData),c("StandID","Year","Characteristic",spk))
      for (var in keys) 
      {
        if (class(dispData[,var]) == "character") 
        {
          x = suppressWarnings(as.numeric(dispData[,var]))
          if (!any(is.na(x))) dispData[,var] = x
        }
      }
      dvs = intersect(names(dispData),
            c("StandID","Year","Characteristic",spk,input$mapDsVar))
      isp = match("Species",dvs)
      if (!is.na(isp) && isp != 3) dvs=c(dvs[1:2],"Species",dvs[-c(1,2,isp)])
      dispData = dispData[,dvs]
      uidsToGet = unique(dispData$StandID)
cat ("length(uidsToGet)=",length(uidsToGet),"\n")
      if (!length(uidsToGet)) return()        
      uidsFound = NULL   
      library(rgdal) 
      spatdat = "SpatialData.RData"
      if (!exists("SpatialData",envir=dbGlb,inherit=FALSE) && 
          file.exists(spatdat)) load(spatdat,envir=dbGlb)
      pts = NULL
      ptsLbs  = NULL
      if (exists("SpatialData",envir=dbGlb,inherit=FALSE)) 
      {
        matchVar = attr(dbGlb$SpatialData,"MatchesStandID")
cat ("1 matchVar=",matchVar,"\n")
        # when matchVar is NULL, it means that there is a list of maps that will be searched
        # for the spatial data. If it is not null, then there is only one item, so use it.
        mapList = if (is.null(matchVar)) dbGlb$SpatialData else list(d=dbGlb$SpatialData)
        polys = NULL
        polyLbs = NULL
        for (map in mapList)
        {
          if (!length(uidsToGet)) break 
          matchVar = attr(map,"MatchesStandID")
cat ("2 matchVar=",matchVar,"\n")
          uids=intersect(uidsToGet, map@data[,matchVar])
          if (length(uids) == 0) next
          uidsFound = c(uidsFound,uids)
          pp = spTransform(map[match(uids,map@data[,matchVar]),],CRS("+init=epsg:4326"))
          if (class(pp)=="SpatialPolygonsDataFrame") 
          {
            polys  = if (is.null(polys))   pp   else rbind(polys,pp)
            polyLbs= if (is.null(polyLbs)) uids else rbind(polyLbs,uids)
          }
          if (class(pp)=="SpatialPointsDataFrame")   
          {
            pts   = if (is.null(pts))    pp   else rbind(pts,pp)
            ptsLbs= if (is.null(ptsLbs)) uids else rbind(ptsLbs,uids)
          }
          uidsToGet = setdiff(uidsToGet,uids)
        }
      }   
cat ("left to get: length(uidsToGet)=",length(uidsToGet),
     " number found: length(uidsFound)=",length(uidsFound),"\n")
      if (length(uidsToGet))
      {
        isolate({
          if (globals$fvsRun$uuid == input$mapDsRunList)
              inInit = globals$fvsRun$refreshDB else 
          {
            saveFvsRun=loadFVSRun(dbGlb$prjDB,input$mapDsRunList)
            if (!is.null(saveFvsRun))
            {
             inInit = saveFvsRun$refreshDB
             rm(saveFvsRun)
            } else inInit=NULL
          }
        })
        if (is.null(inInit)) inInit = getTableName(dbGlb$dbIcon,"FVS_StandInit")
cat ("mapDsRunList trying to use the table=",inInit,"\n")
        dbWriteTable(dbGlb$dbIcon,DBI::SQL("temp.uidsToGet"),data.frame(stds=uidsToGet),overwrite=TRUE)
        sid = if (inInit %in% c("FVS_PlotInit","FVS_PlotInit_Plot"))
               "StandPlot_ID" else "Stand_ID"
        qry = paste0("select distinct ",sid," as Stand_ID,Latitude,Longitude from ",inInit,
                     " where ",sid," in (select * from temp.uidsToGet)")    
        latLng = try(dbGetQuery(dbGlb$dbIcon,qry))
        dbExecute(dbGlb$dbIcon,"drop table if exists temp.uidsToGet")
        if (class(latLng)!="try-error" && nrow(latLng))
        {
          idxLng = grep("Longitude",names(latLng),ignore.case=TRUE)
          idxLat = grep("Latitude",names(latLng),ignore.case=TRUE)
cat ("mapDsRunList idxLng=",idxLng," idxLat=",idxLat," names=",names(dbGlb$SpatialData),"\n")
          if (length(idxLng) && length(idxLat))
          {
            latLng[,idxLng] = as.numeric(latLng[,idxLng])
            latLng[,idxLat] = as.numeric(latLng[,idxLat])
            latLng = na.omit(latLng)
          } else latLng = NULL
        } else latLng = NULL
        if (is.null(latLng) || nrow(latLng) == 0)
        {
cat ("mapDsRunList trying PlotInit\n")
          inInit = getTableName(dbGlb$dbIcon,"FVS_PlotInit")
          latLng = try(dbGetQuery(dbGlb$dbIcon, 
                     paste0("select Stand_ID,avg(Latitude) as Latitude, ",
                            "avg(Longitude) as Longitude from ",inInit,
                            " group by Stand_ID;")))
          if (class(latLng)!="try-error")
          {
            latLng$Longitude = as.numeric(latLng$Longitude)
            latLng$Latitude  = as.numeric(latLng$Latitude)
            latLng = na.omit(latLng)
            if (nrow(latLng) > 0) latLng = subset(latLng, Latitude != 0 & Longitude != 0)
          } else latLng = NULL
        }
        if (!is.null(latLng) && nrow(latLng)>0)
        {
cat ("mapDsRunList names(latLng)=",names(latLng)," class(latLng)=",class(latLng),"\n")
          idxLng = grep("Longitude",names(latLng),ignore.case=TRUE)
          idxLat = grep("Latitude",names(latLng),ignore.case=TRUE)
          idxID  = grep("Stand_ID",names(latLng),ignore.case=TRUE)
cat (" idxLng=",idxLng," idxLat=",idxLat," idxID=",idxID,"\n")
          latLng = latLng[,c(idxID,idxLng,idxLat)]
          names(latLng)=c("Stand_ID","Longitude","Latitude")
          keep = na.omit(match(uidsToGet,latLng[,"Stand_ID"]))
cat ("rows to keep=",length(keep),"\n")
          if (length(keep))
          {
            latLng[,"Longitude"] = ifelse(latLng[,"Longitude"]>0, 
                        -latLng[,"Longitude"], latLng[,"Longitude"])
            latLng = latLng[keep,,drop=FALSE]
            uniq = unique(latLng[,2:3])
            if (nrow(uniq) < nrow(latLng)) 
            {
              newlatLng = NULL
              for (row in 1:nrow(uniq))
              {
                sub=subset(latLng,latLng[,2]==uniq[row,1] & latLng[,3]==uniq[row,2])
                if (nrow(sub) > 1)
                {
                  sub = sub[order(sub[,1]),]
                  delta=nrow(sub)/2*5
                  sub[,3] = sub[,3]+seq(-delta,delta,5)[1:nrow(sub)]*.00005
                }
                newlatLng = rbind(newlatLng,sub)
              }
              latLng = newlatLng
            }
            uids = latLng[,"Stand_ID"]
            uidsFound = c(uidsFound,uids)
            coordinates(latLng) <- ~Longitude+Latitude
            setProj <- function (obj) 
            {
              proj4string(obj) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
              obj
            }
            latLng <- try(setProj(latLng))             
            if (class(latLng)=="try-error")
            {
               output$leafletMessage=renderText("Error setting projection in location data.")
               return()
            }
            pp = spTransform(latLng,CRS("+init=epsg:4326"))
            pts=    if (is.null(pts))    pp   else rbind(pts,pp)
            ptsLbs= if (is.null(ptsLbs)) uids else rbind(ptsLbs,uids)
          }
        }
      }
      if (!length(uidsFound))
      {
        output$leafletMessage=renderText("Couldn't find the stands in the spatial data")
        return()
      }
      progress <- shiny::Progress$new(session,min=1,max=length(uidsFound))
      labs = list()
      url = paste0(session$clientData$url_protocol,"//",
                   session$clientData$url_hostname,
                   session$clientData$url_pathname)
      for (sid in uidsFound)
      {
        tab = subset(dispData,StandID == sid)[,-1]
        labs[[length(labs)+1]] = 
          if (input$mapDsType == "table")
          {
            HTML(paste0('<p style="line-height:1">StandID=',sid,df2html(tab)))
          } else {
            pvar = input$mapDsVar[1]
            tab = subset(dispData,StandID == sid)[,intersect(names(dispData),c("Year","Species",pvar))] 
            pfile=paste0("www/s",sid,".png")
cat ("pfile=",pfile," nrow=",nrow(tab)," sid=",sid,"\n")
            CairoPNG(file=pfile,height=1.7,width=2.3,units="in",res=100,bg = "transparent")
            if (length(intersect(c("Species","Characteristic"),names(tab))) || 
                length(table(tab$Year)) == 1) tab$Year = as.factor(tab$Year)
            p = ggplot(tab,aes_string(x="Year",y=pvar)) + geom_point() + theme(
                legend.position="none",
                text=element_text(size=8),axis.text=element_text(face="bold"),
                panel.background=element_rect(fill=grDevices::rgb(1, 1, 1, .2, maxColorValue = 1)),
                plot.background =element_rect(fill=grDevices::rgb(1, 1, 1, .5, maxColorValue = 1)))
            if (!is.factor(tab$Year)) p = p+geom_line()
            print(p)
            dev.off()
            url = paste0(session$clientData$url_protocol,"//",
                         session$clientData$url_hostname,
                         session$clientData$url_pathname)
            pfile=if (isLocal()) paste0("/www/s",sid,".png") else
                                 paste0(url,"www/s",sid,".png")
            HTML(paste0('<img src="',pfile,'?',as.character(as.numeric(Sys.time())),
                        '" alt="',sid,'" style="width:229px;height:170px;">'))
          }
        progress$set(message = paste0("Preparing ",sid), value = length(labs))  
      }     
      progress$close()
      map = leaflet() %>% addTiles() %>%
        addTiles(urlTemplate = 
                   paste0("https://mts1.google.com/vt/lyrs=",input$mapDsProvider,
                          "&hl=en&src=app&x={x}&y={y}&z={z}&s=G"),attribution = 'Google')
      lops = labelOptions(opacity=.7)
      pops = popupOptions(maxWidth = 2000,autoClose=FALSE,closeButton=TRUE,closeOnClick=FALSE,textOnly=TRUE)
      if (length(pts))
      {
        lbidx = match(ptsLbs,uidsFound)
        map = map %>% addCircleMarkers(data=pts, radius = 6, color="#FFFF00", 
              stroke = FALSE, fillOpacity = 0.5, popup=labs[lbidx], 
              popupOptions = pops, label=labs[lbidx], labelOptions = lops)
      }
      if (length(polys))
      {
        lbidx = match(polyLbs,uidsFound)
        map = map %>% addPolygons(data=polys, color = "#FFFF00", 
              weight = 3, smoothFactor = 0.1, opacity = .3, fillOpacity = 0.2, 
              popup=labs[lbidx], popupOptions = pops, label=labs[lbidx], 
              labelOptions = lops,
              highlightOptions = c(weight = 5, color = "#666", dashArray = NULL,
                fillOpacity = 0.3, opacity = .6, bringToFront = TRUE))
      }
      output$leafletMap = renderLeaflet(map)
    }
  })
  
  ## Tools, related to Copy
  observe({    
    if (input$toolsPan == "Copy projects") 
    {
      backups = dir (pattern="ProjectBackup")
      if (length(backups)) 
      {
        backups = sort(backups,decreasing=TRUE)
        names(backups) = backups 
      } else backups=list()
      updateSelectInput(session=session, inputId="pickBackup", 
        choices = backups, selected=NULL)
    } 
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
        killIfRunning(globals$fvsRun$uuid)
        removeFVSRunFiles(globals$fvsRun$uuid,all=TRUE)
        deleteRelatedDBRows(globals$fvsRun$uuid,dbGlb$dbOcon)
        removeFVSRun(dbGlb$prjDB,input$runSel)
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
      for (uuid in globals$FVS_Runs) removeFVSRunFiles(uuid)
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
      globals$FVS_Runs = getFVSRuns(dbGlb$prjDB)
      for (uuid in globals$FVS_Runs) 
      {
        removeFVSRunFiles(uuid,all=TRUE)
        removeFVSRun(dbGlb$prjDB,uuid)
      }
      globals$FVS_Runs = getFVSRuns(dbGlb$prjDB)
      dbGlb$dbOcon <- dbConnect(dbDriver("SQLite"),"FVSOut.db")
      globals$saveOnExit = FALSE
      globals$reloadAppIsSet=1
      session$reload()
    })  
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
          choices = backups, selected=NULL)
      }
    }
  })    
  

 ## mkZipBackup 
  observe({
    if(input$mkZipBackup > 0)
    {
      flst=dir()
      del = grep("^ProjectBackup",flst)
      if (length(del)) flst = flst[-del]
      del = grep("^www",flst)
      if (length(del)) flst = flst[-del]
      del = grep("^projectIsLocked",flst) 
      if (length(del)) flst = flst[-del]
      delFVSbin = grep ("^FVSbin",flst)
      if (length(delFVSbin)) flst = flst[-delFVSbin]
      createdFVSbin=FALSE
      if (isolate(input$prjBckCnts)=="projFVS")
      {
        if (globals$fvsBin != "FVSbin")
        {
          if (!dir.exists("FVSbin")) mkdir("FVSbin")
          fvsPgms = list.files(fvsBin,pattern=paste0(.Platform$dynlib.ext,"$"),
                               full.names=TRUE)
          file.copy(fvsPgms,"FVSbin")
          createdFVSbin=TRUE
        }
        fvsPgms = list.files("FVSbin",pattern=paste0(.Platform$dynlib.ext,"$"))
        fvsPgms = paste0("FVSbin","/",fvsPgms)
        flst = c(flst,fvsPgms)
      } 
      zfile=paste0("ProjectBackup_",format(Sys.time(),"%Y-%d-%m_%H_%M_%S"),".zip")
      # close the input and output databases if they are openned
      ocon = class(dbGlb$dbOcon) == "SQLiteConnection" && dbIsValid(dbGlb$dbOcon)
      icon = class(dbGlb$dbIcon) == "SQLiteConnection" && dbIsValid(dbGlb$dbIcon)
      if (ocon) dbDisconnect(dbGlb$dbOcon)
      if (icon) dbDisconnect(dbGlb$dbIcon)
      progress <- shiny::Progress$new(session,min=1,max=length(flst))
      for (i in 1:length(flst))
      {
        x = flst[i]
        progress$set(message = paste0("Adding ",x," to ",zfile), value = i)
        rtn=if (file.exists(zfile)) try(zipr_append(zfile,x)) else try(zipr(zfile,x))
        if (class(rtn)=="try-error") 
        {
          progress$set(message = paste0("Failed to add ",x," to ",zfile), value = i+1)
          Sys.sleep(.2)
        }
      }
      if (createdFVSbin) unlink("FVSbin")
      if (ocon) dbGlb$dbOcon <- dbConnect(dbDriver("SQLite"),dbGlb$dbOcon@dbname)   
      if (icon) dbGlb$dbIcon <- dbConnect(dbDriver("SQLite"),dbGlb$dbIcon@dbname)   
      Sys.sleep(.2)
      progress$close()
      backups = dir (pattern="ProjectBackup")
      if (length(backups)) 
      {
        backups = sort(backups,decreasing=TRUE)
        names(backups) = backups 
      } else backups=list()
      updateSelectInput(session=session, inputId="pickBackup", 
        choices = backups, selected=NULL)
    }
  })    
  
 ## Upload Project Backup--upZipBackup
  observe({
    if (!isLocal()) return()
    if (is.null(input$upZipBackup)) return()
    prjBackupUpload = input$upZipBackup$name
cat ("prjBackupUpload=",prjBackupUpload,"\n")
    progress <- shiny::Progress$new(session,min=1,max=5)
    progress$set(message = "Begining project backup upload",value = 2)
    ind <- grep("ProjectBackup_",prjBackupUpload)
    fext <- tools::file_ext(basename(input$upZipBackup$name))
    if (!length(ind) && fext !="zip") 
    {
      output$delPrjActionMsg  = renderText("<b>Uploaded file is not a valid project backup zip file</b>")
      unlink(input$upZipBackup$datapath)
      progress$close()
      return()
    }
    fdir = dirname(input$upZipBackup$datapath)
    progress$set(message = "Copying project backup to current project directory",value = 4)
    file.copy(input$upZipBackup$datapath,prjBackupUpload)
    backups = dir(pattern="ProjectBackup")
    if (length(backups)) 
    {
      backups = sort(backups,decreasing=TRUE)
      names(backups) = backups 
      updateSelectInput(session=session, inputId="pickBackup", 
          choices = backups, selected=backups[length(backups)])
    } else updateSelectInput(session=session, inputId="pickBackup", 
            choices = list(), selected=NULL)
    output$delPrjActionMsg  = renderText("<b>Project backup added to above list of backups to process</b>")
    progress$close()
  })
  
  ## restorePrjBackup 
  observeEvent(input$restorePrjBackup,
  {
    if (is.na(input$pickBackup) || is.null(input$pickBackup) || !file.exists(input$pickBackup)) return()
    cnts = zip_list(input$pickBackup)
    if (length(cnts)==0) return()
    if(length(grep("FVSbin",cnts$filename)) || length(grep("^FVS[a-z]*.so$",cnts$filename)))
    {
      output$btnA <-renderUI(HTML("Project files and FVS software"))
      output$btnB <-renderUI(HTML("Project files only"))
      session$sendCustomMessage(type = "dialogContentUpdate",
            message = list(id = "restorePrjBackupDlg",
                      message = paste0("WARNING: restoring this project backup will overwrite",
                      " any existing project files in this current project. If you don't",
                      " want to lose exiting project files, consider restoring to a new empty",
                      " project instead. This backup also contains FVS software that will",
                      " overwrite your currently installed version with the software in the",
                      " backup, if selected.  What contents would you like to restore?")))
    } else {
      output$btnA <-renderUI(HTML("Yes"))
      output$btnB <-renderUI(HTML("Yes"))
      session$sendCustomMessage(type = "dialogContentUpdate",
            message = list(id = "restorePrjBackupDlg",
                      message = paste0("WARNING: restoring this project backup will overwrite", 
                      " any existing project files in this current project. If you don't", 
                      " want to lose exiting project files, consider restoring to a new",
                      " empty project instead. Are you sure?")))
    }
  })
  
  observeEvent(input$restorePrjBackupDlgBtnA,{  
    isolate({
        if (is.na(input$pickBackup) || is.null(input$pickBackup) || !file.exists(input$pickBackup)) return()
        progress <- shiny::Progress$new(session,min=1,max=5)
        progress$set(message = "Unzipping project backup",value = 2)
        fvsWorkBackup = input$pickBackup
cat ("restorePrjBackupDlgBtnA fvsWorkBackup=",fvsWorkBackup,"\n")    
        if (file.exists(fvsWorkBackup)) 
        {
          progress$set(message = "Checking backup contents",value = 3)
          ocon = class(dbGlb$dbOcon) == "SQLiteConnection" && dbIsValid(dbGlb$dbOcon)
          icon = class(dbGlb$dbIcon) == "SQLiteConnection" && dbIsValid(dbGlb$dbIcon)
          if (ocon) dbDisconnect(dbGlb$dbOcon)
          if (icon) dbDisconnect(dbGlb$dbIcon)
          curdir=getwd()
          td <- paste0(tempdir(),"/pbk")
          suppressWarnings(dir.create(td))
          setwd(td)
          lapply(dir(),function(x) unlink(x,recursive=TRUE,force=TRUE))
          rtn = try(unzip (paste0(curdir,"/",fvsWorkBackup),exdir=td,
                    overwrite=TRUE,junkpaths=FALSE))
          if (class(rtn)=="try-error") return()
          zipConts <- dir(td,include.dirs=TRUE,recursive=TRUE)
          del=NULL
          # TODO: most of this list is related to old versions the software (pre "package")
          # and can be reviewed (many dropped) in the future, say 2024 or so.
          for (todel in c("^www","^rFVS","R$",".html$",".zip$","treeforms.RData",
                           "prms.RData",".log$","FVS_Data.db.default","FVS_Data.db.empty", 
                           "databaseDescription.xlsx","projectIsLocked.txt",".png$", 
                           "SpatialData.RData.default" )) del = c(del,grep (todel,zipConts))
          if (length(del)) lapply(paste0(td,"/",zipConts[del]),unlink,recursive=TRUE)
          mkFVSProjectDB()
          zipConts <- dir(td,include.dirs=TRUE,recursive=TRUE)
          pgms=dir(td,pattern="^FVS[a-z]*.so$")
          if (length(pgms)) 
          {
            frompgms=paste0(td,"/",pgms)
            todir=paste0(td,"/FVSbin")
            dir.create(todir)
            topgms=paste0(todir,"/",pgms)
            file.rename(from=frompgms,to=topgms)
            dir.create("FVSbin")
          }
          setwd(curdir)
          curcnts=dir()
          tokeep = grep("^ProjectBackup",curcnts)
          tokeep = c(tokeep,grep("^projectId",curcnts))
          curcnts = curcnts[-tokeep]
          lapply(paste0(td,"/",curcnts),unlink,recursive=TRUE)          
          progress$set(message = "Copying backup contents",value = 4)
          zipConts <- dir(td,recursive=TRUE)    
          lapply(zipConts,function(x,td) file.copy(from=paste0(td,"/",x),to=x,overwrite=TRUE),td)
          unlink(td,recursive=TRUE)
        } 
        globals$reloadAppIsSet=1
        globals$saveOnExit=FALSE
        progress$close()
        session$reload()
      })
  })
                 
  observeEvent(input$restorePrjBackupDlgBtnB,{  
    isolate({
        if (is.na(input$pickBackup) || is.null(input$pickBackup) || !file.exists(input$pickBackup)) return()
        progress <- shiny::Progress$new(session,min=1,max=5)
        progress$set(message = "Unzipping project backup",value = 2)
        fvsWorkBackup = input$pickBackup
cat ("restorePrjBackupDlgBtB fvsWorkBackup=",fvsWorkBackup,"\n")    
        if (file.exists(fvsWorkBackup)) 
        {
          progress$set(message = "Checking backup contents",value = 3)
          ocon = class(dbGlb$dbOcon) == "SQLiteConnection" && dbIsValid(dbGlb$dbOcon)
          icon = class(dbGlb$dbIcon) == "SQLiteConnection" && dbIsValid(dbGlb$dbIcon)
          if (ocon) dbDisconnect(dbGlb$dbOcon)
          if (icon) dbDisconnect(dbGlb$dbIcon)
          td <- tempdir()
          rtn = try(unzip (paste0(getwd(),"/",fvsWorkBackup),exdir=td,
                    overwrite=TRUE,junkpaths=FALSE))
          if (class(rtn)=="try-error") return()
          zipConts <- dir(td,include.dirs=TRUE,recursive=TRUE)
          del=NULL
          for (todel in c("^www","^rFVS","R$",".html$",".zip$","treeforms.RData",
                          "^FVSbin","prms.RData",".log$")) del = c(del,grep (todel,zipConts))
          if (length(del)) lapply(paste0(td,"/",zipConts[del]),unlink,recursive=TRUE)
          pgms=dir(td,pattern="^FVS[a-z]*.so$")
          if (length(pgms)) lapply(paste0(td,"/",pgms),unlink,recursive=TRUE)
          progress$set(message = "Copying backup contents",value = 4)
          zipConts <- dir(td,recursive=TRUE)    
          lapply(zipConts,function(x,td) file.copy(from=paste0(td,"/",x),to=x,overwrite=TRUE),td)
          unlink(td,recursive=TRUE)
        } 
        if (ocon) dbGlb$dbOcon <- dbConnect(dbDriver("SQLite"),dbGlb$dbOcon@dbname)   
        if (icon) dbGlb$dbIcon <- dbConnect(dbDriver("SQLite"),dbGlb$dbIcon@dbname)
        globals$reloadAppIsSet=1
        globals$saveOnExit=FALSE
        progress$close()
        session$reload()
      })
  })
  
  observeEvent(input$restorePrjBackupDlgBtnC,
     updateSelectInput(session=session, inputId="pickBackup", selected=NULL)
  )


  ## PrjDelete 
  observe({
    if(input$PrjDelete > 0)
    {            
      isolate({
        if (is.null(input$PrjDelSelect)) 
        {
          output$delPrjActionMsg <- renderUI(HTML("No project selected."))
          session$sendCustomMessage(type = "dialogContentUpdate",
            message = list(id = "PrjDeleteDlg", message=
              paste0('Select a project to delete, press Yes or No to continue.')))
        } else {
cat ("PrjDelete, input$PrjDelSelect=",input$PrjDelSelect,"\n")
          prjList=getProjectList()
          nm = names(prjList)[charmatch(input$PrjDelSelect,prjList)]
          output$delPrjActionMsg <- NULL
          msg = if(length(grep("ProjectBackup_",dir("../",input$PrjDelSelect)))) 
            " contains project backups within it that you may want to download first. "  else ""
          session$sendCustomMessage(type = "dialogContentUpdate",
            message = list(id = "PrjDeleteDlg", message = 
            paste0(nm,msg,"Are you sure you still want to delete this project?")))
        }
      })
    }                                        
  })
 observe({
    if (input$PrjDeleteDlgBtn > 0) 
    {
cat("delete project button.") 
      isolate({
        if (is.null(input$PrjDelSelect)) 
        {
          output$delPrjActionMsg <- renderUI(HTML("No project selected."))
        } else {
          delPrj=paste0("../",input$PrjDelSelect)  
          if (file.exists(paste0(delPrj,"/projectIsLocked.txt")))             
          {
            output$delPrjActionMsg <- renderUI(HTML("Cannot delete a locked project."))
          } else {
            if (nchar(delPrj)<4 || !dir.exists(delPrj))    
            {
              output$delPrjActionMsg <- renderUI(HTML("<b>Project directory not found.</b>"))
            } else {
              unlink(delPrj, recursive=TRUE)        
              output$delPrjActionMsg <- renderUI(HTML("<b>Project deleted</b>"))
              updateProjectSelections()
            }
          }
        } 
      })
    }
  })

  ##topHelp
  observe({
    if (input$topPan == "Help")
    {
      progress <- shiny::Progress$new(session,min=1,max=12)
      progress$set(message = "Loading Help File", value = 2)
      # build the help file if it doesn't exist or if it is older than 
      # fvsOnlineHelp.html or databaseDescription.xlsx
      help=NULL
      fr = "fvsOnlineHelpRender.RData"
      fn =  system.file("extdata", "fvsOnlineHelp.html", package = "fvsOL")
      xlsxfile=system.file("extdata", "databaseDescription.xlsx", package = "fvsOL")
      info=file.info(c(fr,fn,xlsxfile))
      if (which.max(info[,4]) != 1) 
      {
        unlink(fr)
        help = readChar(fn, info[2,1]) 
        progress$set(message = "Compiling the help file for this project", 
                     detail = "Loading Output Table Descriptions",value = 5)
        tabs = try(read.xlsx(xlsxFile=xlsxfile,sheet="OutputTableDescriptions"))
        if (class(tabs)!="try-error")
        {
          tablist=xlsx2html(tab="OutputTableDescriptions",xlsxfile=xlsxfile,addLink=TRUE)
          morehtml=paste0(tablist,'<p><a href="#contents">Back to Contents</a></p>')
          for (tab in tabs$Table) morehtml=paste0(morehtml,'<a name="',tab,'"></a>',
            xlsx2html(tab=tab,xlsxfile=xlsxfile),
          '<p><a href="#outputTables">Back to Output Table Descriptions</a>&nbsp;&nbsp;',
          '<a href="#contents">Back to Contents</a></p>')  
          if (!is.null(morehtml)) help = sub(x=help,fixed=TRUE,
                  pattern="**OUTPUTHTML**",replacement=morehtml)
        }
        progress$set(message = "Compiling the help file for this project", 
                     detail = "Loading Input Table Descriptions",value = 8)
        tabs = try(read.xlsx(xlsxFile=xlsxfile,sheet="InputTableDescriptions"))
        if (class(tabs)!="try-error")                                                         
        {
          morehtml=paste0(xlsx2html(tab="InputTableDescriptions",xlsxfile=xlsxfile,addLink=TRUE),
                                  '<p><a href="#contents">Back to Contents</a></p>')
          for (tab in tabs$Table) morehtml=paste0(morehtml,'<a name="',tab,'"></a>',
            xlsx2html(tab=tab,xlsxfile=xlsxfile), 
            '<p><a href="#inputTables">Back to Input Table Descriptions</a>&nbsp;&nbsp;',
            '<a href="#contents">Back to Contents</a></p>')            
          if (!is.null(morehtml)) help = sub(x=help,fixed=TRUE,
                  pattern="**INPUTHTML**",replacement=morehtml)
        }
        save(help,file=fr)
      } else  if (is.null(help) && file.exists(fr)) load(fr) 
      if (is.null(help)) help="<h4>Help is not available</h4>"
      output$uiHelpText <- renderUI(HTML(help))
      progress$close()
    }
  })

  df2html <- function(sdat=NULL)
  {
    if (is.null(sdat) || nrow(sdat)==0 || ncol(sdat)==0) return (NULL)
    sdat[sdat == " "]=NA
    html = paste0('<table border="1" style="text-align:right"><tr><th style="text-align:center">', 
           paste0(colnames(sdat),collapse='</th><th  style="text-align:center">'),"</th></tr>")
    for (i in 1:nrow(sdat))                   
    {
      tbrow=unlist(lapply(sdat[i,],function (x) if (is.character(x)) x else format(x,digits=3)))
      html = paste0(html,"<tr><td>",paste0(tbrow,collapse="</td><td>"),"</td></tr>")
    }
    paste0(html,"</table>")
  }
  
  
  xlsx2html <- function(tab=NULL,xlsxfile=NULL,cols=NULL,addLink=FALSE)
  {
    if (is.null(xlsxfile) || !file.exists(xlsxfile)) return(NULL)
    cleanlines=function(line) 
    {
      line=gsub(pattern="\n",replacement="",x=line,fixed=TRUE)
      gsub(pattern="\r",replacement="",x=line,fixed=TRUE)
    }
    if (!file.exists(xlsxfile) || is.null(tab)) return(NULL)
    if (tab %in% getSheetNames(xlsxfile))
    {
      sdat = try(read.xlsx(xlsxFile=xlsxfile,sheet=tab))
      if (class(sdat) == "try-error") return (NULL)
      if (nrow(sdat)==0 || ncol(sdat)==0) return (NULL)
      if (!is.null(cols) && max(cols)<=ncol(sdat)) sdat = sdat[,cols]
      sdat[sdat == " "]=NA
      if (nrow(sdat)==0 || ncol(sdat)==0) return (NULL)
      sdat = sdat[,!apply(sdat,2,function(x) all(is.na(x)))]
      if (nrow(sdat)==0 || ncol(sdat)==0) return (NULL)
      sdat = sdat[ !apply(sdat,1,function(x) all(is.na(x))),]
      if (nrow(sdat)==0 || ncol(sdat)==0) return (NULL)
      html = paste0("<b>",tab,"</b>")
      html = paste0(html,'<p><TABLE border="1"><TR><TH>', 
             paste0(cleanlines(colnames(sdat)),collapse="</TH><TH>"),"</TH></TR>")
      for (i in 1:nrow(sdat))                   
      {
        tbrow=cleanlines(as.character(sdat[i,]))
        if (addLink) tbrow[1] = paste0('<a href="#',tbrow[1],'">',tbrow[1],'</a>')
        html = paste0(html,"<TR><TD>",paste0(tbrow,collapse="</TD><TD>"),"</TD></TR>")
      }
      html = paste0(html,"</TABLE><br>")
      return (html)
    } else return (NULL)
  } 
  
  mkTableDescription <- function (tab,xlsxfile)
  {
    html = NULL
    if (!is.null(tab) && nchar(tab)>0 && !is.null(xlsxfile) && file.exists(xlsxfile))
    {
      sheets = sort(getSheetNames(xlsxfile), decreasing=FALSE)
      if ("OutputTableDescriptions" %in% sheets)
      {
        tabs = read.xlsx(xlsxFile=xlsxfile,sheet="OutputTableDescriptions")
        row = charmatch(toupper(tab),toupper(tabs[,1]))
        html = paste0("<b>",tab,"</b> ",tabs[row,2])
        mhtml = xlsx2html(tab,xlsxfile=xlsxfile,cols=c(1,4))
        if (!is.null(mhtml)) html = paste0(html,mhtml)
      }
      if ("GuideLinks" %in% sheets)
      { 
        tabs = read.xlsx(xlsxFile=xlsxfile,sheet="GuideLinks")
        row = charmatch(toupper(tab),toupper(tabs[,1]))
        if(!is.null(html))html = paste0(html,tabs[row,2])
      }
    }
    HTML(html)
  }
  ##tabDescSel
  observe({
    tab = input$tabDescSel
cat ("tabDescSel, tab=",tab,"\n")
    output$tabDesc <- renderUI(mkTableDescription(tab))
  })
  ##tabDescSel2
  observe({
    tab = input$tabDescSel2
cat ("tabDescSel2, tab=",tab,"\n")
    output$tabDesc2 <- renderUI(mkTableDescription(tab))
  })
  
  ##### data upload code  
  observe({
    if(input$topPan == "Import Input Data")
    {
      updateTabsetPanel(session=session, inputId="inputDBPan", 
        selected="Upload inventory data")
      output$step1ActionMsg <- NULL
      output$step2ActionMsg <- NULL
    }
  })
  observe({
    if(input$inputDBPan == "Upload inventory data") 
    {
cat ("Upload inventory data\n")
      output$step1ActionMsg <- NULL
      output$step2ActionMsg <- NULL
    }
  })
  
  initNewInputDB <- function (session,output,dbGlb)
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
    resetActiveFVS(globals)
  }
  
  installDefaultData <- function(empty=FALSE)
  {
    dbDisconnect(dbGlb$dbIcon)
    if (empty)
    {
      frm=system.file("extdata", "FVS_Data.db.empty", package = "fvsOL")
      file.copy(frm,"FVS_Data.db",overwrite=TRUE)
      unlink("SpatialData.RData")
    } else {
      frm=system.file("extdata", "FVS_Data.db.default", package = "fvsOL")
      file.copy(frm,"FVS_Data.db",overwrite=TRUE)
      frm=system.file("extdata", "SpatialData.RData.default", package = "fvsOL")
      file.copy(frm,"SpatialData.RData",overwrite=TRUE)
    }
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db") 
    initNewInputDB(session,output,dbGlb)
    loadStandTableData(globals, dbGlb$dbIcon)
    updateStandTableSelection(session,input,globals)
    loadVarData(globals,input,dbGlb$dbIcon)                                              
    updateVarSelection(globals,session,input)
  }
  ## installTrainDB
  observe({  
    if (input$installTrainDB == 0) return()
    installDefaultData()
    output$step1ActionMsg <- NULL
    output$step2ActionMsg <- output$mapActionMsg <- renderText(HTML(paste0("<b>Training database installed",
         " (the inventory data and the related spatial data).</b>")))
  })
  ## installTrainDB2
  observe({  
    if (input$installTrainDB2 == 0) return()
    installDefaultData()
    output$mapActionMsg <- renderText(HTML(paste0("<b>Training database installed",
         " (the inventory data and the related spatial data).</b>")))
  }) 
  ## installEmptyDB
  observe({  
    if (input$installEmptyDB == 0) return()
    installData(empty=TRUE)
    output$step1ActionMsg <- NULL
    output$step2ActionMsg <- renderText(HTML("<b>Empty database installed and spatial data deleted.</b>"))
    dbGlb$dbIcon <- dbConnect(dbDrv,"FVS_Data.db")
  }) 
  ## Upload new database
  observe({
    if (is.null(input$uploadNewDB)) return()
    output$step1ActionMsg <- NULL
    output$step2ActionMsg <- NULL
    fext = tools::file_ext(basename(input$uploadNewDB$name))
cat ("fext=",fext,"\n")
    session$sendCustomMessage(type="jsCode",
                          list(code= "$('#input$installNewDB').prop('disabled',true)"))
    session$sendCustomMessage(type="jsCode",
                          list(code= "$('#input$addNewDB').prop('disabled',true)"))
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#installTrainDB').prop('disabled',true)"))
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#installEmptyDB').prop('disabled',true)"))
    if (! (fext %in% c("accdb","mdb","db","sqlite","xlsx","zip"))) 
    {
      output$step1ActionMsg  = renderText("Uploaded file is not suitable database types described in Step 1.")
      unlink(input$uploadNewDB$datapath)
      return()
    } else {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#installNewDB').prop('disabled',true)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#addNewDB').prop('disabled',true)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#installTrainDB').prop('disabled',true)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#installEmptyDB').prop('disabled',true)"))
    }
    fdir = dirname(input$uploadNewDB$datapath)
    progress <- shiny::Progress$new(session,min=1,max=20)
    if (fext == "zip") 
    {
      progress$set(message = "Unzip data", value = 1)
      unzip(input$uploadNewDB$datapath, junkpaths = TRUE, exdir = fdir)
      unlink(input$uploadNewDB$datapath)
      fname = dir(dirname(input$uploadNewDB$datapath))
      if (length(fname)>1) 
      {
        output$step1ActionMsg = renderText(".zip contains more than one file.")
        lapply (dir(dirname(input$uploadNewDB$datapath),full.names=TRUE),unlink)
        progress$close()
        return()
      } else if (length(fname) == 0) {
        output$actionMsg = renderText(".zip was empty.")
        progress$close()
        return()
      } 
      fext = tools::file_ext(fname)
      if (! (fext %in% c("accdb","mdb","db","sqlite","xlsx"))) 
      {
        output$step1ActionMsg = renderText(".zip did not contain one of the suitable file types described in Step 1.")
        lapply (dir(dirname(input$uploadNewDB$datapath),full.names=TRUE),unlink)
        progress$close()
        return()
      }
    } else fname = basename(input$uploadNewDB$datapath)
cat ("fext=",fext," fname=",fname," fdir=",fdir,"\n")
    curDir=getwd()
    setwd(fdir)
    if (fext %in% c("accdb","mdb"))
    {
      progress$set(message = "Process schema", value = 2)
cat("curDir=",curDir," input dir=",getwd(),"\n") 
      cmd = if (.Platform$OS.type == "windows") 
        shQuote(paste0("C:/Users/Public/Documents/mdbtools/mdb-schema ",fname)) else
        paste0(paste0("mdb-schema ",fname))
cat ("cmd=",cmd,"\n")
      schema = if (.Platform$OS.type == "windows") try(shell(cmd,intern=TRUE)) else 
                                                   try(system(cmd,intern=TRUE))
      if (class(schema)=="try-error" || !exists("schema") || length(schema) < 2 || schema[1] =="Unknown Jet version.") 
      {
        setwd(curDir) 
        progress$close()  
        if (schema[1] =="Unknown Jet version.") output$step1ActionMsg = renderText("Unknown Jet version. Possible corrupt database.") else
        output$step1ActionMsg = renderText("Error when attempting to extract data from Access database.")
        session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
        return()
      }
      tbls = grep ("CREATE TABLE",schema,ignore.case=TRUE)
      schema = schema[tbls[1]:length(schema)]
      schema = gsub("\t"," ",schema,fixed=TRUE)   
      schema = gsub("[","]",schema,fixed=TRUE) 
      tbls = grep ("CREATE TABLE",schema,ignore.case=TRUE)
      tbln=unlist(lapply(schema[tbls],function(x) if (length(grep("]",x,fixed=TRUE)))
        scan(text=x,what="character",sep="]",quiet=TRUE)[2] else
        scan(text=x,what="character",quiet=TRUE)[3]))
      schema = gsub(" Long Integer"," Integer",schema,ignore.case=TRUE)
      schema = gsub(" Int"," Integer",schema,ignore.case=TRUE)
      schema = gsub(" Integereger"," Integer",schema,ignore.case=TRUE)
      schema = gsub(" Memo.*)"," Text",schema,ignore.case=TRUE)
      schema = gsub(" Memo"," Text",schema,ignore.case=TRUE)
      schema = gsub(" Text.*)"," Text",schema,ignore.case=TRUE)
      schema = gsub(" Double"," Real",schema,ignore.case=TRUE)
      schema = gsub(" SHORT_DATE_TIME,"," Text,",schema,ignore.case=TRUE)
      schema = gsub(" FLOAT,"," Real,",schema,ignore.case=TRUE)
      schema = gsub(" NOT NULL"," ",schema,,ignore.case=TRUE)
      schema = gsub(" Single"," Real",schema)                    
      schema = gsub("]",'"',schema,fixed=TRUE) 
      cat ("begin;\n",file="sqlite3.import")
      cat (paste0(schema,"\n"),file="sqlite3.import",append=TRUE)
      cat ("commit;\n",file="sqlite3.import",append=TRUE)
      progress$set(message = "Extract data", value = 3)  
      if(!length(grep("FVS_StandInit",tbln,ignore.case=TRUE))){
        setwd(curDir) 
        progress$close()     
        output$step1ActionMsg = renderText("FVS_StandInit table is missing from your input data.")
        session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
        return()
      }
      findpgm <- function (pgm="mdb-export")
      {
        path = Sys.getenv(x = "PATH")
        paths = unlist(strsplit(path,fixed=TRUE,split=if (.Platform$OS.type == "windows") ";" else ":"))
        if (.Platform$OS.type == "windows") paths = c(if (pgm=="mdb-export") 
            "C:/Users/Public/Documents/mdbtools" else "C:/Users/Public/Documents/SQLite",paths)
        paths = paths[!duplicated(paths)]
        for (d in paths) 
        {
          pg = if (dir.exists(d)) dir(d,pattern=pgm) else NULL
          if (length(pg)) return(paste0(d,"/",pg[which.min(unlist(lapply(pg,nchar)))])) 
        }
        return (pgm)
      }
          
      for (tab in tbln) 
      {

        progress$set(message = paste0("Export table ",tab), value = 3)
        cat ("begin;\n",file="sqlite3.import",append=TRUE)
        cmd = paste0 (findpgm()," -I sqlite ",fname," ",tab," >> sqlite3.import")
        cat ("cmd=",cmd,"\n")
        result = if (.Platform$OS.type == "windows") shell(cmd,intern=TRUE) else system(cmd,intern=TRUE)
        cat ("commit;\n",file="sqlite3.import",append=TRUE)
      }
      cat (".quit\n",file="sqlite3.import",append=TRUE)
      progress$set(message = "Import data to Sqlite3", value = 4) 
      cmd = paste0(findpgm("sqlite3")," FVS_Data.db < sqlite3.import")
cat ("cmd=",cmd,"\n")
      if (.Platform$OS.type == "windows") shell(cmd) else system(cmd)
cat ("cmd done.\n")
      dbo = dbConnect(dbDrv,"FVS_Data.db")
    } else if (fext == "xlsx") 
    {
      progress$set(message = "Get data sheets", value = 3)
      sheets = getSheetNames(fname)
      sheetsU <- toupper(sheets)
      if(!length(grep("FVS_STANDINIT",sheetsU)))
      {
        setwd(curDir) 
        progress$close()     
        output$step1ActionMsg = renderText("FVS_StandInit table is missing from your input data.")
        session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
        return()
      }
      normNames = c("FVS_GroupAddFilesAndKeywords","FVS_PlotInit",                
                    "FVS_StandInit","FVS_TreeInit")
      dbo = dbConnect(dbDrv,"FVS_Data.db")
      dbdis=system.file("extdata","databaseDescription.xlsx", package = "fvsOL")
      standNT = try(read.xlsx(xlsxFile=dbdis,sheet="FVS_StandInit"))
      standNT = if (class(standNT) == "try-error") NULL else apply(standNT[,c(1,3)],2,toupper)
      treeNT = try(read.xlsx(xlsxFile=dbdis,sheet="FVS_TreeInit"))
      treeNT = if (class(treeNT) == "try-error") NULL else apply(treeNT[,c(1,3)],2,toupper)
      plotNT = try(read.xlsx(xlsxFile=dbdis,sheet="FVS_PlotInit"))
      plotNT = if (class(plotNT) == "try-error") NULL else apply(plotNT[,c(1,3)],2,toupper)
      i = 3
      for (sheet in sheets)
      {
        i = i+1
cat ("sheet = ",sheet," i=",i,"\n")
        progress$set(message = paste0("Processing sheet ",i," name=",sheet), value=i)
        sdat = read.xlsx(xlsxFile=fname,sheet=sheet)
        sdat[[3]] <- gsub("_x000D_", "", sdat[[3]])
        im = grep(sheet,normNames,ignore.case=TRUE)
        if (length(im)) sheet = normNames[im]
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
      tabs = toupper(myListTables(dbo))
      if(!length(grep("FVS_STANDINIT",tabs)))
      {
        setwd(curDir) 
        progress$close()     
        output$step1ActionMsg = renderText("FVS_StandInit table is missing from your input data.")
        session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
        return()
      }
    }
    tabs = myListTables(dbo)
    fiaData = "FVS_STANDINIT_COND" %in% toupper(tabs) && 
              "FVS_STANDINIT_PLOT" %in% toupper(tabs)
    if (fiaData) 
    {
      fiaMsg=NULL
      progress$set(message = "FIA data detected, most checks skipped", value = 1) 
      # insure that the DSNIn keywords address FVS_Data.db in the FVS_GroupAddFilesAndKeywords table
      grpAdd = try(dbGetQuery(dbo,'select * from "fvs_groupaddfilesandkeywords"'))
      if (class(grpAdd) != "try.error") 
      {
        kwi = match("FVSKEYWORDS",toupper(names(grpAdd)))[1]
        if (!is.na(kwi)) 
        {
          ch = gsub("\nDSNIn\n.{1,}\nStandSQL","\nDSNIn\nFVS_Data.db\nStandSQL",grpAdd[,kwi])
          if (any(ch!=grpAdd[,kwi])) 
          {
            grpAdd[,kwi] = ch
            fiaMsg = "FVS_GroupAddFilesAndKeywords FVSKeywords field was modified"
            dbWriteTable(dbo,"FVS_GroupAddFilesAndKeywords",grpAdd,overwrite=TRUE)
          }
        }
      }
    } else {
      # get rid of "NRIS_" part of names if any
      for (tab in tabs)
      {
cat("loaded table=",tab,"\n")      
        nn = sub("NRIS_","",tab)
        if (nchar(nn) && nn != tab) dbExecute(dbo,paste0("alter table ",tab," rename to ",nn))
      }
      tabs = myListTables(dbo)
      ltabs = tolower(tabs)
      fixTabs=c(grep ("standinit",ltabs,fixed=TRUE),grep ("plotinit",ltabs))
      # if there is a FVS_GroupAddFilesAndKeywords table, grab the unique group codes
      grpmsg=NULL
      progress$set(message = "Checking FVS_GroupAddFilesAndKeywords", value = 4)
      if ("fvs_groupaddfilesandkeywords" %in% ltabs)
      {
        addgrps=try(dbGetQuery(dbo,'select distinct groups from "fvs_groupaddfilesandkeywords"'))
        if (class(addgrps)!="try-error")
        {
          addgrps=unique(unlist(lapply(addgrps[,1],function (x) scan(text=x,what="character",quiet=TRUE))))
          for (idx in fixTabs)
          {
            tab2fix=tabs[idx]
            grps=try(dbGetQuery(dbo,paste0("select distinct groups from '",tab2fix,"'")))
            if (class(grps)=="try-error") next
            grps=unique(unlist(lapply(grps[,1],function (x) scan(text=x,what="character",quiet=TRUE))))
            if (any(is.na(match(addgrps,grps))) && !length(match(grps,addgrps)))  
            {
              Tb=try(dbReadTable(dbo,tab2fix))
              if (class(Tb)=="try-error") next
              idx=match("groups",tolower(names(Tb)))
              if (!is.na(idx) && nrow(Tb)) 
              {
                idf = if (length(grep("plot",tab2fix,ignore.case=TRUE))) grep("Plots",addgrps) else grep("Stands",addgrps)
                ridxs <- grep(grps[grep("NA",as.list(match(addgrps,grps)))],Tb[,idx])
                Tb[ridxs,idx]=paste0(addgrps[idf]," ",Tb[ridxs,idx])
                if (class(try(dbWriteTable(dbo,tab2fix,Tb,overwrite=TRUE)))!="try-error") 
                    grpmsg=c(grpmsg,tab2fix)
              }
            }
          }
        }
      }
cat ("checking duplicate stand or standplot ids\n")
      progress$set(message = "Checking for duplicate StandID values", value = 5)
      # loop over tables and omit duplicate stand or standplot id's from being uploaded
      sidmsg=NULL
      newID=NULL  
      for (idx in fixTabs)
      {
cat ("checking tabs[idx]=",tabs[idx],"\n")
        if (tolower(tabs[idx]) %in% c("fvs_standinit_plot","fvs_standinit_cond",
           "fvs_treeinit_plot","fvs_treeinit_cond","fvs_plotinit_plot")) next
        tab2fix=tabs[idx]
        idf = if (length(grep("plot",tab2fix,ignore.case=TRUE))) "standplot_id" else "stand_id"
        qry = paste0("select ",idf," from '",tab2fix,"'") 
cat ("qry=",qry,"\n") 
        sidTb=try(dbGetQuery(dbo,qry))
        if (class(sidTb)=="try-error") next
        dups = duplicated(sidTb[,1])
        if (all(!dups)) next
        keep <- list()
        cntr <- 1
        for (i in 1:length(dups)){
          if (dups[i]==FALSE){
            keep[cntr] <- i
            cntr <- cntr +1
          }
        }
        sidTb=try(dbReadTable(dbo,tab2fix))
        if (class(sidTb)=="try-error") next
        sidTb=sidTb[as.numeric(keep),]
        dbWriteTable(dbo,tab2fix,sidTb,overwrite=TRUE)
        sidmsg=c(sidmsg,tab2fix)
      }
      # remove any leading or trailing spaces in stand id's which blow up the SQL queries at run time
      fixTabs=c(grep ("standinit",ltabs,fixed=TRUE),grep ("plotinit",ltabs),grep ("treeinit",ltabs))
      for (idx in fixTabs)
      {
cat ("checking tabs[idx]=",tabs[idx],"\n")
        if (tolower(tabs[idx]) %in% c("fvs_standinit_plot","fvs_standinit_cond",
           "fvs_treeinit_plot","fvs_treeinit_cond","fvs_plotinit_plot")) next
        tab2fix=tabs[idx]
        idf = if (length(grep("plot",tab2fix,ignore.case=TRUE))) "standplot_id" else "stand_id"
        qry = paste0("select ",idf," from '",tab2fix,"'")
cat ("qry=",qry,"\n") 
        sidTb=try(dbGetQuery(dbo,qry))
        if (class(sidTb)=="try-error") next
        if(length(sidTb[[1]])==0) next
        sidTb <- data.frame(trim(sidTb[[1]]))
        names(sidTb) <- toupper(idf)
        sidTbAll=try(dbReadTable(dbo,tab2fix))
        if (idf == "standplot_id") oldSID <- grep("StandPlot_ID",names(sidTbAll),ignore.case=TRUE) else
          oldSID <- grep("Stand_ID",names(sidTbAll),ignore.case=TRUE)
        sidTbAll <- sidTbAll[,-oldSID]
        sidTbAll <- append(sidTbAll,sidTb, after=0)
        if (class(sidTbAll)=="try-error") next
        dbWriteTable(dbo,tab2fix,data.frame(sidTbAll),overwrite=TRUE)
      }
cat ("sidmsg=",sidmsg,"\n")
    }
    progress$set(message = "Getting row counts", value = 6)
    rowCnts = unlist(lapply(tabs,function (x) dbGetQuery(dbo,
      paste0("select count(*) as '",x,"' from '",x,"';"))))
    msg = lapply(names(rowCnts),function(x) paste0(x," (",rowCnts[x]," rows)"))
    msg = paste0("<b>Uploaded data:</b><br>",paste0(msg,collapse="<br>"))
    if (!fiaData)
    {
      if (!is.null(grpmsg)) msg=paste0(msg,"<br>Groups values were modified in table(s): ",
          paste0(grpmsg,collapse=", "))
      if (!is.null(sidmsg)) msg=paste0(msg,"<br>Duplicate Stand_ID or StandPlot_ID values were found in table(s): ",
          paste0(sidmsg,collapse=", "),". <br>All duplicate values after the first value were not kept.")
cat ("calling fixFVSKeywords\n")
      progress$set(message = "Checking FVSKeywords", value = 7)      
      tt = try(fixFVSKeywords(dbo))
      canuse=class(tt) == "NULL"
      if (class(tt)=="character") msg = paste0(msg,
        "<br>Checking keywords: ",tt)
      progress$set(message = "Checking for minimum column definitions", value = 8)      
      tt = try(checkMinColumnDefs(dbo,progress,9))
      canuse=canuse && class(tt) == "NULL"
      if (class(tt)=="character") msg = paste0(msg,"<br>Checking columns: ",tt)
cat ("msg=",msg,"\n")
      if (!canuse) msg = paste0(msg,
        "<h4>Data checks indicate there are unresolved problems in the input.</h4>")
cat ("msg=",msg,"\n")
    } else msg = paste0(msg,if (!is.null(fiaMsg)) paste0("<br>",fiaMsg) else "",
        "<h4>Data checks are skipped when FIA data is detected.</h4>")
    output$step1ActionMsg = renderUI(HTML(msg))
    dbGlb$newFVSData = tempfile()
    dbDisconnect(dbo)
    file.copy(from="FVS_Data.db",to=dbGlb$newFVSData,overwrite=TRUE)
    session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#installNewDB').prop('disabled',false)"))
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#addNewDB').prop('disabled',false)"))
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#installTrainDB').prop('disabled',false)"))
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#installEmptyDB').prop('disabled',false)"))
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
    tabs = myListTables(dbGlb$dbIcon)
    progress <- shiny::Progress$new(session,min=1,max=length(tabs)+2)
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
      } else if (tolower(tb) == "fvs_standinit_cond" || tolower(tb) == "fvs_treeinit_cond")
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
      } else if (tolower(tb) == "fvs_standinit_plot")
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
      } else if (tolower(tb) == "fvs_plotinit_plot")
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
    progress$set(message = "Load variant data", value = i+1)
    resetActiveFVS(globals)
    loadVarData(globals,input,dbGlb$dbIcon)
    output$step2ActionMsg = renderText(HTML(paste0("<br>Uploaded data installed.<br>",
      "<b>WARNING:</b> If existing runs in this project were created using input ",
      "data that are not present in the database just installed, ",
      "you will need to re-load those data to run them again.<br>",
      "Note that the output from the previous runs will remain in the output database.")))
    initNewInputDB(session,output,dbGlb)
    progress$close()
  }) 
  ## addNewDB
  observe({  
    if (input$addNewDB == 0) return()  
    output$step2ActionMsg <- NULL
    if (is.null(dbGlb$newFVSData)) {output$step1ActionMsg<-NULL;return()}
    # set an exclusive lock on the database
    dbExecute(dbGlb$dbIcon,"PRAGMA locking_mode = EXCLUSIVE")
    trycnt=0
    while (TRUE)
    {
      trycnt=trycnt+1
      if (trycnt > 1000) 
      {
        dbExecute(dbGlb$dbIcon,"PRAGMA locking_mode = NORMAL")
        myListTables(dbGlb$dbIcon) # this forces the new locking mode to take effect
        output$step2ActionMsg <- renderText("Error: Exclusive lock was not obtained.")
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
    oldtabs = myListTables(dbGlb$dbIcon)
    dbo = dbConnect(dbDrv,dbGlb$newFVSData)
    newtabs = myListTables(dbo)
    progress <- shiny::Progress$new(session,min=1,max=length(newtabs)*2+1)
    i = 0
    dbDisconnect(dbo)
    attach = try(dbExecute(dbGlb$dbIcon,paste0("attach `",dbGlb$newFVSData,"` as addnew;")))
    if (class(attach) == "try-error")
    {
      output$step2ActionMsg <- renderText("New data could not be added")
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
      if ("STAND_ID" %in% toupper(dbListFields(dbGlb$dbIcon,tab)) &&
          "STAND_ID" %in% toupper(dbListFields(dbGlb$dbIcon,paste0("addnew.",tab))))
        dbExecute(dbGlb$dbIcon,paste0("delete from ",tab," where Stand_ID in ",
                    "(select Stand_ID from addnew.",tab,")"))
      if (tolower(tab) == "fvs_groupaddfilesandkeywords") 
        dbExecute(dbGlb$dbIcon,paste0("delete from ",tab," where Groups in ",
                    " (select Groups from addnew.",tab,")"))
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
          rtn = try(dbExecute(dbGlb$dbIcon,qry))
        }
      }
      alln = paste0(newTdef$name,collapse=",")
      qry = paste0("insert into ",tab," (",alln,") select ",alln,
                   " from addnew.",tab,";") 
cat ("homogenize qry=",qry,"\n")
      rtn = try(dbExecute(dbGlb$dbIcon,qry))
    }
    dbCommit(dbGlb$dbIcon)
    dbExecute(dbGlb$dbIcon,paste0("detach addnew;"))
    unlink(dbGlb$newFVSData)
    dbGlb$newFVSData=NULL
    tabs = dbListTabels(dbGlb$dbIcon)
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
    output$step2ActionMsg <- renderText(msg)
    loadVarData(globals,input,dbGlb$dbIcon) 
    initNewInputDB(session,output,dbGlb)
    progress$close()
  }) 
  observe({
    if(input$inputDBPan == "Append .csv data to existing tables") 
    {
cat ("Upload new rows\n")
      tbs <- myListTables(dbGlb$dbIcon)
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
      output$step2ActionMsg <- renderText(if (length(tbs)) "" else 
        "No tables in existing database.")        
      initNewInputDB(session,output,dbGlb)
    }
  })
  ## uploadStdTree
  observe({  
    if (is.null(input$uploadStdTree)) return()
    isolate({ 
      indat = try(read.csv(file=input$uploadStdTree$datapath,as.is=TRUE,colClasses="character"))
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
        types = dbGlb$tbsCTypes[[input$uploadSelDBtabs]]
        for (icol in addCols)
        {
          newVar=names(indat)[icol]
          defType=charmatch(tolower(newVar),tolower(names(types)))
          dtyp = if (is.na(defType)) "character" else
                 if (types[defType]) "character" else "real"
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
         fvs_standinit = c("stand_id","variant","inv_year"),
         fvs_plotinit  = c("stand_id","variant","inv_year"),
         fvs_treeinit  = c("stand_id","species","dbh"),
         fvs_groupaddfilesandkeywords = c("groups"),
         NULL)
      if (!is.null(req) && !all(req %in% tolower(names(types))))
      {
        output$uploadActionMsg = renderText(paste0("Required columns were missing for ",
               input$uploadSelDBtabs,", no data loaded."))
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      }
      nums = tolower(names(types[!types]))
      lnams = tolower(names(indat))
      for (nn in nums) 
      {
        indx=match(nn,lnams)
        indat[,indx] = as.numeric(indat[,indx])
      }

      sids=try(dbGetQuery(dbGlb$dbIcon,paste0("select distinct stand_id from ",
                          isolate(input$uploadSelDBtabs))))
      sids=if (class(sids)=="try-error") NA else sids[,1]
      isid=charmatch("stand_id",tolower(names(indat)))
      msg=NULL
      if (!(is.na(sids) || is.na(isid))) 
      {
        tokeep=is.na(match(indat[,isid],sids))
        ntokill=sum(!tokeep)
        if (ntokill==nrow(indat))
        {
          output$uploadActionMsg = renderUI(HTML("All uploaded data have Stand_ID(s) that are already loaded and are ignored."))  
          return()
        } else {
          msg = paste0(ntokill," lines of uploaded data have Stand_ID(s) that are already loaded and are ignored.")
          indat = indat[tokeep,,drop=FALSE]
        }
      }
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
        vals=paste0(lapply(row[1,],function (x) if (class(x)=="character") paste0('"',x,'"') else x),collapse=",")
        qry = paste0("insert into ",input$uploadSelDBtabs," (",
                paste0(colnames(row),collapse=","),") values (",vals,");")
cat ("insert qry=",qry,"\n")
        res = try(dbExecute(dbGlb$dbIcon,qry))
        if (class(res) == "try-error") {err=TRUE; break} else insertCount = insertCount+1
      }
      if (err) 
      {
        dbRollback(dbGlb$dbIcon) 
        output$uploadActionMsg = renderUI(HTML(paste0("Error processing: ",qry)))
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        return()
      } else {
cat ("insertCount=",insertCount,"\n")
        dbCommit(dbGlb$dbIcon)
        msg=paste0(msg,"<br>",insertCount," row(s) inserted into ",isolate(input$uploadSelDBtabs))
          output$uploadActionMsg = renderUI(HTML(msg))
        session$sendCustomMessage(type = "resetFileInputHandler","uploadStdTree")
        loadVarData(globals,input,dbGlb$dbIcon)                                              
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
      initNewInputDB(session,output,dbGlb)
    })
  }) 
  ## climateFVSUpload
  observe({  
    if (is.null(input$climateFVSUpload)) return()
    progress <- shiny::Progress$new(session,min=1,max=10)
    progress$set(message = "Loading data set",value = 2)
    climAtt="FVSClimAttrs.csv"
    if (input$climateFVSUpload$type == "application/zip")
      try(unzip(input$climateFVSUpload$datapath, files = climAtt)) else 
      file.copy(input$climateFVSUpload$datapath,climAtt,
        overwrite = TRUE)
    if (!file.exists(climAtt)) 
    {
cat ("no FVSClimAttrs.csv file\n")
      output$uploadClimActionMsg = renderUI(HTML("FVSClimAttrs.csv not found."))
      progress$set(message = "FVSClimAttrs.csv not found", value = 6)
      Sys.sleep (2)
      session$sendCustomMessage(type = "resetFileInputHandler","climateFVSUpload")
      progress$close()
      return()
    }
cat ("processing FVSClimAttrs.csv\n")
    progress$set(message = "Loading data set (big files take a while)",value = 2) 
    climd = read.csv(climAtt,nrows=1)
    climd = read.csv(climAtt,colClasses=c(rep("character",2),
        "integer",rep("numeric",ncol(climd)-3)),as.is=TRUE)        
    colnames(climd)[1] <- "Stand_ID"
    unlink(climAtt)
    climTab <- myListTables(dbGlb$dbIcon)
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
    climDb="FVSClimAttrs.db"
    if (file.exists(climDb)) unlink(climDb)
    dbclim <- dbConnect(dbDrv,climDb)
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
    dbExecute(dbGlb$dbIcon,'attach database "',climDb,'" as new')
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
    unlink(climDb)
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
    if(input$inputDBPan == "View and edit existing tables" && input$topPan == "Import Input Data") 
    {
cat ("dataEditor View and edit existing tables\n")
      tbs <- myListTables(dbGlb$dbIcon)
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
      msg=checkMinColumnDefs(dbGlb$dbIcon)
cat ("msg=",msg,"\n")
      dbGlb$tbl <- NULL                                           
      dbGlb$tblCols <- names(dbGlb$tbsCTypes[[dbGlb$tblName]])
      if (length(grep("Stand_ID",dbGlb$tblCols,ignore.case=TRUE))) 
      {
        rtn = try(dbGetQuery(dbGlb$dbIcon,
          paste0("select distinct Stand_ID from '",dbGlb$tblName,"'")))
        if (class(rtn)=="try-error")
        {
cat ("stand_ID query error.\n")
           return()
        } else dbGlb$sids = rtn[,1]
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
      xlsxFile=system.file("extdata", "databaseDescription.xlsx", package = "fvsOL")
      tabs = try(read.xlsx(xlsxFile=xlsxFile,sheet="InputTableDescriptions"))
      if (class(tabs) != "try-error")
      {
        row = charmatch(toupper(input$editSelDBtabs),toupper(tabs[,1]))
        if (!is.na(row))
        {
          tab = tabs[row,1]
          html = paste0("<b>",tab,"</b> ",tabs[row,2])
          mhtml = xlsx2html(tab,xlsxfile=xlsxFile)
          if (!is.null(mhtml)) html = paste0(html,mhtml)
        }
      }
      output$inputTabDesc <- renderUI(HTML(html))
    }
cat ("editSelDBtabs returns\n")
  })              
  
  observe({              
    if (length(input$editSelDBvars)) 
    {
cat ("editSelDBvars, input$editSelDBvars=",input$editSelDBvars," mode=",input$mode,"\n")       
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
          qry <- paste0("select _ROWID_,* from '",dbGlb$tblName,"'")
          qry <- if (length(intersect("stand_id",tolower(dbGlb$tblCols))) && 
                     length(input$rowSelector))
            paste0(qry," where Stand_ID in (",
                  paste0("'",input$rowSelector,"'",collapse=","),");") else
            paste0(qry,";")                             
          dbGlb$tbl <- suppressWarnings(dbGetQuery(dbGlb$dbIcon,qry))
          lnames = tolower(colnames(dbGlb$tbl))
          stdSearch = trim(input$editStandSearch)
          if (nchar(stdSearch)>0) 
          {
            keep = try(grep (stdSearch,dbGlb$tbl[,charmatch("stand_id",lnames)]))
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
     " Stand_ID yes=",length(intersect("stand_id",tolower(dbGlb$tblCols))),"\n")
            if (is.null(dbGlb$sids) && 
                length(intersect("stand_id",tolower(dbGlb$tblCols))))
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
            qry <- if (length(grep("stand_id",tolower(dbGlb$tblCols))) && 
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
    reloadStandSelection(session,input)
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
    if(input$inputDBPan == "Upload Map data") 
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
        setwd(curdir)                                
cat ("mapUpload, class(lyrs)=",class(lyrs),"\n")
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
      datadir = dir()
cat ("input$mapUpLayers =",input$mapUpLayers,"\n")
      if (length(dir(datadir)) == 1) setwd(datadir)
      progress <- shiny::Progress$new(session,min=1,max=3)
      progress$set(message = paste0("Loading map: ",datadir," Layer: ",input$mapUpLayers),value=2)
      txtoutput = capture.output(dbGlb$spd <- try(readOGR(dir(),input$mapUpLayers,
                                                   drop_unsupported_fields=TRUE)))
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
         output$mapActionMsg = renderText("No map, upload one then set projection")
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
   
   prepSpatialData = function(dbGlb)
   {
     if (!exists("spd",envir=dbGlb,inherit=FALSE)) return(NULL)   
     stdInit = getTableName(dbGlb$dbIcon,"FVS_StandInit")
     ids1 = try(dbGetQuery(dbGlb$dbIcon,paste0('select distinct Stand_ID from ',stdInit)))
     ids1 = if (class(ids1)=="try-error") list() else unlist(ids1)
     names(ids1) = NULL
     ids2 = try(dbGetQuery(dbGlb$dbOcon,'select distinct StandID from FVS_Cases;'))
     ids2 = if (class(ids2)=="try-error") list() else unlist(ids2)
     names(ids2) = NULL
     keep=union(ids1,ids2)   
     matID = unlist(strsplit(input$mapUpIDMatch," "))[1]
     keep=na.omit(charmatch(keep,dbGlb$spd@data[,matID]))
     if (length(keep)) 
     {
       SpatialData=dbGlb$spd[keep,]
       attr(SpatialData,"MatchesStandID") =  matID
       output$mapActionMsg = renderText(paste0("Map saved for this project, StandID match=",
           matID,", Number of objects kept=",nrow(SpatialData@data)))
     } else {
       SpatialData=NULL
       output$mapActionMsg = renderText("No map or data to save.")
     }
     rm (spd,envir=dbGlb)    
     return(SpatialData)
   }
   observe({
     if(input$mapUpSave > 0)
     {
       SpatialData=prepSpatialData(dbGlb)
       if (!is.null(SpatialData)) 
       {
         save (SpatialData,file="SpatialData.RData")
         dbGlb$SpatialData = SpatialData
       }
     }
   })
   observe({
     if(input$mapUpAdd > 0)
     {     
       NewSpatialData=prepSpatialData(dbGlb)
       if (!is.null(NewSpatialData)) 
       {
         spatdat="SpatialData.RData"
         if (file.exists(spatdat)) load(file=spatdat)
         if (!exists("SpatialData")) SpatialData=NewSpatialData else
           SpatialData = if (class(SpatialData)=="list") 
             append(after=0,NewSpatialData) else list(SpatialData,NewSpatialData)        
         save (SpatialData,file=spatdat)
         dbGlb$SpatialData = SpatialData
       }
     }
   })

   observe({
     if(input$toolsPan == "Import runs and other items") 
     {
       choices = getProjectList(includeLocked=TRUE)
       actprj <- grep(basename(getwd()),choices) # remove current project
       if (length(actprj)) choices <- choices[-actprj]      
       updateSelectInput(session=session, inputId="impPrjSource", 
         choices=choices,selected=0)
       output$selectedSourceMsg <- renderText(
            paste0('<p style="font-size:17px;color:darkred"><b>',
                   'No source selected.</b>'))
       output$impPrjSourceMsg     <- NULL
       output$uploadRunsRdatMsg   <- NULL
       output$impRunsMsg          <- NULL  
       output$impCustomCmpsMsg    <- NULL
       output$impGraphSettingMsg  <- NULL
       output$impCustomQueriesMsg <- NULL
       output$impFVSDataMsg       <- NULL    
       output$impSpatialDataMsg   <- NULL
       updateSelectInput(session=session, inputId="uploadRunsRdat",choices=list())
       updateSelectInput(session=session, inputId="impRuns",choices=list())
       updateSelectInput(session=session, inputId="impCustomCmps",choices=list())
       updateSelectInput(session=session, inputId="impGraphSettings",choices=list())
       updateSelectInput(session=session, inputId="impCustomQueries",choices=list())
       updateSelectInput(session=session, inputId="impFVSData",choices=list())
       updateSelectInput(session=session, inputId="impSpatialData",choices=list())
     }  
  })                                     

  mkSrcMsgAndList <- function(db,nruns)
  {
    msg = paste0("File contains ",nruns," runs")
    tbs=dbListTables(db)
    itms=listTableNames(db)
    itms=intersect(itms,c("GraphSettings","customCmps","customQueries"))
    if (file.exists("SpatialData.RData")) itms=c(itms,"SpatialData")
    if (file.exists("FVS_Data.db")) itms=c(itms,"FVS_Data")
    if (length(itms)>0) msg=paste0(msg," plus: ",paste0(itms,collapse=", "))
    if (nruns > 0) itms=c(itms,"Runs")
    rtn = list(itms,msg)
    attr(rtn,"dir") = getwd()
    for (itm in itms) 
    { 
      switch(itm,
      "Runs" = {
        updateSelectInput(session=session, inputId="impRuns",
                          choices=getFVSRuns(db))
      },
      "GraphSettings" = {
        loadObject(db,"GraphSettings")
        names=setdiff(names(GraphSettings),"None")
        updateSelectInput(session=session, inputId="impGraphSettings",
          choices=as.list(names))
      },
      "customCmps" = {
        loadObject(db,"customCmps")      
        updateSelectInput(session=session, inputId="impCustomCmps",
          choices=as.list(names(customCmps)))
      },    
      "customQueries" = {
        loadObject(db,"customQueries")
        updateSelectInput(session=session, inputId="impCustomQueries",
          choices=as.list(names(customQueries)))
      })
    }
    zout = setdiff(c("Runs","GraphSettings","customCmps","customQueries"),itms)
    for (itm in zout) 
    { 
      switch(itm,
      "Runs" = updateSelectInput(session=session, inputId="impRuns",choices=list()),
      "GraphSettings" = updateSelectInput(session=session, inputId="impGraphSettings",choices=list()),
      "customCmps" = updateSelectInput(session=session, inputId="impCustomCmps",choices=list()),    
      "customQueries" = updateSelectInput(session=session, inputId="impCustomQueries",choices=list())
      )
    }
    rtn
  }                                          
    
  ## Upload zip file.                  
  observe({ 
    if (is.null(input$uploadRunsRdat)) return()
    if (input$uploadRunsRdat$type != "application/zip") {                                 
      output$uploadRunsRdatMsg  <- renderText("Uploaded file is not a .zip")
    } else { 
      isolate({
        if (length(globals$importItems))
        {
          if (attr(globals$importItems,"temp")) 
            unlink(attr(globals$importItems,"dir"),recursive = TRUE)
          globals$importItems=list()
        }   
        curdir = getwd()
        tdir = dirname(input$uploadRunsRdat$datapath)
        setwd(tdir)
        tmpPrj = uuidgen()
        dir.create(tmpPrj)
        tmpPrj = file.path(getwd(),tmpPrj)
        setwd(tmpPrj)
        uz = try(unzip(input$uploadRunsRdat$datapath))
        if (class(uz)=="try-error") 
        {
cat("uploaded zip failed\n")
          output$uploadRunsRdatMsg <- renderText("Uploaded file could not be unzipped.")
          unlink(input$uploadRunsRdat$datapath)
          unlink(tmpPrj,recursive=TRUE)
        } else {
          updateSelectInput(session=session, inputId="impPrjSource",selected=0)
          nruns=mkFVSProjectDB()
          db=connectFVSProjectDB()
          ml = mkSrcMsgAndList(db,nruns)
          dbDisconnect(db)
          output$uploadRunsRdatMsg <- renderUI(HTML(ml[[2]]))
          attr(ml,"temp") = TRUE # this directory can be deleted
          output$selectedSourceMsg <- renderText(
            paste0('<p style="font-size:17px;color:darkred"><b>Source: ',
                   ml[[2]]))
          globals$importItems = ml
cat("unload zip had ",length(uz),"items. ml[[2]]=",ml[[2]],"\n")
        }   
      setwd(curdir)
      })
    }
    session$sendCustomMessage(type = "resetFileInputHandler","uploadRunsRdat")          
  })

  observe({
    if (is.null(input$impPrjSource)) return() 
    {
      curdir = getwd()
      setwd("../")
      tmpPrj = file.path(getwd(),input$impPrjSource)
      if (dir.exists(tmpPrj)) 
      {
        setwd(tmpPrj)
        db = connectFVSProjectDB()
        nruns=mkFVSProjectDB()
        ml = mkSrcMsgAndList(db,nruns)
        ml[[2]] = gsub("File",paste("Project",input$impPrjSource),ml[[2]])
        dbDisconnect(db)
        output$selectedSourceMsg <- renderText(
            paste0('<p style="font-size:17px;color:darkred"><b>Source: ',
                   ml[[2]]))
        attr(ml,"temp") = FALSE  # don't delete this source directory
        globals$importItems = ml
      }   
      setwd(curdir)
    }    
  })                                                                                   
           
  observe({
    if (input$doImpRuns > 0 && !is.null(input$impRuns)) 
    {
      prjDir=attr(globals$importItems,"dir")
      pDB=connectFVSProjectDB(prjDir)
      on.exit(dbDisconnect(pDB))
      curRuns = names(getFVSRuns(dbGlb$prjDB))
      theRun = loadFVSRun(pDB,input$impRuns)
      if (is.null(theRun)) 
      { 
        output$impRunsMsg = renderText("The run could not be loaded.")
        return()
      }
      curTitle = theRun$title
      theRun$title = mkNameUnique(curTitle,names(getFVSRuns(dbGlb$prjDB)))
      theRun$uuid = uuidgen()
      storeFVSRun(dbGlb$prjDB,theRun)
      globals$FVS_Runs = getFVSRuns(dbGlb$prjDB)     
      output$impRunsMsg = renderText(paste0('Run "',curTitle,'" imported and ',
       ' is named "',theRun$title,'" in your current project.'))
      updateSelectInput(session=session, inputId="runSel", 
                        choices=globals$FVS_Runs,selected=globals$fvsRun$uuid)
    }
  })

  observe({
    if (input$doImpCustomCmps > 0  && !is.null(input$impCustomCmps))
    {
      prjDir=attr(globals$importItems,"dir")
      pDB=connectFVSProjectDB(prjDir)
      on.exit(dbDisconnect(pDB))
      loadObject(pDB,"customCmps",asName="source")
      loadObject(dbGlb$prjDB,"customCmps")
      curTitle = input$impCustomCmps
      newtitle = mkNameUnique(curTitle,names(customCmps))
      customCmps[newtitle] = source[curTitle]
      storeOrUpdateObject(dbGlb$prjDB,customCmps)
      output$impCustomCmpsMsg = renderText(paste0('Component "',curTitle,'" imported and ',
       ' is named "',newtitle,'" in your current project.'))      
    }
  })

  observe({
    if (input$doImpGraphSettings > 0 && !is.null(input$impGraphSettings))
    {
      prjDir=attr(globals$importItems,"dir")
      pDB=connectFVSProjectDB(prjDir)
      on.exit(dbDisconnect(pDB))
      loadObject(pDB,"GraphSettings",asName="source")
      loadObject(dbGlb$prjDB,"GraphSettings")
      curTitle = input$impGraphSettings
      newtitle = mkNameUnique(curTitle,names(GraphSettings))
      GraphSettings[newtitle] = source[curTitle]
      storeOrUpdateObject(dbGlb$prjDB,GraphSettings)
      output$impGraphSettingsMsg = renderText(paste0('Graph setting "',curTitle,'" imported and ',
       ' is named "',newtitle,'" in your current project.'))      
    }
  })

  observe({
    if (input$doImpCustomQueries > 0 && !is.null(input$impCustomQueries))
    {
      prjDir=attr(globals$importItems,"dir")
      pDB=connectFVSProjectDB(prjDir)
      on.exit(dbDisconnect(pDB))
      loadObject(pDB,"customQueries",asName="source")
      loadObject(dbGlb$prjDB,"customQueries")
      curTitle = input$impCustomQueries
      newtitle = mkNameUnique(curTitle,names(customQueries))
      customQueries[newtitle] = source[curTitle]
      storeOrUpdateObject(dbGlb$prjDB,customQueries)
      output$impCustomQueriesMsg = renderText(paste0('Query "',curTitle,'" imported and ',
       ' is named "',newtitle,'" in your current project.'))      
    }
  })
  observe({
    if (input$impFVS_Data > 0)
    {
cat(" input$impFVS_Data=",input$impFVS_Data,"\n")
      session$sendCustomMessage(type = "dialogContentUpdate",
        message = list(id = "impFVS_DataDlg",
          message = "This action overwrites your current FVS_Data.db")) 
    }
  })
  observe({  
    if (input$impFVS_DataDlgBtn == 0) return()
    isolate({
cat(" input$impFVS_DataDlgBtn=",input$impFVS_DataDlgBtn,"\n")
      needfile = file.path(attr(globals$importItems,"dir"),"FVS_Data.db")
      if (length(needfile) && nchar(needfile) && file.exists(needfile)) 
      {
        file.copy(from=needfile,to="FVS_Data.db",overwrite=TRUE) 
        output$impFVS_DataMsg = renderText("FVS_Data.db has been imported.")
      } else output$impFVS_DataMsg = renderText("Source FVS_Data.db was NOT found.")
    })  
  })
  observe({
    if(input$impSpatialData > 0)
    {           
cat(" input$impSpatialData=",input$impSpatialData,"\n")
      session$sendCustomMessage(type = "dialogContentUpdate",
        message = list(id = "impSpatialDataDlg",
          message = "This action overwrites your current SpatialData")) 
     }                                                                    
  })
  observe({  
    if (input$impSpatialDataDlgBtn == 0) return()
    isolate({
cat(" input$impSpatialDataDlgBtn=",input$impSpatialDataDlgBtn,"\n")
      needfile = file.path(attr(globals$importItems,"dir"),"SpatialData.RData")
      if (length(needfile) && nchar(needfile) && file.exists(needfile)) 
      {
        file.copy(from=needfile,to="SpatialData.RData",overwrite=TRUE) 
        output$impSpatialDataMsg = renderText("SpatialData.RDatahas been imported.")
      } else output$impSpatialDataMsg = renderText("Source SpatialData.RData was NOT found.")
    })  
  })  
 
  #runScript selection                                                         
  observe(if (length(input$runScript)) customRunOps())

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
        fn=paste0("customRun_",globals$fvsRun$runScript,".R")
        if (!file.exists(fn)) fn=system.file("extdata", fn, package = "fvsOL")
        if (!file.exists(fn)) return()        
        rtn = try(source(fn))
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
    
  updateProjectSelections <- function ()
  {
    selChoices = getProjectList() 
    nsel = charmatch(basename(getwd()),selChoices)
    nsel = if(is.na(nsel)) NULL else nsel[1]
    sel = if (is.null(nsel)) NULL else selChoices[[nsel]]
    updateSelectInput(session=session, inputId="PrjSelect", 
        choices=selChoices,selected=sel)
    ### Block the ability to delete Project_1 on windows
    if(.Platform$OS.type == "windows")
    {
      prj1 = charmatch("Project_1",selChoices)
      if (!is.na(prj1)) selChoices=selChoices[-prj1]
      actprj <- grep(basename(getwd()),selChoices)
      if (length(actprj)) selChoices <- selChoices[-actprj]
    }
    updateSelectInput(session=session, inputId="PrjDelSelect",choices=selChoices,
                      selected=0)
    backups = dir (pattern="ProjectBackup")
    if (length(backups)) 
    {
      backups = sort(backups,decreasing=TRUE)
      names(backups) = backups 
    } else backups=list()
    updateSelectInput(session=session, inputId="pickBackup", 
      choices = backups, selected=NULL)
  }

   ## Projects hit
  observe({    
    if (input$topPan == "Project Tools" && input$toolsPan == "Manage project") 
    {
cat ("Manage project hit\n")
      updateProjectSelections()
    }
    
  })
 
  ## Make New Project (PrjNew)
  observe({
    if (length(input$PrjNew)==0 || input$PrjNew == 0) return()
    isolate({                                                  
cat ("Make new project, input$PrjNewTitle=",input$PrjNewTitle,"\n")
      if (nchar(input$PrjNewTitle)==0) return()
      prjid = if (file.exists("projectId.txt")) scan("projectId.txt",
         what="character",sep="\n",quiet=TRUE) else NUL
      fbin = Sys.readlink(fvsBin) #will be na if file does not exist, "" if not symbolic link.
      if (is.na(fbin)) return()
      curdir = getwd()
      setwd("../")
      newTitle = input$PrjNewTitle
      fn = if (isLocal()) 
      {       
        basedir = basename(curdir)
        newTitle <- mkFileNameUnique(newTitle)
        newTitle
      } else uuidgen()
      dir.create(fn)
      setwd(fn)
      if (dirname(fvsBin) == ".")  #fvsBin points to an entry in the current dir.
      {
        if (nchar(fbin)) file.symlink(fbin, "FVSbin") else 
          file.copy(paste0(normalizePath(curdir),"/FVSbin"), getwd(), recursive = TRUE,
            copy.mode = TRUE, copy.date = TRUE)
      }
      if (!isLocal()) newTitle=mkNameUnique(newTitle,setOfNames=names(getProjectList(includeLocked=TRUE)))
      ntit=paste0("title= ",newTitle)
      idrow = grep("title=",prjid)
      if (length(idrow)==0) prjid=c(prjid,ntit) else prjid[idrow]=ntit  
      write(file="projectId.txt",prjid)
      updateTextInput(session=session, inputId="PrjNewTitle",value="")
      setwd(curdir)
      updateProjectSelections()
    })
  }) 
  
  observe(if (length(input$PrjOpen) && input$PrjOpen > 0) 
  {
    isolate({
      newPrj=paste0("../",input$PrjSelect)
      plk = file.exists(paste0(newPrj,"/projectIsLocked.txt"))
cat("PrjOpen to=",newPrj," dir.exists(newPrj)=",dir.exists(newPrj),
" locked=",plk,"\n")
      if (plk) {updateProjectSelections();return()}
      if (dir.exists(newPrj))
      { 
        if (isLocal()) 
        {
          rscript = if (exists("RscriptLocation")) RscriptLocation else 
            commandArgs(trailingOnly=FALSE)[1]
          cmd = paste0(rscript," --vanilla -e $require(fvsOL);fvsOL(prjDir='",newPrj,
                       "',fvsBin='",fvsBin,"');quit()$")
          cmd = gsub('$','"',cmd,fixed=TRUE)
          if (.Platform$OS.type == "unix") cmd = paste0("nohup ",cmd," >> /dev/null")
cat ("cmd for launch project=",cmd,"\n")
          system (cmd,wait=FALSE)
        } else {                                           
          url = paste0(session$clientData$url_protocol,"//",
                       session$clientData$url_hostname,"/FVSwork/",input$PrjSelect)
cat ("launch url:",url,"\n")
          session$sendCustomMessage(type = "openURL",url)
        }
      }          
    })
  })
  
  observe({
    mkSimCnts(globals$fvsRun,justGrps=input$simContType=="Just groups") 
    updateSelectInput(session=session, inputId="simCont",       
         choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
  })

  saveRun <- function(input,session) 
  {
    isolate({
      runName = trim(input$title)
      if (nchar(input$title) == 0) runName <- nextRunName(names(globals$FVS_Runs))
      me=match(globals$fvsRun$uuid,globals$FVS_Runs)
cat ("saveRun, length(me)=",length(me)," uuid=",globals$fvsRun$uuid," class(globals$fvsRun)=",class(globals$fvsRun),"\n")
      if (length(me)==0 || is.na(me)) return() else runNames=names(globals$FVS_Runs)[-me]
      runName=mkNameUnique(runName,runNames)
      if (runName != input$title) updateTextInput(session=session, inputId="title",
         value=runName)         
      globals$fvsRun$title = runName
cat ("in saveRun, globals$fvsRun$defMgmtID=",globals$fvsRun$defMgmtID," input$defMgmtID=",input$defMgmtID,"\n")
      globals$fvsRun$defMgmtID = input$defMgmtID
      globals$fvsRun$runScript = if (length(input$runScript)) input$runScript else "fvsRun"
      if (globals$fvsRun$runScript == "fvsRun") globals$fvsRun$uiCustomRunOps = list() else
      {
        for (item in names(globals$fvsRun$uiCustomRunOps))                                
          globals$fvsRun$uiCustomRunOps[[item]] = input[[item]]
      }
cat ("saveRun class(globals$fvsRun)=",class(globals$fvsRun),"\n")
      # sometimes the class fvsRun is assigned to package ".GlobalEnv" and it
      # should be the for this package.
      if (attr(class(globals$fvsRun),"package")==".GlobalEnv") 
          attr(class(globals$fvsRun),"package") = "fvsOL"
      storeFVSRun(dbGlb$prjDB,globals$fvsRun)
      globals$FVS_Runs=getFVSRuns(dbGlb$prjDB)
cat ("saveRun, input$inVars=",input$inVars,"\n") 
      globals$lastRunVar = globals$activeVariants[1]   
cat ("leaving saveRun, globals$lastRunVar=",globals$lastRunVar,"\n") 
    }) 
  }
}

