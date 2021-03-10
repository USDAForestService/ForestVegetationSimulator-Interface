# $Id$


if (exists("mkfvsStd",envir=.GlobalEnv)) rm (mkfvsStd,envir=.GlobalEnv)
mkfvsStd <<- setRefClass("fvsStd",
  fields = list(sid = "character", rep = "numeric", repwt = "numeric", 
    invyr = "character", grps = "list", cmps = "list",uuid="character"))

if (exists("mkfvsGrp",envir=.GlobalEnv)) rm (mkfvsGrp,envir=.GlobalEnv)
mkfvsGrp <<- setRefClass("fvsGrp",
  fields = list(grp = "character", cmps = "list", uuid="character"))
                          
if (exists("mkfvsCmp",envir=.GlobalEnv)) rm (mkfvsCmp,envir=.GlobalEnv)
mkfvsCmp <<- setRefClass("fvsCmp",
  fields = list(kwds = "character", kwdName = "character", exten="character",
    title="character", variant="character",uuid="character", atag="character", 
    reopn="character"))
# atag is always "c" if the component is a condition, "k" if it is a keyword
# component that is not attached to a specific component. If it is longer than 1
# character it is the uuid of the related condition

if (exists("mkfvsRun",envir=.GlobalEnv)) rm (mkfvsRun,envir=.GlobalEnv)
mkfvsRun <<- setRefClass("fvsRun", 
  fields = list(stands = "list", grps = "list", simcnts = "list",
    selsim = "list", FVSpgm = "character", title = "character", 
    startyr = "character", endyr = "character", cyclelen = "character",
    cycleat = "character", refreshDB = "character", uuid="character",
    defMgmtID = "character", autoOut = "list", runScript = "character" ,
    uiCustomRunOps = "list", startDisp = "character"))

if (exists("mkGlobals",envir=.GlobalEnv)) rm(mkGlobals,envir=.GlobalEnv)
mkGlobals <<- setRefClass("globals", 
  fields = list(activeFVS = "list", activeVariants = "character", 
    activeExtens = "character", schedBoxYrLastUsed = "character",
    extnsel = "character", kwdsel = "list", mgmtsel = "list",
    mevsel = "list", mmodsel = "list", pastelist = "list",
    pastelistShadow = "list", inData = "list", FVS_Runs = "list",
    customCmps = "list", selStds = "character",
    schedBoxPkey = "character", currentCmdPkey = "character",GrpNum="numeric",
    currentCndPkey = "character", winBuildFunction = "character",GenGrp="list", 
    existingCmps = "list",currentQuickPlot = "character", currentCmdDefs="character",
    currentEditCmp = "fvsCmp", NULLfvsCmp = "fvsCmp", saveOnExit= "logical",
    customQueries = "list", fvsRun = "fvsRun", foundStand="integer", 
    reloadAppIsSet = "numeric", hostname= "character", toggleind="character",
    selStandTableList = "list",kcpAppendConts = "list",opencond="numeric",
    condKeyCntr="numeric",prevDBname="list",changeind="numeric",timeissue="numeric",
    lastRunVar="character",deleteLockFile="logical",gFreeze="logical",
    settingChoices="list",exploreChoices="list",simLvl="list",stdLvl="list",
    specLvl="list",dClsLvl="list",htClsLvl="list",treeLvl="list",tbsFinal="list",
    localWindows="logical",selRuns = "character", selUuids = "character",
    selAllVars="logical",explorePass="numeric"))

loadStandTableData <- function (globals, dbIcon)
{
  tbls=myListTables(dbIcon)
  globals$selStandTableList = list(
    "Stands (FVS_StandInit)"                                     = "FVS_StandInit",
    "Plots within stands (FVS_PlotInit)"                         = "FVS_PlotInit",
    "Inventory Plots (FVS_StandInit_Plot)(e.g.: FIA plots)"      = "FVS_StandInit_Plot",
    "Inventory Subplots (FVS_PlotInit_Plot)(e.g.: FIA subplots)" = "FVS_PlotInit_Plot",
    "Conditions (FVS_StandInit_Cond)(e.g.: FIA conditions)"      = "FVS_StandInit_Cond")
  have=na.omit(match(toupper(tbls),toupper(globals$selStandTableList)))
  if (length(have) && !is.null(have)) globals$selStandTableList = globals$selStandTableList[sort(have)]
} 

loadVarData <- function(globals,dbIcon)
isolate({
cat ("in loadVarData, input$inTabs=",input$inTabs," globals$activeVariants=",globals$activeVariants,"\n") 
  dbtabs = dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[,1]
  dbtabsU = toupper(dbtabs)
  intab = if (is.null(input$inTabs)) toupper("FVS_StandInit") else toupper(input$inTabs)
  if (! intab %in% dbtabsU) intab = toupper("FVS_StandInit")
  if (! intab %in% dbtabsU) intab = toupper("FVS_StandInit_Cond")
  if (! intab %in% dbtabsU) intab = toupper("FVS_StandInit_Plot")
  if (! intab %in% dbtabsU) intab = NULL
  if (!is.null(intab))
  {
    vars = try(dbGetQuery(dbIcon,paste0('select distinct variant from ',intab)))    
    if (class(vars) != "try-error")
    {
      vars=sort(unique(tolower(scan(text=gsub(","," ",vars[,1]),what="character",
           strip.white=TRUE,sep=" ",quiet=TRUE))))
      vars=vars[vars != ""]
      keep=na.omit(match(vars,globals$activeVariants))
      if (length(keep) && !is.na(keep)) globals$activeVariants = globals$activeVariants[keep] 
    } 
  }
cat ("in loadVarData, globals$activeVariants=",globals$activeVariants,"\n")
  fvsKeys = getTableName(dbIcon,"FVS_GroupAddFilesAndKeywords")
  if (!is.null(fvsKeys)) 
  {
    globals$inData$FVS_GroupAddFilesAndKeywords <- dbReadTable (dbIcon,fvsKeys)
    names(globals$inData$FVS_GroupAddFilesAndKeywords) <- 
          toupper(names(globals$inData$FVS_GroupAddFilesAndKeywords))
  } 
})

reorderFVSRuns <- function(FVS_Runs)
{
  if (length(FVS_Runs) > 1)
  {
    tims = unlist(lapply(FVS_Runs,function (x) attr(x,"time")))
    idx = sort(tims,decreasing=TRUE,index.return=TRUE)$ix
    FVS_Runs = FVS_Runs[idx]
  }
  FVS_Runs
}

findIdx = function (list, uuid)
{
  if (length(list) == 0 || is.null(uuid)) return(NULL)
  for (i in 1:length(list)) if (list[[i]]$uuid == uuid) return (i)
  NULL
}


resetfvsRun <- function(fvsRun,FVS_Runs)
{
cat("resetfvsRun\n")
  fvsRun$stands = list()
  fvsRun$grps = list()
  fvsRun$simcnts = list()
  fvsRun$selsim = list()
  fvsRun$FVSpgm = character(0)
  fvsRun$autoOut = list()
  np1 = length(FVS_Runs)+1
  fvsRun$title = paste0("Run ",np1)
  fvsRun$startyr = character(0)
  fvsRun$endyr = character(0)
  fvsRun$cyclelen = character(0)
  fvsRun$cycleat = character(0)
  fvsRun$defMgmtID = sprintf("A%3.3d",np1)
  fvsRun$runScript = "fvsRun"
  uiCustomRunOps = list()
  fvsRun$uuid = uuidgen()
  fvsRun$startDisp = character(0)
}

trim <- function (x) gsub("^\\s+|\\s+$","",x)

mkpair <- function(alist)
{
cat("mkpair, length(alist)=",length(alist),"\n")
  if (length(list) == 0) return(list())
  l <- as.list(as.character(1:length(alist)))
  names(l) <- names(alist)
  l
}


getBkgRunList = function ()
{
  if(!globals$localWindows)pidfiles=dir(pattern="pidStatus$")
  if(globals$localWindows){
    pidfiles=dir(path=prjDir,pattern="pidStatus$")
    if (length(pidfiles))pidfiles=paste0(prjDir,"/",pidfiles)
  }
  if (length(pidfiles) == 0) return (list())
  theList = unlist(lapply(pidfiles,function (x)
    scan(file=x,what="character",sep="\n",quiet=TRUE)))
  pidfiles=as.list(pidfiles)
  names(pidfiles) = theList
  pidfiles 
}

killIfRunning = function (uuid)
{
  if(!globals$localWindows)fn = paste0(uuid,".pidStatus")
  if(globals$localWindows)fn = paste0(prjDir,"/",uuid,".pidStatus")
cat ("killIfRunning, fn=",fn,"\n")
  if (file.exists(fn))
  {
    pid = scan (file=fn,what="character",n=1,sep=" ",quiet=TRUE)
    cmd = if (.Platform$OS.type == "windows") paste("taskkill /pid",pid) else 
      paste("kill ",pid)    
cat ("kill cmd =",cmd,"\n")
    system (cmd)
    Sys.sleep(.5)
    unlink(fn)
  }
}
    
   
removeFVSRunFiles = function (uuid,all=FALSE)
{
  if(globals$localWindows)uuid = paste0(prjDir,"/",uuid)
  if (file.exists(uuid)) 
  {
    unlink(paste0(uuid,"/",dir(uuid)), recursive = TRUE, force = TRUE)
    unlink(uuid)
  }
  if(!globals$localWindows){
    fls = dir(pattern=uuid)
    if (!all) fls = setdiff(fls,paste0(uuid,".RData"))
    unlink(fls, recursive = TRUE, force = TRUE)
  }
  if(globals$localWindows){
    fls=dir(prjDir,pattern=basename(uuid))
    if (!all) fls = setdiff(fls,paste0(basename(uuid),".RData"))
    unlink(paste0(prjDir,"/",fls), recursive = TRUE, force = TRUE)
  }
}

findCmp = function (fvsRun,cmp)
{
cat("findCmp, cmp=",cmp,"\n")
  for (grp in fvsRun$grps) 
  {
    icm = findIdx(grp$cmps,cmp)
    if (!is.null(icm)) return(grp$cmps[[icm]])
  }
  for (std in fvsRun$stands) 
  {
    icm = findIdx(std$cmps,cmp)
    if (!is.null(icm)) return(std$cmps[[icm]])
  }
  NULL                                 
}


writeKeyFile <- function (fvsRun,dbIcon,prms,newSum=TRUE)
{
  stds = unlist(lapply(fvsRun$stands,function(x) x$sid))
cat("writeKeyFile, num stds=",length(stds),
    " fvsRun$title=",fvsRun$title," uuid=",fvsRun$uuid,"\n")
  globals$timeissue <- 0
  if (length(stds)==0) return("No stands to process.")
  dbExecute(dbIcon,'drop table if exists temp.RunStds')                   
  dbWriteTable(dbIcon,DBI::SQL("temp.RunStds"),data.frame(RunStds = stds))
 
  # get the preferred ids depending on the table that was used to build the run
  intable=toupper(fvsRun$refreshDB)
  queryIDs=switch(intable,
                  "FVS_STANDINIT"     =c("STAND_ID","STAND_CN"),
                  "FVS_PLOTINIT"      =c("STANDPLOT_ID","STANDPLOT_CN"),
                  "FVS_STANDINIT_COND"=c("STAND_ID","STAND_CN"),
                  "FVS_STANDINIT_PLOT"=c("STAND_ID","STAND_CN"),
                  "FVS_PLOTINIT_PLOT" =c("STANDPLOT_ID","STANDPLOT_CN"))
  initfields = try(toupper(dbListFields(dbIcon,intable)))
  if (class(initfields) == "try-error") return("Run data query returned no data to run.")
  queryIDs = queryIDs[queryIDs %in% initfields] 
  if (length(queryIDs) == 0) return("Needed stand id fields are missing")
  qry = paste0('select ',paste0(queryIDs,collapse=','),',Groups,Inv_Year,Sam_Wt from ',
              intable,' where ',queryIDs[1],' in (select RunStds from temp.RunStds)')  
cat ("qry=",qry,"\n")
  fvsInit = try(dbGetQuery(dbIcon,qry))
  if (class(fvsInit) == "try-error") return("Run data query failed.")
  if (nrow(fvsInit) == 0) return("Run data query returned no data to run.")
  # compute replication weights
  stofix=table(stds)
  stofix=names(stofix[stofix>1])
  rwts=unlist(lapply(fvsRun$stands,function(x) x$repwt))
  wtofix=list()
  for (sf in stofix)
  {
    idxs=grep(sf,stds,fixed=TRUE)
    if (length(idxs)) wtofix[[sf]] = rwts[idxs]/sum(rwts[idxs])
  }
  extns = globals$activeFVS[fvsRun$FVSpgm][[1]]
  source("autoOutKeys.R",local=TRUE)
  defaultOut = sub ("FVSOut",fvsRun$uuid,defaultOut)
  if (!newSum)  defaultOut = sub ("Summary        2","Summary",defaultOut)
  if(!globals$localWindows)fc = file(description=paste0(fvsRun$uuid,".key"),open="wt")
  if(globals$localWindows)fc = file(description=paste0(prjDir,"/",fvsRun$uuid,".key"),open="wt")
  cat ("!!title:",fvsRun$title,"\n",file=fc)
  cat ("!!uuid: ",fvsRun$uuid,"\n",file=fc)
  cat ("!!built:",format(Sys.time(), 
        "%Y-%m-%d_%H:%M:%S"),"\n",file=fc)
  thisYr = as.numeric(format(Sys.time(), "%Y"))
  # Start year checks
  for(i in 1:length(globals$fvsRun$stands)){
    if (((input$startyr !="" && ((as.numeric(input$startyr)) > (thisYr + 50))) ||
         ((input$startyr !="") && nchar(input$startyr) > 4))){
      session$sendCustomMessage(type = "infomessage",
              message = paste0("The common starting year of ",input$startyr," is more than 50 years from the current year of ", thisYr))
      globals$timeissue <- 1
      return()
    }
    if ((input$startyr !="") && (input$startyr < globals$fvsRun$stands[[i]]$invyr)){
      session$sendCustomMessage(type = "infomessage",
              message = paste0("The common starting year of ",input$startyr," is before the inventory year of ", globals$fvsRun$stands[[i]]$invyr))
      globals$timeissue <- 1
      return()
    }
    if (input$startyr =="") {
      session$sendCustomMessage(type = "infomessage",
              message = paste0("The common starting year is blank."))
      globals$timeissue <- 1
      return()
    }
  }
  # End year checks
  for(i in 1:length(globals$fvsRun$stands)){
    if (((input$endyr !="" && ((as.numeric(input$endyr)) > 
      (as.numeric(input$cyclelen) * 40 + as.numeric(input$startyr)))) ||
         ((input$endyr !="") && nchar(input$endyr) > 4))){
      session$sendCustomMessage(type = "infomessage",
              message = paste0("The common ending year of ", input$endyr,
              " is more than 40 growth cycles from the current year of ", thisYr))
      globals$timeissue <- 1
      return("Year check error")
    }
    if ((input$endyr !="") && ((as.numeric(input$endyr) < 
      as.numeric(globals$fvsRun$stands[[i]]$invyr)))){
      session$sendCustomMessage(type = "infomessage",
              message = paste0("The common ending year of ", input$endyr,
              " is before the inventory year of ", globals$fvsRun$stands[[i]]$invyr))
      globals$timeissue <- 1
      return("Year check error")
    }
    if (input$endyr =="") {
      session$sendCustomMessage(type = "infomessage",
              message = paste0("The common ending year is blank."))
      globals$timeissue <- 1
      return("Year check error")
    }
  }
  # Cycle length checks
  if (((input$cyclelen !="" && ((as.numeric(input$cyclelen)) > 50))) ||
       ((input$cyclelen !="") && nchar(input$cyclelen) > 4)){
    session$sendCustomMessage(type = "infomessage",
            message = paste0("The growth interval of ", input$cyclelen,
            " years is greater than the maximum 50 years"))
    globals$timeissue <- 1
    return("Year check error")
  }
  if (input$cyclelen =="") {
    session$sendCustomMessage(type = "infomessage",
            message = paste0("The growth interval is blank."))
    globals$timeissue <- 1
    return("Year check error")
  }
  baseCycles = seq(as.numeric(fvsRun$startyr),as.numeric(fvsRun$endyr),
                   as.numeric(fvsRun$cyclelen))
  cycleat = scan(text=gsub(";"," ",gsub(","," ",fvsRun$cycleat)),
                 what=0,quiet=TRUE)
  # Cycle break checks
  if (length(cycleat)){
    for(i in 1:length(globals$fvsRun$stands)){
      for(j in 1:length(cycleat)){
        if ((cycleat[j] > (thisYr + 400))){
          session$sendCustomMessage(type = "infomessage",
                  message = paste0("The additional reporting year of ", cycleat[j],
                  " is more than 400 years from the current year of", thisYr))
          globals$timeissue <- 1
          return("Cycle break check error")
        }
        if ((cycleat[j] < as.numeric(globals$fvsRun$stands[[i]]$invyr))){
          session$sendCustomMessage(type = "infomessage",
                  message = paste0("The additional reporting year of ", cycleat[j],
                  " is before the inventory year of ", globals$fvsRun$stands[[i]]$invyr))
          globals$timeissue <- 1
          return("Cycle break check error")
        }
      }
    }
  }
  cycleat = union(baseCycles,cycleat)
  cycleat = sort(union(cycleat,as.numeric(fvsRun$endyr))) 
  for (std in fvsRun$stands)
  { 
    names(fvsInit) <- toupper(names(fvsInit))
    sRows = match (std$sid, fvsInit$STAND_ID)
    sRowp = match (std$sid, fvsInit$STANDPLOT_ID)
cat ("processing std=",std$sid," sRows=",sRows," sRowp=",sRowp,"\n")    
    if (is.na(sRows) && is.na(sRowp)) next
    cat ("StdIdent\n",sprintf("%-26s",std$sid)," ",fvsRun$title,"\n",file=fc,sep="")
    if (!is.null(fvsInit$STAND_CN[sRows]) && !is.na(fvsInit$STAND_CN[sRows]) && 
        fvsInit$STAND_CN[sRows] != " "){ 
      cat ("StandCN\n",fvsInit$STAND_CN[sRows],"\n",file=fc,sep="")
    } else if (!is.null(fvsInit$STANDPLOT_CN[sRowp]) && !is.na(fvsInit$STANDPLOT_CN[sRowp]) && 
        fvsInit$STANDPLOT_CN[sRowp] != " "){ 
      cat ("StandCN\n",fvsInit$STANDPLOT_CN[sRowp],"\n",file=fc,sep="")
      }else cat ("StandCN\n",std$sid,"\n",file=fc,sep="")
    cat ("MgmtId\n",fvsRun$defMgmtID,"\n",file=fc,sep="") 
    if (length(std$invyr) == 0) std$invyr = as.character(thisYr) 
    ninvyr = as.numeric(std$invyr)
    cat ("InvYear       ",std$invyr,"\n",file=fc,sep="")
    thiscyc = union(seq(ninvyr,cycleat[1],as.numeric(fvsRun$cyclelen)),cycleat)
    ints = diff(sort(thiscyc))
    if (length(ints) > 40) ints = ints[1:40]
    if (length(ints)==0) ints = fvsRun$cyclelen
    mostint = names(which.max(table(ints)))
    ints = as.character(ints)
    cat ("TimeInt                ",mostint,"\n",file=fc)
    for (i in 1:length(ints)) if (ints[i] != mostint) 
       cat ("TimeInt      ",as.character(i),"      ",ints[i],"\n",file=fc)
    cat ("NumCycle    ",as.character(i),"\n",file=fc)
    cat (defaultOut,file=fc)
    # "checking" the FVS Outputs suppresses adding autoDelOTab so make that logical switch here
    autos = if (is.null(names(fvsRun$autoOut))) unlist(fvsRun$autoOut) else 
                unlist(fvsRun$autoOut[["autoOut"]])
    autos = if ("autoDelOTab" %in% autos) 
    { 
      aa = setdiff(autos,"autoDelOTab")
      unlist(lapply(aa,function(a) {aw = paste0(a,".withText"); if (exists(aw)) aw else a}))
    } else c(autos,"autoDelOTab")
    for (out in autos) if (exists(out) && !is.null(out)) eval(parse(text=paste0("cat(",out,",file=fc)")))

    if (!is.null(fvsRun$autoOut[["svsOut"]]) && !is.null(fvsRun$autoOut[["svsOut"]][["svs"]]) && 
      exists("autoSVS"))
    {
      shape = if (fvsRun$autoOut[["svsOut"]][["shape"]] == "Square") "1" else "3"
      nfire = as.character(fvsRun$autoOut[["svsOut"]][["nfire"]])
      keys=unlist(strsplit(autoSVS,"\n"))
      svs=grep("^SVS ",keys)
      if (length(svs)) substr(keys[svs],11,20) = sprintf("%10s",shape)
      svs=grep("^SVImages ",keys)
      if (length(svs)) substr(keys[svs],11,20) = sprintf("%10s",nfire)
      lapply (keys,function(x,fc) cat (x,"\n",file=fc),fc); cat ("\n",file=fc)
    } 
    lastExt = "base"
    lastCnd = NULL
    extensPrefixes = c("estb"="Estab","strp"="Estab","cover"="Cover",
      "fire"="FMIn","mist"="Mistoe",
      "ardwrd3"="RDIn","armwrd3"="RDIn\nRRType             3",
      "phewrd3"="RDIn\nRRType             4","dbs"="DataBase",
      "econ"="Econ","climate"="Climate","organon"="Organon")

    if (length(std$grps)) for (grp in std$grps)
    {
      if (length(grp$cmps)) for (cmp in grp$cmps)
      {
        if (length(grep("Addfile:",cmp$title)) || length(grep("Editor:",cmp$title))){
          # we have ourselves an addfile
          basekwds <- list("ADDFILE","AGPLABEL","ALSOTRY","ATRTLIST","BAIMULT","BAMAX","BFDEFECT",
                           "BFFDLN","BFVOLEQU","BFVOLUME","CALBSTAT","CCADJ","CFVOLEQU","CHEAPO",
                           "CLOSE","COMMENT","COMPRESS","CRNMULT","CRUZFILE","CUTEFF",
                           "CUTLIST","CWEQN","CYCLEAT","DATASCRN","DEBUG","DEFECT","DELOTAB",
                           "DESIGN","DGSTDEV","ECHO","ECHOSUM","ENDFILE","FERTILIZ","FIXCW",
                           "FIXDG","FIXHTG","FIXMORT","FVSSTAND","GROWTH","HTGMULT","HTGSTOP",
                           "INPFILE","INVYEAR","LOCATE","MANAGED","MCDEFECT","MCFDLN","MGMTID",
                           "MINHARV","MODTYPE","MORTMSB","MORTMULT","NOAUTOES","NOCALIB",
                           "NODEBUG","NOECHO","NOHTDREG","NOSCREEN","NOSUM","NOTREES","NOTRIPLE",
                           "NUMCYCLE","NUMTRIP","OPEN","ORINFO","ORGVOL","POINTGRP","POINTREF",
                           "PROCESS","PRUNE","RANNSEED","READCORD","READCORH","READCORR",
                           "REGDMULT","REGHMULT","RESETAGE","REUSCORD","REUSCORH","REUSCORR",
                           "REWIND","SCREEN","SDICALC","SDIMAX","SERLCORR","SETPTHIN","SETSITE",
                           "SITECODE","SPCODES","SPECPREF","SPGROUP","SPLABEL","SPLEAVE",
                           "STANDCN","STATS","STDIDENT","STDINFO","STOP","STRCLASS","SVS",
                           "TCONDMLT","TFIXAREA","THINABA","THINATA","THINAUTO","THINBBA",
                           "THINBTA","THINCC","THINDBH","THINHT","THINMIST","THINPRSC","THINPT",
                           "THINQFA","THINRDEN","THINRDSL","THINSDI","TIMEINT","TOPKILL","TREEDATA",
                           "TREEFMT","TREELIST","TREESZCP","VOLEQNUM","VOLUME","YARDLOSS")
          invokekwds <- list("ESTAB","FMIN","DATABASE","CLIMATE","ECON","COVER","RDIN","MISTOE")
          regenkwds <- list("AUTALLY","BUDWORM","BURNPREP","EXCRUISE","HABGROUP","HTADJ","INGROW",
                            "MECHPREP","MINPLOTS","NATURAL","NOAUTALY","NOINGROW","NOSPROUT","OUTPUT",
                            "PASSALL","PLANT","PLOTINFO","SPECMULT","SPROUT",
                            "STOCKADJ","TALLY","TALLYONE","TALLYTWO","THRSHOLD")
          firekwds <- list("BURNREPT","CANCALC","CANFPROF","CARBCALC","CARBCUT","CARBREPT","DEFULMOD",
                           "DROUGHT","DUFFPROD","DWDCVOUT","FIRECALC","FLAMEADJ","FMODLIST","FMORTMLT",
                           "FUELDCAY","FUELFOTO","FUELINIT","FUELMODL","FUELMOVE","FUELMULT","FUELOUT",
                           "FUELPOOL","FUELREPT","FUELSOFT","FUELTRET","MOISTURE","MORTCLASS","MORTREPT",
                           "PILEBURN","POTFIRE","POTFMOIS","POTFPAB","POTFSEAS","POTFTEMP","POTFWIND",
                           "SALVAGE","SALVSP","SIMFIRE","SNAGBRK","SNAGCLAS","SNAGDCAY","SNAGFALL",
                           "SNAGINIT","SNAGOUT","SNAGPBN","SNAGPSFT","SNAGSUM","SOILHEAT","STATFUEL",
                           "SVIMAGES")
          dbkwds <- list("ATRTLIST","BURNREPT","CARBRPTS","COMPUTE","CUTLIST","DWDCVOUT","DWDVLOUT",
                         "ECONRPTS","FUELREPT","FUELSOFT","MISRPTS","MORTREPT","POTFIRE","RDBBMORT",
                         "RDDETAIL","RDSUM","SNAGOUT","SNAGSUM","STRCLASS","SUMMARY","TREELIST",
                         "STANDSQL","TREESQL","SQLIN","SQLOUT","DSNIN","DSNOUT","INVSTATS","REGREPTS")
          climatekwds <- list("CLIMDATA","SETATTR","AUTOESTB","GROWMULT","MORTMULT","MXDENMLT","CLIMREPT")
          econkwds <- list("ANNUCST","ANNURVN","BURNCST","HRVFXCST","HRVVRCST","HRNRVN","LBSCFV",
                           "MECHCST","NOTABLE","PCTFXCST","PCTSPEC","PCTVRCST","PLANTCST","PRETEND",
                           "SPECCST","SPECRVN","STRTECON")
          coverkwds <- list("CANOPY","COVER","NOCOVOUT","NOSHBOUT","NOSUMOUT","SHOWSHRB","SHRBLAYR",
                            "SHRUBHT","SHRUBPC","SHRUBS")
          rdkwds <- list("BBCLEAR","BBOUT","BBTYPE1","BBTYPE2","BBTYPE3","BBTYPE4","BORATE","DSNCALC",
                         "INFCOLO","INFKILL","INFMULT","INFSIMS","INOCLIFE","INOCSPAN","PLOTINFO",
                         "PLREAD","PSTUMP","RRCOMP","RRDOUT","RRECHO","RRHOSTS","RRINIT","RRJUMP","RRMINK",
                         "RRTREIN","RRTYPE","RSEED","SAREA","SDIRMULT","SMCOUT","SPORE","SPREAD","STREAD",
                         "TDISTN","TIMEDEAD","TTDMULT","WINDTHR")
          mistkwds <- list("MISTABLE","MISTGMOD","MISTHMOD","MISTMORT","MISTMULT","MISTOFF","MISTPINF",
                           "MISTPREF","MISTPRT")
          extkwds <- c(regenkwds,firekwds,dbkwds,climatekwds,econkwds,coverkwds,rdkwds,mistkwds)
          # DBS-duplicate keywords, where the DBS keyword has fewer paraemters
          dbless <- list("ATRTLIST","CUTLIST","SNAGOUT","STRCLASS","TREELIST")
          # DBS-duplicate keywords, where the DBS keyword has more paraemters
          dbmore <- list("BURNREPT","COMPUTE","DWDCVOUT","DWDVLOUT","FUELREPT","MORTREPT","POTFIRE",
                         "SNAGSUM")
          altless <- c(4,5,5,8,6) # number of parameters the non-DBS keywords have that are exact in name
          altmore <- c(1,2,1,1,1,1,1,1) # number of parameters the non-DBS keywords have that are exact in name 
          dbflag <- 0 # denotes whether we have a DBS-duplicate keyword
          extflag <- 0 # denotes whether we are in an extension block, and which extension by it's value (1-8)
          condflag <- 0 # denotes whether we are in a conditional block
          computeflag <- 0 # denotes whether are in a compute block
          commentflag <- 0 # denoted whether we are in a COMMENT keyword block
          k <- 1 # index counter for the next 2 lsts
          insertkw <- list() # list of which keywords to insert at the end
          insertidx <- list() # list of indices of where to insert those keywords at the end
          numinserts <- 0 # number of keywords to insert
          kcpconts <- (strsplit(cmp$kwds,"\n"))[[1]] # top down list of the addfile
          # loop through each existing line of the addfile
          for(j in 1:length(kcpconts)){
            comment <- strsplit(kcpconts[j],"")[[1]][1]=="*"
            suppcomp <- grep("!!C",kcpconts[j])
            continuation <- strsplit(kcpconts[j],"")[[1]][length(strsplit(kcpconts[j],"")[[1]])]=="&"
            numvalue <- match(strsplit(kcpconts[j],"")[[1]][1],c(1,2,3,4,5,6,7,8,9))
            expression <- grep("=",kcpconts[j])
            ifkw <- toupper(strsplit(kcpconts[j]," ")[[1]][1])=="IF"
            thenkw <- toupper(kcpconts[j])=="THEN"
            endkw <- match("END", toupper(kcpconts[j]))
            endifkw <- match("ENDIF", toupper(kcpconts[j]))
            commkw <- grep("COMMENT", toupper(kcpconts[j]))
            commkw <- length(commkw)
            compkw <- toupper(strsplit(kcpconts[j]," ")[[1]][1])=="COMPUTE"
            # if it's a DBS-duplicate keyword, where the DBS keyword has fewer parameters
            if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbless))){
              test <- strsplit(kcpconts[j]," ")[[1]]
              test[test==""] <- NA
              test <- na.omit(test)
              # if the keyword has less parameters than the other keyword with the same name, it's a DBS keyword
              if(length(test) < altless[match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbless)]){
                dbflag <- 1
              }
              # if the keyword has more parameters than the other keyword with the same name, it's a non-DBS keyword
              if(length(test)>=altless[match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbless)]){
                dbflag <- 2
              }
            }
            # if it's a DBS-duplicate keyword, where the DBS keyword has more parameters
            if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbmore))){
              test <- strsplit(kcpconts[j]," ")[[1]]
              test[test==""] <- NA
              test <- na.omit(test)
              # if the keyword has more parameters than the other keyword with the same name, it's a DBS keyword
              if(length(test) > altmore[match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbmore)]) {
                # if it's an old KCP that has more than 1 additional parameter for FFE reporting keywords, it's actually FFE not DBS
                if (toupper(strsplit(kcpconts[j]," ")[[1]][1])!="COMPUTE" && length(test) > 2) {
                  dbflag <- 2
                } else 
                  {
                dbflag <- 1
                }
              }
              # if the keyword has less parameters than the other keyword with the same name, it's a non-DBS keyword
              if(length(test)<=altmore[match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbmore)]) {
                dbflag <- 2
              }
            }
            # omit comments, lines that continue (supplemental records), parameter-only lines, compute expressions (contains "="), and THEN keywords
            if(is.na(!comment && commentflag==0 && !continuation && is.na(numvalue) && !length(expression) && !thenkw)) next
            if(!comment && commentflag==0 && !continuation && is.na(numvalue) && !length(expression) && !thenkw){
              # if it's a suppose-generated KCP (has "!")
              if(strsplit(kcpconts[j],"")[[1]][1]=="!"){
                # if it's a component specifier and we aren't in a condition block
                if (length(suppcomp) && condflag==0){
                  # if it's specifying that the component is conditionally scheduled, set the flag
                  if(strsplit(kcpconts[j]," ")[[1]][(length(strsplit(kcpconts[j]," ")[[1]])-4)]==3){
                    condflag <- 1
                  }
                  next
                }
                # if it's a component specifier and we are in a condition block
                else if (length(suppcomp) && condflag==1){
                  # if it's not conditionally scheduled, insert an ENDIF in the line above, and reset the flag
                  if(strsplit(kcpconts[j]," ")[[1]][(length(strsplit(kcpconts[j]," ")[[1]])-4)]!=3){
                    insertkw[k] <- "ENDIF"
                    insertidx[k] <- j-1
                    k <- k+1
                    numinserts <- numinserts +1
                    condflag <- 0
                  }
                }
                # Maybe I'll come back to this block. Not required--might be nice to have END inserted where the old timers are used to.
                # If it's a component specifier, for a base keyword and we re already in an extension block
                # else if(length(suppcomp) && extflag > 0 &&
                #         strsplit(kcpconts[j]," ")[[1]][(length(strsplit(kcpconts[j]," ")[[1]])-1)]=="base"){
                #   insertkw[k] <- "END"
                #   insertidx[k] <- j-1
                #   k <- k+1
                #   numinserts <- numinserts +1
                #   extflag <- 0
                # }
                
                # otherwise, ignore the "bam" and move to the next line
                else next
              }
              # if it's an extension invocation keyword and we're not in an extension block,
              # set the flag to the corresponding group number of extension keywords
              else if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),invokekwds)) && extflag==0){
                extflag <- match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),invokekwds)
                next
              }
              # if it's an extension invocation keyword and we are in an extension block
              else if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),invokekwds)) && extflag > 0){
                # if the flag value doesn't correspond to the extension of the invocation keyword
                # insert an END in the line above, and set the flag to the corresponding group number
                # of extension keywords
                if(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),invokekwds)!=extflag){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),invokekwds)
                }
                else next
              }
              # If it's an extension keyword, it's not a DBS-duplicate, we're not already in an extension block
              else if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),extkwds)) && dbflag==0 
                      && extflag==0 && is.na(endkw) && !compkw){
                # if it's a regen keyword, insert ESTAB in the line above and set the flag to 1, etc
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),regenkwds))){
                  invoke <- as.character(invokekwds[1])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 1
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),firekwds))){
                  invoke <- as.character(invokekwds[2])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 2
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbkwds))){
                  invoke <- as.character(invokekwds[3])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 3
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),climatekwds))){
                  invoke <- as.character(invokekwds[4])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 4
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),econkwds))){
                  invoke <- as.character(invokekwds[5])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 5
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),coverkwds))){
                  invoke <- as.character(invokekwds[6])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 6
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),rdkwds))){
                  invoke <- as.character(invokekwds[7])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 7
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),mistkwds))){
                  invoke <- as.character(invokekwds[8])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 8
                  next
                }
              }
              # If it's an extension keyword, we're not already in an extension block, but it is a DBS-duplicate 
              else if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),extkwds)) && dbflag > 0 
                      && extflag==0 && is.na(endkw) && !compkw){
                # if it's a regen keyword, insert ESTAB in the line above and set the flag to 1, etc
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),regenkwds))){
                  invoke <- as.character(invokekwds[1])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 1
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),firekwds))){
                  invoke <- as.character(invokekwds[2])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 2
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbkwds))){
                  if (dbflag==2) {
                    dbflag <- 0
                    next
                  }
                  invoke <- as.character(invokekwds[3])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 3
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),climatekwds))){
                  invoke <- as.character(invokekwds[4])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 4
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),econkwds))){
                  invoke <- as.character(invokekwds[5])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 5
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),coverkwds))){
                  invoke <- as.character(invokekwds[6])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 6
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),rdkwds))){
                  invoke <- as.character(invokekwds[7])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 7
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),mistkwds))){
                  invoke <- as.character(invokekwds[8])
                  insertkw[k] <- invoke
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 8
                  next
                }
                dbflag <- 0
              }
              # If it's an extension keyword, it's not a DBS-duplicate, and we are already in an extension block
              else if (!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),extkwds)) && dbflag==0 
                       && extflag > 0 && is.na(endkw) && !compkw){
                # if it's a regen keyword and the flag doesn't equal 1, insert an END keyword,
                # and then insert ESTAB below that, etc.
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),regenkwds)) && extflag!=1){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[1])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 1
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),firekwds)) && extflag!=2){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[2])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 2
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbkwds)) && extflag!=3){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[3])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 3
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),climatekwds)) && extflag!=4){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[4])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 4
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),econkwds)) && extflag!=5){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[5])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 5
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),coverkwds)) && extflag!=6){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[6])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 6
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),rdkwds)) && extflag!=7){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[7])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 7
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),mistkwds)) && extflag!=8){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[8])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 8
                  next
                }
              }
              # If it's an extension keyword, we are already in an extension block, but it is a DBS-duplicate 
              else if (!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),extkwds)) && dbflag > 0 
                       && extflag > 0 && is.na(endkw) && !compkw){
                # if it's a regen keyword and the flag doesn't equal 1, insert an END keyord,
                # and then insert ESTAB below that, etc.
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),regenkwds)) && extflag!=1){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[1])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 1
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),firekwds)) && extflag!=2){
                  if(dbflag==2 || extflag==3 ){
                    dbflag <- 0
                    next
                  }
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[2])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 2
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbkwds)) && extflag!=3){
                  if (dbflag==2){
                    dbflag <- 0
                    next
                  } 
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[3])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 3
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),climatekwds)) && extflag!=4){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[4])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 4
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),econkwds)) && extflag!=5){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[5])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 5
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),coverkwds)) && extflag!=6){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[6])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 6
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),rdkwds)) && extflag!=7){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[7])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 7
                  next
                }
                if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),mistkwds)) && extflag!=8){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  invoke <- as.character(invokekwds[8])
                  insertkw[k] <- invoke
                  insertidx[k] <- j
                  numinserts <- numinserts +1
                  extflag <- 8
                  next
                }
                dbflag <- 0
              }
              # If it's a base model keyword  but we haven't closed the extension block
              else if (!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),basekwds))&& extflag > 0){
                # insert an END and reset the flag to 0
                insertkw[k] <- "END"
                insertidx[k] <- j-1
                k <- k+1
                numinserts <- numinserts+1
                extflag <- 0
              }
              # if it's an IF keyword
              else if (ifkw){
                # if we weren't already in a conditional block, set the flag to 1 indicating we are now
                if(condflag==0){
                  condflag <- 1
                }
                # if we already were, insert an ENDIF
                else{
                  insertkw[k] <- "ENDIF"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                }
              }
              # if it's a COMMENT keyword
              else if (commkw==1){
                # if we weren't already in a compute block, set the flag to 1 indicating we are now
                if(commentflag==0){
                  commentflag <- 1
                }
                if(extflag > 0){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 0
                }
              }
              # if it's a COMPUTE keyword
              else if (compkw){
                # if we weren't already in a compute block, set the flag to 1 indicating we are now
                if(computeflag==0){
                  computeflag <- 1
                }
                if(extflag > 0){
                  insertkw[k] <- "END"
                  insertidx[k] <- j-1
                  k <- k+1
                  numinserts <- numinserts +1
                  extflag <- 0
                }
              }
              # if it's an END
              else if (!is.na(endkw)){
                if(computeflag==1){
                  computeflag <- 0
                } 
                else if(commentflag > 0){
                  commentflag <- 0
                }
                else if(extflag > 0){
                  extflag <- 0
                }
                else next
              }
              # if it's an ENDIF
              else if (!is.na(endifkw)){
                if(condflag==1){
                  condflag <- 0
                }
                else next
              }
              else next
            }
            # if we're on our last line 
            if (j==length(kcpconts)){
              # merge the needed invocation, END and/or ENDIF keywords at the previously identified indicies 
              if (length(insertkw)){
                n <- 0
                for (l in 1:length(insertkw)){
                  kcpconts <- append(kcpconts,insertkw[[l]][1],after=(insertidx[[l]][1]+n))
                  n <- n+1
                }
              }
              m <- 0
              # also, check to see if an END and/or ENDIF needs added to finish the addfile
              if(computeflag==1){
                k <- k+1
                insertkw[k] <- "END"
                insertidx[k] <- j+numinserts
                kcpconts <- append(kcpconts,insertkw[[k]][1],after=insertidx[[k]][1])
                m <- 1
              }
              if(extflag > 0){
                k <- k+1
                insertkw[k] <- "END"
                insertidx[k] <- j+numinserts
                kcpconts <- append(kcpconts,insertkw[[k]][1],after=insertidx[[k]][1])
                m <- 1
              }
              if(condflag==1 && m==0)kcpconts <- kcpconts <- append(kcpconts,'ENDIF',after=(j+numinserts))
              if(condflag==1 && m==1)kcpconts <- kcpconts <- append(kcpconts,'ENDIF',after=(j+numinserts+1))
            }
          }
          # if any insertions were made to the addfile, replace the cmp$kwds vector with kcpconts
          if (length(kcpconts) > j) cmp$kwds <- paste(kcpconts,collapse="\n")
        }
        if (cmp$atag == "k" && !is.null(lastCnd))
        {
          cat ("EndIf\n",file=fc,sep="")
          lastCnd = NULL
        }
        if (cmp$atag == "c") lastCnd = cmp$uuid
        if (is.na(cmp$exten)) cmp$exten="base" #should not be needed
        exten= if (length(grep("&",cmp$exten,fixed=TRUE)))
          unlist(strsplit(cmp$exten,"&"))[1] else cmp$exten
        if (lastExt != exten && lastExt != "base") 
        {
          lastExt = "base"
          cat ("End\n",file=fc,sep="")
        } 
        naughty <- c("PlantNatural","Econ_reports")
        if (lastExt != exten && !any(!is.na(match(naughty,cmp$kwdName))))
        { 
          cat (extensPrefixes[exten],"\n",file=fc,sep="")
          lastExt = exten
        }
        if (exten == "climate" && substr(cmp$kwds,1,8) == "ClimData")
        {
          scn = unlist(strsplit(cmp$kwds,"\n"))[2]
          qur = paste0("select * from FVS_Climattrs\n"," where Stand_ID = '",
                       std$sid,"' and Scenario = '",scn,"';\n")
          d = dbGetQuery(dbIcon,qur)
          ans = apply(d,2,function (x) !any(is.na(x)))
          d = d[,ans]          
          if (nrow(d)) 
          {
            cat ("ClimData\n",scn,"\n*\n",file=fc,sep="")
            suppressWarnings(write.table(d,file=fc,append=TRUE,col.names=TRUE,
                                         sep=",",quote=FALSE,row.names=FALSE))
            cat ("-999\n",file=fc,sep="")
          }
        } else {
          # remove trailing spaces after last parameter of keywords with supplemental 
          # records entered in the GAAK table (i.e., SPGROUP, MGMTID, etc.)
          if (length(grep("FVS_GroupAddFilesAndKeywords",cmp$title))){
            kwdlist <- strsplit(cmp$kwds,"\n")[[1]]  
            cmpkwds <- length(kwdlist)
            for(i in 1:cmpkwds){
              space <- strsplit(kwdlist[i],"")[[1]][length(strsplit(kwdlist[i],"")[[1]])-1]==" "
              if(space){
                keyrec <- strsplit(kwdlist[i],"")[[1]]
                keyrec <- keyrec[-(length(keyrec)-1)]
                keyrec <- paste0(keyrec, collapse="")
                kwdlist[i] <- keyrec
              }
            }
            kwdlist<- gsub("[\r]", "", kwdlist)
            if(globals$localWindows) kwdlist[grep("FVS_Data.db",kwdlist)] <- paste0(prjDir,"/FVS_Data.db")
            cmp$kwds <- paste(kwdlist,collapse="\n")
          }
          cat ("!Exten:",cmp$exten," Name:",cmp$kwdName,"\n",
                    cmp$kwds,"\n",file=fc,sep="")
        }
      }
    } 
    if (length(std$cmps)) for (cmp in std$cmps)
    {     
      if (cmp$atag == "k" && !is.null(lastCnd))
      {
        cat ("EndIf\n",file=fc,sep="")
        lastCnd = NULL
      }
      if (cmp$atag == "c") lastCnd = cmp$uuid
      exten= if (length(grep("&",cmp$exten,fixed=TRUE)))
             unlist(strsplit(cmp$exten,"&"))[1] else cmp$exten
      if (lastExt != exten && lastExt != "base") 
      {
        lastExt = "base"
        cat ("End\n",file=fc,sep="")
      } 
      if (lastExt != exten)
      {   
          cat (extensPrefixes[exten],"\n",file=fc,sep="")
          lastExt = exten
      }
      cat ("!Exten:",cmp$exten," Name:",cmp$kwdName,"\n",
                     cmp$kwds,"\n",file=fc,sep="")    
    }
    if (!is.null(lastCnd) && lastExt != "base") {
      cat ("End\n",file=fc,sep="")
      lastExt = "base"
    }
    if (!is.null(lastCnd) && lastExt == "base") cat ("EndIf\n",file=fc,sep="")
    if (is.null(lastCnd) && lastExt != "base" && 
        !length(grep("PlantNatural",cmp$kwdName))) cat ("End\n",file=fc,sep="")
    # insert modified sampling weight if needed.
    if (!is.null(wtofix[[std$sid]]))
    {
      swt=as.numeric(fvsInit$SAM_WT[sRows])
      if (is.na(swt)) swt=1
      swt=swt*wtofix[[std$sid]][std$rep]
      cswt=sprintf("%10s",as.character(swt))
      if (nchar(cswt)>10) cswt=sprintf("%9.5g",swt)
      cat ("Design",strrep(" ",53),cswt,"\n",file=fc,sep="")
    } 
    cat ("SPLabel\n",file=fc,sep="")
    for (i in 1:length(std$grps))
    {
      grp = std$grps[[i]]$grp
      cat ("  ",grp,if (i == length(std$grps)) "\n" else 
                        ", & \n",file=fc,sep="")
    }                       
    cat ("Process\n\n",file=fc)    
  }
  cat ("Stop\n",file=fc)    
  close(fc)
cat ("end of writeKeyFile\n")
  return("Keyword file write complete")
}

mkSimCnts <- function (fvsRun,sels=NULL,foundStand=0L,justGrps=FALSE)
{
  tmpcnts = list()
  tmptags = list()                         
  if (!is.null(sels)) if (length(sels) == 0) sels = NULL
  if (!is.null(sels)) if (length(sels) && is.null(sels[[1]])) sels = NULL 
  if (justGrps) 
  {
    start=1
    end=0
    for (grp in fvsRun$grps) 
    {
      end=end+1+length(grp$cmps)
      tmpcnts <- append(tmpcnts,paste(">",grp$grp))
      tmptags <- append(tmptags,grp$uuid)
      if (length(grp$cmps)) for (k in 1:length(grp$cmps))
      {
        tag = switch(grp$cmps[[k]]$atag,
              "c" = "-> Cnd:",
              "k" = "-> Kwd:",
                    "--> Kwd:")
        tmpcnts <- append(tmpcnts,paste(tag,grp$cmps[[k]]$title))
        tmptags <- append(tmptags,grp$cmps[[k]]$uuid)
      }
    }
    if (length(intersect(unlist(fvsRun$selsim),unlist(tmptags))) == 0)
      fvsRun$selsim = tmptags[1]
  } else {
    start = if (length(fvsRun$startDisp)) as.numeric(fvsRun$startDisp) else 1
    if (!is.null(sels) && length(sels) == 1)
    {
      if (substr(sels[[1]],1,1) == "-") 
      {
        start = max((-as.numeric(sels[[1]]))-50,1) 
      } else if (substr(sels[[1]],1,1)== "+") 
      {
        start = min(as.numeric(sels[[1]])+50,length(fvsRun$stands)-49)
        if(start<1) start=1
      }      
    }
    end = min(start+length(fvsRun$stands)-1,start+49) 
    if (foundStand > 0L) 
    {
      if (foundStand < start || foundStand > end) 
      {
        start = foundStand
        end = min(start+length(fvsRun$stands)-1,start+49)
        if (end > length(fvsRun$stands))
        {
          start = max(length(fvsRun$stands)-50,1)
          end = length(fvsRun$stands)
        }
      }
    } 
    fvsRun$startDisp = as.character(start)
cat("mkSimCnts, foundStand=",foundStand," start=",start," end=",end,
  " sels=",if (is.null(sels)) "NULL" else if (is.list(sels)) 
  paste0("length(list)=",length(list)) else sels,"\n")
    if (length(fvsRun$stands)) for (i in start:end) 
    {
      ## these two lines are needed to deal with old runs that may not have these elements in the stand class
      if (class(fvsRun$stands[[i]]$rep  )!="numeric") fvsRun$stands[[i]]$rep  =0
      if (class(fvsRun$stands[[i]]$repwt)!="numeric") fvsRun$stands[[i]]$repwt=1
      # insure "rep" is defined.
      if (length(fvsRun$stands[[i]]$rep)==0) fvsRun$stands[[i]]$rep=0
      tmpcnts<-append(tmpcnts, 
        if (fvsRun$stands[[i]]$rep == 0) fvsRun$stands[[i]]$sid else
            sprintf("%s r%03i %g",fvsRun$stands[[i]]$sid,fvsRun$stands[[i]]$rep,
                    fvsRun$stands[[i]]$repwt))          
      tmptags <- append(tmptags,fvsRun$stands[[i]]$uuid)
      if (length(fvsRun$stands[[i]]$grps))
        for (j in 1:length(fvsRun$stands[[i]]$grps))
        { 
          tmpcnts <- append(tmpcnts,
            paste("> Grp:",fvsRun$stands[[i]]$grps[[j]]$grp))
          tmptags <- append(tmptags,fvsRun$stands[[i]]$grps[[j]]$uuid)
          if (length(fvsRun$stands[[i]]$grps[[j]]$cmps) > 0)
            for (k in 1:length(fvsRun$stands[[i]]$grps[[j]]$cmps))
            {
              tag = switch(fvsRun$stands[[i]]$grps[[j]]$cmps[[k]]$atag,
                    "c" = "-> Cnd:",
                    "k" = "-> Kwd:",
                          "--> Kwd:")
              tmpcnts<-append(tmpcnts, paste(tag,
                  fvsRun$stands[[i]]$grps[[j]]$cmps[[k]]$title))
              tmptags <- append(tmptags,
                 fvsRun$stands[[i]]$grps[[j]]$cmps[[k]]$uuid)
            }
        }
      if (length(fvsRun$stands[[i]]$cmps))
        for (k in 1:length(fvsRun$stands[[i]]$cmps))
        { 
          tag = switch(fvsRun$stands[[i]]$cmps[[k]]$atag,
                "c" = "> Cnd:",
                "k" = "> Kwd:",
                "-> Kwd:")
          tmpcnts<-append(tmpcnts, 
            paste(tag,fvsRun$stands[[i]]$cmps[[k]]$title))
          tmptags <- append(tmptags,fvsRun$stands[[i]]$cmps[[k]]$uuid)
        }
    }
    if (start > 1) 
    {
      tmptags <- append(tmptags,paste0("-",as.character(start)),after=0)
      newstart <- max(start-50,1) 
      if(newstart<1) newstart=1
      newend <-  min(newstart+length(fvsRun$stands)-1,newstart+49)  
      tmpcnts <- append(tmpcnts,paste0("<< Display ",newstart," to ",newend,
                 " of ",length(fvsRun$stands)," stands >>"),after=0)
    }
    if (end < length(fvsRun$stands))
    {
      tmptags <- append(tmptags,paste0("+",as.character(start)))
      newstart <- min(start+50,length(fvsRun$stands)-49)
      newend <-  min(newstart+length(fvsRun$stands)-1,newstart+49)  
      tmpcnts <- append(tmpcnts,paste0("<< Display ",newstart," to ",newend,
                 " of ",length(fvsRun$stands)," stands >>"))
    }
  }
  fvsRun$simcnts <- tmptags
  names(fvsRun$simcnts) <- tmpcnts
  if (is.null(sels) && length(fvsRun$selsim)) 
  {
     names(fvsRun$selsim) <- 
       names(fvsRun$simcnts)[match(fvsRun$selsim,fvsRun$simcnts)]
     if (any(is.na(names(fvsRun$selsim)))) fvsRun$selsim = list()
  }
  if (!is.null(sels) || length(fvsRun$selsim) == 0) 
  {
    selset <- NULL
    tmpcnts <- unlist(tmpcnts) 
    if (!is.null(sels)) 
    { 
      if (length(unlist(sels)) == length(unlist(tmptags)))
      {
        selset=unlist(sels) == unlist(tmptags)
        if (all(!selset)) selset=NULL
      } else selset = match(sels,tmptags)      
    }
    if (length(selset) == 0) selset <- grep(" Grp: All_",tmpcnts)
    if (length(selset) == 0) selset <- grep(" Grp: All",tmpcnts)
    if (length(selset) == 0) selset <- grep(" Grp: FIA_",tmpcnts)
    if (length(selset) == 0) selset <- grep(" Grp: FIA",tmpcnts)
    if (length(selset) == 0) fvsRun$selsim <- list() else
    {
       fvsRun$selsim <- tmptags[selset]
       names(fvsRun$selsim) <- tmpcnts[selset]
    }
  }
cat("...return, loaded=",start," to ",end," of ",length(fvsRun$stands),"\n")
}

findStand <- function(globals,search=NULL)
{
cat ("findStand, search=",search,"\n")
  if (is.null(search) || search=="") 
  {
    globals$foundStand=0L
    return(NULL)
  }
  first=if (globals$foundStand) globals$foundStand+1 else 1
  if (first>length(globals$fvsRun$stands)) first=1
  for (i in first:length(globals$fvsRun$stands))
  {
    if (length(grep(search,globals$fvsRun$stands[[i]]$sid)))
    {
      globals$foundStand=as.integer(i)
      return(globals$fvsRun$stands[[i]]$uuid)
    }
  }
  if (first==1) return(NULL) else first=1
  for (i in first:globals$foundStand)
  {
    if (length(grep(search,globals$fvsRun$stands[i]$sid)))
    {
      globals$foundStand=as.integer(i)
      return(globals$fvsRun$stands[[i]]$uuid)
    }      
  } 
  globals$foundStand=0L
  return(NULL)
}           

nextRunName <- function(FVS_Runs)
{
  i=1
  rs=unlist(FVS_Runs)
  repeat 
  {
    rn=paste0("Run ",length(FVS_Runs)+i) 
    if (rn %in% rs) i=i+1 else break
  }
  rn
}
  
resetActiveFVS <- function(globals)
{
  globals$activeFVS = list(
    FVSak = c("ak", "estb", "dbs", "mist", "fire", "econ", "cover" ), 
    FVSbm = c("bm", "fire", "econ", "strp", "dbs", "climate",  "cover", 
              "mist", "armwrd3", "phewrd3", "ardwrd3"), 
    FVSca = c("ca",  "fire", "econ", "strp", "dbs", "mist", "climate", "cover"), 
    FVSci = c("ci",  "fire", "econ", "estb", "dbs", "climate", "cover", "mist", 
              "fire",  "armwrd3", "phewrd3", "ardwrd3"), 
    FVScr = c("cr", "fire", "econ",  "strp", "dbs", "climate", "mist", "cover", 
              "armwrd3", "phewrd3",  "ardwrd3"), 
    FVScs = c("cs", "strp", "dbs", "fire", "econ"), 
    FVSec = c("ec",  "strp", "dbs", "cover", "mist", "fire", 
              "climate", "econ", "phewrd3",  "armwrd3", "ardwrd3"), 
    FVSem = c("em", "estb", "dbs", "cover",  "mist", "fire", "climate", 
              "econ", "phewrd3", "armwrd3", "ardwrd3" ), 
    FVSie = c("ie", "estb", "dbs", "cover", "mist", "fire", "climate",  
              "econ", "armwrd3", "phewrd3", "ardwrd3"), 
    FVSkt = c("kt", "estb",  "dbs", "cover", "mist", "fire", "climate", 
              "econ", "phewrd3",  "armwrd3", "ardwrd3"), 
    FVSls = c("ls", "dbs", "strp", "fire",  "econ"), 
    FVSnc = c("nc", "strp", "dbs", "cover", "mist", "fire",  "econ", 
              "climate", "phewrd3", "armwrd3", "ardwrd3"), 
    FVSne = c("ne",  "dbs", "strp", "fire", "econ"), 
    FVSoc = c("oc", "fire", "econ",  "strp", "dbs", "mist", 
              "climate", "cover", "organon"), 
    FVSop = c("op",  "strp", "dbs", "mist", "fire", "econ", "climate", 
              "cover", "armwrd3",  "phewrd3", "ardwrd3", "organon"), 
    FVSpn = c("pn", "strp", "dbs",  "mist", "fire", "econ", "climate", 
              "cover", "armwrd3", "phewrd3",  "ardwrd3"), 
    FVSsn = c("sn", "dbs", "strp", "fire", "econ"), 
    FVSso = c("so",  "strp", "dbs", "cover", "mist", "fire", "econ", 
              "climate", "phewrd3",  "armwrd3", "ardwrd3"), 
    FVStt = c("tt", "fire", "strp", "dbs",  "cover", "mist", "climate", 
              "econ", "phewrd3", "armwrd3", "ardwrd3" ), 
    FVSut = c("ut", "strp", "dbs", "cover", "mist", "fire", "climate",  
              "econ", "phewrd3", "armwrd3", "ardwrd3"), 
    FVSwc = c("wc", "strp",  "dbs", "mist", "fire", "climate", "econ", 
              "cover", "phewrd3",  "armwrd3", "ardwrd3"), 
    FVSws = c("ws", "strp", "dbs", "cover",  "mist", "fire", "climate", 
              "econ", "phewrd3", "armwrd3", "ardwrd3" )) 
  shlibsufx <- if (.Platform$OS.type == "windows") "[.]dll$" else "[.]so$"
  binDir = if (file.exists("FVSbin/")) "FVSbin/" else fvsBinDir
  avalFVS <- dir(binDir,pattern=shlibsufx)
  avalFVS <- sub(shlibsufx,"",avalFVS)
  if (length(avalFVS)) globals$activeFVS = globals$activeFVS[avalFVS] 
  globals$activeVariants <- unlist(lapply(globals$activeFVS, function(x) x[1]))
  vars = c("ak: Southeast AK - Coastal BC"="ak",
           "bm: Blue Mountains,Oregon"="bm",
           "ca: Inland CA,Southern Cascades"="ca",
           "ci: Central ID"="ci",
           "cr: Central Rockies GENGYM"="cr",
           "sw: GENGYM: Southwest Mixed Conifers"="sw",
           "sp: GENGYM: Southwest Ponderosa Pine"="sp",
           "bp: GENGYM: Black Hills Ponderosa Pine"="bp",
           "sf: GENGYM: Spruce-fir"="sf",
           "lp: GENGYM: Lodgepole pine"="lp",
           "ec: East Cascades,Washington"="ec",
           "em: Eastern Montana"="em",
           "ie: Inland Empire"="ie",
           "nc: Klammath Mountains,Northern CA"="nc",
           "oc: ORGANON SWO - FVSca hybrid"="oc",
           "op: ORGANON NWO/SMC - FVSpn hybrid"="op",
           "so: South Central OR N CA"="so",
           "tt: Tetons,Wyoming"="tt",
           "ut: Utah"="ut",                        
           "wc: West Cascades"="wc",
           "pn: Pacific Northwest Coast"="pn",     
           "ws: Western Sierra Nevada,CA"="ws",
           "cs: Central States"="cs",
           "kt: Kootenai/Kaniksu/Tally LK,ID - MT"="kt",
           "ls: Lake States"="ls",
           "ne: Northeast"="ne",
           "sn: Southern"="sn")
  keep=match(globals$activeVariants,vars)
  globals$activeVariants = if (length(keep) && !is.na(keep)) vars[keep] else character(0)
  globals$activeExtens=character(0)
cat ("in resetActiveFVS, globals$activeVariants=",globals$activeVariants,"\n")
}

resetGlobals <- function(globals,fvsRun,prms)
{
cat("resetGlobals, fvsRun NULL=",is.null(fvsRun),"\n")
  resetActiveFVS(globals)
  globals$schedBoxYrLastUsed=character(0)
  globals$currentEditCmp=globals$NULLfvsCmp
  if (!is.null(fvsRun))                                        
  {
    globals$schedBoxYrLastUsed=fvsRun$startyr                  
    if (length(fvsRun$FVSpgm) > 0)            
    {                                                            
      indx = match(fvsRun$FVSpgm,names(globals$activeFVS))
      if (length(indx) && !is.na(indx))                         
      {
        fvsRun$FVSpgm = names(globals$activeFVS)[indx]
        globals$activeFVS <- globals$activeFVS[indx]
        globals$activeVariants <- subset(globals$activeVariants,
                globals$activeVariants==globals$activeFVS[[1]][1])
        globals$activeExtens <- c("base",globals$activeFVS[[1]][-1])
        globals$lastRunVar <- globals$activeVariants
      }                                       
    }
  }
cat ("globals$activeVariants=",globals$activeVariants,"\n")
cat ("activeFVS=",length(globals$activeFVS),"\n")
if (length(globals$activeFVS)) cat ("names(globals$activeFVS)=",names(globals$activeFVS),"\n")
cat ("reset activeExtens= ");lapply(globals$activeExtens,cat," ");cat("\n")
  globals$extnsel <- character(0)
  globals$mgmtsel <- list()
  globals$mmodsel <- list()
  globals$mevsel <- list()
  globals$schedBoxPkey <- character(0)  
  globals$currentCmdPkey <- character(0)  
  globals$currentCndPkey <- character(0)  
  globals$winBuildFunction <- character(0)
  globals$foundStand <- 0L 
  globals$changeind <- 0
}


updateVarSelection <- function ()
{
  if (length(globals$fvsRun$FVSpgm) == 0) 
  {
    vlst = as.list(globals$activeVariants)
    selected = if (length(globals$lastRunVar)) globals$lastRunVar else globals$activeVariants[1]
  } else {
    if (is.null(globals$activeFVS[[globals$fvsRun$FVSpgm]])) 
    {
      vlst <- list()
      selected = NULL
    } else {
      vlst <- globals$activeFVS[globals$fvsRun$FVSpgm][[1]][1]
      vlst <- globals$activeVariants[match(vlst,globals$activeVariants)]
      vlst <- if(is.null(vlst[[1]])) as.list(globals$activeVariants) else as.list(vlst)
      selected = if(is.null(vlst[[1]])) unlist(globals$activeVariants)[1] else unlist(vlst)[1]
      if (!is.null(selected)) globals$lastRunVar=selected
    }
  }
  if (is.null(selected)) 
  {
    globals$activeVariants = character(0) 
    globals$activeExtens = "base"
  } else {
    globals$activeVariants = unlist(vlst)
    globals$activeExtens = c("base",globals$activeFVS[[paste0("FVS",selected)]][-1])
  }
cat ("in updateVarSelection selected=",selected," globals$lastRunVar=",globals$lastRunVar," vlst=",unlist(vlst),"\n")
  updateSelectInput(session=session, inputId="inVars", choices=vlst,
                    selected=selected)
} 


uuidgen <- function (n=1)
{
# generate a version 4 uuid using R's random number generator and a
# special seed/status.
# example: "36d3054f-553b-4f52-ac3f-b1a028f3dfa8"

# designed to make it very difficult to cause this generator
# to generate a duplicate...even when the user first calls set.seed
# and does not have package digest.

  ss <- if (exists(".Random.seed",envir=.GlobalEnv,inherit=FALSE)) 
        get(".Random.seed",envir=.GlobalEnv) else NULL
  cp <- Sys.getpid()
  if (exists(".uuid.seedpid",envir=.GlobalEnv,inherit=FALSE) &&
         get(".uuid.seedpid",envir=.GlobalEnv) == cp &&
      exists(".uuid.seed",   envir=.GlobalEnv,inherit=FALSE))
    .Random.seed <<- get(".uuid.seed",envir=.GlobalEnv) else 
  {
    .uuid.seedpid <<- cp
    if (file.exists("/dev/random")) {
      rn <- file ("/dev/random",open="rb",raw=TRUE)
      set.seed(readBin(rn,"integer"))
      close(rn)
    } else 
    {
      if (require (digest,quietly=TRUE)) { 
        i <- as.integer(runif(1,min=1,max=32-7))
        dig <- paste0("0x",substring(digest(Sys.getenv()),i,i+6))
        dig <- strtoi(dig) * if (strtoi(substring(dig,1,3)) %% 2) 1 else -1
      } else dig <- 1
      set.seed(as.integer(Sys.time())+cp+as.integer(runif(1)*1000)+dig)
    }
  }   
  uuid <- vector("character",n)
  for (i in 1:n) {
    rnum = runif(4)
    rstr <- substring(sprintf("%.8a",rnum),5,12)
    uuid[i] <- sprintf("%s-%s-4%s%s%s-%s%s",rstr[1],substring(rstr[2],1,4),
               substring(rstr[2],5,7),
               c("-8","-9","-a","-b","-8")[as.integer(rnum[1]*4)+1],
               substring(rstr[3],1,3),substring(rstr[3],4,7),rstr[4])
  }  
  .uuid.seed <<- .Random.seed
  #if the seed did not exist upon function call, then remove it, otherwise
  #restore the seed to the original value.
  if (is.null(ss)) rm(.Random.seed,envir=.GlobalEnv) else .Random.seed <<- ss
  uuid
}


getPstring = function (pkeys,pkey,atag = NULL)
{
  pkeys = pkeys[pkeys == pkey]
  if (length(pkeys) == 0) return (NULL)
  if (is.null(atag)) 
  {
    for (pk in pkeys)
    {
      if (is.null(attr(pk,"atlist"))) 
      { 
        ps = attr(pk,"pstring")
        names(ps) = as.character(pk)
        return(ps)
      }
    }
  } else {
    for (pk in pkeys)
    {
      if (!is.null(attr(pk,"atlist")) &&
          length(grep (atag,attr(pk,"atlist"))) > 0)       
      { 
        ps = attr(pk,"pstring")
        names(ps) = as.character(pk)
        return(ps)
      }
    }
    return (getPstring(pkeys,pkey,NULL))
  }
  return (NULL)
}


mkKeyWrd = function (ansFrm,input,pkeys,variant)
{
cat("mkKeyWrd, ansFrm=\n",ansFrm,"\ninput=",input,"\n")
  state=0
  out = NULL
  for (i in 1:length(input)){
    if(!is.null(input) && input[i]==" ") next
    if(length(grep(" ", input[i]))) input[i] <- trim(input[i])
  }
  for (i in 1:nchar(ansFrm))
  {
    c = substr(ansFrm,i,i)
    if (state==0) # looking for first !
    {
      if (c != "!") out = paste0(out,c) else
      {
        fldw = ""
        fld = ""
        state = 1
        fs = i+1
      }
      next
    }
    if (state==1) # looking for the end of the field number
    {
      if (c == "," || c == "!") fld = as.numeric(substr(ansFrm,fs,i-1))
      if (c == "!")
      {
        inp = if (length(input) < fld) "" else input[fld]
        out = paste0(out,inp)
        state = 0 
        next
      }
      if (c == ",") # start looking for a field width
      {
        state = 2 
        fs = i+1
        next
      }
    }
    if (state == 2) # looking for end of field width
    {
      if (c == "," || c == "!") 
      {
        fldw = substr(ansFrm,fs,i-1)
        if (nchar(fldw) > 0) fldw = paste0("%",fldw,".",sub("-","",fldw),"s")
      }
      if (c == "!") 
      {
        inp = if (length(input) < fld) "" else input[fld]
        out = if (nchar(fldw) > 0) paste0(out,sprintf(fldw,inp)) else
              paste0(out,input[fld]) 
        state = 0 
        next
      }
      if (c == ",") # start looking for a value translator
      {
        state = 3 
        fs = i+1
        next
      }
    }
    if (state == 3) # looking for value translator token
    {
      if (c == "!") 
      {
        trns = substr(ansFrm,fs,i-1)
        trns = getPstring(pkeys,trns,variant)
        trns = scan(text=trns,what="character",quiet=TRUE)
        inp = if (length(input) < fld) NA else 
            switch (input[fld],
              "FALSE" = 0, 
              "TRUE"  = 1,
              as.numeric(input[fld]))
        fld = if (is.na(inp)) "*error*" else 
          if (inp<=0) trns[1] else if (inp>length(trns)) trns[length(trns)] else 
             trns[inp+1]
        out = if (nchar(fldw) > 0) paste0(out,sprintf(fldw,fld)) else
              paste0(out,fld)
        state = 0
        next
      }
    }
  }
  out <- trim(out)
cat ("mkKeyWrd, out=",out,"\n")
  out
}
  

mkMgmtCats <- function(globals)
{
  globals$activeExtens = c("base",globals$activeFVS[[1]]) 
  catsel=list(
    "Planting & Natural Regeneration"=
    if ("estb" %in% globals$activeExtens) 
      c("Plant/Natural with Full Estab Model" = "PlantNaturalFullWin") else
      c("Plant/Natural with Partial Estab Model" = "PlantNaturalPartialWin"),
  "Regeneration Methods: Even-aged"=c(
    "Clearcut/Coppice" = "ClearcutWin",
    "Seedtree" = "SeedTreeWin",
    "Shelterwood" = "ShelterwoodWin"),
  "Regeneration Methods: Uneven-aged"=c(
    "Thin to a Q-factor" = "uneven-aged_Q",
    "Group Selection, Distance-independent" = "uneven-aged_grp_select"),
  "Thinning & Pruning Operations"=c(
    "Thin from below" = "ThinFromBelowWin",
    "Thin from above" = "ThinFromAboveWin",
    "Thin throughout a diameter range" = "ThinThroughout",
    "Thin points" = "ThinPoints",
    "Thin individually \"marked\" trees" = "ThinInd",
    "Thin from a specific height range" = "keyword.base.ThinHt",
    "Thin from below w/ species retention" = "species_retention",
    "Thin dwarf mistltoe infected trees" = "keyword.base.ThinMist",
    "Thin to a residual percent canopy cover" = "keyword.base.ThinCC",
    "Thin to a residual stand density index" = "keyword.base.ThinSDI",
    "Thin to a residual relative density (Curtis' RD)" = "keyword.base.ThinRDen",
    "Thin to a residual relative density (Silvah RD)" = "keyword.base.ThinRDSL",
    "Mechanical thinning" = "MechThin",
    "Prune" = "keyword.base.Prune"),
  "Cleaning & Release Operations"=c(
    "Cut dwarf mistletoe infected trees" = "keyword.base.ThinMist",
    "Clean or release by cutting" = "Release",
    "Clean or release by girdling/chemical treatment" = "Girdling"),
  "Tree Removal Preference"=
    if ("mist" %in% globals$activeExtens) 
    c("Exclude or include a species from harvest" = "keyword.base.SpLeave",
    "Removal pref by tree value classes" = "keyword.base.TCondMLT",
    "Removal pref by species" = "keyword.base.SpecPref",
    "Removal pref by mistletoe rating" = "mist keyword.mist.MistPref") else
    c("Exclude or include a species from harvest" = "keyword.base.SpLeave",
    "Removal pref by tree value classes" = "keyword.base.TCondMLT",
    "Removal pref by species" = "keyword.base.SpecPref"),
  "Biomass Removal and Retention"=c(
    "Basic: Manage logging slash" = "yardloss_options",
    "Advanced: Full set of yarding options" = "keyword.base.YardLoss"))
  if ("fire" %in% globals$activeExtens) catsel = append(catsel,list(
  "Fuel Treatments"=c(
    "Thin from below" = "ffe_thin",
    "Thin a species across a dbh range" = "fueltrt_thindbh",
    "Thin with fuel piled and burned" = "thin_pileburn_new",
    "Mastication" = "mastication",
    "Prescribed burn" = "fire ffe_rxburn",
    "Pile burn surface fuel" = "pileBurn_options"),
  "Salvage Operations"=c(
    "Cut dead trees" = "fire salvage_options")))
  catsel = append(catsel,list(
  "Identify Groups"=c(
    "Species to target" = "CutGroup",
    "General species group" = "GenGroup",
    "Create a group of points" = "keyword.base.PointGrp"),
  "Fertilizer"=c(
    "Fertilize" = "keyword.base.Fertiliz")))
  catsel = append(catsel,list(
  "Disease Management"=c(
    "Thin dwarf mistltoe infected trees" = "keyword.base.ThinMist",
    if ("armwrd3" %in% globals$activeExtens) c(
    "Push stumps (Western Root Disease)" = "keyword.armwrd3.PStump",
    "Borax stumps (Western Root Disease)" = "keyword.ardwrd3.Borate"))))
  catsel
} 

mkModMCats <- function(globals)
{
  catsel=list(
    "Volume Modifiers" = c(
      "Adjust merchantability limits" = "merch_vol",
      "Add Volume Defect" = "keyword.base.Defect"),
    "Mortality Modifiers" = c(
      "Adjust mortality rates" = "keyword.base.FixMort",
      "Adjust maximum SDI" = "keyword.base.SDIMax",
      "Adjust maximum basal area" = "keyword.base.BAMax",
      "Set maximum tree size" = "keyword.base.TreeSzCp"),
    "Diameter Growth Modifiers" = c(
      "Pre-calibration, multiplier for large tree dbh growth" = "keyword.base.ReadCorD",
      "Adjust large tree basal area increment" = "keyword.base.BAIMult",
      "Fix diameter growth rates" = "keyword.base.FixDG",
      "Adjust small tree diameter growth" = "keyword.base.RegDMult"),
    "Height Growth Modifiers" = c(
      "Pre-calibration, multiplier for small tree height growth" = "keyword.base.ReadCorR",
      "Turn \"on\" or \"off\" use of local Height-DBH parameters" = "keyword.base.NoHtDReg",
      "Adjust large tree height growth" = "keyword.base.HtgMult",
      "Adjust small tree height model" = "keyword.base.RegHMult",
      "Fix height growth rates" = "keyword.base.FixHtG"),
    "Crown Modifiers" = c(
      "Adjust crown ratio estimates" = "keyword.base.CrnMult",
      "Adjust crown width equation" = "keyword.base.CWEqn",
      "Fix crown width estimates" = "keyword.base.FixCW"))
  if ("fire" %in% globals$activeExtens) catsel = append(catsel,list(
    "Modify Potential Fire Conditions" = c(   
      "Set fuel moistures for potential fires" = "keyword.fire.PotFMois",
      "Set wind speed for potential fires" = "keyword.fire.PotFWind",
      "Set temperature for potential fires" = "keyword.fire.PotFTemp",
      "Set % of the stand area burned for potential fires" = "keyword.fire.PotFPAB",
      "Set season of potential fires" = "keyword.fire.PotFSeas"),
    "Modify Fuel Dynamics" = c(
      "Set initial surface fuel loadings" = "fire fuelinitfuelsoft",
      "Set surface fuels with a photoseries photo" = "keyword.fire.FuelFoto",
      "Set surface fuel decay rates" = "keyword.fire.FuelDcay",
      "Adjust surface fuel decay rates" = "keyword.fire.FuelMult",
      "Specify the decay rate class for a species" = "keyword.fire.FuelPool",
      "Set prop. of decayed fuel that becomes duff" = "keyword.fire.DuffProd"),         
    "Modify Snag Dynamics" = c(
      "Adjust snag fall rate" = "keyword.fire.SnagFall",
      "Adjust snag decay rate" = "keyword.fire.SnagDCay",
      "Set snag height loss" = "keyword.fire.SnagBrk",
      "Set post-burn snag fall rates" = "keyword.fire.SnagPBN",
      "Set prop. of snags considered soft" = "keyword.fire.SnagPSft"),
    "Modify Fire Behavior" = c(
      "Set fuel model(s)" = "keyword.fire.FuelModl",
      "Set fuel moistures for simulated fires" = "keyword.fire.Moisture",
      "Modify calculation of canopy fuels" = "keyword.fire.CanCalc",
      "Set the flame length of simulated fires" = "keyword.fire.FlameAdj",
      "Define or modify a fuel model" = "keyword.fire.DeFulMod",
      "Specify drought years" = "keyword.fire.Drought",
      "Select the \"new\" fuel model logic or modelled loads option" = "keyword.fire.FireCalc",
      "Adjust the fuel models with the \"new\" fuel model logic" = "keyword.fire.FModList")))
  catsel = append(catsel,list(  
    "Modify Time Scale" = c(
      "Create cycle boundary" = "keyword.base.CycleAt",
      "Set time interval for a cycle" = "keyword.base.TimeInt",
      "Set the number of cycles" = "keyword.base.NumCycle",
      "Set inventory year" = "keyword.base.InvYear")))
  if ("mist" %in% globals$activeExtens) catsel = append(catsel,list(   
    "Modify Dwarf Mistletoe Impact Model" = c(
      "Turn off DM impacts" = "keyword.mist.MistOff", 
      "Add DM infections" = "keyword.mist.MistPInf",
      "Modify DM spread and intensification" = "keyword.mist.MistMult",
      "Modify DM diameter growth impacts" = "keyword.mist.MistGMod",
      "Modify DM height growth impacts" = "keyword.mist.MistHMod",
      "Modify DM mortality impacts" = "keyword.mist.MistMort")))
  if (length(intersect(c("ardwrd3","armwrd3","phewrd3"),globals$activeExtens))) 
    catsel = append(catsel,list("Modify Western Root Disease Model" = c(
      "Modify Disease Host Species" = "keyword.ardwrd3.RRHosts",
      "Initialize Root Disease" = "wrd_initialization",
      "Modify Root Disease behavior" = "wrd_behavior",
      "Modify Root Disease bark beetles" = "wrd_brk_btl")))
  catsel = append(catsel,list(   
    "Modify Sprouting" = c(
      "Turn off Sprouting" = "keyword.estbstrp.NoSprout",
      "Adjust Sprouting" = "keyword.estbstrp.Sprout"),
    "Modify Percent Canopy Cover" = c( 
      "Adjust Overlap Correction" = "keyword.base.CCAdj")))
  catsel
} 
 
mkEvMonCats <- function(globals)
{
  catsel=list(
    "Event Monitor (EM) Compute Variables" = c(
      "Compute Pre-Defined Stand Variables" = "Compute_PreDefined",
      "Compute Stand Variables with SpMcDBH Function" = "Compute_SpMcDBH",
      "Compute Snag Variables with Snags Function" = "Compute_Snags",
      "Compute Surface Fuel Loading with FuelLoad Function" = "Compute_FuelLoad",
      "Compute WRENSS Variables" = "Compute_WRENSS",
      "Compute Stand Variables in Editor" = "keyword.base.Compute"))
  catsel
}

mkextkwd <- function(prms,globals)
{
cat("mkextkwd\n")
  extnslist <- c(
    "Base FVS system" = "base", 
    "Cover Model" = "cover", 
    "Full Establishment Model" = "estb", 
    "Partial Establishment Model" = "strp", 
    "Database Extension" = "dbs", 
    "Economic Analysis Extension" = "econ",
    "Dwarf Mistletoe Impact Model" = "mist", 
    "ORGANON in FVS" = "organon", 
    "Fire and Fuels Extension" = "fire",
    "Climate-FVS Extension" = "climate", 
    "WRD (Annosus Root Disease)" = "ardwrd3", 
    "WRD (Armillaria Root Disease)" = "armwrd3", 
    "WRD (Laminated Root Rot)" = "phewrd3")
  globals$extnsel <- na.omit(extnslist[match(globals$activeExtens,extnslist)])
  kwds <- prms[["keyword_list"]]  
  globals$kwdsel <- vector("list",length(globals$extnsel))
  names(globals$kwdsel) = globals$extnsel
  for (kwd in kwds)
  {
    ex <- attr(kwd,"atlist")
    ex <- if (length(ex) > 1 && ex[1] == "estb" && ex[2] == "strp") 
          "estbstrp" else ex[1] 
    keypn <- paste0("keyword.",ex,".",kwd)
    sk <- match(keypn,names(prms))
    if (is.na(sk)) next
    if (ex == "estbstrp") ex = if("estb" %in% globals$extnsel) "estb" else "strp" 
    entry <- paste(ex,keypn)
    names(entry) <- paste0(kwd,": ",attr(kwd,"pstring"))
    globals$kwdsel[[ex]] <- append(globals$kwdsel[[ex]],entry)
  }
}


moveToPaste <- function(item,globals,fvsRun,atag=NULL)
{
  # iterate from the bottom to the top in all cases.
  # remove a stand
  if (!is.null(item) && length(fvsRun$stands)) for (i in length(fvsRun$stands):1)
  {
    if (fvsRun$stands[[i]]$uuid == item) 
    {
      # if stand has components, add those components to the paste list.
      if (length(fvsRun$stands[[i]]$cmps))
      {
        for(toRm in fvsRun$stands[[i]]$cmps)
        { 
          globals$pastelist <- append(globals$pastelist,toRm,after=0)
          globals$pastelistShadow <- append(globals$pastelistShadow,
            toRm$uuid,after=0)
          names(globals$pastelistShadow)[1] = toRm$title
        }
      }
      fvsRun$stands[[i]] = NULL
      return(TRUE)
    }
  }
  # remove a group
  if (!is.null(item) && length(fvsRun$grps)) for (i in length(fvsRun$grps):1)
  {
    if (fvsRun$grps[[i]]$uuid == item) 
    {
      # if group has components, add them to the paste list.
      if (length(fvsRun$grps[[i]]$cmps))
      {
        for(toRm in fvsRun$grps[[i]]$cmps)
        { 
          globals$pastelist <- append(globals$pastelist,toRm,after=0)
          globals$pastelistShadow <- append(globals$pastelistShadow,
            toRm$uuid,after=0)
          names(globals$pastelistShadow)[1] = toRm$title
        }
      }
      fvsRun$grps[[i]] = NULL
      # now go through all the stands and remove the group from the 
      # stands which are members of the group.
      if (length(fvsRun$stands)) for (j in length(fvsRun$stands):1)
        if (length(fvsRun$stands[[j]]$grps)) 
            for (k in length(fvsRun$stands[[j]]$grps):1) 
              if (fvsRun$stands[[j]]$grps[[k]]$uuid == item) 
                 fvsRun$stands[[j]]$grps[[k]] = NULL 
      return(TRUE)
    }
  }
  cntr <- 0
  # remove a component from a grp... 
  if (length(fvsRun$grps)) for (i in length(fvsRun$grps):1)
  {
    if (length(fvsRun$grps[[i]]$cmps)) 
    {
      for (j in length(fvsRun$grps[[i]]$cmps):1)
      {
          spgtest <- grep("^SpGroup",fvsRun$grps[[i]]$cmps[[j]]$kwds)
          if (length(spgtest) && cntr == 0){
            globals$GrpNum <- globals$GrpNum[-(length(globals$GrpNum))]
            globals$GenGrp<- globals$GenGrp[-(length(globals$GenGrp))]
            cntr <- cntr +1
            }
        if ((!is.null(item) && fvsRun$grps[[i]]$cmps[[j]]$uuid == item) || 
            (!is.null(atag) && fvsRun$grps[[i]]$cmps[[j]]$atag == atag))
        {
          toRm = fvsRun$grps[[i]]$cmps[[j]]
          globals$pastelist <- append(globals$pastelist,toRm,after=0)
          globals$pastelistShadow <- append(globals$pastelistShadow,toRm$uuid,after=0)
          names(globals$pastelistShadow)[1] = toRm$title
          fvsRun$grps[[i]]$cmps[[j]] = NULL
          if (toRm$atag == "c") repeat 
            if (!moveToPaste(NULL,globals,fvsRun,atag=toRm$uuid)) break
          return(TRUE)
        }
      }
    }
  }
  # remove a component from a stand...
  if (length(fvsRun$stands)) for (i in length(fvsRun$stands):1)
  {
    if (length(fvsRun$stands[[i]]$cmps)) 
      for (j in length(fvsRun$stands[[i]]$cmps):1)
      {
        if ((!is.null(item) && fvsRun$stands[[i]]$cmps[[j]]$uuid == item) || 
            (!is.null(atag) && fvsRun$stands[[i]]$cmps[[j]]$atag == atag))
        {
          toRm = fvsRun$stands[[i]]$cmps[[j]]
          globals$pastelist <- append(globals$pastelist,toRm,after=0)
          globals$pastelistShadow <- append(globals$pastelistShadow,toRm$uuid,after=0)
          names(globals$pastelistShadow)[1] = toRm$title
          fvsRun$stands[[i]]$cmps[[j]] = NULL
          if (toRm$atag == "c") repeat 
            if (!moveToPaste(NULL,globals,fvsRun,atag=toRm$uuid)) break
          return(TRUE)
        }
      }
  }
  FALSE
}
      
pasteComponent <- function(fvsRun,sel,topaste)
{
  # search for selected element in grps...attach to grp cmps.
  idx = findIdx(fvsRun$grps,sel)
  if (!is.null(idx)) 
  {
    fvsRun$grps[[idx]]$cmps <- append(fvsRun$grps[[idx]]$cmps,topaste)
    # test to see if the SpGroup needs to be added back to the species dropdown lists after pasting
    spgtest <- grep("^SpGroup",globals$pastelist[[1]]$kwds)
    if (length(spgtest)){
      test <- globals$pastelist[[1]]$kwds
      spgname <- list()
      spgname<- trim(unlist(strsplit(strsplit(test, split = "\n")[[1]][1],
                split=" "))[length(unlist(strsplit(strsplit(test, split = "\n")[[1]][1],split=" ")))])
      if(!length(globals$GrpNum)) globals$GrpNum[1] <- 1 else
        globals$GrpNum[(length(globals$GrpNum)+1)] <- length(globals$GrpNum)+1
      spgname <- gsub(" ","", spgname)
      tmpk <- match(spgname, globals$GenGrp)
      if (!is.na(tmpk)) globals$GrpNum <- globals$GrpNum[-length(globals$GrpNum)] else 
        globals$GenGrp[length(globals$GrpNum)]<-spgname
    }
    return(idx)
  } 
  if (is.null(idx))
  {
  # search for selected element in stands...attach to cmps.
    idx = findIdx(fvsRun$stands,sel)
    if (!is.null(idx))
    {
      fvsRun$stands[[idx]]$cmps <- append(fvsRun$stands[[idx]]$cmps,topaste)
      return(idx)
    }
  }
  if (is.null(idx))
  {
    # selected element is a component. Find the grp in which it is listed.
    for (grp in fvsRun$grps)
    {
cat("pasteComponent finding cmp in groups, sel=",sel,"\n")     
      idx = findIdx(grp$cmps,sel)
      if (!is.null(idx)) 
      {
        cmp = grp$cmps[[idx]]
        if (cmp$atag == "c" && nchar(topaste$atag) > 1) 
          topaste$atag = cmp$uuid
        grp$cmps <- append(grp$cmps,topaste,after=idx)
        return(idx)
      }
    }
  }
  if (is.null(idx))
  {
    # search for selected component in stands...attach to std cmps.
cat("pasteComponent finding cmp in stands, sel=",sel,"\n")     
    for (std in fvsRun$stands)
    {
      idx = findIdx(std$cmps,sel)
      if (!is.null(idx)) 
      {
        cmp = std$cmps[[idx]]
        if (cmp$atag == "c" && nchar(topaste$atag) > 1) 
          topaste$atag = cmp$uuid
        std$cmps <- append(std$cmps,topaste,after=idx)
        return(idx)
      }
    }
  }
}



deleteRelatedDBRows <- function(runuuid,dbcon)
{
  tbs <- dbGetQuery(dbcon,"select name from sqlite_master where type='table';")[,1]
  if (! ("FVS_Cases" %in% tbs)) return()
  tmpDel = paste0("tmp",gsub("-","",runuuid),Sys.getpid())
  qry = paste0("attach database ':memory:' as ",tmpDel)
cat ("qry=",qry,"\n") 
  dbExecute(dbcon,qry)
  todel = paste0(tmpDel,".todel")
  qry = paste0("drop table if exists ",todel)
cat ("qry=",qry,"\n") 
  dbExecute(dbcon,qry)      
  qry = paste0("create table ",todel,
     " as select CaseID from FVS_Cases where KeywordFile = '",runuuid,"'")
cat ("qry=",qry,"\n") 
  dbExecute(dbcon,qry) 
  nr = dbGetQuery(dbcon,paste0("select count(*) from ",todel))[,1]
cat ("ncases to delete=",nr,"\n")
  if (nr)
  {
    for (tb in tbs)
    {
      if ("CaseID" %in% dbListFields(dbcon,name=tb))
      { 
        qry = paste0("delete from ",tb," where ",tb,".CaseID in (",
                     "select CaseID from ",todel,")")
        rtn = dbExecute(dbcon,qry)
cat ("rtn=",rtn," qry=",qry,"\n")                      
        nr = nrow(dbGetQuery(dbcon,paste0("select CaseID from ",tb," limit 1")))
cat ("nr=",nr,"\n")                      
        if (nr == 0) 
        {
          qry = paste0("drop table ",tb)
          rtn = dbExecute(dbcon,qry)
cat ("no rows in table after delete rtn=",rtn," qry=",qry,"\n")                      
        }
      } else {
        qry = paste0("drop table ",tb)
        rtn = dbExecute(dbcon,qry)
cat ("caseID not in table, rtn=",rtn," qry=",qry,"\n")                      
      }
    }
  }
  dbExecute(dbcon,paste0("detach database '",tmpDel,"'"))
}

mkDBIndices <- function(dbcon)
{
  tabs = dbGetQuery(dbcon,"select * from sqlite_master")[,1:2]
  if (nrow(tabs)==0) return()
  fvs=grep("FVS_",tabs$name,fixed=TRUE)
  if (length(fvs)==0) return()
  todo=sub("FVS_","",tabs$name[fvs])
  idx=grep("IDX_",tabs$name,fixed=TRUE)
  if (length(idx)) todo=setdiff(todo,sub("IDX_","",tabs$name[idx]))
  for (mk in todo)
  {
    qry=paste0("create index IDX_",mk," on FVS_",mk," (CaseID);")
cat ("qry=",qry,"\n")
    dbExecute(dbcon,qry)
  }
}

addNewRun2DB <- function(runuuid,dbcon)
{
  # dbcon is the connection to the existing output database
  # runuuid is the uuid of the run that will be merged to the output database

cat ("addNewRun2DB, runuuid=",runuuid,"\n") 
  fn = paste0(runuuid,".db")
  # breaking these two clauses allows for Windows to see that the new db has a size greater than 0
  if (!file.exists((fn))) {
    ids = try(file.size(fn))
    if (class(ids) == "try-error") {
      return("no new database found")
    } else return("no new database found")
  }
  #get and hold an exclusive lock, wait if one can't be obtained.
  trycnt=0
  dbExecute(dbcon,"PRAGMA locking_mode = EXCLUSIVE")
  while (TRUE)
  {
    trycnt=trycnt+1
    if (trycnt > 1000) 
    {
      dbExecute(dbcon,"PRAGMA locking_mode = NORMAL")
      myListTables(dbcon) #any query will cause the locking mode to become active
      return("could not get exclusive lock.")
    }
cat ("try to get exclusive lock, trycnt=",trycnt,"\n");
    rtn <- try(dbExecute(dbcon,"create table dummy (dummy int)"))
    if (class(rtn) != "try-error") break;
    Sys.sleep (10)
  } 
  dbExecute(dbcon,"drop table if exists dummy")

  deleteRelatedDBRows(runuuid,dbcon)   

  mkDBIndices(dbcon)
  newrun = paste0("newrun",gsub("-","",runuuid),Sys.getpid())
  qry = paste0("attach database '",fn,"' as ",newrun)
cat ("qry=",qry,"\n") 
  res = try (dbExecute(dbcon,qry))
  if (class(res) == "try-error") return ("new run database attach failed")
  qry = paste0("select * from ",newrun,".sqlite_master where type='table'")
cat ("qry=",qry,"\n") 
  newtabs = dbGetQuery(dbcon,qry)[,"tbl_name",drop=TRUE]
  if (length(newtabs)==0)
  {
    try (dbExecute(dbcon,paste0("detach database '",newrun,"'")))
    return("no data found in new run")
  }
  ic = grep ("FVS_Cases",newtabs)
  if (length(ic) == 0) return("no FVS_Cases found in new run") 
  qry = paste0("select * from ",newrun,".FVS_Cases")
cat ("qry=",qry,"\n") 
  res = dbGetQuery(dbcon,qry)
cat ("nrow(res)=",nrow(res)," CaseID=",res$CaseID,"\n")
  if (nrow(res) == 0)
  {
    dbExecute(dbcon,paste0("detach database '",newrun,"'"))
    return("no new cases found in new run")
  }
  tbs <- dbGetQuery(dbcon,"select name from sqlite_master where type='table';")[,1]
cat ("length(tbs)=",length(tbs),"\n")
  for (newtab in newtabs) 
  {        
    if (newtab %in% tbs)
    {
      qry = paste0("PRAGMA ",newrun,".table_info('",newtab,"')")
cat ("qry=",qry,"\n") 
      newCols = dbGetQuery(dbcon,qry)
      newColNs = newCols[,"name",drop=TRUE]
      qry = paste0("PRAGMA table_info('",newtab,"')")
cat ("qry=",qry,"\n") 
      existCols = dbGetQuery(dbcon,qry)[,"name",drop=TRUE]
      toAdd = setdiff(newColNs,existCols)
      if (length(toAdd))
      {
        for (addCol in toAdd)
        {
          qry = paste0("alter table ",newtab," add column ",addCol," ",
                subset(newCols,name==addCol)[,"type"])
cat ("qry=",qry,"\n") 
          dbExecute(dbcon,qry)
        }
      }
      qry = if (identical(newColNs,existCols)) 
        paste0("insert into ",newtab," select * from ",newrun,".",newtab) else
        paste0("insert into ",newtab," (",
          paste0(newColNs,collapse=","),") select * from ",newrun,".",newtab)
cat ("qry=",qry,"\n") 
      rtn=dbExecute(dbcon,qry)
cat ("rtn=",rtn,"\n")
    } else {
      qry = paste0("create table ",newtab," as select * from ",newrun,".",newtab,";")
cat ("qry=",qry,"\n") 
      dbExecute(dbcon,qry)
    }
  }    
  dbExecute(dbcon,paste0("detach database '",newrun,"'"))
  unlink(fn)
  mkDBIndices(dbcon)
  dbExecute(dbcon,"PRAGMA locking_mode = NORMAL")
  myListTables(dbcon) #any query will cause the new locking mode to become active
  "data inserted"
}                                                    


ncmps <- function(fvsRun)
{
  sum(unlist(lapply(fvsRun$stands,function(x) length(x$cmps)))) +
  sum(unlist(lapply(fvsRun$grps,  function(x) length(x$cmps))))
}

AddFiles <- function (addfiles)
{
  addkeys = list()
  if (!is.null(addfiles) && !is.na(addfiles))
  { 
    fns=scan(text=addfiles,what="character",sep="\n",quiet=TRUE)
    semicolon <- grep(";",fns)
    if(length(semicolon)){
      splits <- unlist(strsplit(fns[semicolon],";"))
      fns <- fns[-semicolon]
      fns <- append(fns,splits)
    }
    for (fn in fns)
    {
      addkeys[length(addkeys)+1]=paste0("Open        133.\n",fn,
                     "\nAddFile     133.\nClose       133.\n")
      names(addkeys)[length(addkeys)] = fn
    }
  }
  return(addkeys)
}


addStandsToRun <- function (session,input,output,selType,globals,dbGlb)
{
  isolate({
cat ("in addStandsToRun, selType=",selType," input$inVars=",input$inVars,"\n")
    if (length(input$inStds)+length(input$inGrps) == 0) return()
    timescale <- if(length(globals$fvsRun$stands)) 1 else 0
    for (i in 1:length(globals$activeFVS))
    {
      if (!is.null(globals$activeFVS[[i]][1]) && globals$activeFVS[[i]][1] == input$inVars) 
      {
        globals$fvsRun$FVSpgm <- names(globals$activeFVS[i])[1]
        break                
      }
    }
    globals$fvsRun$refreshDB=input$inTabs
cat ("globals$fvsRun$refreshDB=",globals$fvsRun$refreshDB,"\n")
    resetGlobals(globals,globals$fvsRun,prms) 
    if (length(globals$fvsRun$startyr)==0) globals$fvsRun$startyr <- format(Sys.time(), "%Y")
    curstartyr = as.numeric(globals$fvsRun$startyr)
    stdInit <- input$inTabs
    fields = dbListFields(dbGlb$dbIcon,stdInit)
    if (stdInit %in% c("FVS_PlotInit","FVS_PlotInit_Plot")) 
    {
      sidid = "StandPlot_ID" 
      needFs = c("StandPlot_ID","StandPlot_CN") 
    } else {
      sidid = "Stand_ID"
      needFs = c("Stand_ID","Stand_CN")
    }
    allNeed = c("Groups","Inv_Year","AddFiles","FVSKeywords","Sam_Wt",needFs)
    fields = intersect(toupper(fields),toupper(allNeed))
    if (selType == "inAdd")
    {
      dbExecute(dbGlb$dbIcon,'drop table if exists temp.Stds') 
      if (length(input$inStds))
      {
        dbWriteTable(dbGlb$dbIcon,DBI::SQL("temp.Stds"),data.frame(SelStds = input$inStds))
      } else return()
    } else {
      # use if inAddGrp
      qry = paste0('select ',sidid,' from temp.Grps',
                 ' where Grp in (select SelGrps from temp.SGrps)')
      stds = try(dbGetQuery(dbGlb$dbIcon,qry))
      if (class(stds) == "try-error") return()                                                             
      if (nrow(stds) == 0) return()
      stds = stds[,1]
      stds = if (input$inAnyAll == "Any") unique(stds) else
      {
        stdCnts = table(stds) 
        stds = names(stdCnts[stdCnts == length(input$inGrps)])                                                                                                                           
      } 
      if (length(stds) == 0) return()  
      dbExecute(dbGlb$dbIcon,'drop table if exists temp.Stds') 
      dbWriteTable(dbGlb$dbIcon,DBI::SQL("temp.Stds"),data.frame(SelStds = stds))
    }
    qry = paste0('select ',paste0(fields,collapse=","),' from ',stdInit,
                 ' where ',sidid,' in (select SelStds from temp.Stds)')
cat ("qry=",qry,"\n")
    fvsInit = try(dbGetQuery(dbGlb$dbIcon,qry))
    if (class(fvsInit)=="try-error") return()
    if (nrow(fvsInit) == 0) return()
    names(fvsInit) = toupper(names(fvsInit))
    maxMsgs = (nrow(fvsInit) %/% 10) + 2
    progress <- shiny::Progress$new(session,min=1,max=maxMsgs)
    msgVal = 1
    progress$set(message = paste0("Loading ",nrow(fvsInit)," stands "), 
        value = msgVal)
    nreps = as.numeric(input$inReps)
    rwts = try(scan(text=gsub(","," ",input$inRwts),sep="",quiet=TRUE))
    if (class(rwts)!="numeric") rwts=1
    if (!length(rwts)) rwts=1
cat ("nreps=",nreps," rwts=",rwts," (recycled as needed)\n")
    if (is.na(nreps) || is.null(nreps) || nreps < 1) nreps = 1
    for (row in 1:nrow(fvsInit))  # the selectInput list
    {
      if (row %% 10 == 0) 
      {
        msgVal = msgVal+1
        progress$set(value = msgVal)
      }
      sid = fvsInit[row,toupper(sidid)]  
      iwt = 0
      for (rep in 1:nreps)
      {
        iwt = iwt+1
        if (iwt>length(rwts)) iwt=1
        newstd <- mkfvsStd(sid=sid,uuid=uuidgen(),repwt=rwts[iwt])
        addfiles = AddFiles(fvsInit[row,"ADDFILES"])
        for (addf in names(addfiles))
          newstd$cmps <- append(newstd$cmps,
                   mkfvsCmp(kwds=addfiles[[addf]],uuid=uuidgen(),
                     exten="base", atag="k",kwdName=paste0("AddFile: ",addf),
                     title=paste0("AddFile: ",addf)))                    
        addkeys <- fvsInit[row,"FVSKEYWORDS"]
        if (!is.null(addkeys) && !is.na(addkeys) && nchar(addkeys) && addkeys != "NA") 
          newstd$cmps <- append(newstd$cmps,mkfvsCmp(kwds=addkeys,uuid=uuidgen(),
                   exten="base", atag="k",kwdName=paste0("From: ",stdInit), 
                   title=paste0("From: ",stdInit)))
        grps <- if (!is.null(fvsInit$GROUPS))
           scan(text=fvsInit[row,"GROUPS"],
                what=" ",quiet=TRUE) else c("All All_Stands")
        requ <- unlist(grps[grep("^All",grps)])
        grps <- sort(union(intersect(input$inGrps,grps),requ))
        have <- unlist(lapply(globals$fvsRun$grps,function(x) 
                if (x$grp != "") x$grp else NULL))
        need <- setdiff(grps, have)
        for (grp in need) 
        {
          newgrp <- mkfvsGrp(grp=grp,uuid=uuidgen())
          grprow <- if (!is.null(globals$inData$FVS_GroupAddFilesAndKeywords)) 
            grep(grp,globals$inData$FVS_GroupAddFilesAndKeywords[,"GROUPS"],
                 fixed=TRUE) else c()
          for (grow in grprow)
          {
            addkeys <- globals$inData$
                       FVS_GroupAddFilesAndKeywords[grow,"FVSKEYWORDS"]
            if (!is.null(addkeys) && !is.na(addkeys)) 
              newgrp$cmps[[length(newgrp$cmps)+1]] <- 
                mkfvsCmp(kwds=addkeys,uuid=uuidgen(),atag="k",exten="base",
                         kwdName="From: FVS_GroupAddFilesAndKeywords",
                           title="From: FVS_GroupAddFilesAndKeywords")
            addfiles <- AddFiles(globals$inData$
                        FVS_GroupAddFilesAndKeywords[grow,"ADDFILES"])
            for (addf in names(addfiles))
                newgrp$cmps[[length(newgrp$cmps)+1]] <-
                mkfvsCmp(kwds=as.character(addfiles[addf]),uuid=uuidgen(),atag="k",exten="base",
                      kwdName=paste0("AddFile: ",addf),
                        title=paste0("AddFile: ",addf))
          }
          globals$fvsRun$grps <- append(globals$fvsRun$grps,newgrp)
        }
        invyr <- as.numeric(fvsInit[row,"INV_YEAR"])
        if (invyr > curstartyr) 
        {
          curstartyr <- invyr
          globals$fvsRun$startyr <- as.character(curstartyr)
        }
        newstd$invyr <- as.character(invyr)
        have <- unlist(lapply(globals$fvsRun$grps,function(x) 
                if (x$grp != "") x$grp else NULL))
        newstd$grps <- globals$fvsRun$grps[sort(match(grps,have))]
        globals$fvsRun$stands <- append(globals$fvsRun$stands,newstd)
      }
    }
    if (length(globals$activeVariants) == 1 && 
               globals$activeVariants %in% c("sn","nc","oc","op")) 
    {
      cycleLength="5"
      simLength="50"
    } else {
      cycleLength="10"
      simLength="100"
    }

    globals$fvsRun$endyr <- as.character(as.numeric(globals$fvsRun$startyr) +
                                         as.numeric(simLength))
    globals$fvsRun$cyclelen <- cycleLength
    if (timescale==0)
    {
      updateTextInput(session=session, inputId="startyr",  
                      value=globals$fvsRun$startyr)
      updateTextInput(session=session, inputId="endyr",    
                      value=globals$fvsRun$endyr)
      updateTextInput(session=session, inputId="cyclelen", 
                      value=globals$fvsRun$cyclelen)
    }
    msgVal = msgVal+1
    progress$set(detail="Updating reps tags",value = msgVal)
    updateReps(globals)
    msgVal = msgVal+1
    progress$set(detail="Loading contents listbox",value = msgVal)
    mkSimCnts(globals$fvsRun,justGrps=isolate(input$simContType=="Just groups"))
    updateSelectInput(session=session, inputId="simCont", 
      choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
    output$contCnts <- renderUI(HTML(paste0("<b>Contents</b><br>",
      length(globals$fvsRun$stands)," stand(s)<br>",
      length(globals$fvsRun$grps)," group(s)")))
    updateStandTableSelection()
    globals$changeind <- 1
    output$contChange <- renderText({
      HTML(paste0("<b>","*Run*","</b>"))
    })
    progress$close()
  })
}


updateReps <- function(globals)
{ 
cat ("in updateReps, num stands=",length(globals$fvsRun$stands),"\n") 
  if (length(globals$fvsRun$stands))
  {
    stds <- unlist(lapply(globals$fvsRun$stands,function(x) x$sid))  
    cnts <- table(stds) 
    have <- unlist(lapply(globals$fvsRun$grps,function(x) 
            if (x$grp != "") x$grp else NULL))
    need = paste0("AutoRep=",1:max(cnts))
    mkgps <- setdiff(need, have)
    for (grp in mkgps) 
    {
      newgrp <- mkfvsGrp(grp=grp,uuid=uuidgen())
      globals$fvsRun$grps <- append(globals$fvsRun$grps,newgrp)
    }
    have <- unlist(lapply(globals$fvsRun$grps,function(x) 
            if (x$grp != "") x$grp else NULL))
    grpIdxs = match(need,have)
    names(grpIdxs) = need
    for (cn in 1:length(cnts)) 
    {
      cnt <- cnts[cn]
      reps <- (1:length(stds))[names(cnt) == stds]
      if (length(reps) > 1)
      {                                                                                             
        i <- 1
        for (r in reps) 
        {
          globals$fvsRun$stands[[r]]$rep <- i
          have <- unlist(lapply(globals$fvsRun$stands[[r]]$grps,function(x) 
                         if (x$grp != "") x$grp else NULL))
          if (! names(grpIdxs)[i] %in% have) globals$fvsRun$stands[[r]]$grps <- 
            append(globals$fvsRun$stands[[r]]$grps,globals$fvsRun$grps[[grpIdxs[i]]])
          i <- i+1
        }     
      } else globals$fvsRun$stands[[reps]]$rep <- 0                           
    }
  }
}

getProjectList <- function(includeLocked=FALSE)
{
  if (isLocal())
  {  
    if(!globals$localWindows){
      selChoices = unlist(lapply (dir(".."), function (x,inc) {
      if (!inc) if (file.exists(
        paste0("../",x,"/projectIsLocked.txt"))) return(NULL)
      fn = paste0("../",x,"/projectId.txt") 
      if (file.exists(paste0("../",x,"/projectId.txt"))) x
    },includeLocked))
    }
    if(globals$localWindows){
      proj = prjInst
      selChoices = unlist(lapply (dir(proj), function (x,inc) {    
      if (!inc) if (!is.na(match(basename(prjDir),x))) return(NULL)
      fn = paste0(proj,"/",x,"/projectId.txt")
      if (file.exists(fn)) x
    },includeLocked))  
    }
    if (length(selChoices)) 
    {
      names(selChoices) = selChoices
      selChoices = as.list(selChoices)
    } else selChoices=NULL
  } else {
    curEmail=scan(file="projectId.txt",what="character",sep="\n",quiet=TRUE)
    curEmail=toupper(trim(sub("email=","",curEmail[1]))) 
    data = lapply (dir(".."), function (x,inc) {         
      if (!inc) if (file.exists(
        paste0("../",x,"/projectIsLocked.txt"))) return(NULL)
      fn = paste0("../",x,"/projectId.txt") 
      if (file.exists(fn))
      {
        prj = scan (fn,what="character",sep="\n",quiet=TRUE)
        ans = c(prj=x,email=trim(sub("email=","",prj[1])),
          title=trim(sub("title=","",prj[2])))
        ans
      } else NULL
    },includeLocked) 
    if (length(data))
    {
      data = as.data.frame(do.call(rbind,data),stringsAsFactors=FALSE)
      names(data)=c("prj","email","title")
      data$email=toupper(data$email)
      data = data[data$email == curEmail,,drop=FALSE]
      selChoices=as.list(data$prj)
      names(selChoices)=data$title 
    } else selChoices=NULL
  }                                                 
  selChoices
}


myListTables <- function(db)
{
  dbGetQuery(db,"select name from sqlite_master where type = 'table';")[,1]
}
    
mkNameUnique <- function(name,setOfNames=NULL)
{
  if (is.null(setOfNames)) return(name)
  if (!name %in% setOfNames) return(name)
  pl=regexpr(pattern="\\([0-9]*\\)$",name)
  repeat
  {
    if (pl != -1)
    {
      i=as.numeric(substring(name,pl+1,pl+attr(pl,"match.length")-2))
      if (is.na(i)) i=1 else
      {
        i=i+1
        name=gsub("^\\s+|\\s+$","",substr(name,1,pl-1))
      }
    } else i=1
    name = paste0(name," (",i,")")
    if (!name %in% setOfNames) break
  }
  return(name)
}
                    
mkFileNameUnique <- function(fn)
{ 
  trim <- function (x) gsub("^\\s+|\\s+$","",x)
  fn = trim(fn)
  if(!globals$localWindows)fex <- file.exists(fn)
  if(globals$localWindows)fex <- file.exists(paste0(prjDir,"/",fn))
  if (!fex) return(fn)
  ext = tools::file_ext(fn)
  name = tools::file_path_sans_ext(fn)
  name = trim(unlist(strsplit(name,"(",fixed=TRUE))[1])
  i=0
  repeat
  {
    if (!file.exists(fn)) return(fn)
    i = i+1
    fn = paste0(name," (",i,")")
    if (nchar(ext)) fn = paste0(fn,".",ext)
  } 
}

getTableName <- function(dbcon,basename)
{
  tbs <- myListTables(dbcon)
  ltbs <- tolower(tbs)
  basename <- tolower(basename)
  itab <- match(basename,ltbs) 
  if (is.na(itab)) itab <- grep(basename,ltbs,fixed=TRUE)[1]
  if (is.na(itab)) return(NULL) else return(tbs[itab])  
}

