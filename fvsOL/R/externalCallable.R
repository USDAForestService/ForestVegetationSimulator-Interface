#' Build an FVS run in a project
#'
#' Build an FVS run in a project and add it to the list of runs in the project. 
#' If some FVS runs are already present, another is added with this call.
#' The name of the input data base is FVS_Data.db and it must already exist.
#'
#' @param prjDir is the path name to the project directory, if null the system
#'   assumes that the current directory is the project directory.
#' @param title a character string with the run title, if null, the system generates 
#'   the name, that is, it would be "Run 1", or "Run 2", and so on for 
#'   all the runs already present. If none are present, then the default 
#'   run title with be "Run 1" corresponding to the first run.
#' @param standIDs a vector of character strings holding the stand IDs (for example: 
#'   \code{standIDs=c("id1","id2")} would load 2 stands, whereas: \code{standIDs="id1"} 
#'    would load just one. If NULL, all stands in the init table are loaded. 
#' @param stdInit a character string of the name of the standinit table you 
#'    want to use, the stands would be loaded from that table.
#' @param variant a 2 character string specifying the variant (required).  
#'    Ihe standID and the variant must match in the designated init table.
#' @param autoOut a character vector of output table selections using these
#'    codes (if null, defaults are used): 
#' @param startyr the start year of the simulation, if NULL, the current year
#'    is used. a 2 character string specifying the variant (required).  
#' @param endyr the end year of the simulation, if NULL one is computed using
#'    variant-specific settings.
#' @param cycleat a vector of years where cycle boundaries are requested, none
#'    outside the startyr to endyr interval are used.
#' @return the new run uuid, NULL if not created.
#' @export
extnMakeRun <- function (prjDir=getwd(),title=NULL,standIDs=NULL,
   stdInit="FVS_StandInit", variant)
{
  if (missing(variant)) stop("variant required")
  if (dir.exists(prjDir)) prjDir=normalizePath(prjDir) else 
    stop("The specified project directory must exist.")
  dbfile = file.path(prjDir,"FVS_Data.db")
  if (!file.exists(dbfile)) stop ("FVS_Data.db must exist")
  runsFile=file.path(prjDir,"FVS_Runs.RData")
  if (file.exists(runsFile)) load(runsFile) else FVS_Runs=list()
  if (is.null(title)) title=nextRunName(FVS_Runs)
  fvsRun=mkfvsRun(title=title,uuid=uuidgen(),runScript="fvsRun",
                  FVSpgm=paste0("FVS",variant),
                  refreshDB=stdInit,startyr=format(Sys.time(), "%Y"))
  dbcon <- dbConnect(dbDriver("SQLite"),dbfile)
  on.exit(expr = dbDisconnect(dbcon))
  
  if (! stdInit %in% dbListTables(dbcon)) stop(paste0(stdInit," not found in database."))
  
  fields = dbListFields(dbcon,stdInit)
  if (toupper(stdInit) %in% toupper(c("FVS_PlotInit","FVS_PlotInit_Plot"))) 
  {
    sidid = "StandPlot_ID" 
    needFs = c("StandPlot_ID","StandPlot_CN") 
  } else {  
    sidid = "Stand_ID"
    needFs = c("Stand_ID","Stand_CN")
  }
  allNeed = c("Groups","Inv_Year","AddFiles","FVSKeywords","Sam_Wt",needFs)
  fields = intersect(toupper(fields),toupper(allNeed))
  dbExecute(dbcon,'drop table if exists temp.Stds')
  qry = paste0("select ",paste(fields,collapse=",")," from ",stdInit, 
    ' where lower(variant) like "%',tolower(variant),'%"')
  dbWriteTable(dbcon,DBI::SQL("temp.Stds"),data.frame(SelStds = standIDs))
  qry = paste0(qry," and ",sidid," in (select SelStds from temp.Stds)")
  
  fvsInit = try(dbGetQuery(dbcon,qry))

  if (class(fvsInit) == "try-error") stop(paste0("query failed, qry=",qry))                                                          
  if (nrow(fvsInit) == 0) stop(paste0("query returned no data, qry=",qry))   

  keyCmps=list()
  if (class(keywords)=="character")
  {
    for (i in 1:length(keywords))
    {
      kys = keywords[i]
      nam = names(kys)
      if (is.null(nam) || nchar(nam)==0) nam=paste0("KeywordSet",i)
      names(kys) = NULL
      key = mkfvsCmp(kwds=kys,uuid=uuidgen(), exten="base", atag="k",
                     kwdName="freeForm",title=nam,variant=variant)
      keyCmps = append(keyCmps,key)
    }
  }
 
  FVS_GroupAddFilesAndKeywords = try(dbReadTable(dbcon,"FVS_GroupAddFilesAndKeywords"))
  if (class(FVS_GroupAddFilesAndKeywords) == "try-error") 
            FVS_GroupAddFilesAndKeywords = NULL

  for (row in 1:nrow(fvsInit)) 
  {
    sid = fvsInit[row,toupper(sidid)]  
    iwt = 0
    newstd <- mkfvsStd(sid=sid,uuid=uuidgen(),repwt=1)
    addfiles = AddFiles(fvsInit[row,"ADDFILES"])
    for (addf in names(addfiles)) newstd$cmps <- append(newstd$cmps,
        mkfvsCmp(kwds=addfiles[[addf]],uuid=uuidgen(),
                 exten="base", atag="k",kwdName="freeForm",
                 title=paste0("AddFile: ",addf)))                    
    addkeys <- fvsInit[row,"FVSKEYWORDS"]
    if (!is.null(addkeys) && !is.na(addkeys) && nchar(addkeys) && addkeys != "NA") 
      newstd$cmps <- append(newstd$cmps,mkfvsCmp(kwds=addkeys,uuid=uuidgen(),
               exten="base", atag="k",kwdName="freeForm", 
               title=paste0("From: ",stdInit)))

    if (is.null(group)) for (key in keyCmps)
      newstd$cmps[[length(newstd$cmps)+1]] <- mkfvsCmp(key,uuid=uuidgen())

    grps <- if (!is.null(fvsInit$GROUPS))
       scan(text=fvsInit[row,"GROUPS"],
            what=" ",quiet=TRUE) else c("All All_Stands")
    requ <- unlist(grps[grep("^All",grps)])
    have <- unlist(lapply(fvsRun$grps,function(x) 
            if (x$grp != "") x$grp else NULL))
    need <- setdiff(grps, have)
    for (grp in need) 
    {
      newgrp <- mkfvsGrp(grp=grp,uuid=uuidgen())
      grprow <- if (!is.null(FVS_GroupAddFilesAndKeywords)) 
        grep(grp,FVS_GroupAddFilesAndKeywords[,"GROUPS"],
             fixed=TRUE) else c()
      for (grow in grprow)
      {
        addkeys <- FVS_GroupAddFilesAndKeywords[grow,"FVSKEYWORDS"]
        if (!is.null(addkeys) && !is.na(addkeys)) 
          newgrp$cmps[[length(newgrp$cmps)+1]] <- 
            mkfvsCmp(kwds=addkeys,uuid=uuidgen(),atag="k",exten="base",
                     kwdName="freeEdit",title="From: FVS_GroupAddFilesAndKeywords")
        addfiles <- AddFiles(FVS_GroupAddFilesAndKeywords[grow,"ADDFILES"])
        for (addf in names(addfiles))
            newgrp$cmps[[length(newgrp$cmps)+1]] <-
            mkfvsCmp(kwds=as.character(addfiles[addf]),uuid=uuidgen(),atag="k",exten="base",
                  kwdName="freeForm",title=paste0("AddFile: ",addf))
        if (!is.null(group) && group == grp) for (key in keyCmps) 
           newgrp$cmps[[length(newgrp$cmps)+1]] <- key
      }
      fvsRun$grps <- append(fvsRun$grps,newgrp)
    }
    invyr <- as.numeric(fvsInit[row,"INV_YEAR"])
    if (invyr > as.numeric(fvsRun$startyr)) fvsRun$startyr <- as.character(invyr)
    newstd$invyr <- as.character(invyr)
    have <- unlist(lapply(fvsRun$grps,function(x) 
            if (x$grp != "") x$grp else NULL))
    newstd$grps <- fvsRun$grps[sort(match(grps,have))]
    fvsRun$stands <- append(fvsRun$stands,newstd)
  }
  if (variant %in% c("sn","nc","oc","op")) 
  {
    cycleLength="5"
    simLength="50"
  } else {
    cycleLength="10"
    simLength="100"
  }  
  fvsRun$endyr <- as.character(as.numeric(fvsRun$startyr) + as.numeric(simLength))
  fvsRun$cyclelen <- cycleLength
  db = connectFVSProjectDB(prjDir)
  storeFVSRun(db,fvsRun)
  dbDisconnect(db)
  return(fvsRun$uuid)
}

#' Duplicate a run and give the duplicate a new title
#'
#' Pass in a project directory and an existing runUUID, the duplicates title
#' and the run is duplicated and given the new title and default management ID.
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID the uuid of the run that will be duplicated.
#' @param dupTitle a character string with the duplicated run's title
#'   if null, the system generates the name.
#' @param dupMgmtID a character string with the duplicated run's management ID.
#'   if null, the system generates the ID.
#' @param toPrjDir if not null, the duplicate is placed in a different 
#'   project directory given by this argument. 
#' @return the new run uuid, NULL if not created.
#' @export
extnDuplicateRun <- function(prjDir=getwd(),runUUID=NULL,dupTitle=NULL,
   dupMgmtID=NULL,toPrjDir=NULL)
{
  if (!dir.exists(prjDir)) stop("The specified project directory must exist.")
  if (is.null(runUUID)) stop("runUUID must be specified.")
  db = connectFVSProjectDB(prjDir)
  if (is.null(toPrjDir)) dbo = db else 
  {
    dbo = connectFVSProjectDB(toPrjDir)
    if (is.null(dbo)) stop("'toPrjDir' directory error.")
  }
  on.exit({ 
    dbDisconnect(db)
    if (!is.null(dbo)) suppressWarnings(dbDisconnect(dbo))
  })
  saveFvsRun=loadFVSRun(db,runUUID)
  if (is.null(saveFvsRun)) stop("runUUID run data not found.")
  prjs=getFVSRuns(dbo)  
  if (is.null(dupTitle)) dupTitle=nextRunName(names(prjs))
  dupTitle=mkNameUnique(dupTitle,names(prjs))
  saveFvsRun$title=dupTitle
  uuid=uuidgen()
  saveFvsRun$uuid=uuid
  saveFvsRun$defMgmtID = if (is.null(dupMgmtID)) nextMgmtID(length(prjs)) else dupMgmtID
  storeFVSRun(dbo,saveFvsRun)
  return(uuid)
} 


#' Convert an R object to a raw vector
#'
#' param object is any R object to be convered to a raw
#' @return a raw vector representation of the object
#' @export
extnToRaw <- function(x) memCompress(serialize(x,NULL),type="gzip")

#' Convert a raw vector to an R object
#'
#' param object a raw vector
#' @return the object
#' @export
extnFromRaw = function(x) unserialize(memDecompress(x,type="gzip"))

#' Get FVS Runs
#'
#' Pass in a project directory and get back a data.fram of the FVS runs. 
#'
#' @param prjDir is the path name to the project directory, default is  
#'   current directory.
#' @return a data.frame listing the uuid, title, and datetime of existing runs
#'   and NULL if the project does not exist or if no runs exist.
#' @export
extnGetRuns <- function (prjDir=getwd())
{
  if (!dir.exists(prjDir)) return(NULL) 
  prjDir = normalizePath(prjDir)
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db))
  runs = getFVSRuns(db,asList=FALSE)
  class(runs$time)= c('POSIXt','POSIXct')
  return(runs)
}

#' Delete FVS Runs
#'
#' Given a project directory and 1 or more run uuids, the function deletes
#' the runs. 
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUIDs a character vector of 1 or more run uuids to be deleted.
#' @param delOutput if TRUE (the default) the data in FVSOut.db is also
#'   deleted.
#' @return a data.frame listing the uuid, title, and datetime of the remaining runs
#'   and NULL if the no runs exist.
#' @export
extnDeleteRuns <- function (prjDir=NULL,runUUIDs=NULL,delOutput=TRUE)
{
  if (is.null(runUUIDs)) stop("runUUIDs must be specified.")
  if (is.null(prjDir)) prjDir=getwd() 
  if (!dir.exists(prjDir)) return(NULL) 
  prjDir = normalizePath(prjDir)
  db = connectFVSProjectDB(prjDir)
  on.exit({
    if (class(db)  == "SQLiteConnection") dbDisconnect(db)
    if (exists("dbO") && class(dbO) == "SQLiteConnection") dbDisconnect(dbO)
  })    
  runs = getFVSRuns(db)
  todel=na.omit(match(runUUIDs,runs))
  if (length(todel))
  {
    deluuid = runs[todel]
    for (du in deluuid) 
    {
      removeFVSRun(db,du)
      removeFVSRunFiles(du,all=TRUE)
    }
    if (delOutput)
    {
      dbO=dbConnect(dbDriver("SQLite"),file.path(prjDir,"FVSOut.db"))
      for (du in deluuid) deleteRelatedDBRows(du,dbO)
    }
  }
  return(extnGetRuns())
}
       
#' Add Components to a run.
#'                                         
#' Given a project directory a run uuid, a dataframe of 
#' keywords is added to groups or stands in the run.
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character vector of 1 run uuid that is processed
#' @param cmps is a list of components that will be processed (in order). Each
#'   list member can be an fvsOL keyword component, a raw vector that can be 
#'   transformed intoan fvsOL keyword component, or a character 
#'   string of keywords. If a character string,
#    they are considered "free form. Several lines of keywords can be separated
#'   by \\n (newline) chars to indicated muliple keywords in the component. If
#'   If the list item are named, the component name is set to that name.
#'   a component, the list item is turned into one internally to this function. 
#' @param groups is a character string vector (or a list) of group names (or uuids)
#'    to which the keyword components will be attached.
#' @param stands is a character string vector (or a list) of stand ids (or uuids)
#'    to which the keyword components will be attached.
#' 
#' @return The number of keyword components added to the run.
#' @export
extnAddKwds <- function(prjDir=getwd(),runUUID,cmps,groups=NULL,stands=NULL)
{
  if (missing(runUUID)) stop("runUUID required")
  if (missing(cmps)) stop("cmps is required") 
  prjDir = normalizePath(prjDir)
  if (file.exists(paste0(prjDir),"/projectIsLocked.txt")) stop("project is locked")
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db)) 
  saveFvsRun = loadFVSRun(db,runUUID)
  if (!exists("saveFvsRun")) stop("runUUID run data not found")
  if (attr(class(saveFvsRun),"package") != "fvsOL") stop("Don't recognize the loaded run object")
  # process the cmps. convert "raw" and/or "character" as needed.
  onames = names(cmps)
  for (i in 1:length(cmps))
  {
    cmps[i] = switch(class(cmps[i]),
      "fvsCmp" = mkfvsCmp(cmps[i],uuid=uuidgen()),
      "raw"=extnFromRaw(cmp[i]),
      "character" = 
      {
        title = onames[i]
        if (is.null(title) || nchar(title)==0) title=paste0("Added from external source (",i,")")
        mkfvsCmp(kwds = cmp[i], exten="base", title=title, 
         variant=substring(saveFvsRun$FVSpgm,4),uuid=uuidgen(),atag="k")
      })
    cname=names(cmps[i])
    if (!is.null(cname) && nchar(cname)) cmps[i]$title = cname
  }
  nadd=0
  # process groups
  if (!is.null(groups))
  {
    for (grp in saveFvsRun$grps)
    {
      if (grp$grp %in% groups) for(cmp in cmps) 
      {
        append(grp$cmps,cmp)
        nadd=nadd+1
      }
    }
  }
  # process stands
  if (!is.null(stands))
  {
    for (std in saveFvsRun$stds)
    {
      if (sid$sid %in% stands) for(cmp in cmps) 
      {  
        append(sid$cmps,cmp)
        nadd=nadd+1
      }
    }
  }
  if (nadd==0) return(0)
  storeFVSRun(db,saveFvsRun)  
  nadd
}

#' Set FVS output and timing options
#'
#' Given a project directory a run uuid, set the output selections
#' and the timing keyword controls
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character vector of the run uuid that is processed
#' @param autoOut a vector of character strings corresponding to the automatic
#'   output selections to be set (not the svs ones), where these values "turn on"
#'   the corresponding selections and existing selections are not changed (if NULL
#'   no changes are made: "Treelists", "Carbon", "Fire", "Dead", "CanProfile", 
#'   "SnagDet",  "StrClass", "CalibStats", "Climate", "Econ", "DM_Sz_Sum", 
#'   "RD_Sum",  "RD_Det", "RD_Beetle", "InvStats", "Regen", "KeepTextTables"
#' @param svsOut a vector of two character strings where the first is the
#'   plot shape ("round" or "square") and the second is the number of images
#'   of firelines, for example: svsOut=c("square",2). If only one value is given
#'   it must be the plot shape and the number of intervals is set to 4.
#' @param startyr is the common starting year of the simulation, if NULL, 
#'   no changes are made.
#' @param endyr is the common ending year of the simulation, if NULL, 
#'   no changes are made.
#' @param cyclelen the "normal" cycle length, usually left to the variant-specific default.
#' @param cycleat is a vector of years where cycles are scheduled, if NULL, 
#'   no changes are made.
#' @return the runUUID if changes are made, else NULL
#' @export
extnSetRunOptions <- function(prjDir=getwd(),runUUID,autoOut=NULL,svsOut=NULL,
   startyr=NULL,endyr=NULL,cyclelen=NULL,cycleat=NULL)
{
  changed=FALSE
  if (missing(runUUID)) stop("runUUID required")
  prjDir = normalizePath(prjDir)
  if (file.exists(paste0(prjDir),"/projectIsLocked.txt")) stop("project is locked")
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db)) 
  saveFvsRun = loadFVSRun(db,runUUID)
  if (!exists("saveFvsRun")) stop("runUUID run data not loaded")
  if (attr(class(saveFvsRun),"package") != "fvsOL") stop("Don't recognize the loaded object")
  if (!is.null(autoOut))
  {
    autoSets = c(autoTreelists = "Treelists", autoCarbon = "Carbon",               
      autoFire = "Fire",  autoDead = "Dead", autoCanProfile = "CanProfile", 
      autoSnagDet = "SnagDet",  autoStrClass = "StrClass", 
      autoCalibStats = "CalibStats", autoClimate = "Climate",  
      autoEcon = "Econ", autoDM_Sz_Sum = "DM_Sz_Sum", autoRD_Sum = "RD_Sum",  
      autoRD_Det = "RD_Det", autoRD_Beetle = "RD_Beetle", autoInvStats = "InvStats",  
      autoRegen = "Regen", autoDelOTab = "KeepTextTables")
    set=charmatch(tolower(autoOut),tolower(autoSets))
    if (is.na(set)) warning(paste0("autoOut does not contains one or more of: ",
                      paste0(autoSets,collapse=", "))) else
    {                 
      saveFvsRun$autoOut$autoOut=as.list(names(autoSets)[set])
      changed=TRUE
    }
  }
  if (!is.null(svsOut))
  {
    svsSets = list(shape=if (tolower(svsOut[1])=="round") "Round" else "Square")
    nfire=if (length(svsOut)>1) suppressWarnings(as.integer(svsOut[2])) else NA
    if (is.na(nfire)) nfire=4
    saveFvsRun$autoOut$svsOut = c(svsSets,nfire=nfire)
    changed=TRUE
  }
  if (!is.null(startyr)) {
    saveFvsRun$startyr=as.character(startyr)
    changed=TRUE
  }
  if (!is.null(endyr))   {
    saveFvsRun$endyr  =as.character(endyr) 
    changed=TRUE
  }
  if (!is.null(cyclelen)) {
    saveFvsRun$cyclelen=as.character(cyclelen) 
    changed=TRUE
  }
  if (!is.null(cycleat)) {
    saveFvsRun$cycleat=as.character(cycleat) 
    changed=TRUE
  }
  if (changed) 
  {
    storeFVSRun(db,saveFvsRun)
    return(runUUID)
  } 
  NULL
}  

#' Get FVS keyword components from a run
#'
#' Given a project and a run uuid, get the keyword components in the run. 
#' The keyword components can be returned as a raw vector, an fvsOL keyword component,
#' or just the text of the keywords depending on the returnType. Note that
#' the original components are never returned, copies are generated and returned.
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character vector of the run uuid that is processed
#' @param returnType requested where: 
#'   "fvsCmp" the components are returned as copies of original fvsCmp objects,
#'   "raw" the components are returned a compressed raw data vectors suitable for storing
#'   in a database.
#'   "keywords" the keyword part of the components are returned as a character vector.
#' @return a named list of two other named lists. The first named list 
#'   contains a named list of components attached to groups. The type of the items
#'   is depends on the value of returnType. The names of the items are take from the
#'   "titles" of components returned. The second named list is like the first but
#'   contains items attached to groups. The first named list is named "grpCmps" and
#'   the second "stdCmps". 
#' @export
extnGetComponentKwds <- function(prjDir=getwd(),runUUID,returnType="fvsCmp")
{
  if (missing(runUUID)) stop("runUUID required")
  if (! returnType %in% c("fvsCmp","raw","keywords")) stop ("invalid value for 'returnType'")
  prjDir = normalizePath(prjDir)
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db)) 
  saveFvsRun = loadFVSRun(db,runUUID)
  if (!exists("saveFvsRun")) stop("runUUID run data not loaded") 
  if (attr(class(saveFvsRun),"package") != "fvsOL") stop("the loaded FVS run was not recognized")
  togrps = list() 
  togrpsnames = c()
  for (grp in saveFvsRun$grps) for (ocmp in grp$cmps) 
  { 
    cmp = switch(returnType,
      "fvsCmp"= mkfvsCmp(ocmp,uuid=uuidgen()),
      "raw" =  I(list(extnToRaw(ocmp))), 
      "keywords" = ocmp$kwds
    )
    togrpsnames = c(togrpsnames,ocmp$title)
    togrps = append(togrps,cmp)
  }
  if (length(togrps)) names(togrps)=togrpsnames
  tostds = list() 
  tostdsnames = c()
  for (std in saveFvsRun$stands) for (ocmp in std$cmps) 
  {
    cmp = switch(returnType,
      "fvsCmp"= mkfvsCmp(ocmp,uuid=uuidgen()),
      "raw" =  I(list(extnToRaw(ocmp))), 
      "keywords" = ocmp$kwds
    )
    tostdsnames = c(tostdsnames,ocmp$title)
    tostds = append(tostds,cmp)
  }
  return (list(Groups=togrps,Stands=tostds))       
}

#' Delete FVS keyword components from a run.
#'
#' Given a project directory a run uuid, and a list of component UUIDs,
#' this function deletes those components.
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character string of the run uuid that is processed
#' @param compUUIDs a vector of character strings holding the UUIDs of
#'   the components that will be deleted from the run.
#' @return the number of deletions.
#' @export
extnDeleteComponents <- function(prjDir=getwd(),runUUID,compUUIDs)
{
  if (missing(runUUID)) stop("runUUID required")
  if (missing(compUUIDs)) stop("compUUIDs required")
  prjDir = normalizePath(prjDir)
  if (file.exists(paste0(prjDir),"/projectIsLocked.txt")) stop("project is locked")
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db)) 
  saveFvsRun = loadFVSRun(db,runUUID)
  if (!exists("saveFvsRun")) stop("runUUID run data not loaded")
  if (attr(class(saveFvsRun),"package") != "fvsOL") stop("Don't recognize the loaded object")
  changed = 0
  for (grp in saveFvsRun$grps)
  {
    todel=NULL
    if (length(grp$cmps)) for (i in 1:length(grp$cmps)) 
    {
      uuid = grp$cmps[[i]]$uuid
      if (uuid %in% compUUIDs) todel=c(todel,i)
    }
    if (length(todel))
    {
      grp$cmps = if (length(todel) == length(grp$cmps)) NULL else grp$cmps[-todel]
      changed = changed+length(todel)
    }
  }
  for (std in saveFvsRun$stands)
  {
    todel=NULL
    if (length(std$cmps)) for (i in 1:length(std$cmps)) 
    {
      uuid = std$cmps[[i]]$uuid
      if (uuid %in% compUUIDs) todel=c(todel,i)
    }
    if (length(todel))
    {
      std$cmps = if (length(todel) == length(std$cmps)) NULL else std$cmps[-todel]
      changed = changed+length(todel)
    }
  }
  if (changed) storeFVSRun(db,saveFvsRun)
  return(changed)
}

  