# $Id$
#
#' Build an FVS run in a project
#'
#' Build an FVS run in a project and add it to the list of runs in the project. 
#' If some FVS runs are already present, another is added with this function.
#' The name of the input data base is FVS_Data.db and it must already exist.
#'
#' @param prjDir is the path name to the project directory, if null the system
#'   assumes that the current directory is the project directory.
#' @param title a character string with the run title, if null, the system generates 
#'   the name, that is, it would be "Run 1", or "Run 2", and so on for 
#'   all the runs already present. If none are present, then the default 
#'   run title with be "Run 1" corresponding to the first run.
#' @param standIDs a vector (or list) of character strings holding the stand IDs (for example: 
#'   \code{standIDs=c("id1","id2")} would load 2 stands, whereas: \code{standIDs="id1"} 
#'    would load just one. If NULL, all stands in the init table are loaded. 
#' @param stdInit a character string of the name of the standinit table you 
#'    want to use, the stands would be loaded from that table.
#' @param variant a 2 character string specifying the variant (required).  
#'    Ihe standID and the variant must match in the designated init table.
#' @return The new run uuid, NULL if not created.
#' @export
extnMakeRun <- function (prjDir=getwd(),title=NULL,standIDs=NULL,
   stdInit="FVS_StandInit", mgmtID=NULL, variant)
{
  if (missing(variant)) stop("variant required")
  if (dir.exists(prjDir)) prjDir=normalizePath(prjDir) else 
    stop("The specified project directory must exist.")
  dbfile = file.path(prjDir,"FVS_Data.db")
  if (!file.exists(dbfile)) stop ("FVS_Data.db must exist")
  if (file.exists(file.path(prjDir,"/projectIsLocked.txt"))) stop("project is locked")
  db = connectFVSProjectDB(prjDir)
  curRunNames=names(getFVSRuns(db))
  if (is.null(title)) title=nextRunName(curRunNames)
  title = mkNameUnique(title,curRunNames)
  fvsRun=mkfvsRun(title=title,uuid=uuidgen(),runScript="fvsRun",
                  FVSpgm=paste0("FVS",variant),
                  refreshDB=stdInit,startyr=format(Sys.time(), "%Y"))
  dbcon <- dbConnect(dbDriver("SQLite"),dbfile)
  on.exit({
    suppressWarnings(dbDisconnect(dbcon))
    suppressWarnings(dbDisconnect(db))
  })
  
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

    grps <- if (!is.null(fvsInit$GROUPS))
      scan(text=fvsInit[row,"GROUPS"], what=" ",quiet=TRUE) else c("All All_Stands")
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
  fvsRun$defMgmtID = if (is.null(mgmtID)) 
  {
    nruns=if ("FVSRuns" %in% dbListTables(db)) 
      dbGetQuery(db,"select count(*) from FVSRuns")[1,1] else 0
    nextMgmtID(nruns)
  } else mgmtID
  storeFVSRun(db,fvsRun)
  return(fvsRun$uuid)
}

#' Duplicate a run and assign the duplicate a new title and new management ID.
#'
#' Pass in a project directory and an existing runUUID, the duplicate's title
#' and new management ID, and the run is duplicated into the current project
#' or another project.
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID is the uuid of the run that will be duplicated.
#' @param dupTitle a character string with the duplicated run's title
#'   if null, the system generates the name.
#' @param dupMgmtID a character string with the duplicated run's management ID.
#'   if null, the system generates the ID.
#' @param toPrjDir if not null, the duplicate is placed in a different 
#'   project directory given by this argument. 
#' @return The new run uuid, NULL if not created.
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
  fvsRun=loadFVSRun(db,runUUID)
  if (is.null(fvsRun)) stop("runUUID run data not found.")
  prjs=getFVSRuns(dbo)  
  if (is.null(dupTitle)) dupTitle=nextRunName(names(prjs))
  dupTitle=mkNameUnique(dupTitle,names(prjs))
  fvsRun$title=dupTitle
  uuid=uuidgen()
  fvsRun$uuid=uuid
  fvsRun$defMgmtID = if (is.null(dupMgmtID)) nextMgmtID(length(prjs)) else dupMgmtID
  storeFVSRun(dbo,fvsRun)
  return(uuid)
} 


#' Convert an R object to a raw vector
#'
#' @param x is an R object that is convered to a raw
#' @return A raw vector representation of the object
#' @export
extnToRaw <- function(x) memCompress(serialize(x,NULL),type="gzip")

#' Convert a raw vector to an R object
#'
#' @param x is a raw that converted to an R object
#' @return The R object
#' @export
extnFromRaw = function(x) unserialize(memDecompress(x,type="gzip"))

#' Get FVS Runs
#'
#' Pass in a project directory and get back a data.fram of the FVS runs. 
#'
#' @param prjDir is the path name to the project directory, default is  
#'   current directory.
#' @return NULL if the project does not exist or if no runs exist, otherwise,
#' a data.frame with these columns:
#' * uuid of the run, 
#' * title of the run, 
#' * datetime the run was last saved. 
#' @export
extnListRuns <- function (prjDir=getwd())
{
  if (!dir.exists(prjDir)) return(NULL) 
  prjDir = normalizePath(prjDir)
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db))
  runs = getFVSRuns(db,asList=FALSE)
  if (!is.null(runs)) class(runs$time)= c('POSIXt','POSIXct')
  return(runs)
}

#' Delete FVS Runs
#'
#' Given a project directory and 1 or more run uuids, the function deletes
#' the runs. 
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUIDs a vector (or list) character strings holding the 1 or more 
#'   run uuids to be deleted.
#' @param delOutput if TRUE (the default) the data in FVSOut.db is also
#'   deleted.
#' @return a data.frame listing the uuid, title, and datetime of the remaining runs
#'   and NULL if the no runs exist created using [extnListRuns].
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
  return(extnListRuns())
}
       
#' Add Keyword Components to a run
#'
#' Given a project directory and a run uuid, a list of 
#' keywords components are added to groups or stands in the run.
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character string of 1 run uuid that is processed.
#' @param cmps is a list of keyword components that will be processed (in order, 
#'   also see [extnGetComponentKwds]). Each list member can be one of the following:
#'   * an fvsOL keyword component (of object of class fvsCmp which is internal to fvsOL), 
#'   * a raw vector (see [extnToRaw] and [extnFromRaw]) that can be transformed into an 
#'     fvsOL keyword component, or
#'   * a character string of keywords. Several lines of keywords can be separated
#'     by \\n (newline) chars to indicated muliple keywords. 
#'     If the list items are named, the component name is set to the corresponding
#'     list item name.
#' @param groups is a list of group names 
#'   to which the keyword components will be attached.
#' @param stands is a list of stand ids
#'   to which the keyword components will be attached.
#' 
#' @return The number of keyword components added to the run.
#' @export
extnAddComponentKwds <- function(prjDir=getwd(),runUUID,cmps,groups=NULL,stands=NULL)
{
  if (missing(runUUID)) stop("runUUID required")
  if (missing(cmps)) stop("cmps is required")
  if (class(cmps) %in% c("raw", "fvscmp", "character")) cmps=list(cmps)
  if (is.null(groups) && is.null(stands)) stop("groups or stands must be supplied")
  prjDir = normalizePath(prjDir)
  if (file.exists(file.path(prjDir,"/projectIsLocked.txt"))) stop("project is locked")
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db)) 
  fvsRun = loadFVSRun(db,runUUID)
  if (!exists("fvsRun")) stop("runUUID run data not found")
  if (attr(class(fvsRun),"package") != "fvsOL") stop("Don't recognize the loaded run object")
  # process the cmps. convert "raw" and/or "character" as needed.
  onames = names(cmps)
  nadd=0 
  for (i in 1:length(cmps))
  {
    cmp = switch(class(cmps[[i]]),
      "fvsCmp" = mkfvsCmp(cmps[[i]],uuid=uuidgen()),
      "raw" = 
      {                      
        x = extnFromRaw(cmps[[i]])
        if (class(x) != "fvsCmp") stop(paste0("A member of 'cmps' of type 'raw'",
                                       " could not be converted to a component"))
        x
      },                                                          
      "character" = 
      {
        title = onames[i]                                                          
        if (is.null(title) || nchar(title)==0) title=paste0("Added from external source (",i,")")
        mkfvsCmp(kwds = cmps[[i]], exten="base", title=title, kwdName="freeEdit",
         variant=substring(fvsRun$FVSpgm,4),uuid=uuidgen(),atag="k")
      }
    )  
    # process groups
    if (!is.null(groups))
    {
      for (grp in fvsRun$grps)
      {
        if (grp$grp %in% groups) 
        {                                                       
          grp$cmps = append(grp$cmps,cmp)
          nadd=nadd+1
        }
      }
    }
    # process stands
    if (!is.null(stands))
    {
      for (std in fvsRun$stands)
      {
        if (std$sid %in% stands) 
        {  
          std$cmps = append(std$cmps,cmp)
          nadd=nadd+1
        }
      }
    } 
  }
  if (nadd==0) return(0)
  storeFVSRun(db,fvsRun)  
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
#'   no changes are made): "Treelists", "Carbon", "Fire", "Dead", "CanProfile", 
#'   "SnagDet",  "StrClass", "CalibStats", "Climate", "Econ", "DM_Sz_Sum", 
#'   "RD_Sum",  "RD_Det", "RD_Beetle", "InvStats", "Regen", "KeepTextTables"
#' @param svsOut a vector of two character strings where the first is the
#'   plot shape ("round" or "square") and the second is the number of images
#'   of firelines, for example: svsOut=c("square",4). If only one value is given
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
  if (file.exists(file.path(prjDir,"/projectIsLocked.txt"))) stop("project is locked")
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db)) 
  fvsRun = loadFVSRun(db,runUUID)
  if (!exists("fvsRun")) stop("runUUID run data not loaded")
  if (attr(class(fvsRun),"package") != "fvsOL") stop("Don't recognize the loaded object")
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
      fvsRun$autoOut$autoOut=as.list(names(autoSets)[set])
      changed=TRUE
    }
  }
  if (!is.null(svsOut))
  {
    svsSets = list(shape=if (tolower(svsOut[1])=="round") "Round" else "Square")
    nfire=if (length(svsOut)>1) suppressWarnings(as.integer(svsOut[2])) else NA
    if (is.na(nfire)) nfire=4
    fvsRun$autoOut$svsOut = c(svsSets,nfire=nfire)
    changed=TRUE
  }
  if (!is.null(startyr)) {
    fvsRun$startyr=as.character(startyr)
    changed=TRUE
  }
  if (!is.null(endyr))   {
    fvsRun$endyr  =as.character(endyr) 
    changed=TRUE
  }
  if (!is.null(cyclelen)) {
    fvsRun$cyclelen=as.character(cyclelen) 
    changed=TRUE
  }
  if (!is.null(cycleat)) {
    fvsRun$cycleat=as.character(cycleat) 
    changed=TRUE
  }
  if (changed) 
  {
    storeFVSRun(db,fvsRun)
    return(runUUID)
  } 
  NULL
}  
                                                                                                  
#' Get FVS keyword components from a run
#'
#' Given a project directory and a run uuid, get the keyword components in the run. 
#' The keyword components can be returned as a raw vector, an fvsOL keyword component,
#' or just the text of the keywords depending on the returnType parameter value. Note that
#' the original components are never returned, copies are generated and returned.
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character vector of the run uuid that is processed
#' @param returnType requested where [see extnAddComponentKwds]: 
#'   * "fvsCmp" the components are returned as copies of original fvsCmp objects,
#'   * "raw" the components are returned a compressed raw data vectors suitable for storing
#'   in a database (see [[extnToRaw] and [extnFromRaw]).
#'   * "character" the keyword part of the components are returned as a vector of character strings.
#' @return A named list of two other named lists. The first named list 
#'   contains a named list of components attached to groups. The type of the items
#'   is depends on the value of returnType. The names of the items are take from the
#'   "titles" of components returned. The second named list is like the first but
#'   contains items attached to groups. The first named list is named "grpCmps" and
#'   the second "stdCmps". 
#' @export
extnGetComponentKwds <- function(prjDir=getwd(),runUUID,returnType="fvsCmp")
{
  if (missing(runUUID)) stop("runUUID required")
  if (! returnType %in% c("fvsCmp","raw","character")) stop ("invalid value for 'returnType'")
  prjDir = normalizePath(prjDir)
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db)) 
  fvsRun = loadFVSRun(db,runUUID)
  if (!exists("fvsRun")) stop("runUUID run data not loaded") 
  if (attr(class(fvsRun),"package") != "fvsOL") stop("the loaded FVS run was not recognized")
  togrps = list() 
  togrpsnames = c()
  for (grp in fvsRun$grps) for (ocmp in grp$cmps) 
  { 
    cmp = switch(returnType,
      "fvsCmp"= mkfvsCmp(ocmp,uuid=uuidgen()),
      "raw" =  I(list(extnToRaw(ocmp))), 
      "character" = ocmp$kwds
    )
    togrpsnames = c(togrpsnames,ocmp$title)
    togrps = append(togrps,cmp)
  }
  if (length(togrps)) names(togrps)=togrpsnames
  tostds = list() 
  tostdsnames = c()
  for (std in fvsRun$stands) for (ocmp in std$cmps) 
  {
    cmp = switch(returnType,
      "fvsCmp"= mkfvsCmp(ocmp,uuid=uuidgen()),
      "raw" =  I(list(extnToRaw(ocmp))), 
      "character" = ocmp$kwds
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
#' @return The number of deletions.
#' @export
extnDeleteComponents <- function(prjDir=getwd(),runUUID,compUUIDs)
{
  if (missing(runUUID)) stop("runUUID required")
  if (missing(compUUIDs)) stop("compUUIDs required")
  prjDir = normalizePath(prjDir)
  if (file.exists(file.path(prjDir,"/projectIsLocked.txt"))) stop("project is locked")
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db)) 
  fvsRun = loadFVSRun(db,runUUID)
  if (!exists("fvsRun")) stop("runUUID run data not loaded")
  if (attr(class(fvsRun),"package") != "fvsOL") stop("Don't recognize the loaded object")
  changed = 0
  for (grp in fvsRun$grps)
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
  for (std in fvsRun$stands)
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
  if (changed) storeFVSRun(db,fvsRun)
  return(changed)
}


#' Generate the FVS .key file for a run.
#'
#' Given a project directory and a run uuid, generate the FVS .key file and, 
#' optionally, an RScript file suitable for running the run. The name 
#' of the .key file will be the runUUID followed by .key and it will be created
#' in the project file. The Rscript file name will be the runUUID followed by .RScript.
#' See [extnSimulateRun] for running the simulation.
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character vector of the run uuid that is processed
#' @param fvsBin is the name of the directory that contains the FVSBin
#' @param keyFileName is the name of the keyword file, the default is the
#'    the runUUID with .key suffix.
#' @param verbose turns on some extra output to the console
#' @return A text string reporting the nmber of stands and keyword file name.
#' @export
extnMakeKeyfile <- function(prjDir=getwd(),runUUID,fvsBin="FVSBin",
  keyFileName=NULL,verbose=FALSE)
{
  if (!dir.exists(fvsBin)) fvsBin=file.path(prjDir,fvsBin)
  if (!dir.exists(fvsBin)) stop("fvsBin can not be located.")   
  globals=new.env()
  globals$fvsBin=fvsBin
  resetActiveFVS(globals)
  db = connectFVSProjectDB(prjDir)
  globals$fvsRun = loadFVSRun(db,runUUID)
  dbDisconnect(db)
  prjDir = normalizePath(prjDir)
  prjDB = file.path(prjDir, "FVSProject.db")
  db=dbConnect(SQLite(), dbname = "FVS_Data.db")
  rtn = writeKeyFile(globals,db,newSum=TRUE,keyFileName,verbose=verbose)
  dbDisconnect(db) 
  rtn
}                                                                    


#' Given a project directory a run uuid, this function returns a list
#' of the stands in the run.
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character string of the run uuid that is processed
#' @return a vector of stand ids that are in the run.
#' @export
extnListStands <- function(prjDir=getwd(),runUUID)
{
  if (missing(runUUID)) stop("runUUID required")
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db)) 
  fvsRun = loadFVSRun(db,runUUID)
  if (!exists("fvsRun")) stop("runUUID run data not loaded")
  stands = c()
  for (std in fvsRun$stands) stands=c(stands,std$sid)
  return(stands)
}

#' Fetch a run
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character string of the run uuid that is fetched
#' @return the run.
#' @export
extnLoadFVSRun <- function(prjDir=getwd(),runUUID)
{
  if (missing(runUUID)) stop("runUUID required")
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db))
  loadFVSRun(db,runUUID)
}


#' Store a run
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param theRun an object of class fvsRun.
#' @return return value from data base action.
#' @export
extnStoreFVSRun <- function(prjDir=getwd(),theRun)
{
  if (missing(theRun)) stop("theRun required")
  if (class(theRun) != "fvsRun") stop ("fvsRun is not of class fvsRun")
  db = connectFVSProjectDB(prjDir)
  on.exit(dbDisconnect(db))
  runUUID=theRun$uuid
  rowid=0
  if ("FVSRuns" %in% dbListTables(db))
  {
    rowid=dbGetQuery(db,paste0("select rowid from FVSRuns where (uuid='",runUUID,"')"))      
    rowid=if (nrow(rowid)) rowid[1,1] else 0
  } else dbExecute(db, "create table FVSRuns (uuid text, name text, time integer, run blob)")
  if (rowid) dbExecute(db,paste0("delete from FVSRuns where (rowid=",rowid,");"))
  prjs=getFVSRuns(db)
  theRun$title=mkNameUnique(theRun$title,names(prjs))
  if (nchar(trim(theRun$defMgmtID))==0) theRun$defMgmtID=nextMgmtID(length(prjs))
  df = data.frame(uuid=theRun$uuid,name=theRun$title,time=as.integer(Sys.time()),
       run=I(list(extnToRaw(theRun))))
  rtn = dbExecute(db,paste0("insert into FVSRuns (uuid,name,time,run) values ",
                 "((:uuid), (:name), (:time), (:run))"), params=df)
  rtn
}


#' Add Stands to a run
#'
#' Given a project directory and a run uuid, a list of 
#' stands are added to the run. Stands that are already in the
#' run are not added.
#'
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character string of 1 run uuid that is processed.
#' @param stands a vector (or list) of stand ids that will be added.
#' @addStandsRegardless, if TRUE, stands are added even if they are already 
#'   present.
#' @return The number of stands, groups, and components added to the run 
#' @export
extnAddStands <- function(prjDir=getwd(),runUUID,stands,
   addStandsRegardless=FALSE)
{
  if (missing(runUUID) || is.null(runUUID)) stop("runUUID required")
  if (missing(stands) || is.null(stands)) stop("stands is required") 
  if (length(stands)==0) stop("stands list is empty")
  prjDir = normalizePath(prjDir)
  if (file.exists(file.path(prjDir,"/projectIsLocked.txt"))) stop("project is locked")
  dbfile = file.path(prjDir,"FVS_Data.db")
  if (!file.exists(dbfile)) stop ("FVS_Data.db must exist")
  db = connectFVSProjectDB(prjDir)
  fvsRun=loadFVSRun(db,runUUID)                       
  if (is.null("fvsRun")) stop("run data not found")
  if (attr(class(fvsRun),"package") != "fvsOL") stop("Don't recognize the loaded run object")
  dbcon <- dbConnect(dbDriver("SQLite"),dbfile)            
  on.exit({
    try(dbExecute(dbcon,'drop table if exists temp.Stds'))
    suppressWarnings(dbDisconnect(dbcon))           
    suppressWarnings(dbDisconnect(db))
  })
  nadd=list(nstd=0,ngrps=0,ncmps=0)               
  stdInit = fvsRun$refreshDB
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
  if (length(fields) < length(allNeed)) stop("required db fields are missing")

  getStds = data.frame(getStds=if (addStandsRegardless) stands else setdiff(stands,
            unlist(lapply(fvsRun$stands,function(x) x$sid))))
  if (nrow(getStds) == 0) return(nadd)
    
  dbWriteTable(dbcon,name=DBI::SQL("temp.getStds"),value=getStds,overwrite=TRUE)
  variant = substring(fvsRun$FVSpgm,4)
  dbExecute(dbcon,'drop table if exists temp.Stds')
  qry = paste0("select ",paste(fields,collapse=",")," from ",stdInit, 
    ' where lower(variant) like "%',tolower(variant),'%" and "',sidid,
    '" in (select getStds from temp.getStds);')
  fvsInit = try(dbGetQuery(dbcon,qry))
  if (class(fvsInit)=="try-error") stop("stdinit query error")
  fvsInit=na.omit(fvsInit)
  if (nrow(fvsInit) == 0) return(nadd)
  names(fvsInit) = toupper(names(fvsInit))
  grpAddKey=try(dbReadTable(dbcon,"FVS_GroupAddFilesAndKeywords"))
  if (class(grpAddKey)=="try-error" || nrow(grpAddKey)==0) grpAddKey=NULL else
    names(grpAddKey) = toupper(names(grpAddKey))
 
  for (row in 1:nrow(fvsInit))  # the selectInput list               
  {                                                      
    sid = fvsInit[row,toupper(sidid)]  
    newstd <- fvsOL:::mkfvsStd(sid=sid,uuid=uuidgen(),rep=0,repwt=1)
    addfiles = fvsInit[row,"ADDFILES"]
    for (addf in names(addfiles))                            
    {                                                  
      nadd$ncmps=nadd$ncmps+1
      newstd$cmps <- append(newstd$cmps,
               mkfvsCmp(kwds=addfiles[[addf]],uuid=uuidgen(),variant=variant,
                 exten="base", atag="k",kwdName=paste0("AddFile: ",addf),
                 title=paste0("AddFile: ",addf)))  
    }
    addkeys <- fvsInit[row,"FVSKEYWORDS"]
    if (!is.null(addkeys) && !is.na(addkeys) && nchar(addkeys) && addkeys != "NA") 
    {
      nadd$ncmps=nadd$cmps+1
      newstd$cmps <- append(newstd$cmps,mkfvsCmp(kwds=addkeys,uuid=uuidgen(),
        exten="base", atag="k",kwdName=paste0("From: ",stdInit),variant=variant, 
        title=paste0("From: ",stdInit)))
    }            
    grps <- if (!is.null(fvsInit$GROUPS))
         scan(text=fvsInit[row,"GROUPS"],
              what=" ",quiet=TRUE) else c("All All_Stands")              
    requ <- unlist(grps[grep("^All",grps)])
    have <- unlist(lapply(fvsRun$grps,function(x) x$grp))
    need <- setdiff(grps, have)
    for (grp in need) 
    {
      nadd$grps = nadd$grps+1
      newgrp <- mkfvsGrp(grp=grp,uuid=uuidgen())              
      grprow <- if (!is.null(grpAddKey)) 
        grep(grp,grpAddKey[,"GROUPS"],fixed=TRUE) else c()
      for (grow in grprow)
      {
        addkeys <- grpAddKey[grow,"FVSKEYWORDS"]
        if (!is.null(addkeys) && !is.na(addkeys))
        {
          nadd$ncmps=nadd$cmps+1
          newgrp$cmps[[length(newgrp$cmps)+1]] <- 
            mkfvsCmp(kwds=addkeys,uuid=uuidgen(),atag="k",exten="base",
                     kwdName="From: FVS_GroupAddFilesAndKeywords",
                       title="From: FVS_GroupAddFilesAndKeywords")
        }
        addfiles <- AddFiles(grpAddKey[grow,"ADDFILES"])
        for (addf in names(addfiles)) 
        {
          nadd$ncmps=nadd$cmps+1
          newgrp$cmps[[length(newgrp$cmps)+1]] <-
          mkfvsCmp(kwds=as.character(addfiles[addf]),uuid=uuidgen(),atag="k",exten="base",
            kwdName=paste0("AddFile: ",addf),title=paste0("AddFile: ",addf))
        }           
      }
      fvsRun$grps <- append(fvsRun$grps,newgrp)
    }
    invyr <- as.numeric(fvsInit[row,"INV_YEAR"])
    if (invyr > as.numeric(fvsRun$startyr)) fvsRun$startyr <- as.character(invyr)
    newstd$invyr <- as.character(invyr)
    have <- unlist(lapply(fvsRun$grps,function(x) if (x$grp != "") x$grp else NULL))
    newstd$grps <- fvsRun$grps[sort(match(grps,have))]    
    fvsRun$stands <- append(fvsRun$stands,newstd)
    nadd$nstd = nadd$nstd+1
  }  
  if (any(nadd>0)) 
  {
    if (addStandsRegardless) updateReps(fvsRun)
    storeFVSRun(db,fvsRun)
  }
  nadd
}

#' Simulate (run) a run's .key and .RScript
#'
#' Given a project directory and a run uuid, start the simulation that was
#' created using function [extnMakeKeyfile]. 
#' @param prjDir is the path name to the project directory, if null the 
#'   current directory is the project directory.
#' @param runUUID a character vector of the run uuid .key created by [extnMakeKeyfile].
#' @param fvsBin is the name of the directory that contains the FVSBin
#' @param ncpu is the max number of CPUs to use. When > 1, the run is broken into
#'   parts and simulated using [parallel::parallel-package] in a set of R processes. 
#'   The default is return value of [parallel::detectCores()].
#' @param keyFileName use this keyword file if specified, otherwise one is built.
#' @param wait if TRUE, the function does not return until the run has finished
#'   otherwise the run is started in background.
#' @param verbose when true, extra informative output is sent to the console.
#' @return the system PID of the process that is started when wait is FALSE, otherwise
#'   NULL.
#' @export
extnSimulateRun <- function(prjDir=getwd(),runUUID,fvsBin="FVSBin",ncpu=detectCores(),
         keyFileName=NULL,wait=FALSE,verbose=TRUE)
{
#for testing:
#prjDir=getwd();runUUID=extnListRuns()[1,1];fvsBin="FVSBin";ncpu=detectCores()
#keyFileName=NULL;wait=FALSE;verbose=TRUE

  curdir=getwd()
  if (missing(runUUID)) stop("runUUID required")
  setwd(prjDir)
  db = connectFVSProjectDB(prjDir)
  on.exit({
    dbDisconnect(db)
    setwd(curdir)
  })
  fvsRun = loadFVSRun(db,runUUID)
  if (!exists("fvsRun")) stop("runUUID run data not loaded")
  killIfRunning(runUUID)
  removeFVSRunFiles(runUUID)
  if (!is.null(keyFileName) && !file.exists(keyFileName)) 
    stop (paste0("keyFileName=",keyFileName," does not exist.")) 
  if (is.null(keyFileName)) 
  {
    keyFileName=paste0(runUUID,".key")
    cat("keyFileName=",keyFileName," is being created.")
    extnMakeKeyfile(runUUID=runUUID,fvsBin=fvsBin,
                    keyFileName=keyFileName,verbose=verbose)
  }
  # process .key file into ncpu sets.
  nstnds = length(fvsRun$stands)
  kwds = readLines(keyFileName)
  pkwds = grep ("^Process$",kwds)
  if (nstnds != length(pkwds)) stop("Not all stands in run are present in the keyword file.")
  ncpu = min(length(pkwds),ncpu) 
  sets=paste0("-set",1:ncpu)
  asign = suppressWarnings(split(1:nstnds,sets))
  # adjust location of the input database in the keyword file.  
  indb=grep ("FVS_Data.db$",kwds)
  if (length(indb)) kwds[indb]=paste0("../",kwds[indb])
  clindx=1
  for (set in names(asign))
  {
    rundir=paste0(runUUID,set)
    if (dir.exists(rundir)) unlink(rundir,force=TRUE,recursive=TRUE)
    dir.create(rundir)
    opnout = file(file.path(rundir,keyFileName),open="wt")
    cat(file=opnout,kwds[1:3],sep="\n")
    for (i in asign[[set]])
    {
      fl = if(i==1) 4 else pkwds[i-1]+1
      cat(file=opnout,kwds[fl:pkwds[i]],sep="\n",append=TRUE)
    }
    cat(file=opnout,"\nStop\n",append=TRUE)
    close(opnout)
    # make the run script
    opnout = file(file.path(rundir,sub(".key$",".Rscript",keyFileName)),open="wt")
    cat ("library(rFVS)\n",file=opnout)
    cat ("fvsLoad('",fvsRun$FVSpgm,"',bin='../",fvsBin,"')\n",sep="",file=opnout)    
    if (fvsRun$runScript != "fvsRun")
    {   
       # if the custom run script exists in the project dir, use it, otherwise
       # look in the system extdata directory to find it in the package
       cmdfil=paste0("customRun_",fvsRun$runScript,".R")
       if (!file.exists(cmdfil)) cmdfil=system.file("extdata", cmdfil, package = "fvsOL")
       if (file.exists(paste=cmdfil))
       {
         cat ("curdir=getwd();setwd('..')\n",file=opnout)
         cat ("source('",cmdfil,"')\n",sep="",file=opnout)
         cat ("setwd(curdir)\n",file=opnout)
         cat (paste0("uiCustomRunOps=",paste0(deparse(fvsRun$uiCustomRunOps),
              collapse=""),"\n"),sep="",file=opnout)
       }                                    
    }
    cat('fvsSetCmdLine("--keywordfile=',paste0(fvsRun$uuid,'.key")\n'),sep="",file=opnout)
    runCmd = if (fvsRun$runScript == "fvsRun") "fvsRun()" else
             paste0(fvsRun$runScript,"(uiCustomRunOps)")
    runCmd = paste0("while (",runCmd," != 2)")
    cat(runCmd,"\n",file=opnout)
    cat('unlink(paste0("',runUUID,'","_genrpt.txt"))\n',sep="",file=opnout)
    clindx=clindx+1
    close(opnout)
  }
  rscript=paste0(runUUID,".Rscript")
  pidStat=paste0(runUUID,".pidStatus")
  cat ('cat (Sys.getpid()," Running ',fvsRun$title,' on ',ncpu,' processes; started ",',
       'date(),"\\n",sep="",file="',pidStat,'")\n',sep="",file=rscript)
  cat('require(fvsOL)\n',file=rscript,append=TRUE)
  cat('fvsprocs = makePSOCKcluster(',ncpu,')\n',sep="",file=rscript,append=TRUE)
  cat('pids = unlist(clusterEvalQ(fvsprocs,Sys.getpid()))\n',sep="",file=rscript,append=TRUE)
  cat('cat ("fvsPids:",pids,"\\n",file="',pidStat,'",append=TRUE)\n',sep="",file=rscript,append=TRUE)
  cat('clusterEvalQ(fvsprocs,library(fvsOL))\n',sep="",file=rscript,append=TRUE)
  for (i in 1:ncpu) cat('clusterEvalQ(fvsprocs[',i,'],setwd("',paste0(runUUID,names(asign)[i]),'"))\n',
        sep="",file=rscript,append=TRUE)     
  cat ('try(clusterEvalQ(fvsprocs,source("',runUUID,'.Rscript")))\n',sep="",file=rscript,append=TRUE)
  cat ('stopCluster(fvsprocs)\n',file=rscript,append=TRUE) 
  rdirs=paste0(runUUID,names(asign))
  cat (paste0("rundirs=",paste0(deparse(rdirs),collapse=""),"\n"),sep="",append=TRUE,file=rscript)
  cat (paste0("runUUID=",paste0(deparse(runUUID),collapse=""),"\n"),sep="",append=TRUE,file=rscript)
  cat ('
    out=paste0(runUUID,".out")
    first=TRUE
    dbcon=dbConnect(SQLite(), dbname = "FVSOut.db")
    for (rundir in rundirs)
    {
      if (dir.exists(rundir)) 
      {
        frm=file.path(rundir,paste0(runUUID,".out"))
        if (file.exists(frm)) 
          if (file.exists(out)) file.append(out,frm) else file.copy(from=frm,to=out)
        setwd(rundir)
        addNewRun2DB(runUUID, dbcon, removeOldOutput=first, verbose=TRUE)
        first=FALSE
        setwd("..")
      }
    }
    dbDisconnect(dbcon)\n',
  file=rscript,append=TRUE) 
  cat ('file.remove("',paste0(runUUID,".pidStatus"),'")\n',sep="",file=rscript,append=TRUE)
  rsloc = if (exists("RscriptLocation")) RscriptLocation else "Rscript"
  cmd = paste0(rsloc," --no-restore --no-save --no-init-file ",rscript,
                       " > ",rscript,".Rout")
  if (.Platform$OS.type == "unix") cmd = paste0("nohup ",cmd)
  if (verbose) cat("rsloc=",rsloc," rscript=",rscript," wait=",wait,"\ncmd=",cmd,"\n")
  system (cmd,wait=wait) 
}  
  
  