externalMakeRun <- function (title=NULL,standIDs=NULL,stdInit="FVS_StandInit",
                   variant=NULL, keywords=NULL, group=NULL)
{
#
#  title = a character string with the run title, if null, the system generates 
#          the name, that is, it would be “Run 1”, … or “Run 2”, and so on for 
#          all the runs already present (if none are present, then the default 
#          run title with be “Run 1”).
#
#  standIDs = a vector of character strings holding the stand IDs (for example: 
#             standIDs=c(“id1”,”id2”) would load 2 stands, whereas: standIDs=”id1” 
#             would load just one. If NULL, all stands in the init table are loaded. 
#
#  stdInit = a character string of the name of the standinit table you 
#            want to use, the stands would be loaded from that table.
#
#  variant = a 2 character string specifying the variant. If NULL, and error. If not null, 
#            then the standID and the variant must match in the designated init table.
#
#  keywords = a named vector of character strings with the keywords 
#             you want added to the group specified in the next argument 
#             (see below): Here is an example:
#             keywords = c(“title of first keyword set” = “Keyword line 1\nKeyword line 2”, … “,
#                        “title of the second set” = “Keyword line 1\nKeyword line 2”, … “, … )
#             All of the keywords will be set to be owned by the "base", that is, 
#             if you include "extension" keywords, they must already have the 
#             necessary extension start keyword (ie, for the FFE, its: FFIN) 
#             and the necessary End.
#             If keywords is “NULL”, then no keywords are added.
#
#   group = a character string naming the group to which the keywords are attached. 
#           If NULL, then the keywords are attached to each stand separately. If
#           the group is not already in the input database, then the group is changed
#           to NULL and the keywords are each stand separately.
#           
#  assumptions: The current directory is the projecet directory and the name of the 
#  input data base is FVS_Data.db. 
#

  if (!file.exists("FVS_Data.db")) stop ("FVS_Data.db must exist")
  if (is.null(variant)) stop ("variant must be defined") 
  library(RSQLite)

  if (file.exists("FVS_Runs.RData")) load("FVS_Runs.RData") else FVS_Runs=list()
  if (is.null(title)) title=nextRunName(FVS_Runs)
  fvsRun=mkfvsRun(title=title,uuid=uuidgen(),runScript="fvsRun",
                  FVSpgm=paste0("FVS",variant),
                  refreshDB=stdInit,startyr=format(Sys.time(), "%Y"))

  dbIcon <- dbConnect(dbDriver("SQLite"),"FVS_Data.db")
  on.exit(expr = dbDisconnect(dbIcon))
  
  if (! stdInit %in% dbListTables(dbIcon)) stop(paste0(stdInit," not found in database."))
  
  fields = dbListFields(dbIcon,stdInit)
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
  dbExecute(dbIcon,'drop table if exists temp.Stds')
  qry = paste0("select ",paste(fields,collapse=",")," from ",stdInit, 
    ' where lower(variant) like "%',tolower(variant),'%"')
  if (!is.null(standIDs)) 
  {
    dbWriteTable(dbIcon,DBI::SQL("temp.Stds"),data.frame(SelStds = standIDs))
    qry = paste0(qry," and ",sidid," in (select SelStds from temp.Stds)")
  } 
  fvsInit = try(dbGetQuery(dbIcon,qry))

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
 
  FVS_GroupAddFilesAndKeywords = try(dbReadTable(dbIcon,"FVS_GroupAddFilesAndKeywords"))
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
  FVS_Runs <- append(FVS_Runs,fvsRun$title)
  names (FVS_Runs)[length(FVS_Runs)] <- fvsRun$uuid
  attr(FVS_Runs[[length(FVS_Runs)]],"time")=as.integer(Sys.time())  
  FVS_Runs=reorderFVSRuns(FVS_Runs)
  save(FVS_Runs,file="FVS_Runs.RData")
  saveFvsRun = fvsRun 
  save(saveFvsRun,file=paste0(fvsRun$uuid,".RData"))
  return(length(saveFvsRun$stands))
}





 


                                                                              