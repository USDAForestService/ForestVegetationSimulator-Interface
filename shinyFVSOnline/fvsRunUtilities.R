 
if (exists("mkfvsStd")) rm (mkfvsStd)
mkfvsStd <<- setRefClass("fvsStd",
  fields = list(sid = "character", rep = "numeric", invyr = "character",
    grps = "list", cmps = "list",uuid="character"))

if (exists("mkfvsGrp")) rm (mkfvsGrp)
mkfvsGrp <<- setRefClass("fvsGrp",
  fields = list(grp = "character", cmps = "list", uuid="character"))

if (exists("mkfvsCmp")) rm (mkfvsCmp)
mkfvsCmp <<- setRefClass("fvsCmp",
  fields = list(kwds = "character", kwdName = "character", exten="character",
    title="character", variant="character",uuid="character", atag="character", 
    reopn="character"))
# atag is always "c" if the component is a condition, "k" if it is a keyword
# component that is not attached to a specific component. If it is longer than 1
# character it is the uuid of the related condition

if (exists("mkfvsRun")) rm (mkfvsRun) 
mkfvsRun <<- setRefClass("fvsRun", 
  fields = list(stands = "list", grps = "list", simcnts = "list",
    selsim = "list", FVSpgm = "character", title = "character", 
    startyr = "character", endyr = "character", cyclelen = "character",
    cycleat = "character", refreshDB = "character", uuid="character",
    defMgmtID = "character", autoOut = "list", runScript = "character" ,
    uiCustomRunOps = "list"))

if (exists("mkglobals")) rm(mkglobals)
mkglobals <<- setRefClass("globals", 
  fields = list(activeFVS = "list", activeVariants = "character", 
    activeExtens = "character", schedBoxYrLastUsed = "character",
    extnsel = "character", kwdsel = "list", mgmtsel = "list",
    moutsel = "list", mmodsel = "list", pastelist = "list",
    pastelistShadow = "list", inData = "list", FVS_Runs = "list",
    selStdList = "character", selVarList = "list", customCmps = "list",
    schedBoxPkey = "character", currentCmdPkey = "character",
    currentCndPkey = "character", winBuildFunction = "character", 
    existingCmps = "list",currentQuickPlot = "character", 
    currentEditCmp = "fvsCmp", NULLfvsCmp = "fvsCmp", saveOnExit= "logical",
    autoPanNav = "logical"))

# load (and/or build) the prms.RData object
mtrd <-  if (file.exists("prms.RData")) 
  as.integer(file.info("prms.RData")["mtime"]) else 0
mtpm <-  as.integer(file.info("suppose.prm")["mtime"])
if (mtrd < mtpm) 
{
  source("mkpkeys.R")
  prms <-  rdparms()
  prms <-  lapply(prms,mkpkeys)
  save(prms,file="prms.RData")
  rm (prms)
}

loadInvData <- function(globals,prms)
{
  if (!file.exists("FVS_Data.db"))
  {
    if (file.exists("FVS_Data.db.backup")) 
        file.rename("FVS_Data.db.backup","FVS_Data.db") else
        file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
  }
  library(RSQLite)
  dbDrv <- dbDriver("SQLite")
  con <- dbConnect(dbDrv,"FVS_Data.db")
  tbs <- dbListTables(con)
  itab <- grep (tolower("FVS_StandInit"),tolower(tbs))
  if (length(itab)) 
  {
    globals$inData$FVS_StandInit <- dbReadTable (con,tbs[itab])
    names(globals$inData$FVS_StandInit) <- 
          toupper(names(globals$inData$FVS_StandInit))
    if (is.null(globals$inData$FVS_StandInit$GROUPS))
       globals$inData$FVS_StandInit$GROUPS = "All All_Stands"   
  } 
  itab <- grep (tolower("FVS_PlotInit"),tolower(tbs))
  if (length(itab)) 
  {
    globals$inData$FVS_PlotInit <- dbReadTable (con,tbs[itab])
    names(globals$inData$FVS_PlotInit) <- 
          toupper(names(globals$inData$FVS_PlotInit))
    if (is.null(globals$inData$FVS_PlotInit$GROUPS))
       globals$inData$FVS_PlotInit$GROUPS = "All All_Plots"   
  } 
  itab <- grep (tolower("FVS_GroupAddFilesAndKeywords"),tolower(tbs))
  if (length(itab)) 
  {
    globals$inData$FVS_GroupAddFilesAndKeywords <- dbReadTable (con,tbs[itab])
    names(globals$inData$FVS_GroupAddFilesAndKeywords) <- 
          toupper(names(globals$inData$FVS_GroupAddFilesAndKeywords))
  } 
  dbDisconnect(con)
  if (nrow(globals$inData$FVS_StandInit))
  {
    globals$selStdList <- globals$inData$FVS_StandInit$STAND_ID
    names(globals$selStdList) <- globals$inData$FVS_StandInit$STAND_ID
    selVars <- as.list(sort(unique(unlist(lapply 
                  (tolower(globals$inData$FVS_StandInit$VARIANT),
                    function (x) scan(text=x,what=" ",quiet=TRUE))))))
    globals$selVarList <- lapply(selVars,function (x,pk) 
        paste(x,":",getPstring(pk,x)),prms$variants)
    names(globals$selVarList) <- selVars
  }
}


asList <- function(rc)
{
  cpcmp <- function (incmp)
  {
    cmpnames <- names(mkfvsCmp$fields())
    cmp <- vector("list",length(cmpnames))
    names(cmp) = cmpnames
    for (name in cmpnames) cmp[[name]] = incmp[[name]]
    cmp
  }
  onames   <- names(mkfvsRun$fields())
  stdnames <- names(mkfvsStd$fields())
  grpnames <- names(mkfvsGrp$fields())
  out <- vector("list",length(onames))
  names(out) = onames
  for (name in onames) out[[name]] = rc[[name]]
  if (length(rc$stands)) for (i in 1:length(rc$stands))
  {
    std <- vector("list",length(stdnames))
    names(std) = stdnames
    for (name in stdnames) std[[name]] = rc$stands[[i]][[name]]
    if (length(std$cmps)) for (cp in 1:length(std$cmps))
      std$cmps[[cp]] = cpcmp(std$cmps[[cp]])
    if (length(std$grps)) for (gr in 1:length(std$grps))
      std$grps[[gr]] = std$grps[[gr]]$uuid
    out$stands[[i]] = std
  }
  if (length(rc$grps)) for (i in 1:length(rc$grps))
  {
    grp <- vector("list",length(grpnames))
    names(grp) = grpnames
    for (name in grpnames) grp[[name]] = rc$grps[[i]][[name]]
    if (length(grp$cmps)) for (cp in 1:length(grp$cmps))
      grp$cmps[[cp]] = cpcmp(grp$cmps[[cp]])
    out$grps[[i]] = grp
  }
  attr(out,"time") = as.integer(Sys.time())
  out  
}

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
  if (length(list) == 0) return(NULL)
  for (i in 1:length(list)) if (list[[i]]$uuid == uuid) return (i)
  NULL
}


loadFromList <- function(refc,alist)
{
  for (name in names(mkfvsRun$fields()))
    if (!is.null(alist[[name]])) refc[[name]] = alist[[name]]
  if (length(alist$grps)) for (i in 1:length(alist$grps))
  {
    refc$grps[[i]] = mkfvsGrp()
    for (name in names(mkfvsGrp$fields()))
      if (!is.null(alist$grps[[i]][[name]])) refc$grps[[i]][[name]] = 
         alist$grps[[i]][[name]]    
    if (length(alist$grps[[i]]$cmps)) for (j in 1:length(alist$grps[[i]]$cmps))
    {
      refc$grps[[i]]$cmps[[j]] = mkfvsCmp()
      for (name in names(mkfvsCmp$fields()))
        if (!is.null(alist$grps[[i]]$cmps[[j]][[name]])) 
          refc$grps[[i]]$cmps[[j]][[name]] = alist$grps[[i]]$cmps[[j]][[name]]
    }
  }  
  if (length(alist$stands)) for (i in 1:length(alist$stands))
  {
    refc$stands[[i]] = mkfvsStd()
    for (name in names(mkfvsStd$fields()))
      if (!is.null(alist$stands[[i]][[name]])) refc$stands[[i]][[name]] = 
         alist$stands[[i]][[name]]
    if (length(alist$stands[[i]]$cmps)) for (j in 1:length(alist$stands[[i]]$cmps))
    {
      refc$stands[[i]]$cmps[[j]] = mkfvsCmp()
      for (name in names(mkfvsCmp$fields()))
        if (!is.null(alist$stands[[i]]$cmps[[j]][[name]])) 
          refc$stands[[i]]$cmps[[j]][[name]] = alist$stands[[i]]$cmps[[j]][[name]]
    }
    if (length(alist$stands[[i]]$grps)) for (j in 1:length(alist$stands[[i]]$grps))
    {
      idx = findIdx(refc$grps,alist$stands[[i]]$grps[[j]])
      if (!is.null(idx)) refc$stands[[i]]$grps[[j]] = refc$grps[[idx]]
    }
  }
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
}

trim <- function (x) gsub("^\\s+|\\s+$","",x)

mkpair <- function(alist)
{
cat("mkpair, length(alist)=",length(alist),"\n")
  l <- as.list(as.character(1:length(alist)))
  names(l) <- names(alist)
  l
}
  

removeFVSRunFiles = function (uuid)
{
  if (file.exists(uuid)) 
  {
    file.remove(paste0(uuid,"/",dir(uuid)))
    file.remove(uuid)
  }
  file.remove(dir(pattern=uuid))
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


writeKeyFile <- function (fvsRun,fvsInit,prms)
{
cat("writeKeyFile, fvsRun$title=",fvsRun$title," uuid=",fvsRun$uuid,"\n")
  extns = attr(prms$programs[prms$programs == fvsRun$FVSpgm][[1]],"atlist")
  source("autoOutKeys.R")
  if (!is.na(match("mist",extns))) defaultOutMist=NULL
  fc = file(description=paste0(fvsRun$uuid,".key"),open="wt")
  cat ("!!title:",fvsRun$title,"\n",file=fc)
  cat ("!!uuid: ",fvsRun$uuid,"\n",file=fc)
  cat ("!!built:",format(Sys.time(), 
        "%Y-%m-%d_%H:%M:%S"),"\n",file=fc)
  thisYr = as.numeric(format(Sys.time(), "%Y"))
  baseCycles = seq(as.numeric(fvsRun$startyr),as.numeric(fvsRun$endyr),
                   as.numeric(fvsRun$cyclelen))
  cycleat = scan(text=gsub(";"," ",gsub(","," ",fvsRun$cycleat)),
                 what=0,quiet=TRUE)
  cycleat = sort(union(baseCycles,cycleat))
  cycleat = sort(union(cycleat,as.numeric(fvsRun$endyr)))
  for (std in fvsRun$stands)
  {
    sRow = match (std$sid, fvsInit$STAND_ID)
    if (is.na(sRow)) next
    cat ("StdIdent\n",std$sid," ",fvsRun$title,"\n",file=fc,sep="")
    if (!is.null(fvsInit$STAND_CN[sRow])) 
      cat ("StandCN\n",fvsInit$STAND_CN[sRow],"\n",file=fc,sep="")
    cat ("MgmtId\n",if (length(std$rep)) sprintf("R%3.3d",std$rep) else 
        fvsRun$defMgmtID,"\n",file=fc,sep="")                       
    if (length(std$invyr) == 0) std$invyr = as.character(thisYr) 
    ninvyr = as.numeric(std$invyr)
    cat ("InvYear       ",std$invyr,"\n",file=fc,sep="")
    thiscyc = union(seq(ninvyr,cycleat[1],as.numeric(fvsRun$cyclelen)),cycleat)
    ints = diff(sort(thiscyc))
    mostint = names(which.max(table(ints)))
    ints = as.character(ints)
    cat ("TimeInt                ",mostint,"\n",file=fc)
    for (i in 1:length(ints)) if (ints[i] != mostint) 
         cat ("TimeInt      ",as.character(i),"      ",ints[i],"\n",file=fc)
    cat ("NumCycle    ",as.character(i),"\n",file=fc)
    cat ("DelOTab            1\n",file=fc)
    cat ("DelOTab            2\n",file=fc)
    cat ("DelOTab            4\n",file=fc)
    cat (defaultOut,file=fc)
    if (!is.na(match("mist",extns))) cat (defaultOutMist,file=fc)
    for (out in fvsRun$autoOut)
    {
      switch (out,
       "autoTreelists"=cat(autoTreelists,file=fc),
       "autoCarbon"=cat(autoCarbon,file=fc),
       "autoFire"=cat(autoFire,file=fc),
       "autoDead"=cat(autoDead,file=fc))
    }
    lastExt = "base"
    lastCnd = NULL
    if (length(std$grps)) for (grp in std$grps)
    {
      if (length(grp$cmps)) for (cmp in grp$cmps)
      {
        if (cmp$atag == "k" && !is.null(lastCnd))
        {
          cat ("EndIf\n",file=fc,sep="")
          lastCnd = NULL
        }
        if (cmp$atag == "c") lastCnd = cmp$uuid
        if (lastExt != cmp$exten && lastExt != "base") 
        {
          lastExt = "base"
          cat ("End\n",file=fc,sep="")
        } 
        if (lastExt != cmp$exten)
        { 
          cat (getPstring(prms[["extensPrefixes"]],
               cmp$exten),"\n",file=fc,sep="")
          lastExt = cmp$exten
        }
        cat (cmp$kwds,"\n",file=fc,sep="")
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
      if (lastExt != cmp$exten && lastExt != "base") 
      {
        lastExt = "base"
        cat ("End\n",file=fc,sep="")
      } 
      if (lastExt != cmp$exten)
      {   
        cat (getPstring(prms[["extensPrefixes"]],
             cmp$exten),"\n",file=fc,sep="")
        lastExt = cmp$exten
      }
      cat (cmp$kwds,"\n",file=fc,sep="")
    }
    if (lastExt != "base") cat ("End\n",file=fc,sep="")
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
}


mkSimCnts <- function (fvsRun,sels=NULL)
{
  tmpcnts = list()
  tmptags = list()
  if (!is.null(sels)) if (length(sels) == 0) sels = NULL
cat("mkSimCnts, sels NULL?",is.null(sels))
  if (length(fvsRun$stands)) for (i in 1:length(fvsRun$stands))
  {
    tmpcnts<-append(tmpcnts, 
      if (length(fvsRun$stands[[i]]$rep) == 0) fvsRun$stands[[i]]$sid else 
          paste(fvsRun$stands[[i]]$sid,"rep",fvsRun$stands[[i]]$rep))
    tmptags <- append(tmptags,fvsRun$stands[[i]]$uuid)
    if (length(fvsRun$stands[[i]]$grps) > 0)
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
    if (length(fvsRun$stands[[i]]$cmps) > 0)
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
      selset=unlist(sels) == unlist(tmptags)
      if (all(!selset)) selset=NULL
    }
    if (length(selset) == 0) selset <- grep(" Grp: All_",tmpcnts)
    if (length(selset) == 0) selset <- grep(" Grp: All",tmpcnts)
    if (length(selset) == 0) fvsRun$selsim <- list() else
    {
       fvsRun$selsim <- tmptags[selset]
       names(fvsRun$selsim) <- tmpcnts[selset]
    }
  }
  cat("...return\n")
}


resetGlobals <- function(globals,fvsRun,prms)
{
cat("resetGlobals, fvsRun NULL=",is.null(fvsRun),"\n")
  if (is.null(fvsRun))
  {
    shlibsufx <- if (.Platform$OS.type == "windows") "[.]dll$" else "[.]so$"
    binDir = if (file.exists("FVSbin/")) "FVSbin/" else fvsBinDir
    avalFVS <- dir(binDir,pattern=shlibsufx) 
    avalFVSp <- sub(shlibsufx,"",avalFVS)
    globals$activeExtens <- "base"
    if (length(avalFVSp) == 0) avalFVSp = "FVSiec"
    pgmNames = unique(unlist(prms[["programs"]]))
    if (length(avalFVSp)) for (i in 1:length(avalFVSp))
    {
      ats = grep(avalFVSp[i],pgmNames,fixed=TRUE)
      if (length(ats) == 0) next
      ats = attr(prms[["programs"]][prms[["programs"]] == avalFVSp[i]][[1]],
                "atlist",exact=TRUE)
      if (!is.null(ats)) 
      {
        globals$activeExtens <- union(globals$activeExtens, ats[-1])
        globals$activeVariants <- union(globals$activeVariants, ats[1])
        globals$activeFVS[[length(globals$activeFVS)+1]] <- ats
        names(globals$activeFVS)[length(globals$activeFVS)] <- 
              avalFVSp[i]
      }
    } 
    globals$schedBoxYrLastUsed <- character(0)
    globals$currentEditCmp <- globals$NULLfvsCmp
  } else
  { 
    globals$schedBoxYrLastUsed <- fvsRun$startyr
    if (length(fvsRun$FVSpgm) > 0 && 
        !is.null(globals$activeFVS[[fvsRun$FVSpgm]]))
    {
      globals$activeFVS <- globals$activeFVS[fvsRun$FVSpgm]
      globals$activeVariants <- globals$activeFVS[[1]][1]
      globals$activeExtens <- c("base",globals$activeFVS[[1]][-1])
    }
  }

cat ("globals$activeVariants=",globals$activeVariants,
     " globals$activeFVS[[1]][1]=", globals$activeFVS[[1]][1],"\n")
cat ("reset activeExtens= ");lapply(globals$activeExtens,cat," ");cat("\n")
  globals$extnsel <- character(0)  
  globals$schedBoxPkey <- character(0)  
  globals$currentCmdPkey <- "0"
  globals$currentCndPkey <- "0"
  globals$winBuildFunction <- character(0)
}


uuidgen <- function (n=1)
{
# generate a version 4 uuid using R's random number generator and a
# special seed/status.
# example: "36d3054f-553b-4f52-ac3f-b1a028f3dfa8"

# designed to make it very difficult to cause this generator
# to generate a duplicate...even when the user first calls set.seed
# and does not have package digest.

  ss <- if (exists(".Random.seed",envir=.GlobalEnv)) 
        get(".Random.seed",envir=.GlobalEnv) else NULL
  cp <- Sys.getpid()
  if (exists(".uuid.seedpid",envir=.GlobalEnv) &&
         get(".uuid.seedpid",envir=.GlobalEnv) == cp &&
      exists(".uuid.seed",   envir=.GlobalEnv))
    .Random.seed <<- get(".uuid.seed",envir=.GlobalEnv) else 
  {
    .uuid.seedpid <<- cp
    if (file.exists("/dev/random")) {
      rn <- file ("/dev/random",open="rb")
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
#cat("getPstring, pkey=",pkey," atag=",atag,"\n")
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
#cat (" ps1 to return=",ps,"\n")
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
#cat (" ps2 to return=",ps,"\n")
        return(ps)
      }
    }
    return (getPstring(pkeys,pkey,NULL))
  }
  return (NULL)
}
  

mkKeyWrd = function (ansFrm,input,pkeys,variant)
{
cat("mkKeyWrd, ansFrm=",ansFrm," input=",input,"\n")
  state=0
  out = NULL
  for (i in 1:nchar(ansFrm))
  {
    c = substr(ansFrm,i,i)
#cat("mkKeyWrd, c=",c," state=",state,"\n")
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
              "FALSE" = 1, 
              "TRUE"  = 2,
              as.numeric(input[fld]))
        fld = if (is.na(inp)) "*error*" else trns[inp]
        out = if (nchar(fldw) > 0) paste0(out,sprintf(fldw,fld)) else
              paste0(out,fld)
        state = 0 
        next
      }
    }
  }
cat ("mkKeyWrd, out=",out,"\n")
  trim(out)
}
  

mkcatsel <- function(prms,name,globals)
{
cat ("mkcatsel, name=",name," mgmtsel=",length(globals$mgmtsel),
 " moutsel=",length(globals$moutsel),
 " mmodsel=",length(globals$mmodsel),"\n") 
  if (is.null(prm <- prms[[name]])) return (NULL)
  catsel <- NULL
  for (i in 1:length(prm))
  {
    sel <- NULL
    lines <- scan(text=attr(prm[[i]],"pstring"),what=" ",sep="\n",quiet=TRUE)
    if (length(lines) < 2) next
    for (j in 2:length(lines))
    {
      ssl <- NULL
      sl <- unlist(strsplit(lines[j],";"))
      # skip those that have no action elements
      if (length(sl) < 2) next
      p2 <- unlist(strsplit(sl[2]," "))
      p2 <- unlist(lapply(p2,function (x) if (x=="") NULL else x))
      ex <- unlist(strsplit(p2[1],"&"))
      # skip those that require inactive extensions
      if (length(intersect(ex,globals$activeExtens)) == 0) next
      # skip those that are tagged to non-existent mstext 
      mstext <- p2[length(p2)]
      mstextNum <- match(mstext,names(prms))
      if (is.na(mstextNum))
      {
        # if the mstext exists, then it is a function to be called
        # but if it doesn't exist then we need to skip this entry
        if (!exists(mstext)) next 
      } else {
        mstext <- as.character(mstextNum)
      }
      ssl <- as.character(mstext)
      names(ssl) <- sl[1]
      ssl <- as.character(mstext)
      names(ssl) <- sl[1]
      if (!is.null(ssl)) sel <- append(sel,ssl)
    }
    if (!is.null(sel)) 
    {
      catsel <- append(catsel,list(sel))
      names(catsel)[length(catsel)] = lines[1]
    }
  }
  catsel
}


mkextkwd <- function(prms,globals)
{
cat("mkextkwd\n")
  extns <-  prms[["extensions"]]
  extnslist <-  as.list(unlist(lapply(extns,function (x,extns) 
                    getPstring(extns,x), extns)))
  extn <- extnslist[globals$activeExtens]
  kwds <- prms[["keyword_list"]]  
  globals$kwdsel <- vector("list",length(extn))
  names(globals$kwdsel) = names(extn)
  for (kwd in kwds)
  {
    ex <- attr(kwd,"atlist")
    ex <- if (length(ex) > 1 && ex[1] == "estb" && ex[2] == "strp") 
          "estbstrp" else ex[1] 
    keypn <- paste0("keyword.",ex,".",kwd)
    sk <- match(keypn,names(prms))
    if (is.na(sk)) next
    if (ex == "estbstrp") ex = if(is.null(extn$estb)) "strp" else "estb"
    entry <- as.character(sk)
    names(entry) <- paste0(kwd,": ",attr(kwd,"pstring"))
    globals$kwdsel[[ex]] <- append(globals$kwdsel[[ex]],entry)
  }  
  globals$extnsel <- names(extn)
  names(globals$extnsel) <- extn
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
          globals$pastelist <- append(globals$pastelist,toRm)
          globals$pastelistShadow <- append(globals$pastelistShadow,
            toRm$uuid)
          names(globals$pastelistShadow)[length(globals$pastelistShadow)] = 
            toRm$kwdName
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
          globals$pastelist <- append(globals$pastelist,toRm)
          globals$pastelistShadow <- append(globals$pastelistShadow,
            toRm$uuid)
          names(globals$pastelistShadow)[length(globals$pastelistShadow)] = 
            toRm$kwdName
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
  # remove a component from a grp... 
  if (length(fvsRun$grps)) for (i in length(fvsRun$grps):1)
  {
    if (length(fvsRun$grps[[i]]$cmps)) 
    {
      for (j in length(fvsRun$grps[[i]]$cmps):1)
      {
        if ((!is.null(item) && fvsRun$grps[[i]]$cmps[[j]]$uuid == item) || 
            (!is.null(atag) && fvsRun$grps[[i]]$cmps[[j]]$atag == atag))
        {
          toRm = fvsRun$grps[[i]]$cmps[[j]]
          globals$pastelist <- append(globals$pastelist,toRm)
          globals$pastelistShadow <- append(globals$pastelistShadow,toRm$uuid)
          names(globals$pastelistShadow)[length(globals$pastelistShadow)] = 
            toRm$kwdName
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
          globals$pastelist <- append(globals$pastelist,toRm)
          globals$pastelistShadow <- append(globals$pastelistShadow,
            toRm$uuid)
          names(globals$pastelistShadow)[length(globals$pastelistShadow)] = 
            toRm$kwdName
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
    # selected element is a component. Find the grp to which it is listed.
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
  tbs <- dbListTables(dbcon)
  icas <- grep ("FVS_Cases",tbs) 
  if (length(icas) == 0) return()
  cases = tbs[icas]
  tbs = tbs[-icas]
  tbs = c(tbs,cases) 
  casestodel = dbGetQuery(dbcon,paste0(
     "select CaseID from FVS_Cases where KeywordFile = '",runuuid,"'"))[,1]
  if (length(casestodel) == 0) return()
  for (tab in tbs)
  {
    dbBegin(dbcon)
    if (is.na(match("CaseID",dbListFields(dbcon,name=tab))))
      dbSendQuery(dbcon,paste0("drop table ",tab)) else
    { 
      for (casetodel in casestodel)  dbSendQuery(dbcon,
        paste0("delete from ",tab," where CaseID = '",casetodel,"'"))
      nr = dbGetQuery(dbcon,paste0("select count(*) from ",tab))[,1]
      if (nr == 0) dbSendQuery(dbcon,paste0("drop table ",tab))
    }
    dbCommit(dbcon)
  }
}


fixFVSKeywords <- function()
{
  dbDrv <- dbDriver("SQLite")
  dbcon <- dbConnect(dbDrv,"FVS_Data.db")
  tbs <- dbListTables(dbcon)
  for (tb in tbs)
  {
cat ("in fixFVSKeywords, tb=",tb,"\n")
    flds <- dbListFields(dbcon, tb)
    kwdsIdxs <- grep ("keywords",flds,ignore.case = TRUE)
    if (length(kwdsIdxs) == 0) next
    for (kwdname in flds[kwdsIdxs])
    {    
      qry = paste0("select _ROWID_,",kwdname," from ",tb,
        " where ",kwdname," is not null and ",kwdname," != '';")
cat ("qry=",qry,"\n")              
      res <- dbSendQuery(dbcon,qry)
      kwdf <- dbFetch(res, n=-1)
cat ("result nrow=",nrow(kwdf),"\n")      
      dbClearResult(dbcon)
      if (nrow(kwdf))
      {
        for (row in 1:nrow(kwdf))
        {
          if (nchar(kwdf[row,2]) < 2) {kwdf[row,1] = -1; next}
          one <- gsub ("\r","",kwdf[row,2])
          dsnin <- grep ("dsnin",one,ignore.case = TRUE)
          if (length(dsnin) == 0) {kwdf[row,1] = -1; next}
          one <- scan(text=one,sep="\n",what="character",quiet=TRUE)
          dsnin <- grep ("dsnin",one,ignore.case = TRUE)+1
          one[dsnin] <- "FVS_Data.db"
          kwdf[row,2] <- paste0(paste0(one,collapse="\n"),"\n")
        }
        kwdf = subset(kwdf,rowid > 0)
        if (nrow(kwdf) > 0)
        {
          dbBegin(dbcon)
          for (row in 1:nrow(kwdf))
          {
            qut <- if (length(grep("'",kwdf[row,2],fixed=TRUE))) "\"" else "'"
            qry <- paste0("update ",tb," set ",kwdname," = ",qut,
              kwdf[row,2],qut," where _ROWID_ = ",kwdf[row,1],";")
cat ("qry=",qry,"\n")              
             dbSendQuery(dbcon,qry)              
          }
          dbCommit(dbcon)
        }
      }
    }
  }
cat ("exit fixFVSKeywords\n")
  dbDisconnect(dbcon)  
}    

  

