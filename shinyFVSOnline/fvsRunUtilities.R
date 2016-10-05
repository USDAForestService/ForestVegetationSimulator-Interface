 
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
    uiCustomRunOps = "list", startDisp = "character"))

if (exists("mkglobals")) rm(mkglobals)
mkglobals <<- setRefClass("globals", 
  fields = list(activeFVS = "list", activeVariants = "character", 
    activeExtens = "character", schedBoxYrLastUsed = "character",
    extnsel = "character", kwdsel = "list", mgmtsel = "list",
    moutsel = "list", mmodsel = "list", pastelist = "list",
    pastelistShadow = "list", inData = "list", FVS_Runs = "list",
    selVarList = "list", oldInAdd = "numeric", customCmps = "list",
    schedBoxPkey = "character", currentCmdPkey = "character",
    currentCndPkey = "character", winBuildFunction = "character", 
    existingCmps = "list",currentQuickPlot = "character", 
    currentEditCmp = "fvsCmp", NULLfvsCmp = "fvsCmp", saveOnExit= "logical",
    customQueries = "list", fvsRun = "fvsRun", foundStand="integer"))

loadVarData <- function(globals,prms,dbIcon)
{
  tbs <- dbListTables(dbIcon)
  itab <- grep (tolower("FVS_StandInit"),tolower(tbs)) 
  if (length(itab)) 
  {
    vars = dbGetQuery(dbIcon,'select distinct variant from FVS_StandInit')
    selVars = na.omit(unique(tolower(unlist(lapply(vars[,1],
      function (x) strsplit(x," "))))))
    globals$selVarList <- lapply(selVars,function (x,pk) 
        paste(x,":",getPstring(pk,x)),prms$variants)
    names(globals$selVarList) <- selVars
  } else globals$selVarList = list()
  itab <- grep (tolower("FVS_GroupAddFilesAndKeywords"),tolower(tbs))
  if (length(itab)) 
  {
    globals$inData$FVS_GroupAddFilesAndKeywords <- dbReadTable (dbIcon,tbs[itab])
    names(globals$inData$FVS_GroupAddFilesAndKeywords) <- 
          toupper(names(globals$inData$FVS_GroupAddFilesAndKeywords))
  } 
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
  fvsRun$startDisp = character(0)
}

trim <- function (x) gsub("^\\s+|\\s+$","",x)

mkpair <- function(alist)
{
cat("mkpair, length(alist)=",length(alist),"\n")
  l <- as.list(as.character(1:length(alist)))
  names(l) <- names(alist)
  l
}


getBkgRunList = function ()
{
  pidfiles=dir(pattern="pidStatus$")
  if (length(pidfiles) == 0) return (list())
  theList = unlist(lapply(pidfiles,function (x)
    scan(file=x,what="character",sep="\n",quiet=TRUE)))
  pidfiles=as.list(pidfiles)
  names(pidfiles) = theList
  pidfiles
  
}

killIfRunning = function (uuid)
{
  fn = paste0(uuid,".pidStatus")
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
  if (file.exists(uuid)) 
  {
    file.remove(paste0(uuid,"/",dir(uuid)))
    file.remove(uuid)
  }
  fls = dir(pattern=uuid)
  if (!all) fls = setdiff(fls,paste0(uuid,".RData"))
  file.remove(fls)
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


writeKeyFile <- function (fvsRun,dbIcon,prms)
{
  stds = unlist(lapply(fvsRun$stands,function(x) x$sid))
cat("writeKeyFile, num stds=",length(stds),
    " fvsRun$title=",fvsRun$title," uuid=",fvsRun$uuid,"\n")
  if (length(stds)==0) return()
  dbSendQuery(dbIcon,'drop table if exists m.RunStds') 
  dbWriteTable(dbIcon,"m.RunStds",data.frame(RunStds = stds))
  dbQ = dbSendQuery(dbIcon,
    paste0('select Stand_ID,Stand_CN,Groups,Inv_Year from FVS_StandInit ',
      'where Stand_ID in (select RunStds from m.RunStds)'))
  fvsInit = dbFetch(dbQ,n=-1)
  names(fvsInit) = toupper(names(fvsInit))
  extns = attr(prms$programs[prms$programs == fvsRun$FVSpgm][[1]],"atlist")
  source("autoOutKeys.R")
  defaultOut = sub ("FVSOut",fvsRun$uuid,defaultOut)
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
    cat ("StdIdent\n",sprintf("%-26s",std$sid)," ",fvsRun$title,"\n",file=fc,sep="")
    if (!is.null(fvsInit$STAND_CN[sRow]) && !is.na(fvsInit$STAND_CN[sRow]) && 
        fvsInit$STAND_CN[sRow] != " ") 
      cat ("StandCN\n",fvsInit$STAND_CN[sRow],"\n",file=fc,sep="") else
      cat ("StandCN\n",std$sid,"\n",file=fc,sep="") 
    cat ("MgmtId\n",if (length(std$rep)) sprintf("R%3.3d",std$rep) else 
        fvsRun$defMgmtID,"\n",file=fc,sep="")                       
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
    cat ("DelOTab            1\n",file=fc)
    cat ("DelOTab            2\n",file=fc)
    cat ("DelOTab            4\n",file=fc)
    cat (defaultOut,file=fc)
    if (!is.na(match("mist",extns))) cat (defaultOutMist,file=fc)
    for (out in fvsRun$autoOut)
    {
      switch (out,
       "autoTreelists"=cat(autoTreelists,file=fc),
       "autoCompute"=cat(autoCompute,file=fc),
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
        if (cmp$exten == "climate" && substr(cmp$kwds,1,8) == "ClimData")
        {
          scn = unlist(strsplit(cmp$kwds,"\n"))[2]
          qur = paste0("select * from FVS_Climattrs\n"," where Stand_ID = '",
                std$sid,"' and Scenario = '",scn,"';\n")
          d = dbGetQuery(dbIcon,qur)
          ans = apply(d,2,function (x) !any(is.na(x)))
          d = d[,ans]          
          if (nrow(d) == 0) cat (cmp$kwds,"\n",file=fc,sep="") else
          {
            cat ("ClimData\n",scn,"\n*\n",file=fc,sep="")
            suppressWarnings(write.table(d,file=fc,append=TRUE,col.names=TRUE,
              sep=",",quote=FALSE,row.names=FALSE))
            cat ("-999\n",file=fc,sep="")
          }
        } else cat (cmp$kwds,"\n",file=fc,sep="")
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


mkSimCnts <- function (fvsRun,sels=NULL,foundStand=0L)
{
  tmpcnts = list()
  tmptags = list()                         
  if (!is.null(sels)) if (length(sels) == 0) sels = NULL
  start = if (length(fvsRun$startDisp)) as.numeric(fvsRun$startDisp) else 1
  if (!is.null(sels) && length(sels) == 1)
  {
    if (substr(sels[[1]],1,1) == "-") 
    {
      start = max((-as.numeric(sels[[1]]))-20,1) 
    } else if (substr(sels[[1]],1,1)== "+") 
    {
      start = min(as.numeric(sels[[1]])+20,length(fvsRun$stands)-19)
      if(start<1) start=1
    }     
  }
  end = min(start+length(fvsRun$stands)-1,start+19)
  if (foundStand > 0L) 
  {
    if (foundStand < start || foundStand > end) 
    {
      start = foundStand
      end = min(start+length(fvsRun$stands)-1,start+19)
      if (end > length(fvsRun$stands))
      {
        start = max(length(fvsRun$stands)-20,1)
        end = length(fvsRun$stands)
      }
    }
  } 
  fvsRun$startDisp = as.character(start)
cat("mkSimCnts, foundStand=",foundStand," start=",start," end=",end,
" sels=",if (is.null(sels)) "NULL" else if (is.list(sels)) 
paste0("list n=",length(list)) else sels,"\n")
  if (length(fvsRun$stands)) for (i in start:end)
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
  if (start > 1) 
  {
    tmptags <- append(tmptags,paste0("-",as.character(start)),after=0)
    newstart <- max(start-20,1) 
    if(newstart<1) newstart=1
    newend <-  min(newstart+length(fvsRun$stands)-1,newstart+19)  
    tmpcnts <- append(tmpcnts,paste0("<< Display ",newstart," to ",newend,
               " of ",length(fvsRun$stands)," stands >>"),after=0)
  }
  if (end < length(fvsRun$stands))
  {
    tmptags <- append(tmptags,paste0("+",as.character(start)))
    newstart <- min(start+20,length(fvsRun$stands)-19)
    newend <-  min(newstart+length(fvsRun$stands)-1,newstart+19)  
    tmpcnts <- append(tmpcnts,paste0("<< Display ",newstart," to ",newend,
               " of ",length(fvsRun$stands)," stands >>"))
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
  globals$foundStand=0L  
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
cat("mkKeyWrd, ansFrm=\n",ansFrm,"\ninput=",input,"\n")
  state=0
  out = NULL
  for (i in 1:nchar(ansFrm))
  {
    c = substr(ansFrm,i,i)
#cat("mkKeyWrd, c=",c," state=",state,"\n")
#cat("mkKeyWrd, out='",out,"'\n")
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
  tbs <- dbListTables(dbcon)
  if (! ("FVS_Cases" %in% tbs)) return()
  tmpDel = paste0("tmp",gsub("-","",runuuid),Sys.getpid())
  qry = paste0("attach database ':memory:' as ",tmpDel)
cat ("qry=",qry,"\n") 
  dbSendQuery(dbcon,qry)
  todel = paste0(tmpDel,".todel")
  qry = paste0("drop table if exists ",todel)
cat ("qry=",qry,"\n") 
  dbSendQuery(dbcon,qry)      
  qry = paste0("create table ",todel,
     " as select CaseID from FVS_Cases where KeywordFile = '",runuuid,"'")
cat ("qry=",qry,"\n") 
  dbSendQuery(dbcon,qry)    
  nr = dbGetQuery(dbcon,paste0("select count(*) from ",todel))[,1]
cat ("ncases to delete=",nr,"\n")
  if (nr)
  {
    tbs <- dbListTables(dbcon)
    dbBegin(dbcon)
    for (tab in tbs)
    {
      if ("CaseID" %in% dbListFields(dbcon,name=tab))
      { 
        qry = paste0("delete from ",tab," where CaseID in (",
                     "select CaseID from ",todel,")")
cat ("qry=",qry,"\n")                      
        dbSendQuery(dbcon,qry)
        nr = dbGetQuery(dbcon,paste0("select count(*) from ",tab))[,1]
cat ("nr=",nr,"\n")                      
        if (nr == 0) dbSendQuery(dbcon,paste0("drop table ",tab))
      } else dbSendQuery(dbcon,paste0("drop table ",tab)) 
    }
    dbCommit(dbcon)
  }
  dbSendQuery(dbcon,paste0("detach database '",tmpDel,"'"))
}


addNewRun2DB <- function(runuuid,dbcon)
{
  # dbcon is the connection to the existing output database
  # runuuid is the uuid of the run that will be merged to the output database

cat ("addNewRun2DB, runuuid=",runuuid,"\n")  
  fn = paste0(runuuid,".db")
  if (! (file.exists(fn) && file.size(fn))) return("no new database found")
  newrun = paste0("newrun",gsub("-","",runuuid),Sys.getpid())
  qry = paste0("attach database '",fn,"' as ",newrun)
cat ("qry=",qry,"\n") 
  res = try (dbSendQuery(dbcon,qry))
  if (class(res) == "try-error") return ("new run database attach failed")
  qry = paste0("select * from ",newrun,".sqlite_master where type='table'")
cat ("qry=",qry,"\n") 
  newtabs = dbGetQuery(dbcon,qry)[,"tbl_name",drop=TRUE]
  if (length(newtabs)==0)
  {
    try (dbSendQuery(dbcon,paste0("detach database '",newrun,"'")))
    return("no data found in new run")
  }
  ic = grep ("FVS_Cases",newtabs)
  if (length(ic) == 0) return("no FVS_Cases found in new run") 
  qry = paste0("select * from ",newrun,".FVS_Cases")
cat ("qry=",qry,"\n") 
  res = dbGetQuery(dbcon,qry)
cat ("nrow(res)=",nrow(res),"\n")
  if (nrow(res) == 0)
  {
    dbSendQuery(dbcon,paste0("detach database '",newrun,"'"))
    return("no new cases found in new run")
  }

  deleteRelatedDBRows(runuuid,dbcon)
   
  tbs <- dbListTables(dbcon)
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
        dbBegin(dbcon)
        for (addCol in toAdd)
        {
          qry = paste0("alter table ",newtab," add column ",addCol," ",
                subset(newCols,name==addCol)[,"type"])
cat ("qry=",qry,"\n") 
          dbSendQuery(dbcon,qry)
        }
        dbCommit(dbcon)
      }
      qry = if (identical(newColNs,existCols)) 
        paste0("insert into ",newtab," select * from ",newrun,".",newtab) else
        paste0("insert into ",newtab," (",
          paste0(newColNs,collapse=","),") select * from ",newrun,".",newtab)
cat ("qry=",qry,"\n") 
      dbSendQuery(dbcon,qry)
    } else {
      qry = paste0("create table ",newtab," as select * from ",newrun,".",newtab,";")
cat ("qry=",qry,"\n") 
      dbSendQuery(dbcon,qry)
    }
  }    
  dbSendQuery(dbcon,paste0("detach database '",newrun,"'"))
  unlink(fn)
  "data inserted"
}


ncmps <- function(fvsRun)
{
  sum(unlist(lapply(fvsRun$stands,function(x) length(x$cmps)))) +
  sum(unlist(lapply(fvsRun$grps,  function(x) length(x$cmps))))
}
