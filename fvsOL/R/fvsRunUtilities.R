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

loadVarData <- function(globals,input,dbIcon)
isolate({
cat ("in loadVarData, input$inTabs=",input$inTabs," globals$activeVariants=",globals$activeVariants,"\n") 
  dbtabs = dbGetQuery(dbIcon,"select name from sqlite_master where type='table';")[,1]
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
      if (length(keep)) globals$activeVariants = globals$activeVariants[keep] 
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
  fvsRun$title = paste0("Run ",length(FVS_Runs)+1)
  fvsRun$startyr = character(0)
  fvsRun$endyr = character(0)
  fvsRun$cyclelen = character(0)
  fvsRun$cycleat = character(0)
  fvsRun$defMgmtID = nextMgmtID(length(FVS_Runs))
  fvsRun$runScript = "fvsRun"
  uiCustomRunOps = list()
  fvsRun$uuid = uuidgen()
  fvsRun$startDisp = character(0)
}

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
  pidfiles=dir(pattern="pidStatus$")
  if (length(pidfiles) == 0) return (list())
  theList = unlist(lapply(pidfiles,function (x)
    readLines(x,n=1)))
  pidfiles=as.list(pidfiles)
  for (i in 1:length(pidfiles))
  {
    runuuid=strsplit(pidfiles[[i]],split=".",fixed=TRUE)[[1]][1]
    cat ("runuuid=",runuuid,"\n")
    rundirs = dir (pattern=paste0(runuuid,".set"))
    sumRuns = 0
    for (rdir in rundirs)
    {
      dbfile = file.path(rdir,paste0(runuuid,".db"))
      if (file.exists(dbfile))
      {
        db = dbConnect(SQLite(), dbname = dbfile)
        nrun = try(dbGetQuery(db,"select count(*) as NRun from FVS_Cases"))
        dbDisconnect(db)
        nrun = if (class(nrun)=="try-error") 0 else nrun[1,1]
        sumRuns = sumRuns+nrun
      }
    }
    theList[i] = paste0(theList[i],"; Complete+Started = ",sumRuns)
  }
  names(pidfiles) = theList
  pidfiles 
}

killIfRunning = function (uuid)
{
  fn = paste0(uuid,".pidStatus")
cat ("killIfRunning, fn=",fn,"\n")
  if (file.exists(fn))
  {
    pstat = readLines(fn)
    pids=as.numeric(scan(text=pstat[1],what="character",n=1,sep=" ",quiet=TRUE))
    if (length(pstat) > 1) 
    {
      p2=scan(text=pstat[2],what="character",sep=" ",quiet=TRUE)
      p2=na.omit(suppressWarnings(as.numeric(p2)))
      pids = c(pids,p2)
    }
    for (pid in pids)
    {
      cmd = if (.Platform$OS.type == "windows") paste("taskkill /t /f /pid",pid) else 
        paste("kill ",pid)    
cat ("kill cmd =",cmd,"\n")
      if (.Platform$OS.type == "windows") shell(cmd) else system (cmd)
      Sys.sleep(.1)
    }
    unlink(fn)
  }
}
    
   
removeFVSRunFiles = function (uuid,all=FALSE)
{
  if (file.exists(uuid)) 
  {
    unlink(paste0(uuid,"/",dir(uuid)), recursive = TRUE, force = TRUE)
    unlink(uuid)
  }
  fls = dir(pattern=uuid)
  if (!all) fls = setdiff(fls,paste0(uuid,".RData"))
  unlink(fls, recursive = TRUE, force = TRUE)
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
      if(length(fvsRun$stands) < i) break
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

nextRunName <- function(runNames)
{
  i=1
  repeat 
  {
    rn=paste0("Run ",length(runNames)+i) 
    if (rn %in% runNames) i=i+1 else break
  }
  rn
}
  
resetActiveFVS <- function(globals)
{
  globals$activeFVS = list(
    FVSak = c("ak", "estb", "dbs", "mist", "fire", "econ", "cover"), 
    FVSbc = c("bc", "strp", "dbs", "climate", "fire", "armwrd3", "phewrd3", "ardwrd3",
              "wrd3","mist"),
    FVSbm = c("bm", "fire", "econ", "strp", "dbs", "climate", "cover", 
              "mist", "wrd3","armwrd3", "phewrd3", "ardwrd3"), 
    FVSca = c("ca",  "fire", "econ", "strp", "dbs", "mist", "climate", "cover"), 
    FVSci = c("ci",  "fire", "econ", "estb", "dbs", "climate", "cover", "mist", 
              "fire", "wrd3","armwrd3", "phewrd3", "ardwrd3"), 
    FVScr = c("cr", "fire", "econ",  "strp", "dbs", "climate", "mist", "cover", 
              "wrd3", "armwrd3", "phewrd3",  "ardwrd3"), 
    FVScs = c("cs", "strp", "dbs", "fire", "econ"), 
    FVSec = c("ec",  "strp", "dbs", "cover", "mist", "fire", 
              "climate", "econ", "wrd3", "phewrd3",  "armwrd3", "ardwrd3"), 
    FVSem = c("em", "estb", "dbs", "cover",  "mist", "fire", "climate", 
              "econ", "wrd3", "phewrd3", "armwrd3", "ardwrd3" ), 
    FVSie = c("ie", "estb", "dbs", "cover", "mist", "fire", "climate",  
              "econ", "wrd3", "armwrd3", "phewrd3", "ardwrd3"), 
    FVSkt = c("kt", "estb",  "dbs", "cover", "mist", "fire", "climate", 
              "econ", "wrd3", "phewrd3",  "armwrd3", "ardwrd3"), 
    FVSls = c("ls", "dbs", "strp", "fire",  "econ"), 
    FVSnc = c("nc", "strp", "dbs", "cover", "mist", "fire",  "econ", 
              "climate", "wrd3", "phewrd3", "armwrd3", "ardwrd3"), 
    FVSne = c("ne",  "dbs", "strp", "fire", "econ"), 
    FVSoc = c("oc", "fire", "econ",  "strp", "dbs", "mist", 
              "climate", "cover", "organon"), 
    FVSon = c("on", "strp", "dbs", "fire"),
    FVSop = c("op",  "strp", "dbs", "mist", "fire", "econ", "climate", 
              "cover", "wrd3", "armwrd3",  "phewrd3", "ardwrd3", "organon"), 
    FVSpn = c("pn", "strp", "dbs",  "mist", "fire", "econ", "climate", 
              "cover", "wrd3", "armwrd3", "phewrd3",  "ardwrd3"), 
    FVSsn = c("sn", "dbs", "strp", "fire", "econ"), 
    FVSso = c("so",  "strp", "dbs", "cover", "mist", "fire", "econ", 
              "climate", "wrd3", "phewrd3",  "armwrd3", "ardwrd3"), 
    FVStt = c("tt", "fire", "strp", "dbs",  "cover", "mist", "climate", 
              "econ", "wrd3", "phewrd3", "armwrd3", "ardwrd3" ), 
    FVSut = c("ut", "strp", "dbs", "cover", "mist", "fire", "climate",  
              "econ", "wrd3", "phewrd3", "armwrd3", "ardwrd3"), 
    FVSwc = c("wc", "strp",  "dbs", "mist", "fire", "climate", "econ", 
              "cover", "wrd3", "phewrd3",  "armwrd3", "ardwrd3"), 
    FVSws = c("ws", "strp", "dbs", "cover",  "mist", "fire", "climate", 
              "econ", "wrd3", "phewrd3", "armwrd3", "ardwrd3" )) 
  avalFVS <- dir(globals$fvsBin,pattern=paste0("[.]",
                 substr(.Platform$dynlib.ext,2,999),"$"))
  avalFVS <- unique(sub(.Platform$dynlib.ext,"",avalFVS))
  if (length(avalFVS)) globals$activeFVS = globals$activeFVS[avalFVS] 
  globals$activeVariants <- unlist(lapply(globals$activeFVS, function(x) x[1]))
  vars = c("ak: Alaska"="ak",
           "bc: British Columbia (metric)"="bc",
           "bm: Blue Mountains,Oregon"="bm",
           "ca: Inland CA,Southern Cascades"="ca",
           "ci: Central ID"="ci",
           "cr: Central Rockies GENGYM"="cr",
           "sm: GENGYM: Southwest Mixed Conifers"="sm",
           "sp: GENGYM: Southwest Ponderosa Pine"="sp",
           "bp: GENGYM: Black Hills Ponderosa Pine"="bp",
           "sf: GENGYM: Spruce-fir"="sf",
           "lp: GENGYM: Lodgepole pine"="lp",
           "ec: East Cascades,Washington"="ec",
           "em: Eastern Montana"="em",
           "ie: Inland Empire"="ie",
           "nc: Klammath Mountains,Northern CA"="nc",
           "oc: ORGANON SWO - FVSca hybrid"="oc",
           "on: Ontario (metric)"="on",
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
  keep=na.omit(match(globals$activeVariants,vars))
  globals$activeVariants = if (length(keep)) vars[keep] else character(0)
  globals$activeExtens=character(0)
}

nextMgmtID <- function(nruns=0)
{
  L = as.integer(nruns+1) %/% 1000
  np1 = nruns-(L*1000)+1
  L= if (L<25) LETTERS[L+1] else letters[L+1]
  sprintf("%s%3.3d",L,np1)
}
  

resetGlobals <- function(globals,runDef)
{
cat("resetGlobals, runDef=",runDef,"\n")
  resetActiveFVS(globals)
  globals$schedBoxYrLastUsed=character(0)
  globals$currentEditCmp=globals$NULLfvsCmp
  if (runDef)                                        
  {
    globals$schedBoxYrLastUsed=globals$fvsRun$startyr                  
    if (length(globals$fvsRun$FVSpgm) > 0)            
    {                                                            
      indx = match(globals$fvsRun$FVSpgm,names(globals$activeFVS))
      if (length(indx) && !is.na(indx))                         
      {
        globals$fvsRun$FVSpgm = names(globals$activeFVS)[indx]
        globals$activeFVS <- globals$activeFVS[indx]
        globals$activeVariants <- subset(globals$activeVariants,
                globals$activeVariants==globals$activeFVS[[1]][1])
        globals$activeExtens <- c("base",globals$activeFVS[[1]][-1])
        globals$lastRunVar <- globals$activeVariants
      }                                       
    }
  }
cat ("resetGlobals,globals$activeVariants=",globals$activeVariants,"\n")
cat ("resetGlobals,activeFVS=",length(globals$activeFVS),"\n")
if (length(globals$activeFVS)) cat ("names(globals$activeFVS)=",names(globals$activeFVS),"\n")
cat ("resetGlobals,reset activeExtens= ");lapply(globals$activeExtens,cat," ");cat("\n")
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


updateVarSelection <- function (globals,session,input)
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
  if(variant!="ne" && length(grep("ThinRDSL",ansFrm))>0) out="ThinRDSL"
  if (is.null(pkeys) || is.null(input) || is.null(ansFrm)) return(out)
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
        fld = if (is.na(inp)) " " else 
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
      "Initialize Root Disease" = "keyword.wrd3.wrd_initialization",
      "Modify Root Disease behavior" = "keyword.wrd3.wrd_behavior",
      "Modify Root Disease bark beetles" = "keyword.wrd3.wrd_brk_btl")))
  catsel = append(catsel,list(   
    "Modify Sprouting" = c(
      "Turn off Sprouting" = "Estab keyword.estbstrp.NoSprout",
      "Adjust Sprouting" = "Estab keyword.estbstrp.Sprout"),
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
      if(length(fvsRun$stands)==1)fvsRun$grps = list()
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
      
pasteComponent <- function(globals,sel,topaste)
{
  # search for selected element in grps...attach to grp cmps.
  idx = findIdx(globals$fvsRun$grps,sel)
  if (!is.null(idx)) 
  {
    globals$fvsRun$grps[[idx]]$cmps <- append(globals$fvsRun$grps[[idx]]$cmps,topaste)
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
    idx = findIdx(globals$fvsRun$stands,sel)
    if (!is.null(idx))
    {
      globals$fvsRun$stands[[idx]]$cmps <- append(globals$fvsRun$stands[[idx]]$cmps,topaste)
      return(idx)
    }
  }
  if (is.null(idx))
  {
    # selected element is a component. Find the grp in which it is listed.
    for (grp in globals$fvsRun$grps)
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
    for (std in globals$fvsRun$stands)
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
#cat ("qry=",qry,"\n") 
  dbExecute(dbcon,qry)
  todel = paste0(tmpDel,".todel")
  qry = paste0("drop table if exists ",todel)
#cat ("qry=",qry,"\n") 
  dbExecute(dbcon,qry)      
  qry = paste0("create table ",todel,
     " as select CaseID from FVS_Cases where KeywordFile = '",runuuid,"'")
#cat ("qry=",qry,"\n") 
  dbExecute(dbcon,qry) 
  nr = dbGetQuery(dbcon,paste0("select count(*) from ",todel))[,1]
#cat ("ncases to delete=",nr,"\n")
  if (nr)
  {
    for (tb in tbs)
    {
      if ("CaseID" %in% dbListFields(dbcon,name=tb))
      { 
        qry = paste0("delete from ",tb," where ",tb,".CaseID in (",
                     "select CaseID from ",todel,")")
        rtn = dbExecute(dbcon,qry)
#cat ("rtn=",rtn," qry=",qry,"\n")                      
        nr = nrow(dbGetQuery(dbcon,paste0("select CaseID from ",tb," limit 1")))
#cat ("nr=",nr,"\n")                      
        if (nr == 0) 
        {
          qry = paste0("drop table ",tb)
          rtn = dbExecute(dbcon,qry)
#cat ("no rows in table after delete rtn=",rtn," qry=",qry,"\n")                      
        }
      } else {
        qry = paste0("drop table ",tb)
        rtn = dbExecute(dbcon,qry)
#cat ("caseID not in table, rtn=",rtn," qry=",qry,"\n")                      
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

#' Add the output to the master FVSOut.db
#'
#' Add the output from a recently completed run to the master output database,
#' FVSOut.db
#' @param runuuid a character string with the runUUID that will be merged into 
#'   the connection in argument dbcon.
#' @param dbcon the database connection to FVSOut.db
#' @param removeOldOutput if TRUE (default) any previous output for this run
#'   is removed prior to new output being added.
#' @param verbose if TRUE (default), progress output is written.
#' @return a character string with an informative message as to what was done.
#' @export 
addNewRun2DB <- function(runuuid,dbcon,removeOldOutput=TRUE,verbose=TRUE)
{
  # dbcon is the connection to the existing output database
  # runuuid is the uuid of the run that will be merged to the output database
  if (verbose) cat ("addNewRun2DB, runuuid=",runuuid,"\n") 
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
    if (verbose) cat ("try to get exclusive lock, trycnt=",trycnt,"\n");
    rtn <- try(dbExecute(dbcon,"create table dummy (dummy int)"))
    if (class(rtn) != "try-error") break;
    Sys.sleep (10)
  } 
  dbExecute(dbcon,"drop table if exists dummy")
  if (verbose) cat ("removeOldOutput=",removeOldOutput,"\n")
  if (removeOldOutput) deleteRelatedDBRows(runuuid,dbcon)   

  mkDBIndices(dbcon)
  newrun = paste0("newrun",gsub("-","",runuuid),Sys.getpid())
  qry = paste0("attach database '",fn,"' as ",newrun)
  if (verbose) cat ("qry=",qry,"\n") 
  res = try (dbExecute(dbcon,qry))
  if (class(res) == "try-error") return ("new run database attach failed")
  qry = paste0("select * from ",newrun,".sqlite_master where type='table'")
  if (verbose) cat ("qry=",qry,"\n") 
  newtabs = dbGetQuery(dbcon,qry)[,"tbl_name",drop=TRUE]
  if (length(newtabs)==0)
  {
    try (dbExecute(dbcon,paste0("detach database '",newrun,"'")))
    return("no data found in new run")
  }
  ic = grep ("FVS_Cases",newtabs)
  if (length(ic) == 0) return("no FVS_Cases found in new run") 
  qry = paste0("select * from ",newrun,".FVS_Cases")
  if (verbose) cat ("qry=",qry,"\n") 
  res = dbGetQuery(dbcon,qry)
  if (verbose) cat ("nrow(res)=",nrow(res)," CaseID=",res$CaseID,"\n")
  if (nrow(res) == 0)
  {
    dbExecute(dbcon,paste0("detach database '",newrun,"'"))
    return("no new cases found in new run")
  }
  tbs <- dbGetQuery(dbcon,"select name from sqlite_master where type='table';")[,1]
  for (newtab in newtabs) 
  {        
    if (newtab %in% tbs)
    {
      qry = paste0("PRAGMA ",newrun,".table_info('",newtab,"')")
      newCols = dbGetQuery(dbcon,qry)
      newColNs = newCols[,"name",drop=TRUE]
      qry = paste0("PRAGMA table_info('",newtab,"')")
      existCols = dbGetQuery(dbcon,qry)[,"name",drop=TRUE]
      toAdd = setdiff(newColNs,existCols)
      if (length(toAdd))
      {
        for (addCol in toAdd)
        {
          qry = paste0("alter table ",newtab," add column ",addCol," ",
                subset(newCols,name==addCol)[,"type"])
          dbExecute(dbcon,qry)
        }
      }
      qry = if (identical(newColNs,existCols)) 
        paste0("insert into ",newtab," select * from ",newrun,".",newtab) else
        paste0("insert into ",newtab," (",
          paste0(newColNs,collapse=","),") select * from ",newrun,".",newtab)
      if (verbose) cat ("main insert qry=",qry,"\n") 
      rtn=dbExecute(dbcon,qry)
    } else {
      qry = paste0("create table ",newtab," as select * from ",newrun,".",newtab,";")
      if (verbose) cat ("copy qry=",qry,"\n") 
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
    resetGlobals(globals,TRUE) 
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
    updateReps(globals$fvsRun)
    msgVal = msgVal+1
    progress$set(detail="Loading contents listbox",value = msgVal)
    mkSimCnts(globals$fvsRun,justGrps=isolate(input$simContType=="Just groups"))
    updateSelectInput(session=session, inputId="simCont", 
      choices=globals$fvsRun$simcnts, selected=globals$fvsRun$selsim)
    output$contCnts <- renderUI(HTML(paste0("Run contents: ",
        length(globals$fvsRun$stands)," stand(s), ",
        length(globals$fvsRun$grps)," group(s)")))
    updateStandTableSelection(session,input,globals)
    globals$changeind <- 1
    output$contChange <- renderText({
      HTML(paste0("<b>","*Run*","</b>"))
    })
    progress$close()      
  })
}


updateStandTableSelection <- function (session,input,globals)
{
cat ("in updateStandTableSelection, length(globals$fvsRun$stands)=",length(globals$fvsRun$stands),"\n") 
  if (length(globals$fvsRun$stands) == 0)
    updateSelectInput(session=session, inputId="inTabs", choices=globals$selStandTableList,
      selected=if (length(globals$selStandTableList)) globals$selStandTableList[[1]] else "") else
  {                                                           
    if (length(globals$fvsRun$refreshDB) == 0 && length(globals$selStandTableList)) 
      globals$fvsRun$refreshDB = globals$selStandTableList[[1]] else
    {
      hit=match(globals$fvsRun$refreshDB,globals$selStandTableList)
      if (is.na(hit)) hit = match(globals$fvsRun$refreshDB,names(globals$selStandTableList))
      if (is.na(hit)) return()
      globals$fvsRun$refreshDB = globals$selStandTableList[[hit]]
    }     
    updateSelectInput(session=session, inputId="inTabs", choices=list(globals$fvsRun$refreshDB),
      selected=globals$fvsRun$refreshDB)
  }   
}

updateReps <- function(fvsRun)
{ 
cat ("in updateReps, num stands=",length(fvsRun$stands),"\n") 
  if (length(fvsRun$stands))
  {
    stds <- unlist(lapply(fvsRun$stands,function(x) x$sid))  
    cnts <- table(stds) 
    have <- unlist(lapply(fvsRun$grps,function(x) 
            if (x$grp != "") x$grp else NULL))
    need = paste0("AutoRep=",1:max(cnts))
    mkgps <- setdiff(need, have)
    for (grp in mkgps) 
    {
      newgrp <- mkfvsGrp(grp=grp,uuid=uuidgen())
      fvsRun$grps <- append(fvsRun$grps,newgrp)
    }
    have <- unlist(lapply(fvsRun$grps,function(x) 
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
          fvsRun$stands[[r]]$rep <- i
          have <- unlist(lapply(fvsRun$stands[[r]]$grps,function(x) 
                         if (x$grp != "") x$grp else NULL))
          if (! names(grpIdxs)[i] %in% have) fvsRun$stands[[r]]$grps <- 
            append(fvsRun$stands[[r]]$grps,fvsRun$grps[[grpIdxs[i]]])
          i <- i+1
        }     
      } else fvsRun$stands[[reps]]$rep <- 0                           
    }
  }
}

getProjectList <- function(includeLocked=FALSE)
{
cat ("getProjectList, includeLocked=",includeLocked,"\n") 
  if (isLocal())
  {  
    selChoices = unlist(lapply (dir(".."), function (x,inc) {
      if (!inc) if (file.exists(
        paste0("../",x,"/projectIsLocked.txt"))) return(NULL)
      fn = paste0("../",x,"/projectId.txt") 
      if (file.exists(paste0("../",x,"/projectId.txt"))) x
    },includeLocked))
    if (length(selChoices)) 
    {
      names(selChoices) = selChoices
      selChoices = as.list(selChoices)
    } else selChoices=NULL
  } else {
    curEmail=NULL
    if (file.exists("projectId.txt"))
    {
      curEmail=scan(file="projectId.txt",what="character",sep="\n",quiet=TRUE)
      curEmail=toupper(trim(sub("email=","",curEmail[1])))
cat ("curEmail=",curEmail,"\n")
      if (is.null(curEmail)) return(NULL)
      kdata = lapply (dir(".."), function (x,inc) {         
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
    } else return(NULL)
cat ("length(kdata)=",length(kdata),"\n")
    if (length(kdata))
    {
      kdata = as.data.frame(do.call(rbind,kdata),stringsAsFactors=FALSE)
      names(kdata)=c("prj","email","title")
      kdata$email=toupper(kdata$email)
      kdata = kdata[kdata$email == curEmail,,drop=FALSE]
      selChoices=as.list(kdata$prj)
      names(selChoices)=kdata$title 
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
  if (!name %in% setOfNames) return(name)
  i=1
  nn=NA 
  repeat
  {
    sp=unlist(strsplit(name,split="")) 
    pl = grep("\\(",sp)
    if (length(pl) > 0)
    {
      pl = max(pl)
      if (pl != length(sp)) 
      {
        kp = if (sp[length(sp)]==")") length(sp)-1 else length(sp)
        nn = suppressWarnings(as.numeric(paste0(sp[(pl+1):kp],collapse="")))
      }
    }
    if (is.na(nn))
    {
      nn=i 
      kp = length(sp)
      ac = " ("
    } else {
      kp = pl
      nn=nn+1
      ac = ""
    }   
    sp = sp[1:kp]
    name = paste0(c(sp,ac,as.character(nn),")"),collapse="")
    if (!name %in% setOfNames) return(name)
    i = i+1
  } 
}
                    
mkFileNameUnique <- function(fn)
{ 
  fn = trim(fn)
  fex <- file.exists(fn)
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


storeOrUpdateObject <- function (db,data)
{
  # store (or update) an Robject in database db in table "Robjects"
  if (missing(db) || class(db) != "SQLiteConnection") stop("db required connection")
  if (missing(data)) stop("data required")
  name = deparse(substitute(data))
  if (!("Robjects" %in% dbListTables(db))) dbExecute(db,
    "create table Robjects (name text primary key, data blob)")
  row = dbGetQuery(db,paste0("select rowid from Robjects where (name='",name,"');"))
  row = row[1,1]
  if (is.na(row)) 
  {  # insert
    changed=suppressWarnings(dbExecute(db,
      "insert into Robjects (name,data) values ((:name), (:data))", 
      params=data.frame(name=name,data=I(list(extnToRaw(data))))))
  } else { #update
    changed=suppressWarnings(dbExecute(db,
      paste0("update Robjects set data=(:data) where (rowid=",row,")"),
      params=data.frame(data=I(list(extnToRaw(data))))))
  }
  changed
}

loadObject <- function (db,name,asName=name)
{
  # load an Robject from database db table "Robjects". The object is loaded into
  # the parent.frame and given the name "name" or the one passed in "as.name"
  if (missing(db) || class(db) != "SQLiteConnection") stop("db required connection")
  if (missing(name)) stop("name required")
  if (! "Robjects" %in% dbListTables(db)) return ()
  row = try(dbGetQuery(db,paste0("select rowid from Robjects where (name='",name,"');")))
  if (class(row)=="try-error") return()
  row = row[nrow(row),1]
  if (length(row)==0 || row==0 || is.na(row)) return()
  data=dbGetQuery(db,paste0("select data from Robjects where (rowid=",row,");"))
  if (nrow(data)) assign(envir=parent.frame(),x=asName,value=extnFromRaw(data[1,1][[1]]))
}

removeObject <- function (db,name)
{
  if (missing(db) || class(db) != "SQLiteConnection") stop("db required connection")
  if (missing(name)) stop("name required")
  if (! "Robjects" %in% dbListTables(db)) return (0)
  row = dbGetQuery(db,paste0("select rowid from Robjects where (name='",name,"');"))
  row = row[nrow(row),1]
  if (length(row) == 0 || is.na(row)) return(0)
  for (rr in row) dbExecute(db,paste0("delete from Robjects where (rowid=",rr,");"))
  return(length(row))
}

listTableNames <- function (db) 
{
  if (missing(db) || class(db) != "SQLiteConnection") stop("db required connection")
  if ("Robjects" %in% dbListTables(db)) 
    dbGetQuery(db,"select name from Robjects")[,1] else NULL
}


mkFVSProjectDB <- function (prjDir=getwd())
{
  # used to build an FVSProject database from the .RData file formats
  if (!dir.exists(prjDir)) return(NULL) 
  prjDir = normalizePath(prjDir)
  prjDB = file.path(prjDir,"FVSProject.db")
  db = dbConnect(SQLite(), dbname = prjDB)
  dbExecute(db,paste0("create table if not exists FVSRuns ",
      "(uuid text primary key, name text, time integer, run blob)"))
      
### TODO: this code assumes we have old run files and it loads them. After about 
### the year 2025 this code can be deleted down to the dbDisconnect.
  runsFile = file.path(prjDir,"FVS_Runs.RData")
  if (file.exists(runsFile)) try(load(runsFile))
  if (exists("FVS_Runs"))
  {
    stdstkParms=attr(FVS_Runs,"stdstkParms")
    if (!is.null(stdstkParms)) storeOrUpdateObject(db,stdstkParms)
    for (i in 1:length(FVS_Runs))
    {
      run=FVS_Runs[i]
      runFile = file.path(prjDir,paste0(names(run),".RData"))
      if (!file.exists(runFile)) next
      rtn = try(load(runFile))
      if (class(rtn) == "try.error" || !exists("saveFvsRun")) next
      df = data.frame(uuid=names(run),name=unlist(run),time=attr(which="time",x=run[[1]]),
           run=I(list(extnToRaw(saveFvsRun))))
      row = dbGetQuery(db,paste0("select rowid from FVSRuns where (uuid='",names(run),"');"))
      row = row[nrow(row),1]
      for (rr in row) dbExecute(db,paste0("delete from FVSRuns where (rowid=",rr,");"))      
      rtn = dbExecute(db,paste0("insert into FVSRuns (uuid,name,time,run) values ",
                           "((:uuid), (:name), (:time), (:run))"), params=df)
      unlink(runFile)
    }
    unlink(runsFile)
  }
  # try getting other run files that were not in the project file (orphans)
  for (fl in dir(pattern="RData"))
  {
    if (nchar(fl) == 42) 
    {
      uuid=substr(fl,1,36)
      if (exists("saveFvsRun")) rm (saveFvsRun)
      load(fl)
      if (!exists("saveFvsRun")) next
      if (saveFvsRun$uuid == uuid) 
      {
        attrtime = attr(saveFvsRun,"time")
        if (is.null(attrtime)) attrtime = as.integer(file.mtime(fl))
        storeFVSRun(db,saveFvsRun,time=as.integer(file.mtime(fl)))
        unlink(fl)
      }
    }
  }
  ### process some of the other odd .RData files.
  if (file.exists("customQueries.RData"))
  {
    load("customQueries.RData")
    storeOrUpdateObject(db,customQueries)
    rm (customQueries)
    unlink("customQueries.RData")
  }
  if (file.exists("FVS_kcps.RData"))
  {
    load("FVS_kcps.RData")
    storeOrUpdateObject(db,customCmps)
    rm (customCmps)
    unlink("FVS_kcps.RData")
  }   
  if (file.exists("GraphSettings.RData"))
  {
    load("GraphSettings.RData")
    storeOrUpdateObject(db,GraphSettings)
    rm (GraphSettings)
    unlink("GraphSettings.RData")
  }   
  unlink("prms.RData")
  unlink("treeforms.RData")
  ########################
  cnt = dbGetQuery(db,"select count(*) from FVSRuns")
  dbDisconnect(db)
  cnt[1,1]
}

connectFVSProjectDB <- function (prjDir=getwd())
{
  if (!dir.exists(prjDir)) return(NULL) 
  prjDir = normalizePath(prjDir)
  prjDB = file.path(prjDir,"FVSProject.db")
  dbConnect(SQLite(), dbname = prjDB)
}

getFVSRuns <- function(db,asList=TRUE)
{
  if (missing(db) || class(db) != "SQLiteConnection") stop("db required connection")
  if ("FVSRuns" %in% dbListTables(db))
  {
    df = dbGetQuery(db,"select uuid,name,time from FVSRuns order by time desc")  
  } else return(NULL) 
  if (asList) 
  {  
    rl = df$uuid
    names(rl) = df$name
    return(as.list(rl))
  }
  return(df)
}

removeFVSRun <- function(db,uuid) 
{
  if (missing(db) || class(db) != "SQLiteConnection") stop("db required connection")
  if (missing(uuid)) stop("uuid required")
  if ("FVSRuns" %in% dbListTables(db))
    try(dbExecute(db,paste0("delete from FVSRuns where (uuid='",uuid,"');"))) else 0
}

storeFVSRun <- function(db,FVSRun,time=NULL)
{
  if (missing(db) || class(db) != "SQLiteConnection") stop("db required connection")
  if (missing(FVSRun) || class(FVSRun) != "fvsRun") stop("FVSRun required")
  attr(attr(FVSRun,"class"),"package") = "fvsOL"
  if ("FVSRuns" %in% dbListTables(db)) 
    dbExecute(db,paste0("delete from FVSRuns where (uuid='",FVSRun$uuid,"');")) else
      dbExecute(db, "create table FVSRuns (uuid text, name text, time integer, run blob)")
    time= if (is.null(time)) as.integer(Sys.time()) else as.integer(time)
    df = data.frame(uuid=FVSRun$uuid,name=FVSRun$title,time=time,
       run=I(list(extnToRaw(FVSRun))))
  rtn = dbExecute(db,paste0("insert into FVSRuns (uuid,name,time,run) values ",
                     "((:uuid), (:name), (:time), (:run))"), params=df)
  rtn
}

loadFVSRun <- function(db,uuid)
{
  if (missing(db) || class(db) != "SQLiteConnection") stop("db required connection")
  if (missing(uuid)) stop("uuid required")
  rtn = dbGetQuery(db,paste0("select run from FVSRuns where (uuid='",uuid,"')"))
  fvsRun = if (nrow(rtn)) extnFromRaw(rtn[1,1][[1]]) else NULL
  if (!is.null(fvsRun)) attr(attr(fvsRun,"class"),"package") = "fvsOL"
  fvsRun
}

  
mergeProjects <- function(masterdbfile,addondbfile)
{
  if (!file.exists(masterdbfile)) return(0)
  if (!file.exists(addondbfile))  return(0)
  db = dbConnect(SQLite(), dbname = masterdbfile)
  qry = paste0("attach database '",addondbfile,"' as 'addondb';")
  dbExecute(db,qry)
  qry = paste0("insert into FVSRuns select * from addondb.FVSRuns ",
               "where addondb.uuid not in (select uuid from FVSRuns);")
  dbExecute(db,qry)
  cnt = dbGetQuery(db,"select count(*) from FVSRuns;")[1,1] 
  dbDisconnect(db)
  cnt
}  
  

areFilesIdentical <- function (f1=NULL, f2=NULL)
{
  # returns true if the one file is the same as another (not just the same size)
  if (is.null(f1) || is.null(f2)) return(FALSE)
  if (!file.exists(f1)) return(FALSE)
  if (!file.exists(f2)) return(FALSE)
  f1sz=file.size(f1)
  f2sz=file.size(f2)
  if (f1sz != f2sz) return(FALSE)
  f1bin=readBin(f1,what="raw",size=1,n=f1sz)
  f2bin=readBin(f2,what="raw",size=1,n=f2sz)
  identical(f1bin,f2bin)
}
  
