# $Id: fvsOutUtilities.R 2814 2019-10-10 11:02:11Z nickcrookston $

if (exists("mkfvsOutData")) rm (mkfvsOutData)
mkfvsOutData <- 
  setRefClass("fvsOutData", 
    fields = list(dbLoadData = "list", dbData = "data.frame", 
      dbVars = "character", browseVars = "character", 
      dbSelVars = "character", browseSelVars = "character",
      runs = "character", plotSpecs = "list", 
      render = "data.frame"))

pivot <- function(dat,pvar,dvar)
{
  facts = colnames(dat)[unlist(lapply(dat,is.factor))]
  if (! pvar %in% facts) return(data.frame())
  if (! dvar %in% colnames(dat)) return(data.frame())
  for (v in facts) dat[[v]] = droplevels(dat[[v]])
  facts = setdiff(facts,c(dvar,pvar))
  if (length(facts) == 0) 
  {
    d = t(dat[,dvar,drop=FALSE])
    colnames(d) = dat[,pvar]
    return(as.data.frame(d))
  }
  grps = by(dat,as.list(dat[,facts,drop=FALSE]),
    function(x,allc,facts,pvar,dvar) 
    {
      y = rep(0,length(allc)) 
      y[match(x[,pvar],allc)] = x[,dvar]
      c(as.numeric(x[1,facts]),y)   
    },levels(dat[[pvar]]),facts,pvar,dvar)    
  grps = matrix(unlist(grps),
         ncol=length(levels(dat[[pvar]]))+length(facts),byrow=TRUE)
  colnames(grps)=c(facts,paste0(dvar," ",pvar,":",levels(dat[[pvar]])))
  if (is.factor(dat[[dvar]]))
  {
    colstofactor = (length(facts)+1):ncol(grps)
    grps[,colstofactor] = ifelse(grps[,colstofactor]==0,"",
        levels(dat[[dvar]])[grps[,colstofactor]])
  }
  grps = as.data.frame(grps)  
  for (i in facts)
  {
    grps[[i]] = as.integer(grps[[i]])
    attributes(grps[[i]]) = attributes(dat[[i]])
  }
  cmd = paste0("order(",paste(paste0("grps$",facts),collapse=","),")")
  sby = eval(parse(text=cmd))
  grps[sby,]
}


filterRows <- function (dat, title, groups, stdid, mgtid, year, species, dbhclass)
{
  rows = rep (TRUE,nrow(dat))
  if (!is.null(title) && length(title) == 1 && 
      substring(title[[1]],1,11) == "None loaded") title = NULL
  if (!is.null(stdid) && length(stdid) == 1 && 
      substring(stdid[[1]],1,11) == "None loaded") stdid = NULL
  if (!is.null(groups) && length(groups) == 1 && 
      substring(groups[[1]],1,11) == "None loaded") groups = NULL
  if (!is.null(mgtid) && length(mgtid) == 1 && 
      substring(mgtid[[1]],1,11) == "None loaded") mgtid = NULL
  if (!is.null(year) && length(year) == 1 && 
      substring(year[[1]],1,11) == "None loaded") year = NULL
  if (!is.null(species) && length(species) == 1 && 
      substring(species[[1]],1,11) == "None loaded") species = NULL
  if (!is.null(dbhclass) && length(dbhclass) == 1 && 
      substring(dbhclass[[1]],1,11) == "None loaded") dbhclass = NULL
  rows = if (!is.null(title)    & !is.null(dat$RunTitle)) rows &  
         dat$RunTitle %in% title   else rows
  rows = if (!is.null(stdid)    & !is.null(dat$StandID))  rows &  
         dat$StandID %in% stdid    else rows     
  rows = if (!is.null(groups)   & !is.null(dat$Groups))   rows &
         {
           hits = unique(unlist(lapply(groups,function (x,dgrps)  
                         grep (x,dgrps,fixed=TRUE), dat$Groups)))
           yeses = rep(FALSE,length(rows))
           yeses[hits] = TRUE
           yeses
         } else rows
  rows = if (!is.null(mgtid)    & !is.null(dat$MgmtID))   rows &   
         dat$MgmtID  %in% mgtid    else rows
  rows = if (!is.null(year)     & !is.null(dat$Year))     rows &     
         dat$Year %in% year     else rows
  rows = if (!is.null(species)  & !is.null(dat$Species))  rows & 
         dat$Species %in% species  else rows
  rows = if (!is.null(dbhclass) & !is.null(dat$DBHClass)) rows & 
         dat$DBHClass %in% dbhclass else rows
  rows
}


setupSummary <- function(asum,composite=FALSE)
{
  std=if (length(grep("SCuFt$",colnames(asum)))) 
    c("Tpa","MCuFt","SCuFt","SBdFt") else c("Tpa","TCuFt","MCuFt","BdFt")
  rstd=paste("R",std,sep="")
  stdden = c("BA","SDI","CCF","TopHt","QMD")
  stddenAT = paste0("AT",stdden)
  newrtpa = "RTpa"
  if (composite)
  {
    std=paste0("Cmp",std)
    rstd=paste0("Cmp",rstd)
    stddenAT = paste0("CmpAT",stdden)
    stdden = paste0("Cmp",stdden)
    newrtpa = "CmpRTpa"
  }    
  new=asum[,newrtpa] > 0
  rtpaLoc  = match(newrtpa,colnames(asum))
  if (sum(new) > 0) 
  {
    dups=unlist(lapply(1:length(new),function(x,new) 
                if (new[x]) rep(x,2) else x, new))
    asum=asum[dups,]
    for (row in 1:(nrow(asum)-1))
    {
      nrow=row+1
      if (dups[row] == dups[nrow])
      {
        asum[nrow,std]  = asum[nrow,std] - asum[nrow,rstd]
        asum[nrow,stdden]=asum[nrow,stddenAT]
        asum[row,rstd] = 0
        asum[row,rtpaLoc:(rtpaLoc+3)] = 0
        dups[nrow]=0
      }
    }
  }
  tprd=apply(asum[,rstd],2,cumsum)+asum[,std]
  colnames(tprd)=if (composite) gsub("Cmp","CmpTPrd",std) else 
                                paste("TPrd",std,sep="")
  asum=cbind(asum,tprd,srtOrd=1:nrow(asum))
  asum
}


getRptFile <- function (new=FALSE)
{
  if (file.exists("FVSReport")) 
  {
    if (new) lapply(dir("FVSReport"), function(x) 
      file.remove(paste0("FVSReport/",x)))
  } else dir.create("FVSReport")
  "FVSReport/report.md"
}


mkNextPlotFileName <- function()
{
  if (!file.exists("FVSReport")) dir.create("FVSReport")
  files=dir("FVSReport",pattern="[.]png$")
  sprintf("plot%3.3d.png",length(files)+1)
}
 

appendToReport <- function(obj,rptFile=getRptFile())
{
  if (missing(obj)) return()
  con = if (rptFile==stdout()) stdout() else file(description=rptFile,open="at") 
  if (class(obj) == "data.frame" || class(obj) == "matrix")
  {
    cat (file=con,"Table: Table created",format(Sys.time(),"%a %b %d %X %Z %Y"))
    cat (file=con,"  \n")
    if (!is.null(colnames(obj)))
    {
      cat (file=con,"\n|") 
      lapply(colnames(obj),function (x) cat(file=con,x,"|",sep=""))
      cat (file=con,"\n")
      cat (file=con,"|")
      lapply(colnames(obj),function (x) 
        cat(file=con,paste0(rep("-",max(2,nchar(x)-1)),collapse=""),":|",sep=""))
      cat (file=con,"\n")
    }
    apply(obj,1,function(y)
    {
      cat (file=con,"|")
      lapply(y,function (x) cat(file=con,x,"|",sep="")) 
      cat (file=con,"\n")
    })
    cat (file=con,"\n")
  } else if (class(obj) == "character") 
  {
    lapply(obj,cat,file=con,"  \n")
    cat (file=con,"  \n")
  } 
  if (con != stdout()) close(con)
}


appendPlotToReport <- 
function (plotFile=mkNextPlotFileName(),rptFile=getRptFile(),width=5,height=5)
{
  if (!file.exists("plot.png")) return()
  ct = format(file.info("plot.png")[,"ctime"], "%a %b %d %X %Z %Y")
  con = if (rptFile==stdout()) stdout() else file(description=rptFile,open="at") 
  cat(file=con,"\n![Figure created ",ct,"](",plotFile,"){width=",
      as.character(width),"in height=",as.character(height),"in}\n\n",sep="")
  if (con != stdout()) close(con)
  file.copy(from="plot.png",to=paste0("FVSReport/",plotFile))
}


generateReport <- function(tf)
{
  if (file.exists("FVSReport/report.md"))
  {
    setwd("FVSReport")
    cmd = paste0("pandoc -o ",tf," -t docx report.md")
cat ("generateReport, cmd=",cmd,"\n")
    system(cmd)
    setwd("..")
  }
}

  
errorScan <- function (outfile)
{
  if (missing(outfile)) return("outfile not specified")
  if (!file.exists(outfile)) return("outfile does not exist") 
  fout<-file(outfile,"rt")
  on.exit(close(fout))
  errs<-list()
  sid<-line<-l1<-""
  foundSum = FALSE  
  ln = 0
  pgmRV = NA
  repeat
  {
    l1<-line
    line=scan(fout,what="character",sep="\n",n=1,quiet=TRUE,
         blank.lines.skip = FALSE)
    if (length(line) == 0) break
    ln = ln+1
    if (is.na(pgmRV))
    {
      hit=grep("     FOREST VEGETATION SIMULATOR   ",line,fixed=TRUE)
      if (length(hit)) 
      {
        hit=scan(text=line,what="character",quiet=TRUE)
        pgmRV=grep("RV:",hit,fixed=TRUE)
        pgmRV=if(is.na(pgmRV)) NA else hit[pgmRV]
      }
    }         
    hit=grep("STAND ID= ",line,fixed=TRUE)
    if (!foundSum) foundSum = length(grep("START OF SIMULATION PERIOD",
                                     line,fixed=TRUE))>0
    if (length(hit))
    {
      sid = scan(text=line,what="character",quiet=TRUE)[3]
      next
    }
    hit=grep("ERROR",toupper(line),fixed=TRUE)    
    if (length(hit)) 
    {
      if (length(grep("STANDARD ERRORS",toupper(line),fixed=TRUE))) next
      if (length(grep("SAMPLING",toupper(line),fixed=TRUE))) next
      err <- c(l1,line)
      names(err) <- paste0("Std=",sid,";Line=",as.character(c(ln-1,ln)))
      errs<-append(errs,err)
    }
  }
  outerrs <- list()
  outerrs <- append(outerrs,if (length(errs)) 
    {    
      errs[unlist(lapply(errs,nchar)) == 0] <- NULL
      paste0(paste0(names(unlist(errs)),": ",unlist(errs)),collapse="<br>")
    } else "No errors found")
  if (!foundSum) outerrs <- append(errs,"Run failure, likely due to the database associated with this run not being active.")
  attr(outerrs,"pgmRV")=if (is.na(pgmRV)) " " else pgmRV
  outerrs
}

