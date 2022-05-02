# $Id$
#
kcpVetting <- function (kcpconts)
{
  if(!length(kcpconts)) return()
  basekwds <- list("ADDFILE","AGPLABEL","ALSOTRY","ATRTLIST","BAIMULT","BAMAX","BFDEFECT",
                   "BFFDLN","BFVOLEQU","BFVOLUME","CALBSTAT","CCADJ","CFVOLEQU","CHEAPO",
                   "CLOSE","COMMENT","COMPRESS","CRNMULT","CRUZFILE","CUTEFF",
                   "CUTLIST","CWEQN","CYCLEAT","DATASCRN","DEBUG","DEFECT","DELOTAB",
                   "DESIGN","DGSTDEV","ECHO","ECHOSUM","ENDFILE","FERTILIZ","FIXCW",
                   "FIXDG","FIXHTG","FIXMORT","FVSSTAND","GROWTH","HTGMULT","HTGSTOP",
                   "INPFILE","INVYEAR","LOCATE","MANAGED","MCDEFECT","MCFDLN","MGMTID",
                   "MINHARV","MODTYPE","MORTMSB","MORTMULT","NOAUTOES","NOCALIB",
                   "NODEBUG","NOECHO","NOHTDREG","NOSCREEN","NOSUM","NOTREES","NOTRIPLE",
                   "NUMCYCLE","NUMTRIP","OPEN","ORINFO","ORGVOL","PRMFROST","POINTGRP","POINTREF",
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
  regenkwds <- list("AUTALLY","BUDWORM","BURNPREP","ESRANSD","EXCRUISE","HABGROUP","HTADJ",
                    "INGROW","MECHPREP","MINPLOTS","NATURAL","NOAUTALY","NOINGROW","NOSPROUT",
                    "OUTPUT","PASSALL","PLANT","PLOTINFO","SPECMULT","SPROUT","STOCKADJ",
                    "TALLY","TALLYONE","TALLYTWO","THRSHOLD")
  firekwds <- list("BURNREPT","CANCALC","CANFPROF","CARBCALC","CARBCUT","CARBREPT","DEFULMOD",
                   "DROUGHT","DUFFPROD","DWDCVOUT","DWDVLOUT","FIRECALC","FLAMEADJ","FMODLIST","FMORTMLT",
                   "FUELDCAY","FUELFOTO","FUELINIT","FUELMODL","FUELMOVE","FUELMULT","FUELOUT",
                   "FUELPOOL","FUELREPT","FUELSOFT","FUELTRET","MOISTURE","MORTCLASS","MORTREPT",
                   "PILEBURN","POTFIRE","POTFMOIS","POTFPAB","POTFSEAS","POTFTEMP","POTFWIND",
                   "SALVAGE","SALVSP","SIMFIRE","SNAGBRK","SNAGCLAS","SNAGDCAY","SNAGFALL",
                   "SNAGINIT","SNAGOUT","SNAGPBN","SNAGPSFT","SNAGSUM","SOILHEAT","STATFUEL",
                   "SVIMAGES")
  dbkwds <- list("ATRTLIDB","BURNREDB","CALBSTDB","CARBREDB","CLIMREDB","COMPUTDB",
                 "CUTLIDB","DWDCVDB","DWDVLDB","ECONRPTS","FUELREDB","FUELSOFT","MISRPTS",
                 "MORTREDB","POTFIRDB","RDBBMORT","RDDETAIL","RDSUM","SNAGOUDB","SNAGSUDB","STRCLSDB",
                 "SUMMARY","TREELIDB","STANDSQL","TREESQL","SQLIN","SQLOUT","DSNIN","DSNOUT",
                 "INVSTATS","REGREPTS")
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
  altless <- c(4,5,5,8,6) # number of parameters the non-DBS keywords have that are exact in name
  altmore <- c(1,2,1,1,1,1,1,1) # number of parameters the non-DBS keywords have that are exact in name 
  extflag <- 0 # denotes whether we are in an extension block, and which extension by it's value (1-8)
  condflag <- 0 # denotes whether we are in a conditional block
  computeflag <- 0 # denotes whether we are in a compute block
  commentflag <- 0 # denoted whether we are in a COMMENT keyword block
  k <- 1 # index counter for the next 2 lists
  insertkw <- list() # list of which keywords to insert at the end
  insertidx <- list() # list of indices of where to insert those keywords at the end
  numinserts <- 0 # number of keywords to insert
  kcpconts <- (strsplit(kcpconts,"\n"))[[1]] # top down list of the addfile
  # loop through each existing line of the addfile
  j <- 1
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
          if (j!=length(kcpconts))next
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
        if (j!=length(kcpconts))next
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
      # If it's an extension keyword and we're not already in an extension block
      else if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),extkwds)) 
              && extflag==0 && is.na(endkw) && !compkw){
        # if it's a regen keyword, insert ESTAB in the line above and set the flag to 1, etc
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),regenkwds))){
          invoke <- as.character(invokekwds[1])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 1
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),firekwds))){
          invoke <- as.character(invokekwds[2])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 2
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbkwds))){
          invoke <- as.character(invokekwds[3])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 3
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),climatekwds))){
          invoke <- as.character(invokekwds[4])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 4
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),econkwds))){
          invoke <- as.character(invokekwds[5])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 5
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),coverkwds))){
          invoke <- as.character(invokekwds[6])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 6
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),rdkwds))){
          invoke <- as.character(invokekwds[7])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 7
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),mistkwds))){
          invoke <- as.character(invokekwds[8])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 8
          if (j!=length(kcpconts))next
        }
      }
      # If it's an extension keyword, and we are already in an extension block
      else if (!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),extkwds))
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
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 1
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),firekwds)) && extflag!=2){
          insertkw[k] <- "END"
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          invoke <- as.character(invokekwds[2])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 2
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),dbkwds)) && extflag!=3){
          insertkw[k] <- "END"
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          invoke <- as.character(invokekwds[3])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 3
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),climatekwds)) && extflag!=4){
          insertkw[k] <- "END"
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          invoke <- as.character(invokekwds[4])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 4
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),econkwds)) && extflag!=5){
          insertkw[k] <- "END"
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          invoke <- as.character(invokekwds[5])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 5
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),coverkwds)) && extflag!=6){
          insertkw[k] <- "END"
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          invoke <- as.character(invokekwds[6])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 6
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),rdkwds)) && extflag!=7){
          insertkw[k] <- "END"
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          invoke <- as.character(invokekwds[7])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 7
          if (j!=length(kcpconts))next
        }
        if(!is.na(match(toupper(strsplit(kcpconts[j]," ")[[1]][1]),mistkwds)) && extflag!=8){
          insertkw[k] <- "END"
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          invoke <- as.character(invokekwds[8])
          insertkw[k] <- invoke
          insertidx[k] <- j-1
          k <- k+1
          numinserts <- numinserts +1
          extflag <- 8
          if (j!=length(kcpconts))next
        }
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
        if(extflag > 0){
          insertkw[k] <- "END"
          insertidx[k] <- j-1
          extflag <- 0
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
  vettedKCP <- paste(kcpconts,collapse="\n")
  return(vettedKCP)
}


writeKeyFile <- function (globals,dbIcon,newSum=TRUE,keyFileName=NULL,verbose=TRUE)
{
  stds = unlist(lapply(globals$fvsRun$stands,function(x) x$sid))
  if (verbose) cat("writeKeyFile, num stds=",length(stds),
    " globals$fvsRun$title=",globals$fvsRun$title," uuid=",globals$fvsRun$uuid,"\n")
  if (length(stds)==0) return(paste0("No stands to process. Run =",
           globals$fvsRun$title," uuid=",globals$fvsRun$uuid))
  dbExecute(dbIcon,'drop table if exists temp.RunStds')                   
  dbWriteTable(dbIcon,DBI::SQL("temp.RunStds"),data.frame(RunStds = stds))
 
  # get the preferred ids depending on the table that was used to build the run
  intable=toupper(globals$fvsRun$refreshDB)
  queryIDs=switch(intable,
                  "FVS_STANDINIT"     =c("STAND_ID","STAND_CN"),
                  "FVS_PLOTINIT"      =c("STANDPLOT_ID","STANDPLOT_CN"),
                  "FVS_STANDINIT_COND"=c("STAND_ID","STAND_CN"),
                  "FVS_STANDINIT_PLOT"=c("STAND_ID","STAND_CN"),
                  "FVS_PLOTINIT_PLOT" =c("STANDPLOT_ID","STANDPLOT_CN"))
  initfields = try(toupper(dbListFields(dbIcon,intable)))
  if (class(initfields) == "try-error") 
    return(paste0("Run data query returned no data to run, Run =",
           globals$fvsRun$title," uuid=",globals$fvsRun$uuid))
  queryIDs = queryIDs[queryIDs %in% initfields] 
  if (length(queryIDs) == 0) return("Needed stand id fields are missing")
  qry = paste0('select ',paste0(queryIDs,collapse=','),',Groups,Inv_Year,Sam_Wt from ',
              intable,' where ',queryIDs[1],' in (select RunStds from temp.RunStds)')  
  if (verbose) cat ("Database qry=",qry,"\n")
  fvsInit = try(dbGetQuery(dbIcon,qry))
  if (class(fvsInit) == "try-error") return(paste0("Run data query failed. qry=",qry," Run =",
           globals$fvsRun$title," uuid=",globals$fvsRun$uuid))
  if (nrow(fvsInit) == 0) return("Run data query returned no data to run.")
  # compute replication weights
  stofix=table(stds)
  stofix=names(stofix[stofix>1])
  rwts=unlist(lapply(globals$fvsRun$stands,function(x) x$repwt))
  wtofix=list()
  for (sf in stofix)
  {
    idxs=grep(sf,stds,fixed=TRUE)
    if (length(idxs)) wtofix[[sf]] = rwts[idxs]/sum(rwts[idxs])
  }
  extns = globals$activeFVS[globals$fvsRun$FVSpgm][[1]]
  source(system.file("extdata", "autoOutKeys.R", package = "fvsOL"),local=TRUE)
  defaultOut = sub ("FVSOut",globals$fvsRun$uuid,defaultOut)
  if (!newSum)  defaultOut = sub ("Summary        2","Summary",defaultOut)
  if (is.null(keyFileName)) keyFileName=paste0(globals$fvsRun$uuid,".key")
  fc = file(description=keyFileName,open="wt")
  cat ("!!title:",globals$fvsRun$title,"\n",file=fc)
  cat ("!!uuid: ",globals$fvsRun$uuid,"\n",file=fc)
  cat ("!!built:",format(Sys.time(), 
        "%Y-%m-%d_%H:%M:%S"),"\n",file=fc)
  baseCycles = seq(as.numeric(globals$fvsRun$startyr),as.numeric(globals$fvsRun$endyr),
                   as.numeric(globals$fvsRun$cyclelen))
  cycleat = scan(text=gsub(";"," ",gsub(","," ",globals$fvsRun$cycleat)),
                  what=0,quiet=TRUE)
  cycleat = union(baseCycles,cycleat)
  cycleat = sort(union(cycleat,as.numeric(globals$fvsRun$endyr))) 
  for (std in globals$fvsRun$stands)
  { 
    names(fvsInit) <- toupper(names(fvsInit))
    sRows = match (std$sid, fvsInit$STAND_ID)
    sRowp = match (std$sid, fvsInit$STANDPLOT_ID)
    if (verbose) cat ("processing std=",std$sid," sRows=",sRows," sRowp=",sRowp,"\n")    
    if (is.na(sRows) && is.na(sRowp)) next
    cat ("StdIdent\n",sprintf("%-26s",std$sid)," ",globals$fvsRun$title,"\n",file=fc,sep="")
    if (!is.null(fvsInit$STAND_CN[sRows]) && !is.na(fvsInit$STAND_CN[sRows]) && 
        fvsInit$STAND_CN[sRows] != " "){ 
      cat ("StandCN\n",fvsInit$STAND_CN[sRows],"\n",file=fc,sep="")
    } else if (!is.null(fvsInit$STANDPLOT_CN[sRowp]) && !is.na(fvsInit$STANDPLOT_CN[sRowp]) && 
        fvsInit$STANDPLOT_CN[sRowp] != " "){ 
      cat ("StandCN\n",fvsInit$STANDPLOT_CN[sRowp],"\n",file=fc,sep="")
      }else cat ("StandCN\n",std$sid,"\n",file=fc,sep="")
    cat ("MgmtId\n",globals$fvsRun$defMgmtID,"\n",file=fc,sep="") 
    if (length(std$invyr) == 0) std$invyr = as.character(thisYr) 
    ninvyr = as.numeric(std$invyr)
    cat ("InvYear       ",std$invyr,"\n",file=fc,sep="")
    thiscyc = union(seq(ninvyr,cycleat[1],as.numeric(globals$fvsRun$cyclelen)),cycleat)
    ints = diff(sort(thiscyc))
    if (length(ints) > 40) ints = ints[1:40]
    if (length(ints)==0) ints = globals$fvsRun$cyclelen
    mostint = names(which.max(table(ints)))
    ints = as.character(ints)
    cat ("TimeInt                ",mostint,"\n",file=fc)
    for (i in 1:length(ints)) if (ints[i] != mostint) 
       cat ("TimeInt      ",as.character(i),"      ",ints[i],"\n",file=fc)
    cat ("NumCycle    ",as.character(i),"\n",file=fc)
    cat (defaultOut,file=fc)
    # "checking" the FVS Outputs suppresses adding autoDelOTab so make that logical switch here
    autos = if (is.null(names(globals$fvsRun$autoOut))) unlist(globals$fvsRun$autoOut) else 
                unlist(globals$fvsRun$autoOut[["autoOut"]])
    autos = if ("autoDelOTab" %in% autos) 
    { 
      aa = setdiff(autos,"autoDelOTab")
      unlist(lapply(aa,function(a) {aw = paste0(a,".withText"); if (exists(aw)) aw else a}))
    } else c(autos,"autoDelOTab")
    for (out in autos) if (exists(out) && !is.null(out)) eval(parse(text=paste0("cat(",out,",file=fc)")))
    if (!is.null(globals$fvsRun$autoOut[["svsOut"]]) && !is.null(globals$fvsRun$autoOut[["svsOut"]][["svs"]]) && 
      exists("autoSVS"))
    {
      shape = if (globals$fvsRun$autoOut[["svsOut"]][["shape"]] == "Square") "1" else "3"
      nfire = as.character(globals$fvsRun$autoOut[["svsOut"]][["nfire"]])
      keys=unlist(strsplit(autoSVS,"\n"))
      svs=grep("^SVS ",keys)
      if (length(svs)) substr(keys[svs],11,20) = sprintf("%10s",shape)
      svs=grep("^SVImages ",keys)
      if (length(svs)) substr(keys[svs],11,20) = sprintf("%10s",nfire)
      lapply (keys,function(x,fc) cat (x,"\n",file=fc),fc); cat ("\n",file=fc)
    } 
    lastExt = "base"
    lastCnd = NULL
      browser()
    extensPrefixes = c("estb"="Estab","strp"="Estab","cover"="Cover",
      "fire"="FMIn","mist"="Mistoe","wrd3"="RDIn",
      "ardwrd3"="RDIn","armwrd3"="RDIn\nRRType             3",
      "phewrd3"="RDIn\nRRType             4","dbs"="DataBase",
      "econ"="Econ","climate"="Climate","organon"="Organon")
    if (length(std$grps)) for (grp in std$grps)
    {
      if (length(grp$cmps)) for (cmp in grp$cmps)
      {
        if (length(grep("Addfile:",cmp$title)) || length(grep("Editor:",cmp$title))) cmp$kwds <- kcpVetting(cmp$kwds)
        if (cmp$atag == "k" && !is.null(lastCnd))
        {
          if(lastExt != "base") cat ("End\n",file=fc,sep="")
          cat ("EndIf\n",file=fc,sep="")
          if(lastExt == lastExt) cat (extensPrefixes[exten],"\n",file=fc,sep="")
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
        naughty <- "Econ_reports"
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
            if(length(grep("FVS_Data.db",kwdlist)) > 0) kwdlist[grep("FVS_Data.db",kwdlist)]="FVS_Data.db"
            cmp$kwds <- paste(kwdlist,collapse="\n")
          }
          cat ("!Exten:",cmp$exten," Title:",cmp$title,"\n",
                    cmp$kwds,"\n",file=fc,sep="")
        }
      }
    } 
    if (length(std$cmps)) for (cmp in std$cmps)
    {    
      if (length(grep("Addfile:",cmp$title)) || length(grep("Editor:",cmp$title))) cmp$kwds <- kcpVetting(cmp$kwds)
      if (cmp$atag == "k" && !is.null(lastCnd))
      {
        if(lastExt != "base") cat ("End\n",file=fc,sep="")
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
      naughty <- "Econ_reports"
      if (lastExt != exten && !any(!is.na(match(naughty,cmp$kwdName))))
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
    if (is.null(lastCnd) && lastExt != "base") cat ("End\n",file=fc,sep="")
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
  msg = paste0("Num stands=",length(stds)," keyFileName=",keyFileName,
        " Run title=",globals$fvsRun$title)
  if (verbose) cat ("End of writeKeyFile, msg=",msg,"\n")
  return(msg)
}
