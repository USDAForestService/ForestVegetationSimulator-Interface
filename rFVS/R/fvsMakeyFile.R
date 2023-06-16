#' Make an FVS keyword file suitable for running FVS using fvsLoad, fvsSetCmdLine and fvsRun
#'
#' Pass basic parameters needed to create an FVS input keyword file and this function will
#' generate the file.
#' 
#' @param keyFileName the keyword file name, if not specified unique name is created and used as the 
#'        file name (the file name is returned).
#' @param runTitle   the name of the run
#' @param standIDs   A character vector of one or more standIDs, keywords are generated for each. 
#' @param inDataBase The name of the input database, default is FVS_Data.db
#' @param outDataBase The name of the output database, default is FVSOut.db
#' @param ncycles     The number of FVS cycles, default is 10.
#' @param moreKeywords One of the following: A character vector of properly formatted keyword records
#'        that will be added to each stand, OR
#'        A data.frame where the first column is a keyword and subsequent columns are "keyword" fields
#'        that are added to the keywords. 
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsMakeKeyFile(runTitle="Test",standIDs=c("01100202010142","01100201010056"))  
#' @export

fvsMakeKeyFile <- function (keyFileName=NULL,runTitle=NULL,standIDs=NULL,
    inDataBase="FVS_Data.db",outDataBase="FVSOut.db",ncycles=10,moreKeywords=NULL)
{
  trim <- function (x) gsub("^\\s+|\\s+$","",x)
  if (is.null(standIDs)) stop("standIds must be specified.")
                     
  if (is.null(keyFileName)) keyFileName=tempfile(pattern="fvs",fileext=".key",tmpdir=getwd())
  if (file.exists(keyFileName)) unlink(keyFileName)
  fc = file(description=keyFileName,open="wt")
  cat ("!!title:",runTitle,"\n",file=fc)
  cat ("!!built:",format(Sys.time(), 
        "%Y-%m-%d_%H:%M:%S"),"\n",file=fc)
  for (sid in standIDs)
  {
    sid = trim(sid)
    cat ("\nStdIdent\n",sprintf("%-26s%s\n",sid,runTitle),sep="",file=fc)
    cat (sprintf("%-10s%10s\n","NumCycle",as.character(ncycles)),sep="",file=fc)
    if (!is.null(inDataBase))
    {
      cat ("DataBase\nDSNin\n",inDataBase,"\nStandSQL\n",sep="",file=fc)
      cat ("SELECT * FROM FVS_StandInit WHERE Stand_ID = '%StandID%'\n",sep="",file=fc)
      cat ("EndSQL\nTreeSQL\n",sep="",file=fc)
      cat ("SELECT * FROM FVS_TreeInit WHERE Stand_ID = '%StandID%'\n",sep="",file=fc)
      cat ("EndSQL\nEND\n",sep="",file=fc)                  
    }
    if (!is.null(outDataBase))
    {
      cat ("DataBase\nDSNOut\n",outDataBase,
           "\nSummary        2\nComputdb          0         1\nEnd\n",
           sep="",file=fc)
    }
    if (is.null(moreKeywords)) 
      cat("DelOTab            1\nDelOTab            2\nDelOTab            4\n",sep="",file=fc) 
    if (class(moreKeywords)=="character")
    {
      for (kw in moreKeywords) cat(kw,"\n",file=fc)
    } else if (class(moreKeywords)=="data.frame")
    {
      if (class(moreKeywords) == "data.frame") for (row in 1:nrow(moreKeywords))
      {
        if (nchar(moreKeywords[row,1])==0) next
        kw=sprintf("%-10s",trim(moreKeywords[row,1]),file=fc)       
        if (ncol(morekeywords)>1) 
        {
          for (col in 2:ncol(moreKeywords))
            kw=paste0(kw,sprintf("%10s",trim(as.character(moreKeywords[row,col]))))
        }
      }
      cat(kw,"\n",file=fc)
    }
    cat ("\nProcess\n",sep="",file=fc)
  }
  cat ("Stop\n",sep="",file=fc)
  close(fc)
  keyFileName
}

    
   
