fvsComplexRun <-
function(functionsToCall=list(NULL,NULL,NULL,NULL,NULL,NULL,fvsGetSummary))
{
  allCases <- list()
  oneCase <- NULL
  if (is.null(names(functionsToCall))) names(functionsToCall) <-
      c(paste("StopPnt",1:6,sep=""),"SimEnd")
  else 
  {
    for (i in 1:6) if (names(functionsToCall)[i] == "") 
                       names(functionsToCall)[i] <- paste("StopPnt",i,sep="")
    if (names(functionsToCall)[7] == "") names(functionsToCall)[7] <- "SimEnd"                 
  }

  setNextStopPoint <- function (functionsToCall,currStopPoint)
    {
       pts <- (currStopPoint+1):6  #set up a circlular sequence 
       if (length(pts) < 6) pts <- c(pts,1:(6-length(pts)))
       for (i in pts) 
       {
         if (!is.null(functionsToCall[[i]]))
         {                               # args are: spptcd,spptyr
           .Fortran("setstoppointcodes",as.integer(i),as.integer(-1))
           break
         }
       }
     }
              
  setNextStopPoint(functionsToCall,0)

  repeat
  {
    # run fvs, capture the return code
    rtn <- .Fortran("fvs",as.integer(0))[[1]]
    if (rtn != 0) break  # this will signal completion. 
  
    stopPoint <- .Fortran("getrestartcode",as.integer(0))[[1]]

    if (stopPoint == 0) 
    {
      if (! is.null(functionsToCall[[7]])) 
      {
        ans <- functionsToCall[[7]]()
        if (! is.null(ans))
        { 
          onePtr <- length(allCases)+1
          allCases[[onePtr]] <- ans
          ids <- fvsGetStandIDs()
          caseID <- paste(ids[1], ids[3], names(functionsToCall)[7],sep=":")
          names(allCases)[onePtr] <- caseID
        }
      } 
      setNextStopPoint(functionsToCall,0)
    }
    else
    {
      if (! is.null(functionsToCall[[stopPoint]])) 
      {
        ans <- functionsToCall[[stopPoint]]()
        if (! is.null(ans))
        { 
          if (is.null(oneCase)) oneCase <- list()
          onePtr <- length(oneCase)+1
          oneCase[[onePtr]] <- ans
          names(oneCase)[onePtr] <- names(functionsToCall)[stopPoint]
        }
      }
      setNextStopPoint(functionsToCall,if (stopPoint == 6) 0 else stopPoint)
    }
    if (! is.null(oneCase))
    {
      yr <- fvsGetEventMonitorVariables("year")
      ids <- fvsGetStandIDs()
      caseID <- paste(ids[1], ids[3], as.character(yr),sep=":")
      onePtr <- length(allCases)+1
      allCases[[onePtr]] <- oneCase
      names(allCases)[onePtr] <- caseID
    }
    oneCase <- NULL
  }
  allCases
}


