fvsComplexRun <-
function(...)
{
  args <- list(...)
  argnames <-  names(args)
  needed <- c("BeforeEM1","AfterEM1","BeforeEM2","AfterEM2",
              "BeforeAdd","BeforeEstab","SimEnd")
  toCall <- vector("list",length(needed))
  names(toCall) <- needed
  toCall[needed] <- args[needed]
  ignored <- setdiff(names(args),needed)
  if (length(ignored) > 0) warning("argument(s) ignored: ",
      paste(ignored,collapse=", "))
 
  ntoc <- length(needed)
  allCases <- list()        
  oneCase <- NULL
  setNextStopPoint <- function (toCall,currStopPoint)
    {
       pts <- (currStopPoint+1):(ntoc-1)  #set up a circlular sequence 
       if (length(pts) < ntoc) pts <- c(pts,1:(ntoc-length(pts)-1))
       for (i in pts) 
       {
         if (!is.null(toCall[[i]]))
         {                               # args are: spptcd,spptyr
           .Fortran("setstoppointcodes",as.integer(i),as.integer(-1))
           break
         }
       }
     }
              
  setNextStopPoint(toCall,0)

  repeat
  {
    # run fvs, capture the return code
    rtn <- .Fortran("fvs",as.integer(0))[[1]]
    if (rtn != 0) break  # this will signal completion. 
  
    stopPoint <- .Fortran("getrestartcode",as.integer(0))[[1]]

    if (stopPoint == 0) 
    {
      if (! is.null(toCall[["SimEnd"]])) 
      {
        ans <- if (is.function(toCall[["SimEnd"]])) toCall[["SimEnd"]]() else
               eval(parse(text=toCall[["SimEnd"]]))
        if (! is.null(ans))
        { 
          onePtr <- length(allCases)+1
          allCases[[onePtr]] <- ans
          ids <- fvsGetStandIDs()
          caseID <- paste(ids[1], ids[3], "SimEnd",sep=":")
          names(allCases)[onePtr] <- caseID
        }
      } 
      setNextStopPoint(toCall,0)
    }
    else
    {
      if (! is.null(toCall[[stopPoint]])) 
      {
        ans <- if (is.function(toCall[[stopPoint]])) toCall[[stopPoint]]() else
               eval(parse(text=toCall[[stopPoint]]))
        if (! is.null(ans))
        { 
          if (is.null(oneCase)) oneCase <- list()
          onePtr <- length(oneCase)+1
          oneCase[[onePtr]] <- ans
          names(oneCase)[onePtr] <- names(toCall)[stopPoint]
        }
      }
      setNextStopPoint(toCall,if (stopPoint == ntoc-1) 0 else stopPoint)
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


