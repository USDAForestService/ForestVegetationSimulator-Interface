#' Run FVS where FVS and R interact 
#'
#' Pass one or more R-code blocks where each block is a named argument. FVS is run
#' up to the corresponding stop points and the code block is run. After the code block
#' is finished, FVS runs up to the next stop point. All of the code blocks are optional
#' in that any one or all can be used.
#'
#' @param trace is true or false, when true informative messages are generated. 
#' @param BeforeEM1    R code to run at the stop point just before the first call to the Event Monitor
#' @param AfterEM1     R code to run at the stop point just after the first call to the Event Monitor
#' @param BeforeEM2    R code to run at the stop point just before the second call to the Event Monitor
#' @param AfterEM2     R code to run at the stop point just after the second call to the Event Monitor
#' @param BeforeAdd    R code to run at the stop point after growth and mortality has been computed, but prior to applying them
#' @param BeforeEstab  R code to run at the stop point just before the Regeneration Establishment Model is called
#' @param SimEnd       R code to run at the end of one stand simulation.
#' @return a list of stands and years simulated with the call.
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")                   
#'    fvsSetCmdLine("--keywordfile=base.key") 
#'    testInteract <- function(msg) 
#'    {
#'      cat("msg=",msg," ids=",unlist(fvsGetStandIDs()),               
#'          " year=",fvsGetEventMonitorVariables("year"),"\n")
#'      fvsGetRestartcode()
#'    }
#'    fvsInteractRun(BeforeEM1  = 'testInteract("BeforeEM1  ")',             
#'                   AfterEM1   = 'testInteract("AfterEM1   ")',  
#'                   BeforeEM2  = 'testInteract("BeforeEM2  ")',          
#'                   AfterEM2   = 'testInteract("AfterEM2   ")',  
#'                   BeforeAdd  = 'testInteract("BeforeAdd  ")',              
#'                   BeforeEstab= 'testInteract("BeforeEstab")',                      
#'                   SimEnd     = 'testInteract("SimEnd     ")')
#' @export                                                                                       
fvsInteractRun <-
function(...)
{                                                    
  args <- list(...)                                              
    
  # set up trace                                                       
  tm=match("trace",names(args))
  trace = as.logical( if (is.na(tm)) FALSE else                
  {
     tr=args[tm]
     args = args[-tm]
     tr     
  } )

  argnames <-  names(args)
  needed <- c("BeforeEM1","AfterEM1","BeforeEM2","AfterEM2",
              "BeforeAdd","BeforeEstab","SimEnd")
  toCall <- vector("list",length(needed))
  names(toCall) <- needed                                             
  toCall[needed] <- args[needed] 
browser()                                                          
  ignored <- setdiff(names(args),needed)
  if (length(ignored) > 0) warning("argument(s) ignored: ",          
      paste(ignored,collapse=", "))
  if (trace) 
  {
    for (name in needed) 
    {
      cat ("arg=", name, "value=", 
          if (is.null(toCall[[name]])) "NULL" else if (
              class(toCall[[name]]) == "function") "function" else 
              toCall[[name]],"\n")
    }
  }                                                                           
  ntoc <- length(needed)
  allCases <- list()        
  oneCase <- NULL
  setNextStopPoint <- function (toCall,currStopPoint)
    {                                
       pts <- (currStopPoint+1):(ntoc-1)  #set up a circlular sequence 
       if (length(pts) < ntoc) pts <- c(pts,1:(ntoc-length(pts)-1))
       for (i in pts) 
       {
         if (i == 0) next                                          
         if (!is.null(toCall[[i]]))
         {                               # args are: spptcd,spptyr
           .Fortran("fvsSetStoppointCodes",as.integer(i),as.integer(-1),
                     PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
           break
         }
       }
     }
  setNextStopPoint(toCall,0)

  repeat
  {
    # run fvs, capture the return code
    if (trace) cat ("calling fvs\n")
    rtn <- .Fortran("fvs",as.integer(0),
          PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm) [[1]]
    if (trace) cat ("rtn=",rtn,"\n")
    if (rtn != 0) break  # this will signal completion. 
    
    # if the current stop point is < zero, then the last call
    # is a reload from a stoppoint file.
    stopPoint <- .Fortran("fvsGetRestartCode",as.integer(0),
                  PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)[[1]]
    if (stopPoint < 0)                             
    {
      stopPoint = -stopPoint
      setNextStopPoint(toCall,stopPoint)                 
    }
    if (trace) 
    {
      yr <- fvsGetEventMonitorVariables("year")
      ids <- fvsGetStandIDs()
      cat ("called fvs, stopPoint=",stopPoint," yr=",yr," ids=",unlist(ids),"\n") 
    }
           
    if (stopPoint == 100) 
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


