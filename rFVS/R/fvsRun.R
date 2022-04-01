#' Run FVS up to a designated stop point, year, or both.
#'
#' @param stopPointCode is the integer value of the FVS stop point. If NA, no stop point is defined. 
#' @param stopPointYear is the integer value of the year the stop should happen.	
#' @return the FVS return code where -1 indicates that FVS has not been started, 
#' 0 indicates that FVS is in good running state, 1 indicates that FVS has detected 
#' an error of some kind and should not be used until reset by specifying new input and 
#' 2 indicates that FVS has finished processing all the stands; new input can be specified..
#' @export
fvsRun <-
function(stopPointCode=NA,stopPointYear=NA)
{
  if (! is.na(stopPointCode) && ! is.na(stopPointYear)) 
    .Fortran("fvsSetStoppointCodes",as.integer(stopPointCode),as.integer(stopPointYear),
             PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)

  repeat     
  {
    rtn = .Fortran("fvs",as.integer(0),
          PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm) [[1]]
    if (rtn != 0) break
    stopPoint <- .Fortran("fvsGetRestartCode",as.integer(0),
                          PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)[[1]]
    if (stopPoint != 0) break
  }
  invisible(rtn)
}


