#' Run FVS up to a designated stop point, year, or both. See \link{fvsGetRestartcode}
#' for a way to find out the current stop point code.
#'         
#' @param stopPointCode is the integer value of the FVS stop point. If NA, no stop point is defined. 
#'   The stop point codes are:
#'   \tabular{cl}{
#'     0 \tab  Never stop. \cr
#'    -1 \tab  Stop at every stop location.\cr 
#'    1  \tab Stop just before the first call to the Event Monitor.\cr 
#'    2  \tab Stop just after the first call to the Event Monitor.\cr
#'    3  \tab Stop just before the second call to the Event Monitor.\cr
#'    4  \tab Stop just after the second call to the Event Monitor.\cr
#'    5  \tab Stop after growth and mortality has been computed, but prior to applying them.\cr
#'    6  \tab Stop just before the ESTAB routines are called.\cr
#'    7  \tab Stop just after input is read but before missing values are imputed 
#'       (tree heights and crown ratios, for example) and model calibration (argument 
#'        stopPointYear is ignored).\cr}
#' @param stopPointYear is the integer value of the year the stop should happen.	
#' @return the FVS return code where 
#' \tabular{cl}{
#' -1 \tab indicates that FVS has not been started \cr
#' 0  \tab indicates that FVS is in good running state \cr
#' 1  \tab indicates that FVS has detected \cr
#'         an error of some kind and should not be used until reset by specifying new input \cr 
#' 2  \tab indicates that FVS has finished processing all the stands; new input can be specified \cr}
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsSetCmdLine("--keywordfile=base.key")
#'    fvsRun()
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


