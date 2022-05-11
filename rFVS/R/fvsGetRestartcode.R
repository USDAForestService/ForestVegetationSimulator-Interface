#' Get the current FVS restart code. See \link{fvsRun} for a list of the
#' stop codes
#'
#' @return an integer
#' @examples 
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsGetRestartcode()      
#' @export
fvsGetRestartcode <-
function()
{
  .Fortran("fvsGetRestartCode",as.integer(0),
           PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)[[1]]
}


