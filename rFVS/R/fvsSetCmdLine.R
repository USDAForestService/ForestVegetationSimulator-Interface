#' Sets the command line 
#'
#' @param cl character string of the FVS command line. If missing, the command line
#'   arguments passed to R when it was started (also from RScript) are used)
#' @return the FVS return code or NULL if there is no command line specified.
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsSetCmdLine("--keywordfile=base.key")
#' @export
fvsSetCmdLine <-
function(cl = NULL)
{
  if (is.null(cl)) cl=paste(commandArgs(trailingOnly = TRUE),collapse=" ")
  nch = as.integer(nchar(cl))
  invisible(if (nch > 0) .C("CfvsSetCmdLine",cl,nch,as.integer(0),
            PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm) else NULL)
}

