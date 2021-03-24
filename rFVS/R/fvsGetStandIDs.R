#' Return stand identification codes
#'
#' @return a names character vector containing the "standid", "standcn", "mgmtid", and "caseID"
#' @export
fvsGetStandIDs <-
function()
{
  .C("CfvsStandID",standid="",standcn="",mgmtid="" ,caseID="",
     PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm) 
}

