#' Return stand identification codes
#'
#' @return a names character vector containing the "standid", "standcn", "mgmtid", and "caseID"
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsGetStandIDs()  # will be blank until a run is started    
#' @export
fvsGetStandIDs <-
function()
{
  .C("CfvsStandID",standid="",standcn="",mgmtid="" ,caseID="",
     PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm) 
}

