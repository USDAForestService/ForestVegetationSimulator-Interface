#' Return the max dimensions of important Stand Visualization System data storage.
#'
#' @return a named numeric vector.
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsGetSVSDims()      
#' @export
fvsGetSVSDims <-
function()
{
  fvsSVSDims = unlist(.Fortran("fvsSVSDimSizes",as.integer(0),as.integer(0),
        as.integer(0),as.integer(0),as.integer(0),as.integer(0),
        PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm))
  names(fvsSVSDims)=c("nsvsobjs","ndeadobjs","ncwdobjs","mxsvsobjs",
                      "mxdeadobjs","mxcwdobjs")
  fvsSVSDims
}

