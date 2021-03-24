#' Return the max dimensions of important FVS data storage.
#'
#' @return a named numeric vector.
#' @export
fvsGetDims <-
function()
{
  fvsDims = unlist(.Fortran("fvsDimSizes",as.integer(0),as.integer(0),
        as.integer(0),as.integer(0),as.integer(0),as.integer(0),as.integer(0),
        PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm))
  names(fvsDims)=c("ntrees","ncycles","nplots","maxtrees","maxspecies",
                  "maxplots","maxcycles")
  fvsDims
}

