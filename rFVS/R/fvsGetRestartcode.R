#' Get the current FVS restart code.
#'
#' @return an integer
#' @export
fvsGetRestartcode <-
function()
{
  .Fortran("fvsGetRestartCode",as.integer(0),
           PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)[[1]]
}


