#' Get FVS Units Conversion Factor
#'
#' @param name a character string name of the desired conversion factor.
#' @return the numeric value, or NA if FVS does not contain the desired factor.
#' @export
fvsUnitConversion <-
function(name)
{
  nch =nchar(name)
  ans = .C("CfvsUnitConversion",name,nch,as.numeric(0),as.integer(0),
        PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
  if (ans[[4]] == 0) return(ans[[3]]) else return(NA)
}

