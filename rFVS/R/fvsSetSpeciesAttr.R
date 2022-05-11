#' Set the values of species-specific data
#'
#' @param vars a named list of numeric vectors where the name corresponds to an attribute and
#'  the vector contains values for each species (in order). The attributes can be any of these:
#'  \tabular{cl}{
#'    spccf	    \tab CCF for each species, recomputed in FVS so setting will likely have no effect\cr
#'    spsdi	    \tab SDI maximums for each species \cr
#'    spsiteindx \tab Species site indices\cr}
#' @return scalar integer 0 signals OK and 1 signals an error.
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    vars = fvsGetSpeciesAttrs(vars=c("spccf","spsdi","spsiteindx"))
#'    fvsSetSpeciesAttrs(vars=vars)      
#' @export
fvsSetSpeciesAttrs <-
function(vars)
{
  maxspecies = fvsGetDims()["maxspecies"]
  action = "set"
  if (!is.list(vars)) stop("vars must be a list")
  if (is.null(names(vars))) stop ("vars must have names")
  rtn = 0
  for (name in names(vars))
  {
    atr = as.numeric(vars[[name]])
    if (length(atr) != maxspecies) 
    {
      warning("Length of '",name,"' must be ",maxspecies)
      next
    }
    if (any(is.na(atr)))
    {
      warning ("NA(s) found for variable '",name,"'")
      next
    }
    nch =nchar(name)
    ans = .C("CfvsSpeciesAttr",name,nch,action,atr,as.integer(0),
             PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
    if (ans[[5]] != 0) 
    {
      rtn = if (ans[[5]] > rtn) ans[[5]] else rtn
      warning ("error assigning variable '",name,"'")
      next
    }
  }
  invisible(rtn)
}

