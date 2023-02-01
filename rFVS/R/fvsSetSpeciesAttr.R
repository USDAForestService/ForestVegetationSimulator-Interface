#' Set the values of species-specific data
#'
#' @param vars a named list of numeric vectors where the name corresponds to an attribute and
#'   the vector contains values for each species (in order). See 
#'   \url{https://www.fs.usda.gov/fmsc/ftp/fvs/docs/gtr/EssentialFVS.pdf} for related details.
#'   The attrubytes can be any of the following:
#' \tabular{cl}{
#'    spccf	    \tab CCF for each species, recomputed in FVS so setting may have no effect (depending on variant)\cr
#'    spsdi	    \tab SDI maximums for each species \cr
#'    spsiteindx \tab Species site indices\cr
#'    bfmind     \tab board foot minimum dbh for each species \cr
#'    bftopd    \tab board foot top diameter for each species \cr
#'    bfstmp     \tab board foot stump height for each species \cr
#'    frmcls     \tab board foot form class for each species \cr
#'    bfmeth     \tab board foot calculation method for each species \cr
#'    mcmind     \tab murchantable cubic volume minimum dbh for each species \cr
#'    mctopd     \tab murchantable cubic volume top diameter for each species \cr
#'    mcstmp     \tab murchantable cubic volume stump height for each species \cr
#'    mcmeth     \tab murchantable cubic volume calculation method for each species \cr
#'    baimult    \tab basal area increment multiplier for each species \cr
#'    htgmult    \tab height growth multiplier for each species \cr
#'    mortmult   \tab mortality rate multiplier for each species \cr
#'    mortdia1   \tab lower diameter limit to apply the multiplier for each species \cr
#'    mortdia2   \tab upper diameter limit to apply the multiplier for each species \cr
#'    regdmult   \tab multiplier for diameter growth of regeneration for each species \cr
#'    reghmult   \tab multiplier for height growth of regeneration for each species}
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

