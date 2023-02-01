#' Return the values of species-specific data
#'
#' @param vars a character vector of species-level attributes desired. See 
#'   \url{https://www.fs.usda.gov/fmsc/ftp/fvs/docs/gtr/EssentialFVS.pdf} for related details.
#'   The attributes can be any of the following:
#' \tabular{cl}{
#'    spccf	    \tab CCF for each species, recomputed in FVS so setting may have no effect (depending on variant)\cr
#'    spsdi	    \tab SDI maximums for each species \cr
#'    spsiteindx \tab Species site indices\cr
#'    bfmind     \tab board foot minimum dbh for each species \cr
#'    bftopd     \tab board foot top diameter for each species \cr
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
#' @return a data.frame where the columns are attributes and the rows are species. 
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsGetSpeciesAttrs(vars=c("spccf","spsiteindx"))      
#' @export
fvsGetSpeciesAttrs <-                                             
function(vars)
{
  maxspecies = fvsGetDims()["maxspecies"]
  atr = vector("numeric",maxspecies)   
  action="get"
  all = list()
  for (name in vars)
  {                                               
    nch =nchar(name)
    ans = .C("CfvsSpeciesAttr",name,nch,action,atr,as.integer(0),
             PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
    if (ans[[5]] == 0) all[[name]] = ans[[4]]
  }
  as.data.frame(all)
}

