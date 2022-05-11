#' Return the values of species-specific data
#'
#' @param vars a character vector of species-level attributes desired:
#' \tabular{cl}{
#'    spccf	    \tab CCF for each species, recomputed in FVS so setting will likely have no effect\cr
#'    spsdi	    \tab SDI maximums for each species \cr
#'    spsiteindx \tab Species site indices\cr}
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

