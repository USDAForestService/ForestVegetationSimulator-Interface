#' Return a data.frame of species codes
#'
#' @return a data.frame with 1 row for each species and three character columns 
#'   of the "fvs", "fia", and "plant" codes. 
#' @export
fvsGetSpeciesCodes <-
function ()
{
  maxsp = fvsGetDims()["maxspecies"]
  all=NULL
  for (i in 1:maxsp) {
    ans = .C("CfvsSpeciesCode",fvs_code="",fia_code="",plant_code="",
             indx=as.integer(i),PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
    all = rbind(all,c(ans[[1]],ans[[2]],ans[[3]]))
  }
  rownames(all)=1:maxsp
  colnames(all)=c("fvs","fia","plant")
  all
}

