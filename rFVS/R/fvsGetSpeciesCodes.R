#' Return a data.frame of species codes
#'
#' @return a data.frame with 1 row for each species and three character columns 
#' \tabular{cl}{
#'    rowname \tab internal FVS numeric species index\cr
#'    fvs     \tab FVS species codes (2-character)\cr
#'    fia	   \tab FIA numeric species codes\cr
#'    plant   \tab Plant codes\cr}
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsGetSpeciesCodes()      
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

