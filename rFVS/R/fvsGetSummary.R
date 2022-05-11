#' Return a data.frame of the summary statistics
#'
#' @return summary statistics with one row per period
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsGetSummary()  # will be NULL until a run is made    
#' @export
fvsGetSummary <-
function()
{
  nc = fvsGetDims()["ncycles"]
  if (nc == 0) return(NULL)
  
  asum = vector("list",nc+1)
  summary = vector("integer",20)
  for (i in 1:(nc+1)) {
    asum[[i]] = .Fortran("fvsSummary",as.integer(summary),as.integer(i),as.integer(0),
                          as.integer(0),as.integer(0),as.integer(0),
                          PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)[[1]]
  }
  ans = NULL
  for (r in asum) ans = rbind(ans,r)
  rownames(ans)=1:nrow(ans)
  colnames(ans)=c("Year","Age","Tpa","TCuFt","MCuFt","BdFt","RTpa",
      "RTCuFt","RMCuFt","RBdFt","ATBA","ATCCF","ATTopHt","PrdLen","Acc",
      "Mort","SampWt","ForTyp","SizeCls","StkCls")
  ans
}

