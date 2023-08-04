#' Get Attributes of Trees. 
#'
#' @param vars a character vector of any of these tree attribute names:
#'  \tabular{cl}{
#'    id \tab Tree identification number (may not be unique) \cr
#'    species \tab FVS numeric species code \cr
#'    tpa \tab Trees per acre \cr
#'    mort \tab Trees per acre predicted to die \cr
#'    dbh \tab Diameter breast height (inches) \cr 
#'    dg \tab Diameter growth scaled to cycle length (inches) \cr
#'    ht \tab Height (feet) \cr
#'    htg \tab Height growth scaled to cycle length (feet) \cr
#'    crwdth \tab Crown width (feet) \cr
#'    cratio \tab Crown ratio (proportion of height in live crown \cr
#'    age \tab Tree age \cr
#'    plot \tab FVS numeric plot index \cr
#'    tcuft \tab Total cubic volume \cr
#'    mcuft \tab Merch cubic volume \cr
#'    bdft \tab Board foot volume \cr
#'    ptbal \tab Point basal area in larger trees (sq ft/acre) \cr
#'    bapctile \tab Percentile in the distribution of tree basal area \cr
#'    defect \tab Defect coded as 11223344 as described below \cr
#'    mgmtcd \tab Tree value class or management code (1, 2, or 3) \cr
#'    plotsize \tab Size of plot tree was sampled from \cr
#'    crownwt0 \tab Weight of foliage (pounds) \cr
#'    crownwt1 \tab Weight of 0-.25 inch crown material (pounds) \cr
#'    crownwt2 \tab Weight of .25-1 inch crown material (pounds) \cr
#'    crownwt3 \tab Weight of 1-3 inch crown material (pounds) \cr
#'    crownwt4 \tab Weight of 3-6 inch crown material (pounds) \cr
#'    crownwt5 \tab Weight of 06-12 inch crown material (pounds) \cr}
#' @return a data.frame with as many rows as there are tree records and a column
#'    for each attribute.
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsGetTreeAttrs(vars="dbh","ht","tpa")) # return an empty data frame until a run is made.     
#' @export
fvsGetTreeAttrs <-
function(vars)
{
  ntrees = fvsGetDims()["ntrees"]
  atr = vector("numeric",ntrees)
  action="get"
  all = NULL
  for (name in vars)
  {
    nch =nchar(name)
    ans = .C("CfvsTreeAttr",name,nch,action,ntrees,atr,as.integer(0),
             PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
    if (ans[[6]] == 0) 
    {
      all = append(all,list(ans[[5]]))
      names(all)[length(all)] = name
    }
  }
  as.data.frame(all)
}

