#' Get Attributes of Trees. 
#'
#' @param vars a character vector of tree attribute names.
#' @return a data.frame with as many rows as there are tree records and a column
#'    for each attribute.
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

