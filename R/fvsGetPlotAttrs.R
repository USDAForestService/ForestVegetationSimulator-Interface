fvsGetPlotAttrs <-
function(vars)
{
  nplots = fvsGetDims()["nplots"]
  atr = vector("numeric",nplots)
  action="get"
  all = NULL
  for (name in vars)
  {
    nch =nchar(name)
    ans = .Fortran("fvsPlotAttr",name,nch,action,nplots,atr,as.integer(0))
    if (ans[[6]] == 0) 
    {
      all = append(all,list(ans[[5]]))
      names(all)[length(all)] = name
    }
  }
  as.data.frame(all)
}

