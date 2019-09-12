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
    ans = .Fortran("fvsSpeciesAttr",name,nch,action,atr,as.integer(0))
    if (ans[[5]] == 0) all[[name]] = ans[[4]]
  }
  as.data.frame(all)
}

