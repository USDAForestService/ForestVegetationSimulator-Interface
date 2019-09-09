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
    ans = .C("CfvsTreeAttr",name,nch,action,ntrees,atr,as.integer(0))
    if (ans[[6]] == 0) 
    {
      all = append(all,list(ans[[5]]))
      names(all)[length(all)] = name
    }
  }
  as.data.frame(all)
}

