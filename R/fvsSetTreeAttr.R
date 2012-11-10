fvsSetTreeAttrs <-
function(vars)
{
  ntrees = fvsGetDims()["ntrees"]
  action = "set"
  vars = as.list(vars)
  rtn = 0
  for (name in names(vars))
  {
    atr = as.numeric(vars[[name]])
    if (length(atr) != ntrees) 
    {
      warning("Length of '",name,"' must be ",ntrees)
      next
    }
    if (any(is.na(atr)))
    {
      warning ("NA found for variable '",name,"'")
      next
    }
    nch =nchar(name)
    ans = .Fortran("fvsTreeAttr",name,nch,action,ntrees,atr,as.integer(0))
    if (ans[[6]] != 0) 
    {
      rtn = if (ans[[6]] > rtn) ans[[6]] else rtn
      warning ("error assigning variable '",name,"'")
      next
    }
  }
  rtn
}

