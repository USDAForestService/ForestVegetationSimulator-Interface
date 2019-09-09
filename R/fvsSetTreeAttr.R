fvsSetTreeAttrs <-
function(vars)
{
  ntrees = fvsGetDims()["ntrees"]
  if (!is.list(vars)) stop("vars must be a list")
  if (is.null(names(vars))) stop ("vars must have names")
  action = "set"
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
      warning ("NA(s) found for variable '",name,"'")
      next
    }
    nch =nchar(name)
    ans = .C("CfvsTreeAttr",name,nch,action,ntrees,atr,as.integer(0))
    if (ans[[6]] != 0) 
    {
      rtn = if (ans[[6]] > rtn) ans[[6]] else rtn
      warning ("error assigning variable '",name,"'")
      next
    }
  }
  invisible(rtn)
}

