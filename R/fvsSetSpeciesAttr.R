fvsSetSpeciesAttrs <-
function(vars)
{
  maxspecies = fvsGetDims()["maxspecies"]
  action = "set"
  if (!is.list(vars)) stop("vars must be a list")
  if (is.null(names(vars))) stop ("vars must have names")
  rtn = 0
  for (name in names(vars))
  {
    atr = as.numeric(vars[[name]])
    if (length(atr) != maxspecies) 
    {
      warning("Length of '",name,"' must be ",maxspecies)
      next
    }
    if (any(is.na(atr)))
    {
      warning ("NA(s) found for variable '",name,"'")
      next
    }
    nch =nchar(name)
    ans = .C("CfvsSpeciesAttr",name,nch,action,atr,as.integer(0))
    if (ans[[5]] != 0) 
    {
      rtn = if (ans[[5]] > rtn) ans[[5]] else rtn
      warning ("error assigning variable '",name,"'")
      next
    }
  }
  invisible(rtn)
}

