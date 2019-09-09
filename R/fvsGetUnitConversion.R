fvsUnitConversion <-
function(name)
{
  nch =nchar(name)
  ans = .C("CfvsUnitConversion",name,nch,as.numeric(0),as.integer(0))
  if (ans[[4]] == 0) return(ans[[3]]) else return(NA)
}

