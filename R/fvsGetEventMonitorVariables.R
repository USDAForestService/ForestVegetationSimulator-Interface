fvsGetEventMonitorVariables <-
function(vars)
{
  atr = vector("numeric",length(vars))
  all = NULL
  for (name in vars)
  {
    nch = as.integer(nchar(name))
    all = c(all,.Fortran("fvsevmonattr",name,nch,"get",
                as.double(0),as.integer(0))[[4]])
  }
  names(all) = vars
  all
}

