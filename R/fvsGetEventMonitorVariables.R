fvsGetEventMonitorVariables <-
function(vars)
{
  atr = vector("numeric",length(vars))
  all = NULL
  for (name in vars)
  {
    nch = as.integer(nchar(name))
    ans = .Fortran("fvsevmonattr",name,nch,"get",
                as.double(0),as.integer(0))
    all = c(all,if (ans[[5]] == 0) ans[[4]] else NA)
  }
  names(all) = vars
  all
}

