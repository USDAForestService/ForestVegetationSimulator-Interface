fvsGetEventMonitorVariables <-
function(vars)
{
  if (missing(vars)) stop ("vars must be present")
  if (class(vars) != "character") stop ("vars must be type character")
  atr = vector("numeric",length(vars))
  all = NULL
  for (name in vars)
  {
    nch = as.integer(nchar(name))
    ans = .Fortran("fvsevmonattr",tolower(name),nch,"get",
                as.double(0),as.integer(0))
    all = c(all,if (ans[[5]] == 0) ans[[4]] else NA)
  }
  names(all) = vars
  all
}

fvsSetEventMonitorVariables <-
function(vars)
{
  if (missing(vars)) stop ("vars must be present")
  if (class(vars) != "numeric") stop ("vars must be type numeric")
  if (is.null(names(vars))) stop ("vars must be named")
  atr = vector("numeric",length(vars))
  all = NULL
  for (name in names(vars))
  {
    nch = as.integer(nchar(name))
    ans = .Fortran("fvsevmonattr",tolower(name),nch,"set",
                as.double(vars[name]),as.integer(0))
    all = c(all,if (ans[[5]] == 0) ans[[4]] else NA)
  }
  names(all) = vars
  all
}

