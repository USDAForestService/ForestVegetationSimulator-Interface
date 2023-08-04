#' Get Event Monitor Variables
#'
#' @param vars a character vector of Event Monitor names. Consult FVS documentation
#'   for the possible variable names. 
#' @return a named numeric vector of the variables, NA if the variable name does not exist
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsGetEventMonitorVariables(c("year","atpa","aba","mybba","myaba"))
#'    fvsSetEventMonitorVariables(c("myaba"=100,"another"=40))
#'    fvsGetEventMonitorVariables(c("myaba","another"))
#' @export
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
    ans = .C("CfvsEvmonAttr",tolower(name),nch,"get",as.double(0),as.integer(0),
              PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
    all = c(all,if (ans[[5]] == 0) ans[[4]] else NA)
  }
  names(all) = vars
  all
}

#' Set Event Monitor Variables
#'
#' @param vars a named numeric vector of the variables and corresponding values.
#' @return a named numeric vector of the variables, NA if the variable name does not exit
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsGetEventMonitorVariables(c("year","atpa","aba","mybba","myaba"))
#'    fvsSetEventMonitorVariables(c("myaba"=100,"another"=40))
#'    fvsGetEventMonitorVariables(c("myaba","another"))
#' @export
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
    ans = .C("CfvsEvmonAttr",tolower(name),nch,"set",
              as.double(vars[name]),as.integer(0),
              PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
    all = c(all,if (ans[[5]] == 0) ans[[4]] else NA)
  }
  names(all) = names(vars)
  all
}

