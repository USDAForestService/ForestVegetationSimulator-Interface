#' Specify a thinning/harvest by setting the proportion of each tree record's
#' sampling weight (trees/acre) that will be "cut" in the current cycle. 
#'
#' This function can only be called at stoppoint 2, just after the first call 
#' to the Event Monitor. The memory used to store the proportions in FVS is volatile
#' in that it is used for other purposes after the cut is simulated. The
#' specification of thinning/harvest using this option can be combined with other
#' other FVS thining options including the MinHarv and YardLoss keywords. This
#' feature is implemented using the ThinPrsc keyword. 
#'
#' @param propcut a numeric vector holding the proportions of each tree. If a single value
#'    is entered, it is replicated once of each sample tree in the simulation.
#' @return return code with the value 0 if OK, and non-zero otherwise
#' @export
fvsCutNow <- 
function(propcut)
{
  if (fvsGetRestartcode() != 2) stop("function can only be used at stoppoint 2.")
  if (missing(propcut)) stop("propcut is required.")
  ntrees=fvsGetDims()[["ntrees"]]
  if (length(propcut) == 1) propcut=rep(propcut,ntrees)
  if (length(propcut) != ntrees) stop("a propcut for each tree record is required.")
  r=fvsAddActivity(fvsGetEventMonitorVariables("Year"),"base_thinprsc",c(1.,-1))
  if (r != 0) return(r)
  fvsSetTreeAttrs(list(wk6=propcut))
}
  