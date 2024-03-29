#' Add new tree records to the simulation
#'
#' Pass a data frame of new trees. This function should be called
#' only at FVS stop point 6 or 7 (see \link{fvsRun}).
#'
#' @param newtrees a data.frame of new trees with the following required columns:
#'   dbh (inches),species (FVS species index (integer), use \link{fvsGetSpeciesCodes} 
#'   to list permitted codes), ht (ft), cratio (percent), plot (index),
#'   tpa (trees/acre). No missing values allowed.
#' @return invisable return code with the value 0 if the trees were added and 
#'   1 if there was some error.
#' @export
fvsAddTrees <-
function(newtrees)
{
  if (missing(newtrees)) stop ("newtrees must be specified.")
  cns <- colnames(newtrees)
  if (is.null(cns)) stop ("newtrees must have colnames.")
  req <- c("dbh","species","ht","cratio","plot","tpa")
  locrec <- match(req,cns)
  if (any(is.na(locrec))) stop (paste ("absent attributes=",
                                paste(req[is.na(locrec)],collapse=", ")))
  if (any(is.na(newtrees))) stop ("no missing values allowed")
  room <- fvsGetDims()
  mxsp <- room["maxspecies"]
  plts <- room["nplots"]
  room <- room["maxtrees"] - room["ntrees"]
  ntrees <- as.integer(nrow(newtrees))
  if (ntrees > room) stop (paste("room for",room,"and newtrees has",ntrees,"trees"))
  in_dbh     <- as.numeric(newtrees[,locrec[1]])
  in_species <- as.numeric(newtrees[,locrec[2]])
  if (any(in_species == 0)) stop("species codes must be > 0")
  if (any(in_species > mxsp)) stop ("some (all) species codes must be within range for this FVS.")
  in_ht      <- as.numeric(newtrees[,locrec[3]])
  in_cratio  <- as.numeric(newtrees[,locrec[4]])
  in_plot    <- as.numeric(newtrees[,locrec[5]])
  if (any(in_plot == 0)) stop("plot codes must be > 0")
  if (any(in_plot > plts)) stop ("plot codes must be within range for this run.")
  in_tpa     <- as.numeric(newtrees[,locrec[6]])
  rtn <- .Fortran("fvsAddTrees",in_dbh,in_species,in_ht,in_cratio,
                  in_plot,in_tpa,ntrees,as.integer(0),
                  PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
  invisible(rtn[[8]])
}
