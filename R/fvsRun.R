fvsRun <-
function(stopPointCode=NA,stopPointYear=NA)
{
  if (! is.na(stopPointCode) & ! is.na(stopPointCode)) 
    .Fortran("fvsSetStoppointCodes",as.integer(stopPointCode),as.integer(stopPointYear))

  repeat     
  {
    rtn = .Fortran("fvs",as.integer(0)) [[1]]
    stopPoint <- .Fortran("fvsGetRestartCode",as.integer(0))[[1]]
    if (rtn != 0) break
    if (stopPoint != 0) break
  }
  invisible(rtn)
}


