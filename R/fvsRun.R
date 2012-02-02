fvsRun <-
function(stopPointCode=NA,stopPointYear=NA)
{
  if (! is.na(stopPointCode) & ! is.na(stopPointCode)) 
    .Fortran("setstoppointcodes",as.integer(stopPointCode),as.integer(stopPointYear))

  repeat     
  {
    rtn = .Fortran("fvs",as.integer(0)) [[1]]
    if (rtn != 0) break
  }
  rtn
}


