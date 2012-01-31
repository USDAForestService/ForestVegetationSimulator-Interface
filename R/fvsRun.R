fvsRun <-
function(stopPointCode=NA,stopPointYear=NA)
{
  if (! is.na(stopPointCode) & ! is.na(stopPointCode)) 
    .Fortran("setstoppointcodes",as.integer(stopPointCode),as.integer(stopPointYear))
    
  rtn=as.integer(0)
  while (rtn == 0)
  {
    rtn=unlist(.Fortran("fvs",as.integer(rtn)))
  }
  if (rtn == 2) .Fortran("filclose")
  rtn
}


