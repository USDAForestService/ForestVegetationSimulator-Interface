fvsGetStandIDs <-
function()
{
  ids=.Fortran("fvsstandid"," "," ",as.integer(0),as.integer(0))
  stdid= if (ids[[3]] == 0) " " else substr(ids[[1]],1,ids[[3]])
  mgmid= if (ids[[4]] == 0) " " else substr(ids[[2]],1,ids[[4]])
  c(standid=stdid,mgmtid=mgmid)
}

