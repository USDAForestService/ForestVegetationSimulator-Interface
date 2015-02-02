fvsGetRestartcode <-
function()
{
  .Fortran("fvsGetRestartCode",as.integer(0))[[1]]
}


