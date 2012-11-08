fvsSetCmdLine <-
function(cl = NULL)
{
  if (is.null(cl)) cl=paste(commandArgs(trailingOnly = TRUE),collapse=" ")
  nch = as.integer(nchar(cl))
  invisible(if (nch > 0) .Fortran("fvsSetCmdLine",cl,nch,as.integer(0)) else NULL)
}

