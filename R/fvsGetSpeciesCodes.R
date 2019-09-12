fvsGetSpeciesCodes <-
function ()
{
  maxsp = fvsGetDims()["maxspecies"]

  all=NULL
  for (i in 1:maxsp) {
    ans = .Fortran("fvsSpeciesCode",strrep(" ",10),strrep(" ",10),strrep(" ",10),
          as.integer(i),as.integer(0),as.integer(0),as.integer(0),
          as.integer(0))
    ans = c(substr(ans[[1]],1,ans[[5]]),substr(ans[[2]],1,ans[[6]]),
            substr(ans[[3]],1,ans[[7]]))
    all = if (is.null(all)) ans else rbind(all,ans)
  }
  rownames(all)=1:maxsp
  colnames(all)=c("fvs","fia","plant")
  all
}

