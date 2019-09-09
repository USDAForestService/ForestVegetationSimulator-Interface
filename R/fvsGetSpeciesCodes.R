fvsGetSpeciesCodes <-
function ()
{
  maxsp = fvsGetDims()["maxspecies"]
  all=NULL
  for (i in 1:maxsp) {
    ans = .C("CfvsSpeciesCode",fvs_code="",fia_code="",plant_code="",
          indx=as.integer(i),nchfvs=as.integer(0),nchfia=as.integer(0),nchplant=as.integer(0),
          as.integer(0))
    ans = c(substr(ans[[1]],1,ans[[5]]),substr(ans[[2]],1,ans[[6]]),
            substr(ans[[3]],1,ans[[7]]))
    all = rbind(all,ans)
  }
  rownames(all)=1:maxsp
  colnames(all)=c("fvs","fia","plant")
  all
}

