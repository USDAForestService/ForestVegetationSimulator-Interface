fvsGetSpeciesCodes <-
function ()
{
  maxsp = fvsGetDims()["maxspecies"]
  all=NULL
  for (i in 1:maxsp) {
    ans = .C("CfvsSpeciesCode",fvs_code="",fia_code="",plant_code="",
          indx=as.integer(i))
    all = rbind(all,c(ans[[1]],ans[[2]],ans[[3]]))
  }
  rownames(all)=1:maxsp
  colnames(all)=c("fvs","fia","plant")
  all
}

