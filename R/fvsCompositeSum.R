fvsCompositeSum <- function(allsum)
{ 
  if (class(allsum) != "list") stop("allsum must be a list.")
  if (length(allsum) == 0) return (NULL)
  if (length(allsum) == 1) return (allsum[[1]])
  yrs=unique(unlist(lapply(allsum,function (x) x[,"Year"])))
  
  comp = NULL; sumwt = 0
  for (i in 1:length(allsum))
  {
    one = subset (allsum[[i]],allsum[[i]][,"Year"] %in% yrs)[,c(1:17)]
    
    sum1 = apply(one[,-ncol(one)],2,function (x,one) x*one[,ncol(one)], one)
    if (is.null(comp)) 
    {
      sumwt = one[,ncol(one)]
      comp = sum1
    } else
    {
      sumwt = sumwt + one[,ncol(one)]
      comp  = comp + sum1
    }
  } 
  ans = apply(comp,2,function (x,sumwt) x/sumwt, sumwt)
  cbind(ans,SampWt=sumwt)
}

