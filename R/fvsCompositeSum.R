fvsCompositeSum <- 
function(allsum,yrs=NULL)
{ 
  if (class(allsum) != "list") stop("allsum must be a list.")
  if (length(allsum) == 0) return (NULL)
  if (length(allsum) == 1) return (allsum[[1]])
  if (is.null(yrs)) yrs=allsum[[1]][,"Year"]
  for (x in allsum) yrs = intersect(x[,"Year"],yrs)
   
  if (is.null(yrs) | length(yrs) < 1) stop("no common years.")

#TODO: This code will not compute total production correctly if there are
# removals in cycles other than those within the "common years" vector. 
  
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

