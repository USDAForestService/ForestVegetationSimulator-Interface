#' Build a composite summary statistics table
#'
#' A list of summary statistics tables are summarized into a composite (a 
#' weighted average of values) for years that are common to all individual tables.
#'
#' @param allsum a list of summary tables 
#' @return the composite table
#' @export
fvsCompositeSum <- 
function(allsum)
{ 
  if (class(allsum) != "list") stop("allsum must be a list.")
  if (length(allsum) == 0) return (NULL)

  yrs=allsum[[1]][,"Year"]
  for (x in allsum) yrs = intersect(x[,"Year"],yrs)
   
  if (is.null(yrs) | length(yrs) < 1) stop("no common years.")

  mxyr=max(yrs)
  warn=FALSE  
  comp = NULL; sumwt = 0
  fty = NULL
  for (i in 1:length(allsum))
  {
    one = subset (allsum[[i]],allsum[[i]][,"Year"] %in% yrs)
    fty = if (is.null(fty)) one[,c(1,17:20)] else rbind(fty,one[,c(1,17:20)])
    
    # check for removals outside of common years
    if (! warn) 
    {
      rmv = allsum[[i]][,c(1,7)]    
      noncom=setdiff(rmv[,1],yrs)
      if (length(noncom) > 0)
      {
        rmv = subset(rmv,rmv[,1] %in% noncom)
        rmv = subset(rmv, rmv[,"Year"] <= mxyr)
        if (nrow(rmv) > 0)
        {
          if (sum(rmv[,2]) > 0) 
          {
            warn=TRUE
            warning (paste("Composite removals do not",
                "include removals in cycle years that are not",
                "common to all summary tables."))     
    }}}}
         
    sum1 = apply(one[,1:16],2,function (x,one) x*one[,17], one)
    if (is.null(comp)) 
    {
      sumwt = one[,17]
      comp = sum1
    } else
    {
      sumwt = sumwt + one[,17]
      comp  = comp + sum1
    }
  } 
  ans = apply(comp,2,function (x,sumwt) x/sumwt, sumwt)
  ans = cbind(ans,SampWt=sumwt)
  
  fts=as.character(unique(sort(fty[,3])))
  ForType = matrix(0,nrow=length(yrs),ncol=length(fts))
  colnames(ForType)=fts
  rownames(ForType)=yrs
  for (i in 1:length(allsum))
  {
    one = subset (allsum[[i]],allsum[[i]][,"Year"] %in% yrs)[,c(17,18)]
    for (n in fts) 
    {  
      add = n == one[,2]
      ForType[add,n] = ForType[add,n]+one[add,1]
    }
  }
  
  fts=as.character(unique(sort(fty[,4])))
  SizeCls = matrix(0,nrow=length(yrs),ncol=length(fts))
  colnames(SizeCls)=fts
  rownames(SizeCls)=yrs
  for (i in 1:length(allsum))
  {
    one = subset (allsum[[i]],allsum[[i]][,"Year"] %in% yrs)[,c(17,19)]
    for (n in fts) 
    {  
      add = n == one[,2]
      SizeCls[add,n] = SizeCls[add,n]+one[add,1]
    }
  }

  fts=as.character(unique(sort(fty[,5])))
  StkCls = matrix(0,nrow=length(yrs),ncol=length(fts))
  colnames(StkCls)=fts
  rownames(StkCls)=yrs
  for (i in 1:length(allsum))
  {
    one = subset (allsum[[i]],allsum[[i]][,"Year"] %in% yrs)[,c(17,20)]
    for (n in fts) 
    {  
      add = n == one[,2]
      StkCls[add,n] = StkCls[add,n]+one[add,1]
    }
  }
  ans=list(sumTable=ans,ForType=ForType,SizeCls=SizeCls,StkCls=StkCls)
  ans
}

