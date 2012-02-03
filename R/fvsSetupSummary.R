fvsSetupSummary <- function(asum)
{
  std=c("Tpa","TCuFt","MCuFt","BdFt")
  rstd=paste("R",std,sep="")
  new=asum[,"RTpa"] > 0
  if (sum(new) > 0) 
  {
    dups=unlist(lapply(1:length(new),function(x,new) if (new[x]) rep(x,2) else x, new))
    asum=asum[dups,]
    for (row in 1:(nrow(asum)-1))
    {
      nrow=row+1
      if (dups[row] == dups[nrow])
      {
        asum[nrow,std] = asum[nrow,std] - asum[nrow,rstd]
        asum[row,rstd] = 0
        asum[row,11:20] = NA
        dups[nrow]=0
      }
    }
  }
  tprd=apply(asum[,rstd],2,cumsum)+asum[,std]
  colnames(tprd)=paste("TPrd",std,sep="")
  asum=cbind(asum,tprd)
  asum
}  
     
  
