fvsGetSummary <-
function()
{
  nc = unlist(.Fortran("fvsDimSizes",as.integer(0),as.integer(0),
            as.integer(0),as.integer(0),as.integer(0),as.integer(0)))[[2]]
  asum = vector("list",nc+1)
  summary = vector("integer",20)
  for (i in 1:(nc+1)) {
    asum[[i]] = .Fortran("fvsSummary",as.integer(summary),as.integer(i),as.integer(0),
                          as.integer(0),as.integer(0),as.integer(0))[[1]]
  }
  ans = NULL
  for (r in asum) ans = if (is.null(r)) r else rbind(ans,r)
  rownames(ans)=1:nrow(ans)
  colnames(ans)=c("Year","Age","Tpa","TCuFt","MCuFt","BdFt","RTpa",
      "RTCuFt","RMCuFt","RBdFt","ATBA","ATCCF","ATTopHt","PrdLen","Acc",
      "Mort","SampWt","ForTyp","SizeCls","StkCls")
  ans
}

