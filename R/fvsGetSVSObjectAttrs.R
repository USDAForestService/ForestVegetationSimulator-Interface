fvsGetSVSObjectAttrs <-
function()
{
  svsdims = fvsGetSVSDims()
  svsObjNames = c("objtype","objindex","xloc","yloc")
  nsvsobjs = svsdims["nsvsobjs"] 
  svs = NULL
  for (name in svsObjNames)
  {
    nch =nchar(name)
    atr = vector("numeric",nsvsobjs)
    ans = .Fortran("fvsSVSObjData",name,nch,"get",nsvsobjs,atr,as.integer(0))
    if (ans[[6]] == 0) 
    {
      svs = append(svs,list(ans[[5]]))
      names(svs)[length(svs)] = name
    }
  }
  svs = as.data.frame(svs)
 
  lives = NULL
  liveptrs = svs$objindex[svs$objtype == 1]
  liveptrs = liveptrs[liveptrs != 0]
  if (length(liveptrs) > 0) 
  {
    lives = fvsGetTreeAttrs(c("species","dbh","ht","crwdth","cratio",
                              "crownwt0","crownwt1"))[liveptrs,] 
    lives = cbind (subset(svs,objtype == 1)[,3:4],lives)
  }

  snagNames = c("snagdia","snaglen","snagfdir","snagstat","snagwt0","snagwt1") 
  snags = NULL
  snagptrs = svs$objindex[svs$objtype == 2]
  snagptrs = snagptrs[snagptrs != 0]
  ndeadobjs = svsdims["ndeadobjs"]
  if (length(snagptrs) > 0) 
  {  
    for (name in snagNames)
    {
      nch =nchar(name)
      atr = vector("numeric",ndeadobjs)
      ans = .Fortran("fvsSVSObjData",name,nch,"get",ndeadobjs,atr,as.integer(0))
      if (ans[[6]] == 0) 
      {
        snags = append(snags,list(ans[[5]]))
        names(snags)[length(snags)] = name
      }
    }
    snags = cbind (subset(svs,objtype == 2)[,3:4],as.data.frame(snags)[snagptrs,])
  }

  cwdNames = c("cwddia","cwdpil","cwddir","cwdwt") 
  cwd = NULL
  cwdptrs = svs$objindex[svs$objtype == 4]
  cwdptrs = cwdptrs[cwdptrs != 0]
  ncwdobjs = svsdims["ncwdobjs"]
  if (length(cwdptrs) > 0) 
  {  
    for (name in cwdNames)
    {
      nch =nchar(name)
      atr = vector("numeric",ncwdobjs)
      ans = .Fortran("fvsSVSObjData",name,nch,"get",ncwdobjs,atr,as.integer(0))
      if (ans[[6]] == 0) 
      {
        cwd = append(cwd,list(ans[[5]]))
        names(cwd)[length(cwd)] = name
      }
    }
    cwd = cbind (subset(svs,objtype == 4)[,3:4],as.data.frame(cwd))
  }
  
  list(trees=lives, snags=snags, cwd=cwd)

}
