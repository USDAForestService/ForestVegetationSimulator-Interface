#' Return the Stand Visualization System objects.
#'
#' @return a named list of trees, snags, and course woody debris (cwd). Each
#'   item in the list is a data.frame of items.
#' @examples
#' fvsGetSVSObjectSet() # return list of empty data frames until a run is made using SVS.     
#' @export
fvsGetSVSObjectSet <-
function()
{

### object types and locations

  svsdims = fvsGetSVSDims()
  svsObjNames = c("objtype","objindex","xloc","yloc")
  nsvsobjs = svsdims["nsvsobjs"] 
  svs = NULL
  for (name in svsObjNames)
  {
    nch = nchar(name)
    atr = vector("numeric",nsvsobjs)
    ans = .C("CfvsSVSObjData",name,nch,"get",nsvsobjs,atr,as.integer(0),
             PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
    if (ans[[6]] == 0) 
    {
      svs = append(svs,list(ans[[5]]))
      names(svs)[length(svs)] = name
    }
  }
  svs = as.data.frame(svs)
  
# fetch the species codes for use below.

  sppCds = fvsGetSpeciesCodes()
 
### live trees

  lives = NULL
  liveptrs = svs$objindex[svs$objtype == 1]
  liveptrs = liveptrs[liveptrs != 0]
  if (length(liveptrs) > 0) 
  {
    lives = fvsGetTreeAttrs(c("species","dbh","ht","crwdth","cratio",
                              "crownwt0","crownwt1","crownwt2","crownwt3"))[liveptrs,] 
    lives = cbind (subset(svs,objtype == 1)[,3:4],lives)
  }
  if (length(lives$species) > 0) lives$species = sppCds[lives$species,3]


### snags

  snagNames = c("snagspp","snagdbh","snaglen","snagfdir","snagstat","snagyear",
                "snagwt0","snagwt1","snagwt2","snagwt3") 
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
      ans = .C("CfvsSVSObjData",name,nch,"get",ndeadobjs,atr,as.integer(0))
      if (ans[[6]] == 0) 
      {
        snags = append(snags,list(ans[[5]]))
        names(snags)[length(snags)] = name
      }
    }

    # age the snag weights

    maxsp  = nrow(sppCds)
    ageWts = c( "snagwt0", "snagwt1", "snagwt2", "snagwt3")
    falyrs = c("fallyrs0","fallyrs1","fallyrs2","fallyrs3")
    year = fvsGetEventMonitorVariables(vars="Year")
    sage= year-snags$snagyear-1
    for (i in 1:length(falyrs))
    { 
      name=falyrs[i] 
      nch =nchar(name)
      atr = vector("numeric",maxsp)
      ans = .C("CfvsFFEAttrs",name,nch,"get",maxsp,atr,as.integer(0))
      if (ans[[6]] == 0) 
      {              
        fal = ans[[5]]
        fal = sage/fal[snags$snagspp]
        snags[[ageWts[i]]] = snags[[ageWts[i]]] * ifelse(fal < 1, 1-fal, 0)
      }
    }

    if (length(snags$snagspp) > 0) snags$snagspp = sppCds[snags$snagspp,3]
    snags = cbind (subset(svs,objtype == 2)[,3:4],as.data.frame(snags)[snagptrs,])
  }

###  cwd:

  cwdNames = c("cwddia","cwdlen","cwdpil","cwddir","cwdwt") 
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
      ans = .C("CfvsSVSObjData",name,nch,"get",ncwdobjs,atr,as.integer(0))
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
