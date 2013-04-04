fvsGetSVSDims <-
function()
{
  fvsSVSDims = unlist(.Fortran("fvsSVSDimSizes",as.integer(0),as.integer(0),
        as.integer(0),as.integer(0),as.integer(0),as.integer(0)))
  names(fvsSVSDims)=c("nsvsobjs","ndeadobjs","ncwdobjs","mxsvsobjs",
                   "mxdeadobjs","mxcwdobjs")
  fvsSVSDims
}

