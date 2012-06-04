fvsAddActivity <-
function(year,activity,parms=NULL)
{
  if (missing(year)) stop ("year must be specified.")
  if (missing(activity)) stop ("activity must be specified.")                
  if (class(activity) == "character") 
  {
    actk=c(200,201,202,203,222,223,224,225,226,227,
           228,230,231,232,233,234,235,236,248,249,
           430,431)
    kwd=c("MINHARV","SPECPREF","TCONDMLT","YARDING",
          "THINAUTO","THINBTA","THINATA ","THINBBA",
          "THINABA","THINPRSC","THINDBH ","THINSDI","THINCC",
          "THINHT","THINMIST","THINRDEN","THINPT","THINRDSL",
          "SETPTHIN","PRUNE","PLANT","NATURAL")
    activity = sub(" ","",toupper(activity))
    iactk = match(activity,kwd)
    if (is.na(iactk)) stop(activity," could not be translated to a code.")
    iactk = as.integer(actk[iactk])
  }
  else
  {
    iactk = as.integer(activity)
  }
  if (is.null(parms))
  {
    inprms = as.numeric(0)
    nprms  = as.integer(0)
  }
  else
  {
    if (any(is.na(parms))) stop ("parms can not contain NAs")
    inprms = as.numeric(parms)
    nprms  = length(parms)
  }
  if (is.na(year)) stop ("year may not be an NA") 
  year = as.integer(year)
  rtnCode = as.integer(0)
  rtn = .Fortran("fvsAddActivity",year,iactk,inprms,nprms,rtnCode)
  rtn[[5]]
}

