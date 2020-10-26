# $Id$

# these two variables can be set to the source directories of fvsBin and rFVS

if (isLocal() && .Platform$OS.type == "windows") {
  fvsBinDir=paste0(getwd(),"/FVSbin")
  rFVSDir  =paste0(getwd(),"/R")
  pfexists = file.exists("projectId.txt")
  if (!pfexists || (pfexists && file.size("projectId.txt") < 2))
      cat("title= ",basename(getwd()),"\n",file="projectId.txt")
  prjid = scan("projectId.txt",what="",sep="\n",quiet=TRUE)
  prjDir=prjid[grep("^title",prjid)]
  prjDir=trim(unlist(strsplit(prjDir,split="=",fixed=TRUE))[2])
  prjDir=paste0("C:/FVS/",prjDir)  
}else {
fvsBinDir=NULL
rFVSDir  =NULL
}

#when fvsBinDir is NULL (or when the directory does not exist), 
#then the following url is tried. If the url is NULL it is not tried.

fvsBinURL = if (.Platform$OS.type == "windows") 
     "http://www.fs.fed.us/.ftproot/pub/fmsc/ftp/fvs/software/FVSOnline" else NULL

pgmList= list(
  FVSak = "Southeast AK - Coastal BC",
  FVSbm = "Blue Mountains, Oregon",
  FVSca = "Inland CA, Southern Cascades",
  FVSci = "Central ID",
  FVScr = "Central Rockies",
  FVSec = "East Cascades, Washington",
  FVSem = "Eastern Montana",
  FVSie = "Inland Empire",
  FVSnc = "Klammath Mountains, Northern CA",
  FVSoc = "ORGANON Southwest",
  FVSop = "ORGANON Pacific Northwest",
  FVSso = "South Central OR N CA",
  FVStt = "Tetons, Wyoming",
  FVSut = "Utah",
  FVSwc = "West Cascades",
  FVSpn = "Pacific Northwest Coast",
  FVSws = "Western Sierra Nevada, CA",
  FVScs = "Central States",
  FVSkt = "Kootenai/Kaniksu/Tally LK, ID - MT",
  FVSls = "Lake States",
  FVSne = "Northeast",
  FVSsn = "Southern")

