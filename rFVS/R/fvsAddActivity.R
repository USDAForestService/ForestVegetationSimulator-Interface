#' Add an an FVS Activity to the activity schedule
#'
#' Pass in an activity code and parameters to the FVS activity schedule. A list of
#' possible activity codes is returned if no arguments are passed.
#'
#' @param year year the activity is to be scheduled
#' @param activity activity code as a number or character string 
#' @param parms a numeric vector of parameters for the activity
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsAddActivity()  # a list of all possible activities is return
#'    fvsAddActivity(1993,"base_yardloss",c(0.50, 0.70, 0.50))
#'    fvsAddActivity(1993,"base_thindbh",c(0.00,12.00,1.00,0.00,0.00))
#' @return invisable return code with the value 0 if the activity was added and 1 if there was some error
#'   or, in the case where no arguments are specified, a named vector of possible activity codes.
#' @export

fvsAddActivity <-
function(year,activity,parms=NULL)
{
  activities <-      c(BASE_TREELIST=  80,BASE_CRNMULT =  81,
    BASE_MANAGED =  82,BASE_FIXCW   =  90,BASE_BAIMULT =  91,BASE_HTGMULT =  92,
    BASE_REGHMULT=  93,BASE_MORTMULT=  94,ESTB_SPECMULT=  95,BASE_REGDMULT=  96,
    BASE_FIXMORT =  97,BASE_FIXDG   =  98,BASE_FIXHTG  =  99,BASE_SYSTEM  = 100,
    DBIN_SQLIN   = 101,DBIN_SQLOUT  = 102,BASE_HTGSTOP = 110,BASE_TOPKILL = 111,
    BASE_SETSITE = 120,BASE_ATRTLIST= 198,BASE_CUTLIST = 199,BASE_MINHARV = 200,
    BASE_SPECPREF= 201,BASE_TCONDMLT= 202,BASE_YARDLOSS= 203,BASE_FVSSTAND= 204,
    BASE_CRUZFILE= 205,BASE_MCDEFECT= 215,BASE_BFDEFECT= 216,BASE_VOLUME  = 217,
    BASE_BFVOLUME= 218,BASE_THINAUTO= 222,BASE_THINBTA = 223,BASE_THINATA = 224,
    BASE_THINBBA = 225,BASE_THINABA = 226,BASE_THINPRSC= 227,BASE_THINDBH = 228,
    BASE_SALVAGE = 229,BASE_THINSDI = 230,BASE_THINCC  = 231,BASE_THINHT  = 232,
    BASE_THINMIST= 233,BASE_THINRDEN= 234,BASE_THINPT  = 235,BASE_THINRDSL= 236,
    BASE_SETPTHIN= 248,BASE_PRUNE   = 249,BASE_COMPRESS= 250,BASE_FERTILIZ= 260,
    ESTB_TALLY   = 427,ESTB_TALLYONE= 428,ESTB_TALLYTWO= 429,ESTB_PLANT   = 430,
    ESTB_NATURAL = 431,ESTB_ADDTREES= 432,ESTB_STOCKADJ= 440,ESTB_HTADJ   = 442,
    BASE_RESETAGE= 443,ESTB_SPROUT  = 450,ESTB_NATURAL = 490,ESTB_BURNPREP= 491,
    ESTB_MECHPREP= 493,COVR_COVER   = 900,MIST_MISTMULT=2001,MIST_MISTPREF=2002,
    MIST_MISTMORT=2003,MIST_MISTHMOD=2004,MIST_MISTGMOD=2005,MIST_MISTPINF=2006,
    MIST_MISTABLE=2007,FIRE_SALVSP  =2501,FIRE_SOILHEAT=2503,FIRE_BURNREPT=2504,
    FIRE_MOISTURE=2505,FIRE_SIMFIRE =2506,FIRE_FLAMEADJ=2507,FIRE_POTFIRE =2508,
    FIRE_SNAGOUT =2512,FIRE_FUELOUT =2515,FIRE_SALVAGE =2520,FIRE_FUELINIT=2521,
    FIRE_SNAGINIT=2522,FIRE_PILEBURN=2523,FIRE_FUELTRET=2525,FIRE_FUELREPT=2527,
    FIRE_MORTREPT=2528,FIRE_DROUGHT =2529,FIRE_FUELMOVE=2530,FIRE_FUELMODL=2538,
    FIRE_DEFULMOD=2539,FIRE_CARBREPT=2544,FIRE_CARBCUT =2545,FIRE_CANFPROF=2547,
    FIRE_FUELFOTO=2548,FIRE_FIRECALC=2549,FIRE_FMODLIST=2550,FIRE_DWDVLOUT=2551,
    FIRE_DWDCVOUT=2552,FIRE_FUELSOFT=2553,ECON_PRETEND =2605,ECON_SEVSTART=2606,
    ECON_SPECCST =2607,ECON_SPECRVN =2608,ECON_STRTECON=2609)

  if (missing(year) & missing(activity)) return (activities)                

  if (missing(year)) stop ("year must be specified.")
  if (missing(activity)) stop ("activity must be specified.")                
  if (class(activity) == "character") 
  {
    item=grep (activity,names(activities),ignore.case=TRUE)
    if (length(item) > 1) stop(activity," is matching: ",
                          paste(names(activities)[item],collapse=", "))
    iactk = if (length(item) > 0) as.integer(activities[item]) else NA
    if (is.na(iactk)) stop(activity," could not be translated to a code.")
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
  rtn = .Fortran("fvsAddActivity",year,iactk,inprms,nprms,rtnCode,
        PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
  invisible(rtn[[5]])
}

