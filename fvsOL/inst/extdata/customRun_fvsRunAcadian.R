# $Id: customRun_fvsRunAcadian.R 4019 2022-08-31 20:33:55Z nickcrookston $

unlink("Acadian.log")

# Note: The form of the function call is very carefully coded. Make sure
# "runOps" exists if you want them to be used.
fvsRunAcadian <- function(runOps,logfile="Acadian.log")
{

  if (!is.null(logfile) && !interactive())
  {
    sink()
    sink(logfile,append=TRUE)
  }
  devVersion <<- "fvsOLdev" %in% (.packages())
  
  #load the growth model R code
  rFn="AcadianGY.R"
  if (file.exists(rFn)) source(rFn) else
  {
    rFn = system.file("extdata", rFn, package=if (devVersion) "fvsOLdev" else "fvsOL")
    if (! file.exists(rFn)) stop("can not find and load model code")
    source(rFn)
  }
  cat ("\nSource file for this fvsRunAcadian=\n",rFn,"\n")
  cat ("*** in fvsRunAcadian",date()," AcadianVersionTag=",AcadianVersionTag,"\n")
  cat ("\nrunOps=\n")
  print (runOps)

  # process the ops.
  INGROWTH = if (is.null(runOps$uiAcadianIngrowth)) "N" else
               runOps$uiAcadianIngrowth
  MinDBH   = as.numeric(if (is.null(runOps$uiAcadianMinDBH)) "3.0" else
               runOps$uiAcadianMinDBH)
  mortModel= if (is.null(runOps$uiAcadianMort)) "Acadian" else
               runOps$uiAcadianMort
  CutPoint = if (is.null(runOps$uiAcadianCutPoint)) 0.95 else
               as.numeric(runOps$uiAcadianCutPoin)
  volLogic = if (is.null(runOps$uiAcadianVolume)) "Base Model" else
               runOps$uiAcadianVolume
  wThinMod = if (is.null(runOps$uiAcadianTHIN)) FALSE else
               runOps$uiAcadianTHIN == "Yes"
  CDEF     = if (is.null(runOps$uiAcadianSBWCDEF)) NA else
               as.numeric(runOps$uiAcadianSBWCDEF)
  SBW.YR   = if (is.null(runOps$uiAcadianSBW.YR)) NA else
               as.numeric(runOps$uiAcadianSBW.YR)
  SBW.DUR  = if (is.null(runOps$uiAcadianSBW.DUR)) NA else
               as.numeric(runOps$uiAcadianSBW.DUR)
  SBW      = if (is.null(runOps$uiAcadianSBW)) NULL else
               if (runOps$uiAcadianSBW == "No") NULL else
                 c(CDEF=CDEF,SBW.YR=SBW.YR,SBW.DUR=SBW.DUR)
  if (!is.null(SBW) && any(is.na(SBW))) SBW=NULL

  cat ("fvsRunAcadian, options set\n")

  #load some handy conversion factors
  CMtoIN  = fvsUnitConversion("CMtoIN")
  INtoCM  = fvsUnitConversion("INtoCM")
  FTtoM   = fvsUnitConversion("FTtoM")
  MtoFT   = fvsUnitConversion("MtoFT")
  M3toFT3 = fvsUnitConversion("M3toFT3")
  ACRtoHA = fvsUnitConversion("ACRtoHA")
  HAtoACR = fvsUnitConversion("HAtoACR")
  spcodes = fvsGetSpeciesCodes()

  #initialize THINMOD
  THINMOD = NULL

  incr    = list()
  # define the acadian height function
  calc_acd_ht=function(tree=orgtree){
    tree=tree %>%
        dplyr::rowwise() %>%
        dplyr::mutate(mcw=mcw(sp=SP, dbh=DBH), # Max crown width
                      MCA=100*((pi*(mcw/2)^2)/10000)*EXPF) %>%
        dplyr::group_by(PLOT) %>%
        dplyr::mutate(CCF=sum(MCA)) %>%  # Plot crown competition factor
        dplyr::ungroup() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(pHT= HTPred(SPP=SP, DBH=DBH, CSI=CSI, CCF=CCF, BAL=BAL), # Predicted height
                      HT= case_when(HT == 0 | HT>100 ~pHT, # Use predicted height where value is missing or in excess of 100
                                    TRUE ~ HT),
                      HCB= HCBPred(SPP=SP, DBH=DBH, HT=pHT,CCF=CCF, BAL=BAL)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(pCR= (HT-HCB)/HT, # predicted crown ratio
                      CR= case_when(CR == 0 ~pCR, # use predicted crown ratio where value is missing
                                    TRUE ~ CR))
  tree
  }

  # start FVS but return prior to dubbing and calibration to dub in missing
  # heights and crown ratios

  fvsRun(7,0)
  CSI = fvsGetEventMonitorVariables("csi")
  if (is.na(CSI)) CSI = fvsGetEventMonitorVariables("site")*FTtoM
  CSI   = approxfun(c(0,8,14,20),c(0,8,12,14),rule=2)(CSI)
  orgtree = fvsGetTreeAttrs(c("plot","species","tpa","dbh","ht","cratio"))
  names(orgtree) = toupper(names(orgtree))
  orgtree$TREE= 1:nrow(orgtree)
  names(orgtree)[match("SPECIES",names(orgtree))] = "SP"
  names(orgtree)[match("TPA",names(orgtree))] = "EXPF"
  orgtree$SP = spcodes[orgtree$SP,1]
  #change CR to a proportion and take abs; note that in FVS a negative CR
  #signals that CR change has been computed by the fire or insect/disease model
  orgtree$CR   = abs(orgtree$CRATIO) * .01
  orgtree$ba   = orgtree$DBH  * orgtree$DBH * 0.005454 * orgtree$EXPF * fvsUnitConversion("FT2pACRtoM2pHA")
  orgtree$DBH  = orgtree$DBH  * INtoCM
  orgtree$HT   = orgtree$HT   * FTtoM
  orgtree$EXPF = orgtree$EXPF * HAtoACR
  orgtree = dplyr::arrange(orgtree, PLOT, desc(DBH))
  temp = unlist(by(orgtree$ba,INDICES=orgtree$PLOT,FUN=cumsum))
  orgtree$BAL = temp-orgtree$ba
  orgtree = dplyr::arrange(orgtree, TREE)
  newtree = calc_acd_ht(tree=orgtree)
  fvsSetTreeAttrs(list(ht    =as.numeric(newtree$HT*MtoFT),
                       cratio=round(as.numeric(newtree$CR)*100,2)))

  cat ("Starting repeat loop\n")

  repeat
  {
    #stopPointCode 5 (after growth and mortality, before it is added)
    #stopPointCode 6 (just before estab, place to add new trees)

    #BE CAREFULL: the next few lines control when to exit the loop and
    #the details are very important. It is easy to break this code!
    rtn = fvsRun(stopPointCode=5,stopPointYear=-1)
    if (rtn != 0) break
    stopPoint <- fvsGetRestartcode()
    # end of current stand?
    cat ("first stopPoint code=",stopPoint,"\n")
    if (stopPoint == 100) break

    cat ("fvsRunAcadian: INGROWTH=",INGROWTH," MinDBH=",MinDBH," mortModel=",
      mortModel,"\n    volLogic=",volLogic," SBW=",SBW,"\n")

    # if there are no trees, this code does not work.
    # NB: room is used below, so if this rule changes, move this code
    room=fvsGetDims()
    if (room["ntrees"] == 0) next

    #fetch some stand level information
    stdInfo = fvsGetEventMonitorVariables(c("site","year","cendyear","elev"))
    stdIds  = fvsGetStandIDs()
    cyclen = stdInfo["cendyear"] - stdInfo["year"] + 1
    attributes(cyclen) = NULL
    CSI = fvsGetEventMonitorVariables("csi")
    if (is.na(CSI)) CSI = fvsGetEventMonitorVariables("site")*FTtoM
    CSI   = approxfun(c(0,8,14,20),c(0,8,12,14),rule=2)(CSI)
    ELEV  = as.numeric(stdInfo["elev"]) * FTtoM
    cat ("fvsRunAcadian: CSI=",CSI," ELEV=",ELEV,"\n")

    #set/reset THINMOD based on pre and post event monitor variables
    if (wThinMod)
    {
      thinning = fvsGetEventMonitorVariables(c("bba","aba","badbh","aadbh","rtpa"))
      if (thinning["rtpa"] > 0)
      {
        THINMOD = c(stdInfo["year"],
                    (1-(thinning["aba"]/thinning["bba"]))*100.,
                    thinning["bba"]*fvsUnitConversion("FT2pACRtoM2pHA"),
                    if (thinning["aadbh"]>=1)
                        thinning["badbh"]/thinning["aadbh"] else NA)
        names(THINMOD) = c("YEAR_CT","pBArm","BApre","QMDratio")
      } else if (!is.null(THINMOD) &&
                 stdInfo["year"]-THINMOD["YEAR_CT"] > 20) THINMOD=NULL
    }

    #fetch the fvs trees and form the AcadianGY "tree" dataframe
    orgtree = fvsGetTreeAttrs(c("plot","species","tpa","dbh","ht","cratio","special"))
    names(orgtree) = toupper(names(orgtree))
    orgtree$TREE= 1:nrow(orgtree)
    names(orgtree)[match("SPECIES",names(orgtree))] = "SP"
    names(orgtree)[match("TPA",names(orgtree))] = "EXPF"
    orgtree$SP = spcodes[orgtree$SP,1]
    #change CR to a proportion and take abs; note that in FVS a negative CR
    #signals that CR change has been computed by the fire or insect/disease model
    orgtree$CR   = abs(orgtree$CRATIO) * .01
    orgtree$DBH  = orgtree$DBH  * INtoCM
    orgtree$HT   = orgtree$HT   * FTtoM
    orgtree$EXPF = orgtree$EXPF * HAtoACR
  
    #load the form and risk class data using FVS variable ISPECL loaded using "special"
    
    orgtree$Form = rep(" ",nrow(orgtree))
    orgtree$Risk = rep(" ",nrow(orgtree))
    tmpset = orgtree$SPECIAL > 0 & orgtree$SPECIAL < 85
    orgtree$Form[tmpset] = paste0("F",as.integer(orgtree$SPECIAL[tmpset] %/% 10))
    orgtree$Risk[tmpset] = paste0("R",as.integer(orgtree$SPECIAL[tmpset] %%  10))

    stand = list(CSI=CSI,ELEV=ELEV)
    ops   = list(verbose=TRUE,INGROWTH=INGROWTH,MinDBH=MinDBH,
                 CutPoint=0.5,   # >0 uses threshold probability (>0-1).
                 mortType="continuous", #mortType="discrete",
                 SBW=SBW,THINMOD=THINMOD,verbose=TRUE,
                 rtnVars = c("PLOT","SP","DBH","EXPF","TREE","HT","HCB","Form","Risk"))

    tree=orgtree

    for (year in stdInfo["year"]:stdInfo["cendyear"])
    {
      tree$YEAR = year
      cat ("fvsRunAcadian: calling AcadianGY, year=",year,"\n")
      treeout = try(AcadianGYOneStand(tree,stand=stand,ops=ops))
      if (class(treeout)=="try-error" || any(is.na(treeout$DBH)) || 
               any(is.na(treeout$HT)) || any(is.na(treeout$EXPF)))
      {
        cat("AcadianGYOneStand failed in year=",year,"\n")
        dmpFile=file.path(getwd(),paste0("AcadianGYOneStand.Failure.",year,".RData"))
        if (class(treeout)!="try-error") treeout="critical result contains NA values"
        cat ("dmpFile name=",dmpFile,"\n")
        save(file=dmpFile,treeout,tree,stand,ops)
        tree=NULL
        break
      }
      tree=treeout
    }
    # if there was a failure, tree will be NULL, go on to the next stand cycle
    if (is.null(tree)) next
    # put the PLOT variable back to a character string (defactor it).
    if (is.factor(tree$PLOT)) tree$PLOT = levels(tree$PLOT)[as.numeric(tree$PLOT)]
    # restore the order of the trees
    tree = tree[order(tree$TREE),]

    cat ("fvsRunAcadian: is.null(tree$dEXPF)=",is.null(tree$dEXPF),"\n")
    cat ("fvsRunAcadian: cyclen=",cyclen,"sum1 EXPF=",sum(tree$EXPF),
         " sum dEXPF=",if (is.null(tree$dEXPF)) NA else sum(tree$dEXPF),"\n")

    names(tree)[match("TPA",names(tree))] = "EXPF"

    tree$CR = round((1-(tree$HCB/tree$HT))*100,1)
    tofvs = data.frame(id=orgtree$TREE,
              dg=(tree$DBH[orgtree$TREE]-orgtree$DBH)*CMtoIN,
              htg=(tree$HT[orgtree$TREE]-orgtree$HT)*MtoFT,
              # set the crown ratio sign to negetive so that FVS
              # doesn't change them. if already negetive, don't change them.
              cratio=ifelse(orgtree$CRATIO < 0, orgtree$CRATIO,
                            -tree$CR[orgtree$TREE]))
              special=as.numeric(substr(tree$Form[orgtree$TREE],2,2))*10+
                      as.numeric(substr(tree$Risk[orgtree$TREE],2,2))

    if (mortModel == "Acadian") tofvs$mort=(orgtree$EXPF-
      tree$EXPF[orgtree$TREE])*ACRtoHA
    fvsSetTreeAttrs(tofvs)

    atstop6 = FALSE

    # adding regeneration?
    newTrees = nrow(tree) - nrow(orgtree)
    cat ("fvsRunAcadian: num newtrees=",newTrees,"\n")
    if (newTrees)
    {
      if (newTrees < room["maxtrees"] - room["ntrees"])
      {
        newTrees = (nrow(orgtree)+1):nrow(tree)
        toadd = data.frame(dbh    =tree$DBH[newTrees]*CMtoIN,
                           species=match(tree$SP[newTrees],spcodes[,"fvs"]),
                           ht     =tree$HT[newTrees]*MtoFT,
                           cratio =-tree$CR[newTrees],
                           plot   =as.numeric(tree$PLOT[newTrees]),
                           tpa    =tree$EXPF[newTrees]*ACRtoHA)
        fvsRun(stopPointCode=6,stopPointYear=-1)
        atstop6 = TRUE
        fvsAddTrees(toadd)
      } else cat ("fvsRunAcadian: Not enough room for",newTrees,
          "new trees. Stand=",fvsGetStandIDs()["standid"],"; Year=",
          stdInfo["year"],"\n")
    }

    # modifying volume?
    if (volLogic == "Acadian")
    {
      cat ("fvsRunAcadian: Applying Acadian volume logic\n")

      mcstds = fvsGetSpeciesAttrs(vars=c("mcmind","mctopd","mcstmp"))
      vols = fvsGetTreeAttrs(c("species","ht","dbh","mcuft","defect"))
      vols$mcuft = ifelse (vols$dbh >= mcstds$mcmind[vols$species],
        mapply(KozakTreeVol,Bark="ob",Planted=0,
               DBH=vols$dbh  * INtoCM,
               HT =vols$ht   * FTtoM,
               SPP=spcodes[vols$species,1],
               stump=mcstds$mcstmp[vols$species] * FTtoM,
               topD =mcstds$mctopd[vols$species] * INtoCM), 0)

      if (any(vols$defect != 0)) vols$mcuft = vols$mcuft *
                                 (1-(((vols$defect %% 10000) %/% 100) * .01))
      vols$mcuft  = vols$mcuft * M3toFT3
      vols$species=NULL
      vols$ht     =NULL
      vols$dbh    =NULL
      vols$defect =NULL
      if (!atstop6)
      {
        fvsRun(stopPointCode=6,stopPointYear=-1)
        atstop6 = TRUE
      }
      fvsSetTreeAttrs(vols)
    }
  }
  cat ("rtn=",rtn,"\n")
  rtn
}

# NOTE: I (NLCrookston) tried various ways of building these elements. Setting the 
# initial value to the saved value when the elements are created seems to work well.
# What did not work was setting the initial value to some default and then
# changing it using an update call in the server code.

uiAcadian <- function(fvsRun)
{
cat ("in uiAcadian uiAcadianVolume=",
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianVolume)) "NULL" else
              fvsRun$uiCustomRunOps$uiAcadianVolume,"\n")

  if (is.null(fvsRun$uiCustomRunOps$uiAcadianIngrowth))
              fvsRun$uiCustomRunOps$uiAcadianIngrowth = "No"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianMinDBH))
              fvsRun$uiCustomRunOps$uiAcadianMinDBH   = "3.0"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianMort))
              fvsRun$uiCustomRunOps$uiAcadianMort     = "Acadian"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianCutPoint))
              fvsRun$uiCustomRunOps$uiAcadianCutPoint  = "0.95"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianVolume))
              fvsRun$uiCustomRunOps$uiAcadianVolume   = "Acadian"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianTHIN))
              fvsRun$uiCustomRunOps$uiAcadianTHIN     = "Yes"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianSBW))
              fvsRun$uiCustomRunOps$uiAcadianSBW      = "No"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianSBWCDEF))
              fvsRun$uiCustomRunOps$uiAcadianSBWCDEF  = "100"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianSBW.YR))
              fvsRun$uiCustomRunOps$uiAcadianSBW.YR   = "2020"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianSBW.DUR))
              fvsRun$uiCustomRunOps$uiAcadianSBW.DUR  = "10"
  list(
    myRadioGroup("uiAcadianIngrowth", "Simulate ingrowth:",
      c("Yes","No"),selected=fvsRun$uiCustomRunOps$uiAcadianIngrowth),
    myInlineTextInput("uiAcadianMinDBH","Minimum DBH for ingrowth",
               fvsRun$uiCustomRunOps$uiAcadianMinDBH),
    myRadioGroup("uiAcadianMort", "Mortality model:",
      c("Acadian","Base Model"),selected=fvsRun$uiCustomRunOps$uiAcadianMort),
    myInlineTextInput("uiAcadianCutPoint","CutPoint",
               fvsRun$uiCustomRunOps$uiAcadianCutPoint),
    myRadioGroup("uiAcadianVolume", "Merchantable volume logic:",
      c("Acadian","Base Model"),selected=fvsRun$uiCustomRunOps$uiAcadianVolume),
    myRadioGroup("uiAcadianTHIN", "Run with thinning modifiers:",
      c("Yes","No"),selected=fvsRun$uiCustomRunOps$uiAcadianTHIN),
    myRadioGroup("uiAcadianSBW", "Run with Spruce Budworm modifiers:",
       c("Yes","No"),selected=fvsRun$uiCustomRunOps$uiAcadianSBW),
    myInlineTextInput("uiAcadianSBWCDEF","Cumulative defoliation:",
               fvsRun$uiCustomRunOps$uiAcadianSBWCDEF),
    myInlineTextInput("uiAcadianSBW.YR","Defoliation start year:",
               fvsRun$uiCustomRunOps$uiAcadianSBW.YR),
    myInlineTextInput("uiAcadianSBW.DUR","Defoliation duration (years):",
               fvsRun$uiCustomRunOps$uiAcadianSBW.DUR)
  )
}
