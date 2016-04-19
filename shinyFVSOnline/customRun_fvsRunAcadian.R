
#load Acadian growth functions
source("AcadianGY.R")
if (file.exists("Acadian.log")) file.remove("Acadian.log")

# Note: The form of the function call is very carefully coded. Make sure
# "runOps" exists if you want them to be used.
fvsRunAcadian <- function(runOps,logfile="Acadian.log")
{ 
  if (!is.null(logfile)) sink(logfile,append=TRUE)
  cat ("*** in fvsRunAcadian",date()," AcadianVersionTag=",AcadianVersionTag,"\n")
  
  # process the ops.
  INGROWTH = if (is.null(runOps$uiAcadianIngrowth)) "N" else 
               runOps$uiAcadianIngrowth
  MinDBH   = as.numeric(if (is.null(runOps$uiAcadianMinDBH)) "3.0" else 
               runOps$uiAcadianMinDBH)
  mortModel= if (is.null(runOps$uiAcadianMort)) "Acadian" else 
               runOps$uiAcadianMort
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

  CutPoint=0
  
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
    if (stopPoint == 100) break

    cat ("fvsRunAcadian: INGROWTH=",INGROWTH," MinDBH=",MinDBH," mortModel=",
      mortModel,"\n    volLogic=",volLogic," SBW=",SBW,"\n")            
      
    # if there are no trees, this code does not work.
    # NB: room is used below, so if this rule changes, move this code
    room=fvsGetDims()
    if (room["ntrees"] == 0) next

    #fetch some stand level information
    stdInfo = fvsGetEventMonitorVariables(c("site","year","cendyear"))
    stdIds  = fvsGetStandIDs()
    cyclen = stdInfo["cendyear"] - stdInfo["year"] + 1
    attributes(cyclen) = NULL
    CSIin = stdInfo["site"] * FTtoM
    CSI   = approxfun(c(0,8,14,20),c(0,8,12,14),rule=2)(CSIin)
    cat ("fvsRunAcadian: CSIin=",CSIin," CSI (used)=",CSI,"\n")
    
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
    orgtree = fvsGetTreeAttrs(c("plot","species","tpa","dbh","ht","cratio"))                             
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
                       
    stand = list(CSI=CSI)
    ops   = list(INGROWTH=INGROWTH,MinDBH=MinDBH, 
                 CutPoint=0.5,   # >0 uses threshold probability (>0-1).
                 mortType="continuous", #mortType="discrete", 
                 SBW=SBW,THINMOD=THINMOD,verbose=TRUE,
                 rtnVars = c("PLOT","SP","DBH","EXPF","TREE","HT","HCB"))
    
    tree=orgtree 
    for (year in stdInfo["year"]:stdInfo["cendyear"])
    {
      tree$YEAR = year
      cat ("fvsRunAcadian: calling AcadianGY, year=",year,"\n")
      tree = AcadianGYOneStand(tree,stand=stand,ops=ops)
    }
    # put the PLOT variable back to a character string (defactor it).
    if (is.factor(tree$PLOT)) tree$PLOT = levels(tree$PLOT)[as.numeric(tree$PLOT)]
    # restore the order of the trees
    tree = tree[order(tree$TREE),]

    ### this code saves the input so AcadianGY can be run outside of FVSOnline
    #savefile=paste0("acadianInput","_",stdIds["standid"],"_",stdInfo["year"],
    #                 ".RData")
    #cat ("savefile=",savefile,"\n")
    #save(orgtree,incr,stand,ops,file=savefile)
    ### 

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
  rtn
}

#NOTE: I tried various ways of building these elements. Setting the initial
#value to the saved value when the elements are created seems to work well. 
#What did not work was setting the initial value to some default and then 
#changing it using an update call in the server code.  

uiAcadian <- function(fvsRun)
{  
  source("mkInputElements.R",local=TRUE)  #for myInlineTextInput
cat ("in uiAcadian uiAcadianVolume=",
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianVolume)) "NULL" else 
              fvsRun$uiCustomRunOps$uiAcadianVolume,"\n")
            
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianIngrowth))
              fvsRun$uiCustomRunOps$uiAcadianIngrowth = "No"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianMinDBH))
              fvsRun$uiCustomRunOps$uiAcadianMinDBH   = "3.0"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianMort))
              fvsRun$uiCustomRunOps$uiAcadianMort     = "Acadian"
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
 
   



              
