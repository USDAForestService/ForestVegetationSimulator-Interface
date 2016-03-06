
#load Acadian growth functions
source("AcadianV8.R")
if (file.exists("Acadian.log")) file.remove("Acadian.log")

# Note: The form of the function call is very carefully coded. Make sure
# "runOps" exists if you want them to be used.
fvsRunAcadian <- function(runOps)
{  
  sink("Acadian.log",append=TRUE)
  cat ("*** in fvsRunAcadian",date(),"\n")
  
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

  mkGraphs=FALSE
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
    incr$tree = fvsGetTreeAttrs(c("plot","species","tpa","dbh","ht","cratio",
                                  "dg","htg","mort"))                             
    names(incr$tree) = toupper(names(incr$tree))
    incr$tree$ID = 1:nrow(incr$tree)
    incr$tree$TREE= incr$tree$ID
    names(incr$tree)[match("SPECIES",names(incr$tree))] = "SP"
    names(incr$tree)[match("TPA",names(incr$tree))] = "EXPF"
    incr$tree$SP = spcodes[incr$tree$SP,1]        
    #change CR to a proportion and take abs; note that in FVS a negative CR 
    #signals that CR change has been computed by the fire or insect/disease model
    incr$tree$CR   = abs(incr$tree$CRATIO) * .01  
    incr$tree$DBH  = incr$tree$DBH  * INtoCM
    incr$tree$HT   = incr$tree$HT   * FTtoM
    incr$tree$DG   = incr$tree$DG   * INtoCM
    incr$tree$HTG  = incr$tree$HTG  * FTtoM
    incr$tree$EXPF = incr$tree$EXPF * ACRtoHA
    incr$tree$YEAR = stdInfo["year"]
                       
    cat ("fvsRunAcadian: calling AcadianGY, year=",stdInfo["year"],
         " THINMOD=",THINMOD,"\n")
         
    ### this code saves the input so AcadianGY can be run outside of FVSOnline
    #savefile=paste0("acadianInput","_",stdIds["standid"],"_",stdInfo["year"],
    #                 ".RData")
    #cat ("savefile=",savefile,"\n")
    #save(incr,CSI,INGROWTH,MinDBH,mortModel,SBW,THINMOD,file=savefile)
    ### 
    
    #compute the growth
    incr = AcadianGY(incr$tree,CSI,INGROWTH=INGROWTH,MinDBH=MinDBH, 
                     CutPoint=0,   # >0 uses threshold probability (>0-1).
                     mortModel=mortModel,SBW=SBW,THINMOD=THINMOD,verbose=TRUE) 

    cat ("fvsRunAcadian: is.null(incr$tree$dEXPF)=",is.null(incr$tree$dEXPF),"\n")
                     
    cat ("fvsRunAcadian: cyclen=",cyclen,"sum1 EXPF=",sum(incr$tree$EXPF),
         " dEXPF=",sum(incr$tree$dEXPF),"\n")
    #Scale the growth to the number of years in the period. 
    if (cyclen>1)
    {
      incr$tree$dDBH=incr$tree$dDBH*cyclen
      incr$tree$dHT =incr$tree$dHT *cyclen
      incr$tree$dHCB=incr$tree$dHCB*cyclen
      if (!is.null(incr$tree$dEXPF)) 
      {
        incr$tree$dEXPF = incr$tree$EXPF*
          (1-((1-(incr$tree$dEXPF/incr$tree$EXPF))^cyclen))
        cat ("fvsRunAcadian: sum2 of dEXPF=",sum(incr$tree$dEXPF),"\n")
      }
    }

    names(incr$tree)[match("TPA",names(incr$tree))] = "EXPF"
                     
    #plot growth and ingrowth

    if (mkGraphs)
    {
      file=paste0("SId_",fvsGetStandIDs()["standid"],"_Year_",
                   stdInfo["year"],".png")
      main=paste0("SId=",fvsGetStandIDs()["standid"],"; Year=",
                  stdInfo["year"],"; Cyclen=",cyclen)
      png (filename=file,height=6,width=6,units="in",pointsize=10,res=300)
      par(mfcol=c(3,2),mar=c(4,4,3,1))
      barplot(by(incr$tree$EXPF,FUN=base::sum,
           INDICES=list(incr$tree$PLOT,incr$tree$SP)),
           main=main,ylab="Stocking (t/ha)",xlab="Species",cex.names=.7)
      box("figure")
      barplot(by(incr$tree$dEXPF,FUN=base::sum,
           INDICES=list(incr$tree$PLOT,incr$tree$SP)),
           main=main,ylab="Mortality (t/ha)",xlab="Species",cex.names=.7)
      box("figure")
      plot(y=incr$tree$HT,x=incr$tree$DBH,xlab="DBH (cm)",ylab="Ht (m)",
           col=as.numeric(as.factor(incr$tree$SP)),main=main)
      box("figure")
      plot(y=incr$tree$dHT,x=incr$tree$HT,xlab="Ht (m)",ylab="dHt (m)",
           col=as.numeric(as.factor(incr$tree$SP)),main=main)
      box("figure")
      plot(y=incr$tree$dDBH,ylab="dDBH (cm)",xlab="DBH (cm)",
           x=incr$tree$DBH,col=as.numeric(as.factor(incr$tree$SP)),main=main)
      box("figure")
      if (is.null(incr$ingrow))
      {
        plot.new()
        text(.5,.5,"No ingrowth")
      } else 
      {
        barplot(by(incr$ingrow$EXPF,FUN=base::sum,
             INDICES=list(incr$ingrow$PLOT,incr$ingrow$SP)),
             main=paste0(main,"\nMinDBH=",MinDBH," NumPlots=",
                         max(incr$ingrow$PLOT)),
             ylab="Ingrowth (t/ha)",xlab="Species",cex.names=.7)
      }
      box("figure")
      dev.off()
    }
                
    tofvs = data.frame(id=incr$tree$ID,
            dg=incr$tree$dDBH*CMtoIN,
            htg=incr$tree$dHT*MtoFT,
            # set the crown ratio sign to negetive so that FVS 
            # doesn't change them. if already negetive, don't change them.
            cratio=ifelse(incr$tree$CRATIO < 0, incr$tree$CRATIO,
                   -round((1-((incr$tree$HCB+incr$tree$dHCB) / 
                              (incr$tree$HT +incr$tree$dHT)))*100,0)))
    if (!is.null(incr$tree$dEXPF)) tofvs$mort=incr$tree$dEXPF*HAtoACR          
    fvsSetTreeAttrs(tofvs)

    atstop6 = FALSE
    
    # adding regeneration?
    if (!is.null(incr$ingrow) && nrow(incr$ingrow)>0)
    {
      cat ("fvsRunAcadian: Computing regeneration\n")            
      toadd = data.frame(dbh    =incr$ingrow$DBH*CMtoIN,
                         species=match(incr$ingrow$SP,spcodes[,"fvs"]),
                         ht     =incr$ingrow$HT*MtoFT,
                         cratio =incr$ingrow$CR,
                         plot   =as.numeric(incr$ingrow$PLOT),
                         tpa    =incr$ingrow$EXPF*ACRtoHA)
      if (nrow(toadd) < room["maxtrees"] - room["ntrees"]) 
      {
        fvsRun(stopPointCode=6,stopPointYear=-1)
        atstop6 = TRUE
        fvsAddTrees(toadd)
      } else cat ("fvsRunAcadian: Not enough room for new trees. Stand=",
                  fvsGetStandIDs()["standid"],"; Year=",stdInfo["year"],"\n")            
    }
    
    # modifying volume?
    if (volLogic == "Kozak")
    {
      cat ("fvsRunAcadian: Applying Kozak volume logic\n")            

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
              fvsRun$uiCustomRunOps$uiAcadianMort     = "Base Model" #"Acadian"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianVolume))
              fvsRun$uiCustomRunOps$uiAcadianVolume   = "Base Model"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianTHIN))
              fvsRun$uiCustomRunOps$uiAcadianTHIN     = "Yes"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianSBW))
              fvsRun$uiCustomRunOps$uiAcadianSBW      = "No"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianSBWCDEF))
              fvsRun$uiCustomRunOps$uiAcadianSBWCDEF  = "500"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianSBW.YR))
              fvsRun$uiCustomRunOps$uiAcadianSBW.YR   = "2020"
  if (is.null(fvsRun$uiCustomRunOps$uiAcadianSBW.DUR))
              fvsRun$uiCustomRunOps$uiAcadianSBW.DUR  = "20"
  list(
    myRadioGroup("uiAcadianIngrowth", "Simulate ingrowth:", 
      c("Yes","No"),selected=fvsRun$uiCustomRunOps$uiAcadianIngrowth),
    myInlineTextInput("uiAcadianMinDBH","Minimum DBH for ingrowth", 
               fvsRun$uiCustomRunOps$uiAcadianMinDBH),
    myRadioGroup("uiAcadianMort", "Mortality model:", 
      c("Acadian","Base Model"),selected=fvsRun$uiCustomRunOps$uiAcadianMort),
    myRadioGroup("uiAcadianVolume", "Merchantable volume logic:", 
      c("Kozak","Base Model"),selected=fvsRun$uiCustomRunOps$uiAcadianVolume),
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
 
   



              
