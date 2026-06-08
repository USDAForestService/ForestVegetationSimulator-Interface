
# Note: This is very carefully coded.

unlink("Adirondack.log")

fvsRunAdirondack <- function(runOps,logfile="Adirondack.log")
{
  #load the growth model R code
  rFn="AdirondackGY.R"
  if (file.exists(rFn)) source(rFn) else
  {
    rFn = system.file("extdata", rFn, package = "fvsOL")
    if (! file.exists(rFn)) stop("can not find and load model code")
    source(rFn)
  }

  if (!is.null(logfile) && !interactive())
  {
    sink()
    sink(logfile,append=TRUE)
  }

  cat ("*** in fvsRunAdirondack",date()," AdirondackGYVersionTag=",AdirondackGYVersionTag,"\n")

  
  #load some handy conversion factors
  CMtoIN  = fvsUnitConversion("CMtoIN")
  INtoCM  = fvsUnitConversion("INtoCM")
  FTtoM   = fvsUnitConversion("FTtoM")
  MtoFT   = fvsUnitConversion("MtoFT")
  ACRtoHA = fvsUnitConversion("ACRtoHA")
  HAtoACR = fvsUnitConversion("HAtoACR")
  spcodes = fvsGetSpeciesCodes()
  stdIds  = fvsGetStandIDs()

  # process the ops.
  INGROWTH = if (is.null(runOps$uiAdirondackIngrowth)) "N" else 
    runOps$uiAdirondackIngrowth
  MinDBH   = as.numeric(if (is.null(runOps$uiAdirondackMinDBH)) "3.0" else 
    runOps$uiAdirondackMinDBH) * CMtoIN
  mortModel= if (is.null(runOps$uiAdirondackMort)) "Adirondack" else 
    runOps$uiAdirondackMort 
  volLogic = if (is.null(runOps$uiAdirondackVolume)) "Base Model" else 
    runOps$uiAdirondackVolume
  CutPoint = if (is.null(runOps$uiAdirondackCutPoint)) 0 else 
    as.numeric(runOps$uiAdirondackCutPoint)
  
  cat ("fvsRunAdirondack: INGROWTH=",INGROWTH," MinDBH=",MinDBH," mortModel=")            
  
  
  
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
  
    # if there are no trees, this code does not work.
    # NB: room is used below, so if this rule changes, move this code
    room = fvsGetDims()
    if (room['ntrees'] == 0) next
    num.plots = as.numeric(room['nplots'])

    #fetch some stand level information
    stdInfo = fvsGetEventMonitorVariables(c("site","year","cendyear"))
    cyclen = stdInfo["cendyear"] - stdInfo["year"] + 1
    attributes(cyclen) = NULL
    CSI = stdInfo["site"] * FTtoM

    tree = make_adk_tree(
      fvsGetTreeAttrs(c('plot', 'species', 'tpa', 'dbh', 'ht', 'cratio', 'dg', 'htg', 'mort')),
      num.plots, spcodes)

    cat ("fvsRunAdirondack: calling AdirondackGY, year=",stdInfo["year"],"\n") 
                       
    stand = list(CSI=CSI)
    ops   = list(verbose=TRUE,cyclen=cyclen,INGROWTH=INGROWTH,
                 MinDBH=MinDBH,CutPoint=CutPoint,   # >0 uses threshold probability (>0-1).
                 mortModel=mortModel)
    #compute the growth
    #save(tree,stand,ops,file="test.RData")
    result = AdirondackGYOneStand(tree,stand,ops)
    cat ("return from AdirondackGY, nrow(result)=",nrow(result),"\n")

    incr.tree = result[!is.na(result$id), ]
    incr.ingrow = result[is.na(result$id), ]

    tofvs = make_fvs_tree(incr.tree, num.plots)
    fvsSetTreeAttrs(tofvs)

    atstop6 = FALSE

    # adding regeneration?
    if (nrow(incr.ingrow) > 0)
    {
      toadd = make_fvs_regen(incr.ingrow, num.plots, spcodes)

      if (nrow(toadd) < room["maxtrees"] - room["ntrees"]) 
      {
        fvsRun(stopPointCode=6,stopPointYear=-1)
        atstop6 = TRUE
        fvsAddTrees(toadd)
      } else cat ("fvsRunAdirondack: Not enough room for new trees. Stand=",
                  fvsGetStandIDs()["standid"],"; Year=",stdInfo["year"],"\n")
    }  

    # modifying volume?
    if (volLogic == "Kozak")
    {
      cat ("fvsRunAdirondack: Applying Kozak volume logic\n")            

      mcstds = fvsGetSpeciesAttrs(vars=c("mcmind","mctopd","mcstmp"))
      vols = fvsGetTreeAttrs(c("species","ht","dbh","mcuft","defect"))                             
      vols$mcuft = ifelse (vols$dbh >= mcstds$mcmind[vols$species],
        mapply(KozakTreeVol,Bark="ob",Planted=0,
               DBH  = vols$dbh  * INtoCM,
               HT   = vols$ht   * FTtoM,
               SPP  = spcodes[vols$species,1],
               stump= mcstds$mcstmp[vols$species] * FTtoM,
               topD = mcstds$mctopd[vols$species] * INtoCM), 0)
                          
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

uiAdirondack <- function(fvsRun)
{
cat ("in uiAdirondack uiAdirondackVolume=",
  if (is.null(fvsRun$uiCustomRunOps$uiAdirondackVolume)) "NULL" else 
              fvsRun$uiCustomRunOps$uiAdirondackVolume,"\n")
  if (is.null(fvsRun$uiCustomRunOps$uiAdirondackIngrowth))
              fvsRun$uiCustomRunOps$uiAdirondackIngrowth = "No"
  if (is.null(fvsRun$uiCustomRunOps$uiAdirondackMinDBH))
              fvsRun$uiCustomRunOps$uiAdirondackMinDBH   = "3.0"
  if (is.null(fvsRun$uiCustomRunOps$uiAdirondackMort))
              fvsRun$uiCustomRunOps$uiAdirondackMort     = "Adirondack"
  if (is.null(fvsRun$uiCustomRunOps$uiAdirondackVolume))
              fvsRun$uiCustomRunOps$uiAdirondackVolume   = "Base Model"
  if (is.null(fvsRun$uiCustomRunOps$uiAdirondackCutPoint))
              fvsRun$uiCustomRunOps$uiAdirondackCutPoint   = "0.0"
  list(
    radioButtons("uiAdirondackIngrowth", "Simulate ingrowth:", 
      c("Yes","No"),inline=TRUE,
      selected=fvsRun$uiCustomRunOps$uiAdirondackIngrowth),
    myInlineTextInput("uiAdirondackMinDBH","Minimum DBH for ingrowth", 
               fvsRun$uiCustomRunOps$uiAdirondackMinDBH),
    radioButtons("uiAdirondackMort", "Mortality model:", 
      c("Adirondack","Base Model"),inline=TRUE,
      selected=fvsRun$uiCustomRunOps$uiAdirondackMort),
    radioButtons("uiAdirondackVolume", "Merchantable volume logic:", 
      c("Kozak","Base Model"),inline=TRUE,
      selected=fvsRun$uiCustomRunOps$uiAdirondackVolume),
    myInlineTextInput("uiAdirondackCutPoint","CutPoint", 
               fvsRun$uiCustomRunOps$uiAdirondackCutPoint)
  )
}
 
                      



              
