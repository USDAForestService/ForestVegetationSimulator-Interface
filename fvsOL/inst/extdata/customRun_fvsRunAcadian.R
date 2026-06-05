
unlink("Acadian.log")

# Note: The form of the function call is very carefully coded. Make sure
# "runOps" exists if you want them to be used.
fvsRunAcadian <- function(runOps=NULL, logfile='Acadian.log', autoload.model=TRUE)
{

  if (!is.null(logfile) && !interactive())
  {
    sink()
    sink(logfile,append=TRUE)
  }
  
  #load the growth model R code if autoload.model=TRUE
  if(autoload.model){
  
  rFn="AcadianGY.R"
    if (file.exists(rFn)) {
      source(rFn) 
     
    }else{
      rFn = system.file("extdata", rFn, package="fvsOL")
      if (!file.exists(rFn)){
        cat("fvsRunAcadian: ERROR - cannot find and load model code; rFn=", rFn, "\n")
        stop("can not find and load model code")
      }
      
      source(rFn)
    }
  }
  cat ("\nSource file for this fvsRunAcadian=\n",rFn,"\n")
  cat ("*** in fvsRunAcadian",date()," AcadianVersionTag=",AcadianVersionTag,"\n")
 

 
  # Convert Shiny app selections using make_ops()
  if(!is.null(runOps)){
 
    .GlobalEnv$ops = make_ops(verbose = FALSE,
                   use.ingrowth = runOps$uiAcadianIngrowth,        # Yes converts to TRUE
                   ingrowth.min.dbh = as.numeric(runOps$uiAcadianMinDBH) * 2.54,  # assumes UI entry is inches
                   ingrowth.thrshld = as.numeric(runOps$uiAcadianCutPoint),
                   use.thin.mod = runOps$uiAcadianTHIN,            # Yes converts to TRUE
                   use.sbw.mod = runOps$uiAcadianSBW,              # Yes converts to TRUE
                   sbw.cdef = runOps$uiAcadianSBWCDEF,
                   sbw.yr = runOps$uiAcadianSBW.YR,
                   sbw.dur = runOps$uiAcadianSBW.DUR,
                   use.vol = runOps$uiAcadianVolume == 'Acadian')
  
    }else{
    # make ops will use all defaults
     
      .GlobalEnv$ops = make_ops()

   }
  
  if(!exists('ops')){
    cat("\n=== !!!!!!!!! ops object failed !!!!!!!!! ===\n")
  }else{
    if(ops$verbose){
    cat("\n=== ops object created successfully ===\n")
      cat("Structure of ops:\n")
      print(str(ops))
    }
  }
  
  
  #load some handy conversion factors
  .GlobalEnv$CMtoIN  = fvsUnitConversion("CMtoIN")
  .GlobalEnv$INtoCM  = fvsUnitConversion("INtoCM")
  .GlobalEnv$FTtoM   = fvsUnitConversion("FTtoM")
  .GlobalEnv$MtoFT   = fvsUnitConversion("MtoFT")
  .GlobalEnv$M3toFT3 = fvsUnitConversion("M3toFT3")
  .GlobalEnv$ACRtoHA = fvsUnitConversion("ACRtoHA")
  .GlobalEnv$HAtoACR = fvsUnitConversion("HAtoACR")
  .GlobalEnv$spcodes = fvsGetSpeciesCodes()

 
#### Impute height and crown ratio ####  
  # start FVS and return prior to dubbing and calibration to detect which trees
  # have missing heights and crown ratios.
  
  # start FVS prior to dubbing to identify trees with missing heights and crown ratios
  fvsRun(7,0)
  
  tree.b4dub = fvsGetTreeAttrs(c("ht","cratio"))
  
 # run fvs up to stop point 1 and change the dubbed values if there are any
  fvsRun(1,-1)
  
  # fetch CSI from the Event Monitor
  csi = fvsGetEventMonitorVariables("csi")
  if (is.na(csi)) {
    csi = fvsGetEventMonitorVariables("site") * FTtoM
    csi = approxfun(c(0,8,14,20), c(0,8,12,14), rule=2)(csi)
  }
  
  dubHtRows = tree.b4dub$ht == 0
  dubCrRows = tree.b4dub$cratio == 0
  
  # replace FVS dubbing with Acadian predictions if any heights or crown ratios were dubbed
  if (any(dubHtRows) || any(dubCrRows))
  {
    room = fvsGetDims()
    
    # fetch tree attributes after FVS dubbing step (TPA now populated)
    tree.aftrdub = fvsGetTreeAttrs(c('id', 'plot', 'species', 'tpa', 'dbh', 'ht', 'mgmtcd', 'special', 'cratio'))
    
    # add FVS alpha species codes and identify records outside scope of the model
    tree.aftrdub = validate_acd_tree_spp(tree.aftrdub, spcodes=spcodes)
    
    # create ACD species calibration dataframe
    calib.fvs = make_fvs_calib(spcodes=spcodes,
                               tree.size.cap=tree.size.cap)
    
    # create ACD tree list and remove invalid tree records
    tree.aftrdub = make_acd_tree(tree.aftrdub,
                                 num.plots=as.numeric(room['nplots']),
                                 calib.fvs)
    
    tree = validate_acd_tree_status(tree.aftrdub)
    
    # join species functional attributes, compute basal area and BAL
    tree = tree %>%
      dplyr::left_join(sp.attr, by=c('sp'='species')) %>%
      dplyr::mutate(ba=(dbh^2*0.00007854)*expf) %>%
      calc_bal()
    
    # calculate plot summary
    plot.smry = tree %>% 
      calc_plot_summary()
    
    # calculate tree-level crown metrics (mcw, lcw, mca, ccfl)
    tree = tree %>%
      calc_crown()
    
    # calculate plot CCF
    plot.ccf = tree %>%
      dplyr::group_by(plot) %>%
      dplyr::summarise(ccf=sum(mca, na.rm=TRUE),
                       .groups='drop')
    
    # add CCF to plot summary for use in height and crown ratio predictions
    plot.smry = plot.smry %>%
      dplyr::left_join(plot.ccf, by="plot")
    
    # predict height and height to crown base
    tree = tree %>%
      calc_ht(plot.data=plot.smry, csi=csi) %>%
      calc_hcb(plot.data=plot.smry) %>%
      dplyr::mutate(pcr = round((1 - phcb / pht) * 100, 1))
    
    # write Acadian-predicted values back to FVS for dubbed trees
   
    dub.ht.idx = which(dubHtRows)
    dub.cr.idx = which(dubCrRows)
    
    # imputed height
    tree.aftrdub=tree.aftrdub %>% 
      mutate(ht=ht*3.28084) # height to FVS: feet
    
    if (any(dubHtRows)) {
      ht.rows = tree %>%
        dplyr::filter(tree %in% dub.ht.idx,
                      dbh>2.5) # only records > 2.5cm DBH
      
      tree.aftrdub$ht[ht.rows$tree] = round(ht.rows$pht * MtoFT, 1)
    }
   
    # imputed crown ratio
    tree.aftrdub=tree.aftrdub %>% 
      mutate(cratio=cr*100) # crown ratio to FVS: percent
    
    if (any(dubCrRows)) {
      cr_rows = tree %>% 
        dplyr::filter(tree %in% dub.cr.idx)
      
      tree.aftrdub$cratio[cr_rows$tree] = cr_rows$pcr * -1
    }
    fvsSetTreeAttrs(tree.aftrdub[, c("ht","cratio")])
  }
 

  # call FVS fvsRun() until the end of the projection or an error occurs
  repeat
  {
    #stopPointCode 5 (after growth and mortality, before it is added)
    #stopPointCode 6 (just before estab, place to add new trees)

    #BE CAREFUL: the next few lines control when to exit the loop and
    #the details are very important. It is easy to break this code!
    rtn = fvsRun(stopPointCode=5,stopPointYear=-1)
    if (rtn != 0) break
    stopPoint = fvsGetRestartcode()
    # end of current stand?
    cat ("first stopPoint code=",stopPoint,"\n")
    if (stopPoint == 100) break
    
#### ACD input ####
    # if there are no trees, this code does not work.
    # NB: room is used below, so if this rule changes, move this code
    room = fvsGetDims()
    if (room["ntrees"] == 0) next

    #fetch stand level information
    stdInfo = fvsGetEventMonitorVariables(c("site","year","cendyear","elev"))
    std.id  = fvsGetStandIDs()
    
    # CSI
    csi = fvsGetEventMonitorVariables("csi")
    if (is.na(csi)) {
            csi = fvsGetEventMonitorVariables("site")*FTtoM
            csi   = approxfun(c(0,8,14,20),c(0,8,12,14),rule=2)(csi)
            }
    elev  = as.numeric(stdInfo["elev"]) * FTtoM
    cat ("fvsRunAcadian: CSI=",csi," ELEV=", elev,"m \n")

    # use the make_stand() function to create a stand dataframe- region defaults to ME for northern New England
    .GlobalEnv$stand=make_stand(stand.id=std.id[['standid']],
                                csi = csi,
                                elev = elev, 
                                region = 'ME')
    
     if(!exists('stand')){
       cat("\n=== !!!!!!!!! stand object failed !!!!!!!!! ===\n")
     }else{
       if(ops$verbose){
         cat("\n=== stand object created successfully ===\n")
         cat("Structure of stand:\n")
         print(str(stand))
     }
  }
     
     
    #set/reset THINMOD based on pre and post event monitor variables
    if (ops$use.thin.mod == TRUE)
    {
      thinning = fvsGetEventMonitorVariables(c("bba","aba","badbh","aadbh","rtpa"))
      if (thinning["rtpa"] > 0) {

        # updated thinning variables code
        ops=ops %>% 
          mutate(thin.ba.perc.rmv = (1-(thinning["aba"]/thinning["bba"]))*100.,
                 thin.ba.pre = thinning["bba"]*fvsUnitConversion("FT2pACRtoM2pHA"),
                 thin.qmd.ratio = ifelse(thinning["aadbh"]>=1, 
                                         thinning["badbh"]/thinning["aadbh"], 
                                         NA_real_),
                 thin.year = stdInfo["year"])
        
      } else if (!is.na(ops$thin.ba.perc.rmv) &&
                 stdInfo["year"]-ops$thin.year > 20) {
        # reset thinning variables to NA after 20 years
          # maybe we need an indicator variable ops$thin.active; to bypass thinning modifier calculations when ops$thin.active==F
        ops=ops %>% 
          mutate(thin.ba.perc.rmv = NA_real_,
                 thin.ba.pre = NA_real_,
                 thin.qmd.ratio = NA_real_,
                 thin.year = NA_real_)
      }
    }

    #fetch the fvs trees and form the AcadianGY "tree" dataframe
    orgtree = fvsGetTreeAttrs(c("plot","species","tpa","dbh","ht","cratio","special", "mgmtcd",
                                "dg", "htg", "mort")) 
    
    # add FVS alpha species codes and identify records with species outside scope of the model 
    orgtree = validate_acd_tree_spp(orgtree, spcodes=spcodes)  
    
    # create ACD species calibration dataframe
    calib.fvs = make_fvs_calib(spcodes=spcodes, 
                         tree.size.cap=tree.size.cap)
    
      # the make_fvs_calib() function fetches species calibration data from FVS using fvsGetSpeciesAttrs()
        #fetch the height, ba and mortality multipliers, variable "mults" where the rows are
        # fvs species index values and the columns are the attributes.
        # baimult    basal area increment multiplier for each species
        # htgmult    height growth multiplier for each species
        # mortmult   mortality rate multiplier for each species
        # mortdia1   lower diameter limit to apply the multiplier for each species
        # mortdia2   upper diameter limit to apply the multiplier for each species
        # maxdbh     morphological maximum diameter for each species from TreeSZCp keyword
        # maxht      morphological maximum height for each species from TreeSZCp keyword
        # minmort    minimum proportion of the tree record that will be killed for each species from TreeSZCp keyword
        # maxdbhcd   morphological maximum diameter use code for each species from TreeSZCp keyword
    
    
    # create ACD tree list and remove invalid tree records
      orgtree= make_acd_tree(orgtree, 
                             num.plots=as.numeric(room[['nplots']]), 
                             calib.fvs)   # tree.list, num.plots, calib.spp
        
      tree= validate_acd_tree_status(orgtree)  # tree.list, acd.species
    
      cat('AcadianGY: Stand= ', std.id[['standid']], '; Years= ', stdInfo[['year']], ' - ', stdInfo[['cendyear']], "\n")
      
#### Run ACD ####      
    # advance to the next cycle if no tree records
    if (nrow(tree) == 0) next
    
    for (year in stdInfo["year"]:stdInfo["cendyear"])
    {
      tree$year = year
      
      if(ops$verbose){cat ("fvsRunAcadian: calling AcadianGY, year=",year,"\n")}
      
      treeout = try(AcadianGYOneStand(tree, 
                                      stand = stand,
                                      ops = ops))
      
      # projection error handling
       if (inherits(treeout, "try-error") || any(is.na(treeout$dbh)) ||
               any(is.na(treeout$ht)) || any(is.na(treeout$expf)))
      {
        cat("AcadianGYOneStand failed in year=",year,"\n")
        dmpFile=file.path(getwd(),paste0("AcadianGYOneStand.Failure.",year,".RData"))
        if (class(treeout)!="try-error") treeout="critical result contains NA values"
        cat ("dmpFile name=",dmpFile,"\n")
        save(treeout,tree,stand,ops, file=dmpFile) ####
        tree=NULL
        break
      }
      tree=treeout
    }
    # if there was a failure, tree will be NULL, go on to the next stand cycle
    if (is.null(tree)) next
      
   
    cat ("fvsRunAcadian: is.null(tree$expf)=",is.null(tree$expf),"\n") 

      
#### ACD output ####          
    # tree list to hand back to FVS
    tofvs=make_fvs_tree(tree.data=tree, 
                        orgtree.list=orgtree,
                        num.plots=as.numeric(room[['nplots']]))

   

    fvsSetTreeAttrs(tofvs)

    atstop6 = FALSE

    
#### Regeneration ####    
    # adding regeneration?
    if(ops$use.ingrowth == TRUE){
    toadd= make_fvs_regen(tree.data=tree, 
                           orgtree.list=orgtree,
                           num.plots=as.numeric(room['nplots']),
                           spcodes=spcodes)
   
    newTrees = nrow(toadd)
    
    if(ops$verbose){cat ("fvsRunAcadian: num newtrees=",newTrees,"\n")}
        if (newTrees>0)
          {
            if (newTrees < room["maxtrees"] - room["ntrees"])
            {
             
              fvsRun(stopPointCode=6,stopPointYear=-1)
              atstop6 = TRUE
              fvsAddTrees(toadd)
            } else cat ("fvsRunAcadian: Not enough room for",newTrees,
                "new trees; Year=",year,"\n")
          }
      }
    
#### Volume ####    
    # modifying volume?
    if (ops$use.vol==TRUE)
    {
      if(ops$verbose){cat ("fvsRunAcadian: Applying Acadian volume logic\n")}
      
      mcstds = fvsGetSpeciesAttrs(vars=c("mcmind","mctopd","mcstmp"))
      vols = fvsGetTreeAttrs(c("species","ht","dbh","mcuft","defect"))
      
      # original volume code
      # vols$tcuft = ifelse (vols$dbh >= mcstds$mcmind[vols$species],
      #                      mapply(KozakTreeVol,Bark="ob",Planted=0,
      #                             DBH=vols$dbh  * INtoCM,
      #                             HT =vols$ht   * FTtoM,
      #                             SPP=spcodes[vols$species,1],
      #                             stump=mcstds$mcstmp[vols$species] * FTtoM,
      #                             topD =mcstds$mctopd[vols$species] * INtoCM), 0)
      # 
      # if (any(vols$defect != 0)) vols$tcuft = vols$tcuft *
      #   (1-(((vols$defect %% 10000) %/% 100) * .01))
      # vols$tcuft  = vols$tcuft * M3toFT3
      # vols$species=NULL
      # vols$ht     =NULL
      # vols$dbh    =NULL
      # vols$defect =NULL
      
      
      # updated volume calculation

      vols=vols %>%
        dplyr::mutate(tree=seq.int(1:n()), # sequential tree id used to retain order of tree list from fvs
                      dbh=dbh * INtoCM,
                      ht =ht   * FTtoM,
                      sp=spcodes[species,1],
                      stump=mcstds$mcstmp[species] * FTtoM,
                      diameter.merch= mcstds$mctopd[species] * INtoCM) %>% 
                      # updated for FVS 2025 Q4 release- change from tcuft to mcuft
        calc_tree_volume(taper.model = 'kozak',
                         sp.col = 'sp',
                         dbh.col = 'dbh',
                         ht.col = 'ht',
                         dia.type = 'ib',
                         plt = 0,
                         stump.col='stump',
                         dia.merch.col='diameter.merch',
                         segments = 10) %>% # calc_tree_volume returns dataframe
        # ACD volume calculation output cubic meters 
        # as of FVS release 2025Q4 the volume calculation is mcuft (previously tcuft)
        dplyr::mutate(mcuft=dplyr::case_when(dbh < mcstds$mcmind[species] * INtoCM ~0,
                                             defect != 0 & volume>0 ~volume *(1-(((defect %% 10000) %/% 100) * .01)),
                                             TRUE ~volume),
                      mcuft  = mcuft * M3toFT3) %>%
        # order tree list
        dplyr::arrange(tree) %>%
            #only set merch cubic foot volume
          dplyr::select(mcuft)
      
      
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
    myInlineTextInput("uiAcadianCutPoint","Ingrowth CutPoint",
                      fvsRun$uiCustomRunOps$uiAcadianCutPoint),
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
