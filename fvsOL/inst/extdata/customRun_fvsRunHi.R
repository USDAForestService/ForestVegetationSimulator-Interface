
unlink("FvsHi.log")

# Note: The form of the function call is very carefully coded. Make sure
# "runOps" exists if you want them to be used.
fvsRunHi <- function(runOps=NULL, logfile="FvsHi.log")
{

  if (!is.null(logfile) && !interactive())
  {
    sink()
    sink(logfile,append=TRUE)
  }
  
  cat ("\nLog file created\n")
  
  #load the growth model R code
  rFn="HiGy.R"
  if (file.exists(rFn)) {
    source(rFn)
    cat("fvsRunHi location rFn=", rFn, "\n")
    }else  {
    rFn = system.file("extdata", rFn, package="fvsOL")
    cat("fvsRunHi location rFn=", rFn, "\n")
    if (! file.exists(rFn)) {
      cat("fvsRunHi: ERROR - cannot find and load model code; rFn=", rFn, "\n")
      stop("can not find and load model code")
    }
    source(rFn)
  }
    cat ("*** in fvsRunHi",date()," VersionTag=",VersionTag,"\n")
  

  # process the ops.
  # Convert Shiny app selections using make_ops()
  if(!is.null(runOps)){
    .GlobalEnv$ops = make_ops(use.cap.dbh = runOps$uiHiDbhCap,   
                              use.cap.ht = runOps$uiHiHtCap)
    # verbose and rtn.vars will use defaults
    
  }else{
  # make ops will use all defaults
    .GlobalEnv$ops = make_ops()
    
}
  
  cat ("fvsRunHi, options set\n")

  #load some handy conversion factors
  # .GlobalEnv$CMtoIN  = fvsUnitConversion("CMtoIN")
  # .GlobalEnv$INtoCM  = fvsUnitConversion("INtoCM")
  # .GlobalEnv$FTtoM   = fvsUnitConversion("FTtoM")
  # .GlobalEnv$MtoFT   = fvsUnitConversion("MtoFT")
  # .GlobalEnv$M3toFT3 = fvsUnitConversion("M3toFT3")
  # .GlobalEnv$ACRtoHA = fvsUnitConversion("ACRtoHA")
  # .GlobalEnv$HAtoACR = fvsUnitConversion("HAtoACR")
  .GlobalEnv$spcodes = fvsGetSpeciesCodes()


 
#### height and crown ratio imputation ####  

  # start FVS but return prior to dubbing and calibration to detect which trees
  # have dubbed heights and crown ratios.
  
#  2024.05.29 commented out code for height and crown ratio imputation until errors are resolved
  
  fvsRun(7,0)

  orgtree.b4dub = fvsGetTreeAttrs(c('ht', 'cratio'))

  dub.htrows = orgtree.b4dub$ht == 0
  dub.crrows = orgtree.b4dub$cratio == 0

  # run the dubbing logic if any of the Ht or Cratio values are missing.

  if (any(dub.htrows) || any(dub.crrows))
  {
   
  # run fvs up to stop point 1 and change the dubbed values if there are any
  fvsRun(1,-1)
  # at this point tpa is computed
    
    stopPoint = fvsGetRestartcode()
    cat ('stopPoint code=',stopPoint,'\n Impute height and crown ratio')
    
  # fetch stand level information
  stdInfo = fvsGetEventMonitorVariables(c('site','year','cendyear','elev'))
  std.id  = fvsGetStandIDs()
  
  # fetch stand attributes from the Event Monitor
  climate = fvsGetEventMonitorVariables(c('rain', 'temp'))
  
  # Fetch FVS dimensions
  room=fvsGetDims()
  
  # create a stand dataframe- using the make_stand() function
  .GlobalEnv$stand=make_stand(stand.id=std.id[['standid']],
                              elev = stdInfo[['elev']], 
                              rain=  climate[['rain']], 
                              temp = climate[['temp']])
  
  ## fetch the fvs tree list and form the HiGY tree dataframe
  orgtree = fvsGetTreeAttrs(c("plot","species","tpa","dbh","ht","cratio","special", "mgmtcd",
                              "dg", "htg", "mort")) 
  
  # add FVS alpha species codes and identify records with species outside scope of the model 
  orgtree= validate_tree_spp(orgtree, spcodes=spcodes)  #
  
  # create species calibration dataframe
  calib.fvs =make_fvs_calib(spcodes=spcodes, 
                            tree.size.cap=tree.size.cap)
  
  #### temp --- add calib values for koa ####
  # calib.fvs=rbind(calib.fvs, data.frame(sp='AK', ddbh.mult=1, dht.mult=1,
  #                                       mort.mult=1, max.dbh=999, max.height=999))
  
  # create model tree list and remove invalid tree records
  orgtree= make_tree(orgtree, 
                     num.plots=as.numeric(room['nplots']), 
                     calib.fvs)  
  
  # Add tree attributes 
  tree = orgtree %>% # snags and dead trees are retained in the tree list
    # basal area (plot level)
    dplyr::mutate(ba=(dbh^2*0.0054541539)*expf,
                  # missing value indicators
                  phtrow=dub.htrows,
                  phcbrow=dub.crrows) %>% 
      # Calculate BAL
    calc_bal()

  
  # Plot attributes 
    # Calculate plot summary- for plot BA
  plot.smry = tree %>% 
    calc_plot_summary()
  
  # pred_ht: dbh, bal, ba, rain, temp,
  # pred_hcb: dbh, ht, bal, ba,
  tree = tree %>% 
    #predicted height (returns pht)
    calc_ht(plot.data = plot.smry) %>% 
    # use predicted height if missing height 
    dplyr::mutate(ht= dplyr::case_when(phtrow==TRUE & model.ex==FALSE ~pht,
                           TRUE ~ ht)) %>% 
    #predicted height to crown base (returns phcb)
    calc_hcb(plot.data = plot.smry) %>% 
    #use predicted height to crown base if cr is missing 
    dplyr::mutate(hcb= dplyr::case_when(phcbrow==TRUE & model.ex==FALSE & mgmtcd!=9 ~phcb, 
                                        TRUE ~hcb),
                  cratio = round((1-(hcb/ht))*100, 1)) %>% 
    dplyr::arrange(tree) %>% 
    dplyr::select(ht, 
                  cratio)
 
    fvsSetTreeAttrs(tree)
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
    cat ("stopPoint code=",stopPoint,"\n")
    if (stopPoint == 100) break
    

    # if there are no trees, don't attempt to run projections
    room=fvsGetDims()
    if (room['ntrees'] == 0) next

    # fetch stand level information
    stdInfo = fvsGetEventMonitorVariables(c("site","year","cendyear","elev"))
    std.id  = fvsGetStandIDs()
    
    # fetch stand attributes from the Event Monitor
    climate = fvsGetEventMonitorVariables(c('rain', 'temp'))
    
    # create a stand dataframe- using the make_stand() function
    .GlobalEnv$stand=make_stand(stand.id=std.id[['standid']],
                      elev = stdInfo[['elev']], 
                      rain=  climate[['rain']], 
                      temp = climate[['temp']])
    
    

  ## fetch the fvs tree list and form the HiGY tree dataframe
    orgtree = fvsGetTreeAttrs(c("plot","species","tpa","dbh","ht","cratio","special", "mgmtcd",
                                "dg", "htg", "mort")) 
    
 
    # add FVS alpha species codes and identify records with species outside scope of the model 
    orgtree= validate_tree_spp(orgtree, spcodes=spcodes)  #
    
    # create species calibration dataframe
    calib.fvs =make_fvs_calib(spcodes=spcodes, 
                         tree.size.cap=tree.size.cap)
    
    #### temp --- add calib values for koa ####
    # calib.fvs=rbind(calib.fvs, data.frame(sp='AK', ddbh.mult=1, dht.mult=1,
    #                                       mort.mult=1, max.dbh=999, max.height=999))
  
      
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
    
    
    # create model tree list and remove invalid tree records
      orgtree= make_tree(orgtree, 
                             num.plots=as.numeric(room['nplots']), 
                             calib.fvs)  
        
      tree= validate_tree_status(orgtree)  
    

    # advance to the next cycle if no tree records
    if (nrow(tree) == 0) next
    
    for (year in stdInfo['year']:stdInfo['cendyear'])
    {
      tree$year = year
      cat ("fvsRunHi: calling HiGY, year=",year,"\n")
      treeout = try(HiGYOneStand(tree, 
                                 stand=stand,
                                 ops=ops))
      
      # projection error handling
       if (inherits(treeout, "try-error") || any(is.na(treeout$dbh)) ||
               any(is.na(treeout$ht)) || any(is.na(treeout$expf)))
      {
        cat("hi_gy_stand failed in year=",year,"\n")
        dmpFile=file.path(getwd(),paste0("HiGY.Failure.",year,".RData"))
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
      
   
    cat ("fvsRunHi: is.null(tree$expf)=",is.null(tree$expf),"\n") 
    
    # tree list to hand back to FVS
    tofvs=make_fvs_tree(tree.data=tree, 
                        orgtree.list=orgtree,
                        num.plots=as.numeric(room['nplots']))

    
    fvsSetTreeAttrs(tofvs)

    atstop6 = FALSE

    # regeneration?

    
    # modifying volume?
   
    
  }
  cat ("rtn=",rtn,"\n")
  rtn
}

# NOTE: I (NLCrookston) tried various ways of building these elements. Setting the 
# initial value to the saved value when the elements are created seems to work well.
# What did not work was setting the initial value to some default and then
# changing it using an update call in the server code.

uiHi= function(fvsRun)
{


  if (is.null(fvsRun$uiCustomRunOps$uiHiDbhCap))
              fvsRun$uiCustomRunOps$uiHiDbhCap = "TRUE"
  if (is.null(fvsRun$uiCustomRunOps$uiHiHtCap))
              fvsRun$uiCustomRunOps$uiHiHtCap   = "TRUE"
  
  list(
    myRadioGroup("uiHiDbhCap", "Apply diameter limit:",
      c("TRUE", "FALSE"),selected=fvsRun$uiCustomRunOps$uiHiDbhCap),
    myRadioGroup("uiHiHtCap", "Apply height limit:",
      c("TRUE", "FALSE"),selected=fvsRun$uiCustomRunOps$uiHiHtCap)
    
  )
}
