# $Id: HiGy.R 3968 2026-02-10 10:36:05Z benrice $
################################################################################
# v0.1.0
#
# Hawaii Variant of the Forest Vegetation Simulator (FVS-HI)
#
# Developed by:
# Aaron Weiskittel, University of Maine, School of Forest Resources
# aaron.weiskittel@maine.edu
#
# Ben Rice, Midgard Natural Resources
# Midgard.Natural.Resources@gmail.com
#
################################################################################

library(dplyr) # needed arrange, mutate, left_join, tibble, select, group_by, summarise, ungroup, case_when, all_of
library(purrr) # needed for pmap_*

VersionTag = "HiGyV0.1.0"

##############################
#### major update summary ####
####

# version 0.1.0
  # initial version 
  # designed to work with FVS-HI and customRun_fvsRunHi.R
  # contains equations for koa


##############################

##### Total height prediction ####
ht.pred.parm = dplyr::tribble(
  ~type,    ~species,  ~b0,        ~b1,        ~b2,      ~b3,        ~b4,      ~b5,      ~b6,    ~b7,    ~b8,
  'base',    'AK',   69.78919,  0.06144,      0.85111,    0,      -0.05703, -0.11266, 0.02221, 0,       0,
  'climate', 'AK',  373.2487,   2.112872e-07, 4.1991,   -25.4689, -0.0189,  -0.0248,  0.0063, -0.0325,  0.5438)


#' Predict total height 
#' 
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param bal Numeric: Plot basal area larger (ft^2 per ac)
#' @param ba Numeric: Plot basal area (ft^2 per ac)
#' @param rain Numeric: Average annual rainfall (mm)
#' @param temp Numeric: Average annual temperature (C)
#' @param  b0-b8 Numeric: Parameters

#' @return Numeric: Predicted height (m)
#'
#Total Height =f(DBH (in), BAL (ft2/ac), BAPA (ft2/ac)), rain (mm), temp (C)
pred_ht= function(dbh, bal, ba, rain, temp, 
                  b0, b1, b2, b3, b4, b5, b6, b7, b8){
  
    ht = ifelse(rain %in% c(NA, 0)| temp %in% c(NA, 0),
                # without rain and temp
                4.5 + (b0) * (1 - exp(-b1 * dbh))^(b2 + b4 * log(bal + 1) + b5 * log((ba/100)+1) 
                                            + b6*log(bal*ba+1)),
                # with rain and temp variables
                4.5 + (b0+b3*log(rain*temp)) * (1 - exp(-b1* dbh^b2))^(b8+b4 * log(bal + 1) + b5 * log((ba/100)+1) 
                                            + b6*log(bal*ba+1)+ b7*log(rain*temp+1)))
  ht
}


#' Wrapper to calculate predicted heights for tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data
#' @param rain Numeric: Average annual rainfall (mm)
#' @param temp Numeric: Average annual temperature (C)
#' @param ht.spp.parms Dataframe: Dataframe of parameters (default ht.pred.parm)
#' @return Dataframe: Tree data with ht column added
#'
calc_ht = function(tree.data, plot.data, rain=stand$rain, temp=stand$temp, 
                   ht.pred.parm.df = ht.pred.parm) {
  
  tree.data.names= colnames(tree.data)   
  
  ht.parm.type = ifelse(rain %in% c(NA, 0)| temp %in% c(NA, 0),
                        'base',
                        'climate')
  
  ht.parm = ht.pred.parm.df %>% 
    filter(type==ht.parm.type)
    
  tree = tree.data %>% 
    dplyr::left_join(plot.data %>% 
                       dplyr::select(plot, ba.plot), 
                     by = 'plot') %>%
    # Match parameter estimates on species, Koa is currently the default
    # when the model extends to other species, the code may need to be updated to another default species
    dplyr::mutate(idx = match(sp, ht.pred.parm.df$species, nomatch = match('AK', ht.pred.parm.df$species)),
                  b0 = ht.pred.parm.df$b0[idx], 
                  b1 = ht.pred.parm.df$b1[idx], 
                  b2 = ht.pred.parm.df$b2[idx], 
                  b3 = ht.pred.parm.df$b3[idx], 
                  b4 = ht.pred.parm.df$b4[idx], 
                  b5 = ht.pred.parm.df$b5[idx], 
                  b6 = ht.pred.parm.df$b6[idx], 
                  b7 = ht.pred.parm.df$b7[idx], 
                  b8 = ht.pred.parm.df$b8[idx],
                  rain=rain, 
                  temp=temp, # maintains vectorized call of pred_ht()
                  pht = pred_ht(dbh, bal, ba=ba.plot, rain, temp, 
                                         b0, b1, b2, b3, b4, b5, b6, b7, b8)) %>%
    dplyr::select(dplyr::all_of(tree.data.names), pht)
  
  
  tree
}


##### Height to crown base prediction ####  
  
# Height to crown base species parameters
  hcb.pred.parm = dplyr::tribble(
              ~species,   ~b0,        ~b1,       ~b2,         ~b3,       ~b4,
              'AK',     -1.2329111, -0.2221513, 0.2485774,  0.0014994, 0.3421674)
  

#' Predict height to crown base
#' 
#' @param dbh Numeric: Diameter at breast height (in)
#' @param ht Numeric: Total tree height (ft)
#' @param bal Numeric: Plot basal area larger (ft^2 per ac)
#' @param ba Numeric: Plot basal area (ft^2 per ac)
#' @param b0-b4 Numeric: Species parameters
#' @return Numeric: Predicted height to crown base (ft)
#'
  pred_hcb = function(dbh, ht, bal, ba, b0, b1, b2, b3, b4) {
    
    # Calculate height to crown base
    hcb = ht/(1 + exp(b0 + b1 * sqrt(ht/100) + b2*log(ht/dbh) + 
                        b3 * sqrt(bal*ba + 1) + b4 * log(ba + 1)))
    
    hcb
  }
 
   
#' Wrapper to calculate height to crown base for tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data
#' @param hcb.spp.parms Dataframe: Species parameters (default hcb.pred.spp)
#' @return Dataframe: Tree data with hcb column added
#'
  calc_hcb = function(tree.data, plot.data, 
                      hcb.pred.parm.df = hcb.pred.parm) {
    
    tree.data.names= colnames(tree.data)  
    
    tree=tree.data %>% 
      dplyr::left_join(plot.data %>% 
                         dplyr::select(plot, ba.plot), 
                       by = 'plot') %>%
       # Match parameter estimates on species, Koa is currently the default
      dplyr::mutate(idx = match(sp, hcb.pred.parm.df$species, nomatch = match('AK', hcb.pred.parm.df$species)),
                    b0 = hcb.pred.parm.df$b0[idx],
                    b1 = hcb.pred.parm.df$b1[idx],
                    b2 = hcb.pred.parm.df$b2[idx], 
                    b3 = hcb.pred.parm.df$b3[idx],
                    b4 = hcb.pred.parm.df$b4[idx], 
                    phcb = pred_hcb(dbh, ht, bal, ba=ba.plot, b0, b1, b2, b3, b4)) %>%
      dplyr::select(dplyr::all_of(tree.data.names), phcb)
    
    tree
  }
  

#### Diameter increment ####

# Height increment parameters
  ddbh.parm = dplyr::tribble(
    ~type,    ~species,  ~b0,        ~b1,        ~b2,           ~b3,        ~b4,        ~b5,        ~b6,      ~b7,     ~b8,
    'base',    'AK',   -1.3223315,  0.2260716,   -0.2412006, -0.0002227,   -0.8247849,  0.0210812, 0.2809946, 0,        0,
    'climate', 'AK',  -86.87632,    0.46092,     -0.31036,   -0.00020,     -0.82359,    0.03270,   0.23718,   9.27207, -0.33883)

    
#' Calculate monthly diameter increment 
#' 
#' @param dbh Numeric: Diameter at breast height (in)
#' @param bal Numeric: Plot basal area larger (ft^2 per ac)
#' @param ba Numeric: Plot basal area (ft^2 per ac)
#' @param rain Numeric: Average annual rainfall (mm)
#' @param temp Numeric: Average annual temperature (C)
#' @param b0-b8 Numeric: Species parameters
#' @return Numeric: Diameter increment (in)
ddbh = function(dbh, bal, ba, rain, temp, 
                b0, b1, b2, b3, b4, b5, b6, b7, b8) {
  
  # diameter increment
  ddbh = exp(b0+b1*log(dbh+1)+b2*dbh+b3*(bal^2/log(dbh+5))+b4*log(bal+1)+
               b5*sqrt(ba*dbh)+b6*log(bal*ba+1)+
               ifelse(rain %in% c(NA, 0)| temp %in% c(NA, 0), 
                      0, b7*log(rain*temp))+
               ifelse(rain %in% c(NA, 0)| temp %in% c(NA, 0), 
                      0, b8*(rain*temp/1000)))
  
  ddbh
}


#' Wrapper function to calculate diameter increment with modifiers and constraints
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data  
#' @param use.cap.dbh Logical: Apply maximum DBH constraint (default ops$use.cap.dbh)
#' @param ddbh.parm.df Dataframe: Species parameter table for diameter increment
#' @param rain Numeric: Average annual rainfall (mm)
#' @param temp Numeric: Average annual temperature (C)
#' @return Dataframe: Tree data with diameter increment calculations
calc_ddbh = function(tree.data, plot.data, 
                     use.cap.dbh = ops$use.cap.dbh, 
                     ddbh.parm.df=ddbh.parm,
                     rain=stand$rain, 
                     temp=stand$temp ) {
  
  # get tree list variable names
  tree.data.names= colnames(tree.data)   
  
  # filter ddbh parameter estimate to type base or climate
  ddbh.parm.type = ifelse(rain %in% c(NA, 0)| temp %in% c(NA, 0),
                        'base',
                        'climate')
  
  ddbh.parm.df = ddbh.parm.df %>% 
    filter(type==ddbh.parm.type)

  # Calculate diameter increment 
  tree = tree.data %>%
    dplyr::left_join(plot.data %>% 
                       dplyr::select(plot, ba.plot), 
                     by = 'plot') %>%
    # Match parameter estimates on species, Koa is currently the default
    dplyr::mutate(idx = match(sp, ddbh.parm$species, nomatch = match('AK', ddbh.parm$species)),
                  b0 = ddbh.parm.df$b0[idx],
                  b1 = ddbh.parm.df$b1[idx],
                  b2 = ddbh.parm.df$b2[idx], 
                  b3 = ddbh.parm.df$b3[idx],
                  b4 = ddbh.parm.df$b4[idx], 
                  b5 = ddbh.parm.df$b5[idx],
                  b6 = ddbh.parm.df$b6[idx], 
                  b7 = ddbh.parm.df$b7[idx],
                  b8 = ddbh.parm.df$b8[idx],
                  # use purrr::reduce() to generate annual diameter increment
                  dbh.prj = purrr::reduce(1:12, 
                                          ~.x + ddbh(.x, bal, ba = ba.plot, 
                                                     rain, temp, 
                                                     b0, b1, b2, b3, b4, b5, b6, b7, b8), 
                                          .init = dbh),
                  # apply dbh increment multiplier
                  ddbh = (dbh.prj - dbh) * ddbh.mult)
  
  # Apply diameter growth cap if requested
  if (use.cap.dbh == TRUE) {
    tree = tree %>%
      dplyr::mutate(ddbh = dplyr::case_when((ddbh + dbh) > max.dbh ~ 0,
                                            TRUE ~ ddbh))
  }
  
  # Clean up temporary columns
  tree = tree %>%
    dplyr::select(dplyr::all_of(tree.data.names), ddbh)
  
  
  tree
}


#### Height increment ####

# Height increment parameters
dht.parm = dplyr::tribble(
    ~type,    ~species,  ~b0,        ~b1,        ~b2,           ~b3,        ~b4,        ~b5,        ~b6,        ~b7,      ~b8,
    'base',    'AK',   -1.2984999,  1.0873012,   -0.2133439, -0.0001268,    0,         0.0114348,   -0.0957501, 0,         0,
    'climate', 'AK',   163.54729,    2.80342,     -0.21923,   -0.00022,     -0.31141,   0.08924,      0.10094,  13.21554, -17.61490)


#' Calculate height increment
#' 
#' @param dbh Numeric: Diameter at breast height (in)
#' @param ht Numeric: Tree height (ft)
#' @param bal Numeric: Plot basal area larger (ft^2 per ac)
#' @param ba Numeric: Plot basal area (ft^2 per ac)
#' @param rain Numeric: Average annual rainfall (mm)
#' @param temp Numeric: Average annual temperature (C)
#' @param b0-b8 Numeric: Species parameters
#' @return Numeric: Height increment (ft)
dht = function(dbh, ht, bal, ba, rain, temp,
               b0, b1, b2, b3, b4, b5, b6, b7, b8) {
  
  dht =  ifelse(rain %in% c(NA, 0)| temp %in% c(NA, 0),
         # without rain and temp
         exp(b0+b1*log(dbh+1)+b2*dbh+b3*(bal^2/log(dbh+5))+b4*log(bal+1)+
               b5*sqrt(ba*dbh)+b6*log(bal*ba+1)),
         # with rain and temp variables
         exp(b0+b1*log(ht+1)+b2*ht+b3*(bal^2/log(dbh+5))+b4*log(bal+1)+
               b5*sqrt(ba*dbh)+b6*log(bal*ba+1)+b7*sqrt(rain*temp/1000)+b8*log((rain*temp)^2/1000)))
  
  dht
}


#' Wrapper function to calculate height increment with modifiers and constraints
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data  
#' @param use.cap.ht Logical: Apply maximum total tree height constraint (default ops$use.cap.ht)
#' @param dht.parm.df Dataframe: Species parameter table for height increment (dht.parm)
#' @param rain Numeric: Average annual rainfall (mm)
#' @param temp Numeric: Average annual temperature (C)
#' @return Dataframe: Tree data with height increment calculations
calc_dht = function(tree.data, 
                    plot.data,  
                    use.cap.ht = ops$use.cap.ht, 
                    dht.parm.df = dht.parm,
                    rain=stand$rain, 
                    temp=stand$temp) {
  
  # get tree list variable names
  tree.data.names= colnames(tree.data)   
  
  # filter dht parameter estimate to type base or climate
  dht.parm.type = ifelse(rain %in% c(NA, 0)| temp %in% c(NA, 0),
                          'base',
                          'climate')
  
  dht.parm = dht.parm.df %>% 
    filter(type==dht.parm.type)
  
  # Calculate height increment
  tree = tree.data %>%
    dplyr::left_join(plot.data %>% 
                       dplyr::select(plot, ba.plot), 
                     by = 'plot') %>%
    # Species parameters
    dplyr::mutate(idx = match(sp,  
                              dht.parm$species,
                                 nomatch = match('AK', dht.parm$species)), # 
                  b0 = dht.parm$b0[idx],
                  b1 = dht.parm$b1[idx],
                  b2 = dht.parm$b2[idx], 
                  b3 = dht.parm$b3[idx],
                  b4 = dht.parm$b4[idx], 
                  b5 = dht.parm$b5[idx],
                  b6 = dht.parm$b6[idx], 
                  b7 = dht.parm$b7[idx],
                  b8 = dht.parm$b8[idx],
                  rain=rain, 
                  temp=temp,
      # use purrr::reduce() to generate annual height increment
      dht.prj = purrr::reduce(1:12, 
                              ~.x + dht(dbh, ht, bal, ba=ba.plot, rain, temp,
                                        b0, b1, b2, b3, b4, b5, b6, b7, b8),
                              .init = ht),
      #apply ht increment multiplier
      dht = (dht.prj-ht) * dht.mult)
 
  # Apply height cap 
  if (use.cap.ht == TRUE) {
    tree = tree %>%
      dplyr::mutate(dht = dplyr::case_when((dht + ht) > max.height ~ 0,
                                           TRUE ~ dht))
  }
 
  # Clean up temporary columns
  tree = tree %>%
    dplyr::select(dplyr::all_of(tree.data.names), dht)
  
  tree
}


### Crown recession ####

#### Mortality ####

# Tree survival probability  parameters
surv.parm = dplyr::tribble(
  ~type,    ~species,  ~b0,        ~b1,        ~b2,           ~b3,        ~b4,         ~b5,        ~b6,        ~b7,     
  'base',    'AK',    7.320309,  0.207024,   -0.685661,     0.710481,    -0.023592,  -0.945985,     0,         0,       
  'climate', 'AK',   1419.0 ,     -0.6523,      0.03946,      0.5664,      -0.02556,   -0.1086,    -171.1,       2.019)


#' Calculate tree survival probability
#' 
#' @param dbh Numeric: Diameter at breast height (in)
#' @param ht Numeric: Tree height (ft)
#' @param bal Numeric: Plot basal area larger (ft^2 per ac)
#' @param ba Numeric: Plot basal area (ft^2 per ac)
#' @param rain Numeric: Average annual rainfall (mm)
#' @param temp Numeric: Average annual temperature (C)
#' @param yip Numeric: 
#' @param b0-b8 Numeric: Species parameters
#' @return Numeric: Tree survival probability (proportion 0-1)
surv_prob = function(dbh, ht, bal, ba, rain, temp, yip=1,
                     b0, b1, b2, b3, b4, b5, b6, b7) {
  
  lp =  ifelse(rain %in% c(NA, 0)| temp %in% c(NA, 0),
               # without rain and temp
               b0+b1*(dbh)+b2*log(dbh^2)+b3*log(ht/dbh+1)+b4*((bal+1)/log(dbh+1))+b5*log(ba),
               # with rain and temp variables
               b0+b1*(dbh)+b2*log(dbh^2)+b3*log(ht/dbh+1)+b4*((bal+1)/log(dbh+1))+b5*sqrt(ba)+b6*log(temp*rain)+b7*sqrt(temp*rain))
  
  surv =  (1 / (1 + exp(-lp)))^(1/yip)
  
  surv
}


#' Calculate mortality and modifiers for tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data
#' @param surv.parm.df Dataframe: Species parameter table for survival (default surv.parm) 
#' @return Dataframe: Tree data with mortality calculations
calc_mortality = function(tree.data, plot.data,
                          surv.parm.df =surv.parm,
                          rain=stand$rain, 
                          temp=stand$temp) {
  
  # get tree list variable names
  tree.data.names= colnames(tree.data)   
  
  # filter survival parameter estimate to type base or climate
  surv.parm.type = ifelse(rain %in% c(NA, 0)| temp %in% c(NA, 0),
                          'base',
                          'climate')
  
  
  surv.parm = surv.parm.df %>% 
    filter(type==surv.parm.type)
  
  
 
  # Join tree and plot summary data 
  tree = tree.data %>%
    dplyr::left_join(plot.data %>% 
                       dplyr::select(plot, ba.plot), 
                     by = 'plot') %>%
    
    # calculate tree survival probability
    dplyr::mutate(idx = match(sp,  
                              surv.parm$species,
                              nomatch = match('AK', surv.parm$species)), # 
                  b0 = surv.parm$b0[idx],
                  b1 = surv.parm$b1[idx],
                  b2 = surv.parm$b2[idx], 
                  b3 = surv.parm$b3[idx],
                  b4 = surv.parm$b4[idx], 
                  b5 = surv.parm$b5[idx],
                  b6 = surv.parm$b6[idx], 
                  b7 = surv.parm$b7[idx],
                  rain=rain,
                  temp=temp,
                  surv= surv_prob(dbh, ht, bal, ba=ba.plot, rain, temp, yip=1,
                                       b0, b1, b2, b3, b4, b5, b6, b7),
                  dexpf= expf*(1-surv),
                  #apply mortality multiplier
                  dexpf = dexpf * mort.mult)
  
 
  # Remove temporary columns
  tree = tree %>%
    dplyr::select(dplyr::all_of(tree.data.names), dexpf)
  
  tree
}


#### Calibration ####    
# multipliers for diameter increment, height increment and mortality
# maximum tree diameter and height
    
  # default tree size limits from HI FIA data 
    
    tree.size.cap=dplyr::tribble(
      ~species, ~max.dbh, ~max.height,
      'AK',	  90,	    92)	# 
   

#' create table of height and diameter increment and mortality multipliers from FVS species attributes and tree size cap       
#' 
#' @param spcodes Dataframe: FVS species codes from fvsGetSpeciesCodes(). Required fields- fvs (FVS alpha species code). FVS numeric code is the vector "row" number. Note fvsGetSpeciesCodes() returns a character vector 
#' @param tree.size.cap Dataframe: tree size limits, Required fields: species (FVS alpha species code), max.dbh and max.height 
#' @return Dataframe: Species calibration factors

    make_fvs_calib=function(spcodes, tree.size.cap){
      
      # customary-metric conversion
      in.to.cm = 2.54
      ft.to.m = 0.3048
      
      # get fvs dll
      fvs.loaded=try(as.character(get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)[['ldf']]),
                     silent = TRUE)
      
      if (inherits(fvs.loaded, "try-error") || is.na(fvs.loaded)){
        stop('FVS variant DLL not loaded')
      }
      
      # fetch calibration multipliers from FVS   
      calib.fvs = fvsGetSpeciesAttrs(c("baimult","htgmult","mortmult","mortdia1","mortdia2",
                                       "maxdbh", "maxht", "minmort", "maxdbhcd"))
      
      
      # calibration dataframe fields
      calib.df = data.frame(sp=character(),
                            baimult=numeric(), 
                            htgmult=numeric(),
                            mortmult=numeric(),
                            mortdia1=numeric(),
                            mortdia2=numeric(),
                            maxdbh=numeric(), 
                            maxht=numeric(), 
                            minmort=numeric(), 
                            maxdbhcd=numeric())
      
      # create dataframe  
      calib.fvs= calib.fvs %>% 
        dplyr::mutate(fvs.num=as.integer(rownames(.))) %>%
        dplyr::left_join(spcodes %>% 
                           as.data.frame() %>% 
                           dplyr::transmute(sp=as.character(fvs),
                                            fvs.num=as.integer(rownames(.))),
                         by='fvs.num')
      
      # some variables in fvsGetSpeciesAttrs() are limited to the development version 2024-04-01
      # ensure that df contains all variable
      calib.fvs=calib.df %>% 
        dplyr::bind_rows(calib.fvs) %>% 
        dplyr::rename(ddbh.mult=baimult,
                      dht.mult=htgmult,
                      mort.mult=mortmult,
                      maxdbh.fvs=maxdbh, 
                      maxht.fvs=maxht) 
      
      
      # join with default max height and diameter values
      calib.fvs=calib.fvs %>% 
        dplyr::left_join(tree.size.cap,
                         by=c('sp'='species')) %>% 
        dplyr::mutate(dplyr::across(c(ddbh.mult, # if multipliers are 0 or 999 change to NA 
                                      dht.mult, 
                                      mort.mult,
                                      maxdbh.fvs,
                                      maxht.fvs),
                                    ~ifelse(.x %in% c(0, 999), NA, .x)),
                      max.dbh=dplyr::coalesce(maxdbh.fvs, 
                                              max.dbh, 999), 
                      max.height=dplyr::coalesce(maxht.fvs, 
                                                 max.height, 999),
                      ddbh.mult=dplyr::coalesce(ddbh.mult, 1), 
                      dht.mult=dplyr::coalesce(dht.mult, 1), 
                      mort.mult=dplyr::coalesce(mort.mult, 1)) %>% 
        dplyr::select(sp, ddbh.mult, dht.mult, mort.mult, max.dbh, max.height)
      
      
      calib.fvs
    }
   
    # Notes: 
      # function will test if FVS DLL is loaded
      # tree size limits from FVS-NE and tree.size.cap values are in customary units
      # call to FVS will return multipliers via fvsGetSpeciesAttrs(c("baimult","htgmult","mortmult","mortdia1","mortdia2",
        #  "maxdbh", "maxht", "minmort", "maxdbhcd"))
    
    
#### Ingrowth ####

#### Prepare input tree list ####
####

## define model species  
  hi.species.ht.dia=dht.parm %>% 
    dplyr::inner_join(ddbh.parm, by='species', 
                      relationship = "many-to-many") %>% 
    dplyr::distinct(species)

## For tree list from FVS add FVS alpha species codes and identify records with species outside scope of the model
    #' Prepare tree list using FVS tree list 
    #' 
    #' @param tree.data Dataframe: tree list from FVS. Required tree list fields: plot, species (fvs numeric species code), tpa, dbh, ht, cratio, mgmtcd, special (used in form, Risk)
    #' @param spcodes Dataframe: FVS species codes. Required fields: fvs (FVS alpha species code). FVS numeric code is the vector "row" number. Note fvsGetSpeciesCodes() returns a character vector 
    #' @param acd.species Dataframe: Acadian model species. Default species contained in the diameter and height increment parameter estimate tables (acd.species$sp)
    #' @return Dataframe: Tree list with added columns (sp: FVS alpha speies code and model.ex: indicator value to drop records when returning to FVS)
    #'
     validate_tree_spp=function(tree.data, spcodes, model.species= hi.species.ht.dia$species){
    
    # retain records in the projections for accurate plot values and change species to OH
    
    tree.list=tree.data %>% 
      dplyr::rename(fvs.num= species) %>%
      dplyr::left_join(spcodes %>% 
                         as.data.frame() %>% 
                         transmute(sp=fvs,
                                   fvs.num=as.integer(rownames(.))),
                       by='fvs.num') %>%
        dplyr::mutate(model.ex=dplyr::case_when(!sp %in% model.species ~TRUE, # indicator value to drop records when returning to FVS
                                              TRUE ~ FALSE),
                      sp=dplyr::case_when(!sp %in% model.species ~'OT', # assign species code OT
                                          TRUE ~ sp))                                            
      
      tree.list  
  }
 

## drop invalid tree records not handled by the model (snags, dbh=0)
     #' Filter tree records with DBH=0 and snags
     #' @param tree.data Dataframe: tree list from FVS. Required tree list fields: mgmtcd, sp (FVS alpha species code), dbh
     #' @param acd.species Dataframe: Acadian model species. Default species contained in the diameter and height increment parameter estimate tables (acd.species$sp)
     #' @return Dataframe: Tree list 
  validate_tree_status=function(tree.data){
     
    tree.list=tree.data %>% 
      dplyr::filter(mgmtcd!=9, # remove snags from tree list
                    dbh>0) # remove tree records with dbh zero or NULL
                   
    
    tree.list
    
  } 

  
## create model input dataframe from FVS tree list
  #' Create tree list dataframe
  #' 
  #' @param tree.list Dataframe: Tree list from FVS. Required tree.list fields: plot, species (fvs numeric species code), tpa, dbh, ht, cratio, mgmtcd, special (used in Form, Risk)
  #' @param num.plots Numeric: Number of plots in a stand
  #' @param calib.spp Dataframe: Data frame of species calibration and size limits. Required fields: sp (FVS alpha species code), dDBH.mult, d.ht.mult, mort.mult, max.dbh,  max.height
  #' @return Dataframe: Tree list dataframe
  # Note: FVS-NE dg; htg and mort=MORT remain in customary units
  
make_tree=function(tree.list, num.plots, calib.spp){
 
   # customary-metric conversion
  in.to.cm = 2.54
  ft.to.m = 0.3048
  ha.to.ac = 2.47105
  
  tree.list.vars=c('cr', 'dbh', 'ht', 'special', 'sp')
  
  # stop if tree list is missing required variables
  
  # if(all(tree.list.vars %in% names(tree.list))==FALSE){ 
  #   stop('Required tree list variable missing')
  #   message(setdiff(tree.list.vars, names(tree.list)))
  # }
  
  tree.list=tree.list %>% 
    dplyr::rename_with(.fn=tolower) %>% 
    dplyr::rename(cr= cratio,
                  expf= tpa) %>%
    dplyr::mutate(tree = seq.int(1:n()), # sequential tree id used to retain order of tree list from fvs
                  cr = abs(cr) * 0.01,
                  #change cr to a proportion and take abs; note that in FVS a negative cr
                  #signals that cr change has been computed by the fire or insect/disease model
                  hcb = ht-cr*ht,
                  expf = expf * dplyr::coalesce(num.plots, 1) ) %>%  # each plot as "stand"
     # metric to customary option            
        # dbh  = dbh  * in.to.cm, # metric conversion
        # ht   = ht   * ft.to.m,
        # hcb = ht-cr*ht,
        # expf = expf * dplyr::coalesce(num.plots, 1) * ha.to.ac, # each plot as "stand"
    
     
    dplyr::left_join(calib.spp,
                     by='sp')
  
    tree.list
  }


#### Prepare model options (ops) ####
#' Create run options dataframe
#' 
#' @param verbose Logical or character: Print verbose output. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default FALSE)
#' @param rtn.vars Character vector: Variables to return in output (default core variables)
#' @param use.cap.dbh Logical or character: Apply maximum DBH constraint. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default TRUE).
#' @param use.cap.ht Logical or character: Apply maximum total tree height constraint. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default TRUE)
#' @return Dataframe: Run options dataframe
#'
make_ops = function(verbose = FALSE,
                    rtn.vars = c('year', 'plot', 'tree', 'sp', 'dbh', 'ht', 
                                 'hcb', 'expf', 'pht', 'phcb', 
                                 'ddbh.mult', 'dht.mult', 'mort.mult', 'max.dbh', 'max.height'),
                    use.cap.dbh = TRUE,
                    use.cap.ht = TRUE) {
  
  # Convert strings to logical-- in case someone redefines TRUE and FALSE 
  char_to_logical = function(x) {
    
    if (is.logical(x)) return(x)
    if (is.character(x)) {
      x_upper = toupper(trimws(x))
      if (x_upper %in% c("YES", "Y", "TRUE", "T")) return(TRUE)
      if (x_upper %in% c("NO", "N", "FALSE", "F")) return(FALSE)
    }
    # Return original value if not convertible
    x
  }
  
  
  # Create dataframe
  ops = data.frame(verbose = char_to_logical(verbose),
                   rtn.vars = I(list(rtn.vars)), # rtn.vars as a list column 
                   use.cap.dbh = char_to_logical(use.cap.dbh),
                   use.cap.ht = char_to_logical(use.cap.ht)) %>%
    dplyr::mutate(dplyr::across(c(verbose, use.cap.dbh, use.cap.ht), 
                                as.logical))
  
  
  ops
}


#### Prepare stand dataframe ####
#' Create ACD stand list
#' 
#' @param stand.id character: Stand identifier
#' @param rain Numeric: Average annual rainfall (mm)
#' @param temp Numeric: Average annual temperature (C)
#' @return Dataframe: Stand dataframe
#'
  make_stand= function(stand.id,
                       elev =0, 
                       rain=0,
                       temp=0){
    
    stand=data.frame(stand.id=stand.id,
                     elev=elev,
                     rain = rain, 
                     temp =temp)
    
    stand
    
  }

#### Prepare output tree list ####
#### for FVS fvsSetTreeAttrs()
#' Create FVS return tree list
#' 
#' @param tree.data Dataframe: tree list output from model, fields:  year; dbh; ht; hcb; expf; cr
#' @param num.plots Numeric: number of plots in a stand
#' @param orgtree.list Dataframe: input tree list from FVS. orgtree.list fields tree; dbh; ht; expf; dg; htg; mort; cratio
#' @return Dataframe: FVS tree dataframe
#'

make_fvs_tree=function(tree.data, orgtree.list, num.plots){
  
  #customary-metric conversion 
  cm.to.in = 0.393701
  m.to.ft = 3.28084
  ac.to.ha = 0.404686
  
  # remove projected values for species not in model
  tree.list=tree.data %>% 
    dplyr::anti_join(orgtree.list %>% 
                       dplyr::filter(model.ex==TRUE), 
                     by='tree')
  
  # dataframe with tree records not handled by model- snags and invalid DBH; species not in model
  tree.org=orgtree.list %>% 
    dplyr::select(tree, 
                  dbh, # customary units
                  ht, # customary units
                  expf, # plot level tpa 
                  dg, # customary units
                  htg, # customary units
                  mort, # stand level TPA
                  cr) %>% 
    dplyr::anti_join(tree.list, 
                     by='tree')
  # customary to metric option
      # dg=(dbh-dbh.fvs)*cm.to.in, # diameter growth to inches
      # htg=(ht-ht.fvs)*m.to.ft,  # height growth to feet
      # mort=(expf.fvs-expf)*ac.to.ha,  # mortality TPH stand level to trees per acre
  
  tree=orgtree.list %>% 
    dplyr::select(tree, 
                  dbh.fvs=dbh,
                  ht.fvs=ht, 
                  expf.fvs=expf) %>% 
    dplyr::inner_join(tree.list, # inner join excludes records not handled by model
                      by='tree') %>% 
    dplyr::mutate(dg=(dbh-dbh.fvs), # diameter growth 
                  htg=(ht-ht.fvs),  # height growth 
                  # set the crown ratio sign to negative so that FVS doesn't change them. 
                  cratio = round((1-(hcb/ht))*-100, 1), # 
                  mort=(expf.fvs-expf),  # mortality trees per acre
                  mort=mort/dplyr::coalesce(num.plots, 1)) %>%  # calculate stand level mortality TPA
    dplyr::bind_rows(tree.org) %>% # append tree records not handled by model
    dplyr::arrange(tree) %>% 
    dplyr::select(#dbh,
                  #sp,
                  dg,
                  htg,
                  cratio,
                  mort)
  
  #tibble to dataframe  
  tree=as.data.frame(tree)
  
  tree
}

#### tree list for FVS fvsAddTrees() (if adding regen)


#### Calculated tree values ####

#' Calculate basal area in larger trees (BAL) for each tree
#' 
#' @param tree.data Dataframe: Tree list
#' @return Dataframe: Tree data with added BAL column
#'
calc_bal = function(tree.data) {
  
  # Check required columns
  required.cols = c('plot', 'dbh', 'ba')
  missing.cols = setdiff(required.cols, names(tree.data))
  if (length(missing.cols) > 0) {
    stop(paste("Missing required columns:", paste(missing.cols, collapse = ", ")))
  }
  
  # Sort by plot and descending DBH; calculate cumulative BA
  tree=tree.data %>%
    dplyr::arrange(plot, 
            desc(dbh)) %>%
    dplyr::group_by(plot) %>%
    dplyr::mutate(bal = cumsum(ba) - ba) %>% 
    dplyr::ungroup()
  
  tree
}


#### Calculated plot values ####


#' Calculate plot summary statistics from tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.col Character: Column name for plot identifier (default 'plot')
#' @param tree.col Character: Column name for tree identifier (default 'tree')
#' @param sp.col Character: Column name for species code (default 'sp')
#' @param dbh.col Character: Column name for diameter at breast height (default 'dbh')
#' @param expf.col Character: Column name for expansion factor (default 'expf')
#' @param ba.col Character: Column name for basal area (default 'ba')
#' @param sg.col Character: Column name for specific gravity (default 'sg')
#' @param sp.type.col Character: Column name for species type (default 'sp.type')
#' @param shade.col Character: Column name for shade tolerance (default 'sp.type')
#' @return Dataframe: Plot-level summary statistics
#'
calc_plot_summary = function(tree.data) {
  
  # Check required columns
  required.cols = c('plot', 'tree', 'sp', 'dbh', 'expf', 'ba')
  missing.cols = setdiff(required.cols, names(tree.data))
  if (length(missing.cols) > 0) {
    stop(paste("Missing required columns:", paste(missing.cols, collapse = ", ")))
  }
  
  # Plot level summary
  plot.summary = tree.data %>%
    dplyr::group_by(plot) %>%
    dplyr::summarise(tpa.plot = sum(expf, na.rm = TRUE),
                     ba.plot = sum(ba, na.rm = TRUE),
                     # max tree number for ingrowth
                     max.tree.id = max(tree, na.rm = TRUE), .groups = 'drop') %>%
    # QMD
    dplyr::mutate(qmd = sqrt(ba.plot / (0.0054541539 * tpa.plot)))
  
  plot.summary
}

# Plot top height    
#' Calculate plot top height 
#' 
#' @param tree.data Dataframe: Tree list
#' @param topht.tph Numeric: Number of trees per hectare to include (default 100)
#' @return Dataframe: Plot top height values with columns for plot and topht
#'
calc_topht = function(tree.data, topht.tph = 100) {
  
  plot.topht = tree.data %>%
    dplyr::arrange(plot, 
                   desc(ht)) %>%
    dplyr::group_by(plot) %>%
    dplyr::mutate(cum.expf = cumsum(expf),
                  tree.inc = dplyr::case_when(cum.expf <= topht.tph ~ expf,
                                              topht.tph - (cum.expf - expf) > 0 ~ topht.tph - (cum.expf - expf),
                                              TRUE ~ 0),
                  wt.ht = tree.inc * ht) %>%
    dplyr::summarise(mean.ht = weighted.mean(ht, expf, na.rm = TRUE),
                     wt.ht.sum = sum(wt.ht, na.rm = TRUE),
                     tph.actual = sum(tree.inc, na.rm = TRUE),
                     .groups = 'drop') %>%
    dplyr::mutate(topht = ifelse(tph.actual > 0, 
                                 wt.ht.sum / tph.actual, 
                                 mean.ht)) %>%
    dplyr::select(plot, 
                  topht)
  
  plot.topht
}



#### Model execution ####  

### Call growth and yield model for each stand
Hi.GY = function(tree, stand, ops = NULL) {
  
  ans = tree %>%
    dplyr::filter(!is.na(stand.id)) %>%
    split(.$stand.id) %>%
    purrr::imap_dfr(function(tree.subset, stand.id.subset) {
      stand.subset = subset(stand, stand.id == stand.id.subset)
      
      AcadianGYOneStand(tree.subset, 
                        stand = stand.subset, 
                        ops = ops)
    })
  
  tree = ans %>%
    dplyr::arrange(year, stand.id, plot, tree)
  
  tree
}

  
### Growth and yield model called for one stand at time
#' Run growth and yield model for one year for one stand
#' 
#' @param tree Dataframe: Tree list
#' @param stand Dataframe: Stand attributes
#' @param ops Dataframe: 
#' @return Dataframe: Plot top height values with columns for plot and topht
#'
HiGYOneStand = function(tree, stand, ops)
{
  ### -----
  ## before proceeding run 
  ## * make.ops()
  ## * make_stand() 
  ## * make_acd_tree()
  ## * check tree list variables
  ### ----
  
  
##### Add tree attributes ####
  tree = tree %>% 
        # basal area (plot level)
    dplyr::mutate(ba=(dbh^2*0.0054541539)*expf) %>% 
      # Calculate BAL
    calc_bal()

  
##### Plot attributes ####
    # Calculate plot summary
  plot.smry = tree %>% 
    calc_plot_summary()
  
##### Height and crown ratio ####  
  #calculate heights of any with missing values.
  #generally, none will be missing when function is used with FVS, but some or
  #all may be missing when code us used to grow tree lists from other sources. 
  tree = tree %>% 
    # mutate(rain=stand.copy$rain, 
    #        temp=stand.copy$temp) %>% 
          #predicted height
    calc_ht(plot.data = plot.smry) %>% 
    dplyr::mutate(#use predicted height if missing or > 150ft
           ht= dplyr::case_when(ht %in% c(NA, 0)| ht>150 ~pht,
                        TRUE ~ ht)) %>% 
           #predicted height to crown base (returns phcb)
    calc_hcb(plot.data = plot.smry) %>% 
           #use predicted height to crown base if hcb is missing or invalid
    dplyr::mutate(hcb= dplyr::case_when(is.na(hcb) | hcb>ht  ~phcb, 
                                TRUE ~hcb))
  
# Compute plot-level heights
  
  # Plot top height (for 100 tph)
  plot.topht = tree %>% 
    calc_topht()
   
  # Add to plot summary
  plot.smry=plot.smry %>% 
    dplyr::left_join(plot.topht,
                     by='plot') 
 
##### Diameter increment ####
  # calc_ddbh = function(tree.data, plot.data, 
  #                      use.cap.dbh = ops$use.cap.dbh, 
  #                      ddbh.parm.df=ddbh.parm,
  #                      rain=stand$rain, temp=stand$temp, )
  
  tree = tree %>%
    calc_ddbh(plot.data = plot.smry)
 
##### Height increment ####  
  # calc_ht = function(tree.data, plot.data, rain=stand$rain, temp=stand$temp, 
  #                    ht.pred.parm.df = ht.pred.parm)
  
  tree = tree %>%
    calc_dht(plot.data = plot.smry)

#### Crown recession ####
 
#### Ingrowth ####
  
  
#### Mortality ####
  # calc_mortality = function(tree.data, plot.data,
  #                           surv.parm.df =surv.parm) 
  
  tree = tree %>% 
    calc_mortality(plot.data=plot.smry)
  
#### Output ####  
  #Update tree values
  tree=tree %>% 
    dplyr::mutate(year= year+1,
                  dbh= dbh+ dplyr::coalesce(ddbh, 0),
                  ht= ht+ dplyr::coalesce(dht, 0),
                 # hcb= hcb + dplyr::coalesce(dhcb, 0),
                  expf= dplyr::coalesce(expf, 0) - dplyr::coalesce(dexpf, 0),
                  expf= ifelse(expf< 0.00001, 0.00001, expf),
                  cr= 1-(hcb/ht)) 
  
  # select return variables
  rtn.vars=intersect(ops$rtn.vars[[1]],
                     colnames(tree))
  tree=subset(tree,
              select=rtn.vars) %>% 
    as.data.frame()                          

  tree
}         

####
#### Taper ####
####
       
