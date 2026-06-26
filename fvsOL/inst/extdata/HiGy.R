# $Id: HiGy.R 3968 2026-02-10 10:36:05Z benrice $
################################################################################
# v0.2.0
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

VersionTag = "HiGyV0.2.0"

##############################
#### major update summary ####
####

# version 0.2.0
  # updated equations- integration of biomass yield index (BYI) and planted indicator
  # planted indicator is derived from FVS_Standinit.StdOrgCd (Stand Origin Code; also used by the FIAVBC keyword)
    # Natural stand = 0 - established through natural regeneration
    # Plantation = 1 - established through planting

# version 0.1.0
  # initial version 
  # designed to work with FVS-HI and customRun_fvsRunHi.R
  # contains equations for koa


##############################

##### Total height prediction ####
ht.pred.parm = dplyr::tribble(
  ~type,   ~species,  ~a0,      ~a1,    ~b,      ~c,     ~g1,      ~g2,
  'base',  'AK',      19.832,   0,      0.044,   0.863,   -0.198,   0.479,
  'site',  'AK',      19.832,   0.106,  0.044,   0.863,   -0.198,   0.479)



#' Predict total height 
#' 
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param bal Numeric: Plot basal area larger trees (m^2 per ha)
#' @param ba Numeric: Plot basal area (m^2 per ha)
#' @param byi Boolean: Biomass Yield Index (Mg per ha). If NULL or 0, uses basic model
#' @param  a0-g2 Numeric: Parameters
#' @return Numeric: Predicted height (m)
#'
#
pred_ht= function(dbh,  ba, bal, qmd, byi, 
                  a0, a1, b, c, g1, g2){
  
  rdbh = dbh/qmd
  
  ht.intercept = ifelse(byi %in% c(NA, 0), 
                      a0,
                      a0 + a1 * byi / 100)
  
  ht = pmax(ht.intercept * (1 - exp(-b * dbh))^c *
              exp(g1 * log(ba + 1) + g2 * rdbh),
            1.37) # enforce minimum of breast height
  
    
  ht
}


#' Wrapper to calculate predicted heights for tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data
#' @param byi Biomass Yield Index (Mg per ha). If NULL or 0, use base model
#' @param ht.spp.parms Dataframe: Dataframe of parameters (default ht.pred.parm)
#' @return Dataframe: Tree data with ht column added
#'
calc_ht = function(tree.data, plot.data, byi=stand$byi, 
                   ht.pred.parm.df = ht.pred.parm) {
  
  tree.data.names= colnames(tree.data)   
  
  ht.parm.type = ifelse(byi %in% c(NA, 0),
                        'base',
                        'site')
  
  ht.parm = ht.pred.parm.df %>% 
    filter(type==ht.parm.type)
    
  tree = tree.data %>% 
    dplyr::left_join(plot.data %>% 
                       dplyr::select(plot, ba.plot, qmd), 
                     by = 'plot') %>%
    # Match parameter estimates on species, Koa is currently the default
    # when the model extends to other species, the code may need to be updated to another default species
    dplyr::mutate(idx = match(sp, ht.parm$species, nomatch = match('AK', ht.parm$species)),
                  a0 = ht.parm$a0[idx], 
                  a1 = ht.parm$a1[idx], 
                  b  = ht.parm$b[idx], 
                  c  = ht.parm$c[idx], 
                  g1 = ht.parm$g1[idx], 
                  g2 = ht.parm$g2[idx], 
                  byi = coalesce(byi, 0), # maintains vectorized call of pred_ht()
                  pht = pred_ht(dbh, ba=ba.plot, bal, qmd, byi, 
                                a0, a1, b, c, g1, g2)) %>%
    dplyr::select(dplyr::all_of(tree.data.names), pht)
  
  
  tree
}


##### Height to crown base prediction ####  
  
# Height to crown base species parameters
  hcb.pred.parm = dplyr::tribble(
    ~type,   ~species,  ~b0,     ~b1,      ~b2,     ~b3,     ~b4,     ~b5,
    'base',  'AK',      0.1684,  1.0146,  -0.376, -0.0078, -0.3734,   0,
    'site',  'AK',      0.1684,  1.0146,  -0.376, -0.0078, -0.3734,  -0.221)


#' Predict height to crown base
#' 
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param ht Numeric: Total tree height (m)
#' @param bal Numeric: Plot basal area larger trees (m^2 per ac)
#' @param ba Numeric: Plot basal area (m^2 per ac)
#' @param byi Biomass Yield Index (Mg per ha). If NULL or 0, uses basic model
#' @param b0-b4 Numeric: Species parameters
#' @return Numeric: Predicted height to crown base (m)
#'
  pred_hcb = function(dbh, ht, bal, ba, byi, 
                      b0, b1, b2, b3, b4, b5) {
    
    eta = b0 + 
      b1 * sqrt(ht/100) + 
      b2 * log(pmax(ht/pmax(dbh, 0.1), 0.5)) + 
      b3 * sqrt(bal*ba + 1) + 
      b4 * log(ba + 1) + 
      b5 * log(pmax(byi, 1) / 100)
    
    # Calculate height to crown base
    hcb = ht / (1 + exp(-eta))
    
    # constrain between 0 and 95% of height
    hcb = pmin(pmax(hcb, 
                    0), 
               0.95 * ht) 
    
    hcb
  }
 
   
#' Wrapper to calculate height to crown base for tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data
#' @param hcb.spp.parms Dataframe: Species parameters (default hcb.pred.spp)
#' @param byi Numeric: Biomass Yield Index (Mg per ha)
#' @return Dataframe: Tree data with hcb column added
#'
  calc_hcb = function(tree.data, plot.data, 
                      hcb.pred.parm.df = hcb.pred.parm,
                      byi = stand$byi) {
    
    tree.data.names= colnames(tree.data)  
    
    hcb.parm.type = ifelse(byi %in% c(NA, 0),
                          'base',
                          'site')
    
    hcb.parm = hcb.pred.parm.df %>% 
      filter(type==hcb.parm.type)
    
    tree=tree.data %>% 
      dplyr::left_join(plot.data %>% 
                         dplyr::select(plot, ba.plot), 
                       by = 'plot') %>%
       # Match parameter estimates on species, Koa is currently the default
      dplyr::mutate(idx = match(sp, hcb.parm$species, nomatch = match('AK', hcb.parm$species)),
                    b0 = hcb.parm$b0[idx],
                    b1 = hcb.parm$b1[idx],
                    b2 = hcb.parm$b2[idx], 
                    b3 = hcb.parm$b3[idx],
                    b4 = hcb.parm$b4[idx], 
                    b5 = hcb.parm$b5[idx],
                    byi = coalesce(byi, 0), 
                    phcb = pred_hcb(dbh, ht, bal, ba=ba.plot, byi, 
                                    b0, b1, b2, b3, b4, b5)) %>%
      dplyr::select(dplyr::all_of(tree.data.names), phcb)
    
    tree
  }
  

#### Diameter increment ####

# Diameter increment parameters
  ddbh.parm = dplyr::tribble(
    ~type,    ~species,  ~b0,        ~b1,         ~b2,         ~b3,         ~b4,        ~b5,         ~b6,        ~b7,        ~b8,      
    'base',    'AK',   -2.4704737,  0.2072221,  -0.0159616,  -0.0016893,  -0.2972574,  -0.4470330,  -0.0158403,  0.0188938,        0,  
    'site',    'AK',   -2.4704737,  0.2072221,  -0.0159616,  -0.0016893,  -0.2972574,  -0.4470330,  -0.0158403,  0.0188938,   0.4530166)

    
#' Calculate annual diameter increment 
#' 
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param bal Numeric: Plot basal area larger trees (m^2 per ha)
#' @param ba Numeric: Plot basal area (m^2 per ha)
#' @param cr Numeric: Live crown ratio (0-1)
#' @param byi Numeric: Biomass Yield Index (Mg per ha). If NULL or 0, uses base model parameters
#' @param planted Boolean: Origin indicator (1 = planted, 0 = natural)
#' @param b0-b8 Numeric: Species parameters
#' @return Numeric: Diameter increment (cm)
ddbh = function(dbh, bal, ba, cr, byi, planted, 
                b0, b1, b2, b3, b4, b5, b6, b7, b8) {
  
  cf = 1.026   # Duan (1983) smearing correction factor
  
  # diameter increment
  ddbh = exp(b0 + b1*log(dbh+1) + 
               b2 * dbh + 
               b3 * bal^2 / log(dbh + 5) + 
               b4 * log(bal + 1) +
               b5 * log(pmax(cr, 0.01)) + 
               b6 * sqrt(pmax(ba * dbh, 0)) +
               b7 * planted * pmin(dbh, 40) + 
               b8 * log(pmax(byi, 1))) *cf 
  
  # constrain to between 0 and 4 cm
  ddbh = pmin(pmax(ddbh, 0), 4)
  
  ddbh
}


#' Wrapper function to calculate diameter increment with modifiers and constraints
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data  
#' @param use.cap.dbh Logical: Apply maximum DBH constraint (default ops$use.cap.dbh)
#' @param ddbh.parm.df Dataframe: Species parameter table for diameter increment
#' @param byi Numeric: Biomass Yield Index (Mg per ha)
#' @param planted Boolean: Origin indicator (1 = planted, 0 = natural)
#' @return Dataframe: Tree data with diameter increment calculations
calc_ddbh = function(tree.data, plot.data, 
                     use.cap.dbh = ops$use.cap.dbh, 
                     ddbh.parm.df = ddbh.parm,
                     byi = stand$byi, 
                     planted = stand$planted ) {
  
  # get tree list variable names
  tree.data.names= colnames(tree.data)   
  
  # filter ddbh parameter estimate to type base or climate
  ddbh.parm.type = ifelse(byi %in% c(NA, 0),
                        'base',
                        'site')
  
  ddbh.parm = ddbh.parm.df %>% 
    filter(type==ddbh.parm.type)

  # Calculate diameter increment 
  tree = tree.data %>%
    dplyr::left_join(plot.data %>% 
                       dplyr::select(plot, ba.plot), 
                     by = 'plot') %>%
    # Match parameter estimates on species, Koa is currently the default
    dplyr::mutate(idx = match(sp, ddbh.parm$species, nomatch = match('AK', ddbh.parm$species)),
                  b0 = ddbh.parm$b0[idx],
                  b1 = ddbh.parm$b1[idx],
                  b2 = ddbh.parm$b2[idx], 
                  b3 = ddbh.parm$b3[idx],
                  b4 = ddbh.parm$b4[idx], 
                  b5 = ddbh.parm$b5[idx],
                  b6 = ddbh.parm$b6[idx], 
                  b7 = ddbh.parm$b7[idx],
                  b8 = ddbh.parm$b8[idx],
                  byi = dplyr::coalesce(byi, 0),
                  planted = dplyr::coalesce(planted, 0),
                  # 
                  ddbh = dplyr::case_when(ht<1.3716 ~0,
                                          TRUE ~ddbh(dbh, bal, ba=ba.plot, cr, 
                                                     byi, planted, 
                                                     b0, b1, b2, b3, b4, b5, b6, b7, b8)),
                  # apply dbh increment multiplier
                  ddbh = ddbh * ddbh.mult)
 
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
  ~type,   ~species,  ~b0,        ~b1,       ~b2,        ~b3,        ~b4,       ~b5,        ~b6,       ~b7,     ~b8,    
  'base',  'AK',    -3.382162,  0.272454,  -0.105319,   -0.000829, -0.071718,  -1.483889,  0.033035,  0.017887,  0,   
  'site',  'AK',    -3.382162,  0.272454,  -0.105319,   -0.000829, -0.071718,  -1.483889,  0.033035,  0.017887,  0.433224)


#' Calculate height increment
#' 
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param ht Numeric: Tree height (m)
#' @param bal Numeric: Plot basal area larger (m^2 per ha)
#' @param ba Numeric: Plot basal area (m^2 per ha)
#' @param cr Numeric: Live crown ratio (0-1)
#' @param byi Numeric: Biomass Yield Index (Mg per ha). If NULL or 0, uses base model parameters
#' @param planted Boolean: Origin indicator (1 = planted, 0 = natural)
#' @param b0-b8 Numeric: Species parameters
#' @return Numeric: Height increment (m)
dht = function(dbh, ht, bal, ba, cr, byi, planted,
               b0, b1, b2, b3, b4, b5, b6, b7, b8) {
  
  
  cf = 1.030   # Duan (1983) smearing correction factor
  
  
  dht = exp(b0 + b1 * log(ht+1) + 
              b2 * ht + 
              b3 * bal^2 / log(ht + 5) + 
              b4 * log(bal + 1) +
              b5 * log(pmax(cr, 0.01)) + 
              b6 * sqrt(pmax(ba * ht, 0)) +
              b7 * sqrt(planted * pmin(ht, 20)) + 
              b8 * log(pmax(byi, 1))) *cf
  
  # constrain to between 0 and 2 m
  dht = pmin(pmax(dht, 0), 2)
  
  dht
}


#' Wrapper function to calculate height increment with modifiers and constraints
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data  
#' @param use.cap.ht Logical: Apply maximum total tree height constraint (default ops$use.cap.ht)
#' @param dht.parm.df Dataframe: Species parameter table for height increment (dht.parm)
#' @param byi Numeric: Biomass Yield Index (Mg per ha)
#' @param planted Boolean: Origin indicator (1 = planted, 0 = natural)
#' @return Dataframe: Tree data with height increment calculations
calc_dht = function(tree.data, 
                    plot.data,  
                    use.cap.ht = ops$use.cap.ht, 
                    dht.parm.df = dht.parm,
                    byi=stand$byi, 
                    planted=stand$planted ) {
  
  # get tree list variable names
  tree.data.names= colnames(tree.data)   
  
  # filter dht parameter estimate to type base or climate
  dht.parm.type = ifelse(byi %in% c(NA, 0),
                         'base',
                         'site')
  
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
                  byi = dplyr::coalesce(byi, 0),
                  planted = dplyr::coalesce(planted, 0),
                  # 
                  dht = dht(dbh, ht, bal, ba, cr, byi,
                               planted, b0, b1, b2, b3, b4, b5, b6, b7, b8),
      #apply ht increment multiplier
      dht = dht * dht.mult)
 
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
  ~type,  ~species,  ~b0,     ~b1,    ~b2,     ~b3,     ~b4,     ~b5,     ~b6,    ~b7,
  'base',  'AK',     18.133,  0.199,  -5.718,   7.640,  15.678,  -3.396,   0,       0,
  'site',  'AK',     18.133,  0.199,  -5.718,   7.640,  15.678,  -3.396,  3.039,  -25.102)

#' Calculate tree survival probability
#' 
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param ht Numeric: Tree height (ft)
#' @param cr Numeric: Live crown ratio (0-1)
#' @param r.ht Numeric: Relative height ht / max plot ht
#' @param byi Numeric: Biomass Yield Index (Mg per ha). 
#' @param b0-b7 Numeric: Species parameters
#' @return Numeric: Tree survival probability (proportion 0-1)
surv_prob = function(dbh, ht, cr, r.ht, byi,
                     b0, b1, b2, b3, b4, b5, b6, b7) {
  
  
  # constrain input height and diameter
  ht = pmax(ht,  0.1)
  dbh = pmax(dbh, 0.1)
 
  
  surv = exp(-exp((b0 + b1*ht + 
                      b2* log(ht) + 
                      b3* r.ht + 
                      b4* log(pmax(pmin(cr, 0.99), 0.01)) +
                      b5* log(ht / dbh ) + 
                      b6* log(pmax(byi, 1) / 100) + 
                      b7* (byi / 1000))))
  
  # constrain to between 0 and 1 
  surv =  pmin(pmax(surv, 0), 1)
  
  surv
}


#' Calculate mortality and modifiers for tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data
#' @param surv.parm.df Dataframe: Species parameter table for survival (default surv.parm) 
#' @param byi Numeric: Biomass Yield Index (Mg per ha). 
#' @return Dataframe: Tree data with mortality calculations
calc_mortality = function(tree.data, plot.data,
                          surv.parm.df = surv.parm,
                          byi = stand$byi) {
  
  # get tree list variable names
  tree.data.names= colnames(tree.data)   
  
  # filter survival parameter estimate to type base or BYI site
  surv.parm.type =  ifelse(byi %in% c(NA, 0),
                           'base',
                           'site')


  surv.parm = surv.parm.df %>%
    filter(type==surv.parm.type)
  
  
 
  # Join tree and plot summary data 
  tree = tree.data %>%
    dplyr::left_join(plot.data %>% 
                       dplyr::select(plot, ba.plot, htmax), 
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
                  byi = dplyr::coalesce(byi, 0),
                  r.ht = ht / htmax, 
                  # r.ht = pmax(ht / htmax, 0.65), 
                  surv = surv_prob(dbh, ht, cr, r.ht, byi,
                                   b0, b1, b2, b3, b4, b5, b6, b7),
                  dexpf = expf*(1-surv),
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
#' @return Dataframe: Species calibration factors (ddbh multiplier, dht multiplier, mortality multiplier, max dbh, max total height)

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
                                              max.dbh, 999) * in.to.cm, # metric conversion 
                      max.height=dplyr::coalesce(maxht.fvs, 
                                                 max.height, 999) * ft.to.m,
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
    
    # retain records in the projections for accurate plot values and change species to OT
    
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
                  dbh  = dbh  * in.to.cm, # metric conversion
                  ht   = ht   * ft.to.m,
                  #hcb = ht-cr*ht,
                  expf = expf * dplyr::coalesce(num.plots, 1) * ha.to.ac) %>%  # each plot as "stand"
    
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
                                 'cr', 'expf', 
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
                       elev = 0, 
                       byi = 0, 
                       planted = 0){
    
    stand=data.frame(stand.id = stand.id,
                     elev = elev,
                     byi = pmin(coalesce(byi, 0), 600), # BYI capped at 600 
                     planted = coalesce(planted, 0))
    
    stand
    
  }

#### Prepare output tree list ####
#### for FVS fvsSetTreeAttrs()
#' Create FVS return tree list
#' 
#' @param tree.data Dataframe: tree list output from model, fields:  year; dbh; ht; expf; cr
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
                  dbh, # metric
                  ht, # metric
                  expf, # plot level tph 
                  dg, # customary units
                  htg, # customary units
                  mort, # stand level TPA
                  cr) %>% 
    dplyr::anti_join(tree.list, 
                     by='tree')
  # metric to customary option
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
    dplyr::mutate(dg=(dbh-dbh.fvs)*cm.to.in, # diameter growth to inches
                  htg=(ht-ht.fvs)*m.to.ft,  # height growth to feet
                  # set the crown ratio sign to negative so that FVS doesn't change them. 
                  cratio = round(cr*-100, 1), # 
                  mort=(expf.fvs-expf)*ac.to.ha,   # mortality trees per hectare
                  mort=mort/dplyr::coalesce(num.plots, 1), # calculate stand level mortality TPA
                  mort = ifelse(expf.fvs*ac.to.ha - mort < 0.01, 
                                expf.fvs*ac.to.ha/dplyr::coalesce(num.plots, 1), 
                                mort)) %>%  # if TPA <0.01 then 0
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
    dplyr::summarise(tph.plot = sum(expf, na.rm = TRUE),
                     ba.plot = sum(ba, na.rm = TRUE),
                     # max tree number for ingrowth
                     max.tree.id = max(tree, na.rm = TRUE), 
                     # max plot height
                     htmax=max(ht, na.rm = TRUE),
                     .groups = 'drop') %>%
    # QMD
    dplyr::mutate(qmd = sqrt(ba.plot / (0.00007854 * tph.plot)))
  
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
  ## * make_ops()
  ## * make_stand() 
  ## * make_tree()
  ## * check tree list variables
  ### ----
  
  
##### Add tree attributes ####
  tree = tree %>% 
        # basal area (plot level)
    dplyr::mutate(ba = (dbh^2*0.00007854)*expf) %>% 
      # Calculate BAL
    calc_bal()

  
##### Plot attributes ####
    # Calculate plot summary
  plot.smry = tree %>% 
    calc_plot_summary()

  # SDI - height and diameter increment      
    # sdi = expf *(qmd / 25)^1.6
  #  SDI_max = 500 (estimated from upper boundary of FIA koa SDI distribution)
    
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
           ht= dplyr::case_when(ht %in% c(NA, 0)| ht>50 ~pht,
                        TRUE ~ ht),
           hcb = ht-cr*ht) %>% 
           #predicted height to crown base (returns phcb)
    calc_hcb(plot.data = plot.smry) %>% 
           #use predicted height to crown base if hcb is missing or invalid
    dplyr::mutate(hcb= dplyr::case_when(is.na(hcb) | hcb>ht  ~phcb, 
                                TRUE ~hcb),
                  cr = 1-(hcb/ht))
  
# Compute plot-level heights
  
  # Plot top height (for 100 tph)
  plot.topht = tree %>% 
    calc_topht()
   
  # Add to plot summary
  plot.smry=plot.smry %>% 
    dplyr::left_join(plot.topht,
                     by='plot') 
 
##### Diameter increment ####
 
  tree = tree %>%
    calc_ddbh(plot.data = plot.smry)
 
##### Height increment ####  
 
  tree = tree %>%
    calc_dht(plot.data = plot.smry)



#### Ingrowth ####
  
  
#### Mortality ####

  tree = tree %>% 
    calc_mortality(plot.data=plot.smry)
  

#### Update tree values- t+1 ####
  tree=tree %>% 
    dplyr::mutate(year= year+1,
                  dbh= dbh+ dplyr::coalesce(ddbh, 0),
                  ht= ht+ dplyr::coalesce(dht, 0),
                  expf= dplyr::coalesce(expf, 0) - dplyr::coalesce(dexpf, 0),
                  expf= ifelse(expf< 0.00001, 0.00001, expf)) 

#### Crown recession ####
 # calculate t+1 height to crown base  
  tree=tree %>% 
    calc_bal()
  
  # Calculate plot summary
  plot.smry = tree %>% 
    calc_plot_summary()
  
  # crown ratio change limited to 2.5% annual (FIA data median annual change -2.5%)  
  tree=tree %>% 
    calc_hcb(plot.data = plot.smry) %>% 
    dplyr::mutate(pcr= 1-(hcb/ht), 
                  cr= dplyr::case_when((cr-pcr)/cr>0.025 ~cr*0.975, 
                                       (cr-pcr)/cr<(-0.025) ~cr*1.025, 
                                       TRUE ~pcr))
  
#### Output ####      
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
       
