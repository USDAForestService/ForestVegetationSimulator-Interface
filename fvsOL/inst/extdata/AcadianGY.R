# $Id: AcadianGY.R 3968 2022-04-28 10:36:05Z nickcrookston $
################################################################################
# v12.5.0.r 
#
# Acadian Variant of the Forest Vegetation Simulator (FVS-ACD)                                                                                            #
# Developed by Aaron Weiskittel, University of Maine, School of Forest Resources
# aaron.weiskittel@maine.edu
# 
#
#
################################################################################

library(dplyr) # needed arrange, mutate, left_join, tibble, select, group_by, summarise, ungroup, case_when, all_of
library(purrr) # needed for pmap_*

AcadianVersionTag = "AcadianV12.5.0"

##############################
#### major update summary ####
####

# 12.5.0
  # significant rewrite of codebase   
  # standardized variable names
  # removed variables not used in calculations
  # SPP.func() deprecated replaced with tribble (table) sp.attr
  # removed cyclen references; cycle length is always 1 and FVS users can select reporting frequency
  # removed plyr dependency 
  # modularized calculations- each model component has a calc_*() function that calls the component and modifier functions
  # added validation functions for tree, stand and ops dataframes
 
# 12.3.5 
  # Retain rrevious mortality equations; discontinued experimental use of Chen et al equations
  # changed calculation of cubic foot volume from tcuft to mcuft to align with FVS output update (customRun file)

# 12.3.2
  # stand basal area increment calculation modified to better handle cases where both basal area and relative density have high values
  # removed SM from dDBH.HW.mod() and tsurv_hw_mod()
  # removed library(nlme) dependency
  # new or revised functions related to FVS tree list read and write 
    # make_acd_tree(); validate_acd_tree_spp(); validate_acd_tree_status(); make_fvs_tree()
  # new function make_fvs_calib() to create table of multipliers and tree size limits from FVS species attributes and tree size cap object;
    # associated changes in diameter and height increment calculations in AcadianGYOneStand()
  # removed cap height growth using linear interpolation (dHTmult)
  # changed customRun volume logic from mcuft to tcuft 
  # temporarily disabled height and crown ratio imputation due to problems with FVS stop point 7  


# 12.3.1
  # dBA_plot_fun() modified to incorporate new stand basal area increment equation from Aaron Weiskittel 
  # new functions: make_acd_tree(); make_fvs_tree(); make_fvs_regen() to facilitate reading and writing FVS tree lists

# 12.3.0 
  # Implemented plot basal area constraint from Chen et al (in review) applied to tree diameter increment
    # new functions: dBA_plot_fun(); calc_plot_ba()
  # AcadianGYOneStand function 
    # Created code to catch tree records with DBH=0 and add 0.01
    # Limit model execution to species defined in height and diameter increment parameters; others assigned species 99 

# 12.2.0
  # updated mortality calculation to Chen et al (in review) and updated mortality modifier function names
    # new functions: mort_plot_prob(); mort_plot_ba(); surv_tree(); calc_mortality

# 12.1.5
  # ING.TreeList function
    # Modified function in response to ingrowth calculation error; stand id dependency
  # AcadianGYOneStand function 
    # Run at one year cycle length only

# 12.1.4
  # Fixed diameter hardwood form and risk modifier 

# 12.1.3
  # AcadianGYOneStand function
    # added default RD
    # include form and risk 
    # temp calculations include ELEV
    # updated SDI/RD calculation
 
# 12.1.1
  # Weiskittel edits 12/1/2020
  # Rice edits integrate new dHt and dDBH functions with FVS version 12/21/2020
  # removed sort.data.frame <- function(form,dat) substitute dplyr::arrange

# 11.1 Aaron edits 6/6/2019 and several edits
  # from Ben Rice and Jereme Frank to resolve errors
  # Crookston edits for FVS 9/20/2020

##############################



#### Species functional attributes ####
sp.attr = dplyr::tribble(
  ~species,  ~sp.type, ~sg, ~wd, ~shade, ~drought, ~waterlog, ~sp.grp.ingrowth,
  #species sp.type  sg     wd      shade   drought waterlog  sp.grp.ingrowth
  'AB',  'HW',  0.64,  0.56,    4.75,  1.5,   1.5,      'AB',    # AB=American beech
  'AS',  'HW',  0.57,  0.51,    2.84,  2.74,  3.02,     'OH',    # AS=ash                 
  'BA',  'HW',  0.5,   0.45,    2.96,  2,     3.5,      'OH',    # BA=black ash
  'BC',  'HW',  0.5,   0.47,    2.46,  3.02,  1.06,     'OH',    # BC=black cherry
  'BF',  'SW',  0.35,  0.33,    5.01,  1,     2,        'BF',    # BF=balsam fir
  'BP',  'HW',  0.34,  0.31,    1.27,  1.77,  2.63,     'OH',    # BP=balsam poplar
  'BS',  'SW',  0.46,  0.38,    4.08,  2.0,   2.0,      'SPR',   # BS=black spruce
  'BT',  'HW',  0.39,  0.36,    1.21,  2.5,   2,        'OH',    # BT=bigtooth aspen
  'EC',  'HW',  0.4,   0.37,    1.76,  1.57,  3.03,     'OH',    # EC=eastern cottonwood
  'EH',  'SW',  0.4,   0.38,    4.83,  1,     1.25,     'OS',    # EH=eastern hemlock
  'GA',  'HW',  0.56,  0.53,    3.11,  3.85,  2.98,     'OH',    # GA=green ash
  'GB',  'HW',  0.48,  0.45,    1.5,   2.34,  1,        'BCH',   # GB=gray birch
  'HH',  'HW',  0.78,  0.63,    4.58,  3.25,  1.07,     'OH',    # HH=eastern hophornbeam
  'JP',  'SW',  0.43,  0.4,     1.36,  4,     1,        'OS',    # JP=jack pine
  'NS',  'SW',  0.43,  0.37023, 4.45,  1.75,  1.22,     'OS',    # NS=Norway spruce
  'OH',  'HW',  0.5121, 0,      2.29,  0,     0,        'OH',    # OH=other hardwoods
  'OS',  'SW',  0.445, 0,       2.27,  0,     0,        'OS',    # OS=other softwoods
  'PB',  'HW',  0.55,  0.48,    1.54,  2.02,  1.25,     'BCH',   # PB=paper birch
  'PC',  'HW',  0.38,  0.36,    2.26,  0,     0,        'OH',    # PC=pin cherry  
  'PR',  'HW',  0.38,  0.36,    2.26,  0,     0,        'OH',    # PR=pin cherry 
  'QA',  'HW',  0.38,  0.35,    1.21,  1.77,  1.77,     'QA',    # QA=quaking aspen
  'RB',  'HW',  0.62,  0.49,    1.45,  1.53,  2.85,     'BCH',   # RB=river birch
  'RM',  'HW',  0.54,  0.49,    3.44,  1.84,  3.08,     'RM',    # RM=red maple
  'RP',  'SW',  0.46,  0.41,    1.89,  3,     1,        'OS',    # RP=red pine 
  'RN',  'SW',  0.46,  0.41,    1.89,  3,     1,        'OS',    # RN=red pine
  'RO',  'HW',  0.63,  0.56,    2.75,  2.88,  1.12,     'OH',    # RO=red oak
  'RS',  'SW',  0.4,   0.37,    4.39,  2.5,   2,        'SPR',   # RS=red spruce
  'SB',  'HW',  0.65,  0.6,     2.58,  3,     1,        'BCH',   # SB=Sweet birch
  'SM',  'HW',  0.63,  0.56,    4.76,  2.25,  1.09,     'SM',    # SM=sugar maple
  'ST',  'HW',  0.46,  0.44,    3.56,  2,     1,        'OH',    # ST=striped maple
  'TA',  'SW',  0.53,  0.49,    0.98,  2,     3,        'OS',    # TA=larch/tamarack
  'WA',  'HW',  0.6,   0.55,    2.46,  2.38,  2.59,     'OH',    # WA=white ash             
  'WC',  'SW',  0.31,  0.29,    3.45,  2.71,  1.46,     'WC',    # WC=northern white cedar
  'WP',  'SW',  0.35,  0.34,    3.21,  2.29,  1.03,     'WP',    # WP=white pine
  'WS',  'SW',  0.4,   0.33,    4.15,  2.88,  1.02,     'SPR',   # WS=white spruce
  'YB',  'HW',  0.62,  0.55,    3.17,  3,     2,        'BCH',   # YB=yellow birch
  '99',  'HW',  0.3,   0.3,     3.0,   0,     0,        'OH')    # other


#### Crown prediction ####

##### Species parameters ###
# Maximum crown width parameters
mcw.spp = tibble::tribble(
  ~species, ~a1, ~a2,
  'BF', 1.37, 0.572,
  'BS', 0.535, 0.742,
  'EH', 2.44, 0.408,
  'WP', 1.24, 0.585,
  'NC', 1.63, 0.436,
  'RS', 1.80, 0.461,
  'WS', 1.50, 0.496,
  'AB', 2.93, 0.434,
  'GB', 2.24, 0.382,
  'RB', 2.24, 0.382,
  'RO', 4.08, 0.310,
  'PB', 1.48, 0.623,
  'QA', 1.31, 0.586,
  'RM', 2.17, 0.491,
  'SM', 3.31, 0.356,
  'YB', 4.04, 0.308,
  'OH', 4.04, 0.308,
  'OS', 1.597128571, 0.513957143,
  '99', 2.24262, 0.462653333)

# Largest crown width parameters  
lcw.spp = tibble::tribble(
  ~species, ~b1, ~b2,
  'BF', 1.49, 0.105,
  'BS', 1, 0.174,
  'EH', 1.90, -0.057,
  'WP', 1, 0.147,
  'NC', 2.19, -0.080,
  'RS', 4.33, -0.264,
  'WS', 2.09, -0.069,
  'AB', 1, 0.194,
  'GB', 3.10, -0.214,
  'RB', 3.10, -0.214,
  'RO', 4.10, -0.272,
  'PB', 2.10, -0.035,
  'QA', 2.65, 0.157,
  'RM', 2.63, -0.132,
  'SM', 1, 0.161,
  'YB', 4.23, -0.264,
  'OH', 2.65, 0.157,
  'OS', 2.3276, 0.027842857,
  '99', 2.79282, -0.090113333)

##### maximum crown width ####
# Russell, MB; Weiskittel, AR. 2011. Maximum and largest crown width equations 
# for 15 tree species in Maine. Northern Journal of Applied Forestry, 28(2): 
#   84-91. doi: 10.1093/njaf/28.2.84


#' Maximum crown width
#' 
#' @param sp Character: Species code (FVS alpha)
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param a1 Numeric: Species a1 parameter
#' @param a2 Numeric: Species a2 parameter
#' @return Numeric: Maximum crown width (m)
#'
mcw = function(sp, dbh, a1, a2) {
  
  # Calculate maximum crown width
  mcw = a1 * (dbh^a2)
  
  mcw
}

##### largest crown width ####
# Russell, MB; Weiskittel, AR. 2011. Maximum and largest crown width equations 
# for 15 tree species in Maine. Northern Journal of Applied Forestry, 28(2): 
#   84-91. doi: 10.1093/njaf/28.2.84

#' largest crown width
#' 
#' @param sp Character: Species code (FVS alpha)
#' @param mcw Numeric: Maximum crown width (m)
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param b1 Numeric: Species b1 parameter
#' @param b2 Numeric: Species b2 parameter
#' @return Numeric: Largest crown width (m)
#'
lcw = function(sp, mcw, dbh, b1, b2) {
  
  # Calculate largest crown width
  lcw = mcw / (b1 * (dbh^b2))
  
  lcw
}

##### Calculate crown variables ####
#' Calculate crown variables 
#' 
#' @param tree.data Dataframe: Tree list
#' @param mcw.parm Dataframe: MCW parameter table (default mcw.spp)
#' @param lcw.parm Dataframe: LCW parameter table (default lcw.spp)
#' @return Dataframe: Tree data with added crown columns (mcw, lcw, mca, ccfl)
#'
calc_crown = function(tree.data, mcw.parm = mcw.spp, lcw.parm = lcw.spp) {
  
  # tree.data required.cols = c('sp', 'dbh', 'expf', 'plot')
 
  # Calculate crown dimensions
  tree.data=tree.data %>%
    dplyr::mutate(idx.mcw = match(sp, mcw.parm$species, nomatch = match('99', mcw.parm$species)),
           a1 = mcw.parm$a1[idx.mcw],
           a2 = mcw.parm$a2[idx.mcw],
           mcw = mcw(sp, dbh, a1, a2),
           
           idx.lcw = match(sp, lcw.parm$species, nomatch = match('99', lcw.parm$species)),
           b1 = lcw.parm$b1[idx.lcw],
           b2 = lcw.parm$b2[idx.lcw],
           lcw = lcw(sp, mcw, dbh, b1, b2),
           
           mca = 100 * ((pi * (mcw / 2)^2) / 10000) * expf) %>%
    dplyr::select(-idx.mcw, -a1, -a2, -idx.lcw, -b1, -b2) %>%
    dplyr::group_by(plot) %>%
    dplyr::mutate(ccfl = cumsum(mca) - mca) %>%
    dplyr::ungroup()
  
  tree.data
}

#### Height prediction ####

##### Total height ####
### Total height prediction function (updated 8/31/2012) using species as a random effect

# Height prediction species parameters
ht.pred.spp = dplyr::tribble(
  ~species, ~c0, ~c3,
  "AB", -1.63260226433876, -0.123848276720533,
  "AE", -0.692010776894357, 0.0346080772461358,
  "AH", -5.98009416964362, -0.032783189788012,
  "AI", -6.44978562263189, -0.0984022226851643,
  "AP", -12.0735361325049, -0.475976304087567,
  "AS", 6.69760483331092, -0.125318550191217,
  "BA", -1.61716890163543, -0.141587177559468,
  "BC", -4.52724204655813, -0.172605143041673,
  "BE", -1.60563943164767, -0.424565045666305,
  "BF", 1.77471065080046, 0.1571978021787,
  "BL", -9.82751389751524, -0.292624067773788,
  "BN", 0.861243905640667, -0.103226577993538,
  "BO", 1.17024253111731, -0.0431150821737857,
  "BP", 2.52163661595498, -0.0633568443480465,
  "BR", 5.44303876725562, 0.354363882079203,
  "BS", 3.88571664605334, 0.1886808269048,
  "BT", 5.2832906396451, -0.0670620453873463,
  "BW", 2.52499880080404, 0.153925181183304,
  "EC", 7.0881673102164, 0.182126907461261,
  "EH", 0.0643545746867161, 0.260671290553969,
  "EL", -0.460173779709119, 0.194209222023289,
  "GA", -1.47263156202317, -0.0743884734979349,
  "GB", -1.03938349314791, -0.238717166776341,
  "HH", -2.45397316779551, -0.17636502944365,
  "HK", -0.139225685811506, -0.107092112450714,
  "HT", -0.498373011862981, 0.000508524335021695,
  "JP", 12.2041796567474, 0.507127061137884,
  "NC", -0.154777524407996, 0.0506677142901254,
  "NS", 10.7572546403292, 0.761510842024224,
  "OH", -0.152274609199728, -0.0773943323672784,
  "OP", -1.25201364651662, 0.19704750471505,
  "OS", 6.64418468070433, 0.154974733190601,
  "PB", 2.85568786741337, -0.053133050063968,
  "PI", 6.11926846598162, 0.396180643433203,
  "PL", -12.5774578312843, -0.354402924932074,
  "PP", -2.03524755338192, 0.0284495830511636,
  "PR", -5.27943940700261, -0.276675997378359,
  "PY", 2.39961434182412, -0.0302798406740612,
  "QA", 5.28547878831447, -0.0166932060459991,
  "RC", -13.3554875880232, -0.364956123989416,
  "RL", -13.464860796814, -0.373319415146871,
  "RM", 1.13361861116141, -0.124923006598654,
  "RN", 2.35424925615196, 0.4332474439509,
  "RO", 0.567158528857343, 5.49241830858304E-05,
  "RS", 3.28913339723299, 0.197832299388656,
  "SB", 5.0591881231944, 0.263270570106115,
  "SC", -1.77707771556552, 0.272984904670842,
  "SE", -1.20436304154548, -0.217453987421684,
  "SH", 3.42398432816088, 0.00852401379505827,
  "SM", 1.83135273162116, -0.1509017085778,
  "SO", -0.337608194904877, 0.00266584203067429,
  "ST", -4.21455499968947, -0.158534452384565,
  "SV", -1.66795214963112, -0.180378852473763,
  "SW", 1.3081369384269, 0.031660020193251,
  "TA", 3.03898203229266, -0.070139469703331,
  "WA", 1.58571626982993, -0.152599799179656,
  "WC", -2.63796730677436, 0.157097825004126,
  "WO", -0.179572014160004, 0.050014945223132,
  "WP", 1.89177965275704, 0.217933074706457,
  "WS", 2.83053645408208, 0.284913386127066,
  "YB", -1.13450171811265, -0.179629568670318)

# Rijal, B; Weiskittel, AR; Kershaw, JA. 2012a. Development of regional height to 
# diameter equations for 15 tree species in the North American Acadian Region. Forestry: 
# An International Journal of Forest Research 85(3): 379-390.  doi:10.1093/forestry/cps036


#' Predict total height 
#' 
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param csi Numeric: Climate site index (m)
#' @param ccf Numeric: Crown competition factor
#' @param bal Numeric: Basal area larger (m^2 per ha)
#' @param c0.spp Numeric: Species c0 parameter
#' @param c3.spp Numeric: Species c3 parameter
#' @return Numeric: Predicted height (m)
#'
pred_ht = function(dbh, csi, ccf, bal, c0.spp, c3.spp) {
  
  # Fixed parameters
  c0 = 12.44847305
  c1 = 0.801705832
  c2 = 0.043617034
  c3 = 1.048674338
  c4 = 0.011483716
  c5 = -0.007550999
  
  # Calculate total height
  ht = (1.37 + ((c0 + c0.spp) + csi^c1) * 
          (1 - exp(-c2 * dbh))^(c3 + c3.spp + c4 * log(ccf + 1) + c5 * bal))
  
  ht
}

#' Wrapper to calculate predicted heights for tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data
#' @param csi Numeric: Climate site index (m)
#' @param ht.spp.parms Dataframe: Species parameters (default ht.pred.spp)
#' @return Dataframe: Tree data with ht column added
#'
calc_ht = function(tree.data, plot.data, csi = stand$csi, 
                   ht.spp.parms = ht.pred.spp) {
  
  tree.data.names= colnames(tree.data)   
  
  tree = tree.data %>% 
    dplyr::left_join(plot.data %>% 
                       dplyr::select(plot, ccf), 
                     by = 'plot') %>%
    dplyr::mutate(idx = match(sp, ht.spp.parms$species, nomatch = match('OH', ht.spp.parms$species)),
                  c0.spp = ht.spp.parms$c0[idx],
                  c3.spp = ht.spp.parms$c3[idx],
                  pht = pred_ht(dbh, csi, ccf, bal, c0.spp, c3.spp)) %>%
    dplyr::select(dplyr::all_of(tree.data.names), pht)
  
  tree
}

##### Height to crown base ####  
  
### Height to crown base function updated 9/11/12 using species as random effect
  
 # Height to crown base species parameters
  hcb.pred.spp = dplyr::tribble(
    ~species, ~a0.spp,
    "AB", -0.218384027,
    "AI", 0.081713772,
    "AP", 0.177509753,
    "BA", 0.112652176,
    "BC", -0.198822609,
    "BF", 0.093585699,
    "BP", -0.136722421,
    "BR", 0.015598341,
    "BS", -0.227771445,
    "BT", 0.010040571,
    "EC", 0.507259999,
    "EH", 0.403937729,
    "GB", -0.181632328,
    "HK", 0.114597638,
    "JP", -0.270782917,
    "NC", -0.20514384,
    "NS", 0.955552766,
    "OH", -0.042894377,
    "OP", -0.718040335,
    "PB", -0.180946077,
    "PI", 0.548550524,
    "PR", -0.270887959,
    "QA", 0.060833757,
    "RM", -0.202478201,
    "RN", -0.150518738,
    "RO", -0.37021733,
    "RS", -0.121308265,
    "SC", 0.244204218,
    "SE", 0.24302215,
    "SM", -0.16382426,
    "TA", -0.304007105,
    "WA", -0.148338171,
    "WC", 0.419622685,
    "WO", 0.167213142,
    "WP", -0.146685117,
    "WS", 0.100275538,
    "YB", 0.003235064)

# Rijal, B; Weiskittel, AR; Kershaw, JA. 2012b. Development of height to crown base 
# models for thirteen tree species of the North American Acadian Region. Forestry 
# Chronicle 88: 60-73.  

 #' Predict height to crown base
 #' 
 #' @param dbh Numeric: Diameter at breast height (cm)
 #' @param ht Numeric: Total tree height (m)
 #' @param dhr Numeric: Height diameter ratio
 #' @param ccf Numeric: Crown competition factor
 #' @param bal Numeric: Basal area larger (m^2 per ha)
 #' @param a0.spp Numeric: Species parameter
 #' @return Numeric: Predicted height to crown base (m)
 #'
  pred_hcb = function(dbh, ht, dhr, ccf, bal, a0.spp) {
    
    # parameters
    a0 = 0.29070
    a1 = 0.00636
    a2 = -0.02288
    a3 = 0.08232
    a4 = -0.03086
    a5 = -0.01701
    
    # Calculate height to crown base
    hcb = ht / (1 + exp((a0 + a0.spp) + a1 * dbh + a2 * ht + a3 * dhr + 
                          a4 * log(ccf + 1) + a5 * (bal + 1)))
    
    hcb
  }
  
#' Wrapper to calculate height to crown base for tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data
#' @param hcb.spp.parms Dataframe: Species parameters (default hcb.pred.spp)
#' @return Dataframe: Tree data with hcb column added
#'
  calc_hcb = function(tree.data, plot.data, hcb.spp.parms = hcb.pred.spp) {
    
    tree.data.names= colnames(tree.data)  
    
    tree=tree.data %>% 
      dplyr::left_join(plot.data %>% 
                         dplyr::select(plot, ccf), 
                       by = 'plot') %>%
      dplyr::mutate(idx = match(sp, hcb.spp.parms$species, nomatch = match('OH', hcb.spp.parms$species)),
                    a0.spp = hcb.spp.parms$a0.spp[idx],
                    dhr = dbh / ht,
                    phcb = pred_hcb(dbh, ht, dhr, ccf, bal, a0.spp)) %>%
      dplyr::select(dplyr::all_of(tree.data.names), phcb)
    
    tree
  }
  
#### Form and risk prediction ####
  
  # Form prediction parameters 
  form.pred.spp = dplyr::tribble(
    ~species, ~stm.b0, ~stm.b1, ~stm.b2, ~lsw.b0, ~lsw.b1, ~lsw.b2, ~mst.b0, ~mst.b2, ~lf.b0, ~lf.b1, ~lf.b2,
    'RO',     -0.9491, 0.0174, -0.2826, -1.1143, -0.0322,  0.7910, -0.4110, -0.5009, -4.0677, 0.0322, 0.1139,
    'SM',     -0.9491, 0.0174,  0.7541, -1.1143, -0.0322, -0.2325, -0.4110, -1.1347, -4.0677, 0.0322, 0.6278, 
    'YB',     -0.9491, 0.0174, -0.0208, -1.1143, -0.0322,  0.2980, -0.4110, -0.7557, -4.0677, 0.0322, 1.0681,
    'RM',     -0.9491, 0.0174,  0.0000, -1.1143, -0.0322,  0.0000, -0.4110,  0.0000, -4.0677, 0.0322, 0.0000, 
    'OH',      0.0000, 0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000, 0.0000, 0.0000)
  # assumes typo in older versions
  # MST=exp(-0.4110- 0.5009* SPP.RO- 1.1347*SPP.RO- 0.7557*SPP.YB)/(1+
  #     exp(-0.4110- 0.5009*SPP.RO- 1.1347*SPP.RO- 0.7557*SPP.YB))
  # should 1.1347*SPP.RO have been 1.1347*SPP.SM
  
  # Risk prediction parameters  
  risk.pred.spp = dplyr::tribble(
    ~species, ~hr.b0, ~hr.b1, ~hr.b2, ~hr.b3,
    'RO', -0.6886, -0.0001, -0.0184,  -0.0393, 
    'SM', -0.6886, -0.0001, -0.1513,  -0.0164,
    'YB', -0.6886, -0.0001, -0.9851,   0.0196,
    'RM', -0.6886, -0.0001,  0.0000,   0.0000, 
    'OH',  0.0000,  0.0000,  0.0000,   0.0000)

##### Hardwood form ####  
  # Castle, ME; Weiskittel, AR; Wagner, RG; Ducey, MJ; Frank, JF; Pelletier, G. 2017. 
  # Variation in stem form and risk of four commercially important hardwood species 
  # in the Acadian forest: Implications for potential sawlog volume and tree classification 
  # systems. Canadian Journal of Forest Research. 47(11): 1457-1467.
  
  #Returns the probability of single straight stem (STM), 
  #extensive sweep and lean (LSW), multiple stems (MST),
  #significant fork on first 5 m (LF)
  
  #' Calculate form class probabilities for tree list
  #' 
  #' @param tree.data Dataframe: Tree list
  #' @param form.spp.parms Dataframe: Form parameter table (default form.pred.spp)
  #' @return Dataframe: Tree list with form probabilities (stm.prob, lsw.prob, mst.prob, lf.prob)
  calc_form_prob = function(tree.data, form.spp.parms = form.pred.spp) {
    
    # Filter applicable species and calculate probabilities
    tree.form = tree.data %>%
      dplyr::filter(sp %in% c('RO', 'SM', 'RM', 'YB')) %>%
      dplyr::left_join(form.spp.parms, 
                       by = c('sp' = 'species')) %>%
      dplyr::mutate(stm = exp(stm.b0 + stm.b1 * dbh + stm.b2) / 
                      (1 + exp(stm.b0 + stm.b1 * dbh + stm.b2)),
                    
                    lsw = exp(lsw.b0 + lsw.b1 * dbh + lsw.b2 ) / 
                      (1 + exp(lsw.b0 + lsw.b1 * dbh + lsw.b2 )),
                    
                    mst = exp(mst.b0 + mst.b2 ) / 
                      (1 + exp(mst.b0 + mst.b2)),
                    
                    lf = exp(lf.b0 + lf.b1 * dbh + lf.b2) / 
                      (1 + exp(lf.b0 + lf.b1 * dbh + lf.b2)),
                    # Normalize probabilities
                    total.prob = stm + lsw + mst + lf,
                    stm.prob = ifelse(total.prob > 0, stm / total.prob, NA_real_),
                    lsw.prob = ifelse(total.prob > 0, lsw / total.prob, NA_real_),
                    mst.prob = ifelse(total.prob > 0, mst / total.prob, NA_real_),
                    lf.prob = ifelse(total.prob > 0, lf / total.prob, NA_real_)) %>%
      dplyr::select(plot, tree, 
                    stm.prob, lsw.prob, mst.prob, lf.prob)
    
    tree = tree.data %>%
      dplyr::left_join(tree.form, 
                       by = c('plot', 'tree'))
    
    tree
  }
 
##### Hardwood risk ####
  # Castle, ME; Weiskittel, AR; Wagner, RG; Ducey, MJ; Frank, JF; Pelletier, G. 2017. 
  # Variation in stem form and risk of four commercially important hardwood species 
  # in the Acadian forest: Implications for potential sawlog volume and tree classification 
  # systems. Canadian Journal of Forest Research. 47(11): 1457-1467.
  
  #' Calculate hardwood risk probability for tree list
  #' 
  #' @param tree.data Dataframe: Tree list
  #' @param risk.spp.parms Dataframe: Risk parameter table (default risk.pred.spp)
  #' @return Dataframe: Tree data with high risk probability (risk.prob)
  calc_risk_prob = function(tree.data, risk.spp.parms = risk.pred.spp) {
    
    # Filter applicable species and calculate probability
    tree.risk = tree.data %>%
      dplyr::filter(sp %in% c('RO', 'SM', 'RM', 'YB')) %>%
      dplyr::left_join(risk.spp.parms, 
                       by = c('sp' = 'species')) %>%
      dplyr::mutate(hr = exp(hr.b0 + hr.b1 * dbh + hr.b2 + hr.b3 * dbh) /
                      (1+ exp(hr.b0 + hr.b1 * dbh + hr.b2 + hr.b3 * dbh)),
                    risk.prob = ifelse(hr >= 0, 
                                       hr, 
                                       NA_real_)) %>%
      dplyr::select(plot, tree, risk.prob)
    
    tree = tree.data %>%
      dplyr::left_join(tree.risk, 
                       by = c('plot', 'tree'))
    
    tree
  }
 
#### Diameter increment ####

# Species random effects parameters
ddbh.spp = tibble::tribble(
  ~species, ~ddbh.b0.spp, ~ddbh.b2.spp, ~ddbh.b3.spp, ~ddbh.b4.spp,
  
  #species|----------ddbh.b0.spp-|----------ddbh.b2.spp-|----------ddbh.b3.spp-|----------ddbh.b4.spp-|
  #-------|----------------------|----------------------|----------------------|----------------------|
  'AB',     -0.35229300349977,  -0.00320266087038405,   -0.280946863448189,    0.0224134774905559,
  'AE',      0.525848414142466,  0.013991275456242,      0.412199430293274,    0.133837343770701, 
  'AH',     -0.563196640320761, -0.01077075539195,       0.0670133879698583,   0.0411626357856011, 
  'AI',      0.0206561707667808,-0.000133492112204404,   0.00604358781482393, -0.000290675616258897, 
  'AL',     -1.39566897323315,  -0.000609093629818986,  -0.358275097112723,    0.0189886825701186,  # 	Alder spp. FIA:350; Mapped to NC in FVS-NE
  'AP',      0.274305700135926, -0.0102065952875863,     0.67051254435555,     0.0378917157417856, 
  'AW',      0.274056500403582,  0.00332679657906075,    0.11176485808573,    -0.00233348958863975, 
  'BA',     -0.438617439122267, -0.0022924817415948,    -0.226866504779093,    0.000647108990407897, 
  'BC',     -0.279503150796968, -0.0165193146291975,    -0.448777856671972,    0.129218944771525, 
  'BE',      0.757730775072063,  0.00184989739560478,   -0.145270886735823,   -0.079685465435807, 
  'BF',      0.218141150555071, -0.00643400053014112,   -0.0262015176388915,  -0.0646867462894489, 
  'BH',      0.0819668443898863, 0.000841343024792485,   0.0170397550545379,  -0.00382472350301523, 
  'BN',      0.0589621277531259, 0.016876722904582,     -0.512634541103034,   -0.0856035985165029, 
  'BO',      0.107935674233564,  0.0176564165685457,    -0.00544094948482233, -0.00616179384680487, 
  'BP',      0.606188793076239, -0.00977183436369207,    0.214468436803594,   -0.0164368644086746, 
  'BR',     -0.0664233470667451,-0.000131289490261372,  -0.0221300235777483,   0.00315273402640522, 
  'BS',     -0.276634107961408, -0.0223933899622142,    -0.0621958644556339,  -0.0340210607126585, 
  'BT',     -0.350923645391202,  0.00677617700565758,   -0.501315826210755,    0.000443327372257035, 
  'BW',      0.852838875626812, -0.00749469682441489,    0.563426492847638,    0.0865682041795566, 
  'CC',     -0.646060138282966,  0.0185634940525597,    -0.217332456059064,   -0.00468805063509814, # Chokecherry	Prunus virginiana L.; Mapped to PR in FVS-NE
  'DW',     -0.28835019262561,   0.00226777705843645,   -0.0727887365452367,   0.00620864120347165, 
  'EC',      1.58534419438753,   0.0221556233917705,     0.99011590136056,    -0.0316723426356603, 
  'EH',      0.23832806753631,   0.00118716829863115,    0.119839150759218,   -0.0649429567847272, 
  'GA',     -0.266610352981902,  0.0142439347090041,    -0.157160602427669,    0.0201046549950529, 
  'GB',      0.569983699945702, -0.0664764747642257,     0.198468738812332,   -0.000182703428282641, 
  'HH',     -0.501569825162518, -0.00670231905749689,   -0.019651784641892,    0.0906523218981392, 
  'HT',     -0.0613234444247148,-0.000889502151334581,   0.0430025231765345,  -0.00278983116616843, 
  'JP',     -0.420043432830034,  0.00403607564949984,   -0.187599015525425,   -0.0389091733228937, 
  'MA',      0.468084314323129,  0.00958064279983111,    0.678223987792607,    0.0295813570930029,  # Mountain ash Spp.	Sorbus L.; Mapped to OH in FVS-NE
  'MM',      0.0317209810222673,-0.0340400816174698,     0.168052731769139,    0.00882258268527404, # Mountain maple	Acer spicatum Lam.; Mapped to BE in FVS-NE
  'NM',      0.547337559525123,  0.00368970130406897,    0.192702714249798,   -0.0257238213929463, 
  'NS',      0.920841840601976, -0.0105302018827745,     0.36415087893201,    -0.0694155463378462, 
  'PB',     -0.385839297400118, -0.01555970103088,      -0.308091415912301,   -0.0187005222638015, 
  'PP',      0.225132074554987,  0.00034883061835917,   -0.0492140542436018,  -0.0214534992754162, 
  'PR',      0.300351370819549, -0.00518831177987846,    0.287346478205465,   -0.0129989659266202, 
  'QA',     -0.15077181884078,   0.00539704035058058,   -0.340232559896089,    0.0199221562824125, 
  'RM',     -0.298278566308469, -0.00498222690678925,   -0.265474647776576,    0.000258040319045941, 
  'RN',      0.821039623095964, -0.0222637540954861,    -0.0705092414278761,  -0.0620751900997439, 
  'RO',     -1.20176658090374,   0.020834595446663,     -0.650193065347603,    0.0138347896809763, 
  'RS',      0.0618488755956072, 0.00202537566145879,    0.00991911205114839, -0.0471014659203857, 
  'SB',      0.180027893811038,  0.00407646754964104,    0.294830204810925,    0.0456285448463209, 
  'SC',      0.29168375283719,   0.00840462598363303,    0.103224327654649,   -0.0375172178406619, 
  'SE',     -0.84474776429187,   0.0194048566748705,    -0.185320642808431,    0.00481365467481041, 
  'SH',     -0.276495487332175, -0.00407826652188834,    0.0014859893481595,   0.0105292454393637, 
  'SM',     -0.638439361494548,  0.010114858457283,     -0.449980116702653,    0.0359737559039904, 
  'SO',      0.576833877897729,  0.0118108307007871,     0.0504539076473394,  -0.0238409166069867, 
  'ST',     -0.0873041359161447, 0.00436311438537946,   -0.116019942616605,    0.00452898449176065, 
  'SV',      1.95860936146763,  -0.0214431695728021,     1.32444277392846,    -0.0107689951306809, 
  'SW',     -0.0393354836772844,-0.00134712871863332,   -0.0101392382579872,   0.00262020551416272, 
  'SY',      0.118394684213857,  0.00110586034750494,    0.00666275043203567, -0.00950736753890871, 
  'TA',     -0.897319686496974,  0.0258707228911306,    -0.196336997360169,    0.0289693031310198, 
  'TM',     -0.108285936575492,  0.000104669900393849,   0.0210335148827363,   0.00194415039443045, 
  'WA',     -0.590367228757044,  0.00605548042245326,   -0.606966608557039,   -0.0338602035136779, 
  'WC',     -0.585163503410162,  0.0129733373547068,     0.0997296337920071,   0.0176629033621184, 
  'WI',     -0.527848384033003, -0.00395736879810604,   -0.15209423465787,     0.0325890859068867,  # Willow Salix L.; Mapped to BL in FVS-NE
  'WO',     -0.838565888443553,  0.0302991107570669,    -0.102629218123826,    0.0502891203363538, 
  'WP',      0.789109370527834, -0.0027631019480008,     0.0685266140378779,  -0.0334226215626329, 
  'WS',      0.237471389318614, -0.00805483001640084,   -0.0435582953989746,  -0.0628031955872689, 
  'YB',     -0.209369611424538, -0.00157109828388873,   -0.237820855906853,    0.0044801516909863, 
  'YP',     -0.113657528638608, -0.00042568172058744,   -0.0555107654593295,   0.00168118033975285,
  '99',      0.0,                0.0,                    0.0,                  0.0,
  'OH',      0.0,                0.0,                    0.0,                  0.0,
  'OS',      0.0,                0.0,                    0.0,                  0.0)

# Hardwood form and risk modifier species parameters
ddbh.hw.mod.spp = tibble::tribble(
  ~species, ~spp.coeff, ~dbh.interaction,
  'RO', -0.3453, 0.0511,
  'SM', 0.0, 0.0,     # reference species (PB)
  'YB', -0.2494, 0.0251,
  'RM', -0.6377, 0.0477,
  'QA', -0.1059, 0.0476,
  'PB', 0.0, 0.0)       # reference species

# SBW modifier region and species parameters
ddbh.sbw.region = tibble::tribble(
  ~region, ~species, ~b1, ~b2, ~b3, ~b4, ~b5, ~b6, ~b7,
  'ME', 'BF', 0.1187, 0.0019, -0.0327, -0.0412, 0.3950, -1.2813, -0.0016,
  'ME', 'BS', 0.0675, 0.0019, -0.0327, -0.0412, 0.3950, -0.9477, -0.0006,
  'ME', 'RS', 0.0675, 0.0019, -0.0327, -0.0412, 0.3950, -0.9477, -0.0006,
  'ME', 'WS', 0.0321, 0.0019, -0.0327, -0.0412, 0.3950, -0.3715, -0.0183,
  'NB', 'BF', 0.0701, -0.0190, -0.0277, -0.0027, 0.0, -0.8200, -0.0018,
  'NB', 'BS', 0.0320, -0.0190, -0.0277, -0.0027, 0.0, -0.6861, -0.0012,
  'NB', 'RS', 0.0320, -0.0190, -0.0277, -0.0027, 0.0, -0.6861, -0.0012,
  'NB', 'WS', 0.0487, -0.0190, -0.0277, -0.0027, 0.0, -0.7839, -0.0006)

### Kuehne, C., Russell, M., Weiskittel, A., and Kershaw Jr, J. 2020. 
### Comparing strategies for representing individual-tree secondary growth 
### in mixed-species stands in the Acadian Forest region. Forest Ecology 
### and Management 459:117823. https://doi.org/https://doi.org/10.1016/j.foreco.2019.117823

#' Calculate diameter increment using Kuehne model
#' 
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param cr Numeric: Crown ratio (proportion 0-1)
#' @param bal.sw Numeric: Basal area in larger softwood trees (m^2 per ha)
#' @param bal.hw Numeric: Basal area in larger hardwood trees (m^2 per ha)
#' @param csi Numeric: Climate site index (m)
#' @param ddbh.b0.spp Numeric: Species b0 parameter
#' @param ddbh.b2.spp Numeric: Species b2 parameter
#' @param ddbh.b3.spp Numeric: Species b3 parameter
#' @param ddbh.b4.spp Numeric: Species b4 parameter
#' @param ht Numeric: Total tree height (m)
#' @return Numeric: Diameter increment (cm)
ddbh = function(dbh, cr, bal.sw, bal.hw, csi, ddbh.b0.spp, ddbh.b2.spp, ddbh.b3.spp, ddbh.b4.spp, ht) {
  
  # Fixed parameter estimates
  b0 = -1.64233500448509 
  b1 = 0.37697814748048 
  b2 = -0.0256836602459228 
  b3 = 0.713456068528815 
  b4 = -0.0657468647965586 
  b5 = -0.0177402942446082 
  b8 = 0.135377049425137 
  
  # use minimum DBH of 1cm 
  dbh = ifelse(dbh<1, 1, dbh)
  
  # diameter increment
  ddbh = ifelse(ht < 1.3716, 0, exp((b0 + ddbh.b0.spp) + (b1 * log(dbh)) + ((b2 + ddbh.b2.spp) * dbh) + 
               ((b3 + ddbh.b3.spp) * log(cr)) + ((b4 + ddbh.b4.spp) * log(bal.sw + 0.1)) + 
               (b5 * bal.hw) + (b8 * log(csi))))
  
  ddbh
}

# hardwood form and risk diameter increment modifier
# Castle, ME; Weiskittel, AR; Ducey, MJ; Wagner, RG; Frank, JF; Pelletier, G. 
# 2018. Evaluating the influence of stem form and damage on individual tree 
# diameter increment and survival in the Acadian Region: Implications for 
# predicting future value of northern commercial hardwood stands. Canadian 
# Journal of Forest Research 48: 1-13.

#' Calculate hardwood form and risk modifier
#' 
#' @param sp Character: Species code
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param bal Numeric: Basal area in larger trees (m^2 per ha)
#' @param form Character: Form class (F1-F8)
#' @param risk Character: Risk class ('R1', 'R2')
#' @param spp.coeff Numeric: Species coefficient
#' @param dbh.interaction Numeric: DBH interaction coefficient
#' @return Numeric: Modifier value
ddbh_hw_mod = function(sp, dbh, bal, form, risk, spp.coeff, dbh.interaction) {
  
  # Check for valid species, form and risk codes
  if (is.na(form) | !(gsub('[^0-9]', '', form) %in% 1:8) |
      is.na(risk) | !(gsub('[^0-9]', '', risk) %in% 1:4) |
      !sp %in% c('RO', 'SM', 'YB', 'RM', 'PB', 'QA')) {
    return(1)
  }
  
  # Convert NHRI form classes
  new.form = ifelse(form %in% c('F1', 'F7', 'F3', 'F4'), 'A', 'B')
  
  # Convert NHRI risk classes
  new.risk = ifelse(risk %in% c('R1', 'R2'), 'LR', 'HR')
  
  # Set form indicators
  form.b = ifelse(new.form == 'B', 1, 0)
  
  # Set risk indicators  
  lr = ifelse(new.risk == 'LR', 1, 0)
  
  ddbh.hw.mod = (exp(-2.9487 - 0.1090 * dbh + 1.2111 * log(dbh) - 0.0430 * bal + 
           spp.coeff - 0.0250 * form.b + 0.2176 * lr + 
           dbh.interaction * dbh)) /
    
                (exp(-2.9487 - 0.1090 * dbh + 1.2111 * log(dbh) - 0.0430 * bal + 
                     spp.coeff + dbh.interaction * dbh + 0.2176))
  
  ddbh.hw.mod
}

# Post-thinning diameter increment modifier  
  # Weiskittel, AR; Hennigar, C;  Kershaw, JA. 2015. Extending the Acadian variant 
  # of FVS to managed stands. In: Wagner, RG, ed. Cooperative Forestry Research Unit: 
  # 2014 Annual Report. University of Maine, School of Forest Resources, Orono, ME; pp 67-74.

#' Calculate diameter thinning modifier
#' 
#' @param sp Character: Species code
#' @param ba.perc.rmv Numeric: Percent basal area removed (proportion 0-1)
#' @param ba.pre.thin Numeric: Pre-thinning basal area (m^2 per ha)
#' @param qmd.ratio Numeric: QMD ratio before and after thinning 
#' @param year.thin Numeric: Commercial thinning year
#' @param year Numeric: Current year
#' @return Numeric: Modifier value
ddbh_thin_mod = function(sp, ba.perc.rmv, ba.pre.thin, qmd.ratio, year.thin, year) {
  
  # Check for valid species and thinning year
  if (!sp %in% c('BF', 'RS') | is.na(year.thin)) {
    return(1)
  }
  
  # time since thinning
  tst = year - year.thin 
  
  # Species parameter estimates
  # Balsam fir 
  if (sp == 'BF') {
    y0 = -0.2566
    y1 = -22.7609
    y2 = 0.7745
    y3 = 1.0511
  } else {
    # Red spruce 
    y0 = -0.5010
    y1 = -20.1147
    y2 = 0.8067
    y3 = 1.1905
  }
  
  ddbh.thin.mod = ifelse(!is.na(ba.perc.rmv) & 
                           !is.na(qmd.ratio) & 
                           !is.na(ba.pre.thin) & 
                           year.thin <= year,
                         round(1 + (exp(y0 + (y1 / ((100 * ba.perc.rmv * qmd.ratio) + 0.01))) * y2^tst * tst^y3), 5),
                         1.0)
  
  ddbh.thin.mod
}

# Chen, C; Weiskittel, AR; Bataineh, M; MacLean, DA. 2018. Refining the Forest 
# Vegetation Simulator for projecting the effects of spruce budworm defoliation 
# in the Acadian Region of North America. Forestry Chronicles 94: 240-253.

#' Calculate spruce budworm modifier  
#' 
#' @param region Character: Region code; options are ME, NB, NS, QC (default 'ME')
#' @param sp Character: Species code
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param bal.sw Numeric: Basal area in larger softwood trees (m^2 per ha)
#' @param bal.hw Numeric: Basal area in larger hardwood trees (m^2 per ha)
#' @param cr Numeric: Crown ratio (proportion 0-1)
#' @param dbh.sw.avg Numeric: Average softwood DBH (cm)
#' @param ht.top100 Numeric: Average height of top 100 trees (m)
#' @param sbw.cdef Numeric: SBW cumulative defoliation (percent)
#' @param b1 Numeric: b1 parameter
#' @param b2 Numeric: b2 parameter
#' @param b3 Numeric: b3 parameter
#' @param b4 Numeric: b4 parameter
#' @param b5 Numeric: b5 parameter
#' @param b6 Numeric: b6 parameter
#' @param b7 Numeric: b7 parameter
#' @return Numeric: Modifier value
ddbh_sbw_mod = function(region='ME', sp, dbh, bal.sw, bal.hw, cr, dbh.sw.avg, ht.top100, sbw.cdef, 
                        b1, b2, b3, b4, b5, b6, b7) {
  
  # Check species and defoliation
  if (!sp %in% c('BF', 'RS', 'BS', 'WS') | is.na(sbw.cdef)) {
    return(1.0)
  }
  
  ddbh.sbw.mod = (b1 * dbh * 
                    exp(b2 * bal.hw + b3 * bal.sw + b4 * ht.top100 + b5 * cr +
                          (b6 * (dbh / dbh.sw.avg)) + (b7 * sbw.cdef)))/
                  (b1 * dbh * 
                    exp(b2 * bal.hw + b3 * bal.sw + b4 * ht.top100 + b5 * cr +
                          (b6 * (dbh / dbh.sw.avg)) ))
    
  ddbh.sbw.mod
}

#Plot basal area increment 

#### Chen, Cen; Rijal, Baburam and Weiskittel, Aaron. Draft 
#### Comparative assessment of time-explicit, state-space and simultaneous
#### models for stand-level volume growth and yield predictions across 
#### complex forest stands in the Acadian region of North America

#' Calculate plot basal area increment
#' 
#' @param rd Numeric: Relative density
#' @param csi Numeric: Climate site index (m)
#' @param ba.perc.hw Numeric: Percent hardwood basal area (proportion 0-1)
#' @param ba.plot Numeric: Plot basal area (m^2 per ha)
#' @return Numeric: Plot basal area increment
plot_ba_incr = function(rd, csi, ba.perc.hw, ba.plot) {
  
  # Weiskittel revised parameter estimates
  q0 = 0.04968
  q1 = -0.15018
  q2 = -0.13355
  q3 = 0.00010
  q4 = 0.11753
  k = 64.45952
  
  ## Equation 3  
  # dBa/dt=rt*ba.plot*(1-ba.plot/k)
  ## assuming t=1
  
  # rt=q0+q1*RD+q2*csi+q3*ba.perc.hw   
  # dBa= rt*ba.plot*(1-ba.plot/k)
  
  #Weiskittel revised equation form
  rt = q0 + q1 * log(rd + 1e-6) + q2 * log(csi) + q3 * log(ba.perc.hw * 100 + 1e-6) + q4 * log(csi * rd + 1e-6)
  dba = ifelse(rt < 0 & ba.plot * (1 - (ba.plot / k)) < 0, 
               0.00227 * ba.plot, 
               rt * ba.plot * (1 - (ba.plot / k))) # if high relative density and 
  # basal area values, equation will yield positive basal area increment. In these cases limit to 0.227% of plot BA (5th percentile in FIA data analysis)
  
  dba = ifelse(dba < (0.00227 * ba.plot), 0.00227 * ba.plot, dba) # constrain to minimum of 0.227% of plot BA
  
  dba
}

#' Wrapper function to calculate diameter increment with modifiers and constraints
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data  
#' @param csi Numeric: Climate site index (m; default stand$csi)
#' @param region Character: Region code; options are ME, NB, NS, QC (default stand$region)
#' @param use.cap.dbh Logical: Apply maximum DBH constraint (default ops$use.cap.dbh)
#' @param use.sbw.mod Logical: Apply SBW outbreak modifiers (default ops$use.sbw.mod)
#' @param use.hw.mod Logical: Apply hardwood form and risk modifiers (default ops$use.hw.mod)
#' @param use.thin.mod Logical: Apply post-commercial thinning modifiers (default ops$use.thin.mod)
#' @param thin.ba.perc.rmv Numeric: Plot basal area percent removed (percent; default ops$thin.ba.perc.rmv)
#' @param thin.ba.pre Numeric: Plot pre-thin basal area (m^2 per ha; default ops$thin.ba.pre)
#' @param thin.qmd.ratio Numeric: Thinning QMD ratio. Before thinning QMD / After thinning QMD (default ops$thin.qmd.ratio)
#' @param thin.year Numeric: Thinning year (year; default ops$thin.year)
#' @param sbw.cdef Numeric: Spruce budworm outbreak cumulative defoliation (percent; default ops$sbw.cdef)
#' @param sbw.yr Numeric: Initial year of spruce budworm outbreak (year; default ops$sbw.yr)
#' @param sbw.dur Numeric: Duration of spruce budworm outbreak (years; default ops$sbw.dur)
#' @param ddbh.spp.df Dataframe: Species parameter table for diameter increment
#' @param ddbh.hw.mod.spp.df Dataframe: Species parameter table for diameter increment hardwood form and risk modifier
#' @param ddbh.sbw.region.df Dataframe: Region/species parameter table for diameter increment SBW modifier
#' @return Dataframe: Tree data with diameter increment calculations
calc_ddbh = function(tree.data, plot.data, csi = stand$csi, region = stand$region,
                     use.cap.dbh = ops$use.cap.dbh, 
                     use.sbw.mod = ops$use.sbw.mod, 
                     use.hw.mod = ops$use.hw.mod, 
                     use.thin.mod = ops$use.thin.mod,
                     thin.ba.perc.rmv = ops$thin.ba.perc.rmv,
                     thin.ba.pre = ops$thin.ba.pre,
                     thin.qmd.ratio = ops$thin.qmd.ratio,
                     thin.year = ops$thin.year,
                     sbw.cdef = ops$sbw.cdef, 
                     sbw.yr = ops$sbw.yr, 
                     sbw.dur = ops$sbw.dur, 
                     ddbh.spp.df = ddbh.spp, 
                     ddbh.hw.mod.spp.df = ddbh.hw.mod.spp, 
                     ddbh.sbw.region.df= ddbh.sbw.region) {
  
  # tree list names
  tree.data.names= colnames(tree.data)
  
  # Join species parameters
  tree = tree.data %>% 
    dplyr::left_join(ddbh.spp.df, 
                     by = c('sp'='species')) %>%
    dplyr::mutate(across(c(ddbh.b0.spp, ddbh.b2.spp, ddbh.b3.spp, ddbh.b4.spp), 
                         ~ifelse(is.na(.x), 0, .x)))
  
  
  # Calculate diameter increment 
  tree = tree %>%
    dplyr::mutate(ddbh = dplyr::case_when(ht<1.3716 ~0,
                                          TRUE ~ddbh(dbh, cr, bal.sw, bal.hw, csi, 
                              ddbh.b0.spp, ddbh.b2.spp, ddbh.b3.spp, ddbh.b4.spp, ht)))
  
  # Calculate modifiers 
  
  # Hardwood form and risk modifier
  if(use.hw.mod == TRUE) {
   
    tree = tree %>%
      dplyr::mutate(idx.hw.mod = match(sp, ddbh.hw.mod.spp.df$species, nomatch = match('PB', ddbh.hw.mod.spp.df$species)),
                    hw.mod.spp.coeff = ddbh.hw.mod.spp.df$spp.coeff[idx.hw.mod],
                    hw.mod.dbh.interaction = ddbh.hw.mod.spp.df$dbh.interaction[idx.hw.mod])%>%
      dplyr::mutate(ddbh.form.risk.mod = ifelse(!is.na(form) & !is.na(risk),
                                                ddbh_hw_mod(sp = sp, dbh = dbh, bal = bal,
                                                            form = form, risk = risk, 
                                                            spp.coeff = hw.mod.spp.coeff, 
                                                            dbh.interaction = hw.mod.dbh.interaction), 1))%>% 
      select( -idx.hw.mod, 
              -hw.mod.spp.coeff, -hw.mod.dbh.interaction)
  } else {
    tree = tree %>%
      dplyr::mutate(ddbh.form.risk.mod = 1)
  }
  
  # Post-thinning modifier
  if (use.thin.mod == TRUE &
      !is.na(thin.ba.perc.rmv) &
      !is.na(thin.ba.pre) &
      !is.na(thin.qmd.ratio) &
      !is.na(thin.year)) {
    tree = tree %>%
      dplyr::mutate(ddbh.thin.mod = purrr::pmap_dbl(list(sp, thin.ba.perc.rmv, 
                                                         thin.ba.pre, thin.qmd.ratio, thin.year, year), 
                                                    ddbh_thin_mod)) 
  } else {
    tree = tree %>%
      dplyr::mutate(ddbh.thin.mod = 1)
  }
  
  # Spruce budworm modifier
  if(use.sbw.mod == TRUE & 
      region %in% c('ME', 'NB') &
      !is.na(sbw.cdef) & sbw.cdef > 0 &
      !is.na(sbw.yr) & sbw.yr<=tree$year[1] &
      !is.na(sbw.dur) & tree$year[1]<=sbw.yr+sbw.dur)  {
    
    # SBW modifier species and region parameters
    
    # filter parms for selected region #
    ddbh.sbw.region.df=ddbh.sbw.region.df %>% 
      filter(region==!!region)
    
    tree = tree %>%
      dplyr::mutate(sbw.mod.idx = match(sp,  
                                        ddbh.sbw.region.df$species),
                    sbw.b1 = ddbh.sbw.region.df$b1[sbw.mod.idx],
                    sbw.b2 = ddbh.sbw.region.df$b2[sbw.mod.idx],
                    sbw.b3 = ddbh.sbw.region.df$b3[sbw.mod.idx],
                    sbw.b4 = ddbh.sbw.region.df$b4[sbw.mod.idx],
                    sbw.b5 = ddbh.sbw.region.df$b5[sbw.mod.idx],
                    sbw.b6 = ddbh.sbw.region.df$b6[sbw.mod.idx],
                    sbw.b7 = ddbh.sbw.region.df$b7[sbw.mod.idx])
    
    tree = tree %>%
      dplyr::left_join(plot.data %>% 
                         dplyr::select(plot, dbh.sw.avg, ht.top100=topht),
                       by='plot') %>% 
      dplyr::mutate(ddbh.sbw.mod = purrr::pmap_dbl(list(region, sp, dbh, bal.sw, bal.hw, cr, dbh.sw.avg, ht.top100, sbw.cdef,
                                                        sbw.b1, sbw.b2, sbw.b3, sbw.b4, sbw.b5, sbw.b6, sbw.b7), 
                                                   ddbh_sbw_mod)) %>% 
      dplyr::select(-dbh.sw.avg, -ht.top100, -sbw.mod.idx,
             -sbw.b1, -sbw.b2, -sbw.b3, -sbw.b4, -sbw.b5, -sbw.b6, -sbw.b7)
    
  } else {
    tree = tree %>%
      dplyr::mutate(ddbh.sbw.mod = 1)
  }
  
  # Apply modifiers
  tree = tree %>%
    dplyr::mutate(ddbh = ddbh * ddbh.thin.mod * ddbh.sbw.mod * ddbh.form.risk.mod * ddbh.mult)
  
  # plot basal area constraint constant
    k = 64.45952
  
  # Calculate plot basal area constraint
  plot.smry.ba = plot.data %>%
    dplyr::mutate(dba = plot_ba_incr(rd = rd.mod, csi = csi, 
                                     ba.perc.hw = ba.perc.hw, ba.plot = ba.plot))
  
  # Apply plot basal area constraint
    # above 0.8*k - proportion of plot ba only
    # above 0.6*k - average of predicted BA increment and proportion of plot ba
  tree = tree %>%
    dplyr::mutate(dba.tree = 0.00007854 * expf * ((ddbh + dbh)^2 - dbh^2)) %>%
    dplyr::group_by(plot) %>%
    dplyr::mutate(dba.tree.sum = sum(dba.tree, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(plot.smry.ba %>% 
                       dplyr::select(plot, ba.plot, dba), 
                     by = 'plot') %>%
    dplyr::mutate(dba.tree.plot = dba * (dba.tree / dba.tree.sum),
                  ddbh = dplyr::case_when(ba.plot > (k*0.8) & dba.tree.sum > dba ~
                                            sqrt((dba.tree.plot + ba) / 0.00007854 / expf) - dbh, 
                                          ba.plot > (k*0.6) & dba.tree.sum > dba ~
                                            sqrt(((dba.tree.plot+dba.tree)/2 + ba) / 0.00007854 / expf) - dbh, 
                                          TRUE ~ddbh)) 
  
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

# Species random effects parameters
dht.spp = dplyr::tribble(
  ~species, ~dht.b0.spp, ~dht.b2.spp,
  
  #species|--------dht.b0.spp-|----------dht.b2.spp-|
  #-------|-------------------|---------------------|
  "AB",   -1.02612824864817,   0.0346037998631002,
  "AE",    1.43819563280052,  -0.0959572257517136,
  "AH",   -0.673062037647421,  0.0230922230586148, 
  "AL",   -0.664943978541551,  0.0286370517640571, # 	Alder spp. FIA:350; no FVS code
  "AP",   -0.513076210579854,  0.00891799842172517,
  "BA",   -0.37524267342808,   0.0337185642193777,
  "BC",   -1.81264792042519,   0.116681975139121,
  "BE",    1.27871255671255,  -0.0341088659665619,
  "BF",   -0.776163565611841,  0.0555897206492342,
  "BN",    1.28974293813166,  -0.0334592748349059,
  "BO",    1.12645061757292,  -0.0195368383363507,
  "BP",    0.694662181613901, -0.00698754776147194,
  "BR",    0.0755027423746021,-0.0030668208065367,
  "BS",   -0.733121933923716,  0.00145780711570331,
  "BT",   -0.942478265169676,  0.0643943510361381,
  "BW",    0.911461811693869, -0.0365763276923436,
  "CC",   -0.122848430751689,  0.00498902131642062,
  "EC",    0.0851004082247269,-0.000531040757535847,
  "EH",   -0.422651876464928,  0.0138065741083889,
  "GA",   -0.0592752775633811, 0.023337209338204,
  "GB",   -0.239196056043342, -0.0115777571712358,
  "HH",    0.150921901545459, -0.0473114443545307,
  "HT",   -0.235863310157905,  0.0106140505063283,
  "JP",   -0.972249355786984,  0.0819895541291093,
  "MA",    0.339439898941425, -0.0325361018497775,
  "MM",   -0.391994717500281,  0.0228089427348761,
  "NM",   -0.0866372995869185, 0.00270418941934147,
  "NS",   -0.204644855590905,  0.00916747349731601,
  "PB",   -0.672325144256529,  0.0128071971875175,
  "PP",    1.3093846110868,   -0.0602079922258899,
  "PR",    0.631536576687535, -0.0244522671812604,
  "PY",    0.0858081585771575,-0.000595774933743924,
  "QA",   -0.0529031586151793, 0.00999107750243689,
  "RM",   -0.551409784285385,  0.00747419687193155,
  "RN",    0.961225210142485, -0.0624506295445526,
  "RO",   -1.40760436773568,   0.0774545447566624,
  "RS",   -0.0191386094616815,-0.0123820721821638,
  "SB",    0.864845030207318, -0.0248496144856717,
  "SE",    1.01998129466649,  -0.0773472411859139,
  "SH",    0.118342580501145, -0.00302871180770913,
  "SM",   -0.281974719368835,  0.00337387843817102,
  "SO",    0.987461924599635, -0.0229812512122912,
  "ST",    0.667822504069366, -0.0402936551456072,
  "SV",    0.396177483707355, -0.0246358074186711,
  "TA",   -1.07442965846633,   0.0480678095800713,
  "TM",   -0.0963179446297929, 0.00440059570180893,
  "WA",   -0.84083502294648,   0.0362928969955831,
  "WC",   -0.128783409980557, -0.0105127926817691,
  "WI",   -0.0377792893971838, 0.0143055161878269,
  "WO",    0.914075944622758, -0.0395966442560029,
  "WP",    0.573492987883601, -0.0251861980929384,
  "WS",   -0.0343185671955406,-0.000954809895131117,
  "YB",   -0.456836103602571, -0.000189742778138818,
  "YP",   -0.0134632029990978, 0.000636230771304117,
  "99",    0.0,                0.0, 
  "OH",    0.0,                0.0,
  "OS",    0.0,                0.0)

# SBW height modifier species parameters
dht.sbw.spp = dplyr::tribble(
  ~species, ~b1, ~b5, ~b6,
  'BF', 0.0013, 0.3676, -0.0017,
  'BS', 0.0009, 0.2881, -0.0014,
  'RS', 0.0009, 0.2881, -0.0014,
  'WS', 0.0005, 0.6800, 0.0001)

# Kuehne, C; Weiskittel, AR; Kershaw, JA. 2022. Development and evaluation of refined 
# annualized individual tree diameter and height increment equations for the Acadian 
# Variant of the Forest Vegetation Simulator: Implication for forest carbon estimates. 
# Mathematical and Computational Forestry & Natural-Resource Sciences 14(2): 9-31

#' Calculate height increment
#' 
#' @param ht Numeric: Tree height (m)
#' @param cr Numeric: Crown ratio (proportion 0-1)
#' @param ccfl Numeric: Crown competition factor
#' @param csi Numeric: Climate site index (m)
#' @param dht.b0.spp Numeric: Species b0 parameter from dht.spp table
#' @param dht.b2.spp Numeric: Species b2 parameter from dht.spp table
#' @return Numeric: Height increment (m)
dht = function(ht, cr, ccfl, csi, dht.b0.spp, dht.b2.spp) {
  
  # Fixed parameter estimates
  b0 = -2.19445331289132 
  b1 = 0.42640409105239 
  b2 = -0.0647071404836054 
  b3 = 0.394836682501739 
  b4 = -0.0114287748735248 
  b7 = 0.00029415038133825
  
  # Model form from Kuehne
  dht = exp((b0 + dht.b0.spp) + b1 * log(ht) + (b2 + dht.b2.spp) * ht + 
              b3 * cr + b4 * (ccfl / 100) + b7 * csi^2)
  
  dht
}

#' Calculate height thinning modifier
#' 
#' @param sp Character: Species code (FVS alpha)
#' @param ba.perc.rmv Numeric: Percent basal area removed (proportion 0-1)
#' @param ba.pre.thin Numeric: Pre-thinning basal area (m^2 per ha)
#' @param qmd.ratio Numeric: QMD ratio before and after thinning
#' @param year.thin Numeric: Commercial thinning year
#' @param year Numeric: Current year
#' @return Numeric: Height increment modifier value
dht_thin_mod = function(sp, ba.perc.rmv, ba.pre.thin, qmd.ratio, year.thin, year) {
  
  if (!sp %in% c('BF', 'RS') | is.na(year.thin) | year.thin > year | (year - year.thin) >= 5) {
    return(1)
  }
  
  tst = year - year.thin # time since thinning
  
  # Species parameter estimates
  # Balsam fir 
  if (sp == "BF") {
    y0 = -1.8443
    y1 = 5.2969
    y2 = 1.0532
    y3 = 0.0
  } else if (sp == "RS") {
    # Red spruce 
    y0 = -1.8426
    y1 = 6.2781
    y2 = 1.1596
    y3 = 0.0
  }
  
  dht.thin.mod = 1 - (exp(y0 + y1 / ((100 * ba.perc.rmv) + 0.01)) * y2^tst * tst^y3)
  dht.thin.mod = pmax(0.75, pmin(dht.thin.mod, 1.25))
  
  dht.thin.mod
}



#' Calculate SBW height modifier
#' 
#' @param sp Character: Species code (FVS alpha)
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param ht.top100 Numeric: Average height of top 100 trees (m)
#' @param cr Numeric: Crown ratio (proportion 0-1)
#' @param dbh.sw.avg Numeric: Average softwood DBH (cm)
#' @param sbw.cdef Numeric: SBW cumulative defoliation (percent)
#' @param b1 Numeric: Species b1 parameter from dht.sbw.spp table
#' @param b5 Numeric: Species b5 parameter from dht.sbw.spp table
#' @param b6 Numeric: Species b6 parameter from dht.sbw.spp table
#' @return Numeric: Height increment modifier value
dht_sbw_mod = function(sp, dbh, ht.top100, cr, dbh.sw.avg, sbw.cdef, b1, b5, b6) {
  
  if (!sp %in% c('BF', 'BS', 'RS', 'WS') | is.na(sbw.cdef)) {
    return(1.0)
  }
  
  sbw.cdef = ifelse(is.na(sbw.cdef), 0, sbw.cdef)
  
  # Fixed parameter estimates
  b2 = -0.0011
  b3 = 0.0316
  b4 = 2.4512
  
  dht.mod = (b1 * dbh * exp(b2 * dbh^2 + b3 * ht.top100 + b4 * cr + 
                              b5 * (dbh / dbh.sw.avg) + b6 * sbw.cdef))/
    
    (b1 * dbh * exp(b2 * dbh^2 + b3 * ht.top100 + b4 * cr + 
                      b5 * (dbh / dbh.sw.avg)))
  
  dht.mod
}

#' Wrapper function to calculate height increment with modifiers and constraints
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data  
#' @param csi Numeric: Climate site index (m; default stand$csi)
#' @param use.cap.ht Logical: Apply maximum total tree height constraint (default ops$use.cap.ht)
#' @param use.sbw.mod Logical: Apply SBW outbreak modifiers (default ops$use.sbw.mod)
#' @param use.thin.mod Logical: Apply post-commercial thinning modifiers (default ops$use.thin.mod)
#' @param thin.ba.perc.rmv Numeric: Plot basal area percent removed (percent; default ops$thin.ba.perc.rmv)
#' @param thin.ba.pre Numeric: Plot pre-thin basal area (m^2 per ha; default ops$thin.ba.pre)
#' @param thin.qmd.ratio Numeric: Thinning QMD ratio. Before thinning QMD / After thinning QMD (default ops$thin.qmd.ratio)
#' @param thin.year Numeric: Thinning year (year; default ops$thin.year)
#' @param sbw.cdef Numeric: Spruce budworm outbreak cumulative defoliation (percent; default ops$sbw.cdef)
#' @param sbw.yr Numeric: Initial year of spruce budworm outbreak (year; default ops$sbw.yr)
#' @param sbw.dur Numeric: Duration of spruce budworm outbreak (years; default ops$sbw.dur)
#' @param region Character: Region code; options are ME, NB, NS, QC (default stand$region)
#' @param dht.spp.df Dataframe: Species parameter table for height increment (default dht.spp)
#' @param dht.sbw.spp.df Dataframe: Species parameter table for SBW modifier (default dht.sbw.spp)
#' @return Dataframe: Tree data with height increment calculations
calc_dht = function(tree.data, plot.data, csi = stand$csi, 
                    use.cap.ht = ops$use.cap.ht, 
                    use.sbw.mod = ops$use.sbw.mod, 
                    use.thin.mod = ops$use.thin.mod,
                    thin.ba.perc.rmv = ops$thin.ba.perc.rmv,
                    thin.ba.pre = ops$thin.ba.pre,
                    thin.qmd.ratio = ops$thin.qmd.ratio,
                    thin.year=ops$thin.year,
                    sbw.cdef = ops$sbw.cdef,
                    sbw.yr = ops$sbw.yr,
                    sbw.dur = ops$sbw.dur,
                    region=stand$region,
                    dht.spp.df = dht.spp, 
                    dht.sbw.spp.df = dht.sbw.spp) {
  
  # get tree list variable names
  tree.data.names= colnames(tree.data)   
  
  # Height increment
  tree = tree.data %>%
    # Species parameters
    dplyr::mutate(idx = match(sp,  
                                 dht.spp.df$species,
                                 nomatch = match('99', dht.spp.df$species)), # OH, OS and 99 use b0=0 and b2=0
                  dht.b0.spp = dht.spp.df$dht.b0.spp[idx],
                  dht.b2.spp = dht.spp.df$dht.b2.spp[idx],
      # Calculate height increment
      dht = dht(ht, cr, ccfl, csi, dht.b0.spp, dht.b2.spp))
  
  # Post-thinning modifier
  if (use.thin.mod == TRUE &
      !is.na(thin.ba.perc.rmv) &
      !is.na(thin.ba.pre) &
      !is.na(thin.qmd.ratio) &
      !is.na(thin.year)) {
    tree = tree %>%
      dplyr::mutate(dht.thin.mod = purrr::pmap_dbl(list(sp, thin.ba.perc.rmv, thin.ba.pre, 
                                                        thin.qmd.ratio, thin.year, year), 
                                                   dht_thin_mod))
  } else {
    tree = tree %>%
      dplyr::mutate(dht.thin.mod = 1)
  }
  
  # Spruce budworm modifier
  if (use.sbw.mod == TRUE &  #dht
      !is.na(sbw.cdef) & sbw.cdef > 0 &
      !is.na(sbw.yr) & sbw.yr<=tree$year[1] &
      !is.na(sbw.dur) & tree$year[1]<=sbw.yr+sbw.dur)  { 
   
    tree = tree %>% 
        # Join plot-level data 
      dplyr::left_join(plot.data %>% 
                         dplyr::select(plot, 
                                       dbh.sw.avg, 
                                       ht.top100=topht),
                       by = 'plot') %>%
      # SBW modifier species parameters
      dplyr::mutate(sbw.mod.idx = match(sp,  
                                        dht.sbw.spp.df$species),
                    sbw.b1 = dht.sbw.spp.df$b1[sbw.mod.idx],
                    sbw.b5 = dht.sbw.spp.df$b5[sbw.mod.idx],
                    sbw.b6 = dht.sbw.spp.df$b6[sbw.mod.idx],
                    dht.sbw.mod = purrr::pmap_dbl(list(sp, dbh, ht.top100, cr, dbh.sw.avg, sbw.cdef,
                                                       sbw.b1, sbw.b5, sbw.b6), 
                                                  dht_sbw_mod)) %>%
      dplyr::select(-dbh.sw.avg, -ht.top100, 
                    -sbw.mod.idx, -sbw.b1, -sbw.b5, -sbw.b6)
    
  } else {
    tree = tree %>%
      dplyr::mutate(dht.sbw.mod = 1)
  }
  
  
  # Apply modifiers
  tree = tree %>%
    dplyr::mutate(dht = dht * dht.thin.mod * dht.sbw.mod * dht.mult)
  
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


# Dynamic crown recession equation 
# Russell, MB; Weiskittel, AR; Kershaw, JA. 2014. Comparing strategies for modeling 
# individual-tree height and height-to-crown base increment in mixed-species Acadian 
# forests of northeastern North America. European Journal of Forest Research. 
# 133:1121-1135. doi: 10.1007/s10342-014-0827-1

#' Calculate height to crown base change
#' 
#' @param dht Numeric: Height increment (m)
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param ht Numeric: Total tree height (m)
#' @param hcb Numeric: Height to crown base (m)
#' @param ccf Numeric: Crown competition factor
#' @param shade Numeric: Shade tolerance value
#' @return Numeric: Height to crown base change (m)
dhcb = function(dht, dbh, ht, hcb, ccf, shade) {
  
  # Fixed parameter estimates
  b1 = 4.000
  b2 = -0.8395
  b3 = -0.2196
  b4 = -0.3059
  b5 = -0.00553
  b6 = 0.09821 
  
  # Crown length and crown ratio
  cl = ht - hcb
  cr = cl / ht
  
  # Crown base
  dhcb = (cl + dht) / (1 + exp(b1 + b2 * log(cr + 0.01) + b3 * log(ccf + 1) + 
                                 b4 * log(1.01 - cr) + b5 * (shade^2) + b6 * log(shade * cr)))
  
  dhcb
}

#' Calculate crown recession thinning modifier
#' 
#' @param sp Character: Species code (FVS alpha)
#' @param ba.perc.rmv Numeric: Percent basal area removed (proportion 0-1)
#' @param ba.pre.thin Numeric: Pre-thinning basal area (m^2 per ha)
#' @param qmd.ratio Numeric: QMD ratio before and after thinning 
#' @param year.thin Numeric: Commercial thinning year
#' @param year Numeric: Current year
#' @return Numeric: Crown recession modifier value
dhcb_thin_mod = function(sp, ba.perc.rmv, ba.pre.thin, qmd.ratio, year.thin, year) {
  
  if (!sp %in% c('BF', 'RS') | is.na(year.thin)) {
    return(1)
  }
  
  # time since thinning
  tst = year - year.thin 
  
  # Species parameter estimates
  # Balsam fir 
  if (sp == 'BF') {
    y0 = -0.4208
    y1 = -17.0998
    y2 = 0.7986
    y3 = 0.0521
  } else {
    # Red spruce 
    y0 = -1.0778
    y1 = -14.7694
    y2 = 0.7758
    y3 = 1.1164
  }
  
  dhcb.thin.mod = ifelse(!is.na(ba.perc.rmv) & 
                           !is.na(qmd.ratio) & 
                           !is.na(ba.pre.thin) & 
                           year.thin <= year,
                         1 - (exp(y0 + (y1 / ((100 * ba.perc.rmv * qmd.ratio) + 0.01))) * y2^tst * tst^y3),
                         1.0)
  
  dhcb.thin.mod = pmin(abs(dhcb.thin.mod), 1)
  
  dhcb.thin.mod
}

#' Wrapper function to calculate crown recession 
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data  
#' @param use.thin.mod Logical: Apply post-commercial thinning modifiers (default ops$use.thin.mod)
#' @param thin.ba.perc.rmv Numeric: Plot basal area percent removed (percent; default ops$thin.ba.perc.rmv)
#' @param thin.ba.pre Numeric: Plot pre-thin basal area (m^2 per ha; default ops$thin.ba.pre)
#' @param thin.qmd.ratio Numeric: Thinning QMD ratio. Before thinning QMD / After thinning QMD (default ops$thin.qmd.ratio)
#' @param thin.year Numeric: Thinning year (year; default ops$thin.year)
#' @return Dataframe: Tree data with crown recession calculations
calc_dhcb = function(tree.data, plot.data, use.thin.mod = ops$use.thin.mod,
                     thin.ba.perc.rmv = ops$thin.ba.perc.rmv,
                     thin.ba.pre = ops$thin.ba.pre,
                     thin.qmd.ratio = ops$thin.qmd.ratio,
                     thin.year = ops$thin.year) {
  
  # Join plot summary values
  tree = tree.data %>% 
    dplyr::left_join(plot.data %>% 
                       dplyr::select(plot, ccf), 
                     by = 'plot')
  
  # Calculate crown recession
  tree = tree %>%
    dplyr::mutate(dhcb =  dhcb(dht, dbh, ht, hcb, ccf, shade))
  
  # Post-thinning modifier
  if (use.thin.mod == TRUE &
      !is.na(thin.ba.perc.rmv) &
      !is.na(thin.ba.pre) &
      !is.na(thin.qmd.ratio) &
      !is.na(thin.year)) {
    tree = tree %>%
      dplyr::mutate(dhcb.thin.mod = purrr::pmap_dbl(list(sp, thin.ba.perc.rmv, thin.ba.pre, 
                                                         thin.qmd.ratio, thin.year, year), 
                                                    dhcb_thin_mod))
  } else {
    tree = tree %>%
      dplyr::mutate(dhcb.thin.mod = 1)
  }
  
  # Apply modifier to crown recession
  tree = tree %>%
    dplyr::mutate(dhcb = dhcb * dhcb.thin.mod)
  
  # Clean up temporary columns
  tree = tree %>%
    dplyr::select(-dhcb.thin.mod, -ccf)
  
  tree
}

#### Mortality ####

# Tree mortality probability species parameters
tree.mort.spp = dplyr::tribble(
  ~species, ~b0, ~b1, ~b2, ~d.jump, ~scale, ~shape,
  'AB', 2.152681379, -0.0269825907, 0.0002203177, 41, 20, 4.5,
  'AE', 2.948346662, -0.1017558326, 0.0018279137, 22.5, 40, 2,
  'AH', 4.552236589, -0.4626664119, 0.0125996045, 12, 15, 4.5,
  'AP', 5.6430024532, -0.4644532732, 0.0132654057, 16, 10.6, 4.1,
  'BA', 1.7183600838, 0.0393047451, -0.0023514773, 39, 37, 4.25,
  'BC', 4.8627898851, -0.3695674404, 0.0109822297, 9.2, 31, 3.63,
  'BE', 4.552236589, -0.2313332059, 0.0125996045, 14, 20, 4.5,
  'BF', 2.5743949775, -0.0851930923, 0.0015971909, 53, 40.6, 1.51,
  'BL', -3.5183135273, 0.5008393656, -0.0114432915, 10, 10, 2,
  'BN', 9.6140856026, -0.8619281584, 0.0215901194, 15, 27.5, 1.5,
  'BO', 2.7402431243, -0.0403087, 0.0014846314, 40, 40, 2,
  'BP', 1.8795415329, -0.3915484285, 0.0298003249, 33, 33.6, 4.75,
  'BS', 1.9568828063, 0.0535388009, -0.0010376306, 34, 22, 3.75,
  'BT', 2.1791849646, -0.0125375225, 0.0008529794, 40, 30, 3,
  'BW', -1.4145296118, 0.3204863989, -0.0029710752, 15, 30, 3,
  'CC', 4.8627898851, -0.3695674404, 0.0109822297, 10, 20, 3.63,
  'EC', -0.4584998714, 0.1992627013, -0.0028451758, 40.6, 11, 4.28,
  'EH', 4.5205542708, -0.0670350692, 0.0012041907, 50, 40, 3.5,
  'GA', 7.2061395918, -0.2239701333, 0.0070370484, 34, 39.6, 3.41,
  'GB', 0.1922677751, 0.1517490102, -0.0039268819, 16, 8.2, 3.83,
  'HH', 2.9674489273, -0.1009595852, 0.0071673636, 18, 24, 2,
  'JP', -0.4488149338, 0.1939739736, -0.0019541699, 30, 20, 3,
  'NM', 4.552236589, -0.2313332059, 0.0125996045, 24, 40, 4.5,
  'NS', 17.4833923331, -1.809142126, 0.0616970369, 15, 35, 2,
  'OH', 4.552236589, -0.5204997134, 0.0125996045, 12, 10, 4.5,
  'PB', 2.5863343441, -0.0518497247, 0.0021853588, 26.4, 41.2, 1.88,
  'PP', 12.1649655944, -1.0483772747, 0.0233147008, 25.8, 41.6, 4.41,
  'PR', -1.2171488097, 0.3211464783, -0.0097154365, 10, 40, 2,
  'QA', -0.4584998714, 0.1992627013, -0.0028451758, 60.6, 11, 4.28,
  'RM', 2.1674971386, 0.0557266595, -0.0010435394, 60.2, 40.6, 4.38,
  'RO', 3.1202275212, -0.041290776, 0.0022978235, 41, 40.4, 3.27,
  'RP', 1.1361278304, 0.1436446742, 0.0018438454, 30, 30, 3,
  'RS', 2.0420797297, 0.0425701678, -0.0004901795, 41, 32, 4.8,
  'SB', 44.2565091524, -2.4248136198, 0.1388397603, 22, 35.6, 4.57,
  'SH', 4.552236589, -0.2313332059, 0.0125996045, 15, 10, 4.5,
  'SM', 2.7069022565, 0.0086263655, 0.0007235392, 54.4, 42, 1.33,
  'SP', 5, -0.3, 0.01, 20, 40, 3,
  'ST', 4.5522366258, -0.4626664068, 0.012599604, 4.4, 24.4, 4.51,
  'SV', 4.552236589, -0.2313332059, 0.0125996045, 24, 40, 4.5,
  'TA', 1.4269435976, 0.0886275939, -0.0021232407, 30, 33.6, 4.5,
  'WA', 1.0042653571, 0.165359309, -0.0005814562, 21, 40, 4,
  'WC', 3.647647507, -0.0606735724, 0.0008507857, 40, 45, 5,
  'WL', -3.5183135273, 0.5008393656, -0.0114432915, 10, 10, 2,
  'WO', -4.8640326448, 0.6250645999, -0.0064419714, 18, 40, 4,
  'WP', 3.3383526175, -0.0294498474, 0.0009561864, 61.2, 41.2, 2.87,
  'WS', 0.5437824528, 0.1052397713, 0.0006332627, 19, 54, 2,
  'YB', 2.6967072576, -0.001250889, 0.0007521152, 48, 60, 2,
  '99', 2.6967072576, -0.001250889, 0.0007521152, 48, 60, 2)


# Plot mortality probability region parameters 
stand.mort.prob.region = dplyr::tribble(
  ~region, ~b0, ~b1, ~b2, ~b3, ~b4, ~b5, ~b6, ~cut,
  'ME', 0.6906978, 0.149228, -0.001855535, -2.557345, -0.05507579, 0.06414701, 0.0432701, 0.871958,
  'NB', 0.699147, 0.1250758, -0.001855535, -2.557345, -0.05507579, 0.06414701, 0.0432701, 0.7268086,
  'NS', 0.2756542, 0.1499495, -0.001855535, -2.557345, -0.05507579, 0.06414701, 0.0432701, 0.9148455,
  'QC', 1.0472726, 0.161746, -0.001855535, -2.557345, -0.05507579, 0.06414701, 0.0432701, 0.7351621)

# Plot mortality basal area region parameters
stand.mort.ba.region = dplyr::tribble(
  ~region, ~b0, ~b1, ~b2, ~b3, ~b4, ~b5, ~b6, ~b7,
  'ME', 0.1857844, 0.2315199, 0.02020253, 0.5674303, -2.037042, 0.06815229, 0.3345308, 0.09950853,
  'NB', 0.5987741, 0.2315199, 0.02020253, 0.1888859, -2.037042, 0.14607033, 0.3284819, 0.09950853,
  'NS', 0.1302331, 0.2315199, 0.02020253, 0.589446, -2.037042, 0.0867806, 0.2243597, 0.09950853,
  'QC', 0.1068258, 0.2315199, 0.02020253, 0.6810417, -2.037042, 0.01171661, 0.494071, 0.09950853)

# Plot SBW mortality modifier region parameters
smort.sbw.region = dplyr::tribble(
  ~region, ~b1, ~b2, ~b3, ~b4,
  'ME', -2.6380, 0.0114, -0.0076, 0.0074,
  'NB', -3.0893, 0.0071, -0.0037, 0.0)
  
# Tree SBW survival modifier region-species parameters
tsurv.sbw.region.spp = dplyr::tribble(
  ~region, ~sp, ~b1, ~b2, ~b3, ~b4, ~b5, ~b6, ~b7, ~b8,
  'ME', 'BF', -6.5208, -0.4866, -0.0355, 0.0316, 1.5087, -0.0175, 0.0274, 0.0040,
  'ME', 'BS', -6.5208, -0.4866, -0.1231, 0.0316, 1.5087, -0.0175, 0.0274, 0.0056,
  'ME', 'RS', -6.5208, -0.4866, -0.1231, 0.0316, 1.5087, -0.0175, 0.0274, 0.0056,
  'ME', 'WS', -6.5208, -0.4866, -0.1755, 0.0316, 1.5087, -0.0175, 0.0274, 0.0207,
  'NB', 'BF', -6.8310, 0.0, -0.2285, 0.2025, 2.1703, 0.0, 0.0, 0.0029,
  'NB', 'BS', -6.8310, 0.0, -0.2285, 0.2025, 2.0809, 0.0, 0.0, 0.0101,
  'NB', 'RS', -6.8310, 0.0, -0.2285, 0.2025, 2.0809, 0.0, 0.0, 0.0101,
  'NB', 'WS', -6.8310, 0.0, -0.2285, 0.2025, 1.5802, 0.0, 0.0, 0.0021)

# Tree hardwood form and risk survival modifier species parameters
tsurv.hw.spp = dplyr::tribble(
  ~sp, ~b0, ~b1,
  'QA', -2.7907, 0.0791,
  'RM', -3.9809, 0.8343,
  'RO', -0.7937, 0.8944,
  'YB',  5.2531, 0.1528,
  'PB', 0, 0)

#' Calculate plot mortality probability 
#' 
#' @param ba Numeric: Plot basal area (m^2 per ha) 
#' @param bag Numeric: Basal area growth (m^2 per ha)
#' @param qmd Numeric: Quadratic mean diameter (cm)
#' @param ba.bf Numeric: Plot basal area in balsam fir (m^2 per ha)
#' @param ba.ihw Numeric: Plot basal area in intolerant hardwoods (m^2 per ha)
#' @param b0 Numeric: Intercept parameter
#' @param b1 Numeric: BA parameter
#' @param b2 Numeric: BA squared parameter
#' @param b3 Numeric: BAG parameter
#' @param b4 Numeric: QMD parameter
#' @param b5 Numeric: BF BA parameter
#' @param b6 Numeric: IH BA parameter
#' @return Numeric: Mortality probability (proportion 0-1)
stand_mort_prob = function(ba, bag, qmd, ba.bf, ba.ihw, b0, b1, b2, b3, b4, b5, b6) {
  
  
  k = b0 + b1 * ba + b2 * ba^2 + b3 * bag + b4 * qmd + b5 * ba.bf + b6 * ba.ihw
  
  mort.prob = exp(k) / (1 + exp(k))
  
  mort.prob
}

#' Calculate stand mortality basal area
#' 
#' @param ba Numeric: Plot basal area (m^2 per ha)
#' @param bag Numeric: Basal area growth (m^2 per ha)
#' @param qmd Numeric: Quadratic mean diameter (cm)
#' @param qmd.bf Numeric: Balsam fir quadratic mean diameter (cm)
#' @param ba.bf Numeric: Plot basal area in balsam fir (m^2 per ha)
#' @param ba.ihw Numeric: Plot basal area in intolerant hardwoods (m^2 per ha)
#' @param b0 Numeric: Intercept parameter
#' @param b1 Numeric: ba.bf parameter
#' @param b2 Numeric: ba.ihw parameter
#' @param b3 Numeric: BA  parameter
#' @param b4 Numeric: BAG exponent parameter
#' @param b5 Numeric: BF mortality parameter
#' @param b6 Numeric: BF BA parameter
#' @param b7 Numeric: QMD ratio parameter
#' @return Numeric: Total mortality basal area (m^2 per ha)
stand_mort_ba = function(ba, bag, qmd, qmd.bf, ba.bf, ba.ihw, b0, b1, b2, b3, b4, b5, b6, b7) {
  
  
  ba.perc.ihw = ifelse(ba == 0, 0, ba.ihw / ba)
  ba.perc.bf  = ifelse(ba == 0, 0, ba.bf  / ba)
  
  ba.mort = (b0 + b1 * ba.perc.bf + b2 * ba.perc.ihw) * ba^(b3 + b4 * bag)
  bf.mort = ifelse(qmd == 0, 0, b5 * ba.bf^b6 + b7 * (qmd.bf / qmd))
  
  ba.mort + bf.mort
}

#' Calculate tree survival probability
#' 
#' @param sp Character: Species code (FVS alpha)
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param b0 Numeric: Intercept parameter
#' @param b1 Numeric: DBH parameter
#' @param b2 Numeric: DBH squared parameter
#' @return Numeric: Tree survival probability (proportion 0-1)
tree_surv_prob = function(sp, dbh, b0, b1, b2) {
  
  ddd = b0 + b1 * dbh + b2 * dbh^2
  surv.prob = exp(ddd) / (1 + exp(ddd))
  
  surv.prob
}

#' SBW stand mortality modifier 
#' 
#' @param ba.plot Numeric: Plot basal area (m^2 per ha)
#' @param ba.bf Numeric: Plot basal area in balsam fir (m^2 per ha)
#' @param ht.top100 Numeric: Average height of top 100 trees (m)
#' @param sbw.cdef Numeric: SBW cumulative defoliation (percent)
#' @param b1 Numeric: Intercept parameter
#' @param b2 Numeric: BF BA interaction parameter
#' @param b3 Numeric: Volume parameter
#' @param b4 Numeric: CDEF parameter
#' @return Numeric: Mortality probability multiplier (>=1)
smort_sbw_mod = function(ba.plot, ba.bf, ht.top100, sbw.cdef, b1, b2, b3, b4) {
  
  vol = (ht.top100 / 2) * ba.plot
  
  sbw.mod = ((1 / (1 + exp(-b1))) * (1 / (1 + exp(-(b2 * sbw.cdef * ba.bf + b3 * vol + b4 * sbw.cdef)))))/
    ((1 / (1 + exp(-b1))) * (1 / (1 + exp(-(b3 * vol)))))
  
  sbw.mod
}

#' SBW mortality modifier for trees
#' 
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param cr Numeric: Crown ratio (proportion 0-1)
#' @param ht Numeric: Tree height (m)
#' @param bal.hw Numeric: Basal area in larger hardwood trees (m^2 per ha)
#' @param bal.sw Numeric: Basal area in larger softwood trees (m^2 per ha)
#' @param ht.sw.avg Numeric: Average softwood height (m)
#' @param sbw.cdef Numeric: SBW cumulative defoliation (percent)
#' @param b1 Numeric: Intercept parameter
#' @param b2 Numeric: Crown ratio parameter
#' @param b3 Numeric: DBH parameter
#' @param b4 Numeric: Height parameter
#' @param b5 Numeric: Height ratio parameter
#' @param b6 Numeric: BAL.SW parameter
#' @param b7 Numeric: BAL.HW parameter
#' @param b8 Numeric: CDEF parameter
#' @return Numeric: Survival probability modifier (0-1)
tsurv_sbw_mod = function(dbh, cr, ht, bal.hw, bal.sw, ht.sw.avg, sbw.cdef, b1, b2, b3, b4, b5, b6, b7, b8) {
  
  
  sbw.surv.mod =  exp(-exp(b1 + b2 * cr + b3 * dbh + b4 * ht.sw.avg +
                              b5 * (ht / ht.sw.avg) + b6 * bal.sw + b7 * bal.hw +
                              b8 * sbw.cdef)) /
                    exp(-exp(b1 + b2 * cr + b3 * dbh + b4 * ht.sw.avg +
                               b5 * (ht / ht.sw.avg) + b6 * bal.sw + b7 * bal.hw))
  
  sbw.surv.mod
}

#'Tree hardwood form and risk survival modifier
#' 
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param bal Numeric: Basal area in larger trees (m^2 per ha)
#' @param ba.plot Numeric: Plot basal area (m^2 per ha)
#' @param form Character: Form class (F1-F8)
#' @param b0 Numeric: Species intercept
#' @param b1 Numeric: DBH coefficient 
#' @return Numeric: Survival probability modifier (0-1)
tsurv_hw_mod = function(dbh, bal, ba.plot, form, b0, b1) {
  
  # Convert NHRI form classes
  if (form == 'F1') {
    new.form = 'STM'
  } else if (form == 'F2') {
    new.form = 'SWP'
  } else if (form %in% c('F5', 'F8')) {
    new.form = 'MST'
  } else {
    new.form = 'OTHER'
  }
  
  # Set form indicators
  stm = ifelse(new.form == 'STM', 1, 0)
  swp = ifelse(new.form == 'SWP', 1, 0)
  
  
  hw.surv.mod = (exp(15.1991 - 0.1509 * dbh - 0.1232 * bal - 1.4053 * sqrt(ba.plot) +
                       b0 + b1 * dbh + 3.3082 * stm + 2.2518 * swp) /
                   (1 + exp(15.1991 - 0.1509 * dbh - 0.1232 * bal - 1.4053 * sqrt(ba.plot) +
                              b0 + b1 * dbh + 3.3082 * stm + 2.2518 * swp)))/
    (exp(15.1991 - 0.1509 * dbh - 0.1232 * bal - 1.4053 * sqrt(ba.plot) +
           b0 + b1 * dbh + 3.3082) /  
       (1 + exp(15.1991 - 0.1509 * dbh - 0.1232 * bal - 1.4053 * sqrt(ba.plot) +
                  b0 + b1 * dbh + 3.3082)))
  
  
  hw.surv.mod
  
}

#' Plot thinning survival modifier
#' 
#' @param year.thin Numeric: Commercial thinning year
#' @param year Numeric: Current year
#' @param ba.perc.rmv Numeric: Percent basal area removed (proportion 0-1)
#' @param ba.pre.thin Numeric: Pre-thinning basal area (m^2 per ha)
#' @return Numeric: Survival probability multiplier (>=1)
ssurv_thin_mod = function(year.thin, year, ba.perc.rmv, ba.pre.thin) {
  
  tst = ifelse(is.na(year.thin), 0, year - year.thin)
  
  # Parameter estimates
  y30 = 8.3385
  y31 = -601.3096
  y32 = 0.5507
  y33 = 1.5798
  
  thin.mod = ifelse(!is.na(year.thin) & year.thin <= year,
                    1.0 + exp(y30 + (y31 / ((100 * (ba.perc.rmv) + ba.pre.thin) + 0.01))) * y32^tst * tst^y33,
                    1.0)
  
  thin.mod
}

#' Tree thinning survival modifier
#' 
#' @param sp Character: Species code ('BF', 'RS')
#' @param ba.perc.rmv Numeric: Percent basal area removed (proportion 0-1)
#' @param ba.pre.thin Numeric: Pre-thinning basal area (m^2 per ha)
#' @param qmd.ratio Numeric: QMD ratio before and after thinning 
#' @param year.thin Numeric: Commercial thinning year
#' @param year Numeric: Current year
#' @return Numeric: Survival probability multiplier (>=1)
tsurv_thin_mod = function(sp, ba.perc.rmv, ba.pre.thin, qmd.ratio, year.thin, year) {
  
  if(!sp %in% c('BF', 'RS')){
    return(1)
  }
  
  tst = ifelse(is.na(year.thin), 0, year - year.thin)
  
  # Species parameter estimates
  if (sp == 'BF') {
    y0 = 1.7414
    y1 = 7.0805
    y2 = 0.6677
    y3 = 0.8474
    
    thin.mod = ifelse((!is.na(year.thin) & year.thin <= year),
                      1 + (exp(y0 + (y1 / (((100 * ba.perc.rmv + ba.pre.thin) * qmd.ratio) + 0.01))) * y2^tst * tst^y3),
                      1)
  } else if (sp == 'RS') {
    y0 = 10.5057
    y1 = -650.8260
    y2 = 0.6948
    y3 = 0.6429
    
    # RS doesn't include * qmd.ratio
    thin.mod = ifelse((!is.na(year.thin) & year.thin <= year),
                      1 + (exp(y0 + (y1 / ((100 * ba.perc.rmv + ba.pre.thin) + 0.01))) * y2^tst * tst^y3),
                      1)
  } 
  
  
  thin.mod
}

#' Calculate mortality and modifiers for tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data
#' @param region Character: Region code; options are ME, NB, NS, QC (default stand$region)
#' @param use.sbw.mod Logical: Apply SBW outbreak modifiers (default ops$use.sbw.mod)
#' @param use.hw.mod Logical: Apply hardwood form and risk modifiers (default ops$use.hw.mod)
#' @param thin.ba.perc.rmv Numeric: Plot basal area percent removed (percent; default ops$thin.ba.perc.rmv)
#' @param thin.ba.pre Numeric: Plot pre-thin basal area (m^2 per ha; default ops$thin.ba.pre)
#' @param thin.qmd.ratio Numeric: Thinning QMD ratio. Before thinning QMD / After thinning QMD (default ops$thin.qmd.ratio)
#' @param thin.year Numeric: Thinning year (year; default ops$thin.year)
#' @param use.thin.mod Logical: Apply post-commercial thinning modifiers (default ops$use.thin.mod)
#' @param sbw.cdef Numeric: Spruce budworm outbreak cumulative defoliation (percent; default ops$sbw.cdef)
#' @param sbw.yr Numeric: Initial year of spruce budworm outbreak (year; default ops$sbw.yr)
#' @param sbw.dur Numeric: Duration of spruce budworm outbreak (years; default ops$sbw.dur)
#' @param stand.mort.prob.region.df Dataframe: Plot mortality probability region parameters dataframe (default stand.mort.prob.region)
#' @param stand.mort.ba.region.df Dataframe: Plot mortality basal area region parameters dataframe (default stand.mort.ba.region)
#' @param smort.sbw.region.df Dataframe: Plot SBW mortality modifier region parameters dataframe (default smort.sbw.region)
#' @param tsurv.sbw.region.spp.df Dataframe: Tree-level SBW survival modifier region-species parameters dataframe (default tsurv.sbw.region.spp)
#' @return Dataframe: Tree data with mortality calculations
calc_mortality = function(tree.data, plot.data, region = stand$region, 
                          use.sbw.mod = ops$use.sbw.mod,
                          use.hw.mod = ops$use.hw.mod, 
                          use.thin.mod = ops$use.thin.mod, 
                          thin.ba.perc.rmv = ops$thin.ba.perc.rmv,
                          thin.ba.pre = ops$thin.ba.pre,
                          thin.qmd.ratio = ops$thin.qmd.ratio,
                          thin.year=ops$thin.year,
                          sbw.cdef = ops$sbw.cdef,
                          sbw.yr = ops$sbw.yr,
                          sbw.dur = ops$sbw.dur,
                          tsurv.sbw.region.spp.df=tsurv.sbw.region.spp,
                          stand.mort.prob.region.df = stand.mort.prob.region,
                          stand.mort.ba.region.df = stand.mort.ba.region,
                          smort.sbw.region.df = smort.sbw.region) {
  
  # tree list names
  tree.data.names= colnames(tree.data)
  
  # Calculate BA growth for top 30% of trees
  tree = tree.data %>%
    dplyr::arrange(plot, 
                   desc(dbh)) %>%
    dplyr::group_by(plot) %>%
    dplyr::mutate(cum.sdi.ward = cumsum(expf * (0.00015 + 0.00218 * sg) * ((dbh / 25)^1.605)),
                  ba.growth.tree = ((dbh + ddbh)^2 * 0.00007854 - dbh^2 * 0.00007854) * expf,
                  ba.growth.top30 = sum(ifelse(cum.sdi.ward <= 0.3, ba.growth.tree, 0))) %>%
    dplyr::ungroup()
  
  # additional plot summary values
  plot.bf = tree.data %>%
    dplyr::group_by(plot) %>%
    # plot balsam fir basal area
    dplyr::summarise(ba.bf = sum(ifelse(sp == 'BF', ba, 0), na.rm = TRUE),
                     tph.bf = sum(ifelse(sp == 'BF', expf, 0), na.rm = TRUE)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(qmd.bf = sqrt(ba.bf/(0.00007854*tph.bf)))
  
  # Regional parameters for plot mortality probability
  stand.prob.idx = match(region, stand.mort.prob.region.df$region, 
                         nomatch = match('ME', stand.mort.prob.region.df$region)) # default Maine
  stand.prob.parms = stand.mort.prob.region.df[stand.prob.idx, ]
  
  # Regional parameters for plot mortality basal area
  stand.ba.idx = match(region, stand.mort.ba.region.df$region,
                       nomatch = match('ME', stand.mort.ba.region.df$region)) # default Maine
  stand.ba.parms = stand.mort.ba.region.df[stand.ba.idx, ]
  
  # Join tree and plot summary data 
  tree = tree %>%
    dplyr::left_join(plot.data %>% 
                       dplyr::select(plot, ba.plot, qmd, ba.ihw, dbh.sw.avg, ht.sw.avg, topht), 
                     by = 'plot') %>%
    dplyr::left_join(plot.bf, 
                     by = 'plot') %>%
    # Stand mortality probability
    dplyr::mutate(stand.pmort = purrr::pmap_dbl(list(ba.plot, ba.growth.top30, qmd, ba.bf, ba.ihw,
                                                     stand.prob.parms$b0, stand.prob.parms$b1, stand.prob.parms$b2,
                                                     stand.prob.parms$b3, stand.prob.parms$b4, stand.prob.parms$b5,
                                                     stand.prob.parms$b6), 
                                                stand_mort_prob),
                  # Stand mortality basal area
                  stand.mort.ba = purrr::pmap_dbl(list(ba=ba.plot, bag=ba.growth.top30, 
                                                       qmd=qmd, qmd.bf=qmd.bf, ba.bf=ba.bf, 
                                                       ba.ihw=ba.ihw,
                                                       stand.ba.parms$b0, stand.ba.parms$b1, stand.ba.parms$b2,
                                                       stand.ba.parms$b3, stand.ba.parms$b4, stand.ba.parms$b5,
                                                       stand.ba.parms$b6, stand.ba.parms$b7),
                                                  stand_mort_ba))
  
  # Post-thinning modifier
  if (use.thin.mod == TRUE &
      !is.na(thin.ba.perc.rmv) &
      !is.na(thin.ba.pre) &
      !is.na(thin.qmd.ratio) &
      !is.na(thin.year)) {
    
    # Calculate stand-level thinning modifier
    tree = tree %>%
      dplyr::mutate(smort.thin.mod = purrr::pmap_dbl(list(thin.year, year, thin.ba.perc.rmv, thin.ba.pre),
                                                     ssurv_thin_mod))
    
    # Calculate tree-level thinning modifier
    tree = tree %>%
      dplyr::mutate(tsurv.thin.mod = purrr::pmap_dbl(list(sp, thin.ba.perc.rmv, thin.ba.pre, thin.qmd.ratio, thin.year, year),
                                                     tsurv_thin_mod))
  } else {
    tree = tree %>%
      dplyr::mutate(smort.thin.mod = 1,
                    tsurv.thin.mod = 1)
  }
  
  #Spruce budworm modifier
  if (use.sbw.mod == TRUE & 
      region %in% c('ME', 'NB') &
      !is.na(sbw.cdef) & sbw.cdef > 0 &
      !is.na(sbw.yr) & sbw.yr<=tree$year[1] &
      !is.na(sbw.dur) & tree$year[1]<=sbw.yr+sbw.dur)  {
    
    # Calculate plot SBW modifier
    # SBW region parameters 
    sbw.idx = match(region, smort.sbw.region$region, 
                    nomatch = match('ME', smort.sbw.region$region))
    sbw.plot.parms = smort.sbw.region[sbw.idx, ]
    
    tree = tree %>%
      dplyr::mutate(smort.sbw.mod = purrr::pmap_dbl(list(ba.plot, ba.bf, topht, sbw.cdef,
                                                         sbw.plot.parms$b1, sbw.plot.parms$b2, 
                                                         sbw.plot.parms$b3, sbw.plot.parms$b4),
                                                    smort_sbw_mod))
    
    # Calculate tree-level SBW modifier
    # SBW region-species parameters
    sel.region=ifelse(region %in% tsurv.sbw.region.spp$region, region, 'ME')
    
    sbw.tree.parm = tsurv.sbw.region.spp.df %>% 
      dplyr::filter(region == sel.region)
    
    tree = tree %>%
      # in this case join() is probably easier than match()
      dplyr::left_join(sbw.tree.parm, 
                       by = 'sp') %>%
      dplyr::mutate(tsurv.sbw.mod = ifelse(sp %in% c('BF', 'RS', 'BS', 'WS') & !is.na(b1),
                                           purrr::pmap_dbl(list(dbh=dbh, cr=cr, ht=ht, 
                                                                bal.hw=bal.hw, bal.sw=bal.sw,
                                                                ht.sw.avg=ht.sw.avg, sbw.cdef=sbw.cdef,
                                                                b1, b2, b3, b4, b5, b6, b7, b8),
                                                           tsurv_sbw_mod),
                                           1)) %>%
      dplyr::select(-b1, -b2, -b3, -b4, -b5, -b6, -b7, -b8)
    
  } else {
    tree = tree %>%
      dplyr::mutate(smort.sbw.mod = 1,
                    tsurv.sbw.mod = 1)
  }
  
  
  # Hardwood form and risk modifier
  if (use.hw.mod == TRUE) {
    tree = tree %>%
      dplyr::left_join(tsurv.hw.spp, 
                       by = 'sp') %>%
      dplyr::mutate(tsurv.hw.mod = ifelse(!is.na(form) & gsub('[^0-9]', '', form) %in% 1:8 & sp %in% c('RO', 'YB', 'RM', 'PB', 'QA') & !is.na(b0),
                                          purrr::pmap_dbl(list(dbh, bal, ba.plot, form, b0, b1), 
                                                          tsurv_hw_mod),
                                          1)) %>%
      dplyr::select(-b0, -b1)
  } else {
    tree = tree %>%
      dplyr::mutate(tsurv.hw.mod = 1)
  }
  
  # Apply stand mortality basal area modifiers 
  tree = tree %>%
    dplyr::mutate(stand.mort.ba = ifelse(stand.pmort > stand.prob.parms$cut,
                                         stand.mort.ba * (1 / smort.thin.mod) * smort.sbw.mod,
                                         0),
                  stand.mort.ba = pmin(stand.mort.ba, ba.plot))
  
  # Get tree mortality species parameters and calculate survival
  tree = tree %>%
    dplyr::mutate(mort.idx = match(sp, tree.mort.spp$species, nomatch = match('99', tree.mort.spp$species)),
                  b0 = tree.mort.spp$b0[mort.idx],
                  b1 = tree.mort.spp$b1[mort.idx], 
                  b2 = tree.mort.spp$b2[mort.idx],
                  surv.tree = tree_surv_prob(sp, dbh, b0, b1, b2)) %>%
    dplyr::select(-mort.idx, -b0, -b1, -b2)
  
  # Apply tree level mortality modifiers
  tree = tree %>%
    dplyr::mutate(tsurv = pmax(0, pmin(surv.tree *
                                         tsurv.thin.mod * tsurv.sbw.mod * tsurv.hw.mod, 
                                       1.0)),
                  tmort = 1-tsurv)
  
  # Calculate tree mortality basal area and scale to stand basal area mortality
  tree = tree %>%
    dplyr::mutate(tree.mortba = ((dbh + ddbh)^2 * 0.00007854) * expf * tmort) %>%
    dplyr::group_by(plot) %>%
    dplyr::mutate(sum.tree.mortba = sum(tree.mortba)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mort.ratio = ifelse(stand.mort.ba > 0, sum.tree.mortba / stand.mort.ba, 0),
                  tree.mortba.adj = ifelse(stand.mort.ba > 0, tree.mortba / mort.ratio, 0),
                  ba.tree = 0.00007854 * (dbh + ddbh)^2,   
                  dexpf = ifelse(stand.mort.ba > 0, tree.mortba.adj / ba.tree, 0) * mort.mult) # apply mortality multiplier
  
  # Remove temporary columns
  tree = tree %>%
    dplyr::select(dplyr::all_of(tree.data.names), dexpf)
  
  tree
}

#### Calibration ####    
# multipliers for diameter increment, height increment and mortality
# maximum tree diameter and height
    
  # default tree size limits from northern New England FIA data and national champion tree data
    
    tree.size.cap=dplyr::tribble(
      ~species, ~max.dbh, ~max.height,
      'AB',	  56.1,	    101,
      'AE',	  61.7,	    100.5,
      'AH',	  38.4,	    54.5,
      'AP',	  39.4,	    53,
      'BA',	  36.3,	    81.5,
      'BC',	  51.1,	    81,
      'BE',	  47.6,	    64,
      'BF',	  45.6,	    99,
      'BN',	  60.3,	    80.5,
      'BO',	  67.9,	    94,
      'BP',	  25.7,	    97,
      'BR',	  61.4,	    101.3,
      'BS',	  22.6,	    84,
      'BT',	  32.7,	    113.9,
      'BW',	  63.2,	    101.5,
      'EC',	  24.3,	    112,
      'EH',	  37.7,	    115,
      'GA',	  51.0,	    89,
      'GB',	  22.9,	    68,
      'HH',	  23.2,	    73,
      'HT',	  10.7,	    31,
      'JP',	  20.1,	    69.1,
      'NS',	  46.5,	    113.5,
      'PB',	  33.0,	    91.5,
      'PP',	  39.6,	    96,
      'PR',	  18.2,	    66.5,
      'QA',	  27.8,	    104,
      'RM',	  63.5,	    98.8,
      'RN',	  30.4,	    101,
      'RO',	  87.1,	    104.5,
      'RS',	  44.6,	    115.5,
      'SB',	  26.4,	    98,
      'SE',	  12.1,	    63,
      'SH',	  42.8,	    94.5,
      'SM',	  63.4,	    109.3,
      'SO',	  44.9,	    109,
      'ST',	  15.1,	    55.5,
      'SV',	  63.9,	    94.5,
      'TA',	  35.9,	    85.5,
      'TM',	  21.3,	    93.2,
      'WA',	  67.6,	    118.5,
      'WC',	  47.1,	    103,
      'WO',	  72.7,	    92.5,
      'WP',	  67.9,	    130,
      'WS',	  30.1,	    101.5,
      'YB',	  61.4,	    82,
      'YP',	  124.1,	    100,
      '99',	  63.5,	    98.8, # RM values	
      'OH',	  28.7,     98.8, # RM max height value		
      'OS',	  22.6,	    84 )	# BS values
   

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
                                              max.dbh, 
                                              tree.size.cap$max.dbh[tree.size.cap$species=='99']) * in.to.cm, # metric conversion # FVS or default
                      max.height=dplyr::coalesce(maxht.fvs, 
                                                 max.height, 
                                                 tree.size.cap$max.height[tree.size.cap$species=='99']) * ft.to.m,
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
  
    # Ingrowth composition species group parameters
    ingrowth.comp.spp = dplyr::tribble(
      ~group, ~b0,      ~b1,      ~b2,    ~b3,      ~b4,
      "BCH",  -2.5645,  0.0020,   2.6624, -0.0010,  -0.0127,
      "BF",   -3.0291,  0.0027,   2.7779,  0.0211,   0.0221,
      "RM",   -0.6566,  0.0123,   1.7669, -0.0421,  -0.0283,
      "SPR",  -1.2500, -0.0132,   2.0470, -0.0514,   0.0351,
      "WP",   -5.1074, -0.0117,   3.8817,  0.0501,   0.0726,
      "OH",   -2.9832, -0.0020,   2.4837,  0.0673,  -0.0167,
      "OS",   -4.7182,  0.0070,   3.2269,  0.1000,   0.0188,
      "AB",   -2.9832, -0.0020,   2.4837,  0.0673,  -0.0167,
      "QA",   -2.9832, -0.0020,   2.4837,  0.0673,  -0.0167,
      "SM",   -2.9832, -0.0020,   2.4837,  0.0673,  -0.0167,
      "WC",   -4.7182,  0.0070,   3.2269,  0.1000,   0.0188
    )
    
    # Ingrowth probability model parameters
    ingrowth.prob.parms = dplyr::tribble(
      ~model.type, ~a1,       ~a2,       ~a3,       ~a4,       ~a5,      ~a6,      ~a7,       ~b1,      ~b2,       ~b3,       ~b4,        ~b5,      ~b6,       ~b7,
      "GNLS",      -0.2116,   -0.0255,   -0.1396,   -0.0054,   0.0433,   0.0409,   0.0,       3.8982,   -0.0257,   -0.3668,   0.0002,     0.0216,   -0.0514,   0.0,
      "NLME",      -0.08217,   0.1113,   -1.2405,   -0.2319,   0.03673,  -0.7745,  -0.1301,   2.8466,   -0.03114,  -0.2891,   0.003350,   0.2248,   -0.08223,  -0.03548)
    
    # Li, R; Weiskittel, AR; Kershaw, JA. 2011. Modeling annualized occurrence, frequency, and composition of ingrowth using 
    # mixed-effects zero-inflated models and permanent plots in the Acadian Forest Region of North America. Canadian Journal 
    # of Forest Research 41(10): 2077-2089.
    
#' Calculate probability of ingrowth
#' 
#' @param ba Numeric: Plot basal area (m^2 per ha)
#' @param tph Numeric: Trees per hectare
#' @param qmd Numeric: Quadratic mean diameter (cm)
#' @param ba.perc.hw Numeric: Percent hardwood basal area (proportion 0-1)
#' @param min.dbh.ingrowth Numeric: Minimum DBH threshold for ingrowth (cm)
#' @param cut.point.ingrowth Numeric: Probability threshold where ingrowth will occur (default 0.95)
#' @param csi Numeric: Climate site index (m)
#' @param a1 Numeric: Parameter a1
#' @param a2 Numeric: Parameter a2
#' @param a3 Numeric: Parameter a3
#' @param a4 Numeric: Parameter a4
#' @param a5 Numeric: Parameter a5
#' @param a6 Numeric: Parameter a6
#' @param a7 Numeric: Parameter a7
#' @param b1 Numeric: Parameter b1
#' @param b2 Numeric: Parameter b2
#' @param b3 Numeric: Parameter b3
#' @param b4 Numeric: Parameter b4
#' @param b5 Numeric: Parameter b5
#' @param b6 Numeric: Parameter b6
#' @param b7 Numeric: Parameter b7
#' @return Numeric: Ingrowth per hectare (ingrowth.ph)
    prob_ingrowth = function(ba, tph, qmd, ba.perc.hw, min.dbh.ingrowth, 
                             cut.point.ingrowth=0.95, csi, 
                             a1, a2, a3, a4, a5, a6, a7,
                             b1, b2, b3, b4, b5, b6, b7) {
      
      # Calculate link1
      link1 = a1 + a2 * ba + a3 * ba.perc.hw + a4 * (tph / 1000) + 
        a5 * csi + a6 * min.dbh.ingrowth + a7 * qmd
      
      # Calculate probability
      ingrowth.prob = 1 / (1 + exp(-link1))
      
      # Calculate eta
      eta = b1 + b2 * ba + b3 * ba.perc.hw + b4 * (tph / 1000) + 
        b5 * csi + b6 * min.dbh.ingrowth + b7 * qmd
      
      # Calculate ingrowth per hectare
      ingrowth.ph = exp(eta)
      
      # Apply probability threshold
      ingrowth.ph = ifelse(ingrowth.prob >= cut.point.ingrowth, ingrowth.ph, 0)
      
      ingrowth.ph
    }
    
#' Calculate species composition ingrowth probability
#' 
#' @param ba Numeric: Plot basal area (m^2 per ha) 
#' @param ba.perc.grp Numeric: Proportion of plot basal area in species group (0-1)
#' @param csi Numeric: Climate site index (m)
#' @param b0 Numeric: Intercept parameter
#' @param b1 Numeric: Parameter b1
#' @param b2 Numeric: Parameter b2
#' @param b3 Numeric: Parameter b3
#' @param b4 Numeric: Parameter b4
#' @param min.dbh.ingrowth Numeric: Minimum DBH threshold for ingrowth (cm)
#' @return Numeric: Species composition probability (0-1)
    comp_ingrowth = function(ba, ba.perc.grp, b0, b1, b2, b3, b4, 
                             csi=stand$csi, min.dbh.ingrowth) {
      
      # Calculate percent  
      sp.perc = b0 + b1 * ba + b2 * ba.perc.grp + b3 * csi + b4 * min.dbh.ingrowth
      
      # Probability
      sp.perc = 1 / (1 + exp(-(sp.perc)))
      
      sp.perc
    }
    
#' Create ingrowth tree list 
#' 
#' @param tree.ingrowth.data Dataframe: Ingrowth data with species-level ingrowth values
#' @param tree.data Dataframe: Tree list
#' @param plot.data Dataframe: Plot summary data
#' @param min.dbh.ingrowth Numeric: Minimum DBH treshold for ingrowth trees (cm)
#' @param sp.attr.data Dataframe: species functional attributes table (default sp.attr)
#' @return Dataframe: Ingrowth tree list
    create_ingrowth_tree = function(tree.data, 
                                    tree.ingrowth.data, 
                                    plot.data, 
                                    min.dbh.ingrowth, sp.attr.data) {
      
      # Break down species groups into individual species
      species.grp.ba = tree.data %>% 
        dplyr::filter(sp %in% c('GB', 'YB', 'PB', 'RS', 'BS', 'WS')) %>%
        dplyr::group_by(plot, 
                        sp.grp.ingrowth, 
                        sp) %>% 
        dplyr::summarise(ba.sp = sum(ba, na.rm = TRUE), 
                         .groups = 'drop')
      
      tree.group.ingrowth = tree.ingrowth.data %>% 
        dplyr::filter(sp.grp.ingrowth %in% c('SPR', 'BCH')) %>% 
        dplyr::left_join(species.grp.ba, 
                         by = c('plot', 'sp.grp.ingrowth')) %>%
        dplyr::mutate(sp.prop = ifelse(sp.ba == 0, 1/n(), ba.sp / sp.ba),
                      sp.ing = sp.ing * sp.prop) %>%
        dplyr::select(-ba.sp, 
                      -sp.prop)
      
      # Ingrowth year
      year.ingrowth=tree.data$year[1]+1
      
      # Combine individual species and group records
      tree.ingrowth = tree.ingrowth.data %>%
        dplyr::filter(!sp.grp.ingrowth %in% c('SPR', 'BCH')) %>%
        dplyr::mutate(sp=sp.grp.ingrowth) %>% 
        dplyr::bind_rows(tree.group.ingrowth)%>% 
        dplyr::filter(sp.ing>0) %>% 
        dplyr::transmute(plot, 
                         tree = max.tree.id + row_number(),
                         sp,
                         dbh = min.dbh.ingrowth,
                         expf = sp.ing,
                         ht = NA_real_,
                         hcb = NA_real_,
                         year = year.ingrowth,
                         ba=(dbh^2*0.00007854)*expf)
      
      # Add predicted heights and crown bases if there are ingrowth records
      if (!is.null(tree.ingrowth) && nrow(tree.ingrowth) > 0) {
        tree.max.bal=tree.data %>% 
          dplyr::group_by(plot) %>% 
          dplyr::summarise(bal.plot.max=max(bal), 
                           .groups = 'drop')
        
        tree.ingrowth = tree.ingrowth %>%
          dplyr::left_join(sp.attr, 
                           by=c('sp'='species')) %>% # calc BAL needs sp.type for HW/SW
          calc_bal() %>% 
          dplyr::left_join(tree.max.bal, 
                           by = 'plot') %>% 
          dplyr::mutate(bal=bal + dplyr::coalesce(bal.plot.max, 0)) %>% # future proof - coalesce() to handle cases where we have plots with no existing tree records
          calc_ht(tree.data=., plot.data = plot.data) %>%
          dplyr::mutate(ht = pht) %>% 
          calc_hcb(tree.data=., plot.data = plot.data) %>%
          dplyr::mutate(hcb = phcb,
                        cr=1-(phcb/ht))
      }
      
      tree.ingrowth
    }
    
#' Ingrowth calculation wrapper function
#' 
#' @param tree.data Dataframe: Tree list 
#' @param plot.data Dataframe: Plot summary data
#' @param ingrowth.prob.parms.df Dataframe: Ingrowth probability model parameters df
#' @param csi Numeric: Climate site index (m; default=stand$csi)
#' @param min.dbh.ingrowth Numeric: Minimum DBH threshold for ingrowth (cm; default=ops$ingrowth.min.dbh)
#' @param cut.point.ingrowth Numeric: Probability threshold for ingrowth occurrence (default ops$ingrowth.thrshld)
#' @param sp.attr.data Dataframe: species functional attributes table (default sp.attr)
#' @param ingrowth.comp.spp.df Dataframe: Ingrowth composition species group parameters dataframe (default ingrowth.comp.spp)
#' @param model.type Character: Parameter estimates for prob_ingrowth(). Choices are NLME and GNLS (default 'GNLS')
#' @return Dataframe: Ingrowth tree list
    calc_ingrowth = function(tree.data, 
                             plot.data,
                             ingrowth.prob.parms.df = ingrowth.prob.parms,
                             csi = stand$csi, 
                             min.dbh.ingrowth=ops$ingrowth.min.dbh, 
                             cut.point.ingrowth = ops$ingrowth.thrshld, 
                             sp.attr.data = sp.attr,
                             ingrowth.comp.spp.df = ingrowth.comp.spp, 
                             model.type = 'GNLS') {
      
      # Required columns in plot.data
        # 'plot', 'ba.plot', 'tph.plot', 'qmd', 'ba.perc.hw', 'max.tree.id')
     
      # Get parameters for specified model
      parms = dplyr::filter(ingrowth.prob.parms.df, 
                            model.type == !!model.type)
      
      # Calculate species group basal areas (sp.attr$sp.grp.ingrowth)
      species.ba = tree.data %>% 
        dplyr::group_by(plot, 
                        sp.grp.ingrowth) %>% 
        dplyr::summarise(sp.ba = sum(ba, na.rm = TRUE), 
                         .groups = 'drop')
      
      # Calculate ingrowth per hectare at plot level
      plot.data = plot.data %>%
        dplyr::mutate(ingrowth.ph = prob_ingrowth(ba = ba.plot,
                                                  tph = tph.plot,
                                                  qmd = qmd,
                                                  ba.perc.hw = ba.perc.hw,
                                                  min.dbh.ingrowth = min.dbh.ingrowth,
                                                  cut.point.ingrowth = cut.point.ingrowth,
                                                  csi = csi,
                                                  a1 = parms$a1, a2 = parms$a2, a3 = parms$a3, a4 = parms$a4,
                                                  a5 = parms$a5, a6 = parms$a6, a7 = parms$a7,
                                                  b1 = parms$b1, b2 = parms$b2, b3 = parms$b3, b4 = parms$b4,
                                                  b5 = parms$b5, b6 = parms$b6, b7 = parms$b7))
      
      # Ingrowth species groups
      species.groups = c('BF', 'RM', 'WP', 'SPR', 'BCH', 'OH', 
                         'OS', 'AB', 'QA', 'SM', 'WC')
      
      # Calculate species group ingrowth 
      tree.ingrowth = plot.data %>%
        #tidyr::crossing(sp.grp.ingrowth = species.groups) %>% include all possible species
        dplyr::left_join(species.ba, 
                         by = c('plot')) %>%
        # by = c('plot', 'sp.grp.ingrowth')) %>% # when using all possible species
        dplyr::mutate(sp.ba = ifelse(is.na(sp.ba), 0, sp.ba),
                      ba.perc.grp = ifelse(ba.plot == 0, 0, sp.ba / ba.plot),
                      idx = match(sp.grp.ingrowth, ingrowth.comp.spp$group),
                      b0 = ingrowth.comp.spp$b0[idx],
                      b1 = ingrowth.comp.spp$b1[idx],
                      b2 = ingrowth.comp.spp$b2[idx],
                      b3 = ingrowth.comp.spp$b3[idx],
                      b4 = ingrowth.comp.spp$b4[idx],
                      comp.prob = purrr::pmap_dbl(list(ba.plot, 
                                                       ba.perc.grp, 
                                                       b0, b1, b2, b3, b4,
                                                       csi,
                                                       min.dbh.ingrowth), 
                                                  comp_ingrowth),
                      year = first(tree.data$year)) %>%
        dplyr::select(-idx) %>%
        dplyr::group_by(plot) %>%
        dplyr::mutate(total.comp.prob = sum(comp.prob, na.rm = TRUE),
                      comp.prob.norm = ifelse(total.comp.prob == 0, 
                                              0, 
                                              comp.prob / total.comp.prob),
                      sp.ing = ingrowth.ph * comp.prob.norm) %>%
        dplyr::ungroup()
      
      # Generate tree list
      tree.ingrowth = create_ingrowth_tree(tree.data, 
                                           tree.ingrowth, 
                                           plot.data,
                                           min.dbh.ingrowth,
                                           sp.attr.data)
      
      tree.ingrowth
    }
   
#### Prepare input tree list ####
####

## define model species  
  acd.species.ht.dia=ddbh.spp %>% 
    dplyr::inner_join(dht.spp, by='species') %>% 
    dplyr::select(species)

## For tree list from FVS add FVS alpha species codes and identify records with species outside scope of the model
    #' Prepare tree list using FVS tree list 
    #' 
    #' @param tree.data Dataframe: tree list from FVS. Required tree list fields: plot, species (fvs numeric species code), tpa, dbh, ht, cratio, mgmtcd, special (used in form, Risk)
    #' @param spcodes Dataframe: FVS species codes. Required fields: fvs (FVS alpha species code). FVS numeric code is the vector "row" number. Note fvsGetSpeciesCodes() returns a character vector 
    #' @param acd.species Dataframe: Acadian model species. Default species contained in the diameter and height increment parameter estimate tables (acd.species$sp)
    #' @return Dataframe: Tree list with added columns (sp: FVS alpha speies code and acd.ex: indicator value to drop records when returning to FVS)
    #'
     validate_acd_tree_spp=function(tree.data, spcodes, acd.species=acd.species.ht.dia$species){
    
    # retain records in the projections for accurate plot values and change species to OH
    
    tree.list=tree.data %>% 
      dplyr::rename(fvs.num= species) %>%
      dplyr::left_join(spcodes %>% 
                         as.data.frame() %>% 
                         transmute(sp=fvs,
                                   fvs.num=as.integer(rownames(.))),
                       by='fvs.num') %>%
        dplyr::mutate(acd.ex=dplyr::case_when(!sp %in% acd.species ~TRUE, # indicator value to drop records when returning to FVS
                                              TRUE ~ FALSE),
                      sp=dplyr::case_when(!sp %in% acd.species ~'OH', # assign species code OH
                                          TRUE ~ sp))                                            
      
      tree.list  
  }
 

## drop invalid tree records not handled by ACD (snags, dbh=0)
     #' Filter tree records with DBH=0 and snags
     #' @param tree.data Dataframe: tree list from FVS. Required tree list fields: mgmtcd, sp (FVS alpha species code), dbh
     #' @param acd.species Dataframe: Acadian model species. Default species contained in the diameter and height increment parameter estimate tables (acd.species$sp)
     #' @return Dataframe: Tree list 
  validate_acd_tree_status=function(tree.data){
     
    tree.list=tree.data %>% 
      dplyr::filter(mgmtcd!=9, # remove snags from tree list
                    dbh>0) # remove tree records with dbh zero or NULL
                   
    
    tree.list
    
  } 

  
## create Acadian model input dataframe from FVS tree list
  #' Create ACD tree list dataframe
  #' 
  #' @param tree.list Dataframe: Tree list from FVS. Required tree.list fields: plot, species (fvs numeric species code), tpa, dbh, ht, cratio, mgmtcd, special (used in Form, Risk)
  #' @param num.plots Numeric: Number of plots in a stand
  #' @param calib.spp Dataframe: Data frame of species calibration and size limits. Required fields: sp (FVS alpha species code), ddbh.mult, dht.mult, mort.mult, max.dbh,  max.height
  #' @return Dataframe: Tree list dataframe
  # Note: FVS-NE dg; htg and mort=MORT remain in customary units
  
make_acd_tree=function(tree.list, num.plots, calib.spp){
 
   # customary-metric conversion
  in.to.cm = 2.54
  ft.to.m = 0.3048
  ha.to.ac = 2.47105
  
  tree.list.vars=c('cratio', 'dbh', 'ht', 'special', 'sp')
  
  # stop if tree list is missing required variables
  
  # if(all(tree.list.vars %in% names(tree.list))==FALSE){ 
  #   stop('Required tree list variable missing')
  #   message(setdiff(tree.list.vars, names(tree.list)))
  # }
  
  tree.list=tree.list %>% 
    dplyr::rename_with(.fn=tolower) %>% 
    dplyr::rename(cr= cratio,
                  expf= tpa) %>%
    dplyr::mutate(tree=seq.int(1:n()), # sequential tree id used to retain order of tree list from fvs
                  cr = abs(cr) * 0.01,
                  #change cr to a proportion and take abs; note that in FVS a negative cr
                  #signals that cr change has been computed by the fire or insect/disease model
                  dbh  = dbh  * in.to.cm, # metric conversion
                  ht   = ht   * ft.to.m,
                  hcb = ht-cr*ht,
                  expf = expf * dplyr::coalesce(num.plots, 1) * ha.to.ac, # each plot as "stand"
                  form = ifelse(special > 0 & special < 85, 
                                paste0("F", as.integer(special %/% 10)), 
                                NA_character_),
                  risk = ifelse(special > 0 & special < 85, 
                                paste0("R", as.integer(special %%  10)),
                                NA_character_)) %>% 
    dplyr::left_join(calib.spp,
                     by='sp')
  
    tree.list
  }

#' Validate tree dataframe
#' 
#' Validates that the tree dataframe contains all required fields with appropriate
#' types and values for processing in AcadianGYOneStand().
#' 
#' @param tree Dataframe: Tree dataframe with required variables
#' @return Logical: TRUE if validation passes
#' @details
#' Validates:
#' - All required fields are present (year, plot, sp, dbh, expf, ht, hcb, cr, form, risk)
#' - Required fields are not NA (year, plot, sp, dbh, expf)
#' - Numeric fields are numeric type (year, dbh, ht, hcb, cr, expf)
#' - Character fields are character type (sp, form, risk)
#' @examples
#' tree = data.frame(year = 2024, plot = 1, sp = 'BF', dbh = 20, expf = 100, 
#'                   ht = 15, hcb = 5, cr = 0.67, form = NA, risk = NA)
#' validate_tree(tree)
#'
validate_tree = function(tree) {
  
  required.fields = c('year', 'plot', 'sp', 'dbh', 'expf', 'ht', 'hcb', 'cr', 'form', 'risk')
  
  missing.fields = setdiff(required.fields, names(tree))
  
  if (length(missing.fields) > 0) {
    stop('tree dataframe missing required fields: ', 
         paste(missing.fields, collapse = ', '))
  }
  
  numeric.fields = c('year', 'dbh', 'ht', 'hcb', 'cr', 'expf')
  invalid.numeric = numeric.fields[!sapply(tree[numeric.fields], is.numeric)]
  
  if (length(invalid.numeric) > 0) {
    stop('tree fields must be numeric: ', 
         paste(invalid.numeric, collapse = ', '))
  }
  
  character.fields = c('sp', 'form', 'risk')
  invalid.character = character.fields[!sapply(tree[character.fields], is.character)]
  
  if (length(invalid.character) > 0) {
    stop('tree fields must be character: ', 
         paste(invalid.character, collapse = ', '))
  }
  
  required.populated = c('year', 'plot', 'sp', 'dbh', 'expf')
  
  all.na.fields = required.populated[sapply(tree[required.populated], function(x) all(is.na(x)))]
  
  if (length(all.na.fields) > 0) {
    stop('tree dataframe fields have all NA values: ', 
         paste(all.na.fields, collapse = ', '))
  }
  
  any.na.fields = required.populated[sapply(tree[required.populated], function(x) any(is.na(x)))]
  
  if (length(any.na.fields) > 0) {
    stop('tree dataframe fields have some NA values: ', 
            paste(sapply(any.na.fields, function(f) {
              paste0(f, ' (', sum(is.na(tree[[f]])), ')')
            }), collapse = ', '))
  }
  
  TRUE
}

#### Prepare model options (ops) ####
#' Create ACD run options dataframe
#' 
#' @param verbose Logical or character: Print verbose output. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default FALSE)
#' @param rtn.vars Character vector: Variables to return in output (default core variables)
#' @param use.cap.dbh Logical or character: Apply maximum DBH constraint. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default TRUE).
#' @param use.cap.ht Logical or character: Apply maximum total tree height constraint. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default TRUE)
#' @param use.sbw.mod Logical or character: Apply SBW outbreak modifiers. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default FALSE)
#' @param sbw.cdef Numeric: Spruce budworm outbreak cumulative defoliation (percent; default NA)
#' @param sbw.yr Numeric: Initial year of spruce budworm outbreak (year; default NA)
#' @param sbw.dur Numeric: Duration of spruce budworm outbreak (years; default NA)
#' @param use.thin.mod Logical or character: Apply post-commercial thinning modifiers. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default TRUE)
#' @param thin.ba.perc.rmv Numeric: Plot basal area percent removed (percent; default NA)
#' @param thin.ba.pre Numeric: Plot pre-thin basal area (default NA)
#' @param thin.qmd.ratio Numeric: Thinning QMD ratio. Before thinning QMD / After thinning QMD (default NA)
#' @param thin.year Numeric: Thinning year (default NA)
#' @param use.hw.mod Logical or character: Apply hardwood form and risk modifiers. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default FALSE)
#' @param use.ingrowth Logical or character: Use ingrowth sub-model. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default TRUE)
#' @param ingrowth.min.dbh Numeric: Minimum DBH threshold for ingrowth (cm; default 3.0)
#' @param ingrowth.thrshld Numeric: Ingrowth threshold (default 0.95)
#' @param use.vol Logical or character: Use Acadian volume calculation. Accepts TRUE/FALSE or 'Yes'/'No'/'Y'/'N' (default FALSE)
#' @return Dataframe: Run options dataframe
#'
make_ops = function(verbose = FALSE,
                    rtn.vars = c('year', 'plot', 'tree', 'sp', 'dbh', 'ht', 
                                 'hcb', 'cr', 'expf', 'pht', 'phcb', 'form', 'risk', 
                                 'ddbh.mult', 'dht.mult', 'mort.mult', 'max.dbh', 'max.height'),
                    use.cap.dbh = TRUE,
                    use.cap.ht = TRUE,
                    use.sbw.mod = FALSE,
                    sbw.cdef = NA_real_,
                    sbw.yr = NA_real_,
                    sbw.dur = NA_real_,
                    use.thin.mod = FALSE,
                    thin.ba.perc.rmv = NA_real_,
                    thin.ba.pre = NA_real_,
                    thin.qmd.ratio = NA_real_,
                    thin.year = NA_real_,
                    use.hw.mod = FALSE,
                    use.ingrowth = FALSE,
                    ingrowth.min.dbh = 3.0,
                    ingrowth.thrshld = 0.95,
                    use.vol = FALSE) {
  
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
                   rtn.vars = I(list(rtn.vars)),
                   # rtn.vars = I(rtn.vars),
                   use.cap.dbh = char_to_logical(use.cap.dbh),
                   use.cap.ht = char_to_logical(use.cap.ht),
                   use.sbw.mod = char_to_logical(use.sbw.mod),
                   sbw.cdef = sbw.cdef,
                   sbw.yr = sbw.yr,
                   sbw.dur = sbw.dur,
                   use.thin.mod = char_to_logical(use.thin.mod),
                   thin.ba.perc.rmv = thin.ba.perc.rmv,
                   thin.ba.pre = thin.ba.pre,
                   thin.qmd.ratio = thin.qmd.ratio,
                   thin.year = thin.year,
                   use.hw.mod = char_to_logical(use.hw.mod),
                   use.ingrowth = char_to_logical(use.ingrowth),
                   ingrowth.min.dbh = ingrowth.min.dbh,
                   ingrowth.thrshld = ingrowth.thrshld,
                   use.vol=use.vol) %>%
    dplyr::mutate(dplyr::across(c(verbose, use.cap.dbh, use.cap.ht, use.sbw.mod, 
                                  use.thin.mod, use.hw.mod, use.ingrowth), 
                                as.logical),
                  dplyr::across(c(sbw.cdef, sbw.yr, sbw.dur, thin.ba.perc.rmv, 
                                  thin.ba.pre, thin.qmd.ratio, thin.year, 
                                  ingrowth.min.dbh, ingrowth.thrshld), 
                                as.numeric))
  
  # rtn.vars as a list column 
  # ops$rtn.vars = list(rtn.vars)
  
  ops
}

#' Validate ops dataframe
#' 
#' Validates that the ops dataframe contains all required fields with appropriate
#' types and values. Performs conditional validation based on logical flags.
#' 
#' @param ops Dataframe: Operations dataframe created by make_ops()
#' @return Logical: TRUE if validation passes
#' @details
#' Validates:
#' - All required fields are present
#' - Logical fields are TRUE/FALSE (not NA)
#' - Numeric fields are numeric type
#' - Conditional requirements when use.ingrowth = TRUE (ingrowth.min.dbh, ingrowth.thrshld must be populated)
#' - rtn.vars is a non-empty list containing all default variables
#' @examples
#' ops = make_ops()
#' validate_ops(ops)
#'
validate_ops = function(ops) {
  
  required.fields = c('verbose', 'rtn.vars', 'use.cap.dbh', 'use.cap.ht', 
                      'use.sbw.mod', 'sbw.cdef', 'sbw.yr', 'sbw.dur', 
                      'use.thin.mod', 'thin.ba.perc.rmv', 'thin.ba.pre', 
                      'thin.qmd.ratio', 'thin.year', 'use.hw.mod', 
                      'use.ingrowth', 'ingrowth.min.dbh', 'ingrowth.thrshld', 
                      'use.vol')
  
  missing.fields = setdiff(required.fields, names(ops))
  
  if (length(missing.fields) > 0) {
    stop('ops dataframe missing required fields: ', 
         paste(missing.fields, collapse = ', '))
  }
  
  logical.fields = c('verbose', 'use.cap.dbh', 'use.cap.ht', 'use.sbw.mod', 
                     'use.thin.mod', 'use.hw.mod', 'use.ingrowth', 'use.vol')
  
  invalid.logical = logical.fields[!sapply(ops[logical.fields], function(x) is.logical(x) && !is.na(x))]
  
  if (length(invalid.logical) > 0) {
    stop('ops fields must be TRUE or FALSE (not NA): ', 
         paste(invalid.logical, collapse = ', '))
  }
  
  numeric.fields = c('sbw.cdef', 'sbw.yr', 'sbw.dur', 'thin.ba.perc.rmv', 
                     'thin.ba.pre', 'thin.qmd.ratio', 'thin.year', 
                     'ingrowth.min.dbh', 'ingrowth.thrshld')
  
  invalid.numeric = numeric.fields[!sapply(ops[numeric.fields], is.numeric)]
  
  if (length(invalid.numeric) > 0) {
    stop('ops fields must be numeric: ', 
         paste(invalid.numeric, collapse = ', '))
  }
  
  if (ops$use.ingrowth) {
    ingrowth.required = c('ingrowth.min.dbh', 'ingrowth.thrshld')
    ingrowth.missing = ingrowth.required[sapply(ops[ingrowth.required], is.na)]
    if (length(ingrowth.missing) > 0) {
      stop('use.ingrowth = TRUE requires populated fields: ', 
           paste(ingrowth.missing, collapse = ', '))
    }
  }
  
  if (!is.list(ops$rtn.vars) || length(ops$rtn.vars) == 0) {
    stop('ops field rtn.vars must be a non-empty list')
  }
  
  default.rtn.vars = c('year', 'plot', 'tree', 'sp', 'dbh', 'ht', 
                       'hcb', 'cr', 'expf', 'pht', 'phcb', 'form', 'risk', 
                       'ddbh.mult', 'dht.mult', 'mort.mult', 'max.dbh', 'max.height')
  
  missing.rtn.vars = setdiff(default.rtn.vars, ops$rtn.vars[[1]])
  
  if (length(missing.rtn.vars) > 0) {
    stop('ops$rtn.vars missing default variables: ', 
         paste(missing.rtn.vars, collapse = ', '))
  }
  
  TRUE
}

#### Prepare stand dataframe ####
#' Create ACD stand list
#' 
#' @param stand.id character: Stand identifier
#' @param csi Numeric: Climate site index (m; default 12)
#' @param elev Numeric: Elevation (m; default 350) 
#' @param region Character: Region code; options are ME, NB, NS, QC (default 'ME')
#' @return Dataframe: Stand dataframe
#'
  make_stand= function(stand.id,
                       csi =12,
                       elev =350, 
                       region = 'ME'){
    
    stand=data.frame(stand.id=stand.id,
                     csi = as.numeric(csi),
                     elev = as.numeric(elev), 
                     region = region)
    
    stand
    
  }

#' Validate stand dataframe
#' 
#' Validates that the stand dataframe contains all required fields with appropriate
#' types and values.
#' 
#' @param stand Dataframe: Stand dataframe created by make_stand()
#' @return Logical: TRUE if validation passes
#' @details
#' Validates:
#' - All required fields are present (stand.id, csi, elev, region)
#' - stand.id is character type and not NA
#' - csi is numeric and not NA
#' - elev is numeric and not NA
#' - region is character, not NA, and one of: ME, NB, NS, QC
#' @examples
#' stand = make_stand('stand1', csi = 12, elev = 350, region = 'ME')
#' validate_stand(stand)
#'
validate_stand = function(stand) {
  
  required.fields = c('stand.id', 'csi', 'elev', 'region')
  
  missing.fields = setdiff(required.fields, names(stand))
  
  if (length(missing.fields) > 0) {
    stop('stand dataframe missing required fields: ', 
         paste(missing.fields, collapse = ', '))
  }
  
  if (!is.character(stand$stand.id) || is.na(stand$stand.id)) {
    stop('stand$stand.id must be character and not NA')
  }
  
  if (!is.numeric(stand$csi) || is.na(stand$csi)) {
    stop('stand$csi must be numeric and not NA')
  }
  
  if (!is.numeric(stand$elev) || is.na(stand$elev)) {
    stop('stand$elev must be numeric and not NA')
  }
  
  valid.regions = c('ME', 'NB', 'NS', 'QC')
  
  if (!is.character(stand$region) || is.na(stand$region)) {
    stop('stand$region must be character and not NA')
  }
  
  if (!stand$region %in% valid.regions) {
    stop('stand$region must be one of: ', 
         paste(valid.regions, collapse = ', '), 
         '. Got: ', stand$region)
  }
  
  TRUE
}

#### Prepare output tree list ####
#### for FVS fvsSetTreeAttrs()
#' Create FVS return tree list
#' 
#' @param tree.data Dataframe: tree list output from ACD, fields:  year; dbh; ht; hcb; expf; cr
#' @param orgtree.list Dataframe: input tree list from FVS. orgtree.list fields tree; dbh; ht; expf; dg; htg; mort; cratio
#' @param num.plots Numeric: number of plots in a stand
#' @return Dataframe: FVS tree dataframe
#'
# Note- assumes tree.data$expf is plot trees per hectare

make_fvs_tree=function(tree.data, orgtree.list, num.plots){
  
  #customary-metric conversion 
  cm.to.in=0.393701
  m.to.ft = 3.28084
  ac.to.ha = 0.404686
  
  # remove projected values for species not in ACD
  tree.list=tree.data %>% 
    dplyr::anti_join(orgtree.list %>% 
                       dplyr::filter(acd.ex==TRUE), 
                     by='tree')
  
  # dataframe with tree records not handled by ACD- snags and invalid DBH; species not in ACD
  tree.org=orgtree.list %>% 
    dplyr::transmute(tree, 
                  dbh, # metric
                  ht, # metric
                  expf, # plot level tph 
                  dg, # customary units
                  htg, # customary units
                  mort, # stand level TPA
                  cratio = round(cr * 100, 1)) %>%  # convert proportion to percentage
    dplyr::anti_join(tree.list,
                     by='tree')
  
  
  tree=orgtree.list %>% 
    dplyr::select(tree, 
                  dbh.fvs=dbh,
                  ht.fvs=ht, 
                  expf.fvs=expf) %>% 
    dplyr::inner_join(tree.list, # inner join excludes regeneration and records not handled by ACD
                      by='tree') %>% 
    dplyr::mutate(dg=(dbh-dbh.fvs)*cm.to.in, # diameter growth to inches
                  htg=(ht-ht.fvs)*m.to.ft,  # height growth to feet
                  # set the crown ratio sign to negative so that FVS doesn't change them. 
                  cratio = round((1-(hcb/ht))*-100, 1), # 
                  # special=as.numeric(substr(Form,2,2))*10+ # will need this when we allow Acadian model to set Form and Risk 
                  #   as.numeric(substr(Risk,2,2)),
                  mort=(expf.fvs-expf)*ac.to.ha,  # mortality TPH stand level to trees per acre
                  mort=mort/dplyr::coalesce(num.plots, 1)) %>%  # calculate stand level mortality TPA
    dplyr::bind_rows(tree.org) %>% # append tree records not handled by ACD
    dplyr::arrange(tree) %>% 
    dplyr::select(#dbh,
                  #sp,
                  dg,
                  htg,
                  cratio ,
                  #special,
                  mort)
  
  
  # previous version option to use FVS-NE mortality
  # if (mort.model != "Acadian") {
  #     tree=tree %>% 
  #         dplyr::select(-mort)
  # }
  
  #tibble to dataframe  
  tree=as.data.frame(tree)
  
  tree
}

#### tree list for FVS fvsAddTrees()
#' Make dataframe to pass to fvsAddTrees()
#' 
#' @param tree.data Dataframe: Tree list output from ACD. tree.list fields  year; dbh; ht; hcb; expf; cr
#' @param num.plots Numeric: number of plots in a stand
#' @param orgtree.list Dataframe: input tree list from FVS. orgtree.list fields tree; dbh; ht; expf; dg; htg; mort; cratio
#' @param spcodes Dataframe: FVS species codes from fvsGetSpeciesCodes(). Required fields- fvs (FVS alpha species code). FVS numeric code is the vector "row" number. Note fvsGetSpeciesCodes() returns a character vector
#' @return Dataframe: Tree dataframe ready to apply in rFvs::fvsAddTrees()
# Note assumes expf is plot trees per hectare
#'
make_fvs_regen=function(tree.data, orgtree.list, num.plots, spcodes){
  
  #customary-metric conversion 
  cm.to.in = 0.393701
  m.to.ft  = 3.28084
  ac.to.ha = 0.404686
  
  regen=tree.data %>% 
    dplyr::anti_join(orgtree.list,
                     by='tree') %>% 
    dplyr::arrange(tree) %>% 
    dplyr::mutate(expf= expf/dplyr::coalesce(num.plots, 1)) %>%  # calculate stand level TPH
    dplyr::transmute(dbh=dplyr::coalesce(dbh*cm.to.in, 0),
                     species=match(sp, spcodes[,"fvs"]),
                     ht= dplyr::case_when(dbh<0.5 & ht>1 ~3, # if ingrowth dbh is less than 0.5 in and height>1 m - set to 3 ft
                                          TRUE ~dplyr::coalesce(ht*m.to.ft, 0)),
                     cratio= dplyr::coalesce(round((1-(hcb/ht))*-100, 1), 0),
                     plot= as.numeric(plot),
                     tpa= expf*ac.to.ha)
  
  #tibble to dataframe  
  regen=as.data.frame(regen)
  
  regen
}

#### Calculated tree values ####

#' Calculate basal area in larger trees (BAL) for each tree
#' 
#' @param tree.data Dataframe: Tree list
#' @return Dataframe: Tree data with added BAL columns (bal, bal.sw, bal.hw)
#'
calc_bal = function(tree.data) {
  
  # Check required columns
  required.cols = c('plot', 'dbh', 'ba', 'sp.type')
  missing.cols = setdiff(required.cols, names(tree.data))
  if (length(missing.cols) > 0) {
    stop(paste("Missing required columns:", paste(missing.cols, collapse = ", ")))
  }
  
  # Sort by plot and descending DBH; calculate cumulative BA
  tree=tree.data %>%
    dplyr::arrange(plot, 
            desc(dbh)) %>%
    dplyr::group_by(plot) %>%
    dplyr::mutate(bal = cumsum(ba) - ba,
           cumsum.sw = cumsum(ba * (sp.type == 'SW')),
           bal.sw = cumsum.sw - (ba * (sp.type == 'SW')),
           bal.hw = bal - bal.sw) %>%
    dplyr::select(-cumsum.sw) %>% 
    dplyr::ungroup()
  
  tree
}


#### Calculated plot values ####


#' Calculate plot summary statistics from tree list
#' 
#' @param tree.data Dataframe: Tree list
#' @return Dataframe: Plot-level summary statistics
#'
calc_plot_summary = function(tree.data) {
  
  # Check required columns
  required.cols = c('plot', 'tree', 'sp', 'dbh', 'expf', 'ba', 'sg', 'sp.type', 'shade')
  missing.cols = setdiff(required.cols, names(tree.data))
  if (length(missing.cols) > 0) {
    stop(paste("Missing required columns:", paste(missing.cols, collapse = ", ")))
  }
  
  # Plot level summary
  plot.summary = tree.data %>%
    dplyr::group_by(plot) %>%
    dplyr::summarise(tph.plot = sum(expf, na.rm = TRUE),
                     ba.plot = sum(ba, na.rm = TRUE),
                     # Softwood basal area
                     ba.sw = sum(ifelse(sp.type == 'SW', ba, 0), na.rm = TRUE),
                     # intolerant hardwood basal area
                     ba.ihw = sum(ifelse(sp.type == 'HW' & shade <2.0, ba, 0), na.rm = TRUE),
                     # Specific gravity weighted by basal area, capped at 0.8
                     meansg = pmin(sum(sg * ba, na.rm = TRUE) / 
                                     sum(ba, na.rm = TRUE), 0.8, na.rm = TRUE),
                     # SDI
                     sdi = sum(ifelse(dbh >= 10, (dbh / 25.4)^1.6 * expf, 0), na.rm = TRUE),
                     sdi.all = sum((dbh / 25.4)^1.6 * expf, na.rm = TRUE),
                     # Softwood DBH average (where sp.type == 'SW')
                     dbh.sw.avg = mean(ifelse(sp.type == 'SW', dbh, NA), na.rm = TRUE),
                     # max tree number for ingrowth
                     max.tree.id = max(tree, na.rm = TRUE), .groups = 'drop') %>%
    # QMD
    dplyr::mutate(qmd = sqrt(ba.plot / (0.00007854 * tph.plot)), 
                  # percent plot BA in hardwood
                  ba.perc.hw = ifelse(ba.plot == 0, 0, 1 - (ba.sw / ba.plot)))
  
  plot.summary
}


#' Calculate maximum Stand Density Index- Weiskittel and Kuehne (2019) 
#' 
#' @param plot.data Dataframe: Plot summary data
#' @param tree.data Dataframe: Tree list for DBH range calculations
#' @param csi Numeric: Climate site index (m)
#' @param elev Numeric: Elevation (m)
#' @return Dataframe: Input data with added SDI max columns (sdi.max, sdi.max.all)
#'
calc_sdi_max = function(plot.data, tree.data, csi, elev) {
  
  # Check required columns in plot.data
  required.cols = c('plot', 'ba.perc.hw', 'meansg')
  missing.cols = setdiff(required.cols, names(plot.data))
  if (length(missing.cols) > 0) {
    stop(paste("Missing required columns in plot.data:", paste(missing.cols, collapse = ", ")))
  }
  
  # DBH ranges and species diversity 
  plot.dbhrange = tree.data %>%
    dplyr::group_by(plot) %>%
    dplyr::summarise(dbh.range = ifelse(any(dbh >= 10, na.rm = TRUE),
                                        max(ifelse(dbh >= 10, dbh, NA), na.rm = TRUE) - 
                                          min(ifelse(dbh >= 10, dbh, NA), na.rm = TRUE), 0),
                     dbh.range.all = max(dbh, na.rm = TRUE) - min(dbh, na.rm = TRUE),
                     spp.div = n_distinct(sp),
                     .groups = 'drop')
  
  # Join with plot data
  plot.smry = plot.data %>% 
    dplyr::left_join(plot.dbhrange, 
                     by = 'plot')
  
  ## SDI max
  # Weiskittel & Kuehne (2019) 
  sdi_max_weiskittel = function(ba.perc.hw, meansg, dbh.range, spp.div, elev, csi) {
    sdimax = 475.2079 - 1.5908 * ba.perc.hw - 236.9051 * log(meansg) + 
      50.3299 * sqrt(dbh.range) + 13.5202 * spp.div + 
      0.0685 * elev - 2.8537 * sqrt(elev) + 222.7836 / csi
    
    sdimax
  }
  
  #
  sdi_max = function(meansg) { 
    
    sdimax=1347.445 - 1003.870 * meansg 
    
    sdimax
  }
  
  plot.smry=plot.smry %>%
    dplyr::mutate(sdi.max = dplyr::coalesce(sdi_max_weiskittel(ba.perc.hw, 
                                                               meansg, 
                                                               dbh.range,
                                                               spp.div, 
                                                               elev, csi), 
                                            sdi_max(meansg)),
                  sdi.max.all = dplyr::coalesce(sdi_max_weiskittel(ba.perc.hw, 
                                                                   meansg, 
                                                                   dbh.range.all,
                                                                   spp.div, 
                                                                   elev, 
                                                                   csi), 
                                                sdi_max(meansg))) %>%
    dplyr::select(-dbh.range, -dbh.range.all, -spp.div)
  
  plot.smry
}

#' Calculate modified relative density (rd.mod)
#' 
#' @param sdi Numeric: Stand Density Index values for tree records >=10cm
#' @param sdi.all Numeric: Stand Density Index for all tree records
#' @param sdi.max Numeric: Maximum Stand Density Index for tree records >=10cm
#' @param sdi.max.all Numeric: Maximum Stand Density Index for all tree records  
#' @param qmd Numeric: Quadratic mean diameter (cm)
#' @return Numeric: Modified relative density values
#'
calc_relative_density = function(sdi, sdi.all, sdi.max, sdi.max.all, qmd) {
  
  # Calculate rd.mod
  rd.mod = dplyr::case_when(qmd < 10 & !is.na(sdi.all) & !is.na(sdi.max.all) ~ sdi.all / sdi.max.all,
                            qmd >= 10 & !is.na(sdi) & !is.na(sdi.max) ~ sdi / sdi.max,
                            TRUE ~ 0)
  
  rd.mod
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

### Call Acadian growth and yield model for each stand
Acadian.GY = function(tree, stand, ops = NULL) {
  
  ans = tree %>%
    dplyr::filter(!is.na(stand.id)) %>%
    split(.$stand.id) %>%
    purrr::imap_dfr(function(tree.subset, stand.id.subset) {
      stand.subset = subset(stand, stand.id == stand.id.subset)
      
      AcadianGYOneStand(tree = tree.subset, 
                        stand = stand.subset, 
                        ops = ops)
    })
  
  tree = ans %>%
    dplyr::arrange(year, stand.id, plot, tree)
  
  tree
}

  
###Acadian growth and yield model called for one stand at time
#' Run Acadian growth and yield model for one year for one stand
#' 
#' @param tree Dataframe: Tree list
#' @param stand Dataframe: Stand attributes
#' @param ops Dataframe: 
#' @return Dataframe: Plot top height values with columns for plot and topht
#'
AcadianGYOneStand = function(tree, stand, ops)
{
  ### -----
  ## before proceeding run 
  ## * make.ops()
  ## * make_stand() 
  ## * make_acd_tree()
  ### ----

##### Validate input data ####  
  # validate_ops(ops)  
  # validate_stand(stand)
  # validate_tree(tree)
  
##### Add tree attributes ####
  tree = tree %>% 
      # species functional attributes
    dplyr::left_join(sp.attr, 
                     by=c('sp'='species')) %>% 
        # basal area (plot level)
    dplyr::mutate(ba=(dbh^2*0.00007854)*expf,
         # clean form and risk       
         form = dplyr::na_if(form, " "),
         risk = dplyr::na_if(risk, " ")) %>% 
      # Calculate BAL
    calc_bal()

  
##### Plot attributes ####
    # Calculate plot summary
  plot.smry = tree %>% 
    calc_plot_summary() %>% 
    # Calculate SDI max
    calc_sdi_max(plot.data = ., 
                 tree.data = tree, 
                 csi = stand$csi, 
                 elev = stand$elev) %>% 
    # Relative density
    dplyr::mutate(rd.mod = calc_relative_density(sdi, 
                                          sdi.all, 
                                          sdi.max, 
                                          sdi.max.all, 
                                          qmd))
  
##### Crown area ####  
 #calculate tree-level crown metrics (mcw, lcw, mca, ccfl)
  tree = tree %>% 
    calc_crown()
  
  #calculate plot CCF
  plot.ccf = tree %>% 
    dplyr::group_by(plot) %>%
    dplyr::summarise(ccf = sum(mca, na.rm = TRUE),
              .groups = 'drop')
  
  #save the plot CCFs in plot.smry for use with height, height to crown base, crown recession and ingrowth
  plot.smry = plot.smry %>% 
    dplyr::left_join(plot.ccf,
              by="plot")
 
  #form and risk
  # currently not predicting form and risk
  
 
##### Height and crown ratio ####  
  #calculate heights of any with missing values.
  #generally, none will be missing when function is used with FVS, but some or
  #all may be missing when code us used to grow tree lists from other sources. 
  tree=tree %>% 
          #predicted height
    calc_ht(plot.data = plot.smry) %>% 
    dplyr::mutate(#use predicted height if missing or > 50m and DBH> 2.5cm
           ht= dplyr::case_when((ht %in% c(NA, 0)| ht>50) & dbh>2.5 ~pht,
                        TRUE ~ ht)) %>% 
           #predicted height to crown base (returns phcb)
    calc_hcb(plot.data = plot.smry) %>% 
           #use predicted height to crown base if crown ratio is missing or >99 and if hcb is missing or invalid
    dplyr::mutate(hcb= dplyr::case_when(cr %in% c(NA, 0) | cr>99 | is.na(hcb) | hcb>ht  ~phcb, 
                                TRUE ~hcb),
                    cr= dplyr::case_when(cr %in% c(NA, 0) | cr>99 ~1-(phcb/ht),
                        TRUE ~cr))
  
# Compute plot-level heights
  
  # Plot top height (for 100 tph)
  plot.topht = tree %>% 
    calc_topht()
   
  # Average softwood height
  plot.avg.sw.ht=tree %>% 
    dplyr::filter(sp.type=='SW') %>% 
    dplyr::group_by(plot) %>% 
    dplyr::summarise(ht.sw.avg = weighted.mean(ht, expf, na.rm = TRUE),
                     .groups = 'drop') %>% 
    ungroup() %>% 
    dplyr::mutate(ht.sw.avg =  dplyr::coalesce(ht.sw.avg, 0))
  
  # Add to plot summary
  plot.smry=plot.smry %>% 
    dplyr::left_join(plot.topht,
                     by='plot') %>% 
    dplyr::left_join(plot.avg.sw.ht, 
                     by='plot')
 
##### Diameter increment ####
  tree = tree %>%
    calc_ddbh(plot.data = plot.smry)
 
##### Height increment ####  
  tree= tree %>%
    calc_dht(plot.data = plot.smry)

#### Crown recession ####
  tree= tree %>%
    calc_dhcb(plot.data = plot.smry) 
  
  # if (verbose) cat ("mean tree$dHCB.thin.mod=",mean(tree$dHCB.thin.mod),"\n")  
  # cat ("na dbh values=", sum(is.na(tree$dbh)), "\n")
  # cat ("mean dbh=",mean(tree$dbh),"\n")
  # cat ("mean expf=",mean(tree$expf),"\n")
  # cat ("na expf values=", sum(is.na(tree$expf)), "\n")

#### Mortality ####
  ## Mortality from version 12.1.6
  tree=tree %>% 
    calc_mortality(plot.data=plot.smry)
  
#### Ingrowth ####
  if(ops$use.ingrowth == TRUE){
    ingrowth= tree %>% 
      calc_ingrowth(plot.data=plot.smry)
  }

#### Output ####  
  #Update tree values
  tree=tree %>% 
    dplyr::mutate(year= year+1,
                  dbh= dbh+ dplyr::coalesce(ddbh, 0),
                  ht= ht+ dplyr::coalesce(dht, 0),
                  hcb= hcb+ dplyr::coalesce(dhcb, 0),
                  expf= dplyr::coalesce(expf, 0) - dplyr::coalesce(dexpf, 0),
                  expf= ifelse(expf< 0.00001, 0.00001, expf),
                  cr= 1-(hcb/ht)) 
  
  # Append ingrowth
  if(exists('ingrowth')){
    tree=tree %>% 
    dplyr::bind_rows(ingrowth)    
  }

  # select return variables
  rtn.vars=intersect(ops$rtn.vars[[1]],
                     colnames(tree))
  tree=subset(tree,
              select=rtn.vars) %>% 
    as.data.frame()                          

}         

####
#### Taper ####
####

## Kozak taper function parameters
taper.kozak.spp = dplyr::tribble(
  ~bark, ~species, ~a0, ~a1, ~a2, ~b1, ~b2, ~b3, ~b4, ~b5, ~b6, ~b7,
  'ob', 'AB', 1.0693567631, 0.9975021951, -0.01282775, 0.3921013594, -1.054622304, 0.7758393514, 4.1034897617, 0.1185960455, -1.080697381, 0,
  'ob', 'BC', 0.9802172591, 0.9900811022, 0.0215023934, 0.6092829761, -0.54627086, 0.5221909952, 1.6561496035, 0.040879378, -0.302807393, 0,
  'ib', 'BF', 0.88075316, 1.01488665, 0.01958804, 0.41951756, -0.67232564, 0.54329725, 1.48181152, 0.06470371, -0.34684837, 0,
  'ob', 'BF', 0.87045800178728, 0.998148536293802, 0.0584816955042306, 0.302539012401385, -0.605787065734974, 0.588861845770261, 0.8826608914125, 0.103280103524893, -0.57432603217401, 0,
  'ob', 'BP', 1.0036248405, 0.744246238, 0.2876417207, 0.6634046516, -2.004812235, 0.7507983401, 3.9248261105, 0.0276793767, -0.130928845, 0,
  'ob', 'BA', 1.0036248405, 0.744246238, 0.2876417207, 0.6634046516, -2.004812235, 0.7507983401, 3.9248261105, 0.0276793767, -0.130928845, 0,
  'ib', 'BS', 0.80472902, 1.00804553, 0.05601099, 0.35533529, -0.41320046, 0.41527304, 1.11652424, 0.0990167, -0.40992056, 0.11394943,
  'ob', 'BS', 0.896382313496267, 0.979157280469517, 0.07070415827334, 0.288205614793081, -0.303580327062765, 0.435229599780184, 0.287092390832665, 0.0861036484421037, -0.407747649433411, 0.371113950891855,
  'ob', 'BT', 1.0200889056, 1.0054957243, -0.011030907, 0.5104511725, -1.326415929, 0.5568665797, 7.2108347873, 0.071149738, -0.571844802, 0,
  'ib', 'EH', 0.960235102, 1.00821143, -0.025167937, 0.825260258, 1.962520834, 0.415234319, -5.061571874, 0.009839526, -0.095533007, 0,
  'ob', 'EH', 0.846409603849866, 0.984317716125905, 0.0807523481457474, 0.445438700558324, -0.671467572085628, 0.504954501484816, 2.48940465528, 0.124152912027385, -0.722954836646604, 0,
  'ob', 'GA', 1.0852385488, 1.1861877395, -0.226193745, 0.5198788065, 1.4303205202, -0.349453901, 3.1952591271, 0.1391694941, -0.296716822, 0,
  'ob', 'GB', 1.0263926931, 0.8835623138, 0.1307522645, 0.6113533288, -0.114188076, 0.2883217076, 2.657433495, 0.0590046356, -0.175127606, 0,
  'ib', 'JP', 0.931552701, 1.008192708, -0.004177373, 0.431297353, -0.863672736, 0.511698303, 2.232484834, 0.059865263, -0.331897255, 0.039630786,
  'ob', 'JP', 0.842483072142665, 0.99279768524928, 0.0739425827838225, 0.37221919371203, -0.723225866494174, 0.453434142074953, 1.33754275322832, 0.073372838152118, -0.3105255908992, 0.396398949039286,
  'ib', 'NS', 0.9308817, 0.97360573, 0.03522864, 0.65078104, -0.30355787, 0.37832812, 1.18815216, 0.03111631, -0.03172809, 0,
  'ob', 'NS', 0.950952303433305, 0.99162401049595, 0.0357175689757522, 0.507484658718266, -0.44046929698967, 0.405856745795155, 1.2849978191539, 0.0143964536822362, -0.0785889411281423, 0.169725200257675,
  'ib', 'PB', 0.7161229027, 0.9811224473, 0.1382539493, 0.4782152412, 0.3091537448, 0.3266307618, -0.302056097, 0.0858585241, -0.278661048, 0,
  'ob', 'PB', 0.7161229027, 0.9811224473, 0.1382539493, 0.4782152412, 0.3091537448, 0.3266307618, -0.302056097, 0.0858585241, -0.278661048, 0,
  'ob', 'QA', 0.5586975794, 0.9047841359, 0.3075094544, 0.7131251715, -0.588345303, 0.4292045831, 2.8516108932, 0.0381609362, -0.13426388, 0,
  'ib', 'RM', 0.745826994, 1.0092251371, 0.0890931039, 0.5861620841, -0.865905462, 0.6539243149, 3.0603989176, 0.0827619274, -0.64859681, 0,
  'ob', 'RM', 0.745826994, 1.0092251371, 0.0890931039, 0.5861620841, -0.865905462, 0.6539243149, 3.0603989176, 0.0827619274, -0.64859681, 0,
  'ob', 'RO', 1.1751352376, 1.02249704, -0.069888591, 0.4505675893, -0.902884964, 0.5812519636, 3.6267479819, 0.1656137742, -1.114281314, 0,
  'ib', 'RP', 0.9717883, 1.00113806, -0.01597933, 0.51143292, -0.9739954, 0.25844201, 4.75315518, 0.05846224, -0.12372176, 0,
  'ob', 'RP', 1.06470820904747, 0.994899036827748, -0.0123828485987216, 0.458957297467137, -1.04575412640177, 0.361452014890273, 4.00047777431758, 0.0543368451581955, -0.128025447306836, 0,
  'ib', 'RS', 0.89797987, 1.00579742, 0.01667313, 0.49500865, -0.63375155, 0.3836274, 1.41380994, 0.08866994, -0.29753964, 0.15192029,
  'ob', 'RS', 0.886886241411388, 0.995431239145283, 0.0541365481351767, 0.411160410244944, -0.658022227353248, 0.418213595349517, 1.09113756405639, 0.102379812299201, -0.40367256147942, 0.104842994095004,
  'ob', 'SB', 0.8471057131, 0.9875376729, 0.0769690406, 0.9322599144, -0.954580316, 0.48553875, 3.0294545606, 0.0767610836, -0.238398236, 0,
  'ob', 'SM', 1.0517056747, 0.96129896, 0.0386037512, 0.8556437779, -0.249723079, 0.4149367053, 1.2548340569, 0.0412998707, -0.113500099, 0,
  'ob', 'TL', 0.762977580507808, 0.979320525735404, 0.122788251183516, 0.245935863173793, -0.564901857800367, 0.666790795105499, -0.0728778930339496, 0.143651487515151, -0.791188036888163, 0,
  'ob', 'TA', 0.762977580507808, 0.979320525735404, 0.122788251183516, 0.245935863173793, -0.564901857800367, 0.666790795105499, -0.0728778930339496, 0.143651487515151, -0.791188036888163, 0,
  'ob', 'WA', 0.8550736297, 0.9768941226, 0.0770356694, 0.7819090026, -0.791762733, 0.476698925, 3.5003928402, 0.0859040469, -0.487974342, 0,
  'ib', 'WC', 0.86118766, 0.98152118, 0.0568203, 0.40717678, -0.05482572, 0.47809459, -1.32512447, 0.1538487, -0.53687808, 0,
  'ib', 'NC', 0.86118766, 0.98152118, 0.0568203, 0.40717678, -0.05482572, 0.47809459, -1.32512447, 0.1538487, -0.53687808, 0,
  'ob', 'WC', 0.876976728762079, 0.972187200775237, 0.0905032843727524, 0.319643790061659, -0.495778605215774, 0.546605647382787, -0.0540118375921429, 0.131666046721139, -0.454765563250266, 0,
  'ob', 'NC', 0.876976728762079, 0.972187200775237, 0.0905032843727524, 0.319643790061659, -0.495778605215774, 0.546605647382787, -0.0540118375921429, 0.131666046721139, -0.454765563250266, 0,
  'ib', 'WP', 1.04881379, 1.00779696, -0.04595353, 0.38085445, -0.85956463, 0.34380669, 4.60836993, 0.111855, -0.5523203, 0,
  'ob', 'WP', 0.961977278802905, 0.985977453808376, 0.0333180987707418, 0.383416881614619, -0.753661988626837, 0.392529765236197, 3.4224381734935, 0.100601541094101, -0.485617012177084, 0,
  'ib', 'WS', 0.75826241, 0.98481863, 0.09956165, 0.36505143, -0.51501314, 0.55913869, 0.75846281, 0.07011851, -0.44928376, 0.07830011,
  'ob', 'WS', 0.725059647049259, 0.999930744977476, 0.11890841412387, 0.286031149725587, -0.417052954651359, 0.581226449067082, -0.562751307358532, 0.101380520664108, -0.563774194060357, 0.096121529684134,
  'ob', 'YB', 1.1263776728, 0.9485083275, 0.0371321602, 0.7662525552, -0.028147685, 0.2334044323, 4.8569609081, 0.0753180483, -0.205052535, 0,
  'ob', 'OH', 0.947211744, 0.971353083, 0.063182322, 0.633614831, -0.549156049, 0.439010965, 3.187595496, 0.079154063, -0.41277508, 0,
  'ob', 'OS', 0.88047918, 0.988526494, 0.0660791, 0.365548416, -0.607245626, 0.486832282, 1.282373726, 0.094120201, -0.447380533, 0,
  'ib', 'OS', 0.896475601, 1.001886257, 0.020707494, 0.391516469, -0.395638544, -0.011787171, 1.335110611, 0.076311559, -0.286988273, 0
)

## Westfall Scott taper function parameters 
taper.westfall.scott.spp = dplyr::tribble(
  ~species, ~th1, ~th2, ~a1, ~a2, ~gm1, ~gm2, ~phi, ~lmd, ~bet1, ~bet2,
  'SM', 6.5790, 0.0111, 1.0682, 1.1833, 0.1031, 0.2624, 0.1516, 1.1482, 0.6637, 3.0996,
  'YB', 7.5437, 0.0103, 0.9961, 1.1042, 0.1313, 0.3539, 0.2091, 0.9478, 0.5995, 3.4205,
  'PB', 7.5437, 0.0103, 0.9961, 1.1042, 0.1313, 0.3539, 0.2091, 0.9478, 0.5995, 3.4205,
  'GB', 7.5437, 0.0103, 0.9961, 1.1042, 0.1313, 0.3539, 0.2091, 0.9478, 0.5995, 3.4205,
  'SB', 7.5437, 0.0103, 0.9961, 1.1042, 0.1313, 0.3539, 0.2091, 0.9478, 0.5995, 3.4205,
  'RM', 7.5707, 0.0105, 1.5273, 0.7684, 0.0931, 0.4223, 0.1441, 1.3910, 0.6453, 4.0737,
  'GA', 3.3085, 0.0276, 1.2634, 0.9088, 0.1098, 0.5198, 0.1840, 1.7842, 0.6719, 5.1178,
  'WA', 3.3085, 0.0276, 1.2634, 0.9088, 0.1098, 0.5198, 0.1840, 1.7842, 0.6719, 5.1178,
  'BT', 3.3085, 0.0276, 1.2634, 0.9088, 0.1098, 0.5198, 0.1840, 1.7842, 0.6719, 5.1178,
  'QA', 3.3085, 0.0276, 1.2634, 0.9088, 0.1098, 0.5198, 0.1840, 1.7842, 0.6719, 5.1178,
  'BP', 3.3085, 0.0276, 1.2634, 0.9088, 0.1098, 0.5198, 0.1840, 1.7842, 0.6719, 5.1178,
  'AB', 8.9843, 0.0107, 0.7621, 1.3734, 0.0956, 0.1650, 0.1924, 1.2237, 0.4626, 1.0954,
  'RO', 12.8336, 0.0125, 0.9038, 1.0950, 0.0935, 0.3971, 0.2038, 1.0457, 0.5508, 3.4681,
  'BC', 3.2042, 0.0479, 1.2507, 0.8075, 0.0800, 0.4170, 0.2227, 2.7226, 0.7065, 4.6476,
  'BF', 5.3693, 0.0171, 1.4212, 0.3003, 0.0890, 0.6485, 0.1916, 1.8873, 0.4764, 2.6383,
  'RS', 6.8745, 0.0110, 1.1241, 0.4107, 0.1376, 0.4842, 0.2038, 1.2598, 0.4986, 2.7865,
  'BS', 6.8745, 0.0110, 1.1241, 0.4107, 0.1376, 0.4842, 0.2038, 1.2598, 0.4986, 2.7865,
  'WS', 6.8745, 0.0110, 1.1241, 0.4107, 0.1376, 0.4842, 0.2038, 1.2598, 0.4986, 2.7865,
  'WP', 7.1438, 0.0123, 0.8978, 0.7872, 0.0989, 0.4985, 0.2049, 0.9247, 0.5715, 2.0482,
  'TA', 5.2913, 0.0411, 1.1291, 0.6831, 0.0745, 0.5798, 0.1896, 1.5776, 0.6616, 6.0645,
  'NS', 5.2913, 0.0411, 1.1291, 0.6831, 0.0745, 0.5798, 0.1896, 1.5776, 0.6616, 6.0645,
  'JP', 5.2913, 0.0411, 1.1291, 0.6831, 0.0745, 0.5798, 0.1896, 1.5776, 0.6616, 6.0645,
  'WC', 5.4000, 0.0256, 1.9295, 0.8142, 0.0943, 0.9642, 0.2761, 1.8605, 1.3432, 1.3438,
  'RP', 7.6044, 0.0148, 1.2379, 0.3304, 0.0759, 0.6611, 0.3008, 1.1569, 0.5462, 3.0627,
  'EH', 7.2442, 0.0152, 1.4008, 0.8306, 0.0856, 0.4724, 0.2011, 1.5776, 0.6616, 6.0645,
  'OH', 9.0505, 0.0241, 1.2980, 0.7684, 0.0684, 0.4555, 0.1769, 1.6684, 0.5408, 4.1821,
  'OS', 6.418214286, 0.019585714, 1.305771429, 0.593785714, 0.093685714, 0.615528571, 0.223985714, 1.462628571, 0.685271429, 2.842342857,  #composite of all conifers
  '99', 7.114957895, 0.016836842, 1.148384211, 0.857194737, 0.101226316, 0.460905263, 0.195610526, 1.351415789, 0.634036842, 3.302584211 #composite of all species
)


#' Calculate tree diameter at a given height using Li and Weiskittel - Kozak taper model
#' 
#' @param sp Character: Species code (FVS alpha)
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param ht Numeric: Total tree height (m)
#' @param ht.dia Numeric: Height at which to predict diameter (m)
#' @param dia.type Character: Diameter type 'ob' (outside bark) or 'ib' (inside bark), kozak only (default 'ib')
#' @param plt Integer: Plantation indicator (1=plantation, 0=natural), kozak only (default 0)
#' @param taper.kozak.spp Dataframe: Kozak taper function parameters (default 0taper.kozak.spp)
#' @return Numeric: Predicted diameter at the specified height
#'
# Li, R; Weiskittel, AR; Dick, A; Kershaw JA; Seymour, RS. 2012. Regional stem 
# taper equations for eleven conifer species in the Acadian region of North America:
# development and assessment. Northern Journal of Applied Forestry 29(1): 5-14
# 
# Weiskittel, AR; Li, R. 2012. Development of regional taper and volume equations: 
# Hardwood species. In: Roth, BE ed. Cooperative Forestry Research Unit 2011 
# Annual Report. University of Maine, School of Forest Resources, Orono, ME; pp 76-84.

taper_kozak = function(sp, dbh, ht, ht.dia, dia.type = 'ib', plt = 0, 
                       taper.kozak.spp=taper.kozak.spp) {
  
  # parameters
  parms = taper.kozak.spp %>% 
    dplyr::filter(bark == dia.type, 
                  species == sp)
  
  # If no match found, use default parameters
  if (is.na(parms$a0[1])) {
    parms = taper.kozak.spp %>% 
      dplyr::filter(bark == 'ib', 
                    species == 'OS')
  }
  
  # Calculate diameter
  #adjusted if at breast height
  p = 1.3/ht
  z = ht.dia/ht
  xi = (1 - z^(1/3))/(1 - p^(1/3))
  qi = 1 - z^(1/3)
  
  if(dia.type == 'ob' & ht.dia == 1.37){
    dia=dbh
  }else{
    dia=round( (parms$a0 * (dbh^parms$a1) * (ht^parms$a2)) * 
                 xi^(parms$b1 * z^4 + parms$b2 * (exp(-dbh/ht)) +
                       parms$b3 * xi^0.1 + parms$b4 * (1/dbh) + 
                       parms$b5 * ht^qi + parms$b6 * xi + parms$b7*plt),
               4)
  }
  
  dia
}

#' Calculate  diameter at a given height using Westfall & Scott taper model
#' 
#' @param sp Character: Species code (FVS alpha)
#' @param ht.dia Numeric: Height at which to predict diameter (m)
#' @param ht Numeric: Total tree height (m)
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param taper.westfall.scott.spp Dataframe: Westfall Scott taper function parameters (default taper.westfall.scott.spp)
#' @return Numeric: Predicted diameter at the specified height
#'
# Westfall, JA; Scott, CT. 2010. Taper Models for Commercial Tree Species in the
# Northeastern United States, Forest Science 56(6): 515-528. doi: 10.1093/forestscience/56.6.515
taper_westfall_scott = function(sp, dbh, ht, ht.dia, 
                                taper.westfall.scott.spp=taper.westfall.scott.spp) {
  
  # parameters
  idx = match(sp, taper.westfall.scott.spp$species, 
              nomatch = match('99', taper.westfall.scott.spp$species))
  parms = taper.westfall.scott.spp[idx, ]
  
  # Calculate taper
  x = dbh/ht
  z = ht.dia/ht
  S1 = parms$th1/(1+(z/parms$th2)^parms$lmd)
  S2 = (z/parms$bet1)^(parms$bet2*x)/(1+(z/parms$bet1)^(parms$bet2*x))
  
  dia = sqrt(dbh^2*(1.37/ht/parms$gm1)^parms$phi*
               ((1-z)/(1-parms$gm1))^(parms$a1+S1)*
               ((1-z)/(1-parms$gm2))^(parms$a2*S2))
  
  dia
}

# Wrapper function
#' Calculate tree taper using specified model
#' 
#' @param model Character: 'kozak' or 'westfall' 
#' @param sp Character: Species code
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param ht Numeric: Total tree height (m)
#' @param ht.dia Numeric: Height at which to predict diameter (m)
#' @param dia.type Character: Diameter type 'ob' (outside bark) or 'ib' (inside bark), kozak only (default 'ib')
#' @param plt Integer: Plantation indicator (1=plantation, 0=natural), kozak only (default 0)
#' @return Numeric: Predicted diameter
#'
calc_taper = function(model = c('kozak', 'westfall'), sp, dbh, ht, ht.dia, 
                      dia.type = 'ib', plt = 0) {
  model = match.arg(model)
  
  if (model == 'kozak') {
    taper_kozak(sp = sp, dbh = dbh, ht = ht, ht.dia = ht.dia, 
                dia.type = dia.type, plt = plt)
  } else {
    taper_westfall_scott(sp = sp, dbh = dbh, ht = ht, ht.dia = ht.dia)
  }
}

# trees.test=trees %>% 
#   mutate(calc.dia=calc_taper(model = 'kozak', sp=.$sp, dbh=.$dbh, ht=.$ht, ht.dia=.$ht*0.9))

#### Bark ratio ####

# Bark ratio parameters 
bark.ratio.spp = dplyr::tribble(
  ~species, ~pcnt.bark, ~b0, ~b1,
  "AB", 7, 1, 1,
  "BC", 10, 1, 1,
  "BF", 0, 0.878, 1.025,
  "BP", 0, 0.8737, 1.012,
  "BA", 0, 0.8499, 1.041,
  "BS", 0, 0.871, 1.026,
  "BT", 15, 1, 1,
  "EH", 0, 0.8916, 1.0121,
  "GA", 13, 1, 1,
  "GB", 12, 1, 1,
  "JP", 0, 0.916, 1.01,
  "NS", 0, 0.8558, 1.0363,
  "PB", 0, 0.8969, 1.0179,
  "QA", 0, 0.8449, 1.0332,
  "RM", 0, 0.9214, 1.0117,
  "RO", 11, 1, 1,
  "RP", 0, 0.928, 0.999,
  "RS", 0, 0.864, 1.029,
  "SB", 12, 1, 1,
  "SM", 0, 0.9383, 1.0064,
  "TL", 0, 1.5106, 0.8134,
  "TA", 0, 1.5106, 0.8134,
  "WA", 0, 0.8834, 1.0188,
  "WC", 0, 0.7797, 1.0569,
  "NC", 0, 0.7797, 1.0569,
  "WP", 0, 0.926, 1,
  "WS", 0, 0.886, 1.022,
  "YB", 0, 0.8688, 1.0275,
  "OH", 0, 0.892283333, 1.01925,
  "OS", 0, 0.887333009, 1.019266336,
  "99", 0, 0.889808171, 1.019266336
)

#' Calculate diameter inside bark (DIB) from diameter outside bark (DOB)
#' 
#' @param sp Character: Species code (FVS alpha)
#' @param dob Numeric: Diameter outside bark (cm)
#' @param bark.ratio.spp Dataframe: Bark ratio parameters (default bark.ratio.spp)
#' @return Numeric vector: Diameter inside bark, rounded to 4 decimal places
#'
calc_dib = function(sp, dob, bark.ratio.spp=bark.ratio.spp) {
  
  # parameters
  idx = match(sp, bark.ratio.spp$species, nomatch = match('99', bark.ratio.spp$species))
  parms = bark.ratio.spp[idx, ]
  
  #calculate
  dib = ifelse(parms$pcnt.bark == 0, 
               parms$b0 * dob^parms$b1,
               dob * (1 - parms$pcnt.bark/100))
  dib = round(dib, 4)
  
  dib
}

#### Merch height ####
#' Find merchantable height where diameter ~equals minimum merchantable diameter threshold
#' @param sp Character: Species code (FVS alpha)
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param ht Numeric: Total height (m)
#' @param min.dia Numeric: Minimum merchantable diameter threshold (cm)
#' @param stump.ht Numeric: Stump height (m; lower bound)
#' @param ht.merch.limit Numeric: Merchantable height limit (m; upper bound for search; default total height) 
#' @param taper.model Character: Taper model to use ('westfall' or 'kozak'; default 'kozak')
#' @param dia.type Character: Diameter type 'ob' (outside bark) or 'ib' (inside bark), kozak only (default 'ib')
#' @param plt Integer: Plantation indicator (1=plantation, 0=natural), kozak only (default 0)
#' @return Numeric: Height where diameter equals min.dia
calc_merch_height = function(sp, dbh, ht, min.dia, stump.ht = 0, ht.merch.limit = ht, 
                             taper.model = 'kozak', dia.type = 'ib', plt = 0) {
  
  # handle data anomalies
  if (min.dia <= 0.01) return(ht) # tip of tree returns total tree height
  if (stump.ht >= ht) return(0)  # Invalid: stump exceeds total height
  if (min.dia >= dbh) return(0)  # Invalid: merch diameter exceeds DBH
  
  # Objective - difference between actual and merch diameter limit
  diameter.diff = function(height) {
    dia = calc_taper(model = taper.model, sp = sp, dbh = dbh, ht = ht, 
                     ht.dia = height, dia.type = dia.type, plt = plt)
    abs(dia - min.dia)
  }
  
  # optimize to find height 
  result = try(optimize(diameter.diff, interval = c(stump.ht, ht.merch.limit), tol = 0.01), 
               silent = TRUE)
  
  if (inherits(result, "try-error")) {
    return(ht.merch.limit)  # Return limit height if optimization fails
  } else {
    return(result$minimum)
  }
}


#### Volume ####

## Volume using Kozak or Westfall taper function


#' Calculate tree volume using specified taper model
#' 
#' @param tree.data Character: Dataframe: Tree list
#' @param taper.model Character: Taper model to use ('westfall' or 'kozak'; default 'kozak')
#' @param sp.col Character: Column name for species codes (default 'sp')
#' @param dbh.col Character: Column name for diameter at breast height (default 'dbh')
#' @param ht.col Character: Column name for total tree height (default 'ht')
#' @param dia.type Character: Diameter type 'ob' (outside bark) or 'ib' (inside bark), kozak only (default 'ib')
#' @param plt Integer: Plantation indicator (1=plantation, 0=natural), kozak only (default 0)
#' @param stump Numeric: Stump height, overridden if a stump height column is provided (default 0.5)
#' @param stump.col Character: Column name for stump height (optional)
#' @param ht.merch.col Character: Column name for merchantable height (optional if you don't want merch height calculated)
#' @param dia.merch Numeric: Minimum diameter threshold, overridden if minimum merchantable diameter column is provided (default 0.001)
#' @param dia.merch.col Character: Column name for minimum diameter threshold (optional; if omitted defaults to single threshhold)
#' @param segments Integer: Number of segments used to calculate volume (default 100)
#' @return Dataframe: Tree data with added field, volume in cubic meters
calc_tree_volume = function(tree.data, taper.model = c('kozak', 'westfall'),
                            sp.col = 'sp', dbh.col = 'dbh', ht.col = 'ht',
                            dia.type = 'ib', plt = 0, stump = 0.5, 
                            stump.col = NULL, ht.merch.col = NULL, 
                            dia.merch = 0.001, dia.merch.col = NULL, segments = 100) {
  
  # Validate inputs
  taper.model = match.arg(taper.model)
  
  # Check required columns 
  required.cols = c(sp.col, dbh.col, ht.col)
  
  missing.cols = setdiff(required.cols, names(tree.data))
  if (length(missing.cols) > 0) {
    stop(paste('Missing required columns:', paste(missing.cols, collapse = ', ')))
  }
  
  tree.data.names= colnames(tree.data)
  
  tree.vol = tree.data %>%
    dplyr::mutate(stump.height = if (!is.null(stump.col) && stump.col %in% names(tree.data)) {
      .data[[stump.col]]
    } else {
      stump},
    dia = if (!is.null(dia.merch.col) && dia.merch.col %in% names(tree.data)) {
      .data[[dia.merch.col]]
    } else {
      dia.merch},
    merch.height = if (!is.null(ht.merch.col) && ht.merch.col %in% names(tree.data)) {
      .data[[ht.merch.col]]
      
    } else { 
      purrr::pmap_dbl(list(sp = .data[[sp.col]], 
                           dbh = .data[[dbh.col]], 
                           ht = .data[[ht.col]],
                           min.dia = dia,
                           stump.ht = stump.height,
                           taper.model = taper.model),
                      calc_merch_height)
    })
  
  # records which won't have volume calculated
  tree.novol=tree.vol %>% 
    filter(is.na(.data[[sp.col]]) | 
             is.na(.data[[dbh.col]]) | 
             is.na(.data[[ht.col]]) |
             is.na(stump.height) |
             is.na(dia) |
             merch.height<=stump.height) %>% 
    mutate(volume=0)
  
  
  tree.vol=tree.vol %>% 
    dplyr::filter(!is.na(.data[[sp.col]]),
                  !is.na(.data[[dbh.col]]), 
                  !is.na(.data[[ht.col]]),
                  !is.na(stump.height),
                  !is.na(dia),
                  merch.height>stump.height) %>%         
    # Calculate Smalians cubic meter volume using nested tibble
    dplyr::mutate(segment.data = purrr::pmap(list(sp = .data[[sp.col]], 
                                                  dbh = .data[[dbh.col]], 
                                                  ht = .data[[ht.col]], 
                                                  stump = stump.height, 
                                                  merch = merch.height),
                                             function(sp, dbh, ht, stump, merch) {
                                               tibble::tibble(
                                                 segment = 1:(segments + 1),
                                                 segment.height = seq(stump,
                                                                      merch,
                                                                      length.out = segments + 1) ,
                                                 diameter = purrr::map_dbl(segment.height,
                                                                           ~ calc_taper(model = taper.model,
                                                                                        sp = sp, dbh = dbh,
                                                                                        ht = ht, ht.dia = .x,
                                                                                        dia.type = dia.type, plt = plt)),
                                                 area.upper = pi * ((diameter / 100) / 2)^2,
                                                 seg.len = (merch - stump) / segments) %>%
                                                 dplyr::mutate(area.lower = dplyr::lag(area.upper, default = NA),
                                                               segment.volume = dplyr::coalesce(((area.lower + area.upper) / 2) * seg.len, 0))
                                             }),
                  volume = purrr::map_dbl(segment.data, 
                                          ~ round(sum(.x$segment.volume, na.rm = TRUE), 6))) %>%
    dplyr::bind_rows(tree.novol) %>% 
    dplyr::select(dplyr::all_of(tree.data.names), volume)
  
  tree.vol
}

#vol=calc_tree_volume(tree, segments = 10)

## Honer volume 

# Honer volume parameters
honer.vol.spp = dplyr::tribble(
  ~species, ~a, ~b, ~b2, ~p1.ht, ~p2.ht, ~p3.ht, ~p1.d, ~p2.d, ~p3.d,
  'WP', 0.691, 363.676, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'RP', 0.710, 355.623, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'JP', 0.897, 348.520, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'TA', 0.897, 348.520, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'LP', 0.694, 343.896, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'BS', 1.588, 333.364, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'RS', 1.226, 315.832, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'WS', 1.440, 342.175, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'BF', 2.139, 301.634, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'NC', 4.167, 244.906, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'WC', 4.167, 244.906, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'EH', 1.112, 350.092, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'QA', -0.312, 436.683, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'BP', 0.420, 394.644, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'BT', 0.420, 394.644, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'PB', 2.222, 300.373, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'GB', 2.222, 300.373, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'SB', 2.222, 300.373, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'YB', 1.449, 344.754, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'RM', 1.046, 383.972, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'SM', 1.046, 383.972, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'BW', 0.948, 401.456, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'AB', 0.959, 334.829, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'WA', 0.959, 334.829, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'BC', 0.033, 393.336, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'RO', 1.512, 336.509, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736,
  'OH', 1.512, 336.509, 0.184, 0.0145, 2.1164, -1.1387, 1.0180, -0.2323, -0.7736)

# Columns p1.ht through p3.d are not used in volume calculations


#' Calculate Honer volume
#' 
#' @param sp Character: Species code (FVS alpha)
#' @param ht Numeric: Total tree height (m)
#' @param dbh Numeric: Diameter at breast height (cm)
#' @param honer.vol.spp Dataframe: Honer volume parameters
#' @return Numeric: Tree volume  (cubic meters)
#'
calc_honer_vol = function(sp, ht, dbh, honer.vol.spp) {
  
  # Get parameters for species
  idx = match(sp, honer.vol.spp$species, 
              nomatch = match('OH', honer.vol.spp$species))
  parms = honer.vol.spp[idx, ]
  
  # Unit conversions
  dbh.inch = dbh / 2.54
  ht.feet = ht / 0.3048
  
  # Calculate volume in cubic feet, convert to cubic meters
  vtcf = dbh.inch^2 / (parms$a + (parms$b / ht.feet))
  vtm3 = vtcf * 0.02831685
  
  vtm3
}

# # Calculate Honer volumes
# tree = tree %>%
#   mutate(volume_honer = calc_honer_vol(sp = sp, ht = ht, dbh = dbh))


### hardwood sawlog proportion####
hw.saw.prop=function(sp,dbh,form,risk){
    
    #Convert NHRI form classes
    if(form == 'F1'){new.form = 'GF'}
    else if(form == 'F2' | form == 'F6' | form == 'F5' | form == 'F8'){new.form = 'AF'}
    else{new.form = 'PF'} #We could have catch for other form types but may not be needed
    
    #Convert NHRI risk classes
    if(risk == 'R1' | risk == 'R2'){new.risk = 'LR'}
    else{new.risk = 'HR'} #We could have catch for other risk types but probably not needed.
    
    if(sp=='RO'){SPP.RO=1; SPP.SM=0; SPP.YB=0; SPP.RM=0}
    else if(sp=='SM'){SPP.SM=1; SPP.RO=0; SPP.YB=0; SPP.RM=0}
    else if(sp=='YB'){SPP.YB=1; SPP.RO=0; SPP.SM=0; SPP.RM=0}
    else if(sp=='RM'){SPP.RM=1; SPP.RO=0; SPP.SM=0; SPP.YB=0}
    else{SPP.RO=0; SPP.SM=0; SPP.YB=0; SPP.RM=0}
    if(new.form=='GF'){GF=1; PF=0}
    else if(new.form=='PF'){GF=0; PF=1}
    else{GF = 0; PF = 0}
    if(new.risk=='HR'){HR=1; LR=0}
    else{HR=0; LR=1}
    saw=exp(-33.2222 -0.2249*dbh + 11.3953*log(dbh)+ 0.7994*SPP.RM-0.4849*SPP.RO+0.9174*SPP.SM+1.2707*SPP.YB+
                0.3393*GF-0.6922*PF+0.9062*LR-0.0441*(dbh*SPP.RM) +0.0015*(dbh*SPP.RO)-0.0412*(dbh*SPP.SM)-0.0548*(dbh*SPP.YB))/
        (1+exp(-33.2222 -0.2249*dbh + 11.3953*log(dbh)+ 0.7994*SPP.RM-0.4849*SPP.RO+0.9174*SPP.SM+1.2707*SPP.YB+
                   0.3393*GF-0.6922*PF+0.9062*LR-0.0441*(dbh*SPP.RM) +0.0015*(dbh*SPP.RO)-0.0412*(dbh*SPP.SM)-0.0548*(dbh*SPP.YB)))
    saw=ifelse(sp %in% c('RO', 'SM', 'YB', 'RM','PB'), saw, 1)
    return(saw=saw)}

#hw.saw.prop('RM',20,'LSW','LR')

#### Summary ####
Summary.GY=function(tree){
    #library(nlme)
  
  # species functional attributes
  tree=tree %>% 
    dplyr::left_join(sp.attr, 
                     by=c('sp'='species'))
    #aa=sapply(tree$SP,SPP.func)
    #tree$SPtype=t(aa)[,1]
    #tree$shade=as.numeric(t(aa)[,2])
    #tree$sg=as.numeric(t(aa)[,3])
    tree$ba<-round((tree$dbh^2*0.00007854)*tree$expf,2)
    tree$sdi=(tree$dbh/25.4)^1.605*tree$expf
    tree$sg.wt=tree$sg*tree$expf
    tree$ba.WP=ifelse(tree$sp=='WP',tree$ba,0)
    tree$ba.BF=ifelse(tree$sp=='BF',tree$ba,0)
    tree$ba.RM=ifelse(tree$sp=='RM',tree$ba,0)
    tree$ba.RS=ifelse(tree$sp=='RS',tree$ba,0)
    tree$ba.BS=ifelse(tree$sp=='BS',tree$ba,0)
    tree$ba.WS=ifelse(tree$sp=='WS',tree$ba,0)
    tree$ba.PB=ifelse(tree$sp=='PB',tree$ba,0)
    tree$ba.YB=ifelse(tree$sp=='YB',tree$ba,0)
    tree$ba.GB=ifelse(tree$sp=='GB',tree$ba,0)
    
    # Add AB, QA, SM, WC
    tree$ba.AB=ifelse(tree$sp=='AB',tree$ba,0)
    tree$ba.QA=ifelse(tree$sp=='QA',tree$ba,0)
    tree$ba.SM=ifelse(tree$sp=='SM',tree$ba,0)
    tree$ba.WC=ifelse(tree$sp=='WC',tree$ba,0)
    
    tree$ba.HW=ifelse(tree$sp.type=='HW',tree$ba,0)
    tree$ba.SW=ifelse(tree$sp.type!='HW',tree$ba,0)
    tree$cr=((tree$ht-tree$hcb)/tree$ht)*tree$expf
    tree$ht=tree$ht*tree$expf
    temp <- subset(tree,select=c("year","stand","plot",'tree','dbh','ht','cr',
                                 'expf',"ba",'ba.WP','ba.BF','ba.RM','ba.RS','ba.BS','ba.PB','ba.YB',
                                 'ba.AB','ba.QA','ba.SM','ba.WC',                                       # Add AB, QA, SM, WC
                                 'ba.GB','ba.WS','sdi','ba.HW','ba.SW','sg.wt'))
    # temp<-groupedData(ba~ba|stand/plot/YEAR,data=temp)
    # temp <- gsummary(temp,sum,na.rm=TRUE)
    
    temp=temp %>% 
      dplyr::group_by(stand,
                      plot,
                      year) %>% 
      dplyr::summarise(ba.plot=sum(ba),
                tph=sum(expf),
                Avg.sg=sum(sg.wt)/sum(ba),
                SDI=sum(sdi,na.rm=T),
                avgDBH=mean(dbh,na.rm=T),
                sdDBH=sd(dbh,na.rm=T),
                minDBH=min(dbh,na.rm=T),
                maxDBH=max(dbh,na.rm=T), #  
                dbh.RANGE.all=max(dbh,na.rm=T)-min(dbh,na.rm=T),
                
                pHW.ba=sum(ba.HW)/ba.plot,
                pSW.ba=sum(ba.SW)/ba.plot,
                pWP.ba=sum(ba.WP)/ba.plot,
                ba.perc.bf=sum(ba.BF)/ba.plot,
                pRM.ba=sum(ba.RM)/ba.plot,
                pRS.ba=sum(ba.RS)/ba.plot,
                pBS.ba=sum(ba.BS)/ba.plot,
                pWS.ba=sum(ba.WS)/ba.plot,
                pPB.ba=sum(ba.PB)/ba.plot,
                pYB.ba=sum(ba.YB)/ba.plot,
                pGB.ba=sum(ba.GB)/ba.plot,
                # Add AB, QA, SM, WC
                pAB.ba=sum(ba.AB)/ba.plot,
                pQA.ba=sum(ba.QA)/ba.plot,
                pSM.ba=sum(ba.SM)/ba.plot,
                pWC.ba=sum(ba.WC)/ba.plot,
                
                Avg.HT= sum(ht)/tph,
                Avg.LCR= sum(cr)/tph ,
                .groups='keep') %>% 
      mutate(qmd=sqrt(ba.plot/(0.00007854*tph)),
             Avg.sg=ifelse(Avg.sg>.68,.68,Avg.sg),
             SDImax=-6017.3*Avg.sg+4156.3,
             rd=SDI/SDImax)
    
    temp=subset(temp,select=c("year","stand","plot",'ba.plot','tph',
                              'qmd','SDI','Avg.HT','Avg.LCR','pWP.ba','ba.perc.bf','pRM.ba',
                              'pRS.ba','pBS.ba','pWS.ba','pPB.ba','pYB.ba','pGB.ba','rd',
                              'pAB.ba','pQA.ba','pSM.ba','pWC.ba','pHW.ba','pSW.ba'))
    temp
}        
