# $Id$

###########################################################################################################################################################
# Acadian Variant of the Forest Vegetation Simulator (FVS-ACD)                                                                                            #
# Developed by Aaron Weiskittel, University of Maine, School of Forest Resources
# aaron.weiskittel@maine.edu
# 
# Updates include:                                                                                                                                        #
# 1. Revised diameter increment equation
# 2. Fixed issues with ING.TreeList() function
# 3. Added commercial thinning modifiers for dDBH, dHT, dHCB, and mortality based on results from Kuehne et al. (2016)
# 4. Added spruce budworm (SBW) modifiers for dDBH, dHT, and mortality based on results from Cen et al. (2016)
#
# For commercial thinning modifiers, proportion of basal area removed 'pBArm', ratio of QMD before and after thinning 'QMDratio', 
# stand basal area at the time of thinning 'BApre', and year of commerical thinning 'YEAR_CT' need to be defined
#
# For SBW modifiers, cummulative defoliation % 'CDEF', initial year of spruce budworm outbreak 'SBW.YR', and duration of spruce budworm
# outbreak in years 'SBW.DUR'
#
#
###########################################################################################################################################################

library(plyr)  #needed for ddply

AcadianVersionTag = "AcadianV9.1"

#Define all functions below
#sort data frames
sort.data.frame <- function(form,dat)
{
  if(inherits(dat,"formula")){
    f=dat
    dat=form
    form=f
  }
  if(form[[1]] != "~") stop("Formula must be one-sided.")
  formc <- as.character(form[2])
  formc <- gsub(" ","",formc)
  if(!is.element(substring(formc,1,1),c("+","-"))) formc <- paste("+",formc,sep="")
  vars <- unlist(strsplit(formc, "[\\+\\-]"))
  vars <- vars[vars!=""]
  calllist <- list()
  pos=1
  for(i in 1:length(vars)){
    varsign <- substring(formc,pos,pos)
    pos <- pos+1+nchar(vars[i])
    if(is.factor(dat[,vars[i]])){
      if(varsign=="-")
        calllist[[i]] <- -rank(dat[,vars[i]])
      else
        calllist[[i]] <- rank(dat[,vars[i]])
    }
    else {
      if(varsign=="-")
        calllist[[i]] <- -dat[,vars[i]]
      else
        calllist[[i]] <- dat[,vars[i]]
    
    }
  }
  dat[do.call("order",calllist),]
}


#Species function
SPP.func=function(SPP)
{            
  SPcodes = c( 
    'AB',  # AB=American beech
    'AS',  # AS=ash                  
    'BA',  # BA=black ash
    'BC',  # BC=black cherry
    'BF',  # BF=balsam fir
    'BP',  # BP=balsam poplar         
    'BS',  # BS=black spruce
    'BT',  # BT=bigtooth aspen
    'EC',  # EC=eastern cottonwood
    'EH',  # EH=eastern hemlock
    'GA',  # GA=green ash
    'GB',  # GB=gray birch
    'HH',  # HH=eastern hophornbeam
    'JP',  # JP=jack pine
    'NS',  # NS=Norway spruce
    'OH',  # OH=other hardwoods
    'OS',  # OS=other softwoods
    'PB',  # PB=paper birch
    'PC',  # PC=pin cherry  
    'PR',  # PR=pin cherry 
    'QA',  # QA=quaking aspen
    'RB',  # RB=river birch
    'RM',  # RM=red maple
    'RP',  # RP=red pine 
    'RN',  # RN=red pine
    'RO',  # RO=red oak
    'RS',  # RS=red spruce
    'SB',  # SB=Sweet birch
    'SM',  # SM=sugar maple
    'ST',  # ST=striped maple
    'TA',  # TA=larch/tamarack
    'WA',  # WA=white ash
    'WC',  # WC=northern white cedar
    'WP',  # WP=white pine
    'WS',  # WS=white spruce
    'YB',  # YB=yellow birch
    '99')  # other
   SPtype = c(
    'HW', # AB=American beech
    'HW', # AS=ash
    'HW', # BA=black ash
    'HW', # BC=black cherry
    'SW', # BF=balsam fir
    'HW', # BP=balsam poplar
    'SW', # BS=black spruce
    'HW', # BT=bigtooth aspen
    'HW', # EC=eastern cottonwood
    'SW', # EH=eastern hemlock
    'HW', # GA=green ash
    'HW', # GB=gray birch
    'HW', # HH=eastern hophornbeam
    'SW', # JP=jack pine
    'SW', # NS=Norway spruce
    'HW', # OH=other hardwoods
    'SW', # OS=other softwoods
    'SW', # PB=paper birch
    'HW', # PC=pin cherry  
    'HW', # PR=pin cherry 
    'HW', # QA=quaking aspen
    'HW', # RB=river birch
    'HW', # RM=red maple
    'SW', # RP=red pine 
    'SW', # RN=red pine
    'HW', # RO=red oak
    'SW', # RS=red spruce
    'HW', # SB=Sweet birch
    'HW', # SM=sugar maple
    'HW', # ST=striped maple
    'SW', # TA=larch/tamarack
    'HW', # WA=white ash
    'SW', # WC=northern white cedar
    'SW', # WP=white pine
    'SW', # WS=white spruce
    'HW', # YB=yellow birch
    'HW') # other
   attrs = matrix (c(
    # sg      wd     shade drought  waterlog
      0.64  ,0.56   , 4.75 , 1.5  , 1.5 ,    # AB=American beech
      0.57  ,0.51   , 2.84 , 2.74 , 3.02,    # AS=ash                 
      0.5   ,0.45   , 2.96 , 2    , 3.5 ,    # BA=black ash
      0.5   ,0.47   , 2.46 , 3.02 , 1.06,    # BC=black cherry
      0.35  ,0.33   , 5.01 , 1    , 2   ,    # BF=balsam fir
      0.34  ,0.31   , 1.27 , 1.77 , 2.63,    # BP=balsam poplar
      0.46  ,0.38   , 4.08 , 2.0  , 2.0 ,    # BS=black spruce
      0.39  ,0.36   , 1.21 , 2.5  , 2   ,    # BT=bigtooth aspen
      0.4   ,0.37   , 1.76 , 1.57 , 3.03,    # EC=eastern cottonwood
      0.4   ,0.38   , 4.83 , 1    , 1.25,    # EH=eastern hemlock
      0.56  ,0.53   , 3.11 , 3.85 , 2.98,    # GA=green ash
      0.48  ,0.45   , 1.5  , 2.34 , 1   ,    # GB=gray birch
      0.78  ,0.63   , 4.58 , 3.25 , 1.07,    # HH=eastern hophornbeam
      0.43  ,0.4    , 1.36 , 4    , 1   ,    # JP=jack pine
      0.43  ,0.37023, 4.45 , 1.75 , 1.22,    # NS=Norway spruce
      0.5121,0      , 2.29 ,  0   , 0   ,    # OH=other hardwoods
      0.445 ,0      , 2.27 , 0    , 0   ,    # OS=other softwoods
      0.55  ,0.48   , 1.54 , 2.02 , 1.25,    # PB=paper birch
      0.38  ,0.36   , 2.26 , 0    , 0   ,    # PC=pin cherry  
      0.38  ,0.36   , 2.26 , 0    , 0   ,    # PR=pin cherry 
      0.38  ,0.35   , 1.21 , 1.77 , 1.77,    # QA=quaking aspen
      0.62  ,0.49   , 1.45 , 1.53 , 2.85,    # RB=river birch
      0.54  ,0.49   , 3.44 , 1.84 , 3.08,    # RM=red maple
      0.46  ,0.41   , 1.89 , 3    , 1   ,    # RP=red pine 
      0.46  ,0.41   , 1.89 , 3    , 1   ,    # RN=red pine
      0.63  ,0.56   , 2.75 , 2.88 , 1.12,    # RO=red oak
      0.4   ,0.37   , 4.39 , 2.5  , 2   ,    # RS=red spruce
      0.65  ,0.6    , 2.58 , 3    , 1   ,    # SB=Sweet birch
      0.63  ,0.56   , 4.76 , 2.25 , 1.09,    # SM=sugar maple
      0.46  ,0.44   , 3.56 , 2    , 1   ,    # ST=striped maple
      0.53  ,0.49   , 0.98 , 2    , 3   ,    # TA=larch/tamarack
      0.6   ,0.55   , 2.46 , 2.38 , 2.59,    # WA=white ash             
      0.31  ,0.29   , 3.45 , 2.71 , 1.46,    # WC=northern white cedar
      0.35  ,0.34   , 3.21 , 2.29 , 1.03,    # WP=white pine
      0.4   ,0.33   , 4.15 , 2.88 , 1.02,    # WS=white spruce
      0.62  ,0.55   , 3.17 , 3    , 2   ,    # YB=yellow birch
      0.3   ,0.3    , 3.0  , 0    , 0   ),   # other
        ncol=5,byrow=TRUE)
  sprow = match(SPP,SPcodes)
  sprow[is.na(sprow)] = length(SPcodes)   
  return(list(SPtype=SPtype[sprow],shade=attrs[sprow,3],sg=attrs[sprow,1]))
}
 

#Maximum crown width
mcw=function(sp,dbh)
{
  SPcodes=c('BF','BS','EH','WP','NC','RS','WS','AB','GB','RB','RO','PB','QA',
            'RM','SM','YB','OH','OS','99')       
  coefs = matrix(c(
   # a1           a2
    1.37       , 0.572      ,    # BF
    0.535      , 0.742      ,    # BS
    2.44       , 0.408      ,    # EH   
    1.24       , 0.585      ,    # WP
    1.63       , 0.436      ,    # NC
    1.80       , 0.461      ,    # RS
    1.50       , 0.496      ,    # WS
    2.93       , 0.434      ,    # AB
    2.24       , 0.382      ,    # GB
    2.24       , 0.382      ,    # RB
    4.08       , 0.310      ,    # RO
    1.48       , 0.623      ,    # PB
    1.31       , 0.586      ,    # QA
    2.17       , 0.491      ,    # RM
    3.31       , 0.356      ,    # SM
    4.04       , 0.308      ,    # YB
    4.04       , 0.308      ,    # OH
    1.597128571, 0.513957143,    # OS
    2.24262    , 0.462653333),   # 99
    ncol=2,byrow=TRUE)
  sprow = match(sp,SPcodes)
  sprow[is.na(sprow)] = length(SPcodes)  
  mcw=coefs[sprow,1]*dbh**coefs[sprow,2]
  return(mcw)
}

   
#These are the parm ests for estimating largest crown width (lcw)
lcw=function(sp,mcw,dbh){
  SPcodes=c('BF','BS','EH','WP','NC','RS','WS','AB','GB','RB','RO','PB','QA',
            'RM','SM','YB','OH','OS','99')
  coefs = matrix(c(
   # b1           b2            
     1.49   , 0.105       ,   # BF     
     1      , 0.174       ,   # BS
     1.90   , -0.057      ,   # EH
     1      , 0.147       ,   # WP
     2.19   , -0.080      ,   # NC
     4.33   , -0.264      ,   # RS
     2.09   , -0.069      ,   # WS
     1      , 0.194       ,   # AB
     3.10   , -0.214      ,   # GB
     3.10   , -0.214      ,   # RB
     4.10   , -0.272      ,   # RO
     2.10   , -0.035      ,   # PB
     2.65   , 0.157       ,   # QA
     2.63   , -0.132      ,   # RM
     1      , 0.161       ,   # SM
     4.23   , -0.264      ,   # YB
     2.65   , 0.157       ,   # OH
     2.3276 , 0.027842857 ,   # OS
     2.79282, -0.090113333),  # 99
    ncol=2,byrow=TRUE)     
  sprow = match(sp,SPcodes)
  sprow[is.na(sprow)] = length(SPcodes)  
  lcw=mcw/(coefs[sprow,1]*dbh**coefs[sprow,2])      
  return(lcw)
}   



### Total height prediction function (updated 8/31/2012) using species as a random effect
HTPred=function(SPP,DBH,CSI,CCF,BAL) 
{     
  c0=  12.44847305
  c1=  0.801705832
  c2=  0.043617034
  c3=  1.048674338
  c4=  0.011483716
  c5=  -0.007550999
  if   (SPP=='AB')  {c0.spp=-1.63260226433876; c3.spp=-0.123848276720533}
  else if(SPP=='AE'){c0.spp=-0.692010776894357; c3.spp=0.0346080772461358}
  else if(SPP=='AH'){c0.spp=-5.98009416964362; c3.spp=-0.032783189788012}
  else if(SPP=='AI'){c0.spp=-6.44978562263189; c3.spp=-0.0984022226851643}
  else if(SPP=='AP'){c0.spp=-12.0735361325049; c3.spp=-0.475976304087567}
  else if(SPP=='AS'){c0.spp=6.69760483331092; c3.spp=-0.125318550191217}
  else if(SPP=='BA'){c0.spp=-1.61716890163543; c3.spp=-0.141587177559468}
  else if(SPP=='BC'){c0.spp=-4.52724204655813; c3.spp=-0.172605143041673}
  else if(SPP=='BE'){c0.spp=-1.60563943164767; c3.spp=-0.424565045666305}
  else if(SPP=='BF'){c0.spp=1.77471065080046; c3.spp=0.1571978021787}
  else if(SPP=='BL'){c0.spp=-9.82751389751524; c3.spp=-0.292624067773788}
  else if(SPP=='BN'){c0.spp=0.861243905640667; c3.spp=-0.103226577993538}
  else if(SPP=='BO'){c0.spp=1.17024253111731; c3.spp=-0.0431150821737857}
  else if(SPP=='BP'){c0.spp=2.52163661595498; c3.spp=-0.0633568443480465}
  else if(SPP=='BR'){c0.spp=5.44303876725562; c3.spp=0.354363882079203}
  else if(SPP=='BS'){c0.spp=3.88571664605334; c3.spp=0.1886808269048}
  else if(SPP=='BT'){c0.spp=5.2832906396451; c3.spp=-0.0670620453873463}
  else if(SPP=='BW'){c0.spp=2.52499880080404; c3.spp=0.153925181183304}
  else if(SPP=='EC'){c0.spp=7.0881673102164; c3.spp=0.182126907461261}
  else if(SPP=='EH'){c0.spp=0.0643545746867161; c3.spp=0.260671290553969}
  else if(SPP=='EL'){c0.spp=-0.460173779709119; c3.spp=0.194209222023289}
  else if(SPP=='GA'){c0.spp=-1.47263156202317; c3.spp=-0.0743884734979349}
  else if(SPP=='GB'){c0.spp=-1.03938349314791; c3.spp=-0.238717166776341}
  else if(SPP=='HH'){c0.spp=-2.45397316779551; c3.spp=-0.17636502944365}
  else if(SPP=='HK'){c0.spp=-0.139225685811506; c3.spp=-0.107092112450714}
  else if(SPP=='HT'){c0.spp=-0.498373011862981; c3.spp=0.000508524335021695}
  else if(SPP=='JP'){c0.spp=12.2041796567474; c3.spp=0.507127061137884}
  else if(SPP=='NC'){c0.spp=-0.154777524407996; c3.spp=0.0506677142901254}
  else if(SPP=='NS'){c0.spp=10.7572546403292; c3.spp=0.761510842024224}
  else if(SPP=='OH'){c0.spp=-0.152274609199728; c3.spp=-0.0773943323672784}
  else if(SPP=='OP'){c0.spp=-1.25201364651662; c3.spp=0.19704750471505}
  else if(SPP=='OS'){c0.spp=6.64418468070433; c3.spp=0.154974733190601}
  else if(SPP=='PB'){c0.spp=2.85568786741337; c3.spp=-0.053133050063968}
  else if(SPP=='PI'){c0.spp=6.11926846598162; c3.spp=0.396180643433203}
  else if(SPP=='PL'){c0.spp=-12.5774578312843; c3.spp=-0.354402924932074}
  else if(SPP=='PP'){c0.spp=-2.03524755338192; c3.spp=0.0284495830511636}
  else if(SPP=='PR'){c0.spp=-5.27943940700261; c3.spp=-0.276675997378359}
  else if(SPP=='PY'){c0.spp=2.39961434182412; c3.spp=-0.0302798406740612}
  else if(SPP=='QA'){c0.spp=5.28547878831447; c3.spp=-0.0166932060459991}
  else if(SPP=='RC'){c0.spp=-13.3554875880232; c3.spp=-0.364956123989416}
  else if(SPP=='RL'){c0.spp=-13.464860796814; c3.spp=-0.373319415146871}
  else if(SPP=='RM'){c0.spp=1.13361861116141; c3.spp=-0.124923006598654}
  else if(SPP=='RN'){c0.spp=2.35424925615196; c3.spp=0.4332474439509}
  else if(SPP=='RO'){c0.spp=0.567158528857343; c3.spp=5.49241830858304E-05}
  else if(SPP=='RS'){c0.spp=3.28913339723299; c3.spp=0.197832299388656}
  else if(SPP=='SB'){c0.spp=5.0591881231944; c3.spp=0.263270570106115}
  else if(SPP=='SC'){c0.spp=-1.77707771556552; c3.spp=0.272984904670842}
  else if(SPP=='SE'){c0.spp=-1.20436304154548; c3.spp=-0.217453987421684}
  else if(SPP=='SH'){c0.spp=3.42398432816088; c3.spp=0.00852401379505827}
  else if(SPP=='SM'){c0.spp=1.83135273162116; c3.spp=-0.1509017085778}
  else if(SPP=='SO'){c0.spp=-0.337608194904877; c3.spp=0.00266584203067429}
  else if(SPP=='ST'){c0.spp=-4.21455499968947; c3.spp=-0.158534452384565}
  else if(SPP=='SV'){c0.spp=-1.66795214963112; c3.spp=-0.180378852473763}
  else if(SPP=='SW'){c0.spp=1.3081369384269; c3.spp=0.031660020193251}
  else if(SPP=='TA'){c0.spp=3.03898203229266; c3.spp=-0.070139469703331}
  else if(SPP=='WA'){c0.spp=1.58571626982993; c3.spp=-0.152599799179656}
  else if(SPP=='WC'){c0.spp=-2.63796730677436; c3.spp=0.157097825004126}
  else if(SPP=='WO'){c0.spp=-0.179572014160004; c3.spp=0.050014945223132}
  else if(SPP=='WP'){c0.spp=1.89177965275704; c3.spp=0.217933074706457}
  else if(SPP=='WS'){c0.spp=2.83053645408208; c3.spp=0.284913386127066}
  else if(SPP=='YB'){c0.spp=-1.13450171811265; c3.spp=-0.179629568670318}
  else{c0.spp=0; c3.spp=0}
  ht=(1.37+((c0+c0.spp)+CSI^c1)*(1-exp(-c2*DBH))^(c3+c3.spp+c4*log(CCF+1)+c5*BAL))
  #ht=(1.37+((c0)+CSI^c1)*(1-exp(-c2*DBH))^(c3+c4*log(CCF+1)+c5*BAL))
  return(ht=ht)
}

### Height to crown base function (updated 9/11/12 using species as random effect)
HCBPred=function(SPP,DBH,HT,CCF,BAL) 
{     ## Respective parameters are in order
  DHR=DBH/HT
  a0=  0.29070
  a1=  0.00636
  a2=  -0.02288
  a3=  0.08232
  a4=  -0.03086
  a5=  -0.01701
  if(SPP=='AB'){a0.spp=-0.218384027}
  else if(SPP=='AI'){a0.spp=0.081713772}
  else if(SPP=='AP'){a0.spp=0.177509753}
  else if(SPP=='BA'){a0.spp=0.112652176}
  else if(SPP=='BC'){a0.spp=-0.198822609}
  else if(SPP=='BF'){a0.spp=0.093585699}
  else if(SPP=='BP'){a0.spp=-0.136722421}
  else if(SPP=='BR'){a0.spp=0.015598341}
  else if(SPP=='BS'){a0.spp=-0.227771445}
  else if(SPP=='BT'){a0.spp=0.010040571}
  else if(SPP=='EC'){a0.spp=0.507259999}
  else if(SPP=='EH'){a0.spp=0.403937729}
  else if(SPP=='GB'){a0.spp=-0.181632328}
  else if(SPP=='HK'){a0.spp=0.114597638}
  else if(SPP=='JP'){a0.spp=-0.270782917}
  else if(SPP=='NC'){a0.spp=-0.20514384}
  else if(SPP=='NS'){a0.spp=0.955552766}
  else if(SPP=='OH'){a0.spp=-0.042894377}
  else if(SPP=='OP'){a0.spp=-0.718040335}
  else if(SPP=='PB'){a0.spp=-0.180946077}
  else if(SPP=='PI'){a0.spp=0.548550524}
  else if(SPP=='PR'){a0.spp=-0.270887959}
  else if(SPP=='QA'){a0.spp=0.060833757}
  else if(SPP=='RM'){a0.spp=-0.202478201}
  else if(SPP=='RN'){a0.spp=-0.150518738}
  else if(SPP=='RO'){a0.spp=-0.37021733}
  else if(SPP=='RS'){a0.spp=-0.121308265}
  else if(SPP=='SC'){a0.spp=0.244204218}
  else if(SPP=='SE'){a0.spp=0.24302215}
  else if(SPP=='SM'){a0.spp=-0.16382426}
  else if(SPP=='TA'){a0.spp=-0.304007105}
  else if(SPP=='WA'){a0.spp=-0.148338171}
  else if(SPP=='WC'){a0.spp=0.419622685}
  else if(SPP=='WO'){a0.spp=0.167213142}
  else if(SPP=='WP'){a0.spp=-0.146685117}
  else if(SPP=='WS'){a0.spp=0.100275538}
  else if(SPP=='YB'){a0.spp=0.003235064}
  else{a0.spp=0}
  #hcb=HT/(1+exp(-((a0+a0.spp)+a1*DBH+a2*HT+a3*DHR+a4*log(CCF+1)+a5*(BAL+1))))^(1/6)
  hcb=HT/(1+exp((a0+a0.spp)+a1*DBH+a2*HT+a3*DHR+a4*log(CCF+1)+a5*(BAL+1)))
  return(hcb=hcb)
}

#Kershaw's basal area increment model (7/25/2012)
dBA=function(SPP,DBH,BalSW,BalHW,SI)
{
  CSA=.00007854*DBH^2
  b0 = 0.0413607 #0.0131971 13504625    3.1341  0.0017
  b1 = 0.7185669 #0.0389550 13504625   18.4461  0.0000
  b2 =-0.0000131 #0.0000008 13504625  -16.3136  0.0000
  b3 =-1.8972591 #1.4650514 13504625   -1.2950  0.1953
  b4 =-0.2036994 #0.0004054 13504625 -502.4644  0.0000
  b5 =-0.1413207 #0.0005636 13504625 -250.7436  0.0000
  if(SPP=='WC'){b0.sp=-0.0324097292965914; b1.sp=0.0243026410845147; b3.sp=0.430381603974945}
  else if(SPP=='AB'){b0.sp=-0.032155055953277; b1.sp=-0.0758315347115737; b3.sp=-2.50500288487142}
  else if(SPP=='AE'){b0.sp=-0.00289573737773235; b1.sp=0.164454907254628; b3.sp=-6.02159281182253}
  else if(SPP=='AH'){b0.sp=-0.0387554478607925; b1.sp=-0.131070957848901; b3.sp=0.86295454504863}
  else if(SPP=='AP'){b0.sp=-0.0404142788897025; b1.sp=-0.417150782675109; b3.sp=12.2287533013831}
  else if(SPP=='BA'){b0.sp=-0.0406718461197018; b1.sp=-0.448802152885325; b3.sp=13.5274116013659}
  else if(SPP=='BC'){b0.sp=-0.0395355190760311; b1.sp=-0.39435159750363; b3.sp=10.1996802993411}
  else if(SPP=='BF'){b0.sp=-0.0238599785985451; b1.sp=0.0599181413858148; b3.sp=-1.96717173815071}
  else if(SPP=='BL'){b0.sp=-0.0254421996077096; b1.sp=-0.0458182877381106; b3.sp=0.223430786484162}
  else if(SPP=='BN'){b0.sp=0.230460620310085; b1.sp=0.448539282082155; b3.sp=-9.00775634833792}
  else if(SPP=='BO'){b0.sp=-0.0379602681851854; b1.sp=-0.410744374095436; b3.sp=9.13627859872479}
  else if(SPP=='BP'){b0.sp=-0.0208547961977369; b1.sp=0.000958603708340414; b3.sp=-7.82720052419057}
  else if(SPP=='BS'){b0.sp=-0.037626849456682; b1.sp=-0.1348833308022; b3.sp=3.17628561025152}
  else if(SPP=='BT'){b0.sp=-0.0354478542328709; b1.sp=-0.239043702000071; b3.sp=3.99536659734977}
  else if(SPP=='BW'){b0.sp=-0.00947170831202568; b1.sp=0.325441763047741; b3.sp=-3.9987202147779}
  else if(SPP=='CC'){b0.sp=0.0714993646284334; b1.sp=0.394591009428293; b3.sp=-3.09637218566955}
  else if(SPP=='EH'){b0.sp=-0.0212940442931163; b1.sp=0.0863071462419658; b3.sp=-0.671821932228896}
  else if(SPP=='GA'){b0.sp=0.180801022123542; b1.sp=0.737159162730291; b3.sp=-12.1095097605942}
  else if(SPP=='GB'){b0.sp=-0.0405993487574885; b1.sp=-0.479578793537049; b3.sp=12.7602000821997}
  else if(SPP=='HH'){b0.sp=-0.0381534872012484; b1.sp=-0.147981828194394; b3.sp=1.19510436897729}
  else if(SPP=='HT'){b0.sp=-0.03321566435831; b1.sp=-0.0476969903766449; b3.sp=1.21464678110164}
  else if(SPP=='JP'){b0.sp=-0.00962669999406432; b1.sp=0.315133580664912; b3.sp=-10.0582402432695}
  else if(SPP=='LD'){b0.sp=0.151020547228814; b1.sp=0.518105809959927; b3.sp=-2.95111484704989}
  else if(SPP=='MA'){b0.sp=-0.018153353182371; b1.sp=0.155653255018945; b3.sp=-11.9908675554091}
  else if(SPP=='MM'){b0.sp=-0.0265332913512398; b1.sp=0.121550356703452; b3.sp=33.9624348473053}
  else if(SPP=='NM'){b0.sp=0.0660698688832786; b1.sp=0.185189239318732; b3.sp=-6.44317574120285}
  else if(SPP=='NS'){b0.sp=0.58765147428241; b1.sp=0.766068463693553; b3.sp=-15.4055499203027}
  else if(SPP=='PB'){b0.sp=-0.0357555924895321; b1.sp=-0.0803912110328488; b3.sp=-1.6664777029937}
  else if(SPP=='PP'){b0.sp=-0.0373144797077465; b1.sp=-0.350049097337305; b3.sp=-4.21041059617322}
  else if(SPP=='PR'){b0.sp=-0.0346701336372091; b1.sp=-0.10912826421038; b3.sp=-2.01542362791443}
  else if(SPP=='QA'){b0.sp=-0.0311387037811782; b1.sp=-0.0998723553810821; b3.sp=1.41747311042227}
  else if(SPP=='RM'){b0.sp=-0.0312020217524753; b1.sp=0.00720903134134884; b3.sp=0.101816615489034}
  else if(SPP=='RN'){b0.sp=-0.0094347931012042; b1.sp=-0.0396817282548712; b3.sp=-10.1183218991636}
  else if(SPP=='RO'){b0.sp=-0.0141447389984494; b1.sp=0.152148606230763; b3.sp=-1.17046078486604}
  else if(SPP=='RS'){b0.sp=-0.0301196550138488; b1.sp=0.0152297971351949; b3.sp=0.38931544768626}
  else if(SPP=='SB'){b0.sp=-0.0377098791144756; b1.sp=-0.114120076436571; b3.sp=5.65813496069566}
  else if(SPP=='SC'){b0.sp=-0.0199477693630853; b1.sp=0.0891064466739558; b3.sp=12.2330640504621}
  else if(SPP=='SE'){b0.sp=-0.038061047232242; b1.sp=-0.0315005432260497; b3.sp=21.7211732839993}
  else if(SPP=='SM'){b0.sp=-0.0267410758112335; b1.sp=0.0726112628173826; b3.sp=-0.602423638822342}
  else if(SPP=='ST'){b0.sp=-0.0242569297411646; b1.sp=0.0396034490624144; b3.sp=-27.2777484752897}
  else if(SPP=='SV'){b0.sp=-0.0385773040278896; b1.sp=-0.433046128822715; b3.sp=2.33236188029167}
  else if(SPP=='TA'){b0.sp=-0.00915581932849924; b1.sp=0.286151134082257; b3.sp=-3.57106100780012}
  else if(SPP=='WA'){b0.sp=-0.0309530979243472; b1.sp=-0.0403566782836099; b3.sp=1.32035673823902}
  else if(SPP=='WI'){b0.sp=-0.0338956617806498; b1.sp=-0.0295693742206591; b3.sp=-1.6320012371658}
  else if(SPP=='WO'){b0.sp=-0.0388162527590804; b1.sp=-0.254244901384475; b3.sp=7.83872933989312}
  else if(SPP=='WP'){b0.sp=-0.0158814221367455; b1.sp=0.107642128140552; b3.sp=0.0495265132219116}
  else if(SPP=='WS'){b0.sp=-0.0226927810269686; b1.sp=0.072532663216557; b3.sp=-2.89254036760573}
  else if(SPP=='YB'){b0.sp=-0.0324235742130594; b1.sp=-0.0800526792453781; b3.sp=-0.788476908030292}
  else if(SPP=='AL'){b0.sp=-0.037877520095339; b1.sp=-0.0284909664165172; b3.sp=1.41806369173224}
  else if(SPP=='BE'){b0.sp=0.00580751714164938; b1.sp=-0.0542581267639763; b3.sp=-0.100295148320291}
  else if(SPP=='EC'){b0.sp=0.205274222529117; b1.sp=0.217179977143035; b3.sp=-12.5449424235925}
  else if(SPP=='EL'){b0.sp=-0.0399169173458411; b1.sp=-0.072162484493935; b3.sp=1.52617139302259}
  else if(SPP=='EO'){b0.sp=-0.0315612449379492; b1.sp=-0.0471643139545318; b3.sp=1.04781938732441}
  else if(SPP=='SH'){b0.sp=-0.0358714607608239; b1.sp=-0.0529795666761078; b3.sp=1.29375459356858}
  else if(SPP=='SW'){b0.sp=-0.0339616970717493; b1.sp=-0.0581425807898783; b3.sp=0.344946824221794}
  else if(SPP=='OH'){b0.sp=-0.0328689129083733; b1.sp=-0.172713736309933; b3.sp=0.868819474238827}
  else if(SPP=='OK'){b0.sp=-0.0280501839217296; b1.sp=-0.197650906495178; b3.sp=-3.27034720730148}
  else if(SPP=='XX'){b0.sp=-0.0315633927941656; b1.sp=-0.0359255416466161; b3.sp=1.35548365118208}
  else if(SPP=='BH'){b0.sp=-0.0289434160385741; b1.sp=-0.00833226438922751; b3.sp=2.08508775264873}
  else{b0.sp=0.0; b1.sp=0.0; b3.sp=0.0}
  CSAgrow=(b0+b0.sp)*(CSA^((b1+b1.sp)+b2*SI))*exp(((b3+b3.sp)+b4*BalSW+b5*BalHW)*CSA)
  return(CSAgrow=CSAgrow)
}

#Diameter increment function (1/27/16)
dDBH.FUN=function(SPP,DBH,CR,BAL.SW,BAL.HW,BA,CSI,tph,topht,RD)
{
  BAL=BAL.SW+BAL.HW+0.001
  RS=sqrt(10000/tph)/topht
  BAperc=1-((BAL+0.001)/BA)
  BALmod=(1-BAperc)/RS
  pBAL.SW=BAL.SW/(BAL+0.1)
  b0= -1.6331538 #0.06448937 900799 -25.32439  0.0000
  b1=  0.0070441 #0.00253446 900799   2.77935  0.0054
  b2= -0.0002784 #0.00000725 900799 -38.38667  0.0000
  b3=  0.1257197 #0.00524286 900799  23.97921  0.0000
  b4= -0.2705499 #0.02278613 900799 -11.87344  0.0000
  b5=  0.2941056 #0.00440865 900799  66.71111  0.0000
  b6= -0.0616965 #0.01147665 900799  -5.37583  0.0000
  b7= -0.1465285 #0.00254133 900799 -57.65824  0.0000
  spConst = matrix(c(
  #     b0.spp        b1.spp          b4.spp        b6.spp
      -0.034977457, 0.007276127   , 0.117697955 , -0.0382661,      # AB
      0.074435665 , 0.005121491   , 0.004234682 , -0.003699426,    # AE
      -0.552782012, 0.008947672   , 0.092785668 , -0.00685572,     # AH
      -0.784491091, 0.0004018074  , 0.020919252 , 0.034001417,     # AL
      -0.29246248 , -0.006769558  , -0.015389101, 0.077813582,     # AP
      -0.368193403, -0.005871838  , -0.027100093, 0.092172968,     # BA
      0.388327436 , -0.0235497    , -0.267714578, 0.117466223,     # BC
      0.124494862 , 0.00123841    , 0.021706955 , -0.012162306,    # BF
      0.117176372 , 0.002545407   , 0.013811927 , -0.017087026,    # BL
      0.010205759 , -0.002034886  , -0.019239214, 0.01551137,      # BN
      -0.010250719, 0.01297885    , 0.07809603  , -0.023175186,    # BO
      0.586348887 , -0.009991966  , -0.114795789, -0.023104677,    # BP
      -0.032226829, -0.01442781   , -0.021477635, -0.001507636,    # BS
      0.107871306 , -0.0004487118 , -0.049918635, 0.049907667,     # BT
      -0.495538825, 0.005412451   , 0.074999908 , 0.072129181,     # BW
      -0.583232513, 0.01992249    , 0.167838499 , -0.047543001,    # CC
      0.120658466 , 0.004819623   , 0.028503873 , -0.014215554,    # EC
      -0.073679869, 0.01541532    , 0.176123957 , -0.070655479,    # EH
      -0.094347968, 0.008437199   , 0.065152022 , -0.04727368,     # GA
      0.108941251 , -0.03491995   , -0.226167418, 0.095737848,     # GB
      -0.015136196, -0.01588083   , -0.060643885, -0.010730512,    # HH
      -0.219875954, -0.0005893354 , -0.004531752, 0.01724835,      # HT
      0.332589343 , -0.01144129   , -0.207567868, -0.039412593,    # JP
      0.253835715 , 0.01079078    , 0.079762745 , -0.029911935,    # LD
      0.170639841 , 0.002539446   , 0.014799827 , -0.041455623,    # MA
      -0.316673999, -0.006395035  , -0.030971447, 0.020471357,     # MM
      0.538786115 , 0.005732204   , 0.136374018 , -0.109911823,    # NS
      0.182086574 , 0.004291105   , 0.031149091 , -0.038283159,    # OH
      0.394296696 , 0.01192989    , 0.083083665 , -0.065052689,    # OK
      0.147672195 , -0.01733291   , -0.121852844, 0.002444024,     # PB
      0.280883206 , -0.004079587  , -0.036381255, -0.013142199,    # PP
      -0.212449655, 0.01248223    , 0.104456335 , -0.075016334,    # PR
      0.007482821 , 0.002451922   , -0.065695862, 0.048381867,     # QA
      -0.053329455, -0.0000425553 , -0.018221214, 0.02432616,      # RM
      0.593594914 , -0.01538272   , -0.12775315 , 0.072963004,     # RN
      -0.658621415, 0.01809487    , 0.166063488 , 0.051722322,     # RO
      0.040808688 , 0.004242409   , -0.004858248, -0.008108392,    # RS
      -0.380053588, 0.008042379   , 0.077679112 , -0.019651545,    # SB
      1.031379405 , -0.03062604   , -0.274198164, 0.02751689,      # SC
      -0.457689963, 0.006625638   , 0.051058869 , -0.033442386,    # SE
      -0.056357337, -0.002619051  , -0.019521142, 0.012376529,     # SH
      0.12397129  , 0.006428658   , 0.0137845   , -0.047456949,    # SM
      0.030222397 , 0.001147229   , 0.002783061 , 0.003902373,     # ST
      0.151457796 , 0.01306281    , 0.072299347 , -0.137235705,    # SV
      -0.45496371 , 0.01386419    , 0.043701246 , 0.050849231,     # TA
      -0.097174269, 0.008959311   , 0.086349472 , -0.010883404,    # WA
      -0.414076962, 0.01074121    , 0.145425792 , -0.001819793,    # WC
      0.202666239 , -0.04083091   , -0.327089624, 0.207172533,     # WI
      0.024007926 , -0.003156131  , -0.031406355, -0.020914639,    # WO
      0.133589236 , 0.008688826   , 0.064063728 , -0.011417414,    # WP
      0.286219598 , -0.00003104828, -0.049962785, -0.027217847,    # WS
      0.09393567  , 0.003789904   , 0.087753032 , -0.047504164,    # YB
      0.0         , 0.0           , 0.0         , 0.0),            # 99
      ncol=4,byrow=TRUE)
  SPcodes=c('AB','AE','AH','AL','AP','BA','BC','BF','BL','BN','BO','BP','BS',
            'BT','BW','CC','EC','EH','GA','GB','HH','HT','JP','LD','MA','MM',
            'NS','OH','OK','PB','PP','PR','QA','RM','RN','RO','RS','SB','SC',
            'SE','SH','SM','ST','SV','TA','WA','WC','WI','WO','WP','WS','YB',
            '99')
  sprow = match(SPP,SPcodes)
  sprow[is.na(sprow)] = nrow(spConst)
  B0 = b0+spConst[sprow,1]
  B1 = b1+spConst[sprow,2]
  B4 = b4+spConst[sprow,3]
  B6 = b6+spConst[sprow,4]
  
  dDBH=exp(B0+(B1*DBH) +(b2*DBH^2)+(b3*log(CR))+(B4*log(BALmod+0.1))+
          (b5*log(CSI))+(B6*sqrt(BA*RD+1))+b7*sqrt(pBAL.SW+0.0001)) 
  return(dDBH=dDBH)
}
  
  
## Diameter modifier function (3/15/16 based on results by Christian Kuehne)
dDBH.thin.mod = function(SPP, PERCBArm, BApre, QMDratio, YEAR_CT, YEAR){
  TST = YEAR - YEAR_CT # time since thinning
  # balsam fir 
  if(SPP=="BF"){
    y0 = -0.2566
    y1 = -22.7609
    y2 =  0.7745
    y3 =  1.0511
  }
  # red spruce 
  else if(SPP=="RS"){
    y0= -0.5010; 
    y1=-20.1147; 
    y2=0.8067; 
    y3=1.1905}
  
  SP=ifelse(SPP=='BF' | SPP=='RS',1,0)
  dDBH.mod = ifelse(!is.na(PERCBArm) & !is.na(QMDratio) & !is.na(BApre) & !is.na(YEAR_CT) & YEAR_CT <= YEAR & SP==1,
                    1+(exp(y0+(y1/((100*PERCBArm*QMDratio)+0.01)))*y2^TST*TST^y3),1.0)
  return(dDBH.mod = round(dDBH.mod,5))
}        

#Diameter increment modifier for SBW (Cen et al. 2016)
dDBH.SBW.mod=function(Region,SPP,DBH,BAL.SW,BAL.HW,CR,avgDBH.SW,topht,CDEF=NA){
  BF=ifelse(SPP=='BF',1,0)
  BS.RS=ifelse(SPP=='BS'|SPP=='RS',1,0)
  WS=ifelse(SPP=='WS',1,0)
  CDEF2=ifelse(is.na(CDEF),0,CDEF)
  if(Region=='ME'){
    b1.BF=0.1187
    b1.BS.RS=0.0675
    b1.WS=0.0321
    b2=0.0019
    b3=-0.0327
    b4=-0.0412
    b5=0.3950
    b6.BF=-1.2813
    b6.BS.RS=-0.9477
    b6.WS=-0.3715
    b7.BF=-0.0016
    b7.BS.RS=-0.0006
    b7.WS=-0.0183 }              
  else if(Region=='NB'){
    b1.BF=0.0701
    b1.BS.RS=0.0320
    b1.WS=0.0487
    b2=-0.0190
    b3=-0.0277
    b4=-0.0027
    b5=0.0  
    b6.BF=-0.8200
    b6.BS.RS= -0.6861
    b6.WS=-0.7839
    b7.BF= -0.0018
    b7.BS.RS=-0.0012
    b7.WS=-0.0006}
  dDBHa=(b1.BF*BF+b1.BS.RS*BS.RS+b1.WS*WS)*DBH*exp(b2*BAL.HW+b3*BAL.SW+b4*topht+b5*CR+
                ((b6.BF*BF+b6.BS.RS*BS.RS+b6.WS*WS)*(DBH/avgDBH.SW))+(b7.BF*BF+b7.BS.RS*BS.RS+b7.WS*WS)*0)
  dDBHb=(b1.BF*BF+b1.BS.RS*BS.RS+b1.WS*WS)*DBH*exp(b2*BAL.HW+b3*BAL.SW+b4*topht+b5*CR+
                ((b6.BF*BF+b6.BS.RS*BS.RS+b6.WS*WS)*(DBH/avgDBH.SW))+(b7.BF*BF+b7.BS.RS*BS.RS+b7.WS*WS)*CDEF2)
  dDBH.mod=ifelse(is.na(CDEF) | SPP!='BF' & SPP!='RS' & SPP!='BS' & SPP!='WS',1.0,dDBHb/dDBHa)
  return(dDBH.mod)
}


#Height increment (10/8/14) (Russell et al. 2014 EJFR)
Htincr=function(SPP,HT,CR,BAL.SW,BAL.HW,BAPH,CSI)
{
  b0=-3.193
  b1=-0.1302
  b2=-0.001317
  b3=-0.004997
  b4=-0.006320
  b5=0.2652
  b6=1.106
  b7=0.1237
  if(SPP=='AB'){b0.sp=-0.4436; b2.sp=0.0009; b3.sp=0.0068; b4.sp=0.006}
  else if(SPP=='AE'){b0.sp=-0.0078; b2.sp=0.0002; b3.sp=-0.0003; b4.sp=-0.0005}
  else if(SPP=='AL'){b0.sp=-0.1413; b2.sp=0.0005; b3.sp=-0.0006; b4.sp=0.0005}
  else if(SPP=='AP'){b0.sp=-0.2342; b2.sp=0.0007; b3.sp=0.0031; b4.sp=-0.0035}
  else if(SPP=='BA'){b0.sp=-0.0039; b2.sp=0; b3.sp=0; b4.sp=-0.0001}
  else if(SPP=='BC'){b0.sp=-0.2204; b2.sp=0.0013; b3.sp=0.0018; b4.sp=-0.0015}
  else if(SPP=='BF'){b0.sp=0.0646; b2.sp=0.0002; b3.sp=-0.0045; b4.sp=-0.0209}
  else if(SPP=='BS'){b0.sp=-0.2002; b2.sp=-0.001; b3.sp=-0.0055; b4.sp=0.0107}
  else if(SPP=='BT'){b0.sp=-0.0564; b2.sp=0.0011; b3.sp=-0.0067; b4.sp=0.0061}
  else if(SPP=='CC'){b0.sp=0.1895; b2.sp=0; b3.sp=-0.0014; b4.sp=-0.0002}
  else if(SPP=='EH'){b0.sp=-0.3036; b2.sp=0.0006; b3.sp=-0.0001; b4.sp=0.0037}
  else if(SPP=='GB'){b0.sp=0.3512; b2.sp=-0.0024; b3.sp=-0.003; b4.sp=-0.0022}
  else if(SPP=='HH'){b0.sp=-0.2508; b2.sp=0.0011; b3.sp=-0.0078; b4.sp=-0.0022}
  else if(SPP=='HT'){b0.sp=-0.0952; b2.sp=0.0004; b3.sp=-0.0014; b4.sp=-0.0002}
  else if(SPP=='JP'){b0.sp=0.2031; b2.sp=-0.0004; b3.sp=-0.0045; b4.sp=-0.0026}
  else if(SPP=='MA'){b0.sp=-0.3744; b2.sp=0.0012; b3.sp=0.0038; b4.sp=0.0067}
  else if(SPP=='MM'){b0.sp=0.1544; b2.sp=-0.0008; b3.sp=0.0017; b4.sp=-0.0018}
  else if(SPP=='NC'){b0.sp=-0.1074; b2.sp=-0.0002; b3.sp=-0.0037; b4.sp=0.0008}
  else if(SPP=='NS'){b0.sp=0.2483; b2.sp=0.0004; b3.sp=-0.0053; b4.sp=-0.005}
  else if(SPP=='OH'){b0.sp=0.4457; b2.sp=-0.0007; b3.sp=-0.0051; b4.sp=-0.0053}
  else if(SPP=='OK'){b0.sp=0.0649; b2.sp=-0.0001; b3.sp=-0.0003; b4.sp=-0.0005}
  else if(SPP=='PB'){b0.sp=-0.0588; b2.sp=-0.0005; b3.sp=0.0046; b4.sp=-0.0007}
  else if(SPP=='PR'){b0.sp=-0.146; b2.sp=0.0013; b3.sp=0.0091; b4.sp=0.0096}
  else if(SPP=='QA'){b0.sp=0.9602; b2.sp=-0.0018; b3.sp=-0.0157; b4.sp=-0.0014}
  else if(SPP=='RM'){b0.sp=0.138; b2.sp=-0.0011; b3.sp=0.0062; b4.sp=-0.0011}
  else if(SPP=='RN'){b0.sp=0.4573; b2.sp=-0.0039; b3.sp=0.0071; b4.sp=0.0023}
  else if(SPP=='RO'){b0.sp=0.0675; b2.sp=-0.0003; b3.sp=0.0027; b4.sp=-0.0009}
  else if(SPP=='RS'){b0.sp=-0.0852; b2.sp=0.0001; b3.sp=-0.0032; b4.sp=-0.0028}
  else if(SPP=='SE'){b0.sp=0.2746; b2.sp=-0.0007; b3.sp=-0.004; b4.sp=-0.0003}
  else if(SPP=='SM'){b0.sp=-0.1517; b2.sp=0.0006; b3.sp=-0.0026; b4.sp=-0.0018}
  else if(SPP=='ST'){b0.sp=0.0485; b2.sp=-0.0003; b3.sp=0.0061; b4.sp=0.0068}
  else if(SPP=='TA'){b0.sp=-0.2393; b2.sp=0.0015; b3.sp=0.009; b4.sp=0.0076}
  else if(SPP=='WI'){b0.sp=-0.1028; b2.sp=0.0003; b3.sp=0.0004; b4.sp=0.0009}
  else if(SPP=='WP'){b0.sp=0.1903; b2.sp=0.0004; b3.sp=-0.0016; b4.sp=-0.0118}
  else if(SPP=='WS'){b0.sp=0.0347; b2.sp=0.0004; b3.sp=-0.0029; b4.sp=-0.0082}
  else if(SPP=='YB'){b0.sp=-0.0766; b2.sp=-0.0006; b3.sp=0.0032; b4.sp=0.0054}
  else{b0.sp=0.0; b2.sp=0.0; b3.sp=0.0; b4.sp=0.0}
  dHT=exp((b0+b0.sp)+b1*log(HT+1)+(b2+b2.sp)*HT^2+(b3+b3.sp)*BAL.SW+(b4+b4.sp)*BAL.HW+
            b5*log(CR+0.01)+b6*log(CSI)+b7*sqrt(BAPH))
  return(dHT=dHT)
}

#Height increment (2/12/16) 
dHT=function(SPP,HT,CR,BAL.SW,BAL.HW,BA,CSI,tph,topht,RD)
{
  BAL=BAL.SW+BAL.HW
  RS=sqrt(10000/tph)/topht
  BAperc=1-((BAL+0.001)/BA)
  BALmod=(1-BAperc)/RS
  pBAL.SW=BAL.SW/(BAL.SW+BAL.HW+0.1)
  b0= -3.925083  # 0.12585923 88851 -31.18629  0.0000
  b1= -0.061691  # 0.00863269 88851  -7.14623  0.0000
  b2=  0.255589  # 0.02770593 88851   9.22507  0.0000
  b3=  0.199307  # 0.00714525 88851  27.89361  0.0000
  b4= -0.091328  # 0.02934160 88851  -3.11258  0.0019
  b5=  1.025877  # 0.01692808 88851  60.60211  0.0000
  b6=  0.115358  # 0.03248193 88851   3.55145  0.0004
  b7=  0.098963  # 0.06078804 88851   1.62801  0.1035
  b8=  0.003199  # 0.00059196 88851   5.40484  0.0000 
  spConst = matrix(c(
  #   b0.spp        b1.spp          b4.spp        b6.spp        b7.spp
    -0.415979307, 0.0274010197 , 0.196582208 , -0.164287184, 0.17710536 ,  #AB
    -0.097680261, 0.010285478  , -0.034514623, 0.013798257 , 0.05393479 ,  #AE
    -0.518622556, 0.0336615657 , 0.038332872 , -0.073282395, 0.14078092 ,  #AL
    -0.352394417, 0.0189693013 , 0.017053302 , -0.033141501, 0.12245193 ,  #AP
    -0.008991184, -0.0002972318, 0.002365564 , -0.005565805, 0.00451468 ,  #BA
    -0.6947698  , 0.0593437011 , 0.052465134 , -0.154325374, 0.37385923 ,  #BC
    -0.252382258, 0.0100278394 , -0.115915182, -0.019904875, 0.35826384 ,  #BF
    0.09727877  , -0.0270708651, 0.034082833 , 0.023416841 , -0.27796473,  #BS
    0.097631862 , -0.0019892345, -0.033205181, 0.178102299 , -0.42712909,  #BT
    0.100493172 , 0.0024756469 , -0.015890142, 0.014850572 , -0.01463509,  #CC
    -0.338641101, 0.0149152575 , 0.042828473 , -0.023914376, -0.02984288,  #EH
    0.664242356 , -0.0507004025, 0.008938324 , -0.04425826 , -0.04842967,  #GB
    -0.275493334, 0.0114373559 , 0.017621827 , -0.029794703, -0.05039664,  #HH
    -0.294495258, 0.0213413046 , 0.024225138 , -0.058721102, 0.10119773 ,  #HT
    -0.143833414, 0.0264402706 , 0.091549503 , -0.233275814, 0.24538541 ,  #JP
    -0.362473516, 0.0149305507 , -0.014660213, 0.045285396 , 0.04807392 ,  #MA
    0.178128436 , -0.0134191286, 0.006131074 , -0.01282847 , 0.01233981 ,  #MM
    -0.2152392  , 0.0131073208 , 0.044087868 , -0.086432379, 0.09571151 ,  #NC
    0.238378153 , -0.0080475871, -0.26663534 , 0.27525792  , -0.12455711,  #NS
    0.190850506 , 0.008856123  , 0.096511731 , -0.179549618, 0.11449758 ,  #OH
    0.312876712 , -0.0023761163, -0.032469484, 0.002288521 , 0.01575391 ,  #OK
    -0.130932583, -0.0078414315, 0.111669147 , -0.044044815, 0.11333075 ,  #PB
    -0.327481197, 0.0225428886 , -0.031332766, 0.152794717 , -0.0259612 ,  #PR
    1.417306413 , -0.0403686248, 0.006821086 , -0.075758317, -0.37292927,  #QA
    0.12732005  , -0.0245368295, 0.053246849 , 0.00579006  , 0.14672003 ,  #RM
    1.443710317 , -0.1417363545, -0.027265101, 0.309824702 , -0.8134972 ,  #RN
    0.080065633 , -0.0071379908, -0.023614962, 0.010061635 , 0.02383219 ,  #RO
    0           , 0            , 0           , 0           , 0          ,  #RP
    -0.048412618, -0.0079040397, -0.145815645, 0.060567801 , 0.0797388  ,  #RS
    -0.203824654, 0.0094728516 , 0.062125666 , -0.050945155, 0.01973029 ,  #SC
    0.63707386  , -0.0313557553, -0.013765425, 0.02075583  , -0.25319517,  #SE
    0.088620046 , 0.0258811205 , 0.073470611 , -0.218583608, -0.14200099,  #SM
    0.0692123   , -0.0072937033, -0.103051494, 0.091091102 , 0.12312381 ,  #ST
    -0.351090474, 0.0079995727 , -0.019048797, 0.218764622 , -0.13129184,  #TA
    -0.556856239, 0.0312274808 , 0.09900893  , 0.036092242 , -0.01154009,  #WA
    -0.205319428, 0.0109161657 , 0.033229394 , -0.038651961, 0.03410209 ,  #WI
    0.004458537 , 0.0069854589 , -0.079402408, 0.031357732 , 0.16043057 ,  #WP
    -0.004902983, 0.001696414  , -0.204246802, 0.058944828 , 0.15403836 ,  #WS
    0.052168661 , -0.0178393926, 0.048486031 , -0.001779362, 0.00445344 ,  #YB
    0           ,         0    , 0           ,            0,          0),  #99
      ncol=5,byrow=TRUE) 
  SPcodes=c('AB','AE','AL','AP','BA','BC','BF','BS','BT','CC','EH','GB','HH',
            'HT','JP','MA','MM','NC','NS','OH','OK','PB','PR','QA','RM','RN',
            'RO','RP','RS','SC','SE','SM','ST','TA','WA','WI','WP','WS','YB',
            '99')     
  sprow = match(SPP,SPcodes)
  sprow[is.na(sprow)] = length(SPcodes)
  B0 = b0+spConst[sprow,1]
  B1 = b1+spConst[sprow,2]
  B4 = b4+spConst[sprow,3]
  B6 = b6+spConst[sprow,4]
  B7 = b7+spConst[sprow,5]  
  
  dHT=exp(B0+(B1*HT)+(b2*log(HT))+(b3*log(CR))+(B4*log(BALmod+1))+
    (b5*log(CSI))+(B6*log(BA*RD+1))+(B7*sqrt(pBAL.SW))+(b8*BA*RD)) 
  return(dHT=dHT)
}

#thinning height modifier (Kuehne et al. 2016)
dHT.thin.mod = function(SPP, PERCBArm, BApre, QMDratio, YEAR_CT, YEAR){
  TST = YEAR - YEAR_CT # time since thinning
  # balsam fir 
  if(SPP=="BF"){
    y0 = -1.8443
    y1 = 5.2969
    y2 =  1.0532
    y3 =  0.000001
    dHT.mod=1-(exp(y0+y1/((100*PERCBArm)+0.01))*y2^TST*TST^y3)
  }
  # red spruce 
  else if(SPP=="RS"){
    y0 = -1.8426
    y1 = 6.2781
    y2 = 1.1596
    y3 = 0.000001
    dHT.mod=1-(exp(y0+y1/((100*PERCBArm)+0.01))*y2^TST*TST^y3)}
  SP=ifelse(SPP=='BF' | SPP=='RS',1,0)
  dHT.mod = ifelse(!is.na(YEAR_CT) & YEAR_CT <= YEAR & SP==1,max(0.9,min(dHT.mod,1.15)),1)
  return(dHT.mod = dHT.mod)
}


#SBW Height modifier (Cen et al. 2016)
dHT.SBW.mod=function(SPP,DBH,topht,CR,avg.DBH.SW,CDEF=NA){
  BF=ifelse(SPP=='BF',1,0)
  BS.RS=ifelse(SPP=='BS'|SPP=='RS',1,0)
  WS=ifelse(SPP=='WS',1,0)
  CDEF2=ifelse(is.na(CDEF),0,CDEF)
  b1.BF=0.0013
  b1.BS.RS=0.0009
  b1.WS=0.0005
  b2=-0.0011
  b3=0.0316
  b4=2.4512
  b5.BF=0.3676
  b5.BS.RS=0.2881
  b5.WS=0.6800
  b6.BF=-0.0017
  b6.BS.RS=-0.0014
  b6.WS=0.0001
  dHTa=(b1.BF*BF+b1.BS.RS*BS.RS+b1.WS*WS)*DBH*exp(b2*DBH^2+b3*topht+b4*CR+(b5.BF*BF+b5.BS.RS*BS.RS+b5.WS*WS)*(DBH/avg.DBH.SW)
                                                  +(b6.BF*BF+b6.BS.RS*BS.RS+b6.WS*WS)*0)
  dHTb=(b1.BF*BF+b1.BS.RS*BS.RS+b1.WS*WS)*DBH*exp(b2*DBH^2+b3*topht+b4*CR+(b5.BF*BF+b5.BS.RS*BS.RS+b5.WS*WS)*(DBH/avg.DBH.SW)
                                                  +(b6.BF*BF+b6.BS.RS*BS.RS+b6.WS*WS)*CDEF2)
  dHT.mod=ifelse(is.na(CDEF) | SPP!='BF' & SPP!='RS' & SPP!='BS' & SPP!='WS',1.0,dHTb/dHTa)
  return(dHT.mod)
  }

#dHT.SBW.mod('WS',20,15,0.5,15)

#dynamic crown recession equation (Russell et al. 2014; EJFR)
dHCB=function(dHT,DBH,HT,HCB,CCF,shade)
{
  CL=HT-HCB
  CR=CL/HT
  b1=4.000
  b2=-0.8395
  b3=-0.2196
  b4=-0.3059
  b5=-0.00553
  b6=0.09821  
  dHCB=(CL+dHT)/(1+exp(b1+b2*log(CR+0.01)+b3*log(CCF+1)+ b4*log(1.01-CR)
                       +b5*(shade^2)+b6*log(shade*CR)))
  return(dHCB=dHCB)
}


#crown recession thinning modifier
dHCB.thin.mod = function(SPP, PERCBArm, BApre, QMDratio, YEAR_CT, YEAR){
  TST = YEAR - YEAR_CT # time since thinning
  # balsam fir 
  if(SPP=="BF"){
    y0 = -0.4208
    y1 = -17.0998
    y2 =  0.7986
    y3 =  0.0521
    dHCB.mod=1-(exp(y0+y1/((100*PERCBArm*QMDratio)+0.01))*y2^TST*TST^y3)
  }
  # red spruce 
  else if(SPP=="RS"){
    y0 = -1.0778
    y1 = -14.7694
    y2 = 0.7758
    y3 = 1.1164
    dHCB.mod=1-(exp(y0+y1/((100*PERCBArm*QMDratio)+0.01))*y2^TST*TST^y3)}
  SP=ifelse(SPP=='BF' | SPP=='RS',1,0)
  dHCB.mod = ifelse((!is.na(YEAR_CT) & YEAR_CT <= YEAR) & SP==1,min(abs(dHCB.mod),1),1)
  return(dHCB.mod = dHCB.mod)
}

#Mortality using the approach of Kershaw
stand.mort.prob=function(region,BA,BAG,QMD,pBA.BF,pBA.IH)
{
  BA.BF=pBA.BF*BA
  BA.IH=pBA.IH*BA
  if(region=='ME'){b0=0.6906978; b1=0.149228; b2=-0.001855535; b3=-2.557345; b4=-0.05507579; b5=0.06414701; b6=0.0432701; cut=0.871958}
  else if(region=='NB'){b0=0.699147; b1=0.1250758; b2=-0.001855535; b3=-2.557345; b4=-0.05507579; b5=0.06414701; b6=0.0432701; cut=0.7268086}
  else if(region=='NS'){b0=0.2756542; b1=0.1499495; b2=-0.001855535; b3=-2.557345; b4=-0.05507579; b5=0.06414701; b6=0.0432701; cut=0.9148455}
  else if(region=='PQ'){b0=1.0472726; b1=0.161746; b2=-0.001855535; b3=-2.557345; b4=-0.05507579; b5=0.06414701; b6=0.0432701; cut=0.7351621}
  k=b0+b1*BA+b2*BA^2+b3*BAG+b4*QMD+b5*BA.BF+b6*BA.IH
  prob=exp(k)/(1+exp(k))
  return(list(prob=prob,cut=cut))
}

stand.mort.BA=function(region,BA,BAG,QMD,QMD.BF,pBA.bf,pBA.ih){
  BA.BF=pBA.bf*BA
  BA.IH=pBA.ih*BA
  if(region=='ME'){b0=0.1857844; b1=0.2315199; b2=0.02020253; b3=0.5674303; b4=-2.037042; b5=0.06815229;
                   b6=0.3345308; b7=0.09950853}
  else if(region=='NB'){b0=0.5987741; b1=0.2315199; b2=0.02020253; b3=0.1888859; b4=-2.037042; b5=0.14607033;
                        b6=0.3284819; b7=0.09950853}
  else if(region=='NS'){b0=0.1302331; b1=0.2315199; b2=0.02020253; b3=0.589446; b4=-2.037042; b5=0.0867806; 
                        b6=0.2243597; b7=0.09950853}
  else if(region=='PQ'){b0=0.1068258; b1=0.2315199; b2=0.02020253; b3=0.6810417; b4=-2.037042; b5=0.01171661; 
                        b6=0.494071; b7=0.09950853}
  BA.mort=(b0+b1*pBA.bf+b2*pBA.ih)*BA^(b3+b4*BAG)
  BF.mort=ifelse(QMD==0,0,b5*BA.BF^b6+b7*(QMD.BF/QMD))
  mort.tot=BA.mort+BF.mort
  return(mort.tot=mort.tot)
}

#SBW mortality modifier
SBW.smort.mod=function(region,BA,BA.BF,topht,CDEF){
  if(region=='ME'){
    b1=-2.6380
    b2=0.0114
    b3=-0.0076
    b4=0.0074}
  else if(region=='NB'){
    b1=-3.0893
    b2=0.0071
    b3=-0.0037
    b4=0.0}
  VOL=(topht/2)*BA
  pBF=BA.BF/BA
  aa=(1/(1+exp(-b1)))*(1/(1+exp(-(b2*0*BA.BF+b3*VOL+b4*0))))
  bb=(1/(1+exp(-b1)))*(1/(1+exp(-(b2*CDEF*BA.BF+b3*VOL+b4*CDEF))))
  rat=ifelse(is.na(CDEF),1,bb/aa)
  return(rat=rat)
}
                               
#Thinning mortality modifier
BAmort.stand=function(BA,PCT,YEAR_CT,YEAR, PERCBArm, BApre, QMDratio){
  TST=YEAR-YEAR_CT
  b30=-1.2402
  b31=-24.5202
  b32=-1.1302
  b33=1.5884
  y30=8.3385
  y31=-601.3096
  y32=0.5507
  y33=1.5798
  #BAmort=exp(b30+(b31/BA)+b32*PCT+b33*pBA.BF)
  mod=ifelse(!is.na(YEAR_CT) & YEAR_CT <= YEAR,1.0+exp(y30+(y31/((100*(PERCBArm)+BApre)+0.01)))*y32^TST*TST^y33,1.0)
  #BAmort=BAmort*mod
  return(mod=mod)
}


tree.mort.prob=function(SPP,DBH)
{
  SPcodes = c('AB','AE','AH','AP','BA','BC','BE','BF','BL','BN','BO','BP','BS',
              'BT','BW','CC','EC','EH','GA','GB','HH','JP','NM','NS','OH','PB',
              'PP','PR','QA','RM','RO','RP','RS','SB','SH','SM','SP','ST','SV',
              'TA','WA','WC','WL','WO','WP','WS','YB','99')
  spConst = matrix(c(
    # b0             b1              b2        Djump Scale Shape=4.5 
    2.152681379  , -0.0269825907,  0.0002203177 , 41  , 20  , 4.5 ,  # AB
    2.948346662  , -0.1017558326,  0.0018279137 , 22.5, 40  , 2   ,  # AE
    4.552236589  , -0.4626664119,  0.0125996045 , 12  , 15  , 4.5 ,  # AH
    5.6430024532 , -0.4644532732,  0.0132654057 , 16  , 10.6, 4.1 ,  # AP
    1.7183600838 , 0.0393047451 ,  -0.0023514773, 39  , 37  , 4.25,  # BA
    4.8627898851 , -0.3695674404,  0.0109822297 , 9.2 , 31  , 3.63,  # BC
    4.552236589  , -0.2313332059,  0.0125996045 , 14  , 20  , 4.5 ,  # BE
    2.5743949775 , -0.0851930923,  0.0015971909 , 53  , 40.6, 1.51,  # BF
    -3.5183135273, 0.5008393656 ,  -0.0114432915, 10  , 10  , 2   ,  # BL
    9.6140856026 , -0.8619281584,  0.0215901194 , 15  , 27.5, 1.5 ,  # BN
    2.7402431243 , -0.0403087   ,  0.0014846314 , 40  , 40  , 2   ,  # BO
    1.8795415329 , -0.3915484285,  0.0298003249 , 33  , 33.6, 4.75,  # BP
    1.9568828063 , 0.0535388009 ,  -0.0010376306, 34  , 22  , 3.75,  # BS
    2.1791849646 , -0.0125375225,  0.0008529794 , 40  , 30  , 3   ,  # BT
    -1.4145296118, 0.3204863989 ,  -0.0029710752, 15  , 30  , 3   ,  # BW
    4.8627898851 , -0.3695674404,  0.0109822297 , 10  , 20  , 3.63,  # CC
    -0.4584998714, 0.1992627013 ,  -0.0028451758, 40.6, 11  , 4.28,  # EC
    4.5205542708 , -0.0670350692,  0.0012041907 , 50  , 40  , 3.5 ,  # EH
    7.2061395918 , -0.2239701333,  0.0070370484 , 34  , 39.6, 3.41,  # GA
    0.1922677751 , 0.1517490102 ,  -0.0039268819, 16  , 8.2 , 3.83,  # GB
    2.9674489273 , -0.1009595852,  0.0071673636 , 18  , 24  , 2   ,  # HH
    -0.4488149338, 0.1939739736 ,  -0.0019541699, 30  , 20  , 3   ,  # JP
    4.552236589  , -0.2313332059,  0.0125996045 , 24  , 40  , 4.5 ,  # NM
    17.4833923331, -1.809142126 ,  0.0616970369 , 15  , 35  , 2   ,  # NS
    4.552236589  , -0.5204997134,  0.0125996045 , 12  , 10  , 4.5 ,  # OH
    2.5863343441 , -0.0518497247,  0.0021853588 , 26.4, 41.2, 1.88,  # PB
    12.1649655944, -1.0483772747,  0.0233147008 , 25.8, 41.6, 4.41,  # PP
    -1.2171488097, 0.3211464783 ,  -0.0097154365, 10  , 40  , 2   ,  # PR
    -0.4584998714, 0.1992627013 ,  -0.0028451758, 60.6, 11  , 4.28,  # QA
    2.1674971386 , 0.0557266595 ,  -0.0010435394, 60.2, 40.6, 4.38,  # RM
    3.1202275212 , -0.041290776 ,  0.0022978235 , 41  , 40.4, 3.27,  # RO
    1.1361278304 , 0.1436446742 ,  0.0018438454 , 30  , 30  , 3   ,  # RP
    2.0420797297 , 0.0425701678 ,  -0.0004901795, 41  , 32  , 4.8 ,  # RS
    44.2565091524, -2.4248136198,  0.1388397603 , 22  , 35.6, 4.57,  # SB
    4.552236589  , -0.2313332059,  0.0125996045 , 15  , 10  , 4.5 ,  # SH
    2.7069022565 , 0.0086263655 ,  0.0007235392 , 54.4, 42  , 1.33,  # SM
    5            , -0.3         ,  0.01         , 20  , 40  , 3   ,  # SP
    4.5522366258 , -0.4626664068,  0.012599604  , 4.4 , 24.4, 4.51,  # ST
    4.552236589  , -0.2313332059,  0.0125996045 , 24  , 40  , 4.5 ,  # SV
    1.4269435976 , 0.0886275939 ,  -0.0021232407, 30  , 33.6, 4.5 ,  # TA
    1.0042653571 , 0.165359309  ,  -0.0005814562, 21  , 40  , 4   ,  # WA
    3.647647507  , -0.0606735724,  0.0008507857 , 40  , 45  , 5   ,  # WC
    -3.5183135273, 0.5008393656 ,  -0.0114432915, 10  , 10  , 2   ,  # WL
    -4.8640326448, 0.6250645999 ,  -0.0064419714, 18  , 40  , 4   ,  # WO
    3.3383526175 , -0.0294498474,  0.0009561864 , 61.2, 41.2, 2.87,  # WP
    0.5437824528 , 0.1052397713 ,  0.0006332627 , 19  , 54  , 2   ,  # WS
    2.6967072576 , -0.001250889 ,  0.0007521152 , 48  , 60  , 2   ,  # YB
    2.6967072576 , -0.001250889 ,  0.0007521152 , 48  , 60  , 2   ), # 99
      ncol=6,byrow=TRUE)
  sprow = match(SPP,SPcodes)
  sprow[is.na(sprow)] = nrow(spConst)
  # same as: ddd <- b0 + b1 * DBH + b2 * DBH^2
  ddd <- spConst[sprow,1] + spConst[sprow,2] * DBH + spConst[sprow,3] * DBH^2 
  surv <- exp(ddd)/(1 + exp(ddd))
  # same as: IDj <- as.integer(DBH/Djump)
  IDj <- as.integer(DBH/spConst[sprow,4])
             # same as: exp(-Scale * ((IDj * (DBH - Djump))^Shape))
  Wprob <-1.0# exp(-SPcoefs[sprow,5] * ((IDj * (DBH - SPcoefs[sprow,4]))^SPcoefs[sprow,6]))
  tsurv <- surv * Wprob
  return(tsurv=tsurv)
}


#tree probability of mortality modifier for SBW (Cen et al. 2016) 
tree.mort.mod.SBW=function(Region,SPP,DBH,CR,HT,BAL.HW,BAL.SW,avgHT.SW,CDEF=NA){
  BF=ifelse(SPP=='BF',1,0)
  BS.RS=ifelse(SPP=='BS'|SPP=='RS',1,0)
  WS=ifelse(SPP=='WS',1,0)
  CDEF2=ifelse(is.na(CDEF),0,CDEF)
  if(Region=='ME'){
    b1=-6.5208
    b2=-0.4866
    b3.BF=-0.0355
    b3.BS.RS=-0.1231
    b3.WS=-0.1755                        
    b4=0.0316
    b5.BF=1.5087
    b5.BS.RS=1.5087
    b5.WS=1.5087
    b6=-0.0175
    b7=0.0274
    b8.BF=0.0040
    b8.BS.RS=0.0056
    b8.WS=0.0207}
  else if(Region=='NB'){
    b1=-6.8310
    b2=0.0
    b3.BF=-0.2285
    b3.BS.RS=-0.2285
    b3.WS=-0.2285
    b4=0.2025
    b5.BF=2.1703
    b5.BS.RS=2.0809
    b5.WS=1.5802
    b6=0.0
    b7=0.0
    b8.BF=0.0029
    b8.BS.RS=0.0101
    b8.WS=0.0021}
    tmorta=(1-exp(-exp(b1+b2*CR+(b3.BF*BF+b3.BS.RS*BS.RS+b3.WS*WS)*DBH+b4*avgHT.SW+
                        (b5.BF*BF+b5.BS.RS*BS.RS+b5.WS*WS)*(HT/avgHT.SW)+b6*BAL.SW+b7*BAL.HW+
                   (b8.BF*BF+b8.BS.RS*BS.RS+b8.WS*WS)*0)))
    tmortb=(1-exp(-exp(b1+b2*CR+(b3.BF*BF+b3.BS.RS*BS.RS+b3.WS*WS)*DBH+b4*avgHT.SW+
                        (b5.BF*BF+b5.BS.RS*BS.RS+b5.WS*WS)*(HT/avgHT.SW)+b6*BAL.SW+b7*BAL.HW+
                   (b8.BF*BF+b8.BS.RS*BS.RS+b8.WS*WS)*CDEF2)))
    tmort.mod=ifelse(is.na(CDEF) | SPP!='BF' & SPP!='RS' & SPP!='BS' & SPP!='WS',1.0,(1-tmortb)/(1-tmorta))
  return(tmort.mod)  
}

#tree.mort.mod.SBW(Region='ME',SPP='BF',DBH=20,CR=0.5,HT=15,BAL.HW=10,BAL.SW10,avgHT.SW=10,CDEF=200)

#Thinning mortality modifier for trees
tmort.thin.mod = function(SPP, PERCBArm, BApre, QMDratio, YEAR_CT, YEAR){
  TST = YEAR - YEAR_CT # time since thinning
  # balsam fir 
  if(SPP=="BF"){
    y0=1.7414
    y1=7.0805;
    y2=0.6677; 
    y3=0.8474
    tmort.mod=1+(exp(y0+(y1/(((100*PERCBArm+BApre)*QMDratio)+0.01)))*y2^TST*TST^y3)
  }
  # red spruce 
  else if(SPP=="RS"){
    y0=10.5057;
    y1=-650.8260;
    y2=0.6948; 
    y3=0.6429
    tmort.mod=1+(exp(y0+(y1/(((100*PERCBArm)+BApre)+0.01)))*y2^TST*TST^y3)}
  
  SP=ifelse(SPP=='BF' | SPP=='RS',1,0)
  tmort.mod = ifelse((!is.na(YEAR_CT) & YEAR_CT <= YEAR) & SP==1,tmort.mod,1)
  return(tmort.mod = tmort.mod)
}


##INGROWTH FUNCTION of Li et al. (2011; CJFR 41, 2077-2089)
# PARMS is GNLS (generalized least squares) or NLME (mixed effects), 
# CutPoint is the probability threshold where ingrowth will occur
# BA is total basal area,
# TPH is trees per ha, 
# PHW is percent hardwood basal area, 
# MinDBH is the minimum threshold diameter (cm)
# ClimateSI is the climate site index (m)
Ingrowth.FUN=function(PARMS,CutPoint,BA,TPH,QMD,PHW,MinDBH,ClimateSI,cyclen)
{
  if(PARMS=="GNLS"){
    a0=-0.2116   #-4.7867
    a1=-0.0255   #0.0469
    a2=-0.1396   #-0.8623
    a3=-0.0054   #2.3E-5
    a4=0.0433    #0.1541
    a5=0.0409    #0.1508
    a6=0.0
    b0=3.8982    #3.9018
    b1=-0.0257   #-0.0257
    b2=-0.3668   #-0.3694
    b3=0.0002    #0.0002
    b4=0.0216    #0.0216
    b5=-0.0514   #-0.0516
    b6=0.0
  } 
  else if(PARMS=='NLME'){
    b0 =          2.8466  #  0.1255  19E3    22.68    <.0001    0.05    2.6006    3.0926  -181.263
    b1 =        -0.03114  #0.001039  19E3   -29.99    <.0001    0.05  -0.03318  -0.02911  -1243.77
    b2 =         -0.2891  # 0.03785  19E3    -7.64    <.0001    0.05   -0.3633   -0.2149   -70.309
    b3 =        0.003350  #0.000496  19E3     6.76    <.0001    0.05  0.002378  0.004321  -20015.9
    a0 =        -0.08217  #  0.1536  19E3    -0.53    0.5927    0.05   -0.3833    0.2189  35.61166
    a1 =          0.1113  #0.003037  19E3    36.64    <.0001    0.05    0.1053    0.1172  322.2762
    a2 =         -1.2405  #  0.1100  19E3   -11.27    <.0001    0.05   -1.4562   -1.0248  -75.4676
    a3 =         -0.2319  #       .  19E3      .       .        0.05         .         .  -1119.36
    a4 =         0.03673  #       .  19E3      .       .        0.05         .         .  733.5435
    b4 =          0.2248  #0.007891  19E3    28.49    <.0001    0.05    0.2093    0.2402  -801.094
    a5 =         -0.7745  # 0.01281  19E3   -60.48    <.0001    0.05   -0.7996   -0.7494  944.4319
    b5 =        -0.08223  #0.005889  19E3   -13.96    <.0001    0.05  -0.09378  -0.07069   74.8579
    b6 =        -0.03548  #0.002736  19E3   -12.97    <.0001    0.05  -0.04084  -0.03011  1111.637
    a6 =         -0.1301  # 0.009078  19E3   -14.33    <.0001    0.05   -0.1479   -0.1123  115.6522
  } 
  link1 = a0+a1*BA+a2*PHW+a3*(TPH/1000)+a4*ClimateSI+a5*(MinDBH)+a6*QMD #+a6*log(BA+0.1)#+a6*QMD*BA
  PI  = (1/(1+exp(-link1)))
  eta   = b0+b1*BA+ b2*PHW+b3*(TPH/1000)+b4*ClimateSI+b5*(MinDBH)+b6*QMD #+b6*log(BA+0.1)#+b6*QMD*BA
  if (cyclen>1) 
  {
    PI = 1-((1-PI)^(1/cyclen))
    IPH = IPH*cyclen
  }
  IPH = if(CutPoint == 0) IPH*PI else ifelse(PI>=CutPoint,IPH,0)
  return(list(IPH=IPH))
}

Ingrowth.Comp=function(SPP,BA,PBA,ClimateSI,MinDBH)
{
  b10 =-2.5645#            -2.73758   #   0.0901     -30.39       <.0001
  b11 =0.0020#            0.002377   #  0.00107       2.23       0.0258
  b12 =2.6624#            2.686944   #   0.0333      80.57       <.0001
  b13 =-0.0010 #           0.002534   #  0.00606       0.42       0.6759
  b14 =-0.0127  #          0.004319   #  0.00393       1.10       0.2722
  b20 = -3.0291#           -2.99285   #   0.0836     -35.80       <.0001
  b21 = 0.0027#       #    0.002577   #  0.00102       2.52       0.0118
  b22 = 2.7779     #     2.786347   #   0.0342      81.49       <.0001
  b23 = 0.0211  #    0.018084   #  0.00522       3.46       0.0005
  b24 = 0.0221 #            0.020181   #  0.00397       5.09       <.0001
  b30 = -0.6566#            -0.5514   #   0.0658      -8.38       <.0001
  b31 = 0.0123#           0.011912   # 0.000725      16.43       <.0001
  b32 =1.7669#            1.772014   #   0.0174     102.07       <.0001
  b33 =-0.0421#            -0.04203   #  0.00442      -9.52       <.0001
  b34 =-0.0283#           -0.03874   #  0.00292     -13.25       <.0001
  b40 =-1.2500#            -1.19912   #   0.0687     -17.44       <.0001
  b41 =-0.0132 #          -0.01296    #0.000715     -18.12       <.0001
  b42 = 2.0470#           2.056631   #   0.0193     106.43       <.0001
  b43 = -0.0514 #          -0.05463   #  0.00478     -11.42       <.0001
  b44 =  0.0351#          0.029222   #  0.00301       9.71       <.0001
  b50 = -5.1074#           -5.08361   #   0.0912     -55.77       <.0001
  b51 = -0.0117#          -0.01191   #  0.00135      -8.79       <.0001
  b52 =  3.8817#          3.902292   #   0.0560      69.63       <.0001
  b53 =  0.0501#          0.046106   #  0.00620       7.44       <.0001
  b54 =  0.0726#          0.070207   #  0.00556      12.63       <.0001
  b60 =  -2.9832#          -3.10838   #   0.0667     -46.63       <.0001
  b61 =  -0.0020#          -0.00164   # 0.000830      -1.97       0.0485
  b62 =  2.4837#          2.496484   #   0.0228     109.58       <.0001
  b63 =  0.0673#          0.069919   #  0.00439      15.92       <.0001
  b64 =  -0.0167#          -0.00386   #  0.00293      -1.32       0.1868
  b70 = -4.7182#           -4.73521   #   0.0778     -60.90       <.0001
  b71 = 0.0070#           0.007145   # 0.000776       9.20       <.0001
  b72  = 3.2269#          3.234359    #  0.0340      95.04       <.0001
  b73  = 0.1000#          0.098841    # 0.00484      20.41       <.0001
  b74  = 0.0188#          0.020639    # 0.00295       6.99       <.0001
  if(SPP=='BCH')                   
  {perc =b10+b11*BA+b12*PBA+b13*ClimateSI+b14*MinDBH}
  else if(SPP=='BF')
  {perc =b20+b21*BA+b22*PBA+b23*ClimateSI+b24*MinDBH}
  else if(SPP=='RM')
  {perc =b30+b31*BA+b32*PBA+b33*ClimateSI+b34*MinDBH}
  else if (SPP=='SPR')
  {perc =b40+b41*BA+b42*PBA+b43*ClimateSI+b44*MinDBH}
  else if (SPP=='WP')
  {perc =b50+b51*BA+b52*PBA+b53*ClimateSI+b54*MinDBH}
  else if (SPP=='OH')
  {perc =b60+b61*BA+b62*PBA+b63*ClimateSI+b64*MinDBH}
  else if(SPP=='OS')
  {perc =b70+b71*BA+b72*PBA+b73*ClimateSI+b74*MinDBH}
  perc=1/(1+exp(-(perc)))
  #return(list(bcperc=bcperc,rmperc=rmperc,bfperc=bfperc,spperc=spperc,wpperc=wpperc,ohperc=ohperc,osperc=opserc))}
  return(perc=perc)
}

the.includer.func<-function(EXPF,cum.EXPF){
  if((cum.EXPF)<=100) tree.inc <-EXPF
  else if(cum.EXPF>100 & (cum.EXPF-EXPF)<=100 & ((100-(cum.EXPF-EXPF))+(cum.EXPF-EXPF))<=100){  
    tree.inc<-100-(cum.EXPF-EXPF)} 
  else tree.inc <-0
  return(tree.inc)}

## Ingrowth function
ING.TreeList=function(Sum.temp,INGROWTH="Y",MinDBH=10)
{
  # create one tree recored for each species setting DBH to MinDBH
  TreeCon=Sum.temp[Sum.temp$IPH>0,,drop=FALSE]
  InTree=NULL
  if(nrow(TreeCon)>0 && toupper(substr(INGROWTH,1,1)) == "Y")
  {
    SPP=c('BF','RM','WP','OH','OS','GB','PB','YB','RS','BS','WS')
    new=t(TreeCon[,paste0(SPP,".ING")])
    rownames(new)=SPP
    colnames(new)=TreeCon[,"PLOT"]
    for(i in 1:nrow(TreeCon))
    {
      EXPF=new[new[,i]>0,i]
      if (length(EXPF)>0) 
      {
        InTree = rbind(InTree,data.frame(PLOT=TreeCon[i,"PLOT"],
                   SP=names(EXPF),DBH=MinDBH,EXPF=EXPF,
                   TREE=(1:length(EXPF))+TreeCon[i,"maxTREE"]))
      }
    }
  }
  InTree
}

##Li et al. (2012) taper equations
KozakTaper=function(Bark,SPP,DHT,DBH,HT,Planted){
  if(Bark=='ob' & SPP=='AB'){
    a0_tap=1.0693567631
    a1_tap=0.9975021951
    a2_tap=-0.01282775
    b1_tap=0.3921013594
    b2_tap=-1.054622304
    b3_tap=0.7758393514
    b4_tap=4.1034897617
    b5_tap=0.1185960455
    b6_tap=-1.080697381
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='BC'){
    a0_tap=0.9802172591
    a1_tap=0.9900811022
    a2_tap=0.0215023934
    b1_tap=0.6092829761
    b2_tap=-0.54627086
    b3_tap=0.5221909952
    b4_tap=1.6561496035
    b5_tap=0.040879378
    b6_tap=-0.302807393
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='BF'){
    a0_tap=0.88075316
    a1_tap=1.01488665
    a2_tap=0.01958804
    b1_tap=0.41951756
    b2_tap=-0.67232564
    b3_tap=0.54329725
    b4_tap=1.48181152
    b5_tap=0.06470371
    b6_tap=-0.34684837
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='BF'){
    a0_tap=0.7909
    a1_tap=0.9745
    a2_tap=0.1198
    b1_tap=0.2688
    b2_tap=-0.55134
    b3_tap=0.5612
    b4_tap=0.9007
    b5_tap=0.1257
    b6_tap=-0.6708
    b7_tap=0
    #parms w/ FIA data
    a0_tap=0.87045800178728
    a1_tap=0.998148536293802
    a2_tap=0.0584816955042306
    b1_tap=0.302539012401385
    b2_tap=-0.605787065734974
    b3_tap=0.588861845770261
    b4_tap=0.8826608914125
    b5_tap=0.103280103524893
    b6_tap=-0.57432603217401
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='BP' | SPP=='BA'){
    a0_tap=1.0036248405
    a1_tap=0.744246238
    a2_tap=0.2876417207
    b1_tap=0.6634046516
    b2_tap=-2.004812235
    b3_tap=0.7507983401
    b4_tap=3.9248261105
    b5_tap=0.0276793767
    b6_tap=-0.130928845
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='BS'){
    a0_tap=0.80472902
    a1_tap=1.00804553
    a2_tap=0.05601099
    b1_tap=0.35533529
    b2_tap=-0.41320046
    b3_tap=0.41527304
    b4_tap=1.11652424
    b5_tap=0.0990167
    b6_tap=-0.40992056
    b7_tap=0.11394943
  }
  else if(Bark=='ob' & SPP=='BS'){
    a0_tap=0.858
    a1_tap=0.9611
    a2_tap=0.105
    b1_tap=0.2604
    b2_tap=-0.3409
    b3_tap=0.4797
    b4_tap=0.5008
    b5_tap=0.1097
    b6_tap=-0.4952
    b7_tap=0.0969
    #parms w/ FIA data
    a0_tap=0.896382313496267
    a1_tap=0.979157280469517
    a2_tap=0.07070415827334
    b1_tap=0.288205614793081
    b2_tap=-0.303580327062765
    b3_tap=0.435229599780184
    b4_tap=0.287092390832665
    b5_tap=0.0861036484421037
    b6_tap=-0.407747649433411
    b7_tap=0.371113950891855
  }
  else if(Bark=='ob' & SPP=='BT'){
    a0_tap=1.0200889056
    a1_tap=1.0054957243
    a2_tap=-0.011030907
    b1_tap=0.5104511725
    b2_tap=-1.326415929
    b3_tap=0.5568665797
    b4_tap=7.2108347873
    b5_tap=0.071149738
    b6_tap=-0.571844802
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='EH'){
    a0_tap=0.960235102
    a1_tap=1.00821143
    a2_tap=-0.025167937
    b1_tap=0.825260258
    b2_tap=1.962520834
    b3_tap=0.415234319
    b4_tap=-5.061571874
    b5_tap=0.009839526
    b6_tap=-0.095533007
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='EH'){
    a0_tap=0.8681
    a1_tap=0.916
    a2_tap=0.1558
    b1_tap=0.4067
    b2_tap=-0.6163
    b3_tap=0.4177
    b4_tap=3.6257
    b5_tap=0.1686
    b6_tap=-0.8829
    b7_tap=0
    #parms w/ FIA data
    a0_tap=0.846409603849866
    a1_tap=0.984317716125905
    a2_tap=0.0807523481457474
    b1_tap=0.445438700558324
    b2_tap=-0.671467572085628
    b3_tap=0.504954501484816
    b4_tap=2.48940465528
    b5_tap=0.124152912027385
    b6_tap=-0.722954836646604
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='GA'){
    a0_tap=1.0852385488
    a1_tap=1.1861877395
    a2_tap=-0.226193745
    b1_tap=0.5198788065
    b2_tap=1.4303205202
    b3_tap=-0.349453901
    b4_tap=3.1952591271
    b5_tap=0.1391694941
    b6_tap=-0.296716822
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='GB'){
    a0_tap=1.0263926931
    a1_tap=0.8835623138
    a2_tap=0.1307522645
    b1_tap=0.6113533288
    b2_tap=-0.114188076
    b3_tap=0.2883217076
    b4_tap=2.657433495
    b5_tap=0.0590046356
    b6_tap=-0.175127606
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='JP'){
    a0_tap=0.931552701
    a1_tap=1.008192708
    a2_tap=-0.004177373
    b1_tap=0.431297353
    b2_tap=-0.863672736
    b3_tap=0.511698303
    b4_tap=2.232484834
    b5_tap=0.059865263
    b6_tap=-0.331897255
    b7_tap=0.039630786
  }
  else if(Bark=='ob' & SPP=='JP'){
    a0_tap=1.0214
    a1_tap=0.9817
    a2_tap=0.0147
    b1_tap=0.3753
    b2_tap=-0.7954
    b3_tap=0.499
    b4_tap=2.0407
    b5_tap=0.0768
    b6_tap=-0.3335
    b7_tap=0.0408
    #parms w/ FIA data
    a0_tap=0.842483072142665
    a1_tap=0.99279768524928
    a2_tap=0.0739425827838225
    b1_tap=0.37221919371203
    b2_tap=-0.723225866494174
    b3_tap=0.453434142074953
    b4_tap=1.33754275322832
    b5_tap=0.073372838152118
    b6_tap=-0.3105255908992
    b7_tap=0.396398949039286
  }
  else if(Bark=='ib' & SPP=='NS'){
    a0_tap=0.9308817
    a1_tap=0.97360573
    a2_tap=0.03522864
    b1_tap=0.65078104
    b2_tap=-0.30355787
    b3_tap=0.37832812
    b4_tap=1.18815216
    b5_tap=0.03111631
    b6_tap=-0.03172809
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='NS'){
    a0_tap=1.0513
    a1_tap=0.9487
    a2_tap=0.0374
    b1_tap=0.611
    b2_tap=-0.3001
    b3_tap=0.3731
    b4_tap=1.1255
    b5_tap=0.0318
    b6_tap=-0.0297
    b7_tap=0
    #parms w/ FIA data
    a0_tap=0.950952303433305
    a1_tap=0.99162401049595
    a2_tap=0.0357175689757522
    b1_tap=0.507484658718266
    b2_tap=-0.44046929698967
    b3_tap=0.405856745795155
    b4_tap=1.2849978191539
    b5_tap=0.0143964536822362
    b6_tap=-0.0785889411281423
    b7_tap=0.169725200257675
  }
  else if(Bark=='ib' & SPP=='PB'){
    a0_tap=0.7161229027
    a1_tap=0.9811224473
    a2_tap=0.1382539493
    b1_tap=0.4782152412
    b2_tap=0.3091537448
    b3_tap=0.3266307618
    b4_tap=-0.302056097
    b5_tap=0.0858585241
    b6_tap=-0.278661048
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='PB'){
    a0_tap=0.7161229027
    a1_tap=0.9811224473
    a2_tap=0.1382539493
    b1_tap=0.4782152412
    b2_tap=0.3091537448
    b3_tap=0.3266307618
    b4_tap=-0.302056097
    b5_tap=0.0858585241
    b6_tap=-0.278661048
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='QA'){
    a0_tap=0.5586975794
    a1_tap=0.9047841359
    a2_tap=0.3075094544
    b1_tap=0.7131251715
    b2_tap=-0.588345303
    b3_tap=0.4292045831
    b4_tap=2.8516108932
    b5_tap=0.0381609362
    b6_tap=-0.13426388
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='RM'){
    a0_tap=0.745826994
    a1_tap=1.0092251371
    a2_tap=0.0890931039
    b1_tap=0.5861620841
    b2_tap=-0.865905462
    b3_tap=0.6539243149
    b4_tap=3.0603989176
    b5_tap=0.0827619274
    b6_tap=-0.64859681
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='RM'){
    a0_tap=0.745826994
    a1_tap=1.0092251371
    a2_tap=0.0890931039
    b1_tap=0.5861620841
    b2_tap=-0.865905462
    b3_tap=0.6539243149
    b4_tap=3.0603989176
    b5_tap=0.0827619274
    b6_tap=-0.64859681
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='RO'){
    a0_tap=1.1751352376
    a1_tap=1.02249704
    a2_tap=-0.069888591
    b1_tap=0.4505675893
    b2_tap=-0.902884964
    b3_tap=0.5812519636
    b4_tap=3.6267479819
    b5_tap=0.1656137742
    b6_tap=-1.114281314
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='RP'){
    a0_tap=0.9717883
    a1_tap=1.00113806
    a2_tap=-0.01597933
    b1_tap=0.51143292
    b2_tap=-0.9739954
    b3_tap=0.25844201
    b4_tap=4.75315518
    b5_tap=0.05846224
    b6_tap=-0.12372176
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='RP'){
    a0_tap=1.0962
    a1_tap=1.006
    a2_tap=-0.0352
    b1_tap=0.5
    b2_tap=-0.9959
    b3_tap=0.3007
    b4_tap=4.6358
    b5_tap=0.0473
    b6_tap=-0.05
    b7_tap=0
    #parms w/ FIA data
    a0_tap=1.06470820904747
    a1_tap=0.994899036827748
    a2_tap=-0.0123828485987216
    b1_tap=0.458957297467137
    b2_tap=-1.04575412640177
    b3_tap=0.361452014890273
    b4_tap=4.00047777431758
    b5_tap=0.0543368451581955
    b6_tap=-0.128025447306836
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='RS'){
    a0_tap=0.89797987
    a1_tap=1.00579742
    a2_tap=0.01667313
    b1_tap=0.49500865
    b2_tap=-0.63375155
    b3_tap=0.3836274
    b4_tap=1.41380994
    b5_tap=0.08866994
    b6_tap=-0.29753964
    b7_tap=0.15192029
  }
  else if(Bark=='ob' & SPP=='RS'){
    a0_tap=0.8758
    a1_tap=0.992
    a2_tap=0.0633
    b1_tap=0.4128
    b2_tap=-0.6877
    b3_tap=0.4413
    b4_tap=1.1818
    b5_tap=0.1131
    b6_tap=-0.4356
    b7_tap=0.1042
    #parms w/ FIA data
    a0_tap=0.886886241411388
    a1_tap=0.995431239145283
    a2_tap=0.0541365481351767
    b1_tap=0.411160410244944
    b2_tap=-0.658022227353248
    b3_tap=0.418213595349517
    b4_tap=1.09113756405639
    b5_tap=0.102379812299201
    b6_tap=-0.40367256147942
    b7_tap=0.104842994095004
  }
  #Sweet birch
  else if(Bark=='ob' & SPP=='SB'){
    a0_tap=0.8471057131
    a1_tap=0.9875376729
    a2_tap=0.0769690406
    b1_tap=0.9322599144
    b2_tap=-0.954580316
    b3_tap=0.48553875
    b4_tap=3.0294545606
    b5_tap=0.0767610836
    b6_tap=-0.238398236
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='SM'){
    a0_tap=1.0517056747
    a1_tap=0.96129896
    a2_tap=0.0386037512
    b1_tap=0.8556437779
    b2_tap=-0.249723079
    b3_tap=0.4149367053
    b4_tap=1.2548340569
    b5_tap=0.0412998707
    b6_tap=-0.113500099
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='TL'  | SPP=='TA'){
    a0_tap=0.7387
    a1_tap=0.9716
    a2_tap=0.1431
    b1_tap=0.271
    b2_tap=-0.4958
    b3_tap=0.6508
    b4_tap=-0.3887
    b5_tap=0.1324
    b6_tap=-0.7035
    b7_tap=0
    #parms w/ FIA data
    a0_tap=0.762977580507808
    a1_tap=0.979320525735404
    a2_tap=0.122788251183516
    b1_tap=0.245935863173793
    b2_tap=-0.564901857800367
    b3_tap=0.666790795105499
    b4_tap=-0.0728778930339496
    b5_tap=0.143651487515151
    b6_tap=-0.791188036888163
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='WA'){
    a0_tap=0.8550736297
    a1_tap=0.9768941226
    a2_tap=0.0770356694
    b1_tap=0.7819090026
    b2_tap=-0.791762733
    b3_tap=0.476698925
    b4_tap=3.5003928402
    b5_tap=0.0859040469
    b6_tap=-0.487974342
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='WC'  | SPP=='NC'){
    a0_tap=0.86118766
    a1_tap=0.98152118
    a2_tap=0.0568203
    b1_tap=0.40717678
    b2_tap=-0.05482572
    b3_tap=0.47809459
    b4_tap=-1.32512447
    b5_tap=0.1538487
    b6_tap=-0.53687808
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='WC' | SPP=='NC'){
    a0_tap=0.902
    a1_tap=0.9676
    a2_tap=0.085
    b1_tap=0.3204
    b2_tap=-0.4336
    b3_tap=0.5212
    b4_tap=0.0157
    b5_tap=0.137
    b6_tap=-0.4585
    b7_tap=0
    #parms w/ FIA data
    a0_tap=0.876976728762079
    a1_tap=0.972187200775237
    a2_tap=0.0905032843727524
    b1_tap=0.319643790061659
    b2_tap=-0.495778605215774
    b3_tap=0.546605647382787
    b4_tap=-0.0540118375921429
    b5_tap=0.131666046721139
    b6_tap=-0.454765563250266
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='WP'){
    a0_tap=1.04881379
    a1_tap=1.00779696
    a2_tap=-0.04595353
    b1_tap=0.38085445
    b2_tap=-0.85956463
    b3_tap=0.34380669
    b4_tap=4.60836993
    b5_tap=0.111855
    b6_tap=-0.5523203
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='WP'){
    a0_tap=1.0202
    a1_tap=0.985
    a2_tap=0.0149
    b1_tap=0.3697
    b2_tap=-0.7512
    b3_tap=0.3536
    b4_tap=3.8496
    b5_tap=0.1074
    b6_tap=-0.5131
    b7_tap=0
    #parms w/ FIA data
    a0_tap=0.961977278802905
    a1_tap=0.985977453808376
    a2_tap=0.0333180987707418
    b1_tap=0.383416881614619
    b2_tap=-0.753661988626837
    b3_tap=0.392529765236197
    b4_tap=3.4224381734935
    b5_tap=0.100601541094101
    b6_tap=-0.485617012177084
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='WS'){
    a0_tap=1.0202
    a1_tap=0.985
    a2_tap=0.0149
    b1_tap=0.3697
    b2_tap=-0.7512
    b3_tap=0.3536
    b4_tap=3.8496
    b5_tap=0.1074
    b6_tap=-0.5131
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='WS'){
    a0_tap=0.75826241
    a1_tap=0.98481863
    a2_tap=0.09956165
    b1_tap=0.36505143
    b2_tap=-0.51501314
    b3_tap=0.55913869
    b4_tap=0.75846281
    b5_tap=0.07011851
    b6_tap=-0.44928376
    b7_tap=0.07830011
  }
  else if(Bark=='ob' & SPP=='WS'){
    a0_tap=0.7317
    a1_tap=0.9577
    a2_tap=0.1593
    b1_tap=0.2638
    b2_tap=-0.4246
    b3_tap=0.5505
    b4_tap=-0.1269
    b5_tap=0.1145
    b6_tap=-0.6249
    b7_tap=0.088
    #parms w/ FIA data
    a0_tap=0.725059647049259
    a1_tap=0.999930744977476
    a2_tap=0.11890841412387
    b1_tap=0.286031149725587
    b2_tap=-0.417052954651359
    b3_tap=0.581226449067082
    b4_tap=-0.562751307358532
    b5_tap=0.101380520664108
    b6_tap=-0.563774194060357
    b7_tap=0.096121529684134
  }
  else if(Bark=='ob' & SPP=='YB'){
    a0_tap=1.1263776728
    a1_tap=0.9485083275
    a2_tap=0.0371321602
    b1_tap=0.7662525552
    b2_tap=-0.028147685
    b3_tap=0.2334044323
    b4_tap=4.8569609081
    b5_tap=0.0753180483
    b6_tap=-0.205052535
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='OH'){
    a0_tap=0.947211744
    a1_tap=0.971353083
    a2_tap=0.063182322
    b1_tap=0.633614831
    b2_tap=-0.549156049
    b3_tap=0.439010965
    b4_tap=3.187595496
    b5_tap=0.079154063
    b6_tap=-0.41277508
    b7_tap=0
  }
  else if(Bark=='ob' & SPP=='OS'){
    a0_tap=0.88047918
    a1_tap=0.988526494
    a2_tap=0.0660791
    b1_tap=0.365548416
    b2_tap=-0.607245626
    b3_tap=0.486832282
    b4_tap=1.282373726
    b5_tap=0.094120201
    b6_tap=-0.447380533
    b7_tap=0
  }
  else if(Bark=='ib' & SPP=='OS'){
    a0_tap=0.896475601
    a1_tap=1.001886257
    a2_tap=0.020707494
    b1_tap=0.391516469
    b2_tap=-0.395638544
    b3_tap=-0.011787171
    b4_tap=1.335110611
    b5_tap=0.076311559
    b6_tap=-0.286988273
    b7_tap=0
  }
  else{
   a0_tap=0.896475601
      a1_tap=1.001886257
      a2_tap=0.020707494
      b1_tap=0.391516469
      b2_tap=-0.395638544
      b3_tap=-0.011787171
      b4_tap=1.335110611
      b5_tap=0.076311559
      b6_tap=-0.286988273
      b7_tap=0
  }
  p = 1.3/HT
  z = DHT/HT
  Xi = (1 - z^(1/3))/(1 - p^(1/3))
  Qi = 1 - z^(1/3)
  y = (a0_tap * (DBH^a1_tap) * (HT^a2_tap)) * Xi^(b1_tap * z^4 + b2_tap * (exp(-DBH/HT)) +
          b3_tap * Xi^0.1 + b4_tap * (1/DBH) + b5_tap * HT^Qi + b6_tap * Xi + b7_tap*Planted)
  Diam=ifelse(Bark=='ob' & DHT==1.37,DBH,y)
  return(Diam=round(Diam,4))
}
    
DOBtoDIB=function(SPP,dob){
  if(SPP=='AB'){
    pcntbark=7
    b0_bark=1
    b1_bark=1}
  else if(SPP=='BC'){
    pcntbark=10
    b0_bark=1
    b1_bark=1}
  else if(SPP=='BF'){
    pcntbark=0
    b0_bark=0.878
    b1_bark=1.025}
  else if(SPP=='BP' | SPP=='BA'){
    pcntbark=18
    b0_bark=1
    b1_bark=1}
  else if(SPP=='BS'){
    pcntbark=0
    b0_bark=0.871
    b1_bark=1.026}
  else if(SPP=='BT'){
    pcntbark=15
    b0_bark=1
    b1_bark=1}
  else if(SPP=='EH'){
    pcntbark=0
    b0_bark=0.8916
    b1_bark=1.0121}
  else if(SPP=='GA'){
    pcntbark=13
    b0_bark=1
    b1_bark=1}
  else if(SPP=='GB'){
    pcntbark=12
    b0_bark=1
    b1_bark=1}
  else if(SPP=='JP'){
    pcntbark=0
    b0_bark=0.916
    b1_bark=1.01}
  else if(SPP=='NS'){
    pcntbark=0
    b0_bark=0.8558
    b1_bark=1.0363}
  else if(SPP=='PB'){
    pcntbark=0
    b0_bark=0.8969
    b1_bark=1.0179}
  else if(SPP=='QA'){
    pcntbark=0
    b0_bark=0.8449
    b1_bark=1.0332}
  else if(SPP=='RM'){
    pcntbark=0
    b0_bark=0.9214
    b1_bark=1.0117}
  else if(SPP=='RO'){
    pcntbark=11
    b0_bark=1
    b1_bark=1}
  else if(SPP=='RP'){
    pcntbark=0
    b0_bark=0.928
    b1_bark=0.999}
  else if(SPP=='RS'){
    pcntbark=0
    b0_bark=0.864
    b1_bark=1.029}
  else if(SPP=='SB'){
    pcntbark=12
    b0_bark=1
    b1_bark=1}
  else if(SPP=='SM'){
    pcntbark=0
    b0_bark=0.9383
    b1_bark=1.0064}
  else if(SPP=='TL' | SPP=='TA'){
    pcntbark=0
    b0_bark=1.5106
    b1_bark=0.8134}
  else if(SPP=='WA'){
    pcntbark=0
    b0_bark=0.8834
    b1_bark=1.0188}
  else if(SPP=='WC' | SPP=='NC'){
    pcntbark=0
    b0_bark=0.7797
    b1_bark=1.0569}
  else if(SPP=='WP'){
    pcntbark=0
    b0_bark=0.926
    b1_bark=1}
  else if(SPP=='WS'){
    pcntbark=0
    b0_bark=0.886
    b1_bark=1.022}
  else if(SPP=='YB'){
    pcntbark=0
    b0_bark=0.8688
    b1_bark=1.0275}
  else if(SPP=='OH'){
    pcntbark=0
    b0_bark=0.892283333
    b1_bark=1.01925} 
  else if(SPP=='OS'){
    pcntbark=0
    b0_bark=0.887333009
    b1_bark=1.019266336} 
  else{
    pcntbark=0
    b0_bark=0.889808171
    b1_bark=1.019266336}     
  dib=ifelse(pcntbark==0,b0_bark*dob^b1_bark,dob*(1-(pcntbark/100)))
  return(dib=round(dib,4))
}

smalians<-function(r1,r2,len){
  L=(r1/2)^2*pi
  S=(r2/2)^2*pi
  vol=((L+S)/2)*len
  return(round(vol,4))
}

the.includer.func<-function(EXPF,cum.EXPF){
  if(cum.EXPF<=100) tree.inc <-EXPF
  else if((cum.EXPF+EXPF)>100 & 100-cum.EXPF>0) tree.inc <- 100-cum.EXPF
  else tree.inc <-0
  return(tree.inc)}

KozakTreeVol=function(Bark,SPP,DBH,HT,Planted,stump=NA,topHT=NA,topD=NA)
{
  sgmts = 100
  stump=ifelse(is.na(stump),as.numeric(0.0),stump)
  topHT=ifelse(is.na(topHT),as.numeric(HT),topHT)
  topHT=ifelse(topHT>HT,as.numeric(HT),topHT)
  topD=(ifelse(is.na(topD),as.numeric(0.001),topD))
  L = (topHT - stump)/sgmts
  i = 0
  treeVolume = 0
  while (i < sgmts) 
  {
    H1 = L * i + stump
    H2 = L * (i + 1) + stump
    if (HT - H1 < 1e-04){
      dob1 = 0
      dib1 = 0
    }
    else {
      if (H1 == 0) 
          H1 = 0.001
      Esty1 = KozakTaper(Bark='ob',SPP=SPP,DHT=H1,DBH=DBH,HT=HT,Planted=Planted)
      dob1 = as.numeric(Esty1)
      dob1 = ifelse(dob1<topD,0,dob1)
      dib1 = DOBtoDIB(SPP=SPP,dob=dob1)
      dib1= ifelse(dob1<topD,0,dib1)
    }
    if (HT - H2 < 1e-04){
        dob2 = 0
        dib2 = 0
    }
    else {
        if (H2 == 0)
            H2 = 0.001
        Esty2 = KozakTaper(Bark='ob',SPP=SPP,DHT=H2,DBH=DBH,HT=HT,Planted=Planted)
        dob2 = as.numeric(Esty2)
        dob2 = ifelse(dob2<topD,0,dob2)
        dib2 = DOBtoDIB(SPP=SPP,dob=dob2)
        dib2= ifelse(dob1<topD,0,dib2)
    }
    treeVolume <- ifelse(Bark=='ob',treeVolume + smalians(dob1, dob2, L * 100),
       treeVolume + smalians(dib1, dib2, L * 100))
    i <- i + 1
  }
  treeVolume <- round(treeVolume/1e+06, 6)
  return(treeVolume=treeVolume) 
}
    
###################################################################################
## Westfall & Scott 2010 Forest Science (56, 515-582)  FIA taper equations       #
##################################################################################
WestfallScott  <-  function(SPP,h,H,dbh){
  if(SPP=='SM'){         #sugar maple
    th1  =  6.5790
    th2  =  0.0111
    a1    =  1.0682
    a2    =  1.1833
    gm1  =  0.1031
    gm2  =  0.2624
    phi  =  0.1516
    lmd  =  1.1482
    bet1  =  0.6637
    bet2  =  3.0996
  }
  else if(SPP=='YB' | SPP=='PB' | SPP=='GB' | SPP=='SB'){ #birches
    th1 =  7.5437
    th2 =  0.0103
    a1 =  0.9961
    a2  =  1.1042
    gm1  =  0.1313
    gm2  =  0.3539
    phi  =  0.2091
    lmd  =  0.9478
    bet1  =  0.5995
    bet2  =  3.4205
  }
  else if(SPP=='RM'){ #red maple
     th1  =  7.5707
    th2  =  0.0105
    a1    =  1.5273
    a2    =  0.7684
    gm1  =  0.0931
    gm2  =  0.4223
    phi  =  0.1441
    lmd  =  1.3910
    bet1  =  0.6453
    bet2  =  4.0737
  }
  else if(SPP=='GA' | SPP=='WA' | SPP=='BT' | SPP=='QA' | SPP=='BP'){ #ash, quaking aspen, balsam poplar
    th1  =  3.3085
    th2  =  0.0276
    a1    =  1.2634
    a2    =  0.9088
    gm1   =  0.1098
    gm2  =  0.5198
    phi  =  0.1840
    lmd  =  1.7842
    bet1  =  0.6719
    bet2  =  5.1178
  }
  else if(SPP=='AB'){ #American beech
    th1  =  8.9843
    th2  =  0.0107
    a1    =  0.7621
    a2    =  1.3734
    gm1  =  0.0956
    gm2  =  0.1650
    phi  =  0.1924
    lmd  =  1.2237
    bet1  =  0.4626
    bet2  =  1.0954
  }
  else if(SPP=='RO'){ #red oak
    th1  =  12.8336
    th2  =  0.0125
    a1    =  0.9038
    a2    =  1.0950
    gm1  =  0.0935
    gm2  =  0.3971
    phi  =  0.2038
    lmd  =  1.0457
    bet1  =  0.5508
    bet2  =  3.4681
  }
  else if(SPP=='BC'){           #black cherry
    th1  =  3.2042
    th2  =  0.0479
    a1    =  1.2507
    a2    =  0.8075
    gm1  =  0.0800
    gm2  =  0.4170
    phi  =  0.2227
    lmd  =  2.7226
    bet1  =  0.7065
    bet2  =  4.6476
  }
  else if(SPP=='BF'){                        #balsam fir
    th1  =  5.3693
    th2  =  0.0171
    a1    =  1.4212
    a2    =  0.3003
    gm1  =  0.0890
    gm2  =  0.6485
    phi  =  0.1916
    lmd  =  1.8873
    bet1  =  0.4764
    bet2  =  2.6383
  }
  else if(SPP=='RS' | SPP=='BS' | SPP=='WS'){           #spruces
    th1  =  6.8745
    th2  =  0.0110
    a1    =  1.1241
    a2    =  0.4107
    gm1  =  0.1376
    gm2  =  0.4842
    phi  =  0.2038
    lmd  =  1.2598
    bet1  =  0.4986
    bet2  =  2.7865
  }
  else if(SPP=='WP'){ #white pine
    th1  =  7.1438
    th2  =  0.0123
    a1    =  0.8978
    a2    =  0.7872
    gm1  =  0.0989
    gm2  =  0.4985
    phi  =  0.2049
    lmd  =  0.9247
    bet1  =  0.5715
    bet2  =  2.0482
  }
  else if(SPP=='TA' || SPP=='NS' || SPP=='JP'){ #larch, Norway spruce, jack pine
    th1  =  5.2913
    th2  =  0.0411
    a1    =  1.1291
    a2    =  0.6831
    gm1  =  0.0745
    gm2  =  0.5798
    phi  =  0.1896
    lmd  =  1.5776
    bet1  =  0.6616
    bet2  =  6.0645
  }
  else if(SPP=='WC'){ #northern white cedar
    th1  =  5.400
    th2  =  0.0256
    a1    =  1.9295
    a2    =  0.8142
    gm1  =  0.0943
    gm2  =  0.9642
    phi  =  0.2761
    lmd  =  1.8605
    bet1  =  1.3432
    bet2  =  1.3438
  }
  else if(SPP=='RP'){ #red pine
    th1  =  7.6044
    th2  =  0.0148
    a1    =  1.2379
    a2    =  0.3304
    gm1  =  0.0759
    gm2  =  0.6611
    phi  =  0.3008
    lmd  =  1.1569
    bet1  =  0.5462
    bet2  =  3.0627
  }
  else if(SPP=='EH'){ #eastern hemlock
    th1  =  7.2442
    th2  =  0.0152
    a1    =  1.4008
    a2    =  0.8306
    gm1  =  0.0856
    gm2  =  0.4724
    phi  =  0.2011
    lmd  =  1.5776
    bet1  =  0.6616
    bet2  =  6.0645
  }
  else if(SPP=='OH'){
    th1  =  9.0505
    th2  =  0.0241
    a1   =  1.298
    a2    =  0.7684
    gm1  =  0.0684
    gm2  =  0.4555
    phi  =  0.1769
    lmd  =  1.6684
    bet1  =  0.5408
    bet2  =  4.1821}
  else if(SPP=='OS'){ #composite of all conifers
    th1  =  6.418214286
    th2  =  0.019585714
    a1    =  1.305771429
    a2    =  0.593785714
    gm1  =  0.093685714
    gm2  =  0.615528571
    phi  =  0.223985714
    lmd  =  1.462628571
    bet1  =  0.685271429
    bet2  =  2.842342857}  
  else { #composite of all species
    th1  =  7.114957895
    th2  =  0.016836842
    a1    =  1.148384211
    a2    =  0.857194737
    gm1  =  0.101226316
    gm2  =  0.460905263
    phi  =  0.195610526
    lmd  =  1.351415789
    bet1  =  0.634036842
    bet2  =  3.302584211
  }
  x=dbh/H
  z=h/H
  S1=th1/(1+(z/th2)^lmd)
  S2=(z/bet1)^(bet2*x)/(1+(z/bet1)^(bet2*x))
  d=sqrt(dbh^2*(1.37/H/gm1)^phi*((1-z)/(1-gm1))^(a1+S1)*((1-z)/(1-gm2))^(a2*S2))
  return (d)
}
  
Vol.WestFall <- function (SPP,HT,DBHO,stump=NA,topHT=NA,topD=NA)
{
  ## stump, and top are two points, we integrate between them
  ### numerical integration using integrate function in R
  #fdiameter<-function(x) (KozakExp02(x, HT, DBHO, a0, a1, a2, b1, b2, b3, b4, b5, b6))^2
  #volInteg<-0.25*0.0001*pi*integrate(fdiameter, stump, top)$value
  
  ### smalian's volume
  sgmts = 100   # divide into 100 sections, the more sections, the closer to the numerical integration result
  stump=ifelse(is.na(stump),as.numeric(0.0),stump)
  topHT=ifelse(is.na(topHT),as.numeric(HT),topHT)
  topHT=ifelse(topHT>HT,as.numeric(HT),topHT)
  topD=(ifelse(is.na(topD),as.numeric(0.001),topD))
  L  = (topHT-stump) / sgmts
  i = 0
  treeVolume = 0
  while(i<sgmts)
  {
    H1  = L * i+stump
    H2  = L * (i+1)+stump
    
    if (HT-H1<0.0001) dob1=0 # the diameter for the tip of the tree should be 0 instead of an estimated value
    else
    {
      #if (H1==0) H1=0.001   # when x1==0, the estimate will give werid number, optional, see reasons below
      dob1=WestfallScott(SPP,H1,HT,DBHO)
  
    }
    if (HT-H2<0.0001) dob2=0 # the diameter for the tip of the tree should be 0 instead of an estimated value
    else
    {
       #if (H2==0) H2=0.001   # when x2==0, the estimate will give werid number, optional, see reasons below
       dob2=WestfallScott(SPP,H2,HT,DBHO)
         
    }
    treeVolume <-treeVolume+smalians(dob1,dob2,L*100)
    i <- i+1
  }
  treeVolume<-round(treeVolume/1E6,6)
  #vol.pred<-list(numInteg=volInteg, smalians=treeVolume)
  return(treeVolume)
}
  
  
Honer.Vol=function(SPP,HT,DBHO,topD=NA,topHT=NA)
{
  if(SPP=='WP'){
    a=0.691
    b=363.676
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='RP'){
    a=0.710
    b=355.623
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='JP' | SPP=='TA'){
    a=0.897
    b=348.520
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='LP'){
    a=0.694
    b=343.896
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='BS'){
    a=1.588
    b=333.364
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='RS'){
    a=1.226
    b=315.832
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='WS'){
    a=1.440
    b=342.175
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='BF'){
    a=2.139
    b=301.634
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='NC' | SPP=='WC'){
    a=4.167
    b=244.906
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='EH'){
    a=1.112
    b=350.092
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='QA'){
    a=-0.312
    b=436.683
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='BP' | SPP=='BT'){
    a=0.420
    b=394.644
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='PB' | SPP=='GB' | SPP=='SB'  ){
    a=2.222
    b=300.373
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='YB'){
    a=1.449
    b=344.754
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='RM' | SPP=='SM'){
    a=1.046
    b=383.972
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='BW'){
    a=0.948
    b=401.456
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='AB' | SPP=='WA'){                                      
    a=0.959
    b=334.829
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='BC'){
    a=0.033
    b=393.336
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='RO'){
    a=1.512
    b=336.509
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  else if(SPP=='OH'){
    a=1.512
    b=336.509
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736
  }
  DBHin=DBHO/2.54
  HTft=HT/.3048
  Vtcf=DBHin^2/(a+(b/HTft))
  Vtm3=Vtcf* 0.02831685
  MR.ht=topHT/HT
  MR.dib=(topD^2/DBHO^2)*(1-0.04365*b2)^-2
  return(Vtm3)
}  
  
Summary.GY=function(tree){
  library(nlme)

#  temp = mapply(SPP.func,tree$SP)  
#  tree$SPtype=as.vector(temp[1,])
#  tree$shade=as.numeric(as.character(temp[2,]))
#  tree$SG=as.numeric(as.character(temp[3,]))
  temp = SPP.func(tree$SP)  
  tree$SPtype=temp$SPtype
  tree$shade=temp$shade
  tree$SG=temp$sg

  tree$ba<-round((tree$DBH^2*0.00007854)*tree$EXPF,2)
  tree$sdi=(tree$DBH/25.4)^1.605*tree$EXPF
  tree$SG.wt=tree$SG*tree$EXPF
  tree$ba.WP=ifelse(tree$SP=='WP',tree$ba,0)
  tree$ba.BF=ifelse(tree$SP=='BF',tree$ba,0)
  tree$ba.RM=ifelse(tree$SP=='RM',tree$ba,0)
  tree$ba.RS=ifelse(tree$SP=='RS',tree$ba,0)
  tree$ba.BS=ifelse(tree$SP=='BS',tree$ba,0)
  tree$ba.WS=ifelse(tree$SP=='WS',tree$ba,0)
  tree$ba.PB=ifelse(tree$SP=='PB',tree$ba,0)
  tree$ba.YB=ifelse(tree$SP=='YB',tree$ba,0)
  tree$ba.GB=ifelse(tree$SP=='GB',tree$ba,0)
  tree$ba.HW=ifelse(tree$SPtype=='HW',tree$ba,0)
  tree$ba.SW=ifelse(tree$SPtype!='HW',tree$ba,0)
  tree$CR=((tree$HT-tree$HCB)/tree$HT)*tree$EXPF
  tree$HT=tree$HT*tree$EXPF
  temp <- subset(tree,select=c("YEAR","STAND","PLOT",'TREE','DBH','HT','CR',
        'EXPF',"ba",'ba.WP','ba.BF','ba.RM','ba.RS','ba.BS','ba.PB','ba.YB',
        'ba.GB','ba.WS','sdi','ba.HW','ba.SW','SG.wt'))
  temp<-groupedData(ba~ba|STAND/PLOT/YEAR,data=temp)
  temp <- gsummary(temp,sum,na.rm=TRUE)
  temp$BA<-temp$ba
  temp$BAPH<-temp$ba
  temp$tph<-temp$EXPF 
  temp$qmd<-sqrt(temp$BAPH/(0.00007854*temp$tph))
  temp$pHW.ba=temp$ba.HW/temp$BAPH
  temp$pSW.ba=temp$ba.SW/temp$BAPH
  temp$pWP.ba=temp$ba.WP/temp$BAPH
  temp$pBF.ba=temp$ba.BF/temp$BAPH
  temp$pRM.ba=temp$ba.RM/temp$BAPH
  temp$pRS.ba=temp$ba.RS/temp$BAPH
  temp$pBS.ba=temp$ba.BS/temp$BAPH
  temp$pWS.ba=temp$ba.WS/temp$BAPH
  temp$pPB.ba=temp$ba.PB/temp$BAPH
  temp$pYB.ba=temp$ba.YB/temp$BAPH
  temp$pGB.ba=temp$ba.GB/temp$BAPH
  temp$Avg.HT=temp$HT/temp$EXPF
  temp$Avg.LCR=temp$CR/temp$EXPF
  temp$Avg.SG=temp$SG.wt/temp$EXPF
  temp$Avg.SG=ifelse(temp$Avg.SG>.68,.68,temp$Avg.SG)
  temp$SDImax=-6017.3*temp$Avg.SG+4156.3
  temp$RD=temp$sdi/temp$SDImax
  temp=subset(temp,select=c("YEAR","STAND","PLOT",'BA','tph','qmd','sdi','Avg.HT','Avg.LCR','pWP.ba',
                            'pBF.ba','pRM.ba','pRS.ba','pBS.ba','pWS.ba','pPB.ba','pYB.ba','pGB.ba','RD',
                            'pHW.ba','pSW.ba'))
  temp
}
  
###Acadian growth and yield model
Acadian.GY=function(tree,stand,ops)
{
  ops=as.list(ops)
  ans = ddply(tree,.(STAND), function (x,stand,ops) 
    {
      stand = as.list(subset(stand,STAND == x[1,"STAND"]))
      tree = AcadianGYOneStand(tree,stand=stand,ops=ops)
    }, stand,ops)                
  tree<-sort.data.frame(ans,~+YEAR+STAND+PLOT+TREE)
  tree <<- tree   ### this is dangerous and needs to be removed.
  tree
}


###Acadian growth and yield model called for one stand at time
AcadianGYOneStand <- function(tree,stand=list(CSI=12),ops=list(verbose=TRUE))
{
  verbose  = if (is.null(ops$verbose))   FALSE      else ops$verbose
  INGROWTH = if (is.null(ops$INGROWTH))  "Y"        else ops$INGROWTH
  MinDBH   = if (is.null(ops$MinDBH))    10         else ops$MinDBH
  CutPoint = if (is.null(ops$CutPoint))  0.5        else ops$CutPoint
  mortType = if (is.null(ops$mortType))  "discrete" else ops$mortType
  cyclen   = if (is.null(ops$cyclen))    1          else ops$cyclen
  rtnVars  = if (is.null(ops$rtnVars))  c("STAND","YEAR","PLOT","TREE",
     "SP","DBH","HT","HCB","EXPF",'pHT','pHCB')     else ops$rtnVars
  SBW      = ops$SBW      
  THINMOD  = ops$THINMOD  
  CSI      = if (is.null(stand$CSI))    12          else stand$CSI
  
  if (verbose) cat ("AcadianGY: nrow(tree)=",nrow(tree)," CSI=",CSI,
    " INGROWTH=",INGROWTH," CutPoint=",CutPoint,"\n           cyclen=",cyclen,
    " MinDBH=",MinDBH," mortType=",mortType," SBW=",SBW,"\n")
  if (exists("AcadianVersionTag") && verbose) 
    cat("AcadianVersionTag=",AcadianVersionTag,"\n")

  temp = SPP.func(tree$SP)  
  tree$SPtype=temp$SPtype
  tree$shade=temp$shade
  tree$SG=temp$sg
   
  tree$ba<-(tree$DBH^2*0.00007854)*tree$EXPF
  tree$SDIadd=(tree$DBH/25.4)^1.6*tree$EXPF
  tree$ba.SW<-ifelse(tree$SPtype=='SW',tree$ba,0)
  tree$ba.WP=ifelse(tree$SP=='WP',tree$ba,0)
  tree$ba.BF=ifelse(tree$SP=='BF',tree$ba,0)
  tree$ba.RM=ifelse(tree$SP=='RM',tree$ba,0)
  tree$ba.SPR=ifelse(tree$SP=='RS' | tree$SP=='WS' | tree$SP=='BS',tree$ba,0)
  tree$ba.BRH=ifelse(tree$SP=='GB' | tree$SP=='PB' | tree$SP=='RB' | 
                     tree$SP=='YB', tree$ba,0)
  tree$ba.OH=ifelse(tree$SPtype=='HW' & tree$ba.RM==0 & tree$ba.BRH==0,tree$ba,0)
  tree$ba.OS=ifelse(tree$SPtype=='SW' & tree$ba.WP==0 & tree$ba.BF==0 & 
                    tree$ba.SPR==0,tree$ba,0)
  tree$ba.RS=ifelse(tree$SP=='RS',tree$ba,0)
  tree$ba.BS=ifelse(tree$SP=='BS',tree$ba,0)
  tree$ba.PB=ifelse(tree$SP=='PB',tree$ba,0)
  tree$ba.YB=ifelse(tree$SP=='YB',tree$ba,0)
  tree$ba.IHW=ifelse(tree$SPtype=='HW' & tree$shade<2.0,tree$ba,0)
  tree$tph.BF=ifelse(tree$SP=='BF',tree$EXPF,0)

  temp <- subset(tree,select=c("PLOT",'TREE','DBH','EXPF',"ba",
                'ba.SW','ba.WP','ba.BF','ba.RM','ba.SPR','ba.BRH','ba.OH',
                'ba.OS','ba.RS','ba.BS','ba.PB','ba.YB','ba.IHW','tph.BF'))

  temp = ddply(temp,.(PLOT),
    function(x)
    {
      rtn = as.data.frame(t(colSums(x[,c(-1,-2)])))
      rtn$PLOT = x$PLOT[1]
      rtn$maxTREE = max(x$TREE)
      rtn
    })                                
                               
  temp$BAPH<-temp$ba
  tree$SG.wt=tree$SG*tree$EXPF
  tree$DBH.SW=ifelse(tree$SPtype=='SW',tree$DBH,NA)
  
  temp2=ddply(tree,.(PLOT),summarize,
              tph=sum(EXPF),meanSG=sum(SG.wt)/sum(EXPF),
              SDI=sum(SDIadd),avgDBH.SW=mean(DBH.SW,na.rm=T))
  
  temp=merge(temp,temp2,by=c('PLOT'))
  temp$avgDBH.SW=ifelse(is.na(temp$avgDBH.SW),0,temp$avgDBH.SW)
  temp$qmd<-ifelse(temp$tph==0,0,sqrt(temp$BAPH/(0.00007854*temp$tph)))
  temp$qmd.BF=ifelse(temp$tph.BF==0,0,sqrt(temp$ba.BF/(0.00007854*temp$tph.BF)))
  temp$pHW.ba=ifelse(temp$BAPH==0,0,1-(temp$ba.SW/temp$BAPH))
  temp$pWP.ba=ifelse(temp$BAPH==0,0,temp$ba.WP/temp$BAPH)
  temp$pBF.ba=ifelse(temp$BAPH==0,0,temp$ba.BF/temp$BAPH)
  temp$pRM.ba=ifelse(temp$BAPH==0,0,temp$ba.RM/temp$BAPH)
  temp$pSPR.ba=ifelse(temp$BAPH==0,0,temp$ba.SPR/temp$BAPH)
  temp$pBRH.ba=ifelse(temp$BAPH==0,0,temp$ba.BRH/temp$BAPH)
  temp$pOH.ba=ifelse(temp$BAPH==0,0,temp$ba.OH/temp$BAPH)
  temp$pOS.ba=ifelse(temp$BAPH==0,0,temp$ba.OS/temp$BAPH)
  temp$pIHW.ba=ifelse(temp$BAPH==0,0,temp$ba.IHW/temp$BAPH)
  temp$pRS.ba=ifelse(temp$pSPR.ba==0,0,temp$ba.RS/temp$ba.SPR)
  temp$pBS.ba=ifelse(temp$pSPR.ba==0,0,temp$ba.BS/temp$ba.SPR)
  temp$pWS.ba=ifelse(temp$pSPR.ba==0,0,1-(temp$pRS.ba+temp$pBS.ba))
  temp$pPB.ba=ifelse(temp$pBRH==0,0,temp$ba.PB/temp$ba.BRH)
  temp$pYB.ba=ifelse(temp$pBRH==0,0,temp$ba.YB/temp$ba.BRH)
  temp$pGB.ba=ifelse(temp$pBRH==0,0,1-(temp$pPB.ba+temp$pYB.ba))
  temp$qmd.BF=ifelse(is.na(temp$qmd.BF),0,temp$qmd.BF)
  temp$meanSG=ifelse(temp$meanSG>.68,.68,temp$meanSG)
  temp$SDImax=-6017.3*temp$meanSG+4156.3
  temp$RD=temp$SDI/temp$SDImax  

  temp=subset(temp,select=c("PLOT",'BAPH','tph','qmd','pHW.ba',
    'pWP.ba','pBF.ba','pRM.ba','pSPR.ba','pBRH.ba','pOH.ba','pOS.ba','pRS.ba',
    'pBS.ba','pWS.ba','pPB.ba','pYB.ba','pGB.ba','qmd.BF','pIHW.ba','SDI',
    'SDImax','meanSG','RD','avgDBH.SW','maxTREE'))
    
  Sum.temp=temp
  temp$maxTREE = NULL
  tree=merge(tree,temp,by="PLOT")
  
  #set CDEF:
  # to NA if outside a SBW period (or if SBW is not set)
  # to SBW$CDEF if YEAR is within a SBW period.
  curYear = tree$YEAR[1]
  CDEF = if (is.null(SBW) || any(is.null(SBW)) || 
    !(curYear >= SBW["SBW.YR"] && 
      curYear <= SBW["SBW.YR"]+SBW["SBW.DUR"])) NA else SBW["CDEF"]
  if (verbose) cat ("SBW, curYear=",curYear," CDEF=",CDEF,"\n")   
  
  #set thinning factors.
  if (is.null(THINMOD))
  {
    pBArm=NA
    BApre=NA
    QMDratio=NA
    YEAR_CT=NA
  } else {
    pBArm   =if (is.null(THINMOD["pBArm"]))    NA else THINMOD["pBArm"]
    BApre   =if (is.null(THINMOD["BApre"]))    NA else THINMOD["BApre"]
    QMDratio=if (is.null(THINMOD["QMDratio"])) NA else THINMOD["QMDratio"]
    YEAR_CT =if (is.null(THINMOD["YEAR_CT"]))  NA else THINMOD["YEAR_CT"]
  }
  if (verbose) cat ("THINMOD, is.null=",is.null(THINMOD),"pBArm=",pBArm," BApre=",
       BApre," QMDratio=",QMDratio," YEAR_CT=",YEAR_CT,"\n")   
  
  #Compute basal area in larger trees
  tree<-sort.data.frame(tree,~+PLOT-DBH)
  temp = unlist(by(tree$ba,INDICES=tree$PLOT,FUN=cumsum))
  tree$BAL = temp-tree$ba
  temp = unlist(by(tree$ba.SW,INDICES=tree$PLOT,FUN=cumsum))
  tree$BAL.SW = temp-tree$ba.SW
  tree$BAL.HW = tree$BAL-tree$BAL.SW

  #compute tree-level crown width-related metrics
  tree$mcw=mcw(sp=tree$SP,dbh=tree$DBH)
  tree$lcw=lcw(sp=tree$SP,mcw=tree$mcw,dbh=tree$DBH)
  tree$MCA=100*((pi*(tree$mcw/2)^2)/10000)*tree$EXPF
  tree$MCA.SW<-ifelse(tree$SPtype=='SW',tree$MCA,0)
  temp = unlist(by(tree$MCA,INDICES=tree$PLOT,FUN=cumsum))
  tree$CCFL = temp-tree$MCA
  temp = unlist(by(tree$MCA.SW,INDICES=tree$PLOT,FUN=cumsum))
  tree$CCFL.SW = temp-tree$MCA.SW
  tree$CCFL.HW=tree$CCFL-tree$CCFL.SW  
 
  #calculate plot CCF
  temp = ddply(tree[,c('PLOT','MCA')],.(PLOT),function (x) sum(x$MCA))
  colnames(temp)[2] = "CCF"
  tree=merge(tree,temp,by=c('PLOT'))
  #save the plot CCF's in Sum.temp for use with ingrowth
  Sum.temp = merge(Sum.temp,temp,by="PLOT")

  #calculate heights of any with missing values.
  #generally, none will be missing when funciton is used with FVS, but some or
  #all would be missing when code us used to grow tree lists from other sources.
  tree$pHT = is.na(tree$HT) | tree$HT>900 | tree$HT==0
  if (any(tree$pHT)) 
  {
    pHT.m=mapply(HTPred,SPP=tree$SP,DBH=tree$DBH,CSI=CSI,                                
                 CCF=tree$CCF,BAL=tree$BAL)
    tree$HT=ifelse(tree$pHT,pHT.m,tree$HT)
  }
  tree$pHT = as.numeric(tree$pHT)

  #compute plot top height
  topht = ddply(tree,.(PLOT), function (x)
    {
      cum.EXPF = cumsum(x$EXPF)
      tree.inc = mapply(the.includer.func,x$EXPF,cum.EXPF)
      wt.HT=tree.inc*x$HT
      meanHT = mean(x$HT)
      topht = summarize(x,wt.HT=sum(wt.HT),tree.inc=sum(tree.inc))
      topht = ifelse(topht$tree.inc > 0, topht$wt.HT/topht$tree.inc, meanHT)
    })                                                                                
  names(topht)[2] = "topht"
  tree=merge(tree,topht,by=c('PLOT'))

  #compute average height of softwood and hardwood species
  tree$HT.SW=ifelse(tree$SPtype=='SW',tree$HT,NA)
  tree$HT.HW=ifelse(tree$SPtype=='HW',tree$HT,NA)
  
  avgHT=ddply(tree,.(PLOT),summarize,avgHT.SW=mean(HT.SW,na.rm=T),
              avgHT.HW=mean(HT.HW,na.rm=T))
  tree=merge(tree,avgHT,by=c('PLOT'))
  tree$avgHT.SW=ifelse(is.na(tree$avgHT.SW),0,tree$avgHT.SW)
  tree$avgHT.HW=ifelse(is.na(tree$avgHT.HW),0,tree$avgHT.HW)
  
  #calculate crown ratio if it is missing or if any are missing
  #see comment about missing heights
  
  tree$pHCB=FALSE
  if (is.null(tree$CR) || any(is.na(tree$CR)))
  {
    if (is.null(tree$CR)) tree$CR = NA
    # compute crown ratio from base height, compute if missing
    if (is.null(tree$HCB)) tree$HCB = NA    
    tree$pHCB = is.na(tree$HCB) | tree$HCB=='NA' | tree$HCB==0
    {      
      # Height to crown base
      pHCB.m=mapply(HCBPred,SPP=tree$SP,DBH=tree$DBH,HT=tree$HT,
                  CCF=tree$CCF,BAL=tree$BAL)
      tree$HCB=ifelse(tree$pHCB,pHCB.m,tree$HCB)
    }
    tree$CR=ifelse(is.na(tree$CR),1-(tree$HCB/tree$HT),tree$CR)
  }
  tree$pHCB = as.numeric(tree$pHCB)
  # maybe CR is defined on input and HCB is not, at this point CR will be defined.
  if (is.null(tree$HCB)) tree$HCB = tree$HT*tree$CR

  tree$dDBH=dDBH.FUN(SPP=tree$SP, DBH=tree$DBH, BAL.SW=tree$BAL.SW, BAL.HW=tree$BAL.HW,
         BA=tree$BAPH, CSI=CSI, tph=tree$tph,topht=tree$topht,CR=tree$CR,RD=tree$RD)
         
  tree$dDBH.thin.mod = if (is.null(YEAR_CT)) 1 else 
    mapply(dDBH.thin.mod,SPP=tree$SP, PERCBArm = pBArm, BApre=BApre,  
           QMDratio=QMDratio, YEAR_CT=YEAR_CT, YEAR=tree$YEAR)
  
  tree$dDBH.SBW.mod = if (is.null(CDEF)) 1 else 
    mapply(dDBH.SBW.mod,Region='ME',SPP=tree$SP,DBH=tree$DBH,
           BAL.SW=tree$BAL.SW,BAL.HW=tree$BAL.HW,CR=tree$CR,
           avgDBH.SW=tree$avgDBH.SW,topht=tree$topht,CDEF=CDEF)
  if (verbose) cat ("mean tree$dDBH.thin.mod=",mean(tree$dDBH.thin.mod),"\n")          
  if (verbose) cat ("mean tree$dDBH.SBW.mod= ",mean(tree$dDBH.SBW.mod),"\n")
  
  tree$dDBH=tree$dDBH*tree$dDBH.thin.mod*tree$dDBH.SBW.mod*cyclen
  
  #height increment
  tree$dHT=dHT(SPP=tree$SP,HT=tree$HT,CR=tree$CR,BAL.SW=tree$BAL.SW,
     BAL.HW=tree$BAL.HW,BA=tree$BAPH,CSI=CSI,tph=tree$tph,topht=tree$topht,RD=tree$RD)

  tree$dHT.thin.mod = if (is.null(YEAR_CT)) 1 else 
    mapply(dHT.thin.mod,SPP=tree$SP, PERCBArm = pBArm, BApre=BApre,  
           QMDratio=QMDratio, YEAR_CT=YEAR_CT, YEAR=tree$YEAR)
  
  tree$dHT.SBW.mod = if (is.null(CDEF)) 1 else 
    mapply(dHT.SBW.mod,SPP=tree$SP,DBH=tree$DBH,topht=tree$topht,CR=tree$CR,
           avg.DBH.SW=tree$avgDBH.SW,CDEF=CDEF)
  
  if (verbose) cat ("mean tree$dHT.thin.mod=",mean(tree$dHT.thin.mod),"\n")
  if (verbose) cat ("mean tree$dHT.SBW.mod=", mean(tree$dHT.SBW.mod),"\n")
  
  tree$dHT=tree$dHT*tree$dHT.SBW.mod*tree$dHT.thin.mod*cyclen
  
  # cap height growth
  dHTmult = approxfun(c(CSI*2,CSI*2.5),c(1,0),rule=2)(tree$dHT+tree$HT)
  if (verbose) cat ("mean dHTmult=",mean(dHTmult),"\n")
  tree$dHT=tree$dHT*dHTmult
    
  #crown recession
  tree$dHCB=dHCB(dHT=tree$dHT,DBH=tree$DBH,HT=tree$HT,HCB=tree$HCB,
                  CCF=tree$CCF,shade=tree$shade)

  tree$dHCB.thin.mod = if (is.null(YEAR_CT)) 1 else
    mapply(dHCB.thin.mod,SPP=tree$SP, PERCBArm = pBArm, BApre=BApre,  
           QMDratio=QMDratio, YEAR_CT=YEAR_CT, YEAR=tree$YEAR)
  
  if (verbose) cat ("mean tree$dHCB.thin.mod=",mean(tree$dHCB.thin.mod),"\n")
  
  tree$dHCB=tree$dHCB*tree$dHCB.thin.mod*cyclen
  
  ## Mortality
  tree = ddply (tree,.(PLOT),
              function (x)
              { 
                x = x[sort(x$DBH,decreasing=TRUE,index.return=TRUE)$ix,]
                Csward1<-cumsum(x$EXPF*(0.00015+0.00218*x$SG)*((x$DBH/25)^1.6))
                bag=(((x$DBH+x$dDBH)^2*0.00007854)-
                       (x$DBH)^2*0.00007854)*x$EXPF
                x$Sbag30=sum(ifelse(Csward1<=0.3,bag,0))
                x
              })              
                
  tmp = stand.mort.prob(region='ME',BA=tree$BAPH,BAG=tree$Sbag30/cyclen,
               QMD=tree$qmd,pBA.BF=tree$pBF.ba,pBA.IH=tree$pIHW.ba)                  
  tree$stand.pmort=tmp$prob    
  tree$stand.pmort.cut=tmp$cut    

  if (verbose) cat ("mean tree$stand.pmort=",mean(tree$stand.pmort),"\n")
  if (verbose) cat ("mean tree$stand.pmort.cut=",mean(tree$stand.pmort.cut),"\n")

  tree$stand.mort.BA=stand.mort.BA(region='ME',BA=tree$BAPH,
         BAG=tree$Sbag30/cyclen,QMD=tree$qmd,tree$qmd.BF,pBA.bf=tree$pBF.ba,
         pBA.ih=tree$pIHW.ba)

  tree$smort.thin.mod = if (is.null(YEAR_CT)) 1 else 
    BAmort.stand(BA=tree$BAPH, PCT=0, YEAR_CT=YEAR_CT, 
           YEAR=tree$YEAR, PERCBArm=pBArm, BApre=BApre, QMDratio=QMDratio)
  if (verbose) cat ("mean tree$smort.thin.mod=",mean(tree$smort.thin.mod),"\n")
  
  tree$smort.SBW.mod = if (is.null(CDEF)) 1 else 
    SBW.smort.mod(region='ME',BA=tree$BAPH,
           BA.BF=tree$pBF.ba*tree$BAPH,topht=tree$topht,CDEF=CDEF)
  if (verbose) cat ("mean tree$smort.SBW.mod=",mean(tree$smort.SBW.mod),"\n")

  # tree$stand.pmort, annual probability of at least 1 tree dying 
  # tree$stand.mort.BA, the amount of stand basal area mortality in m2/ha/yr given at least 1 tree dying
  # tree$stand.pmort.cut, the probability threshold that dictates whether mortality occurred or not
  # tree$tsurv, annual tree-level probability of survival

  tree$stand.mort.BA = if (mortType == "discrete") 
  {
    # original, discrete mortality
    lamda = -0.1 
    tree$stand.mort.BA = ifelse(
      ((tree$stand.pmort)^(1/cyclen)) > (tree$stand.pmort.cut)^(1/cyclen), 
      tree$stand.mort.BA*tree$smort.thin.mod*tree$smort.SBW.mod*(exp(lamda*(cyclen-1))), 0)   
      # ifelse(tree$stand.pmort > tree$stand.pmort.cut, 
      #   tree$stand.mort.BA*tree$smort.thin.mod*tree$smort.SBW.mod, 0) 
  } 
  else {   
    # Crookston's alternative continuous 
    w=1
    pmort = qbeta(tree$stand.pmort.cut, 
             (tree$stand.pmort*w)/(1-tree$stand.pmort), w, lower.tail = FALSE)
    if (verbose) cat ("mean pmort=",mean(pmort)," sd=",sd(pmort),"\n")
    tree$stand.mort.BA*tree$smort.thin.mod*tree$smort.SBW.mod*pmort
  } 

  #thinning mortalaity modifier
  tree$tmort.thin.mod = if (is.na(YEAR_CT)) 1 else   
    mapply(tmort.thin.mod, SPP=tree$SP, PERCBArm = pBArm, BApre=BApre, 
                  QMDratio=QMDratio, YEAR_CT=YEAR_CT, YEAR=tree$YEAR)
  #spruce budworm mortality modifier
  tree$tsurv.SBW.mod = if (is.null(CDEF)) 1 else 
    mapply(tree.mort.mod.SBW,Region='ME',SPP=tree$SP,DBH=tree$DBH,
           CR=tree$DBH,HT=tree$HT,BAL.HW=tree$BAL.HW,BAL.SW=tree$BAL.SW,
           avgHT.SW=tree$avgHT.SW,CDEF=CDEF)
  if (verbose) cat ("mean tree$tmort.thin.mod=",mean(tree$tmort.thin.mod),"\n")
  if (verbose) cat ("mean tree$tsurv.SBW.mod=",mean(tree$tsurv.SBW.mod),"\n")
  
  tree$tsurv=pmax(0,pmin(tree.mort.prob(tree$SP,tree$DBH)*tree$tsurv.SBW.mod*tree$tmort.thin.mod,1.0))

  tree$mortBA=((tree$DBH+tree$dDBH)^2*0.00007854)*tree$EXPF*(1-tree$tsurv)
         
  tree = ddply (tree,.(PLOT),
                function (x)
                {
                  # sort the trees on the plot on increasing survivorship
                  #x = x[sort(x$tsurv,decreasing=FALSE,index.return=TRUE)$ix,]
                  x = sort.data.frame(x,~+tsurv+DBH)
                  x$Cum.mort<-cumsum(x$mortBA)
                  x$DmortBA = (x$stand.mort.BA-x$Cum.mort)
                  x$Cum.DmortBA=cumsum(x$DmortBA)
                  x$xDmortBA=x$Cum.DmortBA-(x$DmortBA)
                  x$xmortBA=x$Cum.mort-x$mortBA
                  x$vv=x$stand.mort.BA-(x$Cum.mort-x$mortBA)
                  x$xxmortBA=x$Cum.DmortBA-x$Cum.mort
                  x$TotBAmort=round(x$mortBA+x$xDmortBA,8)
                  x$pBAmort=x$vv/x$mortBA#(((x$DBH+(x$dDBH*cyclen))^2*0.00007854)*x$EXPF)
                  x$ba.mort=0.00007854*(x$DBH+x$dDBH)^2
                  x$zTPH=x$vv/x$ba.mort
                  x$dEXPF=ifelse(x$Cum.mort<=x$stand.mort.BA,
                                 x$EXPF-(x$EXPF*x$tsurv),ifelse(x$vv>=0,  #x$TotBAmort<=(x$stand.mort.BA),
                                                                #x$EXPF - x$EXPF*(1-x$pBAmort),0))
                                                                x$zTPH,0))
                  x$EXPF.ba=(((x$DBH+(x$dDBH))^2*0.00007854)*x$dEXPF)
                  x$Dead=cumsum(x$EXPF.ba)
                  x
                })
  ##INGROWTH
  ingrow = NULL
  if (toupper(substr(INGROWTH,1,1)) == "Y")
  {

    Sum.temp$IPH=as.numeric(mapply(Ingrowth.FUN,PARMS='GNLS',CutPoint=CutPoint,
         BA=Sum.temp$BAPH,TPH=Sum.temp$tph,QMD=Sum.temp$qmd,PHW=Sum.temp$pHW.ba,
         MinDBH=MinDBH,ClimateSI=CSI,cyclen=cyclen))
  
    Sum.temp$pBCH.ING=mapply(Ingrowth.Comp,SPP='BCH',BA=Sum.temp$BAPH,PBA=Sum.temp$pBRH.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pBF.ING=mapply(Ingrowth.Comp,SPP='BF',BA=Sum.temp$BAPH,PBA=Sum.temp$pBF.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pRM.ING=mapply(Ingrowth.Comp,SPP='RM',BA=Sum.temp$BAPH,PBA=Sum.temp$pRM.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pSPR.ING=mapply(Ingrowth.Comp,SPP='SPR',BA=Sum.temp$BAPH,PBA=Sum.temp$pSPR.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pWP.ING=mapply(Ingrowth.Comp,SPP='WP',BA=Sum.temp$BAPH,PBA=Sum.temp$pWP.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pOH.ING=mapply(Ingrowth.Comp,SPP='OH',BA=Sum.temp$BAPH,PBA=Sum.temp$pOH.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pOS.ING=mapply(Ingrowth.Comp,SPP='OS',BA=Sum.temp$BAPH,PBA=Sum.temp$pOS.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pSP=Sum.temp$pBCH.ING+Sum.temp$pBF.ING+Sum.temp$pRM.ING+Sum.temp$pSPR.ING+Sum.temp$pWP.ING+Sum.temp$pOH.ING+Sum.temp$pOS.ING
    Sum.temp$pSPx=1/Sum.temp$pSP
    Sum.temp$pBCH.ING=Sum.temp$pBCH.ING*Sum.temp$pSPx
    Sum.temp$pBF.ING=Sum.temp$pBF.ING*Sum.temp$pSPx
    Sum.temp$pRM.ING=Sum.temp$pRM.ING*Sum.temp$pSPx
    Sum.temp$pSPR.ING=Sum.temp$pSPR.ING*Sum.temp$pSPx
    Sum.temp$pWP.ING=Sum.temp$pWP.ING*Sum.temp$pSPx
    Sum.temp$pOH.ING=Sum.temp$pOH.ING*Sum.temp$pSPx
    Sum.temp$pOS.ING=Sum.temp$pOS.ING*Sum.temp$pSPx
    Sum.temp$BCH.ING=Sum.temp$pBCH.ING*Sum.temp$IPH
    Sum.temp$BF.ING=Sum.temp$pBF.ING*Sum.temp$IPH
    Sum.temp$RM.ING=Sum.temp$pRM.ING*Sum.temp$IPH
    Sum.temp$SPR.ING=Sum.temp$pSPR.ING*Sum.temp$IPH
    Sum.temp$WP.ING=Sum.temp$pWP.ING*Sum.temp$IPH
    Sum.temp$OH.ING=Sum.temp$pOH.ING*Sum.temp$IPH
    Sum.temp$OS.ING=Sum.temp$pOS.ING*Sum.temp$IPH
    Sum.temp$GB.ING=Sum.temp$BCH.ING*Sum.temp$pGB.ba
    Sum.temp$PB.ING=Sum.temp$BCH.ING*Sum.temp$pPB.ba
    Sum.temp$YB.ING=Sum.temp$BCH.ING*Sum.temp$pYB.ba
    Sum.temp$RS.ING=Sum.temp$SPR.ING*Sum.temp$pRS.ba
    Sum.temp$BS.ING=Sum.temp$SPR.ING*Sum.temp$pBS.ba
    Sum.temp$WS.ING=Sum.temp$SPR.ING*Sum.temp$pWS.ba

    ingrow = ING.TreeList(Sum.temp,INGROWTH,MinDBH=MinDBH)
    
    if (!is.null(ingrow))
    {
      # get predicted heights and HCB, use plot CCF and BA as BAL.
      CCF =Sum.temp[ingrow[,"PLOT"],"CCF"]
      BAPH=Sum.temp[ingrow[,"PLOT"],"BAPH"]
      ingrow$HT=mapply(HTPred,SPP=ingrow$SP,DBH=ingrow$DBH,CSI=CSI,
                       CCF=CCF,BAL=BAPH)                     
      ingrow$HCB=mapply(HCBPred,SPP=ingrow$SP,DBH=ingrow$DBH,HT=ingrow$HT,
                        CCF=CCF,BAL=BAPH)
    }
  }

  tree$YEAR <- tree$YEAR+cyclen
  tree$DBH  <- tree$DBH + tree$dDBH
  tree$HT   <- tree$HT  + tree$dHT
  tree$HCB  <- tree$HCB + tree$dHCB
  tree$EXPF <- tree$EXPF- tree$dEXPF
  rtnVars   <- setdiff(rtnVars,setdiff(rtnVars,colnames(tree)))  
  tree<-subset(tree,select=rtnVars)
                 
  if (verbose) cat ("AcadianGY return: nrow(tree)=",nrow(tree),
    " Num ingrowth trees=",if (is.null(ingrow)) 0 else nrow(ingrow),"\n") 

  if (!is.null(ingrow))
  {
    missing <- setdiff(rtnVars,colnames(ingrow))
    if (length(missing)) for (miss in missing) ingrow[[miss]] = NA
    tree <- rbind(tree,ingrow)
  }
  
  tree
}

