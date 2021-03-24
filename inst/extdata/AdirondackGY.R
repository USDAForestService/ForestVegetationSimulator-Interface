# $Id: AdirondackGY.R 3269 2020-10-27 15:16:35Z nickcrookston $

AdirondackGYVersionTag = "SUNY_GYv5ck"

###Adirondack functions
library(plyr)  #needed for ddply

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
  if(!is.element(substring(formc,1,1),c("+","-"))) formc<-paste("+",formc,sep="")
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
SPP.func=function(SPP){            
  if(SPP=='AB'){ #American beech
    SPtype='HW'
    sg=0.64
    wd=0.56
    shade=4.75
    drought=1.5
    waterlog=1.5}
  else if(SPP=='AS'){ #ash
    SPtype='HW'
    sg=0.57
    wd=0.51
    shade=2.84
    drought=2.74
    waterlog=3.02}
  else if(SPP=='BA'){ #black ash
    SPtype='HW'
    sg=0.49
    wd=0.45
    shade=2.96
    drought=2
    waterlog=3.5
    sg=0.5}
  else if(SPP=='BC'){ #black cherry
    SPtype='HW'
    sg=0.5
    wd=0.47
    shade=2.46
    drought=3.02
    waterlog=1.06}
  else if(SPP=='BF'){ #balsam fir
    SPtype='SW'
    sg=0.35
    wd=0.33
    shade=5.01
    drought=1
    waterlog=2}
  else if(SPP=='BP'){ #balsam poplar
    SPtype='HW'
    sg=0.34
    wd=0.31
    shade=1.27
    drought=1.77
    waterlog=2.63}
  else if(SPP=='BS'){ #black spruce
    SPtype='SW'
    sg=0.46
    wd=0.38
    shade=4.08
    drought=2.0
    waterlog=2.0}
  else if(SPP=='BT'){ #bigtooth aspen
    SPtype='HW'
    sg=0.39
    wd=0.36
    shade=1.21
    drought=2.5
    waterlog=2}
  else if(SPP=='EC'){ #eastern cottonwood
    SPtype='HW'
    sg=0.4
    wd=0.37
    shade=1.76
    drought=1.57
    waterlog=3.03}
  else if(SPP=='EH'){ #eastern hemlock
    SPtype='SW'
    sg=0.4
    wd=0.38
    shade=4.83
    drought=1
    waterlog=1.25}
  else if(SPP=='GA'){ #green ash
    SPtype='HW'
    sg=0.56
    wd=0.53
    shade=3.11
    drought=3.85
    waterlog=2.98}
  else if(SPP=='GB'){ #gray birch
    SPtype='HW'
    sg=0.48
    wd=0.45
    shade=1.5
    drought=2.34
    waterlog=1}
  else if(SPP=='HH'){ #eastern hophornbeam
    SPtype='HW'
    sg=0.78
    wd=0.63
    shade=4.58
    drought=3.25
    waterlog=1.07}
  else if(SPP=='JP'){ #jack pine
    SPtype='SW'
    sg=0.43
    wd=0.4
    shade=1.36
    drought=4
    waterlog=1}
  else if(SPP=='NS'){ #Norway spruce
    SPtype='SW'
    sg=0.43
    wd=0.37023
    shade=4.45
    drought=1.75
    waterlog=1.22}    
  else if(SPP=='OH'){ #other hardwoods
    SPtype='HW'
    sg=0.5121
    wd=0
    shade=2.29
    drought= 0
    waterlog=0}
  else if(SPP=='OS'){ #other softwoods
    SPtype='SW'
    sg=0.445
    wd=0
    shade=2.27
    drought=0
    waterlog=0}
  else if(SPP=='PB'){ #paper birch
    SPtype='SW'
    sg=0.55
    wd=0.48
    shade=1.54
    drought=2.02
    waterlog=1.25}
  else if(SPP=='PC' | SPP=='PR'){ #pin cherry 
    SPtype='HW'
    sg=0.38
    wd=0.36
    shade=2.26
    drought=0
    waterlog=0}
  else if(SPP=='QA'){ #quaking aspen
    SPtype='HW'
    sg=0.38
    wd=0.35
    shade=1.21
    drought=1.77
    waterlog=1.77}
  else if(SPP=='RB'){ #river birch
    SPtype='HW'
    sg=0.62
    wd=0.49
    shade=1.45
    drought=1.53
    waterlog=2.85}
  else if(SPP=='RM'){ #red maple
    SPtype='HW'
    sg=0.54
    wd=0.49
    shade=3.44
    drought=1.84
    waterlog=3.08}
  else if(SPP=='RP' | SPP=='RN'){ #red pine
    SPtype='SW'
    sg=0.46
    wd=0.41
    shade=1.89
    drought=3
    waterlog=1}
  else if(SPP=='RO'){ #red oak
    SPtype='HW'
    sg=0.63
    wd=0.56
    shade=2.75
    drought=2.88
    waterlog=1.12}
  else if(SPP=='RS'){ #red spruce
    SPtype='SW'
    sg=0.4
    wd=0.37
    shade=4.39
    drought=2.5
    waterlog=2}
  else if(SPP=='SB'){ #Sweet birch
    SPtype='HW'
    sg=0.65
    wd=0.6
    shade=2.58
    drought=3
    waterlog=1}
  else if(SPP=='SM'){ #sugar maple
    SPtype='HW'
    sg=0.63
    wd=0.56
    shade=4.76
    drought=2.25
    waterlog=1.09}
  else if(SPP=='ST'){#striped maple
    SPtype='HW'
    sg=0.46
    wd=0.44
    shade=3.56
    drought=2
    waterlog=1}
  else if(SPP=='TA'){ #larch/tamarack
    SPtype='SW'
    sg=0.53
    wd=0.49
    shade=0.98
    drought=2
    waterlog=3}
  else if(SPP=='WA'){ #white ash
    SPtype='HW'
    sg=0.6
    wd=0.55                     
    shade=2.46
    drought=2.38
    waterlog=2.59}
  else if(SPP=='WC'){ #northern white cedar
    SPtype='SW'
    sg=0.31
    wd=0.29
    shade=3.45
    drought=2.71
    waterlog=1.46}
  else if(SPP=='WP'){ #white pine
    SPtype='SW'
    sg=0.35
    wd=0.34
    shade=3.21
    drought=2.29
    waterlog=1.03}
  else if(SPP=='WS'){ #white spruce
    SPtype='SW'
    sg=0.4
    wd=0.33
    shade=4.15
    drought=2.88
    waterlog=1.02}
  else if(SPP=='YB'){ #yellow birch
    SPtype='HW'
    sg=0.62
    wd=0.55
    shade=3.17
    drought=3
    waterlog=2}
  else{
    SPtype='HW'
    sg=0
    wd=0
    shade=0
    drought=0
    waterlog=0}    
  return(c(SPtype=SPtype,shade=shade))
}

  
#Maximum crown width
mcw=function(sp,dbh){
  if(sp=='BF'){
    a1=1.37; a2=0.572}
  else if(sp=='BS'){
    a1=0.535; a2=0.742}
  else if(sp=='EH'){
    a1=2.44; a2=0.408}
  else if(sp=='WP'){
    a1=1.24; a2=0.585}
  else if(sp=='NC'){
    a1=1.63; a2=0.436}
  else if(sp=='RS'){
    a1=1.80; a2=0.461}
  else if(sp=='WS'){
    a1=1.50; a2=0.496}
  else if(sp=='AB'){
    a1=2.93; a2=0.434}
  else if(sp=='GB' | sp=='RB'){
    a1=2.24; a2=0.382}
  else if(sp=='RO'){
    a1=4.08; a2=0.310}
  else if(sp=='PB'){
    a1=1.48; a2=0.623}
  else if(sp=='QA'){
    a1=1.31; a2=0.586}
  else if(sp=='RM'){
    a1=2.17; a2= 0.491}
  else if(sp=='SM'){
    a1=3.31; a2=0.356}
  else if(sp=='YB'){
    a1=4.04; a2=0.308}
  else if(sp=='OH'){
      a1=4.04; a2=0.308}
  else if(sp=='OS'){
      a1=1.597128571; a2=0.513957143}
  else{a1=2.24262; a2=0.462653333}        
   mcw=a1*dbh**a2
   return(mcw)
}

   
#These are the parm ests for estimating largest crown width (lcw)
lcw=function(sp,mcw,dbh){
  if(sp=='BF'){
    b1=1.49; b2=0.105}
  else if(sp=='BS'){
    b1=1; b2=0.174}
  else if(sp=='EH'){
    b1=1.90; b2=-0.057}
  else if(sp=='WP'){
    b1=1; b2=0.147}
  else if(sp=='NC'){
    b1=2.19; b2=-0.080}
  else if(sp=='RS'){
    b1=4.33; b2=-0.264}
  else if(sp=='WS'){
    b1=2.09; b2=-0.069}
  else if(sp=='AB'){
    b1=1; b2=0.194}
  else if(sp=='GB' | sp=='RB'){
    b1=3.10; b2=-0.214}
  else if(sp=='RO'){
    b1=4.10; b2=-0.272}
  else if(sp=='PB'){
    b1=2.10; b2=-0.035}
  else if(sp=='QA'){
    b1=2.65; b2=0.157}
  else if(sp=='RM'){
    b1=2.63; b2=-0.132}
  else if(sp=='SM'){
    b1=1; b2=0.161}
  else if(sp=='YB'){
    b1=4.23; b2=-0.264}
  else if(sp=='OH'){
    b1=2.65; b2=0.157}
  else if(sp=='OS'){
    b1=2.3276; b2=0.027842857}
  else{
    b1=2.79282; b2=-0.090113333}
   lcw=mcw/(b1*dbh**b2)
   return(lcw)
}   


### Total height prediction function
SUNY.HT=function(SPP,TYPE,DBH,BA,BAL){
  if(SPP=='AB'){c0=33.60202593; c1=0.02013634;  c2=1.05838369; c3=-0.04980422; c4=-0.08893961}
  else if(SPP=='BA' | SPP=='WA')
  {c0=42.061204367; c1=0.009251587; c2=0.737188616; c3=-0.091553958; c4=0.0}
  else if(SPP=='BC'){c0=30.23272045;  c1=0.02427305;  c2=1.42154921; c3=-0.25221320; c4=0.0}
  else if(SPP=='BF'){c0=42.06962633;  c1=0.01211685;  c2=0.88597486; c3=-0.07201581; c4=0.0}
  else if(SPP=='EH'){c0=48.02785238;  c1=0.01170433;  c2=1.29793886; c3=-0.09813474; c4=-0.04289923}
  else if(SPP=='RM'){c0=29.94627655;  c1=0.03305693;  c2=1.27399373; c3=-0.08924157; c4=-0.08776513}
  else if(SPP=='RN'){c0=29.9418633; c1=0.0752220; c2=3.5271938; c3=0.0; c4=-0.5136051}
  else if(SPP=='SM'){c0=38.03839476;  c1=0.02108837;  c2=1.03254194; c3=-0.05436848; c4=-0.06389938}
  else if(SPP=='BS' | SPP=='WS' | SPP=='RS'){
    c0=42.33243329; c1=0.02172757;  c2=1.47383736; c3=-0.04531030; c4=-0.10763772}
  else if(SPP=='WP'){c0=37.72131566; c1=0.02888957;  c2=1.88865891; c3=-0.13676296; c4=-0.13921116}
  else if(SPP=='YB' | SPP=='RB')
  {c0=29.57552840;  c1=0.02858993;  c2=1.00284095; c3=-0.02988684; c4=-0.08856909}
  else if(TYPE=='SW' & SPP!='EH' & SPP!='WP' & SPP!='BF' & SPP!='BS' & SPP!='WS' & SPP!='RS' & SPP!='RN'){
    c0=26.18749877;  c1=0.08191742;  c2=3.53099659; c3=0.0; c4=-0.33749422}
  else if(TYPE=='HW' & SPP!='AB' & SPP!='BA' & SPP!='BC' & SPP!='RB' & SPP!='RM' & SPP!='SM' & SPP!='WA' & SPP!='YB'){
    c0=35.44568914;  c1=0.02416222; c2=1.01276331; c3=0.0; c4=-0.07550222}
  HT=c0*(1-exp(-c1*DBH))^(c2+c3*log(BA+1)+c4*log(BAL+1))  
  return(HT=round(HT,4))}
  

#Height to crown base
SUNY.BHT=function(SPP,TYPE,DBH,HT,BAL,BA,CSI){
  if(SPP=='AB'){c0=-1.37740623; c1=-0.06120574; c2=-0.04234989; c3=0.0; c4=-1.23308340; c5=0.92282549 }
  else if(SPP=='BA' | SPP=='WA')
  {c0=-4.59574215; c1=-0.04488613; c2=0.0; c3=0.0; c4=-0.22742141;  c5=1.75948722}
  else if(SPP=='BC'){c0=-7.2046242; c1=-0.0489819; c2=0.0; c3=0.0; c4=-0.2215719; c5=2.6473577}
  else if(SPP=='BF'){c0=-5.7094789; c1=-0.0990756; c2=0.0; c3=0.0; c4=-1.1597578; c5=2.5493271}
  else if(SPP=='EH'){c0=-3.77145763; c1=-0.06293303; c2=-0.03148338; c3=0.0; c4=-1.09968592;  c5=1.76449115}
  else if(SPP=='RM'){c0=0.76533579; c1=-0.03193009; c2=0.0; c3=-0.13323780; c4=-0.42870392 ; c5=0.0}
  else if(SPP=='RN'){c0=0.07230731; c1=-0.02052358; c2=0.0; c3=-0.14338368; c4=-0.45705576; c5=0.0}
  else if(SPP=='SM'){c0=-2.14155199; c1=-0.02284441; c2=0.0; c3=-0.07290624; c4=-0.25821780;  c5=0.82608798}
  else if(SPP=='BS'|SPP=='WS'|SPP=='RS'){
    c0=0.83106217; c1=-0.06039774; c2=-0.03655386; c3=0.0; c4=-1.11443448; c5=0.0}
  else if(SPP=='WP'){c0=-4.34841564; c1=-0.04371825; c2=-0.02750443; c3=0.0; c4=-0.78588870;  c5=1.61854718}
  else if(SPP=='YB' | SPP=='RB')
  {c0=-1.30437134; c1=-0.04411748; c2=-0.03131312; c3=-0.06780794; c4=-0.85166539;  c5=0.80411046}
  else if(TYPE=='SW' & SPP!='EH' & SPP!='WP' & SPP!='BF' & SPP!='BS' & SPP!='WS' & SPP!='RS' & SPP!='RN'){
    c0=-0.02479770; c1=-0.01628296; c2=-0.02002364; c3=-0.10505744; c4=0.0; c5=0.0}
  else if(TYPE=='HW' & SPP!='AB' & SPP!='BA' & SPP!='BC' & SPP!='RB' & SPP!='RM' & SPP!='SM' & SPP!='WA' & SPP!='YB'){
    c0=-2.29101767; c1=-0.05459734; c2=0.0; c3=-0.18631645; c4=-0.55642682; c5=1.26568748}
  HCB=HT/(1+exp(c0+c1*HT+c2*log(BAL+0.1)+c3*log(BA)+c4*log(DBH/HT)+c5*log(CSI)))
  return(HCB=round(HCB,4))
}   


#Diameter increment model
SUNY.dDBH=function(SPP,TYPE,DBH,BAL,BA,CSI){
  if(SPP=='AB') {b0=-3.44297199;  b1=1.42193221; b2=-0.07270023; b3=-0.02501082; b4=0.0; b5=0.0}
  else if(SPP=='BA' | SPP=='WA')
  {b0=-6.184018928;  b1=0.019519878; b2=-0.001987463; b3=-0.021141816; b4=0.0; b5=1.800040865}
  else if(SPP=='BF') {b0=-2.592405733;  b1=1.205637581; b2=-0.055607527; b3=-0.006418332; b4=-0.201335088; b5=0.0}
  else if(SPP=='EH') {b0=-1.750785553;  b1=0.577486763; b2=-0.011261662; b3=-0.005718925; b4=-0.179518414; b5=0.0}
  else if(TYPE=='HW' & SPP!='AB' & SPP!='RM' & SPP!='SM' & SPP!='YB' & SPP!='WA' & SPP!='BA' & SPP!='RO' & SPP!='QA')
  {b0=-1.51972882;  b1=0.32313807; b2=-0.02246780; b3=-0.02861302; b4=0.0; b5=0.0}
  else if(TYPE=='SW' & SPP!='EH' & SPP!='WC' & SPP!='WP' & SPP!='BF' & SPP!='RS' & SPP!='WS' & SPP!='BS' & SPP!='RN') 
  {b0=-3.42030179;  b1=1.05993031; b2=-0.03010462; b3=-0.01828509; b4=-0.05407572; b5=0.0}
  else if(SPP=='QA') {b0=-2.35910566; b1=1.06593145; b2=-0.06525356; b3=-0.04692269; b4=0.0; b5=0.0}
  else if(SPP=='RM') {b0=-4.14653728; b1=0.28888491; b2=-0.01481288; b3=-0.01561762;  b4=0.0; b5=0.84152296}
  else if(SPP=='RN') {b0=-7.68291514; b1=2.78429128; b2=-0.10168500; b3=-0.02104973; b4=0.0; b5=0.0}
  else if(SPP=='RO') {b0=-2.36802619; b1=0.87757490; b2=-0.05373401; b3=-0.01592160; b4=0.0; b5=0.0}
  else if(SPP=='SM') {b0=-2.60585659; b1=0.46142919; b2=-0.01759435; b3=-0.01557078; b4=-0.13507304;  b5=0.32943264}
  else if(SPP=='RS' | SPP=='WS' | SPP=='BS') 
  {b0=-4.565403197; b1=0.364567336; b2=-0.009782068; b3=-0.025792847;  b4=0.0; b5=0.727236354}
  else if(SPP=='WC') {b0=-2.820472476;  b1=0.429022212; b2=-0.004233925; b3=-0.011098459; b4=0.0;  b5=0.0}
  else if(SPP=='WP') {b0=-4.53544927; b1=0.96487775; b2=-0.02077559; b3=-0.02296264; b4=-0.09857382;  b5=0.62257335}
  else if(SPP=='YB' | SPP=='RB')
  {b0=-1.09560693;  b1=0.40429445; b2=-0.01859219; b3=-0.01226103; b4=-0.18076795; b5=0.0}
  dDBH1=exp(b0+b1*log(DBH)+b2*DBH+b3*(BAL)+b4*sqrt(BA)+b5*log(CSI))
  return(dDBH=dDBH1)
}

#Height increment
SUNY.dHT=function(SPP,TYPE,HT,DBH,BAL,BA){
  if(SPP=='AB') {b0=-2.00164; b1=-0.1519273; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='BA' | SPP=='WA')
  {b0=-2.466252; b1=-0.1134281; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='BC') {b0=-2.626046; b1=-0.1226641; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='BF') {b0=-2.723272; b1=-0.1036923; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='EH') {b0=-2.840891; b1=-0.1169127; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(TYPE=='HW' & SPP!='AB' & SPP!='RM' & SPP!='SM' & SPP!='YB' & SPP!='WA' & SPP!='BA' & SPP!='RO' & SPP!='QA')
  {b0=-2.756019; b1=-0.1110807; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(TYPE=='SW' & SPP!='EH' & SPP!='WC' & SPP!='WP' & SPP!='BF' & SPP!='RS' & SPP!='WS' & SPP!='BS' & SPP!='RN') 
  {b0=-2.753201; b1=-0.1081164; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='QA') {b0=-2.673352; b1=-0.1114477; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='RM') {b0=-2.415736; b1=-0.1284431; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='RN') {b0=-2.831579; b1=-0.09790439; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='RO') {b0=-3.097226; b1=-0.1039751; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='SM') {b0=-2.566654; b1=-0.1179754; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='RS' | SPP=='WS' | SPP=='BS') 
  {b0=-3.143091; b1=-0.1069051; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='WC') {b0=-2.816394; b1=-0.1138721; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='WP') {b0=-3.146636; b1=-0.08360297; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  else if(SPP=='YB' | SPP=='RB')
  {b0=-2.01305; b1=-0.1526285; b2=0.8261186; b3=-0.05829911; b4=-0.01207503; b5=-0.004878126; b6=0.3740947}
  dHT1=exp(b0+b1*(HT)+b2*log(HT)+b3*(HT/DBH)+b4*(BAL)+b5*(BA)+b6*log(BA))
  return(dHT=dHT1)
}



#Mortality function 1 (= survival function using expansion factor!!!)
SUNY.surv=function(SPP,TYPE,DBH,CR,BAL,BA,CSI){
  if(SPP=='AB'){b0=7.40690341; b1=-0.08553107; b2=0.0;  b3=2.96492185; b4=-0.10413708; b5=0.0; b6=0.0}
  else if(SPP=='BA' | SPP=='WA'){
    b0=-2.69723249; b1=-0.05109722; b2=2.72197651; b3=3.18682793; b4=0.0; b5=-0.26771991; b6=0.0}
  else if(SPP=='BF'){b0=5.43210492; b1=-0.06175465; b2=0.0; b3=0.0; b4=-0.03827896; b5=0.0; b6=-0.0}
  else if(SPP=='EH'){b0=-1.98966722; b1=-0.08091288;  b2=3.10196186; b3=0.0; b4=0; b5=0; b6=0}
  else if(SPP=='QA'){b0=0.28105972; b1=-0.05167895; b2=1.76260960; b3=0.0; b4=0.0; b5=-0.21035163; b6=0.0}
  else if(SPP=='RM'){b0=11.2896134; b1=-0.1113001; b2=0.0; b3=0.0; b4=-0.1479613; b5=0.0; b6=0.0}
  else if(SPP=='RN'){b0=8.9149452;  b1=0.2445655; b2=0.0; b3=0.0; b4=0.0; b5=0.0; b6=-0.5345674}
  else if(SPP=='RO'){b0=-6.20209544; b1=-0.08888149;  b2=4.55453873; b3=0.0; b4=0.0; b5=0.0; b6=0.0}
  else if(SPP=='RS' | SPP=='WS' | SPP=='BS'){
    b0=7.12037369; b1=-0.05913447;  b2=0.0; b3=0.0; b4=-0.0794222; b5=0.0; b6=0.0}
  else if(SPP=='SM'){b0=3.7230771; b1=-0.1036785; b2=1.9430865; b3=0.0; b4=-0.1095697; b5=0; b6=0}
  else if(SPP=='WC'){b0=10.5218693; b1=0.1672402; b2=0.0; b3=0.0; b4=0.0; b5=-1.6905914; b6=0.0}
  else if(SPP=='WP'){b0=4.31162020; b1=-0.02420289; b2=2.02740873; b3=0.0; b4=0.0; b5=0.0; b6=-0.29743945}
  else if(SPP=='YB'){b0=8.99944682; b1=-0.06072439; b2=0.0; b3=0.0; b4=-0.13594896; b5=0.0; b6=0.0}
  else if(TYPE=='HW' & SPP!='AB' & SPP!='BA' & SPP!='RM' & SPP!='QA' & SPP!='RO' & SPP!='SM' & SPP!='WA' & SPP!='YB'){
    b0=-0.75288849; b1=-0.07091225; b2=2.58666337; b3=0.0; b4=0.0; b5=0.0; b6=-0.10758300}
  else if(TYPE=='SW' & SPP!='EH' & SPP!='WC' & SPP!='WP' & SPP!='BF' & SPP!='RS' & SPP!='WS' & SPP!='BS' & SPP!='RN'){
    b0=15.154381; b1=-0.271174; b2=2.428250; b3=0.0; b4=-0.275259; b5=0.0; b6=0.0}
  logit = b0+b1*(DBH)+b2*log(DBH)+b3*(CR)+b4*(BAL)+b5*sqrt(BA)+b6*CSI
  PS =  (1/(1+exp(-logit)))
  return(PS=PS)}
  
#Mortality function 2 (= survival function using optimal cutpoint!!!)
SUNY.survOCP=function(SPP,TYPE,DBH,CR,BAL,BA,CSI){
  if(SPP=='AB'){b0=7.40690341; b1=-0.08553107; b2=0.0;  b3=2.96492185; b4=-0.10413708; b5=0.0; b6=0.0; cutpoint=.8028}
  else if(SPP=='BA' | SPP=='WA'){
    b0=-2.69723249; b1=-0.05109722; b2=2.72197651; b3=3.18682793; b4=0.0; b5=-0.26771991; b6=0.0; cutpoint=.89}
  else if(SPP=='BF'){b0=5.43210492; b1=-0.06175465; b2=0.0; b3=0.0; b4=-0.03827896; b5=0.0; b6=-0.0; cutpoint=.748}
  else if(SPP=='EH'){b0=-1.98966722; b1=-0.08091288;  b2=3.10196186; b3=0.0; b4=0; b5=0; b6=0; cutpoint=.9584}
  else if(SPP=='QA'){b0=0.28105972; b1=-0.05167895; b2=1.76260960; b3=0.0; b4=0.0; b5=-0.21035163; b6=0.0; cutpoint=.8296}
  else if(SPP=='RM'){b0=11.2896134; b1=-0.1113001; b2=0.0; b3=0.0; b4=-0.1479613; b5=0.0; b6=0.0; cutpoint=.8906}
  else if(SPP=='RN'){b0=8.9149452;  b1=0.2445655; b2=0.0; b3=0.0; b4=0.0; b5=0.0; b6=-0.5345674; cutpoint=.9365}
  else if(SPP=='RO'){b0=-6.20209544; b1=-0.08888149;  b2=4.55453873; b3=0.0; b4=0.0; b5=0.0; b6=0.0; cutpoint=.9757}
  else if(SPP=='RS' | SPP=='WS' | SPP=='BS'){
    b0=7.12037369; b1=-0.05913447;  b2=0.0; b3=0.0; b4=-0.0794222; b5=0.0; b6=0.0; cutpoint=.8911}
  else if(SPP=='SM'){b0=3.7230771; b1=-0.1036785; b2=1.9430865; b3=0.0; b4=-0.1095697; b5=0; b6=0; cutpoint=.9314}
  else if(SPP=='WC'){b0=10.5218693; b1=0.1672402; b2=0.0; b3=0.0; b4=0.0; b5=-1.6905914; b6=0.0; cutpoint=.9541}
  else if(SPP=='WP'){b0=4.31162020; b1=-0.02420289; b2=2.02740873; b3=0.0; b4=0.0; b5=0.0; b6=-0.29743945; cutpoint=.9212}
  else if(SPP=='YB'){b0=8.99944682; b1=-0.06072439; b2=0.0; b3=0.0; b4=-0.13594896; b5=0.0; b6=0.0; cutpoint=.9081}
  else if(TYPE=='HW' & SPP!='AB' & SPP!='BA' & SPP!='RM' & SPP!='QA' & SPP!='RO' & SPP!='SM' & SPP!='WA' & SPP!='YB'){
    b0=-0.75288849; b1=-0.07091225; b2=2.58666337; b3=0.0; b4=0.0; b5=0.0; b6=-0.10758300; cutpoint=.8760}
  else if(TYPE=='SW' & SPP!='EH' & SPP!='WC' & SPP!='WP' & SPP!='BF' & SPP!='RS' & SPP!='WS' & SPP!='BS' & SPP!='RN'){
    b0=15.154381; b1=-0.271174; b2=2.428250; b3=0.0; b4=-0.275259; b5=0.0; b6=0.0; cutpoint=.8718}
  logit = b0+b1*(DBH)+b2*log(DBH)+b3*(CR)+b4*(BAL)+b5*sqrt(BA)+b6*CSI
  PS =  (1/(1+exp(-logit)))
  #PS=ifelse(PS>cutpoint,1,0)
  return(PS=PS)}


##INGROWTH FUNCTION of Li et al. (2011; CJFR 41, 2077-2089)
#PARMS is GNLS (generalized least squares) or NLME (mixed effects), CutPoint is the probability threshold where ingrowth will occur
#BA is total basal area,TPH is trees per ha, PHW is percent hardwood basal area, MinDBH is the minimum threshold diameter (cm)
#ClimateSI is the climate site index (m)
Ingrowth.FUN=function(PARMS,CutPoint,BA,TPH,QMD,PHW,MinDBH,ClimateSI,cyclen=1)
{
  if(PARMS=="GNLS"){
    a0=-0.2116#-4.7867
    a1=-0.0255#0.0469
    a2=-0.1396#-0.8623
    a3=-0.0054#2.3E-5
    a4=0.0433#0.1541
    a5=0.0409#0.1508
    b0=3.8982#3.9018
    b1=-0.0257#-0.0257
    b2=-0.3668#-0.3694
    b3=0.0002#0.0002
    b4=0.0216#0.0216
    b5=-0.0514}#-0.0516}
  else if(PARMS=='NLME'){
    #b0=4.5826   #0.09524  19E3    48.12    <.0001    0.05    4.3959    4.7693  0.011332
    #b1=-0.03999 # 0.000716  19E3   -55.87    <.0001    0.05  -0.04140  -0.03859  0.303946
    #b2=-0.3081  # 0.03063  19E3   -10.06    <.0001    0.05   -0.3682   -0.2481  0.003914
    #b3=0.08342  #0.003296  19E3    25.31    <.0001    0.05   0.07696   0.08987  0.057555
    #a0=-1.0163  #  0.2086  19E3    -4.87    <.0001    0.05   -1.4251   -0.6075   -0.0083
    #a1=0.05704  #0.002310  19E3    24.69    <.0001    0.05   0.05251   0.06156  -0.25297
    #a2=-0.7071  # 0.08782  19E3    -8.05    <.0001    0.05   -0.8792   -0.5350  -0.00292
    #a3=-0.8042  # 0.02864  19E3   -28.08    <.0001    0.05   -0.8604   -0.7481  -0.01515
    #a4=-0.07626 #  0.01410  19E3    -5.41    <.0001    0.05   -0.1039  -0.04863  -0.11038
    #b4=0.08006  #0.006184  19E3    12.95    <.0001    0.05   0.06794   0.09219  0.138189
    #a5=-0.8011 #  0.01315  19E3   -60.92    <.0001    0.05   -0.8268   -0.7753  -0.01166
    #b5=-0.1584 }# 0.004663  19E3   -33.97    <.0001    0.05   -0.1675   -0.1493  0.037681
    #a0=-0.1596#-4.7314
    #a1=0.0253#-0.0467
    #a2=-0.1241#-0.8273
    #a3=-0.0583#-1.8E-5
    #a4=0.0419#0.1521
    #a5=0.0393#0.1501
    #b0=4.0303#4.0240
    #b1=-0.0277#-0.0277
    #b2=-0.3654#-0.3669
    #b3=0.0002#0.0002
    #b4=0.0159#0.0162
    #b5=-0.0642}#-0.0636}
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
    a6 =         -0.1301} # 0.009078  19E3   -14.33    <.0001    0.05   -0.1479   -0.1123  115.6522
    link1 = a0+a1*BA+a2*PHW+a3*(TPH/1000)+a4*ClimateSI+a5*(MinDBH)+a6*QMD #+a6*log(BA+0.1)#+a6*QMD*BA
    PI  = (1/(1+exp(-link1)))
    eta   = b0+b1*BA+ b2*PHW+b3*(TPH/1000)+b4*ClimateSI+b5*(MinDBH)+b6*QMD #+b6*log(BA+0.1)#+b6*QMD*BA
    IPH	= exp(eta)
    # add scaling by cyclen by Crookston Oct 2020
    if (cyclen>1) 
    {
      PI = 1-((1-PI)^cyclen)
      IPH = IPH*cyclen
    }
    IPH = ifelse(PI>=CutPoint,IPH,0)
    return(list(IPH=IPH))
}
     

Ingrowth.Comp=function(SPP,BA,PBA,ClimateSI,MinDBH){
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0.11394943}
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
    b7_tap=0.371113950891855}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0.039630786}
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
    b7_tap=0.396398949039286}
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
    b7_tap=0}
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
    b7_tap=0.169725200257675}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0.15192029}
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
    b7_tap=0.104842994095004}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0    }
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0.07830011}
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
    b7_tap=0.096121529684134}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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
    b7_tap=0}
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

KozakTreeVol=function(Bark,SPP,DBH,HT,Planted,stump=NA,topHT=NA,topD=NA){
    sgmts = 100
    stump=ifelse(is.na(stump),as.numeric(0.0),stump)
    topHT=ifelse(is.na(topHT),as.numeric(HT),topHT)
    topHT=ifelse(topHT>HT,as.numeric(HT),topHT)
    topD=(ifelse(is.na(topD),as.numeric(0.001),topD))
    L = (topHT - stump)/sgmts
    i = 0
    treeVolume = 0
    while (i < sgmts) {
        H1 = L * i + stump
        H2 = L * (i + 1) + stump
        if (HT - H1 < 1e-04){
            dob1 = 0
            dib1 = 0}
        else {
            if (H1 == 0)
                H1 = 0.001
            Esty1 = KozakTaper(Bark='ob',SPP=SPP,DHT=H1,DBH=DBH,HT=HT,Planted=Planted)
            dob1 = as.numeric(Esty1)
            dob1 = ifelse(dob1<topD,0,dob1)
            dib1 = DOBtoDIB(SPP=SPP,dob=dob1)
            #dib1= dob1
        }
        if (HT - H2 < 1e-04){
            dob2 = 0
            dib2 = 0}
        else {
            if (H2 == 0)
                H2 = 0.001
            Esty2 = KozakTaper(Bark='ob',SPP=SPP,DHT=H2,DBH=DBH,HT=HT,Planted=Planted)
            dob2 = as.numeric(Esty2)
            dob2 = ifelse(dob2<topD,0,dob2)
            dib2 = DOBtoDIB(SPP=SPP,dob=dob2)
            #dib2= dob2
        }
        treeVolume <- ifelse(Bark=='ob',treeVolume + smalians(dob1, dob2, L * 100),
          treeVolume + smalians(dib1, dib2, L * 100))
        i <- i + 1
    }
    treeVolume <- round(treeVolume/1e+06, 6)
    return(treeVolume=treeVolume) }
    
###################################################################################
## Westfall & Scott 2010 Forest Science (56, 515-582)  FIA taper equations       #
##################################################################################
WestfallScott	<-	function(SPP,h,H,dbh){
  if(SPP=='SM'){         #sugar maple
    th1	=	6.5790
  	th2	=	0.0111
	  a1		=	1.0682
    a2		=	1.1833
    gm1	=	0.1031
	   gm2	=	0.2624
	   phi	=	0.1516
	   lmd	=	1.1482
	   bet1	=	0.6637
	   bet2	=	3.0996
     }
   else if(SPP=='YB' | SPP=='PB' | SPP=='GB' | SPP=='SB'){ #birches
    th1=	7.5437
  	th2	=	0.0103
	  a1 =	0.9961
	  a2		=	1.1042
    gm1	=	0.1313
	  gm2	=	0.3539
    phi	=	0.2091
	  lmd	=	0.9478
	  bet1	=	0.5995
	  bet2	=	3.4205}
  else if(SPP=='RM'){ #red maple
   	th1	=	7.5707
	  th2	=	0.0105
	  a1		=	1.5273
	  a2		=	0.7684
	  gm1	=	0.0931
	  gm2	=	0.4223
	  phi	=	0.1441
	  lmd	=	1.3910
	  bet1	=	0.6453
	  bet2	=	4.0737
    }
  else if(SPP=='GA' | SPP=='WA' | SPP=='BT' | SPP=='QA' | SPP=='BP'){ #ash, quaking aspen, balsam poplar
 	  th1	=	3.3085
  	th2	=	0.0276
	  a1		=	1.2634
	  a2		=	0.9088
	  gm1	=	0.1098
	  gm2	=	0.5198
	phi	=	0.1840
	lmd	=	1.7842
	bet1	=	0.6719
	bet2	=	5.1178 }
	else if(SPP=='AB'){ #American beech
	   th1	=	8.9843
  	th2	=	0.0107
	  a1		=	0.7621
	  a2		=	1.3734
	  gm1	=	0.0956
	  gm2	=	0.1650
	 phi	=	0.1924
	 lmd	=	1.2237
	bet1	=	0.4626
	bet2	=	1.0954 }
	else if(SPP=='RO'){ #red oak
	   th1	=	12.8336
  	th2	=	0.0125
	  a1		=	0.9038
	  a2		=	1.0950
	  gm1	=	0.0935
	  gm2	=	0.3971
	 phi	=	0.2038
	 lmd	=	1.0457
	bet1	=	0.5508
	bet2	=	3.4681}
else if(SPP=='BC'){           #black cherry
	   th1	=	3.2042
  	th2	=	0.0479
	  a1		=	1.2507
	  a2		=	0.8075
	  gm1	=	0.0800
	  gm2	=	0.4170
	 phi	=	0.2227
	 lmd	=	2.7226
	bet1	=	0.7065
	bet2	=	4.6476}
else if(SPP=='BF'){                        #balsam fir
	   th1	=	5.3693
  	th2	=	0.0171
	  a1		=	1.4212
	  a2		=	0.3003
	  gm1	=	0.0890
	  gm2	=	0.6485
	 phi	=	0.1916
	 lmd	=	1.8873
	bet1	=	0.4764
	bet2	=	2.6383}
else if(SPP=='RS' | SPP=='BS' | SPP=='WS'){           #spruces
	   th1	=	6.8745
  	th2	=	0.0110
	  a1		=	1.1241
	  a2		=	0.4107
	  gm1	=	0.1376
	  gm2	=	0.4842
	 phi	=	0.2038
	 lmd	=	1.2598
	bet1	=	0.4986
	bet2	=	2.7865}
else if(SPP=='WP'){ #white pine
	   th1	=	7.1438
  	th2	=	0.0123
	  a1		=	0.8978
	  a2		=	0.7872
	  gm1	=	0.0989
	  gm2	=	0.4985
	 phi	=	0.2049
	 lmd	=	0.9247
	bet1	=	0.5715
	bet2	=	2.0482}
else if(SPP=='TA' || SPP=='NS' || SPP=='JP'){ #larch, Norway spruce, jack pine
	   th1	=	5.2913
  	th2	=	0.0411
	  a1		=	1.1291
	  a2		=	0.6831
	  gm1	=	0.0745
	  gm2	=	0.5798
	 phi	=	0.1896
	 lmd	=	1.5776
	bet1	=	0.6616
	bet2	=	6.0645}
else if(SPP=='WC'){ #northern white cedar
	   th1	=	5.400
  	th2	=	0.0256
	  a1		=	1.9295
	  a2		=	0.8142
	  gm1	=	0.0943
	  gm2	=	0.9642
	 phi	=	0.2761
	 lmd	=	1.8605
	bet1	=	1.3432
	bet2	=	1.3438}
else if(SPP=='RP'){ #red pine
	   th1	=	7.6044
  	th2	=	0.0148
	  a1		=	1.2379
	  a2		=	0.3304
	  gm1	=	0.0759
	  gm2	=	0.6611
	 phi	=	0.3008
	 lmd	=	1.1569
	bet1	=	0.5462
	bet2	=	3.0627}
else if(SPP=='EH'){ #eastern hemlock
	   th1	=	7.2442
  	th2	=	0.0152
	  a1		=	1.4008
	  a2		=	0.8306
	  gm1	=	0.0856
	  gm2	=	0.4724
	 phi	=	0.2011
	 lmd	=	1.5776
	bet1	=	0.6616
	bet2	=	6.0645}
else if(SPP=='OH'){
    th1	=	9.0505
  	th2	=	0.0241
	  a1		=	1.298
	  a2		=	0.7684
	  gm1	=	0.0684
	  gm2	=	0.4555
	 phi	=	0.1769
	 lmd	=	1.6684
	bet1	=	0.5408
	bet2	=	4.1821}
else if(SPP=='OS'){ #composite of all conifers
    th1	=	6.418214286
  	th2	=	0.019585714
	  a1		=	1.305771429
	  a2		=	0.593785714
	  gm1	=	0.093685714
	  gm2	=	0.615528571
	 phi	=	0.223985714
	 lmd	=	1.462628571
	bet1	=	0.685271429
	bet2	=	2.842342857}  
else{ #composite of all species
    th1	=	7.114957895
  	th2	=	0.016836842
	  a1		=	1.148384211
	  a2		=	0.857194737
	  gm1	=	0.101226316
	  gm2	=	0.460905263
	 phi	=	0.195610526
	 lmd	=	1.351415789
	bet1	=	0.634036842
	bet2	=	3.302584211}
  x=dbh/H
	z=h/H
	S1=th1/(1+(z/th2)^lmd)
	S2=(z/bet1)^(bet2*x)/(1+(z/bet1)^(bet2*x))
	d=sqrt(dbh^2*(1.37/H/gm1)^phi*((1-z)/(1-gm1))^(a1+S1)*((1-z)/(1-gm2))^(a2*S2))
	return (d)
	}
	
Vol.WestFall <- function (SPP,HT,DBHO,stump=NA,topHT=NA,topD=NA){ ## stump, and top are two points, we integrate between them
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
 
 
Honer.Vol=function(SPP,HT,DBHO,topD=NA,topHT=NA){
  if(SPP=='WP'){
    a=0.691
    b=363.676
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='RP'){
    a=0.710
    b=355.623
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='JP' | SPP=='TA'){
    a=0.897
    b=348.520
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='LP'){
    a=0.694
    b=343.896
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='BS'){
    a=1.588
    b=333.364
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='RS'){
    a=1.226
    b=315.832
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='WS'){
    a=1.440
    b=342.175
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='BF'){
    a=2.139
    b=301.634
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
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
    p3.d=-0.7736}
  else if(SPP=='QA'){
    a=-0.312
    b=436.683
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='BP' | SPP=='BT'){
    a=0.420
    b=394.644
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='PB' | SPP=='GB' | SPP=='SB'  ){
    a=2.222
    b=300.373
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='YB'){
    a=1.449
    b=344.754
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='RM' | SPP=='SM'){
    a=1.046
    b=383.972
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='BW'){
    a=0.948
    b=401.456
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='AB' | SPP=='WA'){
    a=0.959
    b=334.829
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='BC'){
    a=0.033
    b=393.336
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='RO'){
    a=1.512
    b=336.509
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  else if(SPP=='OH'){
     a=1.512
    b=336.509
    b2=0.184
    p1.ht=0.0145
    p2.ht=2.1164
    p3.ht=-1.1387
    p1.d=1.0180
    p2.d=-0.2323
    p3.d=-0.7736}
  DBHin=DBHO/2.54
  HTft=HT/.3048
  Vtcf=DBHin^2/(a+(b/HTft))
  Vtm3=Vtcf* 0.02831685
  MR.ht=topHT/HT
  MR.dib=(topD^2/DBHO^2)*(1-0.04365*b2)^-2
  return(Vtm3)}	
  
Summary.GY=function(tree){
  library(nlme)
  tree$ba<-round((tree$DBH^2*0.00007854)*tree$EXPF,2)
  tree$sdi=(tree$DBH/25.4)^1.605*tree$EXPF
  tree$ba.WP=ifelse(tree$SP=='WP',tree$ba,0)
  tree$ba.BF=ifelse(tree$SP=='BF',tree$ba,0)
  tree$ba.RM=ifelse(tree$SP=='RM',tree$ba,0)
  tree$ba.RS=ifelse(tree$SP=='RS',tree$ba,0)
  tree$ba.BS=ifelse(tree$SP=='BS',tree$ba,0)
  tree$ba.WS=ifelse(tree$SP=='WS',tree$ba,0)
  tree$ba.PB=ifelse(tree$SP=='PB',tree$ba,0)
  tree$ba.YB=ifelse(tree$SP=='YB',tree$ba,0)
  tree$ba.GB=ifelse(tree$SP=='YB',tree$ba,0)
  tree$CR=((tree$HT-tree$HCB)/tree$HT)*tree$EXPF
  tree$HT=tree$HT*tree$EXPF
  temp <- subset(tree,select=c("YEAR","STAND","PLOT",'TREE','DBH','HT','CR','EXPF',"ba",'ba.WP','ba.BF','ba.RM','ba.RS','ba.BS','ba.PB','ba.YB','ba.GB','ba.WS','sdi'))
  temp<-groupedData(ba~ba|YEAR/STAND/PLOT,data=temp)
  temp <- gsummary(temp,sum)
  temp$BA<-temp$ba
  temp$BAPH<-temp$ba
  temp$tph<-temp$EXPF 
  temp$qmd<-sqrt(temp$BAPH/(0.00007854*temp$tph))
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
  temp=subset(temp,select=c("YEAR","STAND","PLOT",'BA','tph','qmd','sdi','Avg.HT','Avg.LCR','pWP.ba','pBF.ba','pRM.ba','pRS.ba','pBS.ba','pWS.ba','pPB.ba','pYB.ba','pGB.ba'))
  temp
}

###Adirondack growth and yield model (not called by FVS)
AdirondackGY=function(tree,stand,ops=NULL)
{
  ops=as.list(ops)
  ans = ddply(tree,.(STAND), function (x,stand,ops) 
    {
      stand = as.list(subset(stand,STAND == x[1,"STAND"]))
      AdirondackGYOneStand(tree,stand=stand,ops=ops)
    }, stand, ops)                
  tree<-sort.data.frame(ans,~+YEAR+STAND+PLOT+TREE)
  tree
}

# this is the call used by FVS
AdirondackGYOneStand=function(tree,stand,ops)
{
  verbose  = if (is.null(ops$verbose))   FALSE        else ops$verbose
  cyclen   = if (is.null(ops$cyclen))    1            else ops$cyclen
  INGROWTH = if (is.null(ops$INGROWTH))  "Y"          else ops$INGROWTH
  MinDBH   = if (is.null(ops$MinDBH))    10           else ops$MinDBH
  CutPoint = if (is.null(ops$CutPoint))  0.5          else ops$CutPoint
  mortModel= if (is.null(ops$mortModel)) "Adirondack" else ops$mortModel
  CSI      = if (is.null(stand$CSI))    12            else stand$CSI

  if (verbose) cat ("in AdirondackGY nrow(tree)=",nrow(tree)," CSI=",CSI,"cyclen=",
     cyclen,"\nINGROWTH=",INGROWTH," MinDBH=",MinDBH," CutPoint=",
      CutPoint," mortModel=",mortModel,"\n") 
  if (verbose && exists("AdirondackGYVersionTag")) 
    cat("AdirondackGYVersionTag=",AdirondackGYVersionTag,"\n")

  aa=sapply(tree$SP,SPP.func)
  tree$SPtype=t(aa)[,1]
  tree$shade=as.numeric(t(aa)[,2])

  tree$ba<-(tree$DBH^2*0.00007854)*tree$EXPF
  tree$ba.SW<-ifelse(tree$SPtype=='SW',tree$ba,0)
  tree$ba.WP=ifelse(tree$SP=='WP',tree$ba,0)
  tree$ba.BF=ifelse(tree$SP=='BF',tree$ba,0)
  tree$ba.RM=ifelse(tree$SP=='RM',tree$ba,0)
  tree$ba.SPR=ifelse(tree$SP=='RS' | tree$SP=='WS' | tree$SP=='BS',tree$ba,0)
  tree$ba.BRH=ifelse(tree$SP=='GB' | tree$SP=='PB' | tree$SP=='RB' | 
                     tree$SP=='YB', tree$ba,0)
  tree$ba.OH=ifelse(tree$SPtype=='HW' & tree$ba.RM==0 & tree$ba.BRH==0, 
                    tree$ba, 0)
  tree$ba.OS=ifelse(tree$SPtype=='SW' & tree$ba.WP==0 & tree$ba.BF==0 & 
                    tree$ba.SPR==0,tree$ba,0)
  tree$ba.RS=ifelse(tree$SP=='RS',tree$ba,0)
  tree$ba.BS=ifelse(tree$SP=='BS',tree$ba,0)
  tree$ba.PB=ifelse(tree$SP=='PB',tree$ba,0)
  tree$ba.YB=ifelse(tree$SP=='YB',tree$ba,0)
  temp=subset(tree,select=c('PLOT','TREE','DBH','EXPF',"ba",'ba.SW','ba.WP',
                'ba.BF','ba.RM','ba.SPR','ba.BRH','ba.OH','ba.OS','ba.RS',
                'ba.BS','ba.PB','ba.YB'))

  temp = ddply(temp,.(PLOT),
    function(x)
    {
      rtn = as.data.frame(t(colSums(x[,c(-1,-2)])))
      rtn$PLOT = x$PLOT[1]
      rtn$maxTREE = max(x$TREE)
      rtn
    }) 
                
  temp$BAPH<-temp$ba
  temp$tph<-temp$EXPF 
  temp$qmd<-sqrt(temp$BAPH/(0.00007854*temp$tph))
  temp$pHW.ba=1-(temp$ba.SW/temp$BAPH)
  temp$pWP.ba=temp$ba.WP/temp$BAPH
  temp$pBF.ba=temp$ba.BF/temp$BAPH
  temp$pRM.ba=temp$ba.RM/temp$BAPH
  temp$pSPR.ba=temp$ba.SPR/temp$BAPH
  temp$pBRH.ba=temp$ba.BRH/temp$BAPH
  temp$pOH.ba=temp$ba.OH/temp$BAPH
  temp$pOS.ba=temp$ba.OS/temp$BAPH
  temp$pRS.ba=ifelse(temp$pSPR.ba==0,0,temp$ba.RS/temp$ba.SPR)
  temp$pBS.ba=ifelse(temp$pSPR.ba==0,0,temp$ba.BS/temp$ba.SPR)
  temp$pWS.ba=ifelse(temp$pSPR.ba==0,0,1-(temp$pRS.ba+temp$pBS.ba))
  temp$pPB.ba=ifelse(temp$pBRH==0,0,temp$ba.PB/temp$ba.BRH)
  temp$pYB.ba=ifelse(temp$pBRH==0,0,temp$ba.YB/temp$ba.BRH)
  temp$pGB.ba=ifelse(temp$pBRH==0,0,1-(temp$pPB.ba+temp$pYB.ba))
  temp=subset(temp,select=c("PLOT",'BAPH','maxTREE','tph','qmd','pHW.ba',
    'pWP.ba','pBF.ba','pRM.ba','pSPR.ba','pBRH.ba','pOH.ba','pOS.ba',
    'pRS.ba','pBS.ba','pWS.ba','pPB.ba','pYB.ba','pGB.ba'))
  Sum.temp=temp
  
  if (verbose) cat ("at Sum.temp\n") 

  temp$maxTREE = NULL
  tree=merge(tree,temp,by="PLOT")
  
  #Compute basal area in larger trees
  tree<-sort.data.frame(tree,~+PLOT-DBH)
  temp = unlist(by(tree$ba,INDICES=tree$PLOT,FUN=cumsum))
  tree$BAL = temp-tree$ba
  temp = unlist(by(tree$ba.SW,INDICES=tree$PLOT,FUN=cumsum))
  tree$BAL.SW = temp-tree$ba.SW
  tree$BAL.HW = tree$BAL-tree$BAL.SW

  #compute tree-level crown width-related metrics
  tree$mcw=mapply(mcw,sp=tree$SP,dbh=tree$DBH)
  tree$lcw=mapply(lcw,sp=tree$SP,mcw=tree$mcw,dbh=tree$DBH)
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
  if (any(is.na(tree$HT))) 
  {
    pHT.m=mapply(SUNY.HT,SPP=tree$SP,TYPE=tree$SPtype,DBH=tree$DBH,
                         BAL=tree$BAL,BA=tree$BAPH)
    tree$HT=ifelse(is.na(tree$HT),pHT.m,tree$HT)
  }

  #calculate crown ratio if it is missing or if any are missing
  #see comment about missing heights
  if (is.null(tree$CR) || any(is.na(tree$CR)))
  {
    if (is.null(tree$CR)) tree$CR = NA
    # compute crown ratio from base height, compute if missing
    if (is.null(tree$HCB) || any(is.na(tree$HCB)))
    {
      if (is.null(tree$HCB)) tree$HCB = NA
      # Height to crown base
      pHCB=mapply(SUNY.HCB,SPP=tree$SP,TYPE=tree$SPtype,DBH=tree$DBH,
                    HT=tree$HT,BAL=tree$BAL,BA=tree$BAPH,CSI=CSI)
      tree$HCB=ifelse(is.na(tree$HCB),pHCB,tree$HCB)
    }
    tree$CR=ifelse(is.na(tree$CR),1-(tree$HCB/tree$HT),tree$CR)
  }
  
  if (verbose) cat ("at growth\n") 

  #Diameter increment model
  tree$dDBH=mapply(SUNY.dDBH,SP=tree$SP,TYPE=tree$SPtype,DBH=tree$DBH,
            BAL=tree$BAL,BA=tree$BAPH,CSI=CSI)

  #Height increment 
  tree$dHT=mapply(SUNY.dHT,SPP=tree$SP,TYPE=tree$SPtype,DBH=tree$DBH,
           HT=tree$HT,BAL=tree$BAL,BA=tree$BAPH)

  if (verbose) cat ("at Mortality, mortModel=",mortModel,"\n") 
  #Mortality
  if (mortModel == "Adirondack") 
  {
    ps = mapply(SUNY.survOCP,SPP=tree$SP,TYPE=tree$SPtype,DBH=tree$DBH,
                CR=tree$CR,BAL=tree$BAL,CSI=CSI,BA=tree$BAPH)
    # convert to a periodic probability of mortality.
    pm = 1-(ps^cyclen)
    tree$dEXPF = tree$EXPF * pm
  } else {
    #setting dEXPF to NULL will result in the base model mortality being used
    tree$dEXPF = NULL
  }
  if (verbose) cat ("sum dEXPF=",sum(tree$dEXPF),"\n")
    
  #Crown base height...need this to be annual change: dHCB...computed here
  #in a difference equation format. 
  tree$HCB = mapply(SUNY.BHT,SPP=tree$SP,TYPE=tree$SPtype,
             tree$DBH+tree$dDBH,HT=tree$HT+tree$dHT,BAL=tree$BAL,
             CSI=CSI,BA=tree$BAPH)

  tree$dHCB= mapply(SUNY.BHT,SPP=tree$SP,TYPE=tree$SPtype,
             tree$DBH+tree$dDBH,HT=tree$HT+tree$dHT,BAL=tree$BAL,
             CSI=CSI,BA=tree$BAPH)-tree$HCB

  
  ##INGROWTH
  ingrow = NULL
  if (toupper(substr(INGROWTH,1,1)) == "Y")
  {
		Sum.temp$IPH=as.numeric(mapply(Ingrowth.FUN,PARMS='NLME',CutPoint=CutPoint,
       BA=Sum.temp$BAPH,TPH=Sum.temp$tph,QMD=Sum.temp$qmd,PHW=Sum.temp$pHW.ba,
       MinDBH=MinDBH,ClimateSI=CSI,cyclen=cyclen))
     
    Sum.temp$pBCH.ING=mapply(Ingrowth.Comp,SPP='BCH',BA=Sum.temp$BAPH,
        PBA=Sum.temp$pBRH.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pBF.ING=mapply(Ingrowth.Comp,SPP='BF',BA=Sum.temp$BAPH,
        PBA=Sum.temp$pBF.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pRM.ING=mapply(Ingrowth.Comp,SPP='RM',BA=Sum.temp$BAPH,
        PBA=Sum.temp$pRM.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pSPR.ING=mapply(Ingrowth.Comp,SPP='SPR',BA=Sum.temp$BAPH,
        PBA=Sum.temp$pSPR.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pWP.ING=mapply(Ingrowth.Comp,SPP='WP',BA=Sum.temp$BAPH,
        PBA=Sum.temp$pWP.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pOH.ING=mapply(Ingrowth.Comp,SPP='OH',BA=Sum.temp$BAPH,
        PBA=Sum.temp$pOH.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pOS.ING=mapply(Ingrowth.Comp,SPP='OS',BA=Sum.temp$BAPH,
        PBA=Sum.temp$pOS.ba,ClimateSI=CSI,MinDBH=MinDBH)
    Sum.temp$pSP=Sum.temp$pBCH.ING+Sum.temp$pBF.ING+Sum.temp$pRM.ING+
                 Sum.temp$pSPR.ING+Sum.temp$pWP.ING+Sum.temp$pOH.ING+
                 Sum.temp$pOS.ING
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
  }
  
  #Scale the growth to the number of years in the period. 
  if (cyclen>1)
  {
    tree$dDBH=tree$dDBH*cyclen
    tree$dHT =tree$dHT *cyclen
    tree$dHCB=tree$dHCB*cyclen
  }

  # put the PLOT variable back to a character string (defactor it).
  if (is.factor(tree$PLOT)) tree$PLOT = 
     levels(tree$PLOT)[as.numeric(tree$PLOT)]

  # restore the order of the trees
  tree = tree[order(tree$id),]

  if (verbose) cat ("at return, nrow(tree)=",nrow(tree)," ingrow=",
       if (is.null(ingrow)) "NULL" else nrow(ingrow),"\n")
       
  # return the original trees and the ingrowth separately
  list(tree=tree,ingrow=ingrow)
}

    
    
	
  