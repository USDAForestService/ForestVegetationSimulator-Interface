
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
SPP.func=function(SPP)
{            
  if(SPP=='AB'){ #American beech
    SPtype='HW'
    sg=0.64
    wd=0.56
    shade=4.75
    drought=1.5
    waterlog=1.5
  }
  else if(SPP=='AS'){ #ash
    SPtype='HW'
    sg=0.57
    wd=0.51
    shade=2.84
    drought=2.74
    waterlog=3.02
  }
  else if(SPP=='BA'){ #black ash
    SPtype='HW'
    sg=0.49
    wd=0.45
    shade=2.96
    drought=2
    waterlog=3.5
    sg=0.5
  }
  else if(SPP=='BC'){ #black cherry
    SPtype='HW'
    sg=0.5
    wd=0.47
    shade=2.46
    drought=3.02
    waterlog=1.06
  }
  else if(SPP=='BF'){ #balsam fir
    SPtype='SW'
    sg=0.35
    wd=0.33
    shade=5.01
    drought=1
    waterlog=2
  }
  else if(SPP=='BP'){ #balsam poplar
    SPtype='HW'
    sg=0.34
    wd=0.31
    shade=1.27
    drought=1.77
    waterlog=2.63
  }
  else if(SPP=='BS'){ #black spruce
    SPtype='SW'
    sg=0.46
    wd=0.38
    shade=4.08
    drought=2.0
    waterlog=2.0
  }
  else if(SPP=='BT'){ #bigtooth aspen
    SPtype='HW'
    sg=0.39
    wd=0.36
    shade=1.21
    drought=2.5
    waterlog=2
  }
  else if(SPP=='EC'){ #eastern cottonwood
    SPtype='HW'
    sg=0.4
    wd=0.37
    shade=1.76
    drought=1.57
    waterlog=3.03
  }
  else if(SPP=='EH'){ #eastern hemlock
    SPtype='SW'
    sg=0.4
    wd=0.38
    shade=4.83
    drought=1
    waterlog=1.25
  }
  else if(SPP=='GA'){ #green ash
    SPtype='HW'
    sg=0.56
    wd=0.53
    shade=3.11
    drought=3.85
    waterlog=2.98
  }
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
    waterlog=1.07
  }
  else if(SPP=='JP'){ #jack pine
    SPtype='SW'
    sg=0.43
    wd=0.4
    shade=1.36
    drought=4
    waterlog=1
  }
  else if(SPP=='NS'){ #Norway spruce
    SPtype='SW'
    sg=0.43
    wd=0.37023
    shade=4.45
    drought=1.75
    waterlog=1.22
  }    
  else if(SPP=='OH'){ #other hardwoods
    SPtype='HW'
    sg=0.5121
    wd=0
    shade=2.29
    drought= 0
    waterlog=0
  }
  else if(SPP=='OS'){ #other softwoods
    SPtype='SW'
    sg=0.445
    wd=0
    shade=2.27
    drought=0
    waterlog=0
  }
  else if(SPP=='PB'){ #paper birch
    SPtype='SW'
    sg=0.55
    wd=0.48
    shade=1.54
    drought=2.02
    waterlog=1.25
  }
  else if(SPP=='PC' | SPP=='PR'){ #pin cherry 
    SPtype='HW'
    sg=0.38
    wd=0.36
    shade=2.26
    drought=0
    waterlog=0
  }
  else if(SPP=='QA'){ #quaking aspen
    SPtype='HW'
    sg=0.38
    wd=0.35
    shade=1.21
    drought=1.77
    waterlog=1.77
  }
  else if(SPP=='RB'){ #river birch
    SPtype='HW'
    sg=0.62
    wd=0.49
    shade=1.45
    drought=1.53
    waterlog=2.85
  }
  else if(SPP=='RM'){ #red maple
    SPtype='HW'
    sg=0.54
    wd=0.49
    shade=3.44
    drought=1.84
    waterlog=3.08
  }
  else if(SPP=='RP' | SPP=='RN'){ #red pine
    SPtype='SW'
    sg=0.46
    wd=0.41
    shade=1.89
    drought=3
    waterlog=1
  }
  else if(SPP=='RO'){ #red oak
    SPtype='HW'
    sg=0.63
    wd=0.56
    shade=2.75
    drought=2.88
    waterlog=1.12
  }
  else if(SPP=='RS'){ #red spruce
    SPtype='SW'
    sg=0.4
    wd=0.37
    shade=4.39
    drought=2.5
    waterlog=2
  }
  else if(SPP=='SB'){ #Sweet birch
    SPtype='HW'
    sg=0.65
    wd=0.6
    shade=2.58
    drought=3
    waterlog=1
  }
  else if(SPP=='SM'){ #sugar maple
    SPtype='HW'
    sg=0.63
    wd=0.56
    shade=4.76
    drought=2.25
    waterlog=1.09
  }
  else if(SPP=='ST'){#striped maple
    SPtype='HW'
    sg=0.46
    wd=0.44
    shade=3.56
    drought=2
    waterlog=1
  }
  else if(SPP=='TA'){ #larch/tamarack
    SPtype='SW'
    sg=0.53
    wd=0.49
    shade=0.98
    drought=2
    waterlog=3
  }
  else if(SPP=='WA'){ #white ash
    SPtype='HW'
    sg=0.6
    wd=0.55                     
    shade=2.46
    drought=2.38
    waterlog=2.59
  }
  else if(SPP=='WC'){ #northern white cedar
    SPtype='SW'
    sg=0.31
    wd=0.29
    shade=3.45
    drought=2.71
    waterlog=1.46
  }
  else if(SPP=='WP'){ #white pine
    SPtype='SW'
    sg=0.35
    wd=0.34
    shade=3.21
    drought=2.29
    waterlog=1.03
  }
  else if(SPP=='WS'){ #white spruce
    SPtype='SW'
    sg=0.4
    wd=0.33
    shade=4.15
    drought=2.88
    waterlog=1.02
  }
  else if(SPP=='YB'){ #yellow birch
    SPtype='HW'
    sg=0.62
    wd=0.55
    shade=3.17
    drought=3
    waterlog=2
  }
  else{
    SPtype='HW'
    sg=0
    wd=0
#    shade=0
    shade=3
    drought=0
    waterlog=0
  }    
  return(c(SPtype=SPtype,shade=shade,sg=sg))
}

#Maximum crown width
mcw=function(sp,dbh)
{
  if(sp=='BF'){
    a1=1.37; a2=0.572
  }
  else if(sp=='BS'){
    a1=0.535; a2=0.742
  }
  else if(sp=='EH'){
    a1=2.44; a2=0.408
  }
  else if(sp=='WP'){
    a1=1.24; a2=0.585
  }
  else if(sp=='NC'){
    a1=1.63; a2=0.436
  }
  else if(sp=='RS'){
    a1=1.80; a2=0.461
  }
  else if(sp=='WS'){
    a1=1.50; a2=0.496
  }
  else if(sp=='AB'){
    a1=2.93; a2=0.434
  }
  else if(sp=='GB' | sp=='RB'){
    a1=2.24; a2=0.382
  }
  else if(sp=='RO'){
    a1=4.08; a2=0.310
  }
  else if(sp=='PB'){
    a1=1.48; a2=0.623}
  else if(sp=='QA'){
    a1=1.31; a2=0.586
  }
  else if(sp=='RM'){
    a1=2.17; a2= 0.491
  }
  else if(sp=='SM'){
    a1=3.31; a2=0.356
  }
  else if(sp=='YB'){
    a1=4.04; a2=0.308
  }
  else if(sp=='OH'){
    a1=4.04; a2=0.308
  }
  else if(sp=='OS'){
    a1=1.597128571; a2=0.513957143
  }
  else{
    a1=2.24262; a2=0.462653333
  }        
  mcw=a1*dbh**a2
  return(mcw)
}

   
#These are the parm ests for estimating largest crown width (lcw)
lcw=function(sp,mcw,dbh){
  if(sp=='BF'){
    b1=1.49; b2=0.105
  }
  else if(sp=='BS'){
    b1=1; b2=0.174
  }
  else if(sp=='EH'){
    b1=1.90; b2=-0.057}
  else if(sp=='WP'){
    b1=1; b2=0.147
  }
  else if(sp=='NC'){
    b1=2.19; b2=-0.080
  }
  else if(sp=='RS'){
    b1=4.33; b2=-0.264
  }
  else if(sp=='WS'){
    b1=2.09; b2=-0.069
  }
  else if(sp=='AB'){
    b1=1; b2=0.194
  }
  else if(sp=='GB' | sp=='RB'){
    b1=3.10; b2=-0.214
  }
  else if(sp=='RO'){
    b1=4.10; b2=-0.272
  }
  else if(sp=='PB'){
    b1=2.10; b2=-0.035}
  else if(sp=='QA'){
    b1=2.65; b2=0.157
  }
  else if(sp=='RM'){
    b1=2.63; b2=-0.132
  }
  else if(sp=='SM'){
    b1=1; b2=0.161
  }
  else if(sp=='YB'){
    b1=4.23; b2=-0.264
  }
  else if(sp=='OH'){
    b1=2.65; b2=0.157
  }
  else if(sp=='OS'){
    b1=2.3276; b2=0.027842857
  }
  else{
    b1=2.79282; b2=-0.090113333
  }
  lcw=mcw/(b1*dbh**b2)
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

#Diameter increment function (1/19/15)
dDBH.FUN=function(SPP,DBH,CR,BAL.SW,BAL.HW,BA,CSI)
{
  b0= -1.950439# 0.5914989 1051730   -3.29745  0.0010
  b1=  0.777421# 0.1872841 1051730    4.15102  0.0000
  b2= -0.021511# 0.0087402 1051730   -2.46111  0.0139
  b3= -0.156782# 0.0151221 1051730  -10.36773  0.0000
  b4= -1.579314# 0.4728687 1051730   -3.33986  0.0008
  b5=  0.866119# 0.0037943 1051730  228.26538  0.0000
  b6= -0.944536# 0.0053781 1051730 -175.62738  0.0000
  b7= -0.129025# 0.0150487 1051730   -8.57378  0.0000
  b8=  0.881566# 0.0048240 1051730  182.74698  0.0000
  b9= -5.950806# 0.0332362 1051730 -179.04583  0.0000
  if(SPP=='AB'){b0.spp=2.6101226;b1.spp=-0.36562072; b2.spp=-0.0001592093; b3.spp=-0.079550678; b4.spp=2.28102518; b7.spp=-0.002693005}
  else if(SPP=='AE'){b0.spp=2.2673275;b1.spp=-1.03975173; b2.spp=0.039236386; b3.spp=-0.107212368; b4.spp=0.08653865; b7.spp=0.06062087}
  else if(SPP=='AH'){b0.spp=-2.685607;b1.spp=0.52156168; b2.spp=-0.0099819503; b3.spp=0.059605112; b4.spp=-1.28943259; b7.spp=0.021724474}
  else if(SPP=='AL'){b0.spp=-4.8972118;b1.spp=-2.73908067; b2.spp=0.1923205435; b3.spp=0.223840971; b4.spp=-11.65178078; b7.spp=0.154413717}
  else if(SPP=='AP'){b0.spp=-3.9702997;b1.spp=0.46333376; b2.spp=-0.0048827586; b3.spp=-0.07055014; b4.spp=-3.74101057; b7.spp=0.021805997}
  else if(SPP=='BA'){b0.spp=2.9678076;b1.spp=-1.29923394; b2.spp=0.0593468636; b3.spp=0.015393691; b4.spp=1.03024324; b7.spp=0.032913086}
#  else if(SPP=='BC'){b0.spp=3.745958;b1.spp=-1.47596712; b2.spp=0.0694527021; b3.spp=-0.08585319; b4.spp=1.13623176; b7.spp=0.03983049}
  else if(SPP=='BE'){b0.spp=0;b1.spp=0; b2.spp=0; b3.spp=0; b4.spp=0; b7.spp=0}
  else if(SPP=='BF'){b0.spp=0.4722754;b1.spp=0.02650822; b2.spp=-0.0053483078; b3.spp=0.107734816; b4.spp=0.86439574; b7.spp=-0.049215284}
  else if(SPP=='BH'){b0.spp=0;b1.spp=0; b2.spp=0; b3.spp=0; b4.spp=0; b7.spp=0}
  else if(SPP=='BL'){b0.spp=0.5158554;b1.spp=0.27332024; b2.spp=-0.0180669304; b3.spp=-0.013053064; b4.spp=0.77124732; b7.spp=-0.036191969}
  else if(SPP=='BN'){b0.spp=3.5330228;b1.spp=0.0102599; b2.spp=-0.0187599435; b3.spp=-0.027455122; b4.spp=4.0610848; b7.spp=-0.107733103}
#  else if(SPP=='BO'){b0.spp=-8.0811195;b1.spp=1.91879668; b2.spp=-0.0235279384; b3.spp=0.165699042; b4.spp=-5.65752421; b7.spp=0.091447011}
  else if(SPP=='BP'){b0.spp=1.6390594;b1.spp=0.03473199; b2.spp=-0.0263994686; b3.spp=-0.065146499; b4.spp=1.34626484; b7.spp=-0.122397452}
  else if(SPP=='BS'){b0.spp=1.0016177;b1.spp=-0.4440314; b2.spp=0.0171210594; b3.spp=0.125966456; b4.spp=0.69264817; b7.spp=-0.018369509}
  else if(SPP=='BT'){b0.spp=3.7787777;b1.spp=-0.44923409; b2.spp=-0.0068167636; b3.spp=-0.177696715; b4.spp=2.95344184; b7.spp=0.03280188}
  else if(SPP=='BW'){b0.spp=1.6425573;b1.spp=-0.86257667; b2.spp=0.0375511412; b3.spp=-0.064197632; b4.spp=-0.15260581; b7.spp=0.092638484}
  else if(SPP=='CC'){b0.spp=1.09406;b1.spp=-1.07158834; b2.spp=0.0694477194; b3.spp=0.032022586; b4.spp=-0.2758611; b7.spp=0.010334852}
  else if(SPP=='EC'){b0.spp=0.6040915;b1.spp=0.62654613; b2.spp=-0.0297585961; b3.spp=-0.051714594; b4.spp=1.59024315; b7.spp=-0.039007143}
  else if(SPP=='EH'){b0.spp=3.2585677;b1.spp=-0.08355463; b2.spp=-0.0103915449; b3.spp=-0.003681258; b4.spp=3.96588172; b7.spp=-0.058817324}
#  else if(SPP=='EL'){b0.spp=0;b1.spp=0; b2.spp=0; b3.spp=0; b4.spp=0; b7.spp=0}
  else if(SPP=='EO'){b0.spp=0;b1.spp=0; b2.spp=0; b3.spp=0; b4.spp=0; b7.spp=0}
  else if(SPP=='GA'){b0.spp=-1.1221188;b1.spp=0.8948402; b2.spp=-0.055186417; b3.spp=-0.052019685; b4.spp=-0.0757749; b7.spp=0.038406438}
  else if(SPP=='GB'){b0.spp=3.7390276;b1.spp=-1.29101834; b2.spp=0.0428521004; b3.spp=-0.152462601; b4.spp=1.7556148; b7.spp=-0.093866572}
  else if(SPP=='HH'){b0.spp=0.3565072;b1.spp=-0.41065139; b2.spp=0.0066364378; b3.spp=-0.039962184; b4.spp=-0.25623901; b7.spp=0.001763895}
  else if(SPP=='HT'){b0.spp=-5.5174371;b1.spp=0.97516386; b2.spp=-0.0118795802; b3.spp=0.085762719; b4.spp=-4.01694393; b7.spp=0.084545153}
  else if(SPP=='JP'){b0.spp=-3.4761431;b1.spp=1.66074622; b2.spp=-0.0810468326; b3.spp=0.066544566; b4.spp=0.48581471; b7.spp=-0.051239483}
  else if(SPP=='LD'){b0.spp=-11.0731049;b1.spp=3.30669005; b2.spp=-0.1020558559; b3.spp=0.113084811; b4.spp=-2.67446811; b7.spp=0.200675632}
  else if(SPP=='MA'){b0.spp=-2.3813284;b1.spp=0.45515939; b2.spp=0.0022085567; b3.spp=0.109266172; b4.spp=-0.92981064; b7.spp=0.068719784}
  else if(SPP=='MM'){b0.spp=2.1275812;b1.spp=-1.3077595; b2.spp=0.0159866485; b3.spp=-0.092795519; b4.spp=-0.9185151; b7.spp=-0.019527289}
  else if(SPP=='NM'){b0.spp=-0.9423148;b1.spp=0.95828672; b2.spp=-0.0483829401; b3.spp=-0.019207158; b4.spp=1.34195407; b7.spp=-0.008473936}
  else if(SPP=='NS'){b0.spp=4.7840042;b1.spp=-0.25002486; b2.spp=-0.0289766535; b3.spp=0.082906036; b4.spp=3.84269461; b7.spp=-0.247928579}
#  else if(SPP=='OH'){b0.spp=2.6213211;b1.spp=-0.6724876; b2.spp=0.0114224512; b3.spp=-0.037955515; b4.spp=0.51366112; b7.spp=-0.09206191}
  else if(SPP=='OK'){b0.spp=-3.9430371;b1.spp=1.40876658; b2.spp=-0.0432642266; b3.spp=0.056019829; b4.spp=-3.03723009; b7.spp=0.023589148}
  else if(SPP=='PB'){b0.spp=2.1318704;b1.spp=-0.5473173; b2.spp=0.0025691412; b3.spp=-0.060770629; b4.spp=1.47077911; b7.spp=-0.032903906}
  else if(SPP=='PP'){b0.spp=0.9315544;b1.spp=0.48612378; b2.spp=-0.0366110251; b3.spp=-0.042480416; b4.spp=1.27815008; b7.spp=-0.069172334}
  else if(SPP=='PR'){b0.spp=4.0097955;b1.spp=-2.5238272; b2.spp=0.1679281883; b3.spp=0.026989791; b4.spp=0.02828393; b7.spp=0.039293449}
  else if(SPP=='QA'){b0.spp=2.5775649;b1.spp=-0.64133332; b2.spp=0.0238329259; b3.spp=-0.070439796; b4.spp=1.67545864; b7.spp=0.009135114}
  else if(SPP=='RM'){b0.spp=2.6832033;b1.spp=-0.41788716; b2.spp=0.0030496229; b3.spp=-0.046460735; b4.spp=2.47920399; b7.spp=0.0151001}
  else if(SPP=='RN'){b0.spp=6.6155665;b1.spp=-1.12785577; b2.spp=-0.0036157021; b3.spp=-0.193304799; b4.spp=3.51229483; b7.spp=-0.244353158}
  else if(SPP=='RO'){b0.spp=2.9689136;b1.spp=-0.03184453; b2.spp=-0.0152279543; b3.spp=-0.076349232; b4.spp=3.62186609; b7.spp=0.011964748}
  else if(SPP=='RS'){b0.spp=2.695951;b1.spp=-0.79443361; b2.spp=0.0295142353; b3.spp=0.075240962; b4.spp=1.4693089; b7.spp=-0.1150735}
#  else if(SPP=='SB'){b0.spp=-9.6870546;b1.spp=0.89703022; b2.spp=0.0269701295; b3.spp=0.18518691; b4.spp=-13.11870456; b7.spp=0.166901925}
  else if(SPP=='SC'){b0.spp=-1.5367687;b1.spp=1.42415193; b2.spp=-0.0931142087; b3.spp=-0.032841317; b4.spp=0.99243335; b7.spp=-0.087710877}
  else if(SPP=='SE'){b0.spp=2.7464782;b1.spp=-1.43712513; b2.spp=0.0761560443; b3.spp=0.045638398; b4.spp=1.20586255; b7.spp=0.054734032}
  else if(SPP=='SH'){b0.spp=-1.6906841;b1.spp=0.25033624; b2.spp=-0.0012192427; b3.spp=0.025769271; b4.spp=-1.03497429; b7.spp=0.034007588}
  else if(SPP=='SM'){b0.spp=0.8646292;b1.spp=0.1606465; b2.spp=-0.0117643059; b3.spp=-0.094728053; b4.spp=1.68141577; b7.spp=0.064950469}
  else if(SPP=='ST'){b0.spp=1.6466569;b1.spp=-0.32984452; b2.spp=-0.0334795138; b3.spp=-0.011950043; b4.spp=0.75315213; b7.spp=0.02941668}
  else if(SPP=='SV'){b0.spp=-4.7709002;b1.spp=1.7082518; b2.spp=-0.0518235403; b3.spp=-0.007472909; b4.spp=-0.75161545; b7.spp=0.073835214}
  else if(SPP=='SW'){b0.spp=-0.3485851;b1.spp=-0.14419722; b2.spp=0.0070381607; b3.spp=0.010987117; b4.spp=-0.39287648; b7.spp=0.02040531}
  else if(SPP=='TA'){b0.spp=-1.5348956;b1.spp=0.35614211; b2.spp=-0.0067970683; b3.spp=0.045015574; b4.spp=-0.19247779; b7.spp=0.009697623}
  else if(SPP=='WA'){b0.spp=2.8060758;b1.spp=-0.32124551; b2.spp=-0.0012416002; b3.spp=-0.11701771; b4.spp=2.34916178; b7.spp=0.026270063}
  else if(SPP=='WC'){b0.spp=-0.7026246;b1.spp=0.2173645; b2.spp=-0.0016794683; b3.spp=0.068694576; b4.spp=0.30863392; b7.spp=0.04509622}
  else if(SPP=='WI'){b0.spp=-1.1338382;b1.spp=-0.08889308; b2.spp=0.0056498224; b3.spp=0.03779252; b4.spp=-0.73738382; b7.spp=0.151790296}
#  else if(SPP=='WO'){b0.spp=-10.1571923;b1.spp=3.40419241; b2.spp=-0.1079682492; b3.spp=0.096566669; b4.spp=-4.55857762; b7.spp=0.006430305}
  else if(SPP=='WP'){b0.spp=3.5635806;b1.spp=-0.52850445; b2.spp=0.0081758174; b3.spp=-0.031875214; b4.spp=2.26001556; b7.spp=-0.145067499}
  else if(SPP=='WS'){b0.spp=0.5059813;b1.spp=0.17060853; b2.spp=-0.0175268964; b3.spp=0.050412522; b4.spp=0.9528732; b7.spp=-0.114848818}
  else if(SPP=='XX'){b0.spp=0;b1.spp=0; b2.spp=0; b3.spp=0; b4.spp=0; b7.spp=0}
  else if(SPP=='YB'){b0.spp=0.144902;b1.spp=0.08733018; b2.spp=-0.007512005; b3.spp=0.014063659; b4.spp=0.7059273; b7.spp=0.021392604}
  else{b0.spp=0; b1.spp=0; b2.spp=0; b3.spp=0; b4.spp=0; b7.spp=0}
  dDBH=exp((b0+b0.spp)+(b1+b1.spp)*(log(DBH+1))+(b2+b2.spp)*(DBH)+(b3+b3.spp)*log(BAL.HW+1)+
             (b4+b4.spp)*log(CR+0.01)+b5*log(CSI)+b6*log(BA+1)+(b7+b7.spp)*log(BAL.SW+1)+b8*log(CR*BA+.1)+b9*CR) 
  return(dDBH=dDBH)
}



## Diameter modifier function 
dDBH.mod.FUN = function(SPP, PERCBArm, BApre, QMDratio, YEAR_CT, YEAR)
{
  # 2/28/2015 based on pre-lim result by Christian Kuehne
  # SPP= species, only care if it is BF or RS
  # PERCBArm= percent basal area removed
  # BApre= initial basal area at the of thinning (pre thin BA)
  # QMDratio = the ratio of QMD before and after thinning

  # balsam fir 
  if(SPP=="BF"){
      a0 = -3.43779
      a1 = 1.48358
      a2 = -0.00156
      a3 = 0.71898
      a4 = -0.00015
      a5 = -0.20536
      a6 = -0.47467
      a7 = 0.00936
      b0 = 0.053
      b1 = 2.77469
      b2 = 0.61421
      b3 = -0.01529
    }
    
  # red spruce 
  if(SPP=="RS"){
      a0 = -4.53275
      a1 = 1.331653
      a2 = -0.00113
      a3 = 0.687466
      a4 = -0.00036
      a5 = -0.12613
      a6 = -0.25083
      a7 = 0.048868
      b0 = 0.021943
      b1 = 3.636217
      b2 = 1.460329
      b3 = -0.02495
  }
   
  TST = YEAR - YEAR_CT # time since thinning
    
  #dDBH.mod <- #exp(a0 + a1 * log(DBH + 1) + a2 * (DBH^2) + a3 * 
  #log(CR) + a4 * (BAL^2) +a5 * sqrt(BAPH) +
  #      a6 * STATUSmod + a7 * CSI) * 

  dDBH.mod = ifelse(!is.na(YEAR_CT) && YEAR_CT <= YEAR && SPP=='BF' | SPP=='RS', 
                    (1 + (b0 * log(((PERCBArm + BApre) * 
                      (QMDratio^b1)) + 1) *exp(b2 * log(TST + 1) + 
                      b3 * (TST^2)))), 1)
  return(dDBH.mod = dDBH.mod)
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

#Mortality using the approach of Kershaw
stand.mort.prob=function(region,BA,BAG,QMD,pBA.BF,pBA.IH)
{
  BA.BF=pBA.BF*BA
  BA.IH=pBA.IH*BA
  if(region=='ME'){b0=0.6906978; b1=0.149228; b2=-0.001855535; b3=-2.557345; b4=-0.05507579; b5=0.06414701; b6=0.0432701}
  else if(region=='NB'){b0=0.699147; b1=0.1250758; b2=-0.001855535; b3=-2.557345; b4=-0.05507579; b5=0.06414701; b6=0.0432701}
  else if(region=='NS'){b0=0.2756542; b1=0.1499495; b2=-0.001855535; b3=-2.557345; b4=-0.05507579; b5=0.06414701; b6=0.0432701}
  else if(region=='PQ'){b0=1.0472726; b1=0.161746; b2=-0.001855535; b3=-2.557345; b4=-0.05507579; b5=0.06414701; b6=0.0432701}
  k=b0+b1*BA+b2*BA^2+b3*BAG+b4*QMD+b5*BA.BF+b6*BA.IH
  prob=exp(k)/(1+exp(k))
  return(prob=prob)
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


tree.mort.prob=function(SPP,DBH)
{
  if     (SPP=='AB'){b0=2.152681379; b1=-0.0269825907; b2=0.0002203177; Djump=41; Scale=20; Shape=4.5}
  else if(SPP=='AE'){b0=2.948346662; b1=-0.1017558326; b2=0.0018279137; Djump=22.5; Scale=40; Shape=2}
  else if(SPP=='AH'){b0=4.552236589; b1=-0.4626664119; b2=0.0125996045; Djump=12; Scale=15; Shape=4.5}
  else if(SPP=='AP'){b0=5.6430024532; b1=-0.4644532732; b2=0.0132654057; Djump=16; Scale=10.6; Shape=4.1}
  else if(SPP=='BA'){b0=1.7183600838; b1=0.0393047451; b2=-0.0023514773; Djump=39; Scale=37; Shape=4.25}
  else if(SPP=='BC'){b0=4.8627898851; b1=-0.3695674404; b2=0.0109822297; Djump=9.2; Scale=31; Shape=3.63}
  else if(SPP=='BE'){b0=4.552236589; b1=-0.2313332059; b2=0.0125996045; Djump=14; Scale=20; Shape=4.5}
  else if(SPP=='BF'){b0=2.5743949775; b1=-0.0851930923; b2=0.0015971909; Djump=53; Scale=40.6; Shape=1.51}
  else if(SPP=='BL'){b0=-3.5183135273; b1=0.5008393656; b2=-0.0114432915; Djump=10; Scale=10; Shape=2}
  else if(SPP=='BN'){b0=9.6140856026; b1=-0.8619281584; b2=0.0215901194; Djump=15; Scale=27.5; Shape=1.5}
  else if(SPP=='BO'){b0=2.7402431243; b1=-0.0403087; b2=0.0014846314; Djump=40; Scale=40; Shape=2}
  else if(SPP=='BP'){b0=1.8795415329; b1=-0.3915484285; b2=0.0298003249; Djump=33; Scale=33.6; Shape=4.75}
  else if(SPP=='BS'){b0=1.9568828063; b1=0.0535388009; b2=-0.0010376306; Djump=34; Scale=22; Shape=3.75}
  else if(SPP=='BT'){b0=2.1791849646; b1=-0.0125375225; b2=0.0008529794; Djump=40; Scale=30; Shape=3}
  else if(SPP=='BW'){b0=-1.4145296118; b1=0.3204863989; b2=-0.0029710752; Djump=15; Scale=30; Shape=3}
  else if(SPP=='CC'){b0=4.8627898851; b1=-0.3695674404; b2=0.0109822297; Djump=10; Scale=20; Shape=3.63}
  else if(SPP=='EC'){b0=-0.4584998714; b1=0.1992627013; b2=-0.0028451758; Djump=40.6; Scale=11; Shape=4.28}
  else if(SPP=='EH'){b0=4.5205542708; b1=-0.0670350692; b2=0.0012041907; Djump=50; Scale=40; Shape=3.5}
  else if(SPP=='GA'){b0=7.2061395918; b1=-0.2239701333; b2=0.0070370484; Djump=34; Scale=39.6; Shape=3.41}
  else if(SPP=='GB'){b0=0.1922677751; b1=0.1517490102; b2=-0.0039268819; Djump=16; Scale=8.2; Shape=3.83}
  else if(SPP=='HH'){b0=2.9674489273; b1=-0.1009595852; b2=0.0071673636; Djump=18; Scale=24; Shape=2}
  else if(SPP=='JP'){b0=-0.4488149338; b1=0.1939739736; b2=-0.0019541699; Djump=30; Scale=20; Shape=3}
  else if(SPP=='NM'){b0=4.552236589; b1=-0.2313332059; b2=0.0125996045; Djump=24; Scale=40; Shape=4.5}
  else if(SPP=='NS'){b0=17.4833923331; b1=-1.809142126; b2=0.0616970369; Djump=15; Scale=35; Shape=2}
  else if(SPP=='OH'){b0=4.552236589; b1=-0.5204997134; b2=0.0125996045; Djump=12; Scale=10; Shape=4.5}
  else if(SPP=='PB'){b0=2.5863343441; b1=-0.0518497247; b2=0.0021853588; Djump=26.4; Scale=41.2; Shape=1.88}
  else if(SPP=='PP'){b0=12.1649655944; b1=-1.0483772747; b2=0.0233147008; Djump=25.8; Scale=41.6; Shape=4.41}
  else if(SPP=='PR'){b0=-1.2171488097; b1=0.3211464783; b2=-0.0097154365; Djump=10; Scale=40; Shape=2}
  else if(SPP=='QA'){b0=-0.4584998714; b1=0.1992627013; b2=-0.0028451758; Djump=60.6; Scale=11; Shape=4.28}
  else if(SPP=='RM'){b0=2.1674971386; b1=0.0557266595; b2=-0.0010435394; Djump=60.2; Scale=40.6; Shape=4.38}
  else if(SPP=='RO'){b0=3.1202275212; b1=-0.041290776; b2=0.0022978235; Djump=41; Scale=40.4; Shape=3.27}
  else if(SPP=='RP'){b0=1.1361278304; b1=0.1436446742; b2=0.0018438454; Djump=30; Scale=30; Shape=3}
  else if(SPP=='RS'){b0=2.0420797297; b1=0.0425701678; b2=-0.0004901795; Djump=41; Scale=32; Shape=4.8}
  else if(SPP=='SB'){b0=44.2565091524; b1=-2.4248136198; b2=0.1388397603; Djump=22; Scale=35.6; Shape=4.57}
  else if(SPP=='SH'){b0=4.552236589; b1=-0.2313332059; b2=0.0125996045; Djump=15; Scale=10; Shape=4.5}
  else if(SPP=='SM'){b0=2.7069022565; b1=0.0086263655; b2=0.0007235392; Djump=54.4; Scale=42; Shape=1.33}
  else if(SPP=='SP'){b0=5; b1=-0.3; b2=0.01; Djump=20; Scale=40; Shape=3}
  else if(SPP=='ST'){b0=4.5522366258; b1=-0.4626664068; b2=0.012599604; Djump=4.4; Scale=24.4; Shape=4.51}
  else if(SPP=='SV'){b0=4.552236589; b1=-0.2313332059; b2=0.0125996045; Djump=24; Scale=40; Shape=4.5}
  else if(SPP=='TA'){b0=1.4269435976; b1=0.0886275939; b2=-0.0021232407; Djump=30; Scale=33.6; Shape=4.5}
  else if(SPP=='WA'){b0=1.0042653571; b1=0.165359309; b2=-0.0005814562; Djump=21; Scale=40; Shape=4}
  else if(SPP=='WC'){b0=3.647647507; b1=-0.0606735724; b2=0.0008507857; Djump=40; Scale=45; Shape=5}
  else if(SPP=='WL'){b0=-3.5183135273; b1=0.5008393656; b2=-0.0114432915; Djump=10; Scale=10; Shape=2}
  else if(SPP=='WO'){b0=-4.8640326448; b1=0.6250645999; b2=-0.0064419714; Djump=18; Scale=40; Shape=4}
  else if(SPP=='WP'){b0=3.3383526175; b1=-0.0294498474; b2=0.0009561864; Djump=61.2; Scale=41.2; Shape=2.87}
  else if(SPP=='WS'){b0=0.5437824528; b1=0.1052397713; b2=0.0006332627; Djump=19; Scale=54; Shape=2}
  else if(SPP=='YB'){b0=2.6967072576; b1=-0.001250889; b2=0.0007521152; Djump=48; Scale=60; Shape=2}
  else              {b0=2.6967072576; b1=-0.001250889; b2=0.0007521152; Djump=48; Scale=60; Shape=2}
  ddd <- b0 + b1 * DBH + b2 * DBH^2 #+b3*BAL +b4*BAG30
  surv <- exp(ddd)/(1 + exp(ddd))
#  IDj <- as.integer(DBH/Djump)
#  Wprob <- exp(-Scale * ((IDj * (DBH - Djump))^Shape))
#  tsurv <- surv * Wprob
#  return(tsurv=tsurv)
  return(tsurv=surv)
}


##INGROWTH FUNCTION of Li et al. (2011; CJFR 41, 2077-2089)
# PARMS is GNLS (generalized least squares) or NLME (mixed effects), 
# CutPoint is the probability threshold where ingrowth will occur
# BA is total basal area,
# TPH is trees per ha, 
# PHW is percent hardwood basal area, 
# MinDBH is the minimum threshold diameter (cm)
# ClimateSI is the climate site index (m)
Ingrowth.FUN=function(PARMS,CutPoint,BA,TPH,QMD,PHW,MinDBH,ClimateSI,cyclen=1)
{    
  if(PARMS=="GNLS"){
    a0=-0.2116   #-4.7867
    a1=-0.0255   #0.0469
    a2=-0.1396   #-0.8623
    a3=-0.0054   #2.3E-5
    a4=0.0433    #0.1541
    a5=0.0409    #0.1508
    b0=3.8982    #3.9018
    b1=-0.0257   #-0.0257
    b2=-0.3668   #-0.3694
    b3=0.0002    #0.0002
    b4=0.0216    #0.0216
    b5=-0.0514   #-0.0516
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
  IPH  = exp(eta)
  if (cyclen>1) 
  {
    PI = 1-((1-PI)^cyclen)
    IPH = IPH*cyclen
  }
  IPH = if (CutPoint == 0) IPH*PI else ifelse(PI>=CutPoint,IPH,0)
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

KozakTreeVol=function(Bark,SPP,DBH,HT,Planted,stump=NA,topHT=NA,topD=NA)
{
  sgmts =  100  #max(5,HT*2) # faster but there seems to be a bias
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
      #dib1= dob1
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
        #dib2= dob2
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
  if (!exists("a")) return (NA)
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
  temp <- subset(tree,select=c("YEAR","STAND","PLOT",'TREE','DBH','HT','CR',
        'EXPF',"ba",'ba.WP','ba.BF','ba.RM','ba.RS','ba.BS','ba.PB','ba.YB',
        'ba.GB','ba.WS','sdi'))
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
  

