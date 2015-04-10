###Adirondack growth and yield model
AdirondackGY=function(tree,CSI,cyclen=1,INGROWTH="Y",MinDBH=10,CutPoint=0.5,
                      mortModel="Adirondack")
{

  cat ("in AdirondackGY nrow(tree)=",nrow(tree)," CSI=",CSI,"cyclen=",
     cyclen,"\nINGROWTH=",INGROWTH," MinDBH=",MinDBH," CutPoint=",CutPoint,
     " mortModel=", mortModel,"\n",file="fromAdirondackGY.txt",append=TRUE) 

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
  
  cat ("at Sum.temp\n",file="fromAdirondackGY.txt",append=TRUE) 

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
  
  cat ("at growth\n",file="fromAdirondackGY.txt",append=TRUE) 

  #Diameter increment model
  tree$dDBH=mapply(SUNY.dDBH,SP=tree$SP,TYPE=tree$SPtype,DBH=tree$DBH,
    CR=tree$CR,BAL=tree$BAL,BA=tree$BAPH,CSI=CSI,PBAHW=tree$pHW.ba)

  #Height increment (revised 8/28/2012)
  tree$dHT=mapply(SUNY.dHT,SPP=tree$SP,TYPE=tree$SPtype,DBH=tree$DBH,
    HT=tree$HT,CR=tree$CR,BAL=tree$BAL,BA=tree$BAPH,CSI=CSI)

  #Crown base height...need this to be annual change: dHCB...computed here
  #in a difference equation format. 
  tree$HCB = mapply(SUNY.HCB,SPP=tree$SP,TYPE=tree$SPtype,
      tree$DBH,HT=tree$HT,
      BAL=tree$BAL,CSI=CSI,BA=tree$BAPH)     
  tree$dHCB= mapply(SUNY.HCB,SPP=tree$SP,TYPE=tree$SPtype,
      tree$DBH+tree$dDBH,HT=tree$HT+tree$dHT,
      BAL=tree$BAL,CSI=CSI,BA=tree$BAPH)-tree$HCB

  cat ("at Mortality\n",file="fromAdirondackGY.txt",append=TRUE) 
  #Mortality
  if (mortModel == "Adirondack") 
  {
    ps = mapply(SUNY.mort,SPP=tree$SP,TYPE=tree$SPtype,DBH=tree$DBH,
           HT=tree$HT,CR=tree$CR,BAL=tree$BAL,CSI=CSI,BA=tree$BAPH)
    # convert to a periodic probability of mortality.
    pm = 1-(ps^cyclen)
    tree$dEXPF = tree$EXPF * pm
  } else {
    #setting dEXPF to NULL will result in the base model mortality being used
    tree$dEXPF = NULL
  }

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

  cat ("at return, nrow(tree)=",nrow(tree)," ingrow=",
       if (is.null(ingrow)) "NULL" else nrow(ingrow),"\n",
         file="fromAdirondackGY.txt",append=TRUE)
       
  # return the original trees and the ingrowth separately
  list(tree=tree,ingrow=ingrow)
}

    
    
	
  