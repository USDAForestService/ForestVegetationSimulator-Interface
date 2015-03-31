AcadianGY <- function(tree,CSI,cyclen=1,INGROWTH="Y",MinDBH=10,CutPoint=0.5,
                      mortModel="Acadian")
{
  cat ("nrow(tree)=",nrow(tree)," CSI=",CSI,"cyclen=",cyclen,"\nINGROWTH=",
     INGROWTH," MinDBH=",MinDBH," CutPoint=",CutPoint," mortModel=",
     mortModel,file="fromAcadianGY.txt") 
  
  aa=sapply(tree$SP,SPP.func)
  tree$SPtype=t(aa)[,1]
  tree$shade=as.numeric(t(aa)[,2])
  tree$SG=as.numeric(t(aa)[,3])
  
  tree$EXPF = tree$EXPF
  tree$DBH = tree$DBH
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
  tree$ba.IHW=ifelse(tree$SPtype=='HW' & tree$shade<2.0,tree$ba,0)
  tree$tph.BF=ifelse(tree$SP=='BF',tree$EXPF,0)
  temp=subset(tree,select=c('PLOT','TREE','EXPF',"ba",'ba.SW','ba.WP',
                'ba.BF','ba.RM','ba.SPR','ba.BRH','ba.OH','ba.OS','ba.RS',
                'ba.BS','ba.PB','ba.YB','ba.IHW','tph.BF'))
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
#  temp$qmd.BF=ifelse(is.na(temp$qmd.BF),0,temp$qmd.BF)
  
  temp=subset(temp,select=c('PLOT','BAPH','maxTREE','tph','qmd','pHW.ba',
    'pWP.ba','pBF.ba','pRM.ba','pSPR.ba','pBRH.ba','pOH.ba','pOS.ba',
    'pRS.ba','pBS.ba','pWS.ba','pPB.ba','pYB.ba','pGB.ba','qmd.BF','pIHW.ba'))
  #Sum.temp is used in the ingrowth calculations
  Sum.temp=temp
  
  temp$maxTREE = NULL
  tree=merge(tree,temp,by="PLOT")

  #Estimate basal area in larger trees

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
    pHT.m=mapply(HTPred,SPP=tree$SP,DBH=tree$DBH,CSI=CSI,
                 CCF=tree$CCF,BAL=tree$BAL)
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
      pHCB=mapply(HCBPred,SPP=tree$SP,DBH=tree$DBH,HT=tree$HT,
                  CCF=tree$CCF,BAL=tree$BAL)
      tree$HCB=ifelse(is.na(tree$HCB),pHCB,tree$HCB)
    }
    tree$CR=ifelse(is.na(tree$CR),1-(tree$HCB/tree$HT),tree$CR)
  }

  
  #Diameter growth
  tree$dDBH=mapply(dDBH.FUN, SPP=tree$SP, DBH=tree$DBH, CR=tree$CR,
            BAL.SW=tree$BAL.SW, BAL.HW=tree$BAL.HW,
            BA=tree$BAPH, CSI=CSI)
  
  #Height growth
  tree$dHT=mapply(Htincr,SPP=tree$SP,HT=tree$HT,CR=tree$CR,
            BAL.SW=tree$BAL.SW,BAL.HW=tree$BAL.HW,
            BAPH=tree$BAPH,CSI=CSI)
            
  #Crown base height change
  tree$HCB = (1-tree$CR)*tree$HT            
  tree$dHCB=mapply(dHCB,dHT=tree$dHT,DBH=tree$DBH,HT=tree$HT,
            HCB=tree$HCB,CCF=tree$CCF,shade=tree$shade)
            
  #Mortality
  if (mortModel == "Acadian") 
  {
    tree$tsurv=mapply(tree.mort.prob,tree$SP,tree$DBH)^cyclen
    tree = ddply (tree,.(PLOT),
      function (x)
      { 
        x = x[sort(x$DBH,decreasing=TRUE,index.return=TRUE)$ix,]
        x$Csward1<-cumsum(x$EXPF*(0.00015+0.00218*x$SG)*((x$DBH/25)^1.6))
        x$bag=(((x$DBH+x$dDBH)^2*0.00007854)-
              (x$DBH)^2*0.00007854)*x$EXPF
        x$Sbag30=sum(ifelse(x$Csward1<=0.3,x$bag,0))
        x
      })              
    tree$stand.pmort=mapply(stand.mort.prob,region='ME',BA=tree$BAPH,
        BAG=tree$Sbag30,QMD=tree$qmd,pBA.BF=tree$pBF.ba,pBA.IH=tree$pIHW.ba)    
    tree$stand.mort.BA=mapply(stand.mort.BA,region='ME',BA=tree$BAPH,
        BAG=tree$Sbag30,QMD=tree$qmd,tree$qmd.BF,pBA.bf=tree$pBF.ba,
        pBA.ih=tree$pIHW.ba)
  
    tree$stand.mort.BA = 1-((1-tree$stand.pmort)^cyclen) * 
                          tree$stand.mort.BA*cyclen
  
    tree$mortBA=((tree$DBH+(tree$dDBH*cyclen))^2*0.00007854)*
                tree$EXPF*(1-tree$tsurv)
    tree = ddply (tree,.(PLOT),  
           function (x)
           {
             # sort the trees on the plot on increasing survivorship and DBH
             x = sort.data.frame(x,~+tsurv+DBH)
             x$Cum.mort<-cumsum(x$mortBA)
             x$DmortBA = (x$stand.mort.BA-x$Cum.mort)
             x$Cum.DmortBA=cumsum(x$DmortBA)
             x$xDmortBA=x$Cum.DmortBA-(x$DmortBA)
             x$xmortBA=x$Cum.mort-x$mortBA
             x$vv=x$stand.mort.BA-(x$Cum.mort-x$mortBA)
             x$xxmortBA=x$Cum.DmortBA-x$Cum.mort
             x$TotBAmort=round(x$mortBA+x$xDmortBA,8)
             x$pBAmort=x$vv/x$mortBA
             x$dEXPF=ifelse(x$Cum.mort<=x$stand.mort.BA,
                            x$EXPF-(x$EXPF*x$tsurv),ifelse(x$vv>=0,  
                            x$EXPF - x$EXPF*(1-x$pBAmort),0))
             x$EXPF.ba=(((x$DBH+(x$dDBH*cyclen))^2*0.00007854)*x$dEXPF)
             x$Dead=cumsum(x$EXPF.ba)
             x
           }) 
  }
  else 
  {
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
    
    # get predicted heights and HCB, use plot CCF and BA as BAL.
    CCF =Sum.temp[ingrow[,"PLOT"],"CCF"]
    BAPH=Sum.temp[ingrow[,"PLOT"],"BAPH"]
    ingrow$HT=mapply(HTPred,SPP=ingrow$SP,DBH=ingrow$DBH,CSI=CSI,
                     CCF=CCF,BAL=BAPH)                     
    ingrow$HCB=mapply(HCBPred,SPP=ingrow$SP,DBH=ingrow$DBH,HT=ingrow$HT,
                      CCF=CCF,BAL=BAPH)
    ingrow$CR=1-(ingrow$HCB/ingrow$HT) 
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
  
  # return the original trees and the ingrowth separately
  list(tree=tree,ingrow=ingrow)
}

