
#load Acadian growth functions
source("AcadianFunctionsV1.R")
source("AcadianV2.R")

# Note: The form of the function call is very carefully coded. Make sure
# "runOps" exists if you want them to be used.
fvsRunAcadian <- function(runOps)
{
  # process the ops.
  INGROWTH = if (is.null(runOps$uiAcadianIngrowth)) "N" else 
             runOps$uiAcadianIngrowth
  MinDBH   = as.numeric(if (is.null(runOps$uiAcadianMinDBH)) "3.0" else 
             runOps$uiAcadianMinDBH)
  mortModel= if (is.null(runOps$uiAcadianMort)) "Acadian" else 
             runOps$uiAcadianMort 
  
  mkGraphs=FALSE
  CutPoint=0
  
  #load some handy conversion factors
  CMtoIN  = fvsUnitConversion("CMtoIN")
  INtoCM  = fvsUnitConversion("INtoCM")
  FTtoM   = fvsUnitConversion("FTtoM")
  MtoFT   = fvsUnitConversion("MtoFT")
  ACRtoHA = fvsUnitConversion("ACRtoHA")
  HAtoACR = fvsUnitConversion("HAtoACR")
  spcodes = fvsGetSpeciesCodes()
  stdIds  = fvsGetStandIDs()

  incr    = list()
  repeat     
  {
    #stopPointCode 5 (after growth and mortality, before it is added)
    #stopPointCode 6 (just before estab, place to add new trees)

    #BE CAREFULL: the next few lines control when to exit the loop and
    #the details are very important. It is easy to break this code!
    rtn = fvsRun(stopPointCode=5,stopPointYear=-1)  
    if (rtn != 0) break
    stopPoint <- fvsGetRestartcode()
    # end of current stand?
    if (stopPoint == 100) break
  
    #fetch some stand level information
    stdInfo = fvsGetEventMonitorVariables(c("site","year","cendyear"))
    cyclen = stdInfo["cendyear"] - stdInfo["year"] + 1
    attributes(cyclen) = NULL
    CSI = stdInfo["site"] * FTtoM
      
    #fetch the fvs trees and form the AcadianGY "tree" dataframe
    incr$tree = fvsGetTreeAttrs(c("plot","species","tpa","dbh","ht","cratio",
                                  "dg","htg","mort"))                             
    names(incr$tree) = toupper(names(incr$tree))
    incr$tree$id = 1:nrow(incr$tree)
    incr$tree$TREE= incr$tree$id
    names(incr$tree)[match("SPECIES",names(incr$tree))] = "SP"
    names(incr$tree)[match("TPA",names(incr$tree))] = "EXPF"
    incr$tree$SP = spcodes[incr$tree$SP,1]        
    #change CR to a proportion and take abs; note that in FVS a negative CR 
    #signals that CR change has been computed by the fire or insect/disease model
    incr$tree$CR   = abs(incr$tree$CRATIO) * .01  
    incr$tree$DBH  = incr$tree$DBH  * INtoCM
    incr$tree$HT   = incr$tree$HT   * FTtoM
    incr$tree$DG   = incr$tree$DG   * INtoCM
    incr$tree$HTG  = incr$tree$HTG  * FTtoM
    incr$tree$EXPF = incr$tree$EXPF * ACRtoHA
                       
    #compute the growth
    incr = AcadianGY(incr$tree,CSI,cyclen=cyclen,
                     INGROWTH=INGROWTH,
                     MinDBH=MinDBH, 
                     CutPoint=0,   # >0 uses threshold probability (>0-1).
                     mortModel=mortModel) 
                     
    #plot growth and ingrowth

    if (mkGraphs)
    {
      file=paste0("SId_",fvsGetStandIDs()["standid"],"_Year_",
                   stdInfo["year"],".png")
      main=paste0("SId=",fvsGetStandIDs()["standid"],"; Year=",
                  stdInfo["year"],"; Cyclen=",cyclen)
      png (filename=file,height=6,width=6,units="in",pointsize=10,res=300)
        par(mfcol=c(3,2),mar=c(4,4,3,1))
        barplot(by(incr$tree$EXPF,FUN=base::sum,
             INDICES=list(incr$tree$PLOT,incr$tree$SP)),
             main=main,ylab="Stocking (t/ha)",xlab="Species",cex.names=.7)
        box("figure")
        barplot(by(incr$tree$dEXPF,FUN=base::sum,
             INDICES=list(incr$tree$PLOT,incr$tree$SP)),
             main=main,ylab="Mortality (t/ha)",xlab="Species",cex.names=.7)
        box("figure")
        plot(y=incr$tree$HT,x=incr$tree$DBH,xlab="DBH (cm)",ylab="Ht (m)",
             col=as.numeric(as.factor(incr$tree$SP)),main=main)
        box("figure")
        plot(y=incr$tree$dHT,x=incr$tree$HT,xlab="Ht (m)",ylab="dHt (m)",
             col=as.numeric(as.factor(incr$tree$SP)),main=main)
        box("figure")
        plot(y=incr$tree$dDBH,ylab="dDBH (cm)",xlab="DBH (cm)",
             x=incr$tree$DBH,col=as.numeric(as.factor(incr$tree$SP)),main=main)
        box("figure")
        if (is.null(incr$ingrow))
        {
          plot.new()
          text(.5,.5,"No ingrowth")
        } else 
        {
          barplot(by(incr$ingrow$EXPF,FUN=base::sum,
               INDICES=list(incr$ingrow$PLOT,incr$ingrow$SP)),
               main=paste0(main,"\nMinDBH=",MinDBH," NumPlots=",
                           max(incr$ingrow$PLOT)),
               ylab="Ingrowth (t/ha)",xlab="Species",cex.names=.7)
        }
        box("figure")
      dev.off()
    }
                
    tofvs = data.frame(id=incr$tree$id,
            dg=incr$tree$dDBH*CMtoIN,
            htg=incr$tree$dHT*MtoFT,
            # set the crown ratio sign to negetive so that FVS 
            # doesn't change them. if already negetive, don't change them.
            cratio=ifelse(incr$tree$CRATIO < 0, incr$tree$CRATIO,
                   -round((1-((incr$tree$HCB+incr$tree$dHCB) / 
                              (incr$tree$HT +incr$tree$dHT)))*100,0)))
    if (!is.null(incr$tree$dEXPF)) tofvs$mort=incr$tree$dEXPF*HAtoACR          
    fvsSetTreeAttrs(tofvs)
    if (!is.null(incr$ingrow) && nrow(incr$ingrow)>0)
    {
      toadd = data.frame(dbh    =incr$ingrow$DBH*CMtoIN,
                         species=match(incr$ingrow$SP,spcodes[,"fvs"]),
                         ht     =incr$ingrow$HT*MtoFT,
                         cratio =incr$ingrow$CR,
                         plot   =as.numeric(incr$ingrow$PLOT),
                         tpa    =incr$ingrow$EXPF*ACRtoHA)
      room=fvsGetDims()
      room=room["maxtrees"] - room["ntrees"]
      if (nrow(toadd) < room) 
      {
        fvsRun(stopPointCode=6,stopPointYear=-1)
        fvsAddTrees(toadd)
      } else cat ("Not enough room for new trees. Stand=",
                  fvsGetStandIDs()["standid"],"; Year=",stdInfo["year"],"\n")
    }  
  }
  rtn
}

#NOTE: I tried various ways of building these elements. Setting the initial
#value to the saved value when the elements are created seems to work well. 
#What did not work was setting the initial value to some default and then 
#changing it using an update call in the server code.  

uiAcadian <- function(fvsRun)
{
  list(
    radioButtons("uiAcadianIngrowth", "Simulate ingrowth:", 
      c("Yes","No"),inline=TRUE,selected=
        if (!is.null(fvsRun$uiCustomRunOps$uiAcadianIngrowth))
                     fvsRun$uiCustomRunOps$uiAcadianIngrowth else "No"),
    textInput("uiAcadianMinDBH","Minimum DBH for ingrowth", 
        if (!is.null(fvsRun$uiCustomRunOps$uiAcadianMinDBH))
                     fvsRun$uiCustomRunOps$uiAcadianMinDBH   else "3.0"),
    radioButtons("uiAcadianMort", "Mortality model:", 
     c("Acadian","Base Model"),inline=TRUE,selected=
        if (!is.null(fvsRun$uiCustomRunOps$uiAcadianMort))
                     fvsRun$uiCustomRunOps$uiAcadianMort     else "Acadian")
  )
}
 
                      

#This is the "server" code for the Acadian model. It is specified as a 
#character variable and then a eval(parse()) sequence is run when this
#code is actually used. 

serverAcadian='
observe({
  if (length(input$uiAcadianIngrowth)) 
    fvsRun$uiCustomRunOps$uiAcadianIngrowth = input$uiAcadianIngrowth
})
observe({
  if (length(input$uiAcadianMinDBH)) 
    fvsRun$uiCustomRunOps$uiAcadianMinDBH   = input$uiAcadianMinDBH
})
observe({
  if (length(input$uiAcadianMort)) 
    fvsRun$uiCustomRunOps$uiAcadianMort     = input$uiAcadianMort
})
'
   



              
