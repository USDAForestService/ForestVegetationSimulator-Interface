ClearcutWin <- function(title, prms, fvsRun, globals) 
{
  pknum = match("management.Clearcut",names(prms))
  globals$currentCmdPkey = as.character(pknum) #point to the pkeys.
  defs <- c(f1=" ",f2="5",f3="10",ccf4="1",ccf5="30",ccf6="5")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in names(defs)) if(globals$currentEditCmp$reopn[name] != "")  
      defs[name] = globals$currentEditCmp$reopn[name]
cat ("in ClearcutWin code, defs=",defs,"\n")    
  # change this global so that the correct prms entry can be found later
  ans <- list(
    list(
      myInlineTextInput("cmdTitle","Component title ", value=title, size=40),
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      myInlineTextInput("f2", "Diameter of smallest tree cut: ", defs["f2"]),
      myInlineTextInput("f3", "Number of legacy trees per acre: ", defs["f3"]),
      radioButtons("ccf4", "How is the minimum diameter of legacy trees computed?", 
        c("Exactly specified, or"="1",
          "computed as percentile point in the distribution of trees."="2"),
        selected=defs["ccf4"],inline=TRUE),
      uiOutput("ClearcutWinMin")
    ),
    list(br(),
      p("Provides a way to simply specify a clearcut with legacy trees."),
      p("Is also useful for simulating coppice system.")))
      
  observe(                                            
  {
    output$ClearcutWinMin = renderUI(list( if (input$ccf4=="1")
      myInlineTextInput ("ccf5",
        "Minimum diameter of legacy trees: ",defs["ccf5"]) else
      radioButtons("ccf6", "Percentile point", 
        c("50th"="3","70th"="4","90th"="5"),defs["ccf6"],inline=TRUE)))        
  })

  ans  
}

ClearcutWin.mkKeyWrd <- function(input,output)
{ 
cat ("in ClearcutWin.mkKeyWrd, input=",c(f1=input$f1,f2=input$f2,
  f3=input$f3,ccf4=input$ccf4,ccf5=input$ccf5,ccf6=input$ccf6),"\n")
  list(ex="base", 
    kwds = if (input$ccf4 == "1") sprintf(
        paste0("ThinDBH   %10s%10s%10s%10s%10s%10s%10s\n",
               "ThinDBH   %10s%10s%10s%10s%10s%10s%10s"),
        input$f1, input$f2, input$ccf5, "1.0", "0.0", "0.0", "0.0",
        input$f1, input$ccf5, "999.0", "1.0", "0.0", input$f3, "0.0") else
      sprintf(
        paste0("ThinDBH   %10s  Parms(%s,DBHDist(3,%s),1,0,0,0)\n",
               "ThinDBH   %10s  Parms(DBHDist(3,%s),999,1,0,%s,0)"),
        input$f1, input$f2, input$ccf6, input$f1, input$ccf6, input$f3),
    reopn = c(f1=input$f1,f2=input$f2,f3=input$f3,ccf4=input$ccf4,
              ccf5=if (length(input$ccf5)) input$ccf5 else "",
              ccf6=if (length(input$ccf6)) input$ccf6 else "")
  )  
}

PlantNaturalFullWin <- function(title, prms, fvsRun, globals, full=TRUE) 
{
  pknum = match("management.PlantNatural",names(prms))
  globals$currentCmdPkey = as.character(pknum)  #point to the pkeys.
  defs <- c(pnDOD="1",pnYD="1",pnPBrn=" ",pnPMch=" ",
    pnSprt=getPstring(atag=globals$activeVariants[1],pkey="hasSproutingSpecies",
          pkeys=prms[[pknum]])[[1]],
    pnYpn1="1",pnTr1="1",pnSp1=" ", pnTpa1=" ",pnPsv1="100.",pnAge1=" ",
    pnHt1=" ",pnShd1="0",
    pnYpn2="1",pnTr2="1",pnSp2=" ", pnTpa2=" ",pnPsv2="100.",pnAge2=" ",
    pnHt2=" ",pnShd2="0")
  if (full) defs <- c(defs,
    c(pnIng=getPstring(atag=globals$activeVariants[1],pkey="inGrowthDefault",
          pkeys=prms[[pknum]])[[1]],pnNt="1",pnSAj="1.0"))
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in names(defs)) if(globals$currentEditCmp$reopn[name] != "")  
      defs[name] = globals$currentEditCmp$reopn[name]
  # change this global so that the correct prms entry can be found later
cat ("in PlantNaturalFullWin code, defs=",defs,"\n")    
  ans <- list(
    list(
      myInlineTextInput("cmdTitle","Component title ", value=title, size=40),
      mkScheduleBox("pnDOD",prms,"Schedule the date of disturbance",
        fvsRun,globals),
      div(style="background-color: rgb(255,240,240)",
        myInlineTextInput("pnYD", "Years following disturbance for site preparation: ", 
          defs["pnYD"]),
        fixedRow(
          column(width=4,
            myInlineTextInput("pnPBrn", "% plots burned: ", defs["pnPBrn"])),
          column(width=6,          
            myInlineTextInput("pnPMch", "% mechanically scarified: ", 
              defs["pnPMch"]))
      )),
      div(style="background-color: rgb(240,240,255)",
        fixedRow(
          column(width=5,          
            myRadioGroup("pnSprt", "Sprouting:", c("On"="1","Off"="0"),
              selected=defs["pnSprt"])),
          if (full) column(width=5,          
            myRadioGroup("pnIng", "Ingrowth:", c("On"="1","Off"="0"),
              selected=defs["pnIng"])) else NULL
         ),
        if (full) fixedRow(
          column(width=8,          
            myRadioGroup("pnNt", "New trees include ", 
              c("just those specified"="0",
                "model predicted"="1"),selected=defs["pnNt"])),
          column(width=3,          
            myInlineTextInput("pnSAj","Stock Adj:", value=defs["pnSAj"], size=6))
         ) else NULL
      ),
      div(style="background-color: rgb(240,255,240)",
        myInlineTextInput("pnYpn1", "Regen 1) Years following disturbance for regeneration: ", 
          defs["pnYpn1"]),
        myRadioGroup("pnTr1", "Type of regeneration scheduled: ", 
          c("Plant"="1","Natural"="2"),selected=defs["pnTr1"]),
        mkSelSpecies("pnSp1",prms,"Species",defs["pnSp1"],"deleteAll",globals$activeVariants[1]),
        fixedRow(
          column(width=5,       
            myInlineTextInput("pnTpa1","Trees/acre: ", value=defs["pnTpa1"], size=10),
            myInlineTextInput("pnPsv1","Percent survival: ", value=defs["pnPsv1"], size=10)),
          column(width=5,         
            myInlineTextInput("pnAge1","Average age: ", value=defs["pnAge1"], size=10),
            myInlineTextInput("pnHt1","Average height: ", value=defs["pnHt1"], size=10))),
        myRadioGroup("pnShd1", "Shade code:", 
          c("Uniform spatial distribution"="0" ,
            "Near dense plots"="1",
            "Near sparse plots"="2"), selected=defs["pnShd1"])
      ),
      div(style="background-color: rgb(255,255,240)",
        myInlineTextInput("pnYpn2", 
          "Regen 2) Years following disturbance for regeneration: ", defs["pnYpn2"]),
        myRadioGroup("pnTr2", "Type of regeneration scheduled: ", 
          c("Plant"="1","Natural"="2"),selected=defs["pnTr2"]),
        mkSelSpecies("pnSp2",prms,"Species",defs["pnSp2"],"deleteAll",globals$activeVariants[1]),
        fixedRow(
          column(width=5,       
            myInlineTextInput("pnTpa2","Trees/acre: ", value=defs["pnTpa2"], size=10),
            myInlineTextInput("pnPsv2","Percent survival: ", value=defs["pnPsv2"], size=10)),
          column(width=5,         
            myInlineTextInput("pnAge2","Average age: ", value=defs["pnAge2"], size=10),
            myInlineTextInput("pnHt2","Average height: ", value=defs["pnHt2"], size=10))),
        myRadioGroup("pnShd2", "Shade code:", 
          c("Uniform spatial distribution"="0" ,
            "Near dense plots"="1",
            "Near sparse plots"="2"), selected=defs["pnShd2"])    
      )
    ),
    list()
  )  
  ans  
}

PlantNaturalFullWin.mkKeyWrd <- function(input,output,full=TRUE)
{ 
cat ("in PlantNaturalFullWin.mkKeyWrd\n")
  kwds = sprintf("Estab     %10s",input$pnDOD)
  if (full & input$pnPBrn != " ") kwds = paste0(kwds,
    sprintf("\nBurnPrep  %10s%10s",
      as.character(as.numeric(input$pnDOD)+as.numeric(input$pnYD)),input$PBrn))
  if (full & input$pnPMch != " ") kwds = paste0(kwds,
    sprintf("\nMechPrep  %10s%10s",
      as.character(as.numeric(input$pnDOD)+as.numeric(input$pnYD)),input$pnPMch))
  kwds = paste0(kwds,"\n",if (input$pnSprt == "1") "Sprout" else "NoSprout")
  if (full) kwds = paste0(kwds,"\n",if (input$pnIng  == "1") "InGrow" else "NoInGrow")
  if (full) kwds = paste0(kwds,sprintf("\nStockAdj  %10s%10s",input$pnDOD,
    if (input$pnNt=="0") "0.0" else input$pnSAj))
  if (input$pnSp1 != " " & input$pnTpa1 != " ") 
  {
    pn = if (input$pnTr1 == "1") "Plant" else "Natural"
    kwds = paste0(kwds,sprintf("\n%-10s%10s%10s%10s%10s%10s%10s%10s",pn,
      as.character(as.numeric(input$pnDOD)+as.numeric(input$pnYpn1)),
      input$pnSp1,input$pnTpa1,input$pnPsv1,input$pnAge1,input$pnHt1,
      input$pnShd1))
  }
  if (input$pnSp2 != " " & input$pnTpa2 != " ") 
  {
    pn = if (input$pnTr2 == "1") "Plant" else "Natural"
    kwds = paste0(kwds,sprintf("\n%-10s%10s%10s%10s%10s%10s%10s%10s",pn,
      as.character(as.numeric(input$pnDOD)+as.numeric(input$pnYpn2)),
      input$pnSp2,input$pnTpa2,input$pnPsv2,input$pnAge2,input$pnHt2,
      input$pnShd2))
  }
  kwds = paste0(kwds,"\nEnd")
  reopn=c(pnDOD  =input$pnDOD , pnYD   =input$pnYD  ,
          pnPBrn =input$pnPBrn, pnPMch =input$pnPMch,
          pnSprt =input$pnSprt,
          pnYpn1 =input$pnYpn1, pnTr1  =input$pnTr1 ,              
          pnSp1  =input$pnSp1 , pnTpa1 =input$pnTpa1,
          pnPsv1 =input$pnPsv1, pnAge1 =input$pnAge1,
          pnHt1  =input$pnHt1 , pnShd1 =input$pnShd1,
          pnYpn2 =input$pnYpn2, pnTr2  =input$pnTr2 ,
          pnSp2  =input$pnSp2 , pnTpa2 =input$pnTpa2,
          pnPsv2 =input$pnPsv2, pnAge2 =input$pnAge2,
          pnHt2  =input$pnHt2 , pnShd2 =input$pnShd2)
  if (full) reopn = c(reopn, 
    c(pnIng=input$pnIng, pnSAj=input$pnSAj, pnNt=input$pnNt))
  list(ex   =if (full) "estb" else "strp",
       kwds = kwds, reopn = reopn)
}


PlantNaturalPartialWin <- function(title, prms, fvsRun, globals, full=FALSE)
  PlantNaturalFullWin(title, prms, fvsRun, globals, full=full)

PlantNaturalPartialWin.mkKeyWrd <- function(input,output,full=FALSE)
  PlantNaturalFullWin.mkKeyWrd(input,output,full=full)

ThinFromBelowWin <- function(title, prms, fvsRun, globals,session=session) 
{
  pknum = match("management.Thin",names(prms))
  globals$currentCmdPkey = as.character(pknum) #point to the pkeys.
  defs <- c(f1=" ",f2="300",f3="1",f4="0",f5="0",f6="999",f7="0",f8="999")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in intersect(names(defs),names(globals$currentEditCmp$reopn))) if(globals$currentEditCmp$reopn[name] != "")  
      defs[name] = globals$currentEditCmp$reopn[name]
  cat ("in ThinFromBelowWin code, defs=",defs,"\n")  
  
  ans <-list(
    list(
      myInlineTextInput("cmdTitle","Component title ", value=title, size=40),
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      myInlineTextInput("tbf2", "Specify residual density ",defs["f2"]),
      radioButtons("tbf3","in terms of: ",choices=list("Trees per acre "="1","Basal area per acre "="2","Percent of trees per acre at year of thin"="3",
                          "Percent of basal area at year of thin"="4"),selected=defs["f3"],inline=FALSE),
      myInlineTextInput("f4", "Proportion of trees left (spacing adjusdment, 1-CutEff) ", defs["f4"]),
      HTML(paste0("<b>","Specify tree size limits of thinning","</b>")),
      myInlineTextInput("f5","Diameter lower limits (inches) ",defs["f5"]),
      myInlineTextInput("f6", "Diameter upper limits (inches) ", defs["f6"]),
      myInlineTextInput("f7", "Height lower limits (feet) ", defs["f7"]),
      myInlineTextInput("f8", "Height upper limits (feet) ", defs["f8"])
    ),
  list(br()))
  ans
} 

observe({
  if(length(input$tbf3)==0) return()
  updateTextInput(session=session,inputId ="tbf2",value=
    switch(input$tbf3,"1"="300","2"="250","3"="70","4"="50"))
})
  


ThinFromBelowWin.mkKeyWrd <- function(input,output)
{
  cat ("in ThinFromBelowWin.mkKeyWrd, input=",c(f1=input$f1,f2=input$tbf2,
      f3=input$tbf3,f4=input$f4,f5=input$f5,f6=input$f6,f7=input$f7,f8=input$f8),"\n")
  list(ex="base",
       kwds = if (input$tbf3 == "1") sprintf(
         paste0("ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
         input$f1, input$tbf2, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) else if(input$tbf3 == "2")
         sprintf(paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n"),
         input$f1, input$tbf2, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) else if(input$tbf3 == "3")
         sprintf(paste0("ThinBTA   %10s  Parms(BTPA*%s,%s,%s,%s,%s,%s)\n"),
         input$f1, as.numeric(input$tbf2)/100, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) else
         sprintf(paste0("ThinBBA   %10s  Parms(BBA*%s,%s,%s,%s,%s,%s)\n"),
         input$f1, as.numeric(input$tbf2)/100, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8),  
       reopn = c(f1=input$f1,tbf2=input$tbf2,tbf3=input$tbf3,f4=input$f4,f5=input$f5,f6=input$f6,f7=input$f7,f8=input$f8)
  )
}

ThinFromAboveWin <- function(title, prms, fvsRun, globals,session=session) 
{
  pknum = match("management.Thin",names(prms))
  globals$currentCmdPkey = as.character(pknum) #point to the pkeys.
  defs <- c(f1=" ",f2="300",f3="1",f4="0",f5="0",f6="999",f7="0",f8="999")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in intersect(names(defs),names(globals$currentEditCmp$reopn))) if(globals$currentEditCmp$reopn[name] != "")  
      defs[name] = globals$currentEditCmp$reopn[name]
  cat ("in ThinFromAboveWin code, defs=",defs,"\n")  
  
  ans <-list(
    list(
      myInlineTextInput("cmdTitle","Component title ", value=title, size=40),
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      myInlineTextInput("taf2", "Specify residual density ",defs["f2"]),
      radioButtons("taf3","in terms of: ",choices=list("Trees per acre "="1","Basal area per acre "="2","Percent of trees per acre at year of thin"="3",
                                                       "Percent of basal area at year of thin"="4"),selected=defs["f3"],inline=FALSE),
      myInlineTextInput("f4", "Proportion of trees left (spacing adjusdment, 1-CutEff) ", defs["f4"]),
      HTML(paste0("<b>","Specify tree size limits of thinning","</b>")),
      myInlineTextInput("f5","Diameter lower limits (inches) ",defs["f5"]),
      myInlineTextInput("f6", "Diameter upper limits (inches) ", defs["f6"]),
      myInlineTextInput("f7", "Height lower limits (feet) ", defs["f7"]),
      myInlineTextInput("f8", "Height upper limits (feet) ", defs["f8"])
    ),
    list(br()))
  ans
} 


observe({
  if(length(input$taf3)==0) return()
  updateTextInput(session=session,inputId ="taf2",value=
                    switch(input$taf3,"1"="300","2"="250","3"="70","4"="50"))
})



ThinFromAboveWin.mkKeyWrd <- function(input,output)
{
  cat ("in ThinFromAboveWin.mkKeyWrd, input=",c(f1=input$f1,f2=input$taf2,
                                                f3=input$taf3,f4=input$f4,f5=input$f5,f6=input$f6,f7=input$f7,f8=input$f8),"\n")
  list(ex="base",
       kwds = if (input$taf3 == "1") sprintf(
         paste0("ThinATA   %10s%10s%10s%10s%10s%10s%10s\n"),
         input$f1, input$tbf2, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) else if(input$taf3 == "2")
         sprintf(paste0("ThinABA   %10s%10s%10s%10s%10s%10s%10s\n"),
         input$f1, input$tbf2, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) else if(input$taf3 == "3")
         sprintf(paste0("ThinATA   %10s  Parms(BTPA*%s,%s,%s,%s,%s,%s)\n"),
         input$f1, as.numeric(input$tbf2)/100, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) else
         sprintf(paste0("ThinABA   %10s  Parms(BBA*%s,%s,%s,%s,%s,%s)\n"),
         input$f1, as.numeric(input$tbf2)/100, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8),  
       reopn = c(f1=input$f1,tbf2=input$taf2,taf3=input$tbf3,f4=input$f4,f5=input$f5,f6=input$f6,f7=input$f7,f8=input$f8)
  )
}
