# $Id$

keyword.base.Compute.Win <- function(title, prms, fvsRun, globals)
{
  globals$currentCmdDefs <- c(f1=" ",freeEdit="")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in names(globals$currentCmdDefs)) if(globals$currentEditCmp$reopn[name] != "")  
      globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
  ans = list( 
    list (
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      mkFreeformEltList(globals,prms,title,globals$currentCmdDefs["freeEdit"]),
      tags$p(id="instruct",HTML(paste0(
          "Enter one or more expressions that define <i>compute</i> variables. ",
          "Example:<br><b>NormStk = 25000*((BADBH+1.)**(-1.588))<br></b>",
          "will define <i>normal stocking</i> (trees/acre) according to Haig's ",
          "(1932) definition.")))
     ),list())
  ans
}


keyword.base.Compute.Win.mkKeyWrd <- function(input,output)
{
cat ("in keyword.base.Compute.Win.mkKeyWrd\n")
  list(ex="base", 
    kwds = paste0(sprintf("Compute   %10s\n",input$f1),input$freeEdit,"\nEnd\n"),
    reopn = c(f1=input$f1,freeEdit=input$freeEdit)
  )  
}

keyword.dbs.StandSQL.Win <- function(title, prms, fvsRun, globals)
{
  globals$currentCmdDefs <- c(freeEdit=" ")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in names(globals$currentCmdDefs)) if(globals$currentEditCmp$reopn[name] != "")  
      globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
  ans = list( 
    list (
      tags$style(type="text/css", 
        "#freeEdit{font-family:monospace;font-size:90%;width:95%;}"), 
      tags$textarea(id="freeEdit", rows=10, globals$currentCmdDefs["freeEdit"]),
      tags$p(id="instruct",HTML(paste0(
          "Specify an SQL statement that from which stand-level ",
          "FVS variables are initialized.<br>Example:<br><b>",
          "SELECT * FROM FVS_StandInit WHERE Stand_ID = '%StandID%';<br></b>",
          "will define the stand-level variables from the FVS_StandInit table.")
           ))),
     list())
  ans
}
keyword.dbs.StandSQL.Win.mkKeyWrd <- function(input,output)
{
  list(ex="base", 
    kwds = paste0("StandSQL\n",input$freeEdit,"\nEndSQL\n"),
    reopn = c(freeEdit=input$freeEdit)
  )  
}


keyword.dbs.TreeSQL.Win <- function(title, prms, fvsRun, globals)
{
  globals$currentCmdDefs <- c(freeEdit=" ")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in names(globals$currentCmdDefs)) if(globals$currentEditCmp$reopn[name] != "")  
      globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
  ans = list( 
    list (
      tags$style(type="text/css", 
        "#freeEdit{font-family:monospace;font-size:90%;width:95%;}"), 
      tags$textarea(id="freeEdit", rows=10, globals$currentCmdDefs["freeEdit"]),
      tags$p(id="instruct",HTML(paste0(
          "Specify an SQL statement that from which tree-level ",
          "FVS variables are initialized.<br>Example:<br><b>",
          "SELECT * FROM FVS_TreeInit WHERE Stand_ID = '%StandID%';<br></b>",
          "will define the tree-level variables from the FVS_TreeInit table.")
           ))),
     list())
  ans
}
keyword.dbs.TreeSQL.Win.mkKeyWrd <- function(input,output)
{
  list(ex="base", 
    kwds = paste0("TreeSQL\n",input$freeEdit,"\nEndSQL\n"),
    reopn = c(freeEdit=input$freeEdit)
  )  
}



keyword.dbs.SQLIn.Win <- function(title, prms, fvsRun, globals)
{
  globals$currentCmdDefs <- c(f1=" ",freeEdit="")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in names(globals$currentCmdDefs)) if(globals$currentEditCmp$reopn[name] != "")  
      globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
  ans = list( 
    list (
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      tags$style(type="text/css", 
        "#freeEdit{font-family:monospace;font-size:90%;width:95%;}"), 
      tags$textarea(id="freeEdit", rows=10, globals$currentCmdDefs["freeEdit"]),
      tags$p(id="instruct",HTML(paste0(
            "Run an query on the DSNIn connection. If the query is a SELECT, ",
            "then the last row of the result table will define the values of ",
            "variables in the Event Monitor. The variables will have the column names.<br>",
            "Example:<br><b>Select Inv_Year as MyYear from FVS_StandInit ",
            "where Stand_ID = '%StandID%';<br></b>will define MyYear in the Event Monitor")
            ))
     ),list())
  ans
}
keyword.dbs.SQLIn.Win.mkKeyWrd <- function(input,output)
{
  list(ex="base", 
    kwds = paste0(sprintf("SQLIn     %10s\n",input$f1),input$freeEdit,"\nEndSQL\n"),
    reopn = c(f1=input$f1,freeEdit=input$freeEdit)
  )  
}


keyword.dbs.SQLOut.Win <- function(title, prms, fvsRun, globals)
{
  globals$currentCmdDefs <- c(f1=" ",freeEdit="")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in names(globals$currentCmdDefs)) if(globals$currentEditCmp$reopn[name] != "")  
      globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
  ans = list( 
    list (
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      tags$style(type="text/css", 
        "#freeEdit{font-family:monospace;font-size:90%;width:95%;}"), 
      tags$textarea(id="freeEdit", rows=10, globals$currentCmdDefs["freeEdit"])),
    list())
  ans
}
keyword.dbs.SQLOut.Win.mkKeyWrd <- function(input,output)
{
  list(ex="base", 
    kwds = paste0(sprintf("SQLOut    %10s\n",input$f1),input$freeEdit,"\nEndSQL\n"),
    reopn = c(f1=input$f1,freeEdit=input$freeEdit)
  )  
}


Compute_PreDefined <- function(title, prms, fvsRun, globals) 
{
  globals$currentCmdDefs <- c(f1="0",f2="VAR1",f3="")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in names(globals$currentCmdDefs)) if(globals$currentEditCmp$reopn[name] != "")  
      globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
  ans <-list(
    list(
      h5("To request for all cycles, enter 0 (zero) in the 'Schedule by Year/Cycle' input box."),
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      myInlineTextInput("f2",
       "Enter the name for your user-defined variable (< 8 characters)",
       globals$currentCmdDefs["f2"]),
      myInlineListButton ("f3", "Pick Event Monitor Varialbe", mkVarList(globals), 
        selected=globals$currentCmdDefs["f3"], deltll=2)),
    list(br(),p(paste(
      "Computes stand variables using pre-defined EM variables.  You must enter the",
      "name of your user-defined variable.  Computed variables may be viewed in the", 
      "Activity Summary of the main output file or in any of the Compute post",
      "processors.")),
      p(paste(
      "Note: The name of your user-defined variable may not be a word that is reserved",
      "for use by FVS or the Event Monitor.")))) 
  ans  
}


Compute_PreDefined.mkKeyWrd <- function(input,output)
{
  list(ex="base", 
    kwds = paste0(sprintf("Compute   %10s\n%s=%s\nEnd\n",input$f1,input$f2,input$f3)),
    reopn = c(f1=input$f1,f2=input$f2,f3=input$f3)
  )  
}


ClearcutWin <- function(title, prms, fvsRun, globals) 
{
  pknum = match("management.Clearcut",names(prms))
  globals$currentCmdPkey = as.character(pknum) #point to the pkeys.
  globals$currentCmdDefs <- c(f1=" ",f2="5",f3="10",ccf4="1",ccf5="30",ccf6="5")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in names(globals$currentCmdDefs)) if(globals$currentEditCmp$reopn[name] != "")  
      globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
cat ("in ClearcutWin code, globals$currentCmdDefs=",globals$currentCmdDefs,"\n")    
  # change this global so that the correct prms entry can be found later
  ans <- list(
    list(
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      myInlineTextInput("f2", "Diameter of smallest tree cut: ", globals$currentCmdDefs["f2"]),
      myInlineTextInput("f3", "Number of legacy trees per acre: ", globals$currentCmdDefs["f3"]),
      radioButtons("ccf4", "How is the minimum diameter of legacy trees computed?", 
        c("Exactly specified, or"="1",
          "computed as percentile point in the distribution of trees."="2"),
        selected=globals$currentCmdDefs["ccf4"],inline=TRUE),
      uiOutput("ClearcutWinMin")
    ),
    list(br(),
      p("Provides a way to simply specify a clearcut with legacy trees."),
      p("Is also useful for simulating coppice system.")))
      
  observe(                                            
  {
    output$ClearcutWinMin = renderUI(list( if (input$ccf4=="1")
      myInlineTextInput ("ccf5",
        "Minimum diameter of legacy trees: ",globals$currentCmdDefs["ccf5"]) else
      radioButtons("ccf6", "Percentile point", 
        c("50th"="3","70th"="4","90th"="5"),globals$currentCmdDefs["ccf6"],inline=TRUE)))        
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
  globals$currentCmdDefs <- c(pnDOD="1",pnYD="1",pnPBrn=" ",pnPMch=" ",
    pnSprt=getPstring(atag=globals$activeVariants[1],pkey="hasSproutingSpecies",
          pkeys=prms[[pknum]])[[1]],
    pnYpn1="1",pnTr1="1",pnSp1=" ", pnTpa1=" ",pnPsv1="100.",pnAge1=" ",
    pnHt1=" ",pnShd1="0",
    pnYpn2="1",pnTr2="1",pnSp2=" ", pnTpa2=" ",pnPsv2="100.",pnAge2=" ",
    pnHt2=" ",pnShd2="0")
  if (full) globals$currentCmdDefs <- c(globals$currentCmdDefs,
    c(pnIng=getPstring(atag=globals$activeVariants[1],pkey="inGrowthDefault",
          pkeys=prms[[pknum]])[[1]],pnNt="1",pnSAj="1.0"))
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in names(globals$currentCmdDefs)) if(globals$currentEditCmp$reopn[name] != "")  
      globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
  # change this global so that the correct prms entry can be found later
cat ("in PlantNaturalFullWin code, globals$currentCmdDefs=",globals$currentCmdDefs,"\n")    
  ans <- list(
    list(
      mkScheduleBox("pnDOD",prms,"Schedule the date of disturbance",
        fvsRun,globals),
      div(style="background-color: rgb(255,240,240)",
        myInlineTextInput("pnYD", "Years following disturbance for site preparation: ", 
          globals$currentCmdDefs["pnYD"]),
        fixedRow(
          column(width=4,
            myInlineTextInput("pnPBrn", "% plots burned: ", globals$currentCmdDefs["pnPBrn"])),
          column(width=6,          
            myInlineTextInput("pnPMch", "% mechanically scarified: ", 
              globals$currentCmdDefs["pnPMch"]))
      )),
      div(style="background-color: rgb(240,240,255)",
        fixedRow(
          column(width=5,          
            myRadioGroup("pnSprt", "Sprouting:", c("On"="1","Off"="0"),
              selected=globals$currentCmdDefs["pnSprt"])),
          if (full) column(width=5,          
            myRadioGroup("pnIng", "Ingrowth:", c("On"="1","Off"="0"),
              selected=globals$currentCmdDefs["pnIng"])) else NULL
         ),
        if (full) fixedRow(
          column(width=8,          
            myRadioGroup("pnNt", "New trees include ", 
              c("just those specified"="0",
                "model predicted"="1"),selected=globals$currentCmdDefs["pnNt"])),
          column(width=3,          
            myInlineTextInput("pnSAj","Stock Adj:", value=globals$currentCmdDefs["pnSAj"], size=6))
         ) else NULL
      ),
      div(style="background-color: rgb(240,255,240)",
        myInlineTextInput("pnYpn1", "Regen 1) Years following disturbance for regeneration: ", 
          globals$currentCmdDefs["pnYpn1"]),
        myRadioGroup("pnTr1", "Type of regeneration scheduled: ", 
          c("Plant"="1","Natural"="2"),selected=globals$currentCmdDefs["pnTr1"]),
        mkSelSpecies("pnSp1",prms,"Species",globals$currentCmdDefs["pnSp1"],"deleteAll",globals$activeVariants[1]),
        fixedRow(
          column(width=5,       
            myInlineTextInput("pnTpa1","Trees/acre: ", value=globals$currentCmdDefs["pnTpa1"], size=10),
            myInlineTextInput("pnPsv1","Percent survival: ", value=globals$currentCmdDefs["pnPsv1"], size=10)),
          column(width=5,         
            myInlineTextInput("pnAge1","Average age: ", value=globals$currentCmdDefs["pnAge1"], size=10),
            myInlineTextInput("pnHt1","Average height: ", value=globals$currentCmdDefs["pnHt1"], size=10))),
        myRadioGroup("pnShd1", "Shade code:", 
          c("Uniform spatial distribution"="0" ,
            "Near dense plots"="1",
            "Near sparse plots"="2"), selected=globals$currentCmdDefs["pnShd1"])
      ),
      div(style="background-color: rgb(255,255,240)",
        myInlineTextInput("pnYpn2", 
          "Regen 2) Years following disturbance for regeneration: ", globals$currentCmdDefs["pnYpn2"]),
        myRadioGroup("pnTr2", "Type of regeneration scheduled: ", 
          c("Plant"="1","Natural"="2"),selected=globals$currentCmdDefs["pnTr2"]),
        mkSelSpecies("pnSp2",prms,"Species",globals$currentCmdDefs["pnSp2"],"deleteAll",globals$activeVariants[1]),
        fixedRow(
          column(width=5,       
            myInlineTextInput("pnTpa2","Trees/acre: ", value=globals$currentCmdDefs["pnTpa2"], size=10),
            myInlineTextInput("pnPsv2","Percent survival: ", value=globals$currentCmdDefs["pnPsv2"], size=10)),
          column(width=5,         
            myInlineTextInput("pnAge2","Average age: ", value=globals$currentCmdDefs["pnAge2"], size=10),
            myInlineTextInput("pnHt2","Average height: ", value=globals$currentCmdDefs["pnHt2"], size=10))),
        myRadioGroup("pnShd2", "Shade code:", 
          c("Uniform spatial distribution"="0" ,
            "Near dense plots"="1",
            "Near sparse plots"="2"), selected=globals$currentCmdDefs["pnShd2"])    
      )
    ),
    list()
  )  
  ans  
}

PlantNaturalFullWin.mkKeyWrd <- function(input,output,full=TRUE)
{ 
  kwds = list()
cat ("in PlantNaturalFullWin.mkKeyWrd\n")
  if (full & input$pnPBrn != " ") kwds = sprintf("\nBurnPrep  %10s%10s",
      as.character(as.numeric(input$pnDOD)+as.numeric(input$pnYD)),input$PBrn)
  if (full & input$pnPMch != " ") kwds = if (length(kwds)) paste0(kwds,
    sprintf("\nMechPrep  %10s%10s",
      as.character(as.numeric(input$pnDOD)+as.numeric(input$pnYD)),input$pnPMch)) else
    sprintf("\nMechPrep  %10s%10s",
      as.character(as.numeric(input$pnDOD)+as.numeric(input$pnYD)),input$pnPMch) 
  kwds = if (length(kwds)) paste0(kwds,"\n",if (input$pnSprt == "1") "Sprout" else "NoSprout") else
         if (input$pnSprt == "1") "Sprout" else "NoSprout"
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
  globals$currentCmdDefs <- c(f1=" ",tbf2="1",tbf3="0",tbf4="0",tbf5="0",tbf6="0",tbf7="0",
                              f4="0",f5="0",f6="999",f7="0",f8="999")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in intersect(names(globals$currentCmdDefs),names(globals$currentEditCmp$reopn))) 
      if(globals$currentEditCmp$reopn[name] != "")  globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
cat ("in ThinFromBelowWin code, globals$currentCmdDefs=",globals$currentCmdDefs,"\n")  

  ans <-list(
    list(
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      HTML(paste0("<b>","Specify residual density","</b>")),
      fixedRow(
        tags$head(tags$script(HTML('
             Shiny.addCustomMessageHandler("jsCode",
               function(message) {
                 eval(message.code);
               }
            );
          '))),
        column(width=5,       
               radioButtons("tbf2","",
                            choices=list("Trees per acre"="1","Trees spacing (feet)"="2",
                            "Basal area per acre "="3",
                            "Percent of trees per acre at year of thin"="4",
                            "Percent of basal area at year of thin"="5"),
                            selected=globals$currentCmdDefs["tbf2"],inline=FALSE)
               ),
        column(width=5,      
               myInlineTextInput("tbf3","", globals$currentCmdDefs["tbf3"], size=10),
               myInlineTextInput("tbf4","", globals$currentCmdDefs["tbf4"], size=10),
               myInlineTextInput("tbf5","", globals$currentCmdDefs["tbf5"], size=10),
               myInlineTextInput("tbf6","", globals$currentCmdDefs["tbf6"], size=10),
               myInlineTextInput("tbf7","", globals$currentCmdDefs["tbf7"], size=10))),
      myInlineTextInput("f4", "Proportion of trees left (spacing adjustment, 1-CutEff) ", globals$currentCmdDefs["f4"]),
      HTML(paste0("<b>","Specify tree size limits of thinning","</b>")),
      myInlineTextInput("f5","Diameter lower limits (inches) ",globals$currentCmdDefs["f5"]),
      myInlineTextInput("f6", "Diameter upper limits (inches) ", globals$currentCmdDefs["f6"]),
      myInlineTextInput("f7", "Height lower limits (feet) ", globals$currentCmdDefs["f7"]),
      myInlineTextInput("f8", "Height upper limits (feet) ", globals$currentCmdDefs["f8"])
    ),
  list(br()))
  ans
}

  observe({
    if(length(input$tbf2)==0) return()
    if(input$tbf2 == "1" || input$tbf2 == "2") {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#tbf3').prop('disabled',false)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#tbf4').prop('disabled',false)"))
      
      if(input$tbf2 == "1" && input$tbf3 > 0 && input$tbf4 <= 0){
        updateTextInput(session=session,inputId ="tbf4",
                        value=round(sqrt(43560/as.numeric(input$tbf3)),digits=4))
      }
      if(input$tbf2 == "2" && input$tbf3 <= 0 && input$tbf4 > 0){
        updateTextInput(session=session,inputId ="tbf3",
                        value=round(43560/(as.numeric(input$tbf4)^2),digits=2))
      }
      if(input$tbf2 == "1" && input$tbf3 > 0 && input$tbf4 >0){
        updateTextInput(session=session,inputId ="tbf4",
                        value=round(sqrt(43560/as.numeric(input$tbf3)),digits=4))
      }
      if(input$tbf2 == "2" && input$tbf3 > 0 && input$tbf4 >0){
        updateTextInput(session=session,inputId ="tbf3",
                        value=round(43560/(as.numeric(input$tbf4)^2),digits=2))
      }

    } else {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#tbf3').prop('disabled',true)"))
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#tbf4').prop('disabled',true)"))
    }
    if(input$tbf2 == "3") {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#tbf5').prop('disabled',false)"))
    } else {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#tbf5').prop('disabled',true)"))
    }
    if(input$tbf2 == "4") {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#tbf6').prop('disabled',false)"))
    } else {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#tbf6').prop('disabled',true)"))
    }
    if(input$tbf2 == "5") {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#tbf7').prop('disabled',false)"))
    } else {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#tbf7').prop('disabled',true)"))
    }
  })
 
ThinFromBelowWin.mkKeyWrd <- function(input,output)
{
  cat ("in ThinFromBelowWin.mkKeyWrd, input=",c(f1=input$f1,f2=input$tbf2,
      f3=(switch(input$tbf2,"1"=input$tbf3,"2"=input$tbf4,"3"=input$tbf5,"4"=input$tbf6,"5"=input$tbf7)),
        f4=input$f4,f5=input$f5,f6=input$f6,f7=input$f7,f8=input$f8),"\n")
  ans<- list(ex="base",
       kwds = if (input$tbf2 == "1") sprintf(
         paste0("ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
         input$f1, input$tbf3, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) 
         else if(input$tbf2 == "2")
         sprintf(paste0("ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
         input$f1, round(43560/(as.numeric(input$tbf4)^2),digits=2), 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) 
         else if(input$tbf2 == "3")
         sprintf(paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n"),
         input$f1, input$tbf5, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) 
         else if(input$tbf2 == "4")
         sprintf(paste0("ThinBTA   %10s  Parms(BTPA*%s,%s,%s,%s,%s,%s)\n"),
         input$f1, as.numeric(input$tbf6)/100, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8)
         else 
         sprintf(paste0("ThinBBA   %10s  Parms(BBA*%s,%s,%s,%s,%s,%s)\n"),
         input$f1, as.numeric(input$tbf7)/100, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8),
       reopn = c(f1=input$f1,tbf2=input$tbf2,
                 tbf3=(switch(input$tbf2,"1"=input$tbf3,"2"=input$tbf4,"3"=input$tbf5,"4"=input$tbf6,"5"=input$tbf7)),
                 f4=input$f4,f5=input$f5,f6=input$f6,f7=input$f7,f8=input$f8)

  )
  list(br())
  varc <- switch(input$tbf2,"1"="tbf3","2"="tbf4","3"="tbf5","4"="tbf6","5"="tbf7")
  names(ans$reopn)[3] <- varc
  as.list(ans)
}

ThinFromAboveWin <- function(title, prms, fvsRun, globals,session=session) 
{
  pknum = match("management.Thin",names(prms))
  globals$currentCmdPkey = as.character(pknum) #point to the pkeys.
  globals$currentCmdDefs <- c(f1=" ",taf2="1",taf3="0",taf4="0",taf5="0",taf6="0",taf7="0",
                              f4="0",f5="0",f6="999",f7="0",f8="999")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in intersect(names(globals$currentCmdDefs),names(globals$currentEditCmp$reopn))) 
      if(globals$currentEditCmp$reopn[name] != "") globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]

cat ("in ThinFromAboveWin code, globals$currentCmdDefs=",globals$currentCmdDefs,"\n")  
  
  ans <-list(
    list(
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      HTML(paste0("<b>","Specify residual density","</b>")),
      fixedRow(
        tags$head(tags$script(HTML('
             Shiny.addCustomMessageHandler("jsCode",
             function(message) {
             eval(message.code);
             }
             );
             '))),
        column(width=5,       
               radioButtons("taf2","",
                            choices=list("Trees per acre"="1","Trees spacing (feet)"="2",
                                         "Basal area per acre "="3",
                                         "Percent of trees per acre at year of thin"="4",
                                         "Percent of basal area at year of thin"="5"),
                            selected=globals$currentCmdDefs["taf2"],inline=FALSE)
        ),
        column(width=5,      
               myInlineTextInput("taf3","", globals$currentCmdDefs["taf3"], size=10),
               myInlineTextInput("taf4","", globals$currentCmdDefs["taf4"], size=10),
               myInlineTextInput("taf5","", globals$currentCmdDefs["taf5"], size=10),
               myInlineTextInput("taf6","", globals$currentCmdDefs["taf6"], size=10),
               myInlineTextInput("taf7","", globals$currentCmdDefs["taf7"], size=10))),
      myInlineTextInput("f4", "Proportion of trees left (spacing adjustment, 1-CutEff) ", globals$currentCmdDefs["f4"]),
      HTML(paste0("<b>","Specify tree size limits of thinning","</b>")),
      myInlineTextInput("f5","Diameter lower limits (inches) ",globals$currentCmdDefs["f5"]),
      myInlineTextInput("f6", "Diameter upper limits (inches) ", globals$currentCmdDefs["f6"]),
      myInlineTextInput("f7", "Height lower limits (feet) ", globals$currentCmdDefs["f7"]),
      myInlineTextInput("f8", "Height upper limits (feet) ", globals$currentCmdDefs["f8"])
    ),
    list(br()))
  ans
} 

observe({
  if(length(input$taf2)==0) return()
  if(input$taf2 == "1" || input$taf2 == "2") {
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#taf3').prop('disabled',false)"))
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#taf4').prop('disabled',false)"))
    
    if(input$taf2 == "1" && input$taf3 > 0 && input$taf4 <= 0){
      updateTextInput(session=session,inputId ="taf4",
                      value=round(sqrt(43560/as.numeric(input$taf3)),digits=4))
    }
    if(input$taf2 == "2" && input$taf3 <= 0 && input$taf4 > 0){
      updateTextInput(session=session,inputId ="taf3",
                      value=round(43560/(as.numeric(input$taf4)^2),digits=2))
    }
    if(input$taf2 == "1" && input$taf3 > 0 && input$taf4 >0){
      updateTextInput(session=session,inputId ="taf4",
                      value=round(sqrt(43560/as.numeric(input$taf3)),digits=4))
    }
    if(input$taf2 == "2" && input$taf3 > 0 && input$taf4 >0){
      updateTextInput(session=session,inputId ="taf3",
                      value=round(43560/(as.numeric(input$taf4)^2),digits=2))
    }
    
  } else {
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#taf3').prop('disabled',true)"))
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#taf4').prop('disabled',true)"))
  }
  if(input$taf2 == "3") {
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#taf5').prop('disabled',false)"))
  } else {
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#taf5').prop('disabled',true)"))
  }
  if(input$taf2 == "4") {
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#taf6').prop('disabled',false)"))
  } else {
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#taf6').prop('disabled',true)"))
  }
  if(input$taf2 == "5") {
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#taf7').prop('disabled',false)"))
  } else {
    session$sendCustomMessage(type="jsCode",
                              list(code= "$('#taf7').prop('disabled',true)"))
  }
})


ThinFromAboveWin.mkKeyWrd <- function(input,output)
{
  cat ("in ThinFromAboveWin.mkKeyWrd, input=",c(f1=input$f1,f2=input$taf2,
                                                f3=(switch(input$taf2,"1"=input$taf3,"2"=input$taf4,"3"=input$taf5,"4"=input$taf6,"5"=input$taf7)),
                                                f4=input$f4,f5=input$f5,f6=input$f6,f7=input$f7,f8=input$f8),"\n")
  ans<- list(ex="base",
             kwds = if (input$taf2 == "1") sprintf(
               paste0("ThinATA   %10s%10s%10s%10s%10s%10s%10s\n"),
               input$f1, input$taf3, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) 
             else if(input$taf2 == "2")
               sprintf(paste0("ThinATA   %10s%10s%10s%10s%10s%10s%10s\n"),
                       input$f1, round(43560/(as.numeric(input$taf4)^2),digits=2), 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) 
             else if(input$taf2 == "3")
               sprintf(paste0("ThinABA   %10s%10s%10s%10s%10s%10s%10s\n"),
                       input$f1, input$taf5, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8) 
             else if(input$taf2 == "4")
               sprintf(paste0("ThinATA   %10s  Parms(BTPA*%s,%s,%s,%s,%s,%s)\n"),
                       input$f1, as.numeric(input$taf6)/100, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8)
             else 
               sprintf(paste0("ThinABA   %10s  Parms(BBA*%s,%s,%s,%s,%s,%s)\n"),
                       input$f1, as.numeric(input$taf7)/100, 1-as.numeric(input$f4), input$f5, input$f6, input$f7, input$f8),
             reopn = c(f1=input$f1,taf2=input$taf2,
                       taf3=(switch(input$taf2,"1"=input$taf3,"2"=input$taf4,"3"=input$taf5,"4"=input$taf6,"5"=input$taf7)),
                       f4=input$f4,f5=input$f5,f6=input$f6,f7=input$f7,f8=input$f8)
             
  )
  list(br())
  varc <- switch(input$taf2,"1"="taf3","2"="taf4","3"="taf5","4"="taf6","5"="taf7")
  names(ans$reopn)[3] <- varc
  as.list(ans)
}

##-----------------------------------Seed Tree-----------------------------------------------##

SeedTreeWin <- function(title, prms, fvsRun, globals,session=session) 
{
  pknum = match("management.Seedtree",names(prms))
  globals$currentCmdPkey = as.character(pknum) #point to the pkeys.
  globals$currentCmdDefs <- c(f1=" ",stf2="5",stf3="2",stf4="200",stf5="3",f6="60",
                              f7="10",f8="5", stf9="2", f10="10", f11="2", f12="6")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in intersect(names(globals$currentCmdDefs),names(globals$currentEditCmp$reopn))) 
      if(globals$currentEditCmp$reopn[name] != "") globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
cat ("in SeedTreeWin code, globals$currentCmdDefs=",globals$currentCmdDefs,"\n")  
  ans <- list(
    list(
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      div(style="background-color: rgb(240,240,255)",
          myInlineTextInput("stf2", "Smallest diameter cut in prep and seed cuts ", 
          globals$currentCmdDefs["stf2"])),
      div(style="background-color: rgb(255,240,240)",
          radioButtons("stf3", "Perform prep cut?", c("Yes"="1","No"="2"),
          globals$currentCmdDefs["stf3"],inline=TRUE)),
      div(style="background-color: rgb(240,240,255)",
          uiOutput("SeedTreePrepCut"),
          myInlineTextInput("f8", "Seed cut residual trees (cut from below) ", 
          globals$currentCmdDefs["f8"])),
      div(style="background-color: rgb(255,255,240)",
          radioButtons("stf9", "Perform removal cut?", c("Yes"="1","No"="2"),
          globals$currentCmdDefs["stf9"],inline=TRUE)),
      uiOutput("SeedTreeWinMin") 
    ),
    list(br()))

  observe({
cat ("stf9 observer, length(input$stf9)=",length(input$stf9),"\n")
    if (length(input$stf9) == 0) return()
cat ("stf9 observer, input$stf9=",input$stf9,"\n")
    output$SeedTreeWinMin = renderUI(list(if(input$stf9==1)
      list(div(style="background-color: rgb(255,255,240)",
               myInlineTextInput("f10", "scheduled how many years after seed cut?", 
                 globals$currentCmdDefs["f10"]),
               myInlineTextInput("f11", "Removal cut residual trees (cut from below)", 
                 globals$currentCmdDefs["f11"]), 
               myInlineTextInput("f12", "Smallest diameter cut in removal cut", 
                 globals$currentCmdDefs["f12"])))))
  })
  
  observe({
cat ("stf3 observer, length(input$stf3)=",length(input$stf3),"\n")
    if (length(input$stf3) == 0) return()
cat ("stf3 observer, input$stf3=",input$stf3,"\n")
    output$SeedTreePrepCut = if(input$stf3=="1")
    {
      stObs5$resume()
      renderUI(list(div(style="background-color: rgb(255,240,240)",
          myInlineTextInput("stf4", "Specify residual density ", value=globals$currentCmdDefs["stf4"]),
            radioButtons("stf5", "in terms of: ", 
              list("Basal area per acre"="3",
                   "Percent of maximum SDI in year of prep cut"="4"), globals$currentCmdDefs["stf5"])),
              HTML("<b>Seed cut</b>"),
                   myInlineTextInput("f7", "scheduled how many years after prep cut?", value=globals$currentCmdDefs["f7"]))
        )
    } else {
      stObs5$suspend()      
      renderUI(list(HTML("<b>Seed cut</b>")))
    }
  })
  ans
}

stObs5 = observe({
cat ("stf5 observer, length(input$stf5)=",length(input$stf5),"\n")
  if (length(input$stf5) == 0) return()
  isolate({
  if (length(input$stf4) == 0) return()
cat ("stf5 observer, input$stf5=",input$stf5,"\n")
  updateTextInput(session=session,inputId ="stf4",value=
                    switch(input$stf5,"3"="200","4"="60"))
  })
},suspended = TRUE)
  


SeedTreeWin.mkKeyWrd <- function(input,output)
{
  cat ("in SeedTreeWin.mkKeyWrd, input=",c(f1=input$f1,f2=input$stf2,
      f3=input$stf3,stf4=input$stf4,stf5=input$stf5,f6=input$f6,f7=input$f7,
      f8=input$f8,stf9=input$stf9,f10=input$f10,f11=input$f11,f12=input$f12,"\n"))
  
  kwds = if (input$stf3 == "1" && input$stf5 == "3" && input$stf9 == "1") # Prep cut=Yes, Residual BA/AC, Removal cut=Yes
    sprintf(
      paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$stf4, 1, input$stf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f8, 1, input$stf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)+as.numeric(input$f10)), input$f11, 1, input$f12, 999, 0, 999)
  else if (input$stf3 == "1" && input$stf5 == "3" && input$stf9 == "2") # Prep cut=Yes, Residual BA/AC, Removal cut=No
    sprintf(
      paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$stf4, 1, input$stf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f8, 1, input$stf2, 999, 0, 999)
  else if (input$stf3 == "1" && input$stf5 == "4" && input$stf9 == "1") # Prep cut=Yes, Residual % of MaxSDI, Removal cut=Yes
    sprintf(
      paste0("ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, (as.numeric((input$stf4))/100), 1, 0, input$stf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f8, 1, input$stf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)+as.numeric(input$f10)), input$f11, 1, input$f12, 999, 0, 999)
  else if (input$stf3 == "1" && input$stf5 == "4" && input$stf9 == "2") # Prep cut=Yes, Residual % of MaxSDI, Removal cut=No
    sprintf(
      paste0("ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, (as.numeric((input$stf4))/100), 1, 0, input$stf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f8, 1, input$stf2, 999, 0, 999)
  else if (input$stf3 == "2" && input$stf9 == "1") # Prep cut=No, Residual BA/AC, Removal cut=Yes
    sprintf(
      paste0("ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$f8, 1, input$stf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f10)), input$f11, 1, input$f12, 999, 0, 999)
  else 
    sprintf(
      paste0("ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),input$f1, input$f8, 1, input$stf2, 999, 0, 999)
  
  list(ex="base",kwds=kwds,
       reopn = c(f1=input$f1,stf2=input$stf2,stf3=input$stf3,stf4=input$stf4,stf5=input$stf5,f6=input$f6,f7=input$f7,f8=input$f8,
                 stf9=input$stf9,f10=input$f10,f11=input$f11,f12=input$f12))
}

##-----------------------------------Shelterwood----------------------------------------------##

ShelterwoodWin <- function(title, prms, fvsRun, globals,session=session) 
{
  pknum = match("management.Shelterwood",names(prms))
  globals$currentCmdPkey = as.character(pknum) #point to the pkeys.
  globals$currentCmdDefs <- c(f1=" ",swf2="5",swf3="2",swf4="200",swf5="3",f6="60", f7="10",f8="2", f9="10", f10="2", f11="6", f12="50", swf13="6")
  if (!identical(globals$currentEditCmp,globals$NULLfvsCmp))
    for (name in intersect(names(globals$currentCmdDefs),names(globals$currentEditCmp$reopn))) 
      if(globals$currentEditCmp$reopn[name] != "") globals$currentCmdDefs[name] = globals$currentEditCmp$reopn[name]
  
  cat ("in ShelterwoodWin code, globals$currentCmdDefs=",globals$currentCmdDefs,"\n")  
  ans <- list(
    list(
      mkScheduleBox("f1",prms,NULL,fvsRun,globals),
      div(style="background-color: rgb(240,240,255)",
          myInlineTextInput("swf2", "Smallest diameter cut in prep and shelterwood cuts ", globals$currentCmdDefs["swf2"])),
      div(style="background-color: rgb(255,240,240)",
          radioButtons("swf3", "Perform prep cut?", c("Yes"="1","No"="2"),globals$currentCmdDefs["swf3"],inline=TRUE)),
      div(style="background-color: rgb(240,240,255)",uiOutput("ShelterwoodPrepCut"),
          myInlineTextInput("f12", "Specify residual density ", globals$currentCmdDefs["f12"]),
          radioButtons("swf13", "in terms of: ", c("Basal area per acre"="5","Trees per acre"="6",
                                                   "Percent of maximum SDI in year of prep cut"="7"),globals$currentCmdDefs["swf13"])),
      div(style="background-color: rgb(255,255,240)",
          radioButtons("f8", "Perform removal cut?", c("Yes"="1","No"="2"),globals$currentCmdDefs["f8"],inline=TRUE)),
      uiOutput("ShelterwoodWinMin") 
    ),
    list(br()))
  
  observe(
    {
      output$ShelterwoodWinMin = renderUI(list(if(input$f8==1)
        list(div(style="background-color: rgb(255,255,240)",
                 myInlineTextInput("f9", "scheduled how many years after shelterwood cut?", globals$currentCmdDefs["f9"]),
                 myInlineTextInput("f10", "Removal cut residual trees (cut from below)", globals$currentCmdDefs["f10"]), 
                 myInlineTextInput("f11", "Smallest diameter cut in removal cut", globals$currentCmdDefs["f11"])))))
      
      if(length(input$swf3)==0) return()
      output$ShelterwoodPrepCut = if(input$swf3==1)
        renderUI(list(div(style="background-color: rgb(255,240,240)",
                          myInlineTextInput("swf4", "Specify residual density ", globals$currentCmdDefs["swf4"]),
                          radioButtons("swf5", "in terms of: ", c("Basal area per acre"="3",
                                                                  "Percent of maximum SDI in year of prep cut"="4"),globals$currentCmdDefs["swf5"])),
                      HTML(paste0("<b>","Shelterwood cut","</b>")),
                      myInlineTextInput("f7", "scheduled how many years after prep cut?", globals$currentCmdDefs["f7"]))
        )
      else
        renderUI(list(HTML(paste0("<b>","Shelterwood cut","</b>"))))
    })
  ans
}

observe({
  if(length(input$swf3)==0) return()
  if(input$swf3==1 && is.null(input$swf4)){
    updateTextInput(session=session,inputId ="swf4",value="200")
    updateTextInput(session=session,inputId ="swf5",value="3")
  }
  if(!is.null(input$swf4) && input$swf4=="200"){
    updateTextInput(session=session,inputId ="swf4",value=
                      switch(input$swf5,"3"="200","4"="60"))}
  else if(!is.null(input$swf4) && input$swf4=="60"){
    updateTextInput(session=session,inputId ="swf4",value=
                      switch(input$swf5,"3"="200","4"="60"))
  }
  else{updateTextInput(session=session,inputId ="swf4",value=input$swf4)}
  updateTextInput(session=session,inputId ="f12",value=
                    switch(input$swf13,"5"="100","6"="50", "7"="20"))
})

ShelterwoodWin.mkKeyWrd <- function(input,output)
{
  cat ("in ShelterwoodWin.mkKeyWrd, input=",c(f1=input$f1,f2=input$swf2,f3=input$swf3,swf4=input$swf4,
                                              swf5=input$swf5,f6=input$f6,f7=input$f7,f8=input$f8,
                                              f9=input$f9,f10=input$f10,f11=input$f11,f12=input$f12,swf13=input$swf13,"\n"))
  kwds = 
    # Prep cut=Yes, Residual BA/AC, Shelterwood residual BA/AC, Removal cut=Yes
    if (input$swf3 == "1" && input$swf5 == "3" && input$f8 == "1" && input$swf13 == "5") 
      sprintf(
        paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
               "ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
               "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
        input$f1, input$swf4, 1, input$swf2, 999, 0, 999,
        (as.numeric(input$f1)+as.numeric(input$f7)), input$f12, 1, input$swf2, 999, 0, 999,
        (as.numeric(input$f1)+as.numeric(input$f7)+as.numeric(input$f9)), input$f10, 1, input$f11, 999, 0, 999)
  # Prep cut=Yes, Residual BA/AC,Shelterwood residual BA/AC, Removal cut=No
  else if (input$swf3 == "1" && input$swf5 == "3" && input$f8 == "2" && input$swf13 == "5") 
    sprintf(
      paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$swf4, 1, input$swf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f12, 1, input$swf2, 999, 0, 999)
  # Prep cut=Yes, Residual BA/AC, Shelterwood residual TPA, Removal cut=Yes
  else if (input$swf3 == "1" && input$swf5 == "3" && input$f8 == "1" && input$swf13 == "6") 
    sprintf(
      paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$swf4, 1, input$swf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f12, 1, input$swf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)+as.numeric(input$f9)), input$f10, 1, input$f11, 999, 0, 999)
  # Prep cut=Yes, Residual BA/AC,Shelterwood residual TPA, Removal cut=No
  else if (input$swf3 == "1" && input$swf5 == "3" && input$f8 == "2" && input$swf13 == "6") 
    sprintf(
      paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$swf4, 1, input$swf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f12, 1, input$swf2, 999, 0, 999)
  # Prep cut=Yes, Residual BA/AC, Shelterwood residual % of MaxSDI, Removal cut=Yes
  else if (input$swf3 == "1" && input$swf5 == "3" && input$f8 == "1" && input$swf13 == "7") 
    sprintf(
      paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$swf4, 1, input$swf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)), (as.numeric((input$f12))/100), 1, 0, input$swf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f7)+as.numeric(input$f9)), input$f10, 1, input$f11, 999, 0, 999)
  # Prep cut=Yes, Residual BA/AC, Shelterwood residual % of MaxSDI, Removal cut=No
  else if (input$swf3 == "1" && input$swf5 == "3" && input$f8 == "2" && input$swf13 == "7") 
    sprintf(
      paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n"),
      input$f1, input$swf4, 1, input$swf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)), (as.numeric((input$swf4))/100), 1, 0, input$swf2, 999, 0)
  # Prep cut=Yes, Residual % of MaxSDI, Shelterwood residual BA/AC, Removal cut=Yes
  else if (input$swf3 == "1" && input$swf5 == "4" && input$f8 == "1" && input$swf13 == "5") 
    sprintf(
      paste0("ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, (as.numeric((input$swf4))/100), 1, 0, input$swf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f12, 1, input$swf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)+as.numeric(input$f9)), input$f10, 1, input$f11, 999, 0, 999)
  # Prep cut=Yes, Residual % of MaxSDI, Shelterwood residual BA/AC, Removal cut=No
  else if (input$swf3 == "1" && input$swf5 == "4" && input$f8 == "2" && input$swf13 == "5") 
    sprintf(
      paste0("ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, (as.numeric((input$swf4))/100), 1, 0, input$swf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f12, 1, input$swf2, 999, 0, 999)
  # Prep cut=Yes, Residual % of MaxSDI, Shelterwood residual TPA, Removal cut=Yes
  else if (input$swf3 == "1" && input$swf5 == "4" && input$f8 == "1" && input$swf13 == "6") 
    sprintf(
      paste0("ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, (as.numeric((input$swf4))/100), 1, 0, input$swf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f12, 1, input$swf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f7)+as.numeric(input$f9)), input$f10, 1, input$f11, 999, 0, 999)
  # Prep cut=Yes, Residual % of MaxSDI, Shelterwood residual TPA, Removal cut=No
  else if (input$swf3 == "1" && input$swf5 == "4" && input$f8 == "2" && input$swf13 == "6") 
    sprintf(
      paste0("ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, (as.numeric((input$swf4))/100), 1, 0, input$swf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f7)), input$f12, 1, input$swf2, 999, 0, 999)
  # Prep cut=Yes, Residual % of MaxSDI, Shelterwood residual % of MaxSDI, Removal cut=Yes
  else if (input$swf3 == "1" && input$swf5 == "4" && input$f8 == "1" && input$swf13 == "7") 
    sprintf(
      paste0("ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, (as.numeric((input$swf4))/100), 1, 0, input$swf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f7)), (as.numeric((input$f12))/100), 1, 0, input$swf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f7)+as.numeric(input$f9)), input$f10, 1, input$f11, 999, 0, 999)
  # Prep cut=Yes, Residual % of MaxSDI, Shelterwood residual % of MaxSDI, Removal cut=Yes
  else if (input$swf3 == "1" && input$swf5 == "4" && input$f8 == "2" && input$swf13 == "7") 
    sprintf(
      paste0("ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n"),
      input$f1, (as.numeric((input$swf4))/100), 1, 0, input$swf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f7)), (as.numeric((input$f12))/100), 1, 0, input$swf2, 999, 0)
  # Prep cut=No,Shelterwood residual BA/AC, Removal cut=Yes
  else if (input$swf3 == "2" && input$f8 == "1" && input$swf13 == "5") 
    sprintf(
      paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$f12, 1, input$swf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f9)), input$f10, 1, input$f11, 999, 0, 999)
  # Prep cut=No,Shelterwood residual BA/AC, Removal cut=No
  else if (input$swf3 == "2" && input$f8 == "2" && input$swf13 == "5") 
    sprintf(
      paste0("ThinBBA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$f12, 1, input$swf2, 999, 0, 999)
  # Prep cut=No,Shelterwood residual TPA, Removal cut=Yes
  else if (input$swf3 == "2" && input$f8 == "1" && input$swf13 == "6") 
    sprintf(
      paste0("ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$f12, 1, input$swf2, 999, 0, 999,
      (as.numeric(input$f1)+as.numeric(input$f9)), input$f10, 1, input$f11, 999, 0, 999)
  # Prep cut=No,Shelterwood residual TPA, Removal cut=No
  else if (input$swf3 == "2" && input$f8 == "2" && input$swf13 == "6") 
    sprintf(
      paste0("ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, input$f12, 1, input$swf2, 999, 0, 999)
  # Prep cut=No,Shelterwood residual % of MaxSDI, Removal cut=Yes
  else if (input$swf3 == "2" && input$f8 == "1" && input$swf13 == "7") 
    sprintf(
      paste0("ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n",
             "ThinBTA   %10s%10s%10s%10s%10s%10s%10s\n"),
      input$f1, (as.numeric((input$f12))/100), 1, 0, input$swf2, 999, 0,
      (as.numeric(input$f1)+as.numeric(input$f9)), input$f10, 1, input$f11, 999, 0, 999)
  # Prep cut=No,Shelterwood residual % of MaxSDI, Removal cut=No
  else 
    sprintf(
      paste0("ThinSDI   %10s  Parms(BSDIMax*%s,%s,%s,%s,%s,%s)\n"),
      input$f1, (as.numeric((input$f12))/100), 1, 0, input$swf2, 999, 0)
  
  list(ex="base",kwds=kwds,
       reopn = c(f1=input$f1,swf2=input$swf2,swf3=input$swf3,swf4=input$swf4,swf5=input$swf5,f6=input$f6,f7=input$f7,
                 f8=input$f8,f9=input$f9,f10=input$f10,f11=input$f11,f12=input$f12,swf13=input$swf13))
}




