mkeltList <- function (pkeys,prms,globals,input,output,
                       cndflag=FALSE,funcflag=FALSE,comptitle=NULL)
{
  waitYears <- NULL
  eltList <- if (cndflag) 
  {
    waityrs <- getPstring(pkeys,"waitYears",globals$activeVariants[1])
    if (is.null(waitYears)) waitYears <- "999"
    fpvs <- if (identical(globals$currentEditCmp,globals$NULLfvsCmp)) NULL else
            globals$currentEditCmp$reopn["waitYears"]    
    list(
      h5("Settings for the condition:"),    
      mkTextInput ("waitYears", "Years before condition can become true again:",
                   waityrs, fpvs))
  } else list()
  f = 0
  repeat
  {
    f = f+1
    pkey = paste0("f",f)
    fps = getPstring(pkeys,pkey,globals$activeVariants[1])                
    if (is.null(fps)) break
    elt = unlist(strsplit(fps," "))
    if (length(elt) > 1)
    {
      elt = elt[1]
      pmt = sub(paste0(elt," "),"",fps)
    } else
    {
      elt = fps
      pmt = NULL
    }
    title = getPstring(pkeys,paste0(pkey,"title"),globals$activeVariants[1]) 
    if (!is.null(title)) eltList <- append(eltList,
        list(HTML(paste0("<p><b>",gsub("\n","<br/>",title),"<b/><p/>"))))
    fpvs <- if (identical(globals$currentEditCmp,globals$NULLfvsCmp)) NULL else
            globals$currentEditCmp$reopn[f] 
    if (!is.null(fpvs) && is.na(fpvs)) fpvs = NULL
    choices <- getPstring(pkeys,paste0(pkey,"v"),globals$activeVariants[1]) 
    if (cndflag) pkey = paste0("cnd.",pkey)
    else if (funcflag) pkey = paste0("func.",pkey)
cat ("mkeltList title=",title,"\nf=",f," elt=",elt," pkey=",pkey," pmt=",pmt,
 "\nglobals$activeVariants[1]=",globals$activeVariants[1]," fpvs=",fpvs,"\n")
    elt = switch(elt,
      listButton     = mkSelectInput (pkey, pmt, choices, fpvs),
      longListButton = mkSelectInput (pkey, pmt, choices, fpvs),
      longListButtonString = mkSelectInput (pkey, pmt, choices, fpvs, valpair=TRUE),
      listButtonString = mkSelectInput (pkey, pmt, choices, fpvs, valpair=TRUE),
      checkBox       = mkCheckBox (pkey, pmt, choices, fpvs),
      radioGroup     = mkSelectInput (pkey, pmt, choices, fpvs, type="radiogroup"),
      sliderBox      = mkTextInput (pkey, pmt, choices, fpvs),  
      numberBox      = mkTextInput (pkey, pmt, choices, fpvs), 
      intNumberBox   = mkTextInput (pkey, pmt, choices, fpvs), 
      textEdit       = mkTextInput (pkey, pmt, choices, fpvs), 
      longTextEdit   = mkTextInput (pkey, pmt, choices, fpvs),
      mkVarList      = myInlineListButton (pkey, pmt, mkVarList(globals), selected=fpvs, deltll=2),
      forestSelection = mkSelForest(pkey,prms,pmt,fpvs,choices,globals),
      habPaSelection = mkSelhabPa(pkey,prms,pmt,fpvs,choices,globals),
      fileBrowse     = {
              choices = gsub("xls$","db",choices)
              mkTextInput (pkey, pmt, choices, fpvs) }, 
      speciesSelection = mkSelSpecies(pkey,prms,pmt,fpvs,choices,globals),
      scheduleBox = mkScheduleBox(pkey,prms,pmt,globals,input,output),
      noInput = list(div(id=pkey,HTML(paste0("<p><b>",gsub("\n","<br/>",pmt),"<b/><p/>")))),
      NULL)
    if (!is.null(elt)) eltList <- append(eltList,list(elt))
  }
  if (cndflag) 
  {
    if (length(eltList) == 1) eltList <- append(eltList, 
      list(h6("No settings for this condition.")))
  }
  if( !is.null(comptitle)) mkTitle(globals,comptitle,output)
  eltList
}

mkTitle <- function(globals,title,output)
{
  if(length(globals$currentEditCmp$atag) && globals$currentEditCmp$atag=="c"){
  rtn <- list(h5(),div(myInlineTextInput("cmdTitle","Condition title ", value=title,size=40)),h5())
  } else 
    rtn <- list(h5(),div(myInlineTextInput("cmdTitle","Component title ", value=title,size=40)),h5())
  if(length(globals$currentEditCmp$title)) rtn <- append(rtn,list(
        h4(paste0('Edit: "',globals$currentEditCmp$title),'"')),after=0)  
  output$titleBuild <- renderUI(rtn)
}

mkTextInput <- function (pkey, pmt, choices, fpvs) 
{
  if (!is.null(fpvs)) choices = fpvs 
  tmp <- if (is.null(choices)) " " else scan(text=choices,what=" ",quiet=TRUE)[1]
  if (length(tmp) == 0 || is.na(tmp) || tmp == "blank") tmp = " " 
  myInlineTextInput(pkey, pmt, tmp)
}


mkCheckBox <- function (pkey, pmt, choices, fpvs) 
{
  if (is.null(fpvs)) fpvs = choices
  tmp <- if (is.null(fpvs)) FALSE else (as.character(fpvs) == "1" ||
                                        fpvs == "TRUE")
  checked <- if (tmp) "checked=\"checked\"" else ""
  list(HTML(paste0('<div class="form-group shiny-input-checkboxgroup 
  shiny-input-container">', '<div calss="checkbox">','<label>
  <input id="',pkey,'" type="checkbox" ',checked, ' value="',fpvs,
  '">&nbsp;',pmt,'</label>', '</div>', '</div>')))
}


mkSelectInput <- function (inputId, label, choices, fpvs, 
                           type="list", valpair=FALSE)
{
  choices = trim(scan(text=choices,what=" ",sep="\n",quiet=TRUE))
  sel = grep ("^>",choices)
  if (length(sel)) choices[sel] = trim(substring(choices[sel],2))
  edt <- 0
  if (! (is.null(fpvs) || is.na(fpvs)))
  {
    sel = if (is.na(suppressWarnings(as.numeric(fpvs)))) 
      grep (paste0("^",fpvs),choices) else fpvs
    edt <- 1
  } 
cat ("in mkSelectInput type=",type," fpvs=",fpvs," sel=",sel,"\n")
  mklist = if (valpair)
    lapply(choices, function (x) trim(unlist(strsplit(x,"="))[1])) else
      as.list(as.character(0:(length(choices)-1)))
  names(mklist) = choices
  
  if (length(sel) && edt==0) 
    {
      if (sel==length(choices)) sel <- sel-1
      if (!valpair) sel = match(as.character(sel),mklist) 
      if (is.na(sel)) sel <- "0" else as.character(if (valpair) sel <- sel else sel <- sel-2)
  } 
  if(!length(sel) && edt==0) sel="0"
  if(!length(sel) && edt==1 && !valpair) sel=mklist[1]
  if (valpair && is.na(mklist[1]) && edt==0) mklist[1] <- " "
  if (valpair && is.na(mklist[1]) && edt==1) mklist[1] <- sel[1]
  if (valpair && gsub('"','',mklist[1])==" "  && edt==0) sel <- as.character(as.numeric(sel)-1)
  if (valpair && gsub('"','',mklist[1])!=" " && edt==1){
    if(choices[1]==""){
    sel <- as.character(mklist[mklist[[1]][1]])
    mklist[1] <- as.character((mklist[1]))
    } else sel <- as.character(as.numeric(sel))
  }
  switch (type,
    "checkboxgroup"=checkboxGroupInput(inputId,label,mklist,selected=sel), 
    "radiogroup"=myRadioGroup(inputId,label,
         mklist,selected=sel),
     myInlineListButton (inputId, label, mklist, selected=sel,deltll=2))
}

mkSelhabPa<- function (pkey,prms,pmt,fpvs,choices,globals)
{
  variant <- globals$activeVariants[1]
  forkeys <- prms[[paste0("HabPa_",variant)]]
  choices = if (!is.null(choices)) scan(text=choices,what="character",quiet=TRUE) else NULL
  fors <- unlist(forkeys)
  names(fors) <- lapply(forkeys,function (x) attr(x,"pstring"))
  fors[1]="blank"
  myInlineListButton (pkey, pmt, fors, selected = if (is.null(fpvs)) choices else fpvs, NULL)
}

mkSelForest <- function (pkey,prms,pmt,fpvs,choices,globals)
{
  variant <- globals$activeVariants[1]
  forkeys <- prms[[paste0("Forests_",variant)]]
  choices = if (!is.null(choices)) scan(text=choices,what="character",quiet=TRUE) else NULL
  fors <- unlist(forkeys)
  names(fors) <- lapply(forkeys,function (x) attr(x,"pstring"))
  fors[1]="blank"
  myInlineListButton (pkey, pmt, fors, selected =  if (is.null(fpvs)) choices else fpvs, NULL)
}
  
    
mkSelSpecies <- function (pkey,prms,pmt,fpvs,choices,globals)
{ 
  variant <- globals$activeVariants[1]
  spGrp <- as.numeric()
  spkeys <- prms[[paste0("species_",variant)]]
  choices = if (!is.null(choices)) scan(text=choices,what="character",quiet=TRUE) else NULL
  addAll = grep ("^deleteAll",choices)
  if (length(addAll))
  { 
    spGrp <- 1
    choices = choices[-addAll]
    if (length(choices)==0) choices=NULL
    addAll = FALSE
  } else addAll = TRUE
  sps = if (addAll) list("All species") else list ()
  for (sp in spkeys) sps <- append(sps,attr(sp,"pstring"))
  dsp = if (addAll) as.list(c("All",unlist(spkeys))) else spkeys
  if (!is.null(fpvs)) 
  {
    if (fpvs == -1) 
    {
      sps <- append(sps," ",after=0)
      dsp <- append(dsp," ",after=0)
    } else choices = fpvs
  }
  if (length(globals$GenGrp)){
    if (length(globals$GrpNum)){
      sptitles<-list ()
      for(spt in 1:length(globals$GrpNum))
      {
        sptitles[spt] <- paste0("SpGroup:",globals$GenGrp[spt])
      }
      sps <- append(sps,sptitles)
      dsp <- as.list(c(unlist(dsp),as.character(globals$GenGrp)))
      names(dsp) = sps
    }
  } else
  names(dsp) = sps
  if (!is.null(fpvs)) spGrp=NULL
  if (!is.null(fpvs) && (addAll)) spGrp <- 2
  myInlineListButton (pkey, pmt, dsp, selected = choices, spGrp)
}

mkScheduleBox <- function (pkey,prms,pmt,globals,input,output)
{
  if (identical(globals$currentEditCmp,globals$NULLfvsCmp)) 
  {
cat ("mkScheduleBox schedBoxPkey is set to:",pkey,"\n")
    globals$schedBoxPkey <- pkey
    mklist <- list("Schedule by year"="1","Schedule by condition"="2")
    globals$existingCmps <- mkExistingCndsList(globals$fvsRun)
    if (length(globals$schedBoxYrLastUsed) == 0) globals$schedBoxYrLastUsed <- 
        globals$fvsRun$startyr
    if (length(globals$existingCmps)) mklist <- append(mklist,
       c("Attach to existing condition"="3"))
    rtn <- list(h5(),div(style="background-color: rgb(240,255,240)",
      radioButtons("schedbox", pmt, mklist, inline=TRUE),
      uiOutput("conditions"),
      myInlineTextInput(pkey, "Year or cycle number ", globals$fvsRun$startyr)
    ))
  } else {    
    sch <- if (globals$currentEditCmp$atag == "k") "Schedule by year " else
      {
        cn = findCmp(globals$fvsRun,globals$currentEditCmp$atag)
        paste0('Schedule by condition: "',cn$title,'"')
      }
    rtn <- list(
      h5(sch),div(style="background-color: rgb(240,255,240)",
      myInlineTextInput(pkey, 
        label = if (globals$currentEditCmp$atag == "k") 
                "Year or cycle number " else
                "Number of years after condition is found true ", 
        value = globals$currentEditCmp$reopn[pkey]))
    )
  }
  output$condBuild <- renderUI(rtn)
  NULL
}
      

mkExistingCndsList <- function (fvsRun)
{
  cmplist = list(); cmpnames = list()
  if (length(fvsRun$grps)) for (j in 1:length(fvsRun$grps)) 
  { 
    if (length(fvsRun$grps[[j]]$cmps)) 
    for (k in 1:length(fvsRun$grps[[j]]$cmps)) 
    {
      if (fvsRun$grps[[j]]$cmps[[k]]$atag == "c") 
      {
         cmplist <- append(cmplist, paste0("Group: ",
                fvsRun$grps[[j]]$grp,"; Condition: ",
                fvsRun$grps[[j]]$cmps[[k]]$title))
         cmpnames <- append(cmpnames, fvsRun$grps[[j]]$cmps[[k]]$uuid)
      }
    }
  }
  if (length(fvsRun$stands)) for (i in 1:length(fvsRun$stands)) 
  { 
    if (length(fvsRun$stands[[i]]$cmps)) 
    for (k in 1:length(fvsRun$stands[[i]]$cmps)) 
    {
      if (fvsRun$stands[[i]]$cmps[[k]]$atag == "c") 
      {
         cmplist <- append(cmplist, paste0("Stand: ",
                fvsRun$stands[[i]]$sid," Condition: ",
                fvsRun$stands[[i]]$cmps[[k]]$title))
         cmpnames <- append(cmpnames, fvsRun$stands[[i]]$cmps[[k]]$uuid)
      }
    }
  }
  names(cmpnames) <- cmplist
  cmpnames
}


myInlineTextInput <- function (inputId, label, value = "", size=10, style=NULL)
{
  style = if (!is.null(style)) paste0(' style="',style,'"') else ""
  HTML(paste0(
  '<div class="form-horizontal control-group"',style,'>',
     '<label class="control-label" for="',inputId,'">',label,'&nbsp;&nbsp;</label>', 
     '<input type="text" class="form-horizontal" id="',inputId,
     '" size="', as.character(size),'" value="',gsub('"','',value),'">',
  '</div>'))
}
myInlineNumericInput <- function (inputId, label, 
    value="0", min="0", max="10", step="1", size=10, labelstyle=NULL)
{
  labelstyle = if (is.null(labelstyle)) "" else paste0('style="',labelstyle,'"')
  HTML(paste0(
  '<div class="form-horizontal control-group">',
     '<label class="control-label" for="',inputId,'" ',labelstyle,'>',label,'&nbsp;&nbsp;</label>', 
     '<input type="number" class="form-horizontal" id="',inputId,
     '" size="', as.character(size),
     '" value="',gsub('"','',value),
     '" min="',gsub('"','',min),
     '" max="',gsub('"','',max),
     '" step="',gsub('"','',step),
     '">',
  '</div>'))
}


myRadioGroup_bkup <- function (inputId, label, mklist, selected=NULL,labelstyle=NULL)
{
  inputs = NULL
  if (is.null(names(mklist))) names(mklist) = mklist
  if (is.null(selected)) selected = mklist[1]
  for (item in 1:length(mklist))
  {
    inputs = c(inputs, paste0('<input type="radio" id="',mklist[item],'" name="',inputId,'" value="',
           gsub('"','',mklist[item]),'" ',
           if (mklist[item] == selected) "checked" else "",
           ' />','<label for="',mklist[item],'">',names(mklist)[item],'</label>',"&nbsp;&nbsp;"))
  }
  labelstyle = if (is.null(labelstyle)) "" else paste0('style="',labelstyle,'"')

  # HTML(paste0('<form>',
  #               '<fieldset>',
  #                 '<legend ',labelstyle,'>',label,'</legend>',
  #                 '<div id="',inputId,'" class="shiny-input-radiogroup">',
  #                 paste0(inputs,collapse=""),"</div>",
  #               '</filedset>',
  #               '</form>'))

  HTML(paste0('<div id="',inputId,'" class="shiny-input-radiogroup">',
    '<label for="',inputId,'" ',labelstyle,'>',label,'&nbsp;&nbsp;</label>'), 
    paste0(inputs,collapse=""),"</div>")
}

myRadioGroup <- function (inputId, label, mklist, selected=NULL,labelstyle=NULL)
{
  inputs = NULL
  if (is.null(names(mklist))) names(mklist) = mklist
  if (is.null(selected)) selected = mklist[1]
  for (item in 1:length(mklist))
  {
    inputs = c(inputs, paste0('<input type="radio" id="',mklist[item],'" name="',inputId,'" value="',
           gsub('"','',mklist[item]),'" ',
           if (mklist[item] == selected) "checked" else "",
           ' />','<label for="',mklist[item],'">',names(mklist)[item],'</label>',"&nbsp;&nbsp;"))
  }
  labelstyle = if (is.null(labelstyle)) "" else paste0('style="',labelstyle,'"')

  HTML(paste0('<fieldset>',
                '<legend ',labelstyle,'>',label,'</legend>',
                '<div id="', inputId, '" class = "shiny-input-radiogroup">',
              #  '<label for="',inputId,'" ',labelstyle,'>',label,'&nbsp;&nbsp;</label>'),
                paste0(inputs, collapse = ""), "</div>",
              '</fieldset>'))

  # HTML(paste0('<div id="',inputId,'" class="shiny-input-radiogroup">',
  #   '<label for="',inputId,'" ',labelstyle,'>',label,'&nbsp;&nbsp;</label>'), 
  #   paste0(inputs,collapse=""),"</div>")
}


myInlineListButton <- function (inputId, label, mklist, selected=NULL, deltll)
{
  inputs = NULL
  if (length(mklist))
  {
    if (is.null(selected)) selected = unlist(mklist[1])
    if ((!length(deltll) && is.null(selected))||(length(deltll) && deltll==2)){
    # all dropdowns where a blank is not allowed (no deleteAll pkey)
    # applies to most keywords, and when editing previously saved selections (deltll==2).
      # Remove duplicate SpGroup names in species dropdowns due to cut/paste
      if(names(mklist[1])=="All species"){
        spgsidxs <- grep("SpGroup", names(mklist))
        spgs <- mklist[spgsidxs]
        if(length(spgsidxs) > 1 && length(spgs)!=length(unique(spgs))){
        for (i in 1:length(spgsidxs))
          if(length(match(trim(spgs[i]),trim(spgs))))
            mklist <- mklist[-spgsidxs[i]] 
        } 
      }
    first <- 0
    for (item in 1:length(mklist))
     {
      if (trim(mklist[[item]][1]) == trim(selected) && first==0){
        tag <- "selected"
        first <- 1
        }else tag <- ""
        inputs = c(inputs, paste0('<option value="',
                                  gsub('"','',mklist[item]),'" ', tag,'>',
                                  names(mklist)[item],"</option>"))
     }
    } 
    # editing an already saved selection (deleteAll pkeys)
    # where previously saved selections are still there
    # but the first option in all other fields are blank (SpGroup)
    else if(!length(deltll) && !is.null(selected)){
    for (item in 1:length(mklist))
     {if (mklist[[item]] == selected)
       {# previously saved selections
        inputs = c(inputs, paste0('<option value="',
                                  gsub('"','',mklist[item]),'" ',
                                  if (mklist[[item]] == selected) "selected" else "",
                                  '>',names(mklist)[item],"</option>"))}
        else{# first option in all other fields are blank
          if (item==1){
              inputs = c(inputs,'<option value=" "></option>',
                         paste0('<option value="',
                         gsub('"','',mklist[item]),'" ',"",
                         '>',names(mklist)[item],"</option>"))}   
          else
              inputs = c(inputs,paste0('<option value="',
                         gsub('"','',mklist[item]),'" ',"",
                         '>',names(mklist)[item],"</option>"))}
      
      }
    } 
    #initial rendering of the species list dropdown (deleteAll pkeys)
    # first option is blank (SpGroup, Plant/Natural, etc)
    else 
      for (item in 1:length(mklist))
      {if (item==1){# first option is blank
        inputs = c(inputs,'<option value=" "></option>',
                   paste0('<option value="',
                   gsub('"','',mklist[item]),'" ',"",
                   '>',names(mklist)[item],"</option>"))} 
        else 
        inputs = c(inputs,paste0('<option value="',
                   gsub('"','',mklist[item]),'" ',"",
                   '>',names(mklist)[item],"</option>"))
      }
  }
  inputs = if (is.null(inputs)) '<option value=" "></option>' else 
                                 paste0(inputs,collapse="")
  if (length(label)== 0) label=""
  selected=NULL
  if (nchar(label) > 15)
    HTML(paste0('<div id="',inputId,'" style="width:100%;" class="shiny-input-container">',
      '<label for="',inputId,'" style="max-width:50%;" ><b>',label,'&nbsp;&nbsp;</b></label>',
      '<select id="',inputId,'" style="max-width:50%;" >', 
     inputs,"</select></div>"))
  else    
    HTML(paste0('<div id="',inputId,'" style="width:400px;" class="shiny-input-container">',
      '<label for="',inputId,'" style="max-width:30%;" ><b>',label,'&nbsp;&nbsp;</b></label>',
      '<select id="',inputId,'" style="max-width:70%;" >', 
     inputs,"</select></div>"))
}

mkVarList <- function (globals)
{
  varList = c(
     " "=" ",
     "Age: Age at beginning of an FVS cycle"="Age", 
     "AgeCmp: Estimated average age for the dominant size class"="AgeCmp",
     "Aspect: Aspect in degrees"="Aspect", 
     "BaDBH: Before thin quadractic mean DBH"="BaDBH", 
     "BBA: Before thin basal area"="BBA", 
     "BBdFt: Before thin board foot (western variants) sawtimber (eastern variants) volume"="BBdFt", 
     "BCanCov: Before thin percent canopy cover (StrClass keyword required)"="BCanCov", 
     "BCCF: Before thin CCF"="BCCF", 
     "BDBHwtBA: Before thin average DBH weighted by stand basal area"="BDBHwtBA", 
     "BHTWTBA: Before thin average height weighted by stand basal area"="BHTWTBA",
     "BMaxHS: Before thin height of tallest tree in uppermost stratum (StrClass keyword required)"="BMaxHS", 
     "BMCuFt: Before thin merchantable (western variants) sawtimber (eastern variants) cubic foot volume"="BMCuFt", 
     "BMinHS: Before thin height of shortest tree in uppermost stratum (StrClass keyword required)"="BMinHS", 
     "BNumSS: Before thin number of valid strata (StrClass keyword required)"="BNumSS", 
     "BRDen: Before thin relative density (Curtis 1982)"="BRDen", 
     "BRDen2: Before thin relative density, SILVAH (Marquis and Ernst 1992)"="BRDen2", 
     "BSClass: Before thin stand structural classification (StrClass keyword required)"="BSClass", 
     "BSDI: Before thin stand density index"="BSDI", 
     "BSDI2: Before thin stand density index (based on Zeide 1983)"="BSDI2", 
     "BSDIMax: Before thin maximum stand density index"="BSDIMax", 
     "BStrDbh: Before thin dbh of the stand uppermost stratum (StrClass keyword required)"="BStrDbh", 
     "BTCuFt: Before thin total (western variants) or merchantable (pulpwood + sawtimber, eastern variants) cubic foot volume"="BTCuFt", 
     "BTopHt: Before thin top height"="BTopHt", 
     "BTPA: Before thin trees/acre"="BTPA", 
     "CEndYear: Year at end of cycle"="CEndYear", 
     "County: Stand location, 2-digit FIA county code"="County", 
     "Cycle: Cycle number"="Cycle", 
     "Elev: Stand elevation"="Elev", 
     "EvPhase: Event Montitor phase: 1=before cutting, 2=after"="EvPhase", 
     "ForTyp: FIA Forest Type code"="ForTyp", 
     "HabType: Stand habitat type code"="HabType", 
     "InvYear: Inventory year"="InvYear", 
     "Lat: The latitude of the stand"="Lat", 
     "Long: The longitude of the stand"="Long", 
     "MAI: Mean annual increment"="MAI",                                                                                         
     "No: The constant 0"="No", 
     "NumTrees: Number tree records"="NumTrees", 
     "PropStk: Proportion of stand considered stockable"="PropStk", 
     "Rann: Uniform random number"="Rann", 
     "SampWt: Stand sampling weight"="SampWt", 
     "SilvahFT: Forest type as defined in SILVAH"="SilvahFT", 
     "Site: Site index"="Site", 
     "SizCls: Size class code"="SizCls", 
     "Slope: Stand slope percent"="Slope", 
     "SMR: Stand mistletoe rating"="SMR", 
     "State: Stand location, 2-digit FIA state code"="State", 
     "StkCls: Stocking class code"="StkCls", 
     "Year: Year at the beginning of an FVS cycle"="Year", 
     "Yes: The constant 1"="Yes", 
     "AADBH: After thin average DBH"="AADBH", 
     "ABA: After thin basal area"="ABA", 
     "ABdFt: After thin board foot (western variants) sawtimber (eastern variants) volume"="ABdFt", 
     "ACanCov: After thin percent canopy cover (StrClass keyword required)"="ACanCov", 
     "ACCF: After thin CCF"="ACCF", 
     "ADBHwtBA: After thin average DBH weighted by stand basal area"="ADBHwtBA",
     "AHTWTBA: After thin average height weighted by stand basal area"="AHTWTBA",
     "AMaxHS: After thin height of tallest tree in uppermost stratum (StrClass keyword required)"="AMaxHS", 
     "AMCuFt: After thin merchantable (western variants) sawtimber (eastern variants) cubic foot volume"="AMCuFt", 
     "AMinHS: After thin height of shortest tree in uppermost stratum (StrClass keyword required)"="AMinHS", 
     "ANumSS: After thin number of valid strata (StrClass keyword required)"="ANumSS", 
     "ARDEN: After thin relative density (Curtis 1982)"="ARDEN", 
     "ARDen2: After thin relative density, SILVAH (Marquis and Ernst 1992)"="ARDen2", 
     "ASClass: After thin stand structural classification (StrClass keyword required)"="ASClass", 
     "ASDI: After thin stand density index"="ASDI", 
     "ASDI2: After thin stand density index (based on Zeide 1983)"="ASDI2", 
     "ASDIMax: After thin maximum stand density index"="ASDIMax", 
     "AStrDbh: After thin dbh of the stand uppermost stratum (StrClass keyword required)"="AStrDbh", 
     "ATCuFt: After thin total (western variants) or merchantable (pulpwood + sawtimber, eastern variants) cubic foot volume"="ATCuFt", 
     "ATopHt: After thin top height"="ATopHt", 
     "ATPA: After thin trees/acre"="ATPA", 
     "Cut: Cutting flag: 0 if no cutting, 1 otherwise"="Cut", 
     "RBdFt: Removed board feet volume"="RBdFt", 
     "RMCuFt: Removed merch cubic feet volume"="RMCuFt", 
     "RTCuFt: Removed total cubic feet volume"="RTCuFt", 
     "RTPA: Removed trees/acre"="RTPA", 
     "ACC: Accreation form last cycle"="ACC", 
     "DBA: Basal area change"="DBA", 
     "DBA%: Basal area change percent"="DBA%", 
     "DCCF: CCF change"="DCCF", 
     "DCCF%: CCF change percent"="DCCF%", 
     "DTPA: Trees/acre change"="DTPA", 
     "DTPA%: Trees/acre change percent"="DTPA%", 
     "Mort: Mortality from last cycle"="Mort", 
     "ORG%CC: ORGANON percent crown closure"="ORG%CC", 
     "ORGAHT: ORGANON top height value based on ORGANON Big-6 species"="ORGAHT", 
     "PAI: Periodic annual increment"="PAI")
  if ("fire" %in% globals$activeExtens) varList=c(varList,c(
     "Fire: 1 if there was a fire during the previous FVS cycle, 0 otherwise (FFE)"="Fire", 
     "FireYear: Year of the previous fire (FFE)"="FireYear", 
     "FisherIn: Fisher Resting Habitat Suitability Index, (FFE)"="FisherIn", 
     "CrownIdx: The crowning index reported in the potential fire report of the FFE"="CrownIdx", 
     "CrBaseHt: The crown base height reported in the potential fire report of the FFE"="CrBaseHt", 
     "CrBulkDn: The crown bulk density reported in the potential fire report of the FFE"="CrBulkDn", 
     "MinSoil: Percent mineral soil exposed (FFE)"="MinSoil", 
     "TorchIdx: The torching index reported in the potential fire report of the FFE"="TorchIdx"))
  if ("econ" %in% globals$activeExtens) varList=c(varList,c(
     "DiscCost: Accumulated discounted costs at end of previous cycle"="DiscCost", 
     "DiscRate: Discount rate"="DiscRate",                                                      
     "DiscRevn: Accumulated discounted revenues at end of previous cycle"="DiscRevn", 
     "EcBdFt: Merchantable sawtimber board foot volume valued"="EcBdFt", 
     "EcCuFt: Merchantable (western varaints) or sawtimber (eastern variants) cubic foot volume valued"="EcCuFt", 
     "ForstVal: Value of forest, land, and trees, at end of previous cycle"="ForstVal", 
     "HarvCost: Harvest cost during the previous cycle"="HarvCost", 
     "HarvRevn: Harvest revenue during the previous cycle"="HarvRevn", 
     "IRR: Internal rate of return at end of previous cycle"="IRR", 
     "PctCost: Precommercial harvest cost during the previous cycle"="PctCost", 
     "PNV: Present net value at end of previous cycle"="PNV", 
     "RprodVal: Value of trees, reprod value, at end of previous cycle"="RprodVal", 
     "SEV: Soil expectation value at end of previous cycle"="SEV", 
     "UndisCst: Accumulated undiscounted costs at end of previous cycle"="UndisCst",         
     "UndisRvn: Accumulated undiscounted revenues at end of previous cycle"="UndisRvn"))
  if ("wsbw" %in% globals$activeExtens) varList=c(varList,c(
     "BW%Stnd: The stand defoliation level caused by WSBW, pervious cycle"="BW%Stnd'", 
     "WSBWProb: Probability of a western spruce budworm outbreak"="WSBWProb"))
  if ("dftm" %in% globals$activeExtens) varList=c(varList,c(
     "DFTMProb: Probability of a tussock moth outbreak"="DFTMProb", 
     "TM%DF: The average defoliation level on Douglas-fir (tussock moth)"="TM%DF", 
     "TM%GF: The average defoliation level on grand fir (tussock moth)"="TM%GF", 
     "TM%Stnd: The stand defoliation level caused by tussock moth, previous cycle"="TM%Stnd"))
  if ("mpb" %in% globals$activeExtens) varList=c(varList,c(
     "MPBProb: Probability of a mountain pine beetle outbreak"="MPBProb", 
     "MPBTPAK: Trees killed per acre by mountain pine beetle, previous cycle"="MPBTPAK"))
  as.list(varList)
}

mkFuncList <- function (globals)
{
  funcList = c(
    " "=" ",
    "ACCFSP: After thin CCF by species"="ACCFSP",
    "Acorns: Estimated number of acorns per acre"="Acorns",
    "BCCFSP: Before thin CCF by species"="BCCFSP",
    "Bound: Returns the second argument if it is greater than the first argument and less than the third"="Bound",
    "DBHDist: Returns the diameter of the tree corresponding to the nominal percentile in the distribution of one of 11 specific attributes"="DBHDist",
    "Decade: Returns the argument the corresponds to the decade the simulation is in"="Decade",
    "HTDist: Returns the height of the tree corresponding to the nominal percentile in the trees per acre distribution"="HTDist",
    "Index: Returns the value associated with the index specified in the first argument"="Index",
    "LinInt: Returns a linear interpolation between points on a simple Y-over-X graph"="LinInt",                   
    "MaxIndex: Returns the argument index corresponding to the largest value"="MaxIndex",                
    "MinIndex: Returns the argument index corresponding to the smallest value"="MinIndex",
    "Normal: Returns a random normal variate given a mean and std. dev"="Normal",
    "PointID: Returns the inventory point number corresponding to the FVS sequential point number"="PointID",
    "SpMcDBH: Returns the trees, basal area, or one of 10 other attributes for trees of a given species, tree value class, or tree-size range"="SpMcDBH",
    "StrStat: Returns the information in the structural statistics report under before or after thinning conditions"="StrStat",
    "SumStat: Returns values from the Summary Statistics table"="SumStat",
    "Time: Returns the argument the corresponds to a specific time frame the simulation is in"="Time")
  if ("fire" %in% globals$activeExtens) 
    funcList=c(funcList,c(                                                                                                                               
    "CarbStat: Returns the carbon pools as output in the FFE carbon reports)"="CarbStat",
    "DWDVal: Returns the volume and % cover for a range of down wood size classes)"="DWDVal",
    "FuelLoad: Returns the tons/acre for a range of fuel size classes)"="FuelLoad",
    "FuelMods: Returns the fuel models and associated weights used in the fire behavior calculations)"="FuelMods",
    "HerbShrb: Returns the total herbs and shrubs in tons/acre)"="HerbShrb",
    "PotFLen: Returns the flame lengths from the potential fire report of the FFE"="PotFLen",
    "PotFMort: Returns the potential fire mortality for severe or moderate fires in terms of %BA or total cuft/acre)"="PotFMort",
    "PotFType: Returns the potential fire type for severe or moderate fires)"="PotFType",
    "PotReInt: Returns the potential fire reaction intensity for severe or moderate fires)"="PotReInt",
    "PotSRate: Returns the potential fire spread rate for severe or moderate fires)"="PotSRate",
    "SalvVol: Returns salvage volume removed by species and diameter class)"="SalvVol",
    "Snags: Returns the number=basal area=or volume of snags for a given subset of the snag list)"="Snags",
    "TreeBio: Returns tree biomass for dead and/or live standing and/or removed trees by species and size class (FFE is required)"="TreeBio"))
  as.list(funcList)
}

mkMathList <- function ()
{
  list(
    " "=" ",
    "+ Simple addition"="+",
    "- Subtraction or change sign"="-",
    "* Multiplication"="*",
    "/ Division"="/",
    "** Exponentiate, X**Y is X raised to the power Y"="**",
    "EQ Logical Equal"="EQ",
    "NE Logical Not Equal"="NE",
    "LT Logical Less than"="LT",
    "LE Logical Less than or equal"="LE",
    "GT Logical Greater than"="GT",
    "GE Logical Greater than or equal"="GE",
    "AND Logical AND"="AND",
    "OR Logical OR"="OR",
    "NOT Logical NOT"="NOT",
    "ABS() Absolute value, ABS(-3) is 3."="ABS()",
    "ALog() Natural logarithm (base e)"="ALog()",
    "ALog10() Common logarithm (base 10)"="ALog10()",
    "ArcCos() Arc cosine (argument in radians)"="ArcCos()",
    "ArcSin() Arc sine (argument in radians)"="ArcSin()",
    "ArcTan() Arc tangent (argument in radians)"="ArcTan()",
    "Cos() Cosine (argument in radians)"="Cos()",
    "Exp() e raised to power"="Exp()",
    "Frac() Fractional part of a number, Frac(3.4) is .4"="Frac()",
    "Int() Integer part of a number, Int(3.4) is 3"="Int()",
    "Max() Maximum value of the arguments, Max(5,3,-1,10,2) is 10"="Max()",
    "Min() Minimum value of the arguments, Min(5,3,-1,10,2) is -1"="Min()",
    "Mod() Remainder of first argument divided by the second"="Mod()",
    "Sin() Sine (argument in radians)"="Sin()",
    "Sqrt() Square root"="Sqrt()",
    "Tan() Tangent (argument in radians)"="Tan()")
}

mkFreeformEltList <- function (globals,input,prms,title,kwds)
{
  varList = mkVarList(globals)
  funcList = mkFuncList(globals)
  mathList = mkMathList()
  if (input$compTabSet=="Editor" && length(globals$currentEditCmp$kwds)==0)
  {
    eltList <- list(           
      myInlineListButton ("freeOpsKCP","Math:",mathList,0,deltll=NULL),
      myInlineListButton ("freeVarsKCP","Variables:",varList,deltll=NULL),
      mkSelSpecies("freeSpeciesKCP",prms,"Species codes:",fpvs=-1,
           choices=NULL,globals),
      myInlineListButton ("freeFuncsKCP","FVS Functions:",funcList,deltll=NULL),
      uiOutput("fvsFuncRender"))
  } else {
    eltList <- list(
      tags$style(type="label/css", "#cmdTitle{display: inline;}"),
      tags$style(type="text/css", 
        "#freeEditCols{font-family:monospace;font-size:90%;width:95%;}"), 
      tags$p(id="freeEditCols", 
             HTML(paste0("&nbsp;",paste0("....+....",1:8,collapse="")))),
      tags$style(type="text/css", 
        "#freeEdit{font-family:monospace;font-size:90%;width:100%;cursor:auto;}"), 
      tags$script(paste0('$(document).ready(function(){ $("textarea").on("focus",',
                         ' function(e){ Shiny.setInputValue("focusedElement", ',
                         'e.target.id);}); }); ')),
      tags$textarea(id="freeEdit", rows=10, paste0(kwds,collapse="\n")), 
      myInlineListButton ("freeOps","Math:",mathList,0,deltll=NULL),
      myInlineListButton ("freeVars","Variables:",varList, deltll=NULL),
      mkSelSpecies("freeSpecies",prms,"Species codes:",fpvs=-1,
            choices=NULL,globals),
      myInlineListButton ("freeFuncs","FVS Functions:",funcList,deltll=NULL),
      uiOutput("fvsFuncRender"))
  } 
  eltList
}

