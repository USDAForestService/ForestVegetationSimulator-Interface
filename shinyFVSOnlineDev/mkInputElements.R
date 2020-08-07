# $Id$

mkeltList <- function (pkeys,prms,globals,fvsRun,cndflag=FALSE,funcflag=FALSE)
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
 "\nglobals$activeVariants[1]=",globals$activeVariants[1]," fpvs=",fpvs,
 "\nchoices=",choices,"\n") 
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
      forestSelection = mkSelForest(pkey,prms,pmt,fpvs,choices,globals$activeVariants[1]),
      habPaSelection = mkSelhabPa(pkey,prms,pmt,fpvs,choices,globals$activeVariants[1]),
      fileBrowse     = {
              choices = gsub("xls$","db",choices)
              mkTextInput (pkey, pmt, choices, fpvs) }, 
      speciesSelection = mkSelSpecies(pkey,prms,pmt,fpvs,choices,globals$activeVariants[1]),
      scheduleBox = mkScheduleBox(pkey,prms,pmt,fvsRun,globals),
      noInput = list(div(id=pkey,HTML(paste0("<p><b>",gsub("\n","<br/>",pmt),"<b/><p/>")))),
      NULL)
    if (!is.null(elt)) eltList <- append(eltList,list(elt))
  }
  if (cndflag) 
  {
    if (length(eltList) == 1) eltList <- append(eltList, 
      list(h6("No settings for this condition.")))
  }
  eltList
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
  list(HTML(paste0('<form><input id="',pkey,'" type="checkbox" ',checked,
       ' value="',fpvs,'">&nbsp;',pmt,'</form>')))
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

mkSelhabPa<- function (pkey,prms,pmt,fpvs,choices,variant)
{
  forkeys <- prms[[paste0("HabPa_",variant)]]
  choices = if (!is.null(choices)) scan(text=choices,what="character",quiet=TRUE) else NULL
  addAll = grep ("^blank",choices)
  if (length(addAll))
  { 
    choices = choices[-addAll]
    if (length(choices)==0) choices=NULL
    addAll = FALSE
  } else addAll = TRUE
  fors = if (addAll) list("All species") else list ()
  for (f in forkeys) fors <- append(fors,attr(f,"pstring"))
  dsp = if (addAll) as.list(c("All",unlist(forkeys))) else forkeys
  if (!is.null(fpvs)) 
  {
    if (fpvs == -1) 
    {
      sps <- append(sps," ",after=0)
      dsp <- append(dsp," ",after=0)
    } else choices = fpvs
  }
  if(length(fors))names(dsp) = fors
  spGrp=NULL
  myInlineListButton (pkey, pmt, dsp, selected = choices, spGrp)
}

mkSelForest <- function (pkey,prms,pmt,fpvs,choices,variant)
{
forkeys <- prms[[paste0("Forests_",variant)]]
choices = if (!is.null(choices)) scan(text=choices,what="character",quiet=TRUE) else NULL
addAll = grep ("^blank",choices)
if (length(addAll))
{ 
choices = choices[-addAll]
if (length(choices)==0) choices=NULL
addAll = FALSE
} else addAll = TRUE
fors = if (addAll) list("All species") else list ()
for (f in forkeys) fors <- append(fors,attr(f,"pstring"))
dsp = if (addAll) as.list(c("All",unlist(forkeys))) else forkeys
if (!is.null(fpvs)) 
{
  if (fpvs == -1) 
  {
    sps <- append(sps," ",after=0)
    dsp <- append(dsp," ",after=0)
  } else choices = fpvs
}
if(length(fors))names(dsp) = fors
spGrp=NULL
myInlineListButton (pkey, pmt, dsp, selected = choices, spGrp)
}

    
mkSelSpecies <- function (pkey,prms,pmt,fpvs,choices,variant)
{ spGrp <- as.numeric()
  spkeys <- prms[[paste0("species_",variant)]]
  choices = if (!is.null(choices)) scan(text=choices,what="character",quiet=TRUE) else NULL
  addAll = grep ("^deleteAll",choices)
  if (length(addAll))
  { spGrp <- 1
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
  }else
  if(length(fors))names(dsp) = sps
  if (!is.null(fpvs)) spGrp=NULL
  if (!is.null(fpvs) && (addAll)) spGrp <- 2
  myInlineListButton (pkey, pmt, dsp, selected = choices, spGrp)
}
      

mkScheduleBox <- function (pkey,prms,pmt,fvsRun,globals)
{
  if (identical(globals$currentEditCmp,globals$NULLfvsCmp)) 
  {
cat ("mkScheduleBox schedBoxPkey is set to:",pkey,"\n")
    globals$schedBoxPkey <- pkey
    mklist <- list("Schedule by year"="1","Schedule by condition"="2")
    globals$existingCmps <- mkExistingCndsList(fvsRun)
    if (length(globals$schedBoxYrLastUsed) == 0) globals$schedBoxYrLastUsed <- 
        fvsRun$startyr
    if (length(globals$existingCmps)) mklist <- append(mklist,
       c("Attach to existing condition"="3"))
    rtn <- list(h5(),div(style="background-color: rgb(240,255,240)",
      radioButtons("schedbox", pmt, mklist, inline=TRUE),
      uiOutput("conditions"),
      myInlineTextInput(pkey, "Year or cycle number ", fvsRun$startyr)
    ))
  } else {    
    sch <- if (globals$currentEditCmp$atag == "k") "Schedule by year " else
      {
        cn = findCmp(fvsRun,globals$currentEditCmp$atag)
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
  rtn
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


myRadioGroup <- function (inputId, label, mklist, selected=NULL,labelstyle=NULL)
{
  inputs = NULL
  if (is.null(names(mklist))) names(mklist) = mklist
  if (is.null(selected)) selected = mklist[1]
  for (item in 1:length(mklist))
  {
    inputs = c(inputs, paste0('<input type="radio" name="',inputId,'" value="',
           gsub('"','',mklist[item]),'" ',
           if (mklist[item] == selected) "checked" else "",
           '>',names(mklist)[item],"&nbsp;&nbsp;"))
  }
  labelstyle = if (is.null(labelstyle)) "" else paste0('style="',labelstyle,'"')
  HTML(paste0('<div id="',inputId,'" class="shiny-input-radiogroup">',
    '<label for="',inputId,'" ',labelstyle,'>',label,'&nbsp;&nbsp;</label>'), 
    paste0(inputs,collapse=""),"</div>")
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
        if(length(spgsidxs) > 1){
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


mkFreeformEltList <- function (globals,prms,title,kwds)
{
  varsDef  = " "
  varsName = " "
  for (elt in prms[["evmon.variables"]])
  {
    atl = attr(elt,"atlist")
    # the appliesTo list will have two tokens if the extension is part of the list
    if (length(atl) > 1 && length(intersect(atl,globals$activeExtens)) == 0) next
    varsDef <- c(varsDef,paste0(elt,": ",attr(elt,"pstring")))
    attributes(elt) <- NULL
    varsName <- c(varsName,elt)
  }
  indx = sort(varsName,index.return=TRUE)$ix
  varsName = as.list(varsName[indx])
  names(varsName) = varsDef[indx]
  funcDef  = " "
  funcName = " "
  for (elt in prms[["evmon.functions"]])
  {
    atl = attr(elt,"atlist")
    # the appliesTo list will have two tokens if the extension is part of the list
    if (length(atl) > 1 && length(intersect(atl,globals$activeExtens)) == 0) next
    funcDef <- c(funcDef,paste0(elt,": ",attr(elt,"pstring")))
    attributes(elt) <- NULL
    funcName <- c(funcName,elt)
  }
  indx = sort(funcName,index.return=TRUE)$ix
  funcName = as.list(funcName[indx])
  names(funcName) = funcDef[indx]
  mathList = list(
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
  if (input$compTabSet=="Editor" && length(globals$currentEditCmp$kwds)==0)
  {
    eltList <- list(
     myInlineListButton ("freeOpsKCP","Math:",mathList,0,deltll=NULL),
     myInlineListButton ("freeVarsKCP","Variables:",varsName,deltll=NULL),
     mkSelSpecies("freeSpeciesKCP",prms,"Species codes:",fpvs=-1,
          choices=NULL,globals$activeVariants[1]),
     myInlineListButton ("freeFuncsKCP","FVS Functions:",funcName,deltll=NULL),
     uiOutput("fvsFuncRender"))
  } else {
    eltList <- list(
    tags$style(type="label/css", "#cmdTitle{display: inline;}"),
    myInlineTextInput("cmdTitle","Component title",title,size=40,NULL),          
    tags$style(type="text/css", 
      "#freeEditCols{font-family:monospace;font-size:90%;width:95%;}"), 
    tags$p(id="freeEditCols", 
           HTML(paste0("&nbsp;",paste0("....+....",1:8,collapse="")))),
    tags$style(type="text/css", 
      "#freeEdit{font-family:monospace;font-size:90%;width:95%;}"), 
    tags$textarea(id="freeEdit", rows=10, paste0(kwds,collapse="\n")), 
    myInlineListButton ("freeOps","Math:",mathList,0,deltll=NULL),
    myInlineListButton ("freeVars","Variables:",varsName, deltll=NULL),
    mkSelSpecies("freeSpecies",prms,"Species codes:",fpvs=-1,
          choices=NULL,globals$activeVariants[1]),
    myInlineListButton ("freeFuncs","FVS Functions:",funcName,deltll=NULL),
    uiOutput("fvsFuncRender"))
  }
  eltList
}

