mkeltList <- function (pkeys,prms,globals,fvsRun,cndflag=FALSE)
{
  eltList <- if (cndflag) list(h5("Settings for the condition:")) else list()
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
    choices <- getPstring(pkeys,paste0(pkey,"v"),globals$activeVariants[1]) 
    if (cndflag) pkey = paste0("cnd.",pkey)
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
      textEdit       = mkTextInput (pkey, pmt, choices, fpvs), 
      fileBrowse     = {
              choices = gsub("xls$","db",choices)
              mkTextInput (pkey, pmt, choices, fpvs) }, 
      speciesSelection = mkSelSpecies(pkey,prms,pmt,fpvs,globals$activeVariants[1]),
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
  if (length(sel) > 0) choices[sel] = trim(substring(choices[sel],2))
  if (!is.null(fpvs))
  {
    sel = if (is.na(suppressWarnings(as.numeric(fpvs)))) 
      grep (paste0("^",fpvs),choices) else fpvs
  } 
cat ("in mkSelectInput type=",type," fpvs=",fpvs," sel=",sel,"\n")
  mklist = if (valpair) 
    lapply(choices, function (x) trim(unlist(strsplit(x,"="))[1])) else
    as.list(as.character(1:length(choices)))
  names(mklist) = choices
  if (length(sel) == 0) sel = 1
  selected = mklist[[as.numeric(sel)]]
  switch (type,
    "checkboxgroup"=checkboxGroupInput(inputId,label,mklist,selected=selected), 
    "radiogroup"=myRadioGroup(inputId,label,
         mklist,selected=selected),
     myInlineListButton (inputId, label, mklist, selected=selected))
}
      

mkSelSpecies <- function (pkey,prms,pmt,fpvs,variant,addAll=TRUE)
{
  spkeys <- prms[[paste0("species_",variant)]]
  sps = if (addAll) list("All species") else list ()
  for (sp in spkeys) sps <- append(sps,attr(sp,"pstring"))
  dsp = if (addAll) as.list(c("All",unlist(spkeys))) else spkeys
  if (is.null(fpvs)) fpvs = " "
  if (fpvs== " ") 
  {
    dsp = append(dsp," ",after=0)
    names(dsp) = c(" ",sps)
  } else names(dsp) = sps
  myInlineListButton (pkey, pmt, dsp, selected = fpvs)
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
      myInlineTextInput(pkey, "Year or cycle number ", globals$schedBoxYrLastUsed)
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


myRadioGroup <- function (inputId, label, mklist, selected=NULL)
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
  HTML(paste0('<div id="',inputId,'" class="shiny-input-radiogroup">',
    '<label for="',inputId,'"><b>',label,'&nbsp;&nbsp;</b></label>'), 
    paste0(inputs,collapse=""),"</div>")
}


myInlineListButton <- function (inputId, label, mklist, selected=NULL)
{
  inputs = NULL
  if (length(mklist))
  {
    if (is.null(selected)) selected = unlist(mklist[1])
    for (item in 1:length(mklist))
    {
      inputs = c(inputs, paste0('<option value="',
             gsub('"','',mklist[item]),'" ',
             if (mklist[[item]] == selected) "selected" else "",
             '>',names(mklist)[item],"</option>"))
    }
  }
  inputs = if (is.null(inputs)) '<option value=" "></option>' else 
                                 paste0(inputs,collapse="")
  if (nchar(label) > 15)
    HTML(paste0('<div id="',inputId,' " style="width:100%;" class="shiny-input-container">',
      '<label for="',inputId,'" style="max-width:50%;" ><b>',label,'&nbsp;&nbsp;</b></label>',
      '<select id="',inputId,'" style="max-width:50%;" >', 
     inputs,"</select></div>"))
  else    
    HTML(paste0('<div id="',inputId,'" style="width:400px;" class="shiny-input-container">',
      '<label for="',inputId,'" style="max-width:30%;" ><b>',label,'&nbsp;&nbsp;</b></label>',
      '<select id="',inputId,'" style="max-width:70%;" >', 
     inputs,"</select></div>"))
}



