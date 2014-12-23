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
                         list(HTML(paste0("<p>",gsub("\n","<br/>",title),"<p/>"))))
    fpvs <- if (identical(globals$currentEditCmp,globals$NULLfvsCmp)) NULL else
            globals$currentEditCmp$reopn[f]    
    choices <- getPstring(pkeys,paste0(pkey,"v"),globals$activeVariants[1]) 
    if (cndflag) pkey = paste0("cnd.",pkey)
cat ("mkeltList f=",f," elt=",elt," pkey=",pkey," pmt=",pmt,
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
      noInput = list(div(id=pkey,HTML(paste0("<p>",gsub("\n","<br/>",pmt),"<p/>")))),
      NULL)
    if (!is.null(elt)) eltList <- append(eltList,elt)
  }
  if (cndflag) 
  {
    if (length(eltList) == 1) eltList <- append(eltList, 
      list(h6("No settings for this condition.")))
    eltList <- append(eltList, 
      list(h5("Settings for the action (e.g. keyword(s)):")))
  }
  eltList
}


mkTextInput <- function (pkey, pmt, choices, fpvs) 
{
  if (!is.null(fpvs)) choices = fpvs 
  tmp <- if (is.null(choices)) " " else scan(text=choices,what=" ",quiet=TRUE)[1]
  if (length(tmp) == 0 || is.na(tmp) || tmp == "blank") tmp = " " 
  textInput(pkey, pmt, tmp)
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
  if (!is.null(fpvs)) sel = fpvs
  mklist = if (valpair) 
    lapply(choices, function (x) trim(unlist(strsplit(x,"="))[1])) else
    as.list(as.character(1:length(choices)))
  names(mklist) = choices
  selected = if (length(sel) > 0) mklist[mklist == sel] else "1"
  switch (type,
    "checkboxgroup"=checkboxGroupInput(inputId,label,mklist,selected=selected), 
    "radiogroup"=list(radioButtons(inputId,label,
         mklist,selected=selected),br()),
    selectInput (inputId,label,mklist,selected=selected, 
                 multiple=FALSE,selectize=FALSE))
}
      

mkSelSpecies <- function (pkey,prms,pmt,fpvs,variant,addAll=TRUE)
{
  spkeys <- prms[[paste0("species_",variant)]]
  sps = if (addAll) list("All species") else list (NULL)
  for (sp in spkeys) sps <- append(sps,attr(sp,"pstring"))
  dsp = as.list(c("All",unlist(spkeys))) 
  names(dsp) = sps
  selectInput (pkey, pmt, dsp, selected = if (is.null(fpvs)) dsp[[1]] else fpvs, 
               multiple = FALSE, selectize = FALSE)
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
    rtn <- list(
      radioButtons("schedbox", pmt, mklist, inline=TRUE),
      uiOutput("conditions"),
      textInput(pkey, "Year or cycle number", globals$schedBoxYrLastUsed)
    )
  } else
  {    
    sch <- if (globals$currentEditCmp$atag == "k") "Schedule by year" else
      {
        cn = findCmp(fvsRun,globals$currentEditCmp$atag)
        paste0('Schedule by condition: "',cn$kwdName,'"')
      }
    rtn <- list(
      HTML(sch),
      textInput(pkey, 
        label = if (globals$currentEditCmp$atag == "k") 
                "Year or cycle number" else
                "Number of years after condition is found true", 
        value = globals$currentEditCmp$reopn[pkey])
    )
  }
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





