initTableGraphTools <- function (globals,session,output,fvsOutData)
{
cat ("initTableGraphTools\n")
  globals$gFreeze = TRUE
  fvsOutData$dbData = data.frame()
  fvsOutData$runs = character(0)
  fvsOutData$dbVars = character(0)
  fvsOutData$browseVars = character(0)
  fvsOutData$dbSelVars = character(0)
  fvsOutData$browseSelVars = character(0)
  choices = list()
  globals$settingChoices=list()
  updateSelectInput(session=session, inputId="pivVar", choices=choices,select="")              
  updateSelectInput(session=session, inputId="hfacet", choices=choices,select="") 
  updateSelectInput(session=session, inputId="vfacet", choices=choices,select="") 
  updateSelectInput(session=session, inputId="pltby",  choices=choices,select="") 
  updateSelectInput(session=session, inputId="dispVar",choices=choices,select="")       
  updateSelectInput(session=session, inputId="xaxis",  choices=choices,select="") 
  updateSelectInput(session=session, inputId="yaxis",  choices=choices,select="")
  updateCheckboxGroupInput(session=session, inputId="browsevars", choices=choices) 
  updateTextInput(session=session,   inputId="sqlOutput", label=NULL, value="")
  choices = list("None loaded")
  updateSelectInput(session=session, inputId="stdtitle", choices=choices,select=NULL)
  updateSelectInput(session=session, inputId="stdgroups",choices=choices,select=NULL)
  updateSelectInput(session=session, inputId="stdid",    choices=choices,select=NULL)
  updateSelectInput(session=session, inputId="mgmid",    choices=choices,select=NULL)
  updateSelectInput(session=session, inputId="year",     choices=choices,select=NULL)
  updateSelectInput(session=session, inputId="species",  choices=choices,select=NULL)
  updateSelectInput(session=session, inputId="dbhclass", choices=choices,select=NULL)
  updateTextInput(session=session,   inputId="ptitle",   value="")
  updateTextInput(session=session,   inputId="ylabel",   value="")
  updateTextInput(session=session,   inputId="xlabel",   value="")
  output$table <- renderTable(NULL)
  globals$gFreeze = FALSE
}                          

pivot <- function(dat,pvar,dvar)
{
  facts = colnames(dat)[unlist(lapply(dat,is.factor))]
  if (! pvar %in% facts) return(data.frame())
  if (! dvar %in% colnames(dat)) return(data.frame())
  for (v in facts) dat[[v]] = droplevels(dat[[v]])
  facts = setdiff(facts,c(dvar,pvar))
  if (length(facts) == 0) 
  {
    d = t(dat[,dvar,drop=FALSE])
    colnames(d) = dat[,pvar]
    return(as.data.frame(d))
  }
  grps = by(dat,as.list(dat[,facts,drop=FALSE]),
    function(x,allc,facts,pvar,dvar) 
    {
      y = rep(0,length(allc)) 
      y[match(x[,pvar],allc)] = x[,dvar]
      c(as.numeric(x[1,facts]),y)   
    },levels(dat[[pvar]]),facts,pvar,dvar)    
  grps = matrix(unlist(grps),
         ncol=length(levels(dat[[pvar]]))+length(facts),byrow=TRUE)
  colnames(grps)=c(facts,paste0(dvar," ",pvar,":",levels(dat[[pvar]])))
  if (is.factor(dat[[dvar]]))
  {
    colstofactor = (length(facts)+1):ncol(grps)
    grps[,colstofactor] = ifelse(grps[,colstofactor]==0,"",
        levels(dat[[dvar]])[grps[,colstofactor]])
  }
  grps = as.data.frame(grps)  
  for (i in facts)
  {
    grps[[i]] = as.integer(grps[[i]])
    attributes(grps[[i]]) = attributes(dat[[i]])
  }
  cmd = paste0("order(",paste(paste0("grps$",facts),collapse=","),")")
  sby = eval(parse(text=cmd))
  grps[sby,]
}


filterRows <- function (dat, title, groups, stdid, mgtid, year, species, dbhclass)
{
  rows = rep (TRUE,nrow(dat))
  if (!is.null(title) && length(title) == 1 && 
      substring(title[[1]],1,11) == "None loaded") title = NULL
  if (!is.null(stdid) && length(stdid) == 1 && 
      substring(stdid[[1]],1,11) == "None loaded") stdid = NULL
  if (!is.null(groups) && length(groups) == 1 && 
      substring(groups[[1]],1,11) == "None loaded") groups = NULL
  if (!is.null(mgtid) && length(mgtid) == 1 && 
      substring(mgtid[[1]],1,11) == "None loaded") mgtid = NULL
  if (!is.null(year) && length(year) == 1 && 
      substring(year[[1]],1,11) == "None loaded") year = NULL
  if (!is.null(species) && length(species) == 1 && 
      substring(species[[1]],1,11) == "None loaded") species = NULL
  if (!is.null(dbhclass) && length(dbhclass) == 1 && 
      substring(dbhclass[[1]],1,11) == "None loaded") dbhclass = NULL
  rows = if (!is.null(title)    & !is.null(dat$RunTitle)) rows &  
         dat$RunTitle %in% title   else rows
  rows = if (!is.null(stdid)    & !is.null(dat$StandID))  rows &  
         dat$StandID %in% stdid    else rows     
  rows = if (!is.null(groups)   & !is.null(dat$Groups))   rows &
         {
           hits = unique(unlist(lapply(groups,function (x,dgrps)  
                         grep (x,dgrps,fixed=TRUE), dat$Groups)))
           yeses = rep(FALSE,length(rows))
           yeses[hits] = TRUE
           yeses
         } else rows
  rows = if (!is.null(mgtid)    & !is.null(dat$MgmtID))   rows &   
         dat$MgmtID  %in% mgtid    else rows
  rows = if (!is.null(year)     & !is.null(dat$Year))     rows &     
         dat$Year %in% year     else rows
  rows = if (!is.null(species)  & !is.null(dat$Species))  rows & 
         dat$Species %in% species  else rows
  rows = if (!is.null(dbhclass) & !is.null(dat$DBHClass)) rows & 
         dat$DBHClass %in% dbhclass else rows
  rows
}


setupSummary <- function(asum,composite=FALSE)
{
  std=if (length(grep("SCuFt$",colnames(asum)))) 
    c("Tpa","MCuFt","SCuFt","SBdFt") else c("Tpa","TCuFt","MCuFt","BdFt")
  rstd=paste("R",std,sep="")
  stdden = c("BA","SDI","CCF","TopHt","QMD")
  stddenAT = paste0("AT",stdden)
  newrtpa = "RTpa"
  if (composite)
  {
    std=paste0("Cmp",std)
    rstd=paste0("Cmp",rstd)
    stddenAT = paste0("CmpAT",stdden)
    stdden = paste0("Cmp",stdden)
    newrtpa = "CmpRTpa"
  }    
  new=asum[,newrtpa] > 0
  rtpaLoc  = match(newrtpa,colnames(asum))
  if (sum(new) > 0) 
  {
    dups=unlist(lapply(1:length(new),function(x,new) 
                if (new[x]) rep(x,2) else x, new))
    asum=asum[dups,]
    for (row in 1:(nrow(asum)-1))
    {
      nrow=row+1
      if (dups[row] == dups[nrow])
      {
        asum[nrow,std]  = asum[nrow,std] - asum[nrow,rstd]
        asum[nrow,stdden]=asum[nrow,stddenAT]
        asum[row,rstd] = 0
        asum[row,rtpaLoc:(rtpaLoc+3)] = 0
        dups[nrow]=0
      }
    }
  }
  tprd=apply(asum[,rstd],2,cumsum)+asum[,std]
  colnames(tprd)=if (composite) gsub("Cmp","CmpTPrd",std) else 
                                paste("TPrd",std,sep="")
  asum=cbind(asum,tprd,srtOrd=1:nrow(asum))
  asum
}


autorecycle <- function(a,n)
{
  if (length(a)<n) 
  {
    add = n%/%length(a)
    if (add) a = rep(a,add)
    add = n%%length(a)
    if (add) a = c(a,a[1:add])                                      
  }
  a[1:n]
} 

removeComment <- function(string)
{
  l1 = gregexpr("/*",string,fixed=TRUE)[[1]][1]
  if (l1==-1) return(string)
  l2 = gregexpr("*/",string,fixed=TRUE)[[1]][1]
  if (l2==-1) return(substring(string,1,l1-1))
  return(removeComment(paste0(substring(string,1,l1-1),
                       substring(string,l2+2,99999))))
}



getGraphSettings <- function(input)
{
  theSettings=list() 
  isolate({   
    theSettings$selectdbtables = input$selectdbtables
    theSettings$selectdbvars   = input$selectdbvars
    theSettings$stdgroups      = input$stdgroups    
    theSettings$stdid          = input$stdid        
    theSettings$mgmid          = input$mgmid 
    theSettings$year           = input$year                 
    theSettings$species        = input$species 
    theSettings$dbhclass       = input$dbhclass           
    theSettings$browsevars     = input$browsevars
    theSettings$plotType       = input$plotType 
    theSettings$colBW          = input$colBW
    theSettings$xaxis          = input$xaxis 
    theSettings$yaxis          = input$yaxis 
    theSettings$hfacet         = input$hfacet
    theSettings$ptitle         = input$ptitle 
    theSettings$xlabel         = input$xlabel 
    theSettings$vfacet         = input$vfacet 
    theSettings$pltby          = input$pltby              
    theSettings$ylabel         = input$ylabel 
    theSettings$width          = input$width 
    theSettings$height         = input$height 
    theSettings$moreControls   = input$moreControls
    theSettings$color1         = input$color1                    
    theSettings$color2         = input$color2
    theSettings$color3         = input$color3
    theSettings$color4         = input$color4  
    theSettings$color5         = input$color5 
    theSettings$color6         = input$color6  
    theSettings$color7         = input$color7      
    theSettings$color8         = input$color8 
    theSettings$color9         = input$color9 
    theSettings$color10        = input$color10 
    theSettings$color11        = input$color11 
    theSettings$color12        = input$color12 
    theSettings$color13        = input$color13 
    theSettings$color14        = input$color14 
    theSettings$color15        = input$color15 
    theSettings$color16        = input$color16 
    theSettings$color17        = input$color17 
    theSettings$color18        = input$color18 
    theSettings$res            = input$res
    theSettings$transparency   = input$transparency 
    theSettings$YLimMin        = input$YLimMin
    theSettings$YLimMax        = input$YLimMax
    theSettings$XLimMin        = input$XLimMin
    theSettings$XLimMax        = input$XLimMax            
    theSettings$YlabRot        = input$YlabRot
    theSettings$XlabRot        = input$XlabRot
    theSettings$barPlace       = input$barPlace
    theSettings$legendPlace    = input$legendPlace
    theSettings$YTrans         = input$YTrans
    theSettings$XTrans         = input$XTrans
    theSettings$facetWrap      = input$facetWrap
    theSettings$SDIvals        = input$SDIvals
    theSettings$YUnits         = input$YUnits
    theSettings$XUnits         = input$XUnits
    theSettings$StkChtvals     = input$StkChtvals 
  })
  theSettings
} 

setGraphSettings <- function(session,globals,theSettings,fvsOutData)
{ 
  globals$gFreeze = TRUE
  setSettings <- function(session,inputId,cho,theS,tag)
  {
    sel = intersect(theS[[inputId]],cho[[inputId]])
    msg = if (length(sel) == length(theS[[inputId]])) NULL else 
      paste0('Some saved selections of "',tag,'" are missing, those present are selected.')
    if (length(sel)) 
      updateSelectInput(session=session, inputId=inputId, selected=sel) else
    {
      cch = if (inputId %in% c("species","dbhclass")) setdiff(cho[[inputId]],"All") else cho[[inputId]]
      sel = cch[1:min(length(theS[[inputId]]),length(cch))]
      msg = if (length(sel)==1 && sel=="None loaded") NULL else
        paste0('All saved selections of "',tag,'" are missing, the first ',length(sel),
                   if (length(sel)==1) ' is' else ' are',' selected.')
      updateSelectInput(session=session, inputId=inputId, selected=sel)
    }   
    return(msg)
  }
  msg =       setSettings (session,"stdgroups",globals$exploreChoices,theSettings,"Groups")
  msg = c(msg,setSettings (session,"stdid",    globals$exploreChoices,theSettings,"Stands"))
  msg = c(msg,setSettings (session,"mgmid",    globals$exploreChoices,theSettings,"MgmtIDs"))
  msg = c(msg,setSettings (session,"year",     globals$exploreChoices,theSettings,"Year"))
  msg = c(msg,setSettings (session,"species",  globals$exploreChoices,theSettings,"Species"))
  msg = c(msg,setSettings (session,"dbhclass", globals$exploreChoices,theSettings,"DBHClasses"))

  updateCheckboxGroupInput(session=session, inputId="browsevars",selected=theSettings$browsevars)           
  updateRadioButtons      (session=session, inputId="plotType",  selected=theSettings$plotType)
  updateSelectInput (session=session, inputId="yaxis", choices=globals$settingChoices[["yaxis"]], selected=theSettings$yaxis) 
  updateSelectInput (session=session, inputId="xaxis", choices=globals$settingChoices[["xaxis"]], selected=theSettings$xaxis) 
  updateSelectInput (session=session, inputId="hfacet",choices=globals$settingChoices[["hfacet"]],selected=theSettings$hfacet)
  updateSelectInput (session=session, inputId="vfacet",choices=globals$settingChoices[["vfacet"]],selected=theSettings$vfacet) 
  updateSelectInput (session=session, inputId="pltby", choices=globals$settingChoices[["pltby"]], selected=theSettings$pltby) 
                   
  updateSliderInput (session=session, inputId="transparency", value   =theSettings$transparency) 
  updateColourInput (session=session, inputId="color1",       value   =theSettings$color1)           
  updateColourInput (session=session, inputId="color2",       value   =theSettings$color2)
  updateColourInput (session=session, inputId="color3",       value   =theSettings$color3)                       
  updateColourInput (session=session, inputId="color4",       value   =theSettings$color4)  
  updateColourInput (session=session, inputId="color5",       value   =theSettings$color5) 
  updateColourInput (session=session, inputId="color6",       value   =theSettings$color6)  
  updateColourInput (session=session, inputId="color7",       value   =theSettings$color7)      
  updateColourInput (session=session, inputId="color8",       value   =theSettings$color8) 
  updateColourInput (session=session, inputId="color9",       value   =theSettings$color9) 
  updateColourInput (session=session, inputId="color10",      value   =theSettings$color10) 
  updateColourInput (session=session, inputId="color11",      value   =theSettings$color11) 
  updateColourInput (session=session, inputId="color12",      value   =theSettings$color12) 
  updateColourInput (session=session, inputId="color13",      value   =theSettings$color13) 
  updateColourInput (session=session, inputId="color14",      value   =theSettings$color14) 
  updateColourInput (session=session, inputId="color15",      value   =theSettings$color15) 
  updateColourInput (session=session, inputId="color16",      value   =theSettings$color16) 
  updateColourInput (session=session, inputId="color17",      value   =theSettings$color17) 
  updateColourInput (session=session, inputId="color18",      value   =theSettings$color18) 
  updateRadioButtons(session=session, inputId="colBW",        selected=theSettings$colBW)
  updateRadioButtons(session=session, inputId="res",          selected=theSettings$res)
  updateRadioButtons(session=session, inputId="XTrans",       selected=theSettings$XTrans)
  updateRadioButtons(session=session, inputId="XUnits",       selected=theSettings$XUnits)
  updateRadioButtons(session=session, inputId="XlabRot",      selected=theSettings$XlabRot)
  updateRadioButtons(session=session, inputId="YTrans",       selected=theSettings$YTrans)
  updateRadioButtons(session=session, inputId="YUnits",       selected=theSettings$YUnits)
  updateRadioButtons(session=session, inputId="YlabRot",      selected=theSettings$YlabRot)
  updateRadioButtons(session=session, inputId="barPlace",     selected=theSettings$barPlace)
  updateRadioButtons(session=session, inputId="facetWrap",    selected=theSettings$facetWrap)
  updateRadioButtons(session=session, inputId="legendPlace",  selected=theSettings$legendPlace)
  updateRadioButtons(session=session, inputId="moreControls", selected=theSettings$moreControls)
  updateTextInput   (session=session, inputId="SDIvals",      value   =theSettings$SDIvals)
  updateTextInput   (session=session, inputId="StkChtvals",   value   =theSettings$StkChtvals)
  updateTextInput   (session=session, inputId="XLimMax",      value   =theSettings$XLimMax)
  updateTextInput   (session=session, inputId="XLimMin",      value   =theSettings$XLimMin)
  updateTextInput   (session=session, inputId="YLimMax",      value   =theSettings$YLimMax)
  updateTextInput   (session=session, inputId="YLimMin",      value   =theSettings$YLimMin)
  updateTextInput   (session=session, inputId="height",       value   =theSettings$height) 
  updateTextInput   (session=session, inputId="ptitle",       value   =theSettings$ptitle) 
  updateTextInput   (session=session, inputId="width",        value   =theSettings$width)         
  updateTextInput   (session=session, inputId="xlabel",       value   =theSettings$xlabel) 
  updateTextInput   (session=session, inputId="ylabel",       value   =theSettings$ylabel) 
  return (msg)
}

    

