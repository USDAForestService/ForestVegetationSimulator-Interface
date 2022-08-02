require(openxlsx)

xlsx2html <- function(tab=NULL,xlsxfile=NULL,cols=NULL,addLink=FALSE,sdat=NULL)
{
  if (is.null(xlsxfile) || !file.exists(xlsxfile)) return(NULL)
  cleanlines=function(line) 
  {       
    line=gsub(pattern="\n",replacement="",x=line,fixed=TRUE)
    gsub(pattern="\r",replacement="",x=line,fixed=TRUE)
  }
  if (is.null(tab)) return(NULL)
  if (tab %in% getSheetNames(xlsxfile))
  {
    if (is.null(sdat)) sdat = try(read.xlsx(xlsxFile=xlsxfile,sheet=tab))
    if (class(sdat) == "try-error") return (NULL)
    if (nrow(sdat)==0 || ncol(sdat)==0) return (NULL)             
    if (!is.null(cols) && max(cols)<=ncol(sdat)) sdat = sdat[,cols]
    sdat[sdat == " "]=NA                                            
    if (nrow(sdat)==0 || ncol(sdat)==0) return (NULL)
    sdat = sdat[,!apply(sdat,2,function(x) all(is.na(x)))]
    if (nrow(sdat)==0 || ncol(sdat)==0) return (NULL)       
    sdat = sdat[ !apply(sdat,1,function(x) all(is.na(x))),]
    if (nrow(sdat)==0 || ncol(sdat)==0) return (NULL)
    html = paste0("<b>",tab,"</b>")
    html = paste0(html,'<p><TABLE border="1"><TR><TH>', 
           paste0(cleanlines(colnames(sdat)),collapse="</TH><TH>"),"</TH></TR>")
    for (i in 1:nrow(sdat))                   
    {
      tbrow=cleanlines(as.character(sdat[i,]))
      if (addLink) tbrow[1] = paste0('<a href="#',tbrow[1],'">',tbrow[1],'</a>')
      html = paste0(html,"<TR><TD>",paste0(tbrow,collapse="</TD><TD>"),"</TD></TR>")
    }
    html = paste0(html,"</TABLE><br>")
    return (html)                                 
  } else return (NULL)
}  
  
fr = "data/fvsOnlineHelpRender.RData"                            
fn =  "inst/extdata/fvsOnlineHelp.html"
xlsxfile="inst/extdata/databaseDescription.xlsx"                            
cat ("Erase ",fr,"\n")
unlink(fr)
fvshelp = readChar(fn, file.size(fn)) 
cat ("Process OutputTableDescriptions\n")
tabs = try(read.xlsx(xlsxFile=xlsxfile,sheet="OutputTableDescriptions"))

if (class(tabs)!="try-error")
{
  metr = grep("Metric",tabs$Table,ignore.case=TRUE)
  if (length(metr))
  {
    theMetr = tabs[metr,]
    tabs = tabs[-metr,]
    tabs = rbind(tabs,theMetr)
  }  
  tablist=xlsx2html(tab="OutputTableDescriptions",xlsxfile=xlsxfile,addLink=TRUE,sdat=tabs)     
  if (length(metr)) tablist=sub("<b>OutputTableDescriptions</b>",
        "<b>OutputTableDescriptions</b><p>Note: metric table descriptions are listed below.</p>",tablist)
      
  morehtml=paste0(tablist,'<p><a href="#contents">Back to Contents</a></p>')
  for (tab in tabs$Table) 
  {                                                  
    cat ("Processing output table description for tab=",tab,"\n")    
    morehtml=paste0(morehtml,'<a name="',tab,'"></a>',
      xlsx2html(tab=tab,xlsxfile=xlsxfile),
      '<p><a href="#outputTables">Back to Output Table Descriptions</a>&nbsp;&nbsp;',
      '<a href="#contents">Back to Contents</a></p>')  
  }
  if (!is.null(morehtml)) fvshelp = sub(x=fvshelp,fixed=TRUE,
     pattern="**OUTPUTHTML**",replacement=morehtml)
}                                                  
cat ("Process InputTableDescriptions\n")
tabs = try(read.xlsx(xlsxFile=xlsxfile,sheet="InputTableDescriptions"))
if (class(tabs)!="try-error")                                                         
{
  morehtml=paste0(xlsx2html(tab="InputTableDescriptions",xlsxfile=xlsxfile,addLink=TRUE),
                          '<p><a href="#contents">Back to Contents</a></p>')
  for (tab in tabs$Table) 
  {
    cat ("Processing input table description for tab=",tab,"\n")
    morehtml=paste0(morehtml,'<a name="',tab,'"></a>',  
      xlsx2html(tab=tab,xlsxfile=xlsxfile), 
      '<p><a href="#inputTables">Back to Input Table Descriptions</a>&nbsp;&nbsp;',
      '<a href="#contents">Back to Contents</a></p>')
  }
  if (!is.null(morehtml)) fvshelp = sub(x=fvshelp,fixed=TRUE,
        pattern="**INPUTHTML**",replacement=morehtml)
}                                       
cat ("Saving fvshelp file ",fr,"\n")
save(fvshelp,file=fr)



