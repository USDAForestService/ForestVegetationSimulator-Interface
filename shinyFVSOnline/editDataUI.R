library(shiny)

trim <- function (x) gsub("^\\s+|\\s+$","",x)

if (file.exists("projectId.txt"))
{
  prjid = scan("projectId.txt",what="",sep="\n",quiet=TRUE)
  tit=prjid[grep("^title",prjid)]
  tit=scan(text=tit,what="",sep="=",quiet=TRUE)
  tit=trim(tit[length(tit)]) #tit variable is used to generate report headings
  email=prjid[grep("^email",prjid)]
  email=scan(text=email,what="",sep="=",quiet=TRUE)
  email=trim(email[length(email)])
  tstring = paste0("Project title: <b>",tit,"</b><br>Email: <b>",email,
         "</b><br>Last accessed: <b>",
         format(file.info(getwd())[1,"mtime"],"%a %b %d %H:%M:%S %Y"),"</b>")
  headstr = "Online"
} else
{
  tstring = paste0("Project working directory: <b>",getwd(),
    "</b> Last accessed: <b>",
    format(if (file.exists("FVS_Runs.RData")) 
      file.info("FVS_Runs.RData")[1,"mtime"] else
      file.info(getwd())         [1,"mtime"],"%a %b %d %H:%M:%S %Y"),"</b>")
  headstr = "Onlocal"
  tit="" #tit variable is used to generate default report headings
}

source("modalDialog.R")

shinyUI(fixedPage(
  
  fixedRow(
    column(width=6,offset=0,
      HTML(paste0('<title>FVS-',headstr,'</title>',
             '<h3><img src="FVSlogo.png" align="middle"</img>',
             '&nbsp;Forest Vegetation Simulator ',headstr,'</h3>'))),
    column(width=5,offset=.5,HTML(paste0("<p>",tstring,"<p/>"))),
    # created a column just to add these invisible elements
    column(width=1,
      tags$style(type="text/css", paste0(".shiny-progress .progress-text {", 
             "background-color: #eef8ff; color: black; ",
             "position: absolute; left: 30px;",            
             "opacity: .9; height: 35px; width: 50%;}")),
      actionButton("topHelp","Help"),
      uiOutput("reload"),
      singleton(tags$head(tags$script(src = "message-handler.js")))
    )
  ),

  fixedRow(
    column(width=11,offset=0,uiOutput("uiHelpText")),
    column(width=1,offset=.5,uiOutput("uiHelpClose"))),

  fixedRow(
    h4("Future input data base editor will be added"),
    actionButton("FVSOnline","Return to FVSOnline")
  )
))
