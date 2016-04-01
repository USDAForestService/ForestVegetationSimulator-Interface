library(shiny)
library(shinysky)

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
}

source("mkInputElements.R")

shinyUI(fixedPage(
  tags$style(HTML(paste0(
    ".nav>li>a {padding:6px;}",
    ".btn {padding:4px 6px;color:darkred; background-color:#eef8ff;}",
    ".form-control {padding:2px 4px; height:auto;}",
    ".form-group {margin-bottom:6px}"))),
  tags$script('
      Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {   
          var el = $("#" + x);
          el.replaceWith(el = el.clone(true));
          var id = "#" + x + "_progress";     
          $(id).css("visibility", "hidden");
        });'),
  fixedRow(
    column(width=6,offset=0,
      HTML(paste0('<title>FVS-',headstr,'</title>',
             '<h3><img src="FVSlogo.png" align="middle"</img>',
             '&nbsp;FVS Input Data Editor (',headstr,')</h3>'))),
    column(width=5,offset=.5,HTML(paste0("<p>",tstring,"<p/>"))),
    # created a column just to add these invisible elements
    column(width=1,
      tags$style(type="text/css", paste0(".shiny-progress .progress-text {", 
             "background-color: #eef8ff; color: black; ",
             "position: absolute; left: 30px;",            
             "opacity: .9; height: 35px; width: 50%;}")),
      uiOutput("reload"),
      singleton(tags$head(tags$script(src = "message-handler.js")))
    )
  ),

  fixedRow(
    column(width=11,offset=0,uiOutput("uiHelpText")),
    column(width=1,offset=.5,uiOutput("uiHelpClose"))),

  fixedRow(
    column(width=3,offset=0,
      myRadioGroup("mode", "Mode ", c("Edit","New rows")),
      myInlineTextInput("disprows",  "Number display rows", value = 20, size=5),
  	  selectInput("selectdbtabs", label="Table to process",
	          choices  = list(), 
	          selected = NULL, multiple = FALSE, selectize=FALSE),
  	  selectInput("selectdbvars", "Variables to consider", 
          choices  = list(), size=10,
          selected = NULL, multiple = TRUE, selectize=FALSE),
      uiOutput("stdSel"),h5(),
      shiny::actionButton("recoverdb","Recover database from backup or default"),h5(),
      shiny::actionButton("clearTable","Remove all rows and commit"),h5(),
      shiny::actionButton("commitChanges","Commit edits or new rows"),h5(),
      shiny::actionButton("FVSOnline","Return to FVSOnline")
    ),
    column(width=9,offset=0,
      uiOutput("navRows"),
      h5(),
      hotable("tbl"),
      h4(" "),
      tags$style(type="text/css","#actionMsg{color:darkred;}"), 
      textOutput("actionMsg"),
      fileInput("upload","Upload and commit FVS-Ready database (.accdb, .mdb, or .db (SQLite3))",
                width="90%"), 
      fileInput("uploadStdTree",
               'Upload and commit to "Table to process" (.csv, data will be appended)',
                width="90%"), 
      fileInput("climateFVSUpload",
                "Upload and commit Climate-FVS data (append and replace; FVSClimAttrs.csv or answers.zip).",
                width="90%")
    )
  )
))








