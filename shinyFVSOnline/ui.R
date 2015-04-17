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

customRunElements = NULL
if (file.exists("runScripts.R"))
{
  source("runScripts.R")
  keep = file.exists(paste0("customRun_",runScripts,".R"))
  runScripts = runScripts[keep]
  if (length(runScripts)) 
  {
    defaultRun = list("Default useful for all FVS variants"="fvsRun")
    runScripts = append(x=runScripts,after=0,defaultRun)
    customRunElements = list(                  
      selectInput("runScript",
                  "Select run script (normally, use the default)",
                  choices=runScripts,
                  selected="fvsRun",multiple=FALSE,selectize=FALSE),
      uiOutput("uiCustomRunOps"))
  } 
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
  column(width=4,offset=0,
    tags$style(type="text/css","#leftPan {background-color: rgb(227,227,255);}"),
    tabsetPanel(id="leftPan",
      tabPanel("Runs",  
        tags$style(type="text/css", "#runSel { width: 100%; }"),
        selectInput("runSel","", NULL, NULL, multiple=FALSE,
                    selectize=FALSE),
        actionButton("newRun","New"),
        actionButton("reload","Reload"),
        tags$style(type="text/css", "#saveRun { color: green; }"),
        actionButton("saveRun","Save"),
        actionButton("dupRun","Duplicate"),
        h6(" "),
        tags$style(type="text/css", "#title { width: 90%; }"),
        textInput("title", "", "nothing loaded yet"), 
        # all the select objects
        tags$style(type="text/css", "select { width: 100%; }"),
        h6(" "),
        tags$style(type="text/css", "#simCont { width: 100%; height: 400px;}"),
        selectInput("simCont","Contents", NULL, NULL, multiple=TRUE,
                    selectize=FALSE),
        actionButton("editSel","Edit"),
        actionButton("mkfree","Change to freeform"),
        actionButton("cutCmp","Cut"),
        actionButton("copyCmp","Copy"),
        h6(" "),
        actionButton("paste","Paste item selected below"),
        selectInput("selpaste","", NULL, 
                    NULL, multiple=FALSE, selectize=FALSE) 
      ),
      tabPanel("Load Output", 
    	  selectInput("runs", "Runs to consider", 
	        choices  = list(), size=5,
	        selected = NULL, multiple = TRUE, selectize=FALSE),
        tags$style(type="text/css", "#sdskwdbh { width: 30%;}"),
        tags$style(type="text/css", "#sdskldbh { width: 50%;}"),
        fixedRow(
          column(width=7,
            numericInput("sdskwdbh", "DBH class size", 4, min=1, 
                          max=10,step=1)),
          column(width=5,
            numericInput("sdskldbh", "Large DBH", 48))),
        actionButton("stdskreb","Rebuild StdStk"), 
    	  selectInput("selectdbtables", "Database tables to consider", 
	        choices  = list(), size=8,
	        selected = NULL, multiple = TRUE, selectize=FALSE),
    	  selectInput("selectdbvars", "Database variables to consider", 
          choices  = list(), size=20,
          selected = NULL, multiple = TRUE, selectize=FALSE)
	    ),
      tabPanel("Explore Output", 
    	  selectInput("stdtitle", "Select run titles", 
	          choices  = list("None loaded"), 
	          selected = NULL, multiple = TRUE, selectize=FALSE),
    	  selectInput("stdid", "Select stand", 
	          choices  = list("None loaded"), 
	          selected = NULL, multiple = TRUE, selectize=FALSE),
        fixedRow(
          column(width=5,
            selectInput("mgmid", "Select MgmtID", 
	            choices  = list("None loaded"), 
	            selected = NULL, multiple = TRUE, selectize=FALSE)),
          column(width=5,
            selectInput("year", "Select years", 
	            choices  = list("None loaded"), 
	            selected = NULL, multiple = TRUE, selectize=FALSE))),
        fixedRow(
	        column(width=5,
	          selectInput("species", "Select species", 
	            choices  = list("None loaded"), 
	            selected = NULL, multiple = TRUE, selectize=FALSE)),
	        column(width=5,
	          selectInput("dbhclass", "Select DBHClass", 
	            choices  = list("None loaded"), 
	            selected = NULL, multiple = TRUE, selectize=FALSE))),
	      checkboxGroupInput("browsevars","Select variables",
	          choices = list("None"),selected = NULL,inline=TRUE)
      ) 
    )                    
  ),

  column(width=8,offset=.2,
    tags$style(type="text/css","#rightPan {background-color: rgb(227,255,227);}"),
    tabsetPanel(id = "rightPan",
      tabPanel("Stands",
        selectInput("inVars","Variants", NULL, NULL, 
                  multiple=FALSE, selectize=FALSE),
        selectInput("inGrps","Groups", NULL, NULL, 
                  multiple=TRUE, selectize=FALSE, size=6),
        selectInput("inStds","Stands", NULL, NULL, 
                  multiple=TRUE, selectize=FALSE),
        br(),
        actionButton("inAdd","Add Selected Stands")
      ),
      tabPanel("Components",
        radioButtons("cmdSet", NULL, c("Management","Modifiers",
          "Outputs","Keywords","Your components"),inline=TRUE),
        selectInput ("addCategories","Categories",NULL,
                     multiple=FALSE,selectize=FALSE),
        selectInput("addComponents","Components",NULL,
                     multiple=FALSE,selectize=FALSE),
        uiOutput("cmdBuild"),
        tags$style(type="text/css", "#cmdCancel { color: red; }"),
        actionButton("cmdCancel","Cancel"),
        tags$style(type="text/css", "#cmdSave { color: green; }"),
        actionButton("cmdSave","Save"),
        uiOutput("cmdBuildDesc")
      ),
      tabPanel("Time",
        textInput("startyr",  "Common starting year", ""), 
        textInput("endyr",    "Common ending year",   ""), 
        textInput("cyclelen", "Common cycle length",  ""), 
        tags$style(type="text/css", "#cycleat { width: 90%; }"),
        textInput("cycleat", "Include cycles at these years", "") 
      ),
      tabPanel("Run",
        fixedRow(
          column(width=3,
            tags$style(type="text/css", "#defMgmtID { width: 65px; }"),
            textInput("defMgmtID","MgmtID (4 chars)",""),
            actionButton("saveandrun","Save and Run")
          ),
          column(width=9,
            checkboxGroupInput("autoOut",
              "Database output (summaries are always produced)",
              c("Treelists"="autoTreelists","Carbon"="autoCarbon",
                "Fire"="autoFire","Deadwood"="autoDead"),inline=TRUE),
            customRunElements
          )
        ), 
        uiOutput("uiRunPlot"),
        uiOutput("uiErrorScan") 
      ),
      tabPanel("Build Components",
        tags$style(type="text/css", "#kcpSel { width: 65%; }"),
        selectInput("kcpSel",NULL, NULL, NULL, multiple=FALSE,
                     selectize=FALSE),
        h6(" "),
        actionButton("kcpNew","New"),
        actionButton("kcpReload","Reload"),
        tags$style(type="text/css", "#kcpSave { color: green; }"),
        actionButton("kcpSave","Save"),
        tags$style(type="text/css", "#kcpDelete { color: red; }"),
        actionButton("kcpDelete","Delete"),
        h6(" "),
        tags$style(type="text/css", "#kcpTitle { width: 60%; }"),
        textInput("kcpTitle", "", ""), 
        h6(" "),
        tags$style(type="text/css", 
           "#kcpCols{font-family:monospace;font-size:90%;width:80%;}"), 
        tags$p(id="kcpCols", 
            HTML(paste0("&nbsp;",paste0("....+....",1:8,collapse="")))),
        tags$style(type="text/css", 
           "#kcpEdit{font-family:monospace;font-size:90%;width:95%;}"), 
        tags$textarea(id="kcpEdit", rows=15),
        h4(" "),
        fileInput("kcpUpload",
                  "Upload and append Component (keyword) file (.kcp).")
      ),
      tabPanel("Tables",
        fixedRow(
          column(width=5,
            selectInput("pivVar", choices=list("None"), 
	            "Variable to convert to columns", selectize=FALSE)),
          column(width=4,
            selectInput("dispVar", choices=list("None"), 
              "Variable to display", selectize=FALSE)),         
          column(width=3,
            actionButton("rpTableAdd","Add table to report")),
        fixedRow(column(width=12,tableOutput("table")))
      )),
      tabPanel("Graphs",
        fixedRow(
          column(width=5,
            radioButtons("plotType","Plot type", c("line","scatter",
              "box","bar"),inline=TRUE)),
          column(width=4,
            radioButtons("colBW","Scheme", c("color","B&W"),inline=TRUE)),
          column(width=3,
            actionButton("rpPlotAdd","Add graph to report"))),
        fixedRow(
          column(width=5,
          selectInput("xaxis", "X-axis", 
	          choices  = list("None"), selected = NULL, selectize=FALSE)),
          column(width=5,
            selectInput("yaxis", "Y-axis", choices  = list("Year"), 
	              selected = NULL, multiple = TRUE, selectize=FALSE))),
        fixedRow(
          column(width=4,
            selectInput("hfacet", "Horizontal facet",
	              choices = list("None","StandID","MgmtID","Year","Species"),
                selected="None", selectize=FALSE)),
          column(width=4,
	          selectInput("vfacet", "Vertical facet",
	              choices = list("None","StandID","MgmtID","Year","Species"),
	              selected="None", selectize=FALSE)),
          column(width=4,
	          selectInput("pltby", "Plot by code",
	              choices = list("None","StandID","MgmtID","Year","Species"),
	              selected="None", selectize=FALSE))),
        fixedRow(
          column(width=4,
            textInput("xlabel", "X-label", value = "")),
          column(width=4,
	          textInput("ylabel", "Y-label", value = "")),
          column(width=4,
            textInput("ptitle", "Title", value = ""))),
        fixedRow(
          column(width=4,
            textInput("width",  "Width (inches)", value = 6)),
          column(width=4,
	          textInput("height", "Height (inches)", value = 4)), 
	        column(width=4,
	          radioButtons("res","Resolution (ppi)", 
              c("144","288","576"),inline=TRUE))),	      
        fixedRow(column(width=12,plotOutput("outplot")))
      ),
      tabPanel("Reports",
        h4(" "),
        textInput("rpTitle", "Custom report title", 
          paste0("Custom report",if (nchar(tit)) " for project: ",tit)), 
          actionButton("rpRestart","Restart custom report"),
          downloadButton("rpBldDwnLd","Build and download custom report")
      ),
      tabPanel("Tools",       
        h4(" "),
        fileInput("upload","Upload FVS-Ready data (.accdb, .mdb, or .db)"),
        actionButton("launchDataEditor","Launch data editor (closes FVSOnline)"),
        h4(" "),
        tags$style(type="text/css", "#recoverdb { color: red; }"),
        actionButton("recoverdb","Recover from input database backup or default"),
        h4(" "),
        downloadButton("dlFVSDatadb","Download input data base"),
        downloadButton("dlFVSOutdb", "Download output data base for all runs"),
        downloadButton("dlFVSRunout","Download FVS output file for current run"),
        downloadButton("dlFVSRunkey","Download keyword file for current run"),
        h4(" "),
        checkboxGroupInput("dlZipSet","Select contents of fvsRun.zip", 
          zipList <- list(
              "Output data base for for all runs"  = "current.db",
              "Keyword file for current run" = "current.key",
              "FVS output file for current run" = "current.out",
              "SVS output files for current run" = "current/",
              "Input data base FVS_Data.db" = "FVS_Data.db",
              "FVS-Online runs archive (FVS_Runs.RData)" = "FVS_Runs.RData",
              "FVS-Online keyword component archive (FVS_kcps.RData)" =
                                                       "FVS_kcps.RData"),
              selected=unlist(zipList[1:4]),inline=FALSE),
        downloadButton("dlFVSRunZip","Download fvsRun.zip"),
        h6(" "),
        actionButton("FVSRefresh","Refresh or add selected FVS programs"),
        selectInput("FVSprograms", "Pick programs to add or refresh", multiple=TRUE,
           choices = list(), selected="", selectize=FALSE),
        h6(" "),
        modalTriggerButton("deleteRun", "#deleteRunDlg", 
          "Delete current run and related outputs"),
        modalDialog(id="deleteRunDlg", footer=list(
          modalTriggerButton("deleteRunDlgBtn", "#deleteRunDlg", 
            "Yes"),
          tags$button(type = "button", class = "btn btn-primary", 
            'data-dismiss' = "modal", "Cancel"))),        
        h6(" "),
        modalTriggerButton("deleteAllRuns", "#deleteAllRunsDlg", 
          "Delete ALL runs and related outputs"),
        modalDialog(id="deleteAllRunsDlg", footer=list(
          modalTriggerButton("deleteAllRunsDlgBtn", "#deleteAllRunsDlg", 
            "Yes"),
          tags$button(type = "button", class = "btn btn-primary", 
            'data-dismiss' = "modal", "Cancel"))),        
        h6(" "),
        modalTriggerButton("interfaceRefresh", "#interfaceRefreshDlg", 
          "Refresh this Interface Software"),
        modalDialog(id="interfaceRefreshDlg", footer=list(
          modalTriggerButton("interfaceRefreshDlgBtn", "#interfaceRefreshDlg", 
            "Yes"),
          tags$button(type = "button", class = "btn btn-primary", 
            'data-dismiss' = "modal", "Cancel"))),
        h6(" "),
        modalTriggerButton("restoreYesterday", "#restoreYesterdayDlg", 
          "Restore all files from yesterday's backup"),
        modalDialog(id="restoreYesterdayDlg", footer=list(
          modalTriggerButton("restoreYesterdayDlgBtn", "#restoreYesterdayDlg", 
            "Yes"),
          tags$button(type = "button", class = "btn btn-primary", 
            'data-dismiss' = "modal", "Cancel")))
	    )
    )
  ))
))
