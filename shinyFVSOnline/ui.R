library(shiny)
library(rhandsontable)
library(colourpicker)
library(rgl)
library(leaflet)


trim <- function (x) gsub("^\\s+|\\s+$","",x)
isLocal <- function () Sys.getenv('SHINY_PORT') == ""

headstr = if (isLocal()) "Onlocal" else "Online"

defaultRun = list("Default useful for all FVS variants"="fvsRun")
if (file.exists("runScripts.R"))
{  
  source("runScripts.R")
  keep = file.exists(paste0("customRun_",runScripts,".R"))
  runScripts = runScripts[keep]
  if (length(runScripts)) runScripts = append(x=runScripts,after=0,defaultRun)
} 
customRunElements = list(                  
  selectInput("runScript",
              "Select run script (normally, use the default)",
              choices=runScripts,
              selected="fvsRun",multiple=FALSE,selectize=FALSE),
  uiOutput("uiCustomRunOps"))

source("modalDialog.R")
source("mkInputElements.R")

# used in Tools, dlZipSet
zipList <- list(	
  "Output data base for for all runs"  = "outdb",	
  "Keyword file for current run" = "key",	
  "FVS output file for current run" = "out",	
  "SVS output files for current run" = "subdir",	
  "Input data base FVS_Data.db" = "FVS_Data",	
  "FVS-Online runs archive (FVS_Runs.RData)" = "FVS_Runs",	
  "Custom SQL query archive (customQueries.RData)" =	
                                           "customSQL",	
  "FVS-Online keyword component archive (FVS_kcps.RData)" =	
                                           "FVS_kcps")	
selZip <- unlist(zipList[1:4])	


shinyUI(fixedPage(
  tags$style(HTML(paste0(
    ".nav>li>a {padding:3px;}",
    ".btn {padding:2px 2px;color:darkred; background-color:#eef8ff;}",
    ".form-control {padding:2px 4px; height:auto;}",
    ".form-group {margin-bottom:5px}"))),  
  fixedRow(
    column(width=5,offset=0,
      HTML(paste0('<title>FVS-',headstr,'</title>',
             '<h4><img src="FVSlogo.png" align="middle"</img>',
             '&nbsp;Forest Vegetation Simulator ',headstr,'</h4>'))),
    column(width=5,offset=.5,uiOutput("projectTitle")),
    column(width=2,
      tags$style(type="text/css", paste0(".shiny-progress .progress-text {", 
             "background-color: #eef8ff; color: black; ",
             "position: absolute; left: 30px;",            
             "opacity: .9; height: 35px; width: 50%;}")),
      uiOutput("contCnts"),
      singleton(tags$head(tags$script(src = "message-handler.js")))
  ) ),
  fixedRow(column(width=12,offset=0,
    tags$style(type="text/css","#topPan {background-color: rgb(227,227,255);}"),
    tabsetPanel(id="topPan",
      tabPanel("Runs",
        fixedRow(column(width=4,offset=0,
            h6(),
            tags$style(type="text/css", "#runSel { width: 100%; }"),
            selectInput("runSel","Selected run", NULL, NULL, multiple=FALSE,
                        selectize=FALSE),
            actionButton("newRun","New"),
            actionButton("reload","Reload"),
            tags$style(type="text/css", "#saveRun { color: green; }"),
            actionButton("saveRun","Save"),
            actionButton("dupRun","Duplicate"),
            modalTriggerButton("deleteRun", "#deleteRunDlg", "Delete"),
            #tags$style(type="text/css", "#deleteRun {color:red;}"),
            modalDialog(id="deleteRunDlg", footer=list(
            modalTriggerButton("deleteRunDlgBtn", "#deleteRunDlg", "Yes"),
              tags$button(type = "button", class = "btn btn-primary", 
               'data-dismiss' = "modal", "Cancel"))),        
            h6(),
            tags$style(type="text/css", "#title { width: 90%; }"),
            textInput("title", "Run title", ""), 
            # all the select objects
            tags$style(type="text/css", "select { width: 100%; }"),
            h6(),
            tags$style(type="text/css", "#simCont { width: 100%; height: 400px;}"),
            selectInput("simCont","Contents", NULL, NULL, multiple=TRUE,
                        selectize=FALSE),
            actionButton("editSel","Edit"),
            actionButton("mkfree","Change to freeform"),
            actionButton("cutCmp","Cut"),
            actionButton("copyCmp","Copy"),
            h6(),
            actionButton("paste","Paste item selected below"),
            selectInput("selpaste","Items to paste", NULL, 
                        NULL, multiple=FALSE, selectize=FALSE), 
            myInlineTextInput("searchString", "Find stand:", value = "", size="25%"),
            actionButton("searchNext","Find")
          ),
          column(width=8,offset=.2,
            tags$style(type="text/css","#rightPan {background-color: rgb(227,255,227);}"),
            h6(),
            tabsetPanel(id = "rightPan",
              tabPanel("Stands",
                selectInput("inVars","Variants", NULL, NULL, 
                          multiple=FALSE, selectize=FALSE),
                selectInput("inGrps","Groups", NULL, NULL, 
                          multiple=TRUE, selectize=FALSE, size=6),
                myRadioGroup("inAnyAll", "Stands must be in any or all selected groups ", 
                  c("Any","All")),               
                tags$style(type="text/css", "#inStds { height: 300px;}"),
                selectInput("inStds", NULL, NULL, NULL, 
                          multiple=TRUE, selectize=FALSE),
                uiOutput("stdSelMsg"),
                actionButton("inAdd",   "Add selected stands"),
                actionButton("inAddGrp","Add stands in selected groups"), 
                myInlineTextInput("inStdFind", "Find stand(s):", value = "", size="25%"),
                actionButton("inStdFindBut","Find")
               ),
              tabPanel("Components",
                radioButtons("cmdSet", NULL, c("Management","Modifiers",
                  "Outputs","Keywords","Your components"),inline=TRUE),
                selectInput ("addCategories","Categories",NULL,
                             multiple=FALSE,selectize=FALSE),
                selectInput("addComponents","Components",NULL,
                             multiple=FALSE,selectize=FALSE),
                uiOutput("cmdBuild"),
                tags$style(type="text/css", "#cmdCancel {color:red;}"),
                actionButton("cmdCancel","Cancel"),
                tags$style(type="text/css", "#cmdSaveInRun {color:green;}"),
                actionButton("cmdSaveInRun","Save in run"),
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
                    radioButtons("runwaitback", NULL, 
                      c("Wait for run","Run in background")),
                    actionButton("saveandrun","Save and Run"),
                    h6(),
                    downloadButton("dlFVSRunout","FVS output"),
                    h4()
                  ),
                  column(width=9,
                    checkboxGroupInput("autoOut",
                      "Select outputs (summaries are always produced)",
                      c("Treelists"="autoTreelists","Compute"="autoCompute",
                        "Carbon"="autoCarbon","Fire"="autoFire",
                        "Deadwood"="autoDead","SVS"="autoSVS"),
                      inline=TRUE),
                    customRunElements
                  )
                ),
                uiOutput("uiRunPlot"),
                uiOutput("uiErrorScan"),
            	  selectInput("bkgRuns", "Background run status", 
        	        choices  = list(), size=2, width = "75%", selected = NULL, selectize=FALSE),
                actionButton("bkgKill","Kill selected background run"),
                actionButton("bkgRefresh","Refresh list")
              ),
              tabPanel("Build Components",
                selectInput("kcpSel","Existing components", NULL, NULL, multiple=FALSE,
                             selectize=FALSE,width="65%"),
                h6(),
                actionButton("kcpNew","New"),
                tags$style(type="text/css", "#kcpDelete { color: red; }"),
                actionButton("kcpDelete","Delete"),
                h6(),
                tags$style(type="text/css", "#kcpTitle { width: 60%; }"),
                myInlineTextInput("kcpTitle", "Title: ", value = "", size="65%"),
                h6(),
                tags$style(type="text/css", 
                   "#kcpCols{font-family:monospace;font-size:90%;width:80%;}"), 
                tags$p(id="kcpCols", 
                    HTML(paste0("&nbsp;",paste0("....+....",1:8,collapse="")))),
                tags$style(type="text/css", 
                   "#kcpEdit{font-family:monospace;font-size:90%;width:95%;}"), 
                tags$textarea(id="kcpEdit", rows=15),                                             
                h6(),
                tags$style(type="text/css", "#kcpSaveCmps { color: green; }"),
                actionButton("kcpSaveCmps","Save in component collection"),h6(),
                actionButton("kcpAppend","Append selected component from run"),h6(),
                fileInput("kcpUpload",
                          "Upload and append Component (keyword) file (.kcp).",
                          width="90%")
              )
          ) )
      ) ),   #END Make Runs
      tabPanel("Output Tables",
        fixedRow(
        column(width=4,offset=0,
          h6(),
          tags$style(type="text/css","#leftPan {background-color: rgb(255,227,227);}"),
          tabsetPanel(id="leftPan", 
            tabPanel("Load", 
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
              actionButton("bldstdsk","Rebuild StdStk"), 
          	  selectInput("selectdbtables", "Database tables to consider", 
      	        choices  = list(), size=8,
      	        selected = NULL, multiple = TRUE, selectize=FALSE),
          	  selectInput("selectdbvars", "Database variables to consider", 
                choices  = list(), size=20,
                selected = NULL, multiple = TRUE, selectize=FALSE)
      	    ),
            tabPanel("Explore", 
          	  selectInput("stdtitle", "Select run titles", size=4,
      	          choices  = list("None loaded"), width="100%",
      	          selected = NULL, multiple = TRUE, selectize=FALSE),
          	  selectInput("stdgroups", "Groups", size=4,
      	          choices  = list("None loaded"), width="100%",
      	          selected = NULL, multiple = TRUE, selectize=FALSE),
              fixedRow(
                column(width=8,
          	      selectInput("stdid", "Stands", size=6,
      	            choices  = list("None loaded"), 
      	            selected = NULL, multiple = TRUE, selectize=FALSE)),
                column(width=4,
                  selectInput("mgmid", "MgmtIDs", size=6, 
      	            choices  = list("None loaded"), 
      	            selected = NULL, multiple = TRUE, selectize=FALSE))),
              fixedRow(
                column(width=4,
                  selectInput("year", "Years", size=6,  
      	              choices  = list("None loaded"), 
      	              selected = NULL, multiple = TRUE, selectize=FALSE)),    	     
      	        column(width=4,
      	          selectInput("species", "Species", size=6, 
      	            choices  = list("None loaded"), 
      	            selected = NULL, multiple = TRUE, selectize=FALSE)),
      	        column(width=4,
      	          selectInput("dbhclass", "DBHClasses", size=6, 
      	            choices  = list("None loaded"), 
      	            selected = NULL, multiple = TRUE, selectize=FALSE))),
      	      checkboxGroupInput("browsevars","Select variables",
      	          choices = list("None"),selected = NULL,inline=TRUE)
            ),
            tabPanel("Custom Query",
              selectInput("sqlSel","SQL queries (run on FVSOut.db (SQLite3))", 
                NULL, NULL, multiple=FALSE,selectize=FALSE,width="100%"),
              textInput("sqlTitle", "Query name: ", value = "", width="100%"),
              h6(),
              tags$style(type="text/css", 
                    "#sqlQuery{font-family:monospace;font-size:90%;width:100%;}"), 
              tags$textarea(id="sqlQuery",rows=15,""),
              h6(),
              tags$style(type="text/css", "#sqlRunQuery { color: green; }"),
              actionButton("sqlRunQuery","Run query"),
              actionButton("sqlSave","Save"),
              actionButton("sqlNew","New"),
              tags$style(type="text/css", "#sqlDelete { color: red; }"),
              actionButton("sqlDelete","Delete"),
              h6("Output from query"),
              tags$style(type="text/css", 
                    "#sqlOutput{font-family:monospace;font-size:90%;width:100%;}"), 
              tags$textarea(id="sqlOutput",rows=5,""),
              tags$p(id="sqlInstructions", 
                  HTML(paste0('Use "<b>;</b>" to separate SQL statements.<br>',
                    'The last statement that results in a table being returned ',
                    'defines the end of the sequence. That table is ',
                    'used in <b>Tables</b> and <b>Graphs</b>')))
        ) ) ),
        column(width=8,offset=.2,
        h6(),
        tags$style(type="text/css","#outputRightPan {background-color: rgb(227,255,227);}"),
        tabsetPanel(id="outputRightPan",
          tabPanel("Tables",
            fixedRow(
              column(width=5,
                selectInput("pivVar", choices=list("None"), 
    	            "Variable to convert to columns", selectize=FALSE)),
              column(width=4,
                selectInput("dispVar", choices=list("None"), 
                  "Variable to display", selectize=FALSE)),         
              column(width=3,
                actionButton("rpTableAdd","Add table to report"),
                downloadButton("dlRenderData","table.csv")),
            fixedRow(column(width=12,rHandsontableOutput("table")))
          ) ),
          tabPanel("Graphs",
            fixedRow(
              column(width=5,
                myRadioGroup("plotType","Plot type", c("line","scatter",
                  "box","bar"))),
              column(width=4,
                myRadioGroup("colBW","Scheme", c("color","B&W"))),
              column(width=2,
                actionButton("rpPlotAdd","Add graph to report"))),
            fixedRow(
              column(width=3,
                selectInput("yaxis", "Y-axis", choices  = list("Year"), 
    	              selected = NULL, multiple = TRUE, selectize=FALSE)),
              column(width=3,
                selectInput("xaxis", "X-axis", 
    	            choices  = list("None"), selected = NULL, selectize=FALSE),
                selectInput("hfacet", "Horizontal facet",
    	              choices = list("None","StandID","MgmtID","Year","Species"),
                    selected="None", selectize=FALSE)),
              column(width=6,
                textInput("ptitle", "Title", value = ""),	            
                textInput("xlabel", "X-label", value = ""))),
            fixedRow(
              column(width=3,
    	          selectInput("vfacet", "Vertical facet",
    	              choices = list("None","StandID","MgmtID","Year","Species"),
    	              selected="None", selectize=FALSE)),
              column(width=3,
    	          selectInput("pltby", "Plot by code",
    	              choices = list("None","StandID","MgmtID","Year","Species"),
    	              selected="None", selectize=FALSE)),
              column(width=6,
    	          textInput("ylabel", "Y-label", value = ""))),
            fixedRow(
              column(width=3,
                myInlineTextInput("width",  "Width (in)", value = 4, size=5)),
              column(width=3,
    	          myInlineTextInput("height", "Height (in)", value = 3, size=5)), 
    	        column(width=6,
    	          actionButton("moreControls","More controls"),
    	          actionButton("hideControls","Hide controls"))),
            fixedRow(uiOutput("graphControls")),
            fixedRow(column(width=12,plotOutput("outplot")))
          ),
          tabPanel("Reports",
            h4(),
            textInput("rpTitle", "Custom report title", ""),
            actionButton("rpRestart","Restart custom report"),
            downloadButton("rpBldDwnLd","Build and download custom report")
        ) )       
      ) ) ),
      tabPanel("SVS3d(alpha)",
        h6(),
        fixedRow(
        column(width=6,offset=0,
          selectInput(inputId="SVSRunList1",label="Select Run", choices=NULL, 
            selected=NULL, multiple=FALSE, selectize=FALSE, width="99%"),
          selectInput(inputId="SVSImgList1",label="Select SVS case", choices=NULL, 
            selected=NULL, multiple=FALSE, selectize=FALSE, width="99%"),
          rglwidgetOutput('SVSImg1',width = "500px", height = "500px")),
        column(width=6,offset=0,
          selectInput(inputId="SVSRunList2",label="Select Run", choices=NULL, 
            selected=NULL, multiple=FALSE, selectize=FALSE, width="99%"),
          selectInput(inputId="SVSImgList2",label="Select SVS case", choices=NULL, 
            selected=NULL, multiple=FALSE, selectize=FALSE, width="99%"),
          rglwidgetOutput('SVSImg2',width = "500px", height = "500px"))
      )),
      tabPanel("Maps(alpha)",
        h6(),
        fixedRow(
        column(width=3,offset=0,
          selectInput(inputId="mapDsRunList",label="Select Run", choices=NULL, 
            selected=NULL, multiple=FALSE, selectize=FALSE, width="95%")),
        column(width=2,offset=0,
          selectInput(inputId="mapDsTable",label="Output Table", choices=NULL, 
            selected=NULL, multiple=FALSE, selectize=FALSE, width="95%")),
        column(width=2,offset=0,
          selectInput(inputId="mapDsVar",label="Variable", choices=NULL, 
            selected=NULL, multiple=FALSE, selectize=FALSE, width="95%")),
        column(width=5,offset=0,
           myRadioGroup("mapDsType","Display", c("table","graph")))            
#        column(width=2,offset=0,
#          selectInput(inputId="mapDsProvider",label="Underlay map", 
#            choices=leaflet::providers,selected="Esri.WorldTopoMap",
#            multiple=FALSE, selectize=FALSE, width="95%"))
         ), 
         fixedRow(
         column(width=12,offset=0,
           textOutput("leafletMessage"),
           leafletOutput("leafletMap",height="800px",width="100%"))
      )),
      tabPanel("Import Data",
        tags$script('
           Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {   
               var el = $("#" + x);
               el.replaceWith(el = el.clone(true));
               var idƒ = "#" + x + "_progress";     
               $(id).css("visibility", "hidden");});'
        ),
        h6(),        
        fixedRow(
        column(width=12,offset=0,
          tags$style(type="text/css","#inputDBPan {background-color: rgb(255,227,227);}"),
          tabsetPanel(id="inputDBPan", 
            tabPanel("Replace existing database", 
              h6(),
              fileInput("uploadNewDB","Upload and install FVS-Ready database (.accdb, .mdb, or .db (SQLite3))",
                      width="90%"), h6(), 
              actionButton("installTrainDB","Install training database"),h6(),
              actionButton("installEmptyDB","Install empty database"),h6(),
              tags$style(type="text/css","#replaceActionMsg{color:darkred;}"), 
              textOutput("replaceActionMsg")
      	    ),
            tabPanel("Upload and insert new rows (.csv)", 
              h4(),             
           	  selectInput("uploadSelDBtabs", label="Table to process",
      	        choices  = list(), selected = NULL, multiple = FALSE, selectize=FALSE),
              fileInput("uploadStdTree",
                       'Upload and commit to "Table to process" (.csv, data will be appended)',
                        width="90%"), 
              fileInput("climateFVSUpload",
                        "Upload and commit Climate-FVS data (append and replace; FVSClimAttrs.csv or answers.zip).",
                        width="90%"),
              tags$style(type="text/css","#uploadActionMsg{color:darkred;}"), 
              textOutput("uploadActionMsg")     
            ),
            tabPanel("View and edit existing tables",        
              fixedRow(
                column(width=3,offset=0,
                  h6(),
                  myRadioGroup("mode", "Mode ", c("Edit","New rows")),
                  myInlineTextInput("disprows",  "Number display rows", value = 20, size=5),
              	  selectInput("editSelDBtabs", label="Table to process",
      	                choices  = list(), 
      	                selected = NULL, multiple = FALSE, selectize=FALSE),
              	  selectInput("editSelDBvars", "Variables to consider", 
                      choices  = list(), size=10,
                      selected = NULL, multiple = TRUE, selectize=FALSE),
                  uiOutput("stdSel"),h6(),
                  actionButton("clearTable","Remove all rows and commit"),h6(),
                  actionButton("commitChanges","Commit edits or new rows")
                ),
                column(width=9,offset=0,
                  h6(),
                  uiOutput("navRows"),
                  h6(),
                  rHandsontableOutput("tbl"),
                  textOutput("actionMsg"))
             )),              
            tabPanel("Map data", h6(),       
              fileInput("mapUpload","Upload polygon data (.zip that contains spatial data)",
                      width="90%"), h6(),
           	  selectInput("mapUpLayers", label="Layer",
      	        choices  = list(), selected = NULL, multiple = FALSE, selectize=FALSE),
           	  selectInput("mapUpIDMatch", label="Variable that matches StandID",
      	        choices  = list(), selected = NULL, multiple = FALSE, selectize=FALSE),
           	  selectInput("mapUpSelectEPSG", label="Projection library",
      	        choices  = list(), selected = NULL, multiple = FALSE, selectize=FALSE),
      	      textInput("mapUpProjection", label="proj4 projection string",width="70%"),
      	      actionButton("mapUpSetPrj","Set/Reset proj4 projection (does not reproject the data)"),h6(),
      	      actionButton("mapUpSave","Save imported map"),h6(),            
      	      textOutput("mapActionMsg") 
            ) #END tabPanel
          ) #END tabsetPanel
        ) ) #END column and fixed row   
      ),
      tabPanel("Tools",       
        h6(),        
        fixedRow(
        column(width=12,offset=0,
          tags$style(type="text/css","#toolsPan {background-color: rgb(255,227,227);}"),
          tabsetPanel(id="toolsPan", 
            tabPanel("Manage projects",        
              if (isLocal()) list(
                h4(),h4("Switch to another project"), 
                selectInput("PrjSelect", "Select project", multiple=FALSE,
                   choices = list(), selected="", selectize=FALSE),       
                actionButton("PrjSwitch","Switch to selected project"), 
                h4(),h4("Setup a new project"),
                textInput("PrjNewTitle", "New project title", ""), 
                actionButton("PrjNew","Make new project")), 
              h4(),h4("Delete runs in current project"),
              modalTriggerButton("deleteAllRuns", "#deleteAllRunsDlg", 
                "Delete ALL runs and related outputs in current project"),
              modalDialog(id="deleteAllRunsDlg", footer=list(
                modalTriggerButton("deleteAllRunsDlgBtn", "#deleteAllRunsDlg", 
                  "Yes"),
                tags$button(type = "button", class = "btn btn-primary", 
                  'data-dismiss' = "modal", "Cancel"))),        
              h4(),h4("Manage current project backup files"),
              actionButton("mkZipBackup","Make a project backup zip file"),
              h4(),
              selectInput("pickBackup", "Select backup to process", multiple=FALSE,
                 choices = list(), selected="", selectize=FALSE),
              actionButton("delZipBackup","Delete backup"),
              downloadButton("dlPrjBackup","Download backup"),
              list(
                modalTriggerButton("restorePrjBackup", "#restorePrjBackupDlg", 
                  "Restore from backup"),
                modalDialog(id="restorePrjBackupDlg", footer=list(
                  modalTriggerButton("restorePrjBackupDlgBtn", "#restorePrjBackupDlg", 
                    "Yes"),
                  tags$button(type = "button", class = "btn btn-primary", 
                    'data-dismiss' = "modal", "Cancel")))
              )
            ), 
            tabPanel("Downloads", 
              h6(),
              downloadButton("dlFVSDatadb","Input data base"),
              downloadButton("dlFVSOutdb", "Output data base"),
              downloadButton("dlFVSRunkey","Keyword file"),
              h4(),        
              checkboxGroupInput("dlZipSet","Select contents of fvsRun.zip", 	
                zipList,selZip,inline=FALSE),	
              downloadButton("dlFVSRunZip","Download fvsRun.zip")
            ),
            tabPanel("Refresh FVS", 
              h4(),
              selectInput("FVSprograms", "Pick programs to add or refresh", multiple=TRUE,
                choices = list(), selected="", selectize=FALSE),
              h6(),
              actionButton("FVSRefresh","Refresh or add selected FVS programs"),
              h6(),
              modalTriggerButton("interfaceRefresh", "#interfaceRefreshDlg", 
                "Refresh this Interface Software"),
              modalDialog(id="interfaceRefreshDlg", footer=list(
              modalTriggerButton("interfaceRefreshDlgBtn", "#interfaceRefreshDlg", 
                  "Yes"),
             tags$button(type = "button", class = "btn btn-primary", 
                'data-dismiss' = "modal", "Cancel")))
            )  #END tabPanel                                         
          ) #END tabsetPanel
        ) ) #END column and fixed row   
      ), ## END Tools
      tabPanel("Help",       
        h5(),
        uiOutput("uiHelpText")
      )
) ) ) ) )



