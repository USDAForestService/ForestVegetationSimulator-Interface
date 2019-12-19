# $Id$

library(shiny)
library(rhandsontable)
library(colourpicker)
options(rgl.useNULL=TRUE)
library(rgl)
library(leaflet)
library(openxlsx)

trim <- function (x) gsub("^\\s+|\\s+$","",x)
isLocal <- function () Sys.getenv('SHINY_PORT') == ""
# cbbPalette is used in the graphics
cbbPalette <- c("#FF0000","#009E73","#0072B2","#E69F00","#CC79A7","#0000FF",
                "#D55E00","#8F7800","#D608FA","#009100","#CF2C73","#00989D",
                "#00FF00","#BAF508","#202020","#6B6B6A","#56B4E9","#20D920")

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
  "FVS runs (RData files)" = "FVS_Runs",	
  "Custom SQL query archive (customQueries.RData)" = "customSQL",	
  "FVS keyword component archive (FVS_kcps.RData)" = "FVS_kcps")	
selZip <- unlist(zipList[1:4])	

tableList = list()
if (file.exists("databaseDescription.xlsx"))
{
  if ("OutputTableDescriptions" %in% getSheetNames("databaseDescription.xlsx"))
  tabs = read.xlsx(xlsxFile="databaseDescription.xlsx",sheet="OutputTableDescriptions")[,1]
  tableList = as.list(c("",tabs))
}

shinyUI(fixedPage(
  tags$head(tags$style(HTML(".shiny-notification {height: 80px;width: 500px;
              position:fixed;top: calc(50% - 40px);;left: calc(50% - 250px);;}"))),
  tags$style(type="text/css", ".progress-bar {color: transparent!important}"),
  tags$style(HTML(paste0(
    ".nav>li>a {padding:3px;}",
    ".btn {padding:2px 2px;color:darkred; background-color:#eef8ff;}",
    ".form-control {padding:2px 4px; height:auto;}",
    ".form-group {margin-bottom:5px}",
    ".leaflet-popup-content-wrapper,.leaflet-popup-tip {background: rgb(255, 255, 255, .7); box-shadow: 0 3px 14px rgba(0,0,0,0.4);"
    ))),  
  fixedRow(
    column(width=4,offset=0,
      HTML(paste0(
             '<h4><img src="FVSlogo.png" align="middle"</img>',
             '&nbsp;Forest Vegetation Simulator</h4>'))),
    column(width=4,offset=.5,uiOutput("projectTitle")),
    column(width=2,
      tags$style(type="text/css", paste0(".shiny-progress .progress-text {", 
             "background-color: #eef8ff; color: black; ",
             "position: absolute; left: 30px;",            
             "opacity: .9; height: 35px; width: 50%;}")),
      uiOutput("contCnts")),
    column(width=2,
      uiOutput("serverDate"),
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
            actionButton("cutCmp","Cut/Delete"),
            actionButton("copyCmp","Copy"),h6(),
            actionButton("paste","Paste item selected below"),
            selectInput("selpaste","Components available to paste", NULL, 
                        NULL, multiple=FALSE, selectize=FALSE), 
            myInlineTextInput("searchString", "Find stand:", value = "", size="25%"),
            actionButton("searchNext","Find")
          ),
          column(width=8,offset=.2,
            tags$style(type="text/css","#rightPan {background-color: rgb(227,255,227);}"),
            tabsetPanel(id = "rightPan",
              tabPanel("Stands",
                selectInput("inTabs","Inventory Data Tables", NULL, NULL, 
                          multiple=FALSE, selectize=FALSE),
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
                myInlineTextInput("inReps", "Number of replicates of each added stand ", value = "1", size="5%"),            
                myInlineTextInput("inRwts", "Relative weights of each replicate ", value = "1", size="20%"), 
                h6(),
                actionButton("inAdd",   "Add selected stands"),
                actionButton("inAddGrp","Add stands in selected groups"), 
                h6(),
                myInlineTextInput("inStdFind", "Find stand(s):", value = "", size="25%"),
                actionButton("inStdFindBut","Find")
              ), 
              tabPanel("Time",
                       textInput("startyr",  "Common starting year", ""), 
                       textInput("endyr",    "Common ending year",   ""), 
                       textInput("cyclelen", "Growth and reporting interval (years)",  ""), 
                       tags$style(type="text/css", "#cycleat { width: 90%; }"),
                       textInput("cycleat", "Additional output reporting years", ""),
                       h4("Projection Timing Summary"),
                       HTML(paste0('FVS will project your data, beginning from the year of inventory, to the common
                             starting year of ',htmlOutput("srtYr", inline=TRUE),' for all stands. Thereafter, FVS
                             will grow the stand, and provide output, in intervals of ',htmlOutput("cyLen", inline=TRUE),'
                             years, with the simulation ending at the common ending year, for all stands, of ',htmlOutput("eYr", inline=TRUE),
                             '. You will receive output for the additional year(s): ',htmlOutput("cyAt", inline=TRUE)))
              ),
              tabPanel("Components",          
                tags$style(type="text/css","#compTabSet {background-color: rgb(255,227,227);}"),     
                tabsetPanel(id = "compTabSet",
                  tabPanel("Management",
                    selectInput ("addMgmtCats","Categories",NULL,
                                 multiple=FALSE,selectize=FALSE),
                    selectInput("addMgmtCmps","Components",NULL,
                                 multiple=FALSE,selectize=FALSE)),
                 tabPanel("Modifiers",                    
                   selectInput("addModCats","Categories",NULL,
                               multiple=FALSE,selectize=FALSE),
                   selectInput("addModCmps","Components",NULL,
                               multiple=FALSE,selectize=FALSE)),
                 tabPanel("Event Monitor",
                   h6(),
                   selectInput("addEvCmps",NULL,NULL,multiple=FALSE,selectize=FALSE)),
                 tabPanel("Economic",h6()),
                 tabPanel("Keywords",
                   h5("Note: Avoid direct use of keywords when possible."),
                   selectInput("addKeyExt","Extensions",NULL,
                               multiple=FALSE,selectize=FALSE),
                   selectInput("addKeyWds","Keywords",NULL,
                               multiple=FALSE,selectize=FALSE)),
                 tabPanel("Addfile",
                   h5("Note: This is an advanced feature."),
                   fileInput("kcpUpload",
                             "Upload Keyword component file (.kcp), or Keyword component archive (FVS_kcps.Rdata).",
                             width="90%"),h6(),
                   tags$style(type="text/css", "#kcpSaveInRun {color:green;}"),
                   actionButton("kcpSaveInRun","Save in run"),
                   tags$style(type="text/css", "#kcpSaveCmps {color:green;}"),
                   actionButton("kcpSaveCmps","Save in component collection"),h6(),
                   selectInput("kcpSel","Existing component collection", NULL, 
                              NULL, multiple=FALSE,selectize=FALSE,width="65%"),h6(),
                   actionButton("kcpNew","New"),
                   tags$style(type="text/css", "#kcpDelete { color: red; }"),
                   actionButton("kcpDelete","Delete"),h6(),
                   tags$style(type="text/css", "#kcpTitle { width: 60%; }"),
                   myInlineTextInput("kcpTitle", "Addfile Title: ", value = "", size="65%"),h6(),      
                   tags$style(type="text/css", 
                      "#kcpCols{font-family:monospace;font-size:90%;width:80%;}"), 
                   tags$p(id="kcpCols", 
                       HTML(paste0("&nbsp;",paste0("....+....",1:8,collapse="")))),
                   tags$style(type="text/css", 
                      "#kcpEdit{font-family:monospace;font-size:90%;width:95%;}"), 
                   tags$textarea(id="kcpEdit", rows=15),h6(),
                   actionButton("kcpAppend","Append selected component from run"),h6())),                 
               uiOutput("cmdBuild"),
               uiOutput("cmdBuildDesc")
              ),
              tabPanel("Select Outputs",
                    h4("Select outputs"),
                    HTML("Note that all outputs are put in output database except for the SVS data.<br>
                         FVS_Cases, FVS_Summary, FVS_Compute, and mistletoe (FVS_DM_Stnd_Sum, 
                         FVS_DM_Spp_Sum) are always produced."),
                    checkboxGroupInput("autoOut",NULL,choices=list(
                        "SVS: Stand Visualization"="autoSVS",  
                        "Tree lists (FVS_Treelist, FVS_CutList (StdStk-stand and stock))"="autoTreelists",
                        "Carbon and fuels (FVS_Carbon, FVS_Consumption, FVS_Hrv_Carbon, FVS_Fuels)"="autoCarbon",
                        "Fire and mortality (FVS_Potfire, FVS_BurnReport, FVS_Mortality)"="autoFire",
                        "Snags and down wood (FVS_SnagSum, FVS_Down_Wood_Cov, FVS_Down_Wood_Vol)"="autoDead",
                        "FFE canopy profile (FVS_CanProfile)"="autoCanProfile",          
                        "FFE detailed snag (FVS_SnagDet)"="autoSnagDet",  
                        "Stand structure (FVS_StrClass)"="autoStrClass",
                        "Calibration stats (FVS_CalibStats)"="autoCalibStats",  
                        "Climate-FVS (FVS_Climate)"="autoClimate",                                    
                        "Economics (FVS_EconSummary, FVS_EconHarvestValue)"="autoEcon",
                        "Mistletoe detail by tree size (FVS_DM_Sz_Sum)"="autoDM_Sz_Sum",  
                        "Western Root Disease summary (FVS_RD_Sum)"="autoRD_Sum",  
                        "Western Root Disease details (FVS_RD_Det)"="autoRD_Det",  
                        "Western Root Disease bark beetles (FVS_RD_Beetle)"="autoRD_Beetle",
                        "Inventory Statistics (FVS_Stats_Species, FVS_Stats_Stand)"="autoInvStats",
                        "Regeneration (All Variants: FVS_Regen_Sprouts, FVS_Regen_SitePrep, FVS_Regen_Tally. 
                         AK, EM, KT, IE, and CI variants also get: FVS_Regen_HabType, FVS_Regen_Ingrowth)"="autoRegen",
                        "Produce all standard FVS text outputs (otherwise some are suppressed)"="autoDelOTab"  
                        ),width="100%",inline=FALSE),
                   selectInput("tabDescSel","Describe tables",choices=tableList,
                        selected=1,multiple=FALSE,selectize=FALSE),
                        h5(),uiOutput("tabDesc")
              ),
              tabPanel(title=htmlOutput("contChange"),
                fixedRow(
                  column(width=3,
                    tags$style(type="text/css", "#defMgmtID { width: 65px; }"),
                    textInput("defMgmtID","MgmtID (4 chars)",""),
                    radioButtons("runwaitback", NULL, 
                      c("Wait for run","Run in background")),
                    actionButton("saveandrun","Save and Run"),
                    h6(),
                    downloadButton("dlFVSRunout","FVS Main Output File"),
                    h4()
                  ),
                  column(width=9,
                    customRunElements
                  )
                ),
                uiOutput("uiRunPlot"),
                uiOutput("uiErrorScan"),
            	  selectInput("bkgRuns", "Background run status", 
        	        choices  = list(), size=4, width = "95%", selected = NULL, selectize=FALSE),
                actionButton("bkgKill","Kill selected background run"),
                actionButton("bkgRefresh","Refresh list")
              )
          ) )
      ) ),   #END Make Runs
      tabPanel("View Outputs",
        fixedRow(
        column(width=4,offset=0,
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
                                max=20,step=1)),
                column(width=5,
                  numericInput("sdskldbh", "Large DBH", 48, min=4, 
                                max=100,step=1))),
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
        tags$style(type="text/css","#outputRightPan {background-color: rgb(227,255,227);}"),
        conditionalPanel("input.leftPan != 'Load' & input.leftPan != 'Reports'", tabsetPanel(id="outputRightPan",
          tabPanel("Tables",
            fixedRow(
              column(width=4,
                selectInput("pivVar", choices=list("None"), 
    	            "Variable to convert to columns", selectize=FALSE)),
              column(width=3,
                selectInput("dispVar", choices=list("None"), 
                  "Variable to display", selectize=FALSE)),         
              column(width=5,
                   actionButton("rpTableAdd","Add table to report"),
                   downloadButton("dlRenderData","Download table"),
                   myRadioGroup("dlRDType","File type", c(".xlsx",".csv"))),
            tags$style(type="text/css","#tableLimitMsg{color:darkred;}"),          
            fixedRow(column(width=12,textOutput("tableLimitMsg"))),
            fixedRow(column(width=12,rHandsontableOutput("table")))
          ) ),
          tabPanel("Graphs",
            fixedRow(
              column(width=6,
                myRadioGroup("plotType","Type", 
                  c("line","scat","box","bar","DMD","StkCht"))),
              column(width=3,
                myRadioGroup("colBW","", c("Color","B&W"))),
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
    	          selectInput("pltby", "Plot-by code",
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
    	          myRadioGroup("moreControls","More controls", c("Hide","Show")))),
             fixedRow(column(width=12,conditionalPanel("input.moreControls == 'Show'",
               fixedRow(
                 column(width=2,               
                   colourInput("color1",  "Color 1",  value = cbbPalette[1]),
                   colourInput("color2",  "Color 2",  value = cbbPalette[2]),
                   colourInput("color3",  "Color 3",  value = cbbPalette[3])),
                 column(width=2,                                         
                   colourInput("color4",  "Color 4",  value = cbbPalette[4]),
                   colourInput("color5",  "Color 5",  value = cbbPalette[5]),
                   colourInput("color6",  "Color 6",  value = cbbPalette[6])),
                 column(width=2,                                         
                   colourInput("color7",  "Color 7",  value = cbbPalette[7]),
                   colourInput("color8",  "Color 8",  value = cbbPalette[8]),
                   colourInput("color9",  "Color 9",  value = cbbPalette[9])),
                 column(width=2,
                   colourInput("color10", "Color 10", value = cbbPalette[10]),
                   colourInput("color11", "Color 11", value = cbbPalette[11]),
                   colourInput("color12", "Color 12", value = cbbPalette[12])),
                 column(width=2,                                           
                   colourInput("color13", "Color 13", value = cbbPalette[13]),
                   colourInput("color14", "Color 14", value = cbbPalette[14]),
                   colourInput("color15", "Color 15", value = cbbPalette[15])),
                 column(width=2,                                         
                   colourInput("color16", "Color 16", value = cbbPalette[16]),
                   colourInput("color17", "Color 17", value = cbbPalette[17]),
                   colourInput("color18", "Color 18", value = cbbPalette[18]))),
               fixedRow(
                 column(width=3, 
  	               radioButtons("res","Resolution (ppi)",c("150","300","600"))),
                 column(width=4,
                 sliderInput("transparency", "Transparency", 0, 1, .3, step = .01)),
                 column(width=1),
                 column(width=2,fixedRow(textInput("YLimMin","Min Y limit:")),
                                fixedRow(textInput("YLimMax","Max Y limit:"))),
                 column(width=2,fixedRow(textInput("XLimMin","Min X limit:")), 
                                fixedRow(textInput("XLimMax","Max X limit:")))),
               fixedRow(
                 column(width=6,
                   myRadioGroup("YlabRot","Rotate Y-Labels (degrees)",
                      c("0"="0","45"="45","90"="90"))),
                 column(width=6,
                   myRadioGroup("XlabRot","Rotate X-Labels (degrees)",
                      c("0"="0","45"="45","90"="90")))),
               fixedRow(
                 column(width=6,
                   myRadioGroup("barPlace","Bars",
                      c("Side-by-side"="dodge","Stacked"="stack"))),
                 column(width=6,
                   myRadioGroup("legendPlace","Legend",
                      c("R"="right","Bot"="bottom","L"="left",
                        "Top"="top","None"="none")))),
               fixedRow(
                 column(width=6,
                   myRadioGroup("YTrans","Transform Y",
                      c("identity"="identity","log10"="log10"))),
                 column(width=6,
                   myRadioGroup("XTrans","Transform X",
                      c("identity"="identity","log10"="log10")))),
               fixedRow(
                 column(width=6,
                   myRadioGroup("facetWrap","Automatic facet wrap",
                      c("On"="On","Off"="Off")))),
               fixedRow(
                 column(width=12,
                   myInlineTextInput("SDIvals", 
                    "SDI values for Density Mgmt Diagram (DMG):",
                     "35%,60%,600", size=25))),
               fixedRow(
                 column(width=6,
                   myRadioGroup("YUnits","DMG Y-Units:",
                      c("Tpa"="Tpa","QMD"="QMD"),selected="Tpa")),
                 column(width=6,
                   myRadioGroup("XUnits","DMG X-Units",
                      c("Tpa"="Tpa","QMD"="QMD"),selected="QMD"))),
               fixedRow(
                 column(width=12,
                   myInlineTextInput("StkChtvals", 
                    "Full stocking percentages for Stocking Chart (StkCht):",
                     "30%,55%,100%,110%", size=25))))
            )),
            fixedRow(column(width=12, 
              tags$style(type="text/css","#plotMessage{color:darkred;}"),          
              textOutput("plotMessage"))),
            fixedRow(column(width=12,plotOutput("outplot")))
          )
        ) )       
      ) ) ),
      tabPanel("SVS3d",
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
      tabPanel("Maps",
        h6(),
        fixedRow(
        column(width=3,offset=0,
          selectInput(inputId="mapDsRunList",label="Select Run", choices=NULL, 
            selected=NULL, multiple=FALSE, selectize=FALSE, width="99%")),
        column(width=2,offset=0,
          selectInput(inputId="mapDsTable",label="Output Table", choices=NULL, 
            selected=NULL, multiple=FALSE, selectize=FALSE, width="99%")),
        column(width=2,offset=0,
          selectInput(inputId="mapDsVar",label="Variable", choices=NULL, 
            selected=NULL, multiple=FALSE, selectize=FALSE, width="99%")),
        column(width=3,offset=0,
           myRadioGroup("mapDsType","Display", c("table","graph"))),            
        column(width=2,offset=0,
          selectInput(inputId="mapDsProvider",label="Base map", 
            choices=list("Google Hybrid"="s,h","Google Satellite"="s",
                         "Google Streets"="m","Google Terrain"="p"),
            multiple=FALSE, selectize=FALSE, width="95%"))
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
        fixedRow(
          tags$head(tags$script(HTML('
             Shiny.addCustomMessageHandler("jsCode",
                                     function(message) {
                                     eval(message.code);
                                     }
          );
          '))),
        column(width=12,offset=0,
          tags$style(type="text/css","#inputDBPan {background-color: rgb(255,227,227);}"),
          tabsetPanel(id="inputDBPan", 
            tabPanel("Replace existing database", 
              h6(),
              fileInput("uploadNewDB",paste0("Step 1: Upload FVS-Ready database ",
                        "(.accdb, .mdb, .db (SQLite3), .sqlite, .xlsx, or .zip that contains one of these)"),
                        width="90%"),
              tags$style(type="text/css","#step1ActionMsg{color:darkred;}"), 
              uiOutput("step1ActionMsg"),
              h6(),
              p(strong("Step 2: Following upload, you must do one of the following")),
              tags$style(type="text/css","#installNewDB{font-size: 120%; color:green;}"),
              tags$style(type="text/css","#addNewDB{font-size: 120%; color:green;}"),
              actionButton("installNewDB","Install uploaded database"),
              actionButton("addNewDB","Add new database to existing database"),
              tags$style(type="text/css","#step2ActionMsg{color:darkred;}"), 
              uiOutput("step2ActionMsg"),
              h6(),
              p(strong("Other options")),
              actionButton("installTrainDB","Install regional training database"),
              h6(),
              actionButton("installEmptyDB","Install blank database"),h6()
      	    ),
            tabPanel("Upload and add new rows to existing tables (.csv)", 
              h4(),             
           	  selectInput("uploadSelDBtabs", label="Table to process",
      	        choices  = list(), selected = NULL, multiple = FALSE, selectize=FALSE),
              fileInput("uploadStdTree",
                       'Upload .csv file and append to "Table to process"',
                        width="90%"), 
              fileInput("climateFVSUpload",
                        "Upload and commit Climate-FVS data (replace existing, append new); FVSClimAttrs.csv or answers.zip).",
                        width="90%"),
              tags$style(type="text/css","#uploadActionMsg{color:darkred;}"), 
              uiOutput("uploadActionMsg")     
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
                  uiOutput("stdSel"),
                  myInlineTextInput("editStandSearch", "Find stand:", value = "", size="25%"),h6(),
                  actionButton("clearTable","Remove all rows and commit"),h6(),
                  actionButton("commitChanges","Commit edits or new rows")
                ),
                column(width=9,offset=0,
                  h6(),
                  uiOutput("navRows"),
                  h6(),
                  rHandsontableOutput("tbl"),
                  textOutput("actionMsg"))
              ),
              fixedRow(
                column(width=12,offset=0,
                  h6(),uiOutput("inputTabDesc")
            ))),              
            tabPanel("Map data", h4("Upload a stand layer to use in the Maps feature."),       
              fileInput("mapUpload","Upload polygon (best) or point data (.zip that contains spatial data)",
                      width="90%"), h6(),
           	  selectInput("mapUpLayers", label="Layer",
      	        choices  = list(), selected = NULL, multiple = FALSE, selectize=FALSE),
           	  selectInput("mapUpIDMatch", label="Variable that matches StandID",
      	        choices  = list(), selected = NULL, multiple = FALSE, selectize=FALSE),
           	  selectInput("mapUpSelectEPSG", label="Projection library (abridged)",
      	        choices  = list(), selected = NULL, multiple = FALSE, selectize=FALSE),
      	      textInput("mapUpProjection", label="proj4 projection string",width="70%"),
      	      actionButton("mapUpSetPrj","Set/Reset proj4 projection (does not reproject the data)"),h6(),
      	      tags$style(type="text/css","#mapUpSave{font-size: 120%; color:green;}"),
      	      actionButton("mapUpSave","Save imported map layer"),h6(),            
      	      textOutput("mapActionMsg") 
            ) #END tabPanel
          ) #END tabsetPanel
        ) ) #END column and fixed row   
      ),
      tabPanel("Tools",           
        fixedRow(
        column(width=12,offset=0,
          tags$style(type="text/css","#toolsPan {background-color: rgb(255,227,227);}"),
          tabsetPanel(id="toolsPan", 
            tabPanel("Manage project",        
                h4(),if (isLocal()) h4("Switch to another project") else
                                    h4("Start another project"), 
                selectInput("PrjSelect", "Select project", multiple=FALSE,
                   choices = list(), selected="", selectize=FALSE),       
                actionButton("PrjSwitch",if (isLocal()) "Switch to selected project" else
                    "Start selected project"),h4(),
                h4("Create a new project from your current project"),
                textInput("PrjNewTitle", "New project title", ""), 
                actionButton("PrjNew","Make new project"),
              h4("Delete outputs in current project"),
              modalTriggerButton("deleteAllOutputs", "#deleteAllOutputsDlg", 
                "Delete ALL outputs in current project"),
              modalDialog(id="deleteAllOutputsDlg", footer=list(
                modalTriggerButton("deleteAllOutputsDlgBtn", "#deleteAllOutputsDlg", 
                  "Yes"),
                tags$button(type = "button", class = "btn btn-primary", 
                  'data-dismiss' = "modal", "Cancel"))),                      
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
            tabPanel("Downloads", h6(),
              downloadButton("dlFVSDatadb","Input data base (all data)"),h6(),
              downloadButton("dlFVSOutdb", "Output data base (.db, all runs)"),h6(),
              downloadButton("dlFVSOutxlsx", "Output .xlsx for current run"),h6(),
              downloadButton("dlFVSRunkey","Keyword file for current run"),h4(),        
              checkboxGroupInput("dlZipSet","Set contents of fvsRun.zip", 	
                zipList,selZip,inline=FALSE),	
              downloadButton("dlFVSRunZip","Download fvsRun.zip")
            ),          
            tabPanel("Refresh/copy projects",
              fixedRow(
                if (isLocal()) list() else 
                column(width=if (isLocal()) 12 else 5,offset=0,
                  h4("Refresh current project from system sources"),
                  selectInput("FVSprograms", "Pick FVS variants to add or refresh", multiple=TRUE,
                    choices = list(), selected="", selectize=FALSE),
                  h6(),
                  actionButton("FVSRefresh","Refresh or add selected variants"),
                  h6(),   
                  radioButtons("interfaceRefreshSource","Select version of interface: ", 	
                             choices=list("Production version"="Prod",
                             "Development version"="Dev"),selected="Prod"),
                  actionButton("interfaceRefresh","Refresh interface software") 
                ),           
                column(width=if (isLocal()) 12 else 7,offset=0,
                  h4("Copy data and software from a source project to target project(s)"),
                  selectInput("sourcePrj", "Source project", multiple=FALSE,
                    choices = list(), selected="", selectize=FALSE),
                  h6(),       
                  selectInput("targetPrj", "Target project(s)", multiple=TRUE,
                    choices = list(), selected="", selectize=FALSE),
                  h6(),
                  checkboxGroupInput("cpyElts",label=NULL,width="100%",inline=FALSE,choices=
                    list("All interface software"="software",
                      "FVS variants"="FVSPrgms",
                      "Input database (FVS_Data.db)"="inDBS",
                      "Spatial data (SpatialData.RData)"="inSpace",
                      "Keyword component (.kcp) library"="kcps",
                      "Custom query library"="custQ"),
                      selected=c("software","FVSPrgms")),
                      actionButton("cpyNow","Copy now"),
                  h6(),
                  tags$style(type="text/css","#copyActionMsg{color:darkred;}"), 
                  uiOutput("copyActionMsg"))
                )
            )  #END tabPanel                                         
          ) #END tabsetPanel
        ) ) #END column and fixed row   
      ), ## END Tools
      tabPanel("Help",       
        h5(),
        uiOutput("uiHelpText")
      )
) ) ) ) )



