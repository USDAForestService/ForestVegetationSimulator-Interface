# all dirs need a final / and all names are case sensitive
# Edit dir names as needed for a specific installation

fvsBinDir    = "/shiny/FVS/open-fvs-read-only/trunk/bin/"
fvsWork      = "/shiny/shiny-server/FVSwork/"
fvsWorkBackup= "/shinyBackup/shiny-server/FVSwork/"
fvsOnlineDir = "/shiny/FVS/open-fvs-read-only/rFVS/shinyFVSOnline/"
fvsPrjBldr   = "/shiny/FVS/open-fvs-read-only/rFVS/shinyFVSPrjBldr/"
rFVSDir      = "/shiny/FVS/open-fvs-read-only/rFVS/R/"

pgmList= list(
  FVSak  = "Southeast AK - Coastal BC",
  FVSbmc = "Blue Mountains, Oregon",
  FVScac = "Inland CA, Southern Cascades",
  FVScic = "Central ID",
  FVScrc = "Central Rockies",
  FVSecc = "East Cascades, Washington",
  FVSemc = "Eastern Montana",
  FVSiec = "Inland Empire - 23 species",
  FVSncc = "Klammath Mountains, Northern CA",
  FVSsoc = "South Central OR N CA",
  FVSttc = "Tetons, Wyoming",
  FVSutc = "Utah",
  FVSwcc = "West Cascades",
  FVSpnc = "Pacific Northwest Coast",
  FVSwsc = "Western Sierra Nevada, CA",
  FVScs  = "Central States",
  FVSktc = "Kootenai/Kaniksu/Tally LK, ID - MT",
  FVSls  = "Lake States",
  FVSne  = "Northeast",
  FVSse  = "Southeast Twigs",
  FVSsn  = "Southern")

FVSOnlineNeeded = c(
"AcadianGY.R",
"AdirondackFunctionsV1.R",
"AdirondackV1.R",
"access2csv.jar",
"autoOutKeys.R",
"componentWins.R",
"customRun_fvsRunAcadian.R",
"customRun_fvsRunAdirondack.R",
"dbNamesAndTypes.R",
"editDataUtilities.R",
"FVS_Data.db.default",
"fvsOutUtilities.R",
"fvsRunUtilities.R",
"mkInputElements.R",
"mkpkeys.R",
"modalDialog.R",
"prms.RData",
"runScripts.R",
"server.R", 
"sqlQueries.R",
"suppose.prm",
"topHelp.R",
"ui.R",
"www")

