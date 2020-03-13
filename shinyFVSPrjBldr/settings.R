# all dirs need a final / and all names are case sensitive
# Edit dir names as needed for a specific installation

fvsBinDir    = "/shiny/FVS/open-fvs-read-only/trunk/bin/"
fvsWork      = "/shiny/shiny-server/FVSwork/"
fvsWorkBackup= "/shinyBackup/shiny-server/FVSwork/"
fvsOnlineDir = "/shiny/FVS/open-fvs-read-only/rFVS/shinyFVSOnline/"
fvsPrjBldr   = "/shiny/FVS/open-fvs-read-only/rFVS/shinyFVSPrjBldr/"
rFVSDir      = "/shiny/FVS/open-fvs-read-only/rFVS/R/"

pgmList= list(
  FVSak = "Southeast AK - Coastal BC",
  FVSbm = "Blue Mountains, Oregon",
  FVSca = "Inland CA, Southern Cascades",
  FVSci = "Central ID",
  FVScr = "Central Rockies",
  FVSec = "East Cascades, Washington",
  FVSem = "Eastern Montana",
  FVSie = "Inland Empire",
  FVSnc = "Klammath Mountains, Northern CA",
  FVSoc = "ORGANON Southwest",
  FVSop = "ORGANON Pacific Northwest",
  FVSso = "South Central OR N CA",
  FVStt = "Tetons, Wyoming",
  FVSut = "Utah",
  FVSwc = "West Cascades",
  FVSpn = "Pacific Northwest Coast",
  FVSws = "Western Sierra Nevada, CA",
  FVScs = "Central States",
  FVSkt = "Kootenai/Kaniksu/Tally LK, ID - MT",
  FVSls = "Lake States",
  FVSne = "Northeast",
  FVSsn = "Southern")

FVSOnlineNeeded = c(
"AcadianGY.R",
"AdirondackFunctionsV1.R",
"AdirondackV1.R",
"autoOutKeys.R",
"componentWins.R",
"customRun_fvsRunAcadian.R",
"customRun_fvsRunAdirondack.R",
"databaseDescription.xlsx",
"editDataUtilities.R",
"FVS_Data.db.default",
"FVS_Data.db.empty",
"fvsOnlineHelp.html",
"fvsOutUtilities.R",
"fvsRunUtilities.R",
"mkInputElements.R",
"mkpkeys.R",
"modalDialog.R",
"prms.RData",
"treeforms.RData",
"runScripts.R",
"SpatialData.RData",
"server.R", 
"sqlQueries.R",
"svsTree.R",
"ui.R",
"www")

