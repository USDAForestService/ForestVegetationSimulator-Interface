# $Id$

# specifies the keywords that are loaded when automatic outputs are selected.

defaultOut="
DataBase
DSNOut
FVSOut.db
* FVS_Summary, FVS_Compute, Mistletoe
Summary        2
Compute            0         1
MisRpts        2
End
"

autoDelOTab="
DelOTab            1
DelOTab            2
DelOTab            4
"

autoTreelists="
* FVS_TreeList, FVS_Cutlist 
Treelist       0                   0
Cutlist        0                   0
Database
Treelist       2
Cutlist        2
End
"

autoCarbon = if ("fire" %in% extns) "
* FVS_Carbon, FVS_Hrv_Carbon, FVS_Fuels, FVS_Consumption
FMIn
CarbRept        2
CarbCut
CarbCalc        0         0
FuelOut         0
FuelRept
End
Database
CarbRpts        1
FuelRept        1
FuelsOut        1
End
" else NULL

autoFire = if ("fire" %in% extns) "
* FVS_Potfire, FVS_BurnReport, FVS_Mortality
FMIn
MortRept        0
BurnRept        0
PotFire         0
End
Database
MortRept        1        2
BurnRept        1
PotFire         1
End
" else NULL

if ("fire" %in% extns) autoDead="
*FVS_SnagSum, FVS_Down_Wood_Cov, FVS_Down_Wood_Vol
FMIn
SnagSum
DWDVlout
DWDCvOut
End
Database
SnagSum        1
DWDVlout       1
DWDCvOut       1
End
" else NULL

autoSVS="
SVS                3                   2         1        15
FMIn
SVImages           5
End
"

autoEcon="
Econ
NoTable            1
End
Database
EconRpts           1
End
"

autoCalibStats="
Database
CalbStat
End
"
autoInvStats="
Stats
Database
InvStats
End
"

autoRegen="
Database
REGREPTS
End
"

if ("climate" %in% extns) autoClimate="
Database
CLIMREPT
End
" else NULL

if ("fire" %in% extns) autoCanProfile="
FMIn
CanfProf
End
" else NULL

if ("fire" %in% extns) autoSnagDet="
FMIn
SnagOut
End
Database
SnagOut        1         2
End
" else NULL

if ("mist" %in% extns) autoDM_Sz_Sum="
Mistoe
MistPrt         1
End
" else NULL

if ("phewrd3" %in% extns) autoRD_Sum="
Database
RDSum
End
" else NULL

if ("phewrd3" %in% extns) autoRD_Det="
Database
RDDetail
End
" else NULL

autoRD_Beetle = if ("phewrd3" %in% extns) "
Database
RDBBMort
End
" else NULL

autoStrClass="
StrClass
Database
StrClass         1
End
"