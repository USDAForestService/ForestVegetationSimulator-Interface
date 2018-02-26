# specifies the keywords that are loaded when automatic outputs are selected.

defaultOut="
DataBase
DSNOut
FVSOut.db
* FVS_Summary, FVS_Compute, FVS_DM_Stnd_Sum
Summary
Compute            0         0
MisRpts        2
End
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

autoCarbon="
* FVS_Carbon, FVS_Hrv_Carbon, FVS_Fuels, FVS_Consumption
FMIn
CarbRept        2
CarbCut
CarbCalc        0         0
FuelOut         0
End
Database
CarbRpts        2
FuelRept        2
FuelsOut        2
End
"

autoFire="
* FVS_Potfire, FVS_BurnReport, FVS_Mortality
FMIn
MortRept        0
BurnRept        0
PotFire         0
End
Database
MortRept        2        2
BurnRept        2
PotFire         2
End
"

autoDead="
* FVS_SnagSum, FVS_Down_Wood_Cov, FVS_Down_Wood_Vol
FMIn
* SnagOut
SnagSum
DWDVlout
DWDCvOut
End
Database
* SnagOut        2         2
SnagSum        2
DWDVlout       2
DWDCvOut       2
End
"

autoSVS="
SVS                2                   2         1        15
"
