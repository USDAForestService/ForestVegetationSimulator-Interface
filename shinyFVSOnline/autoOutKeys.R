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
*FVS_SnagSum, FVS_Down_Wood_Cov, FVS_Down_Wood_Vol
FMIn
SnagSum
DWDVlout
DWDCvOut
End
Database
SnagSum        2
DWDVlout       2
DWDCvOut       2
End
"

autoSVS="
SVS                2                   2         1        15
"

autoEcon="
Econ
NoTable            1
End
Database
EconRpts           2
End
"

autoCalibStats="
Database
CalbStat
End
"

autoClimate="
Database
Climate
End
"

autoCanProfile="
FMIn
CanfProf
End
"

autoSnagDet="
FMIn
SnagOut
End
Database
SnagOut        2         2
End

"

autoDM_Sz_Sum="
Database
MisRpts          3
End
"

autoRD_Sum="
Database
RDSum
End
"

autoRD_Det="
Database
RDDetail
End
"

autoRD_Beetle="
Database
RDBBMort
End
"

autoStrClass="
Database
StrClass         2
End
"