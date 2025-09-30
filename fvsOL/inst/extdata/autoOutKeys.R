
# specifies the keywords that are loaded when automatic outputs are selected.

defaultOut="
DataBase
DSNOut
FVSOut.db
* FVS_Summary, FVS_Compute, Mistletoe
Summary        2
Computdb          0         1
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
Atrtlist       0                   0
Database
TreeLiDB       2
CutLiDB        2
Atrtlidb       2
End
"

autoFIAVBC="
*FIAVBC Outputs
FIAVBC
DATABASE
VBCSUMDB
VBCTRLDB
VBCCUTDB
VBCATRDB
END
"

autoCarbon = if ("fire" %in% extns) "
* FVS_Carbon, FVS_Hrv_Carbon, FVS_Fuels, FVS_Consumption
FMIn
CarbRept        2
CarbCut
CarbCalc
FuelOut         0
FuelRept
End
Database
CarbReDB        2
FuelReDB        2
FuelsOut        2
End
" else NULL

autoCarbon.withText = if ("fire" %in% extns) "
* FVS_Carbon, FVS_Hrv_Carbon, FVS_Fuels, FVS_Consumption
FMIn
CarbRept        2
CarbCut
CarbCalc
FuelOut         0
FuelRept
End
Database
CarbReDB        1
FuelReDB        1
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
MortReDB        2        2
BurnReDB        2
PotFirDB        2
End
" else NULL

autoFire.withText = if ("fire" %in% extns) "
* FVS_Potfire, FVS_BurnReport, FVS_Mortality
FMIn
MortRept        0
BurnRept        0
PotFire         0
End
Database
MortReDB        1        2
BurnReDB        1
PotFirDB        1
End
" else NULL

autoDead = if ("fire" %in% extns) "
*FVS_SnagSum, FVS_Down_Wood_Cov, FVS_Down_Wood_Vol
FMIn
SnagSum
DWDVlout
DWDCvOut
End
Database
SnagSuDB       2
DWDVlDB        2
DWDCvDB        2
End
" else NULL

autoDead.withText = if ("fire" %in% extns)  "
*FVS_SnagSum, FVS_Down_Wood_Cov, FVS_Down_Wood_Vol
FMIn
SnagSum
DWDVlout
DWDCvOut
End
Database
SnagSuDB       1
DWDVlDB        1
DWDCvDB        1
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
NoTable            0
End
Database
EconRpts           2
End
"

autoCalibStats="
Database
CalbStDB
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
autoClimate = if ("climate" %in% extns) "
Database
ClimReDB
End
" else NULL

autoCanProfile = if ("fire" %in% extns) "
FMIn
CanfProf
End
" else NULL

autoSnagDet = if ("fire" %in% extns) "
FMIn
SnagOut
End
Database
SnagOuDB      2         2
End
" else NULL
autoSnagDet.withText <- if ("fire" %in% extns) "
FMIn
SnagOut
End
Database
SnagOuDB      1         2
End
" else NULL

autoDM_Sz_Sum = if ("mist" %in% extns) "
Mistoe
MistPrt         1
End
" else NULL

autoRD_Sum = if ("phewrd3" %in% extns) "
Database
RDSum
End
" else NULL

autoRD_Det = if ("phewrd3" %in% extns) "
Database
RDDetail
End
" else NULL

autoRD_Beetle = if ("phewrd3" %in% extns) "
Database
RDBBMort
End
" else NULL

autoStrClass <- "
StrClass
Database
StrClsDB         2
End
"
autoStrClass.withText <- "
StrClass
Database
StrClsDB         1
End
"
