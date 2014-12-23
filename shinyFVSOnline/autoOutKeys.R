# specifies the keywords that are loaded when automatic outputs are selected.

defaultOut="
DataBase
DSNOut
FVSOut.db
Summary
Compute
MisRpts        2
End
Mistoe
MisTable       0         0
End
"

autoTreelists="
Treelist       0                   0
Cutlist        0                   0
Database
Treelist       2         2
Cutlist        2         2
End
"

autoCarbon="
FMIn
CarbRept       2
CarbCut
FuelOut
End
Database
CarbRpts       2
FuelsOut       2
End
"

autoFire="
FMIn
MortRept
BurnRept
FuelRept        
PotFire
End
Database
MortRept       2        2
BurnRept       2
FuelRept       2
PotFire        2
End
"

autoDead="
FMIn
SnagOut
SnagSum
DWDVlout
DWDCvOut
End
Database
SnagOut       2         2
SnagSum       2
DWDVlout      2
DWDCvOut      2
End
"

