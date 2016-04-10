# specifies the keywords that are loaded when automatic outputs are selected.

defaultOut="
DataBase
DSNOut
FVSOut.db
Summary
Compute
MisRpts        2
End
"

defaultOutMist=" 
Mistoe
MisTable       0         0
End
"

autoTreelists="
Treelist       0                   0
Cutlist        0                   0
Database
Treelist       1
Cutlist        1
End
"

autoCompute="
Database
Compute            0         0
End
"

autoCarbon="
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
FMIn
DWDVlout
DWDCvOut
End
Database
SnagOut        2         2
SnagSum        2
DWDVlout       2
DWDCvOut       2
End
"

