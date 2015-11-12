exqury = function (dbcon,x,dbhclassexp=NULL) 
{
  lapply(scan(text=gsub("\n"," ",x),sep=";",what="",quote="",quiet=TRUE), 
    function (x,dbcon,dbhclassexp) 
    {
      if (!is.null(dbhclassexp)) x = sub("dbhclassexp",dbhclassexp,x)
      if (nchar(x) > 5) try(dbSendQuery(dbcon,statement=x)) else NULL
    },
  dbcon,dbhclassexp) 
}   

# dbhclassexp = "printf('%3.0f',round(dbh*.5,0)*2)"
mkdbhCase = function (smdbh=4,lgdbh=40)
{
  classes = seq(smdbh,lgdbh,smdbh)
  nc = nchar(as.character(classes[length(classes)])) + 1
  nc = paste0("%",nc,".",nc,"i")
  chrclasses = sprintf(nc,as.integer(classes))
  dbhclassexp = paste0("case when (dbh <= ",classes[1],") then '", 
                       chrclasses[1],"'")
  for (i in 1:(length(classes)-1))
  {
    dbhclassexp = paste0(dbhclassexp," when (dbh > ",classes[i]," and dbh <= ",
     classes[i+1],") then '", chrclasses[i+1],"'")
  }
  dbhclassexp = paste0(dbhclassexp," else '",sub("0",">",chrclasses[i+1]),
                       "' end ")
  dbhclassexp
}

Create_m.StdStk = "
drop table if exists m.StdStkDBHSp; 
drop table if exists m.StdStkAllDBH; 
drop table if exists m.StdStkAllSp; 
drop table if exists m.StdStkAllAll; 
create table m.StdStkDBHSp as 
select CaseID,Year,Species, 
    dbhclassexp as DBHClass, 
    sum(Tpa)          as LiveTpa, 
    sum(TCuFt*Tpa)    as LiveTCuFt, 
    sum(MCuFt*Tpa)    as LiveMCuFt, 
    sum(BdFt*Tpa)     as LiveBdFt, 
    sum(MortPA)       as MortPA, 
    sum(TCuFt*MortPA) as MortTCuFt, 
    sum(MCuFt*MortPA) as MortMCuFt,               
    sum(BdFt*MortPA)  as MortBdFt 
  from FVS_TreeList 
  where CaseID in (select CaseID from m.Cases)
  group by CaseID,Year,DBHClass,Species
  order by CaseID,Year,DBHClass,Species;
create table m.StdStkAllDBH as 
select CaseID,Year,Species,'All' as DBHClass,
    sum(LiveTpa)      as LiveTpa, 
    sum(LiveTCuFt)    as LiveTCuFt, 
    sum(LiveMCuFt)    as LiveMCuFt, 
    sum(LiveBdFt)     as LiveBdFt, 
    sum(MortPA)       as MortPA, 
    sum(MortTCuFt)    as MortTCuFt, 
    sum(MortMCuFt)    as MortMCuFt, 
    sum(MortBdFt)     as MortBdFt 
  from m.StdStkDBHSp 
  group by CaseID,Year,Species
  order by CaseID,Year,Species,DBHClass;
create table m.StdStkAllSp as 
select CaseID,Year,'All' as Species, DBHClass,
    sum(LiveTpa)      as LiveTpa, 
    sum(LiveTCuFt)    as LiveTCuFt, 
    sum(LiveMCuFt)    as LiveMCuFt, 
    sum(LiveBdFt)     as LiveBdFt, 
    sum(MortPA)       as MortPA, 
    sum(MortTCuFt)    as MortTCuFt, 
    sum(MortMCuFt)    as MortMCuFt, 
    sum(MortBdFt)     as MortBdFt 
  from m.StdStkDBHSp 
  group by CaseID,Year,DBHClass
  order by CaseID,Year,Species,DBHClass;
create table m.StdStkAllAll as 
select CaseID,Year,'All' as Species, 'All' as DBHClass,
    sum(LiveTpa)      as LiveTpa, 
    sum(LiveTCuFt)    as LiveTCuFt, 
    sum(LiveMCuFt)    as LiveMCuFt, 
    sum(LiveBdFt)     as LiveBdFt, 
    sum(MortPA)       as MortPA, 
    sum(MortTCuFt)    as MortTCuFt, 
    sum(MortMCuFt)    as MortMCuFt, 
    sum(MortBdFt)     as MortBdFt 
  from m.StdStkDBHSp 
  group by CaseID,Year
  order by CaseID,Year,Species,DBHClass;
insert into m.StdStkDBHSp select * from m.StdStkAllSp;
insert into m.StdStkDBHSp select * from m.StdStkAllDBH;
insert into m.StdStkDBHSp select * from m.StdStkAllAll;" 
 
  
Create_m.HrvStdStk = "
drop table if exists m.HrvStdStk;
drop table if exists m.HrvStdStkAllDBH;
drop table if exists m.HrvStdStkAllSp;
drop table if exists m.HrvStdStkAllAll;
create table m.HrvStdStk as
select CaseID,Year,Species, 
    dbhclassexp as dbhclass, 
    sum(Tpa)          as HrvPA,
    sum(TCuFt*Tpa)    as HrvTCuFt,
    sum(MCuFt*Tpa)    as HrvMCuFt,
    sum(BdFt*Tpa)     as HrvBdFt
  from FVS_CutList 
  where CaseID in (select CaseID from m.Cases)
  group by CaseID,Year,Species,DBHClass;
create table m.HrvStdStkAllDBH as 
select CaseID,Year,Species,'All' as DBHClass,
    sum(HrvPA)       as HrvPA,
    sum(HrvTCuFt)    as HrvTCuFt,
    sum(HrvMCuFt)    as HrvMCuFt,
    sum(HrvBdFt)     as HrvBdFt
  from m.HrvStdStk 
  group by CaseID,Year,Species;
create table m.HrvStdStkAllSp as 
select CaseID,Year,'All' as Species, DBHClass,
    sum(HrvPA)       as HrvPA,
    sum(HrvTCuFt)    as HrvTCuFt,
    sum(HrvMCuFt)    as HrvMCuFt,
    sum(HrvBdFt)     as HrvBdFt
  from m.HrvStdStk 
  group by CaseID,Year,DBHClass;
create table m.HrvStdStkAllAll as 
select CaseID,Year,'All' as Species, 'All' as DBHClass,
    sum(HrvPA)       as HrvPA,
    sum(HrvTCuFt)    as HrvTCuFt,
    sum(HrvMCuFt)    as HrvMCuFt,
    sum(HrvBdFt)     as HrvBdFt
  from m.HrvStdStk 
  group by CaseID,Year;
insert into m.HrvStdStk select * from m.HrvStdStkAllSp;
insert into m.HrvStdStk select * from m.HrvStdStkAllDBH;
insert into m.HrvStdStk select * from m.HrvStdStkAllAll;" 
  
Create_StdStk = "
drop table if exists m.StdStk1;
create table m.StdStk1 as select * from m.StdStkDBHSp 
 left join m.HrvStdStk using (CaseID,Year,Species,DBHClass);
drop table if exists StdStk;
create table StdStk as 
select Year,Species,DBHClass,
 LiveTpa, LiveTCuFt, LiveMCuFt, LiveBdFt,  
 HrvPA, HrvTCuFt, HrvMCuFt, HrvBdFt,
 MortPA, MortTCuFt, MortMCuFt, MortBdFt, CaseID
from m.StdStk1;"
#order by MgmtID,StandID,CaseID,Year,Species,DBHClass;"
  
Create_StdStkNoHrv = "
drop table if exists StdStk;
create table StdStk as 
select Year,Species,DBHClass,
 LiveTpa, LiveTCuFt, LiveMCuFt, LiveBdFt,  
 0 as HrvPA, 0 as HrvTCuFt, 0 as HrvMCuFt, 0 as HrvBdFt,
 MortPA, MortTCuFt, MortMCuFt, MortBdFt, CaseID
from m.StdStkDBHSp;"
#order by MgmtID,StandID,CaseID,Year,Species,DBHClass;"


Create_ATRTStdStk = "
drop table if exists m.ATRTStdStk;
drop table if exists m.ATRTStdStkAllDBH;
drop table if exists m.ATRTStdStkAllSp;
drop table if exists m.ATRTStdStkAllAll;
create table m.ATRTStdStk as
select CaseID,Year,Species, 
    printf('%3.0f',dbhclassexp as dbhclass, 
    sum(Tpa)          as ATRTPA,
    sum(TCuFt*Tpa)    as ATRTTCuFt,
    sum(MCuFt*Tpa)    as ATRTMCuFt,
    sum(BdFt*Tpa)     as ATRTBdFt
  from FVS_ATRTList 
  where CaseID in (select CaseID from m.Cases)
  group by CaseID,Year,Species,DBHClass;
create table m.ATRTStdStkAllDBH as 
select CaseID,Year,Species,'All' as DBHClass,
    sum(ATRTPA)       as ATRTPA,
    sum(ATRTTCuFt)    as ATRTTCuFt,
    sum(ATRTMCuFt)    as ATRTMCuFt,
    sum(ATRTBdFt)     as ATRTBdFt
    from m.ATRTStdStk 
  group by CaseID,Year,Species;
create table m.ATRTStdStkAllSp as 
select CaseID,Year,'All' as Species, DBHClass,
    sum(ATRTPA)       as ATRTPA,
    sum(ATRTTCuFt)    as ATRTTCuFt,
    sum(ATRTMCuFt)    as ATRTMCuFt,
    sum(ATRTBdFt)     as ATRTBdFt
    from m.ATRTStdStk 
  group by CaseID,Year,DBHClass;
create table m.ATRTStdStkAllAll as 
select CaseID,Year,'All' as Species, 'All' as DBHClass,
    sum(ATRTPA)       as ATRTPA,
    sum(ATRTTCuFt)    as ATRTTCuFt,
    sum(ATRTMCuFt)    as ATRTMCuFt,
    sum(ATRTBdFt)     as ATRTBdFt
    from m.ATRTStdStk 
  group by CaseID,Year;"

  
Create_Composite = "
drop table if exists Composite;
create table Composite as 
select MgmtID,Year,
    round(sum(Age    *SamplingWt)/sum(SamplingWt),2) as CmpAge,
    round(sum(Tpa    *SamplingWT)/sum(SamplingWt),2) as CmpTpa,    
    round(sum(BA     *SamplingWT)/sum(SamplingWt),2) as CmpBA,
    round(sum(SDI    *SamplingWT)/sum(SamplingWt),2) as CmpSDI,    
    round(sum(CCF    *SamplingWT)/sum(SamplingWt),2) as CmpCCF,
    round(sum(TopHt  *SamplingWT)/sum(SamplingWt),2) as CmpTopHt,
    round(sum(QMD    *SamplingWT)/sum(SamplingWt),2) as CmpQMD,
    round(sum(TCuFt  *SamplingWT)/sum(SamplingWt),2) as CmpTCuFt,
    round(sum(MCuFt  *SamplingWT)/sum(SamplingWt),2) as CmpMCuFt,
    round(sum(BdFt   *SamplingWT)/sum(SamplingWt),2) as CmpBdFt,
    round(sum(RTpa   *SamplingWT)/sum(SamplingWt),2) as CmpRTpa,  
    round(sum(RTCuFt *SamplingWT)/sum(SamplingWt),2) as CmpRTCuFt,      
    round(sum(RMCuFt *SamplingWT)/sum(SamplingWt),2) as CmpRMCuFt,
    round(sum(RBdFt  *SamplingWT)/sum(SamplingWt),2) as CmpRBdFt,
    round(sum(ATBA   *SamplingWT)/sum(SamplingWt),2) as CmpATBA,
    round(sum(ATSDI  *SamplingWT)/sum(SamplingWt),2) as CmpATSDI,
    round(sum(ATCCF  *SamplingWT)/sum(SamplingWt),2) as CmpATCCF,
    round(sum(ATTopHt*SamplingWT)/sum(SamplingWt),2) as CmpATTopHt,
    round(sum(ATQMD  *SamplingWT)/sum(SamplingWt),2) as CmpATQMD,
    round(sum(SamplingWt                        ),2) as CmpSamplingWt
  from (select * from FVS_Summary where CaseID in (select CaseID from m.Cases))
  join FVS_Cases using (CaseID)
  group by MgmtID,Year;"

