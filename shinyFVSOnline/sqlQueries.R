exqury = function (dbcon,x,subExpression=NULL) 
{                 
  lapply(scan(text=gsub("\n"," ",x),sep=";",what="",quote="",quiet=TRUE), 
    function (x,dbcon,subExpression) 
    {
      if (!is.null(subExpression)) x = sub("subExpression",subExpression,x)
      res = if (nchar(x) > 5) try(dbSendQuery(dbcon,statement=x)) else NULL
      if (!is.null(res) && class(res) != "try-error") dbClearResult(res)
    },
  dbcon,subExpression) 
}   

# subExpression = "printf('%3.0f',round(dbh*.5,0)*2)"
mkdbhCase = function (smdbh=4,lgdbh=40)
{
  classes = seq(smdbh,lgdbh,smdbh)
  nc = nchar(as.character(classes[length(classes)])) + 1
  nc = paste0("%",nc,".",nc,"i")
  chrclasses = sprintf(nc,as.integer(classes))     
  subExpression = paste0("case when (dbh <= ",classes[1],") then '", 
                       chrclasses[1],"'")
  for (i in 1:(length(classes)-1))
  {
    subExpression = paste0(subExpression," when (dbh > ",classes[i]," and dbh <= ",
     classes[i+1],") then '", chrclasses[i+1],"'")
  }
  subExpression = paste0(subExpression," else '",sub("0",">",chrclasses[i+1]),
                       "' end ")
  subExpression
}

Create_CmpMetaData = "
drop table if exists CmpMetaData; 
create table CmpMetaData as 
 select RunTitle,RunDateTime,Variant,
   sum(SamplingWt) as TotalSamplingWt,
   count(*)        as NumOfCases,
   Version, RV, KeywordFile from FVS_Cases   
   where CaseID in (select CaseID from temp.Cases)
 group by KeywordFile
 order by RunTitle,RunDateTime;"

Create_StdStkDBHSp = "
drop table if exists temp.StdStkDBHSp; 
drop table if exists temp.StdStkAllDBH; 
drop table if exists temp.StdStkAllSp; 
drop table if exists temp.StdStkAllAll; 
create table temp.StdStkDBHSp as 
select CaseID,Year,Species, 
    subExpression as DBHClass, 
    sum(Tpa)          as LiveTpa, 
    sum(DBH*DBH*.005454154*TPA) as LiveBA, 
    sum(TCuFt*Tpa)    as LiveTCuFt, 
    sum(MCuFt*Tpa)    as LiveMCuFt, 
    sum(BdFt*Tpa)     as LiveBdFt, 
    sum(MortPA)       as MrtTPA, 
    sum(DBH*DBH*.005454154*MortPA) as MrtBA, 
    sum(TCuFt*MortPA) as MrtTCuFt, 
    sum(MCuFt*MortPA) as MrtMCuFt,               
    sum(BdFt*MortPA)  as MrtBdFt 
  from FVS_TreeList 
  where CaseID in (select CaseID from temp.Cases)
  group by CaseID,Year,DBHClass,Species
  order by CaseID,Year,DBHClass,Species;
create table temp.StdStkAllDBH as 
select CaseID,Year,Species,'All' as DBHClass,
    sum(LiveTpa)      as LiveTpa, 
    sum(LiveBA)       as LiveBA, 
    sum(LiveTCuFt)    as LiveTCuFt, 
    sum(LiveMCuFt)    as LiveMCuFt, 
    sum(LiveBdFt)     as LiveBdFt, 
    sum(MrtTPA)       as MrtTPA, 
    sum(MrtBA)        as MrtBA, 
    sum(MrtTCuFt)     as MrtTCuFt, 
    sum(MrtMCuFt)     as MrtMCuFt, 
    sum(MrtBdFt)      as MrtBdFt 
  from temp.StdStkDBHSp 
  group by CaseID,Year,Species
  order by CaseID,Year,Species,DBHClass;
create table temp.StdStkAllSp as 
select CaseID,Year,'All' as Species, DBHClass,
    sum(LiveTpa)      as LiveTpa, 
    sum(LiveBA)       as LiveBA, 
    sum(LiveTCuFt)    as LiveTCuFt, 
    sum(LiveMCuFt)    as LiveMCuFt, 
    sum(LiveBdFt)     as LiveBdFt, 
    sum(MrtTPA)       as MrtTPA, 
    sum(MrtBA)        as MrtBA, 
    sum(MrtTCuFt)     as MrtTCuFt, 
    sum(MrtMCuFt)     as MrtMCuFt, 
    sum(MrtBdFt)      as MrtBdFt 
  from temp.StdStkDBHSp 
  group by CaseID,Year,DBHClass
  order by CaseID,Year,Species,DBHClass;
create table temp.StdStkAllAll as 
select CaseID,Year,'All' as Species, 'All' as DBHClass,
    sum(LiveTpa)      as LiveTpa, 
    sum(LiveBA)       as LiveBA,           
    sum(LiveTCuFt)    as LiveTCuFt, 
    sum(LiveMCuFt)    as LiveMCuFt, 
    sum(LiveBdFt)     as LiveBdFt, 
    sum(MrtTPA)       as MrtTPA, 
    sum(MrtBA)        as MrtBA, 
    sum(MrtTCuFt)     as MrtTCuFt, 
    sum(MrtMCuFt)     as MrtMCuFt, 
    sum(MrtBdFt)      as MrtBdFt 
  from temp.StdStkDBHSp 
  group by CaseID,Year
  order by CaseID,Year,Species,DBHClass;
insert into temp.StdStkDBHSp select * from temp.StdStkAllSp;
insert into temp.StdStkDBHSp select * from temp.StdStkAllDBH;
insert into temp.StdStkDBHSp select * from temp.StdStkAllAll;" 
 
  
Create_HrvStdStk = "
drop table if exists temp.HrvStdStk;
drop table if exists temp.HrvStdStkAllDBH;
drop table if exists temp.HrvStdStkAllSp;
drop table if exists temp.HrvStdStkAllAll;
create table temp.HrvStdStk as
select CaseID,Year,Species, 
    subExpression as dbhclass, 
    sum(Tpa)          as HrvTPA,
    sum(DBH*DBH*.005454154*Tpa) as HrvBA, 
    sum(TCuFt*Tpa)    as HrvTCuFt,
    sum(MCuFt*Tpa)    as HrvMCuFt,
    sum(BdFt*Tpa)     as HrvBdFt
  from FVS_CutList 
  where CaseID in (select CaseID from temp.Cases)
  group by CaseID,Year,Species,DBHClass;
create table temp.HrvStdStkAllDBH as 
select CaseID,Year,Species,'All' as DBHClass,
    sum(HrvTPA)      as HrvTPA,
    sum(HrvBA)       as HrvBA,
    sum(HrvTCuFt)    as HrvTCuFt,
    sum(HrvMCuFt)    as HrvMCuFt,
    sum(HrvBdFt)     as HrvBdFt
  from temp.HrvStdStk 
  group by CaseID,Year,Species;
create table temp.HrvStdStkAllSp as 
select CaseID,Year,'All' as Species, DBHClass,
    sum(HrvTPA)      as HrvTPA,
    sum(HrvBA)       as HrvBA,
    sum(HrvTCuFt)    as HrvTCuFt,
    sum(HrvMCuFt)    as HrvMCuFt,
    sum(HrvBdFt)     as HrvBdFt
  from temp.HrvStdStk 
  group by CaseID,Year,DBHClass;
create table temp.HrvStdStkAllAll as 
select CaseID,Year,'All' as Species, 'All' as DBHClass,
    sum(HrvTPA)      as HrvTPA,
    sum(HrvBA)       as HrvBA,
    sum(HrvTCuFt)    as HrvTCuFt,
    sum(HrvMCuFt)    as HrvMCuFt,
    sum(HrvBdFt)     as HrvBdFt
  from temp.HrvStdStk 
  group by CaseID,Year;
insert into temp.HrvStdStk select * from temp.HrvStdStkAllSp;
insert into temp.HrvStdStk select * from temp.HrvStdStkAllDBH;
insert into temp.HrvStdStk select * from temp.HrvStdStkAllAll;" 
  
Create_StdStk1Hrv = "
drop table if exists temp.StdStk2;
create table temp.StdStk2 as select * from temp.StdStkDBHSp 
 left join temp.HrvStdStk using (CaseID,Year,Species,DBHClass);
drop table if exists temp.StdStk1;
create table temp.StdStk1 as 
select Year,Species,DBHClass,
 LiveTpa, LiveBA, LiveTCuFt, LiveMCuFt, LiveBdFt,  
 case when HrvTPA   is not null then HrvTPA   else 0 end as HrvTPA, 
 case when HrvBA    is not null then HrvBA    else 0 end as HrvBA, 
 case when HrvTCuFt is not null then HrvTCuFt else 0 end as HrvTCuFt, 
 case when HrvMCuFt is not null then HrvMCuFt else 0 end as HrvMCuFt, 
 case when HrvBdFt  is not null then HrvBdFt  else 0 end as HrvBdFt,
 MrtTPA, MrtBA, MrtTCuFt, MrtMCuFt, MrtBdFt, CaseID
from temp.StdStk2;"                                                                         
  
Create_StdStk1NoHrv = "
drop table if exists temp.StdStk1; 
create table temp.StdStk1 as 
select Year,Species,DBHClass,
 LiveTpa, LiveBA, LiveTCuFt, LiveMCuFt, LiveBdFt,                                   
 0 as HrvTPA, 0 as HrvBA, 0 as HrvTCuFt, 0 as HrvMCuFt, 0 as HrvBdFt,
 MrtTPA, MrtBA, MrtTCuFt, MrtMCuFt, MrtBdFt, CaseID
from temp.StdStkDBHSp;"                                 
                                                                                    
Create_StdStkFinal = "
drop table if exists StdStk;
create table StdStk as select Year, Species, DBHClass, 
 LiveTpa,    MrtTPA,   HrvTPA,   LiveTpa   - HrvTPA   as RsdTPA,
 LiveBA,     MrtBA,    HrvBA,    LiveBA    - HrvBA    as RsdBA,
 LiveTCuFt,  MrtTCuFt, HrvTCuFt, LiveTCuFt - HrvTCuFt as RsdTCuFt,
 LiveMCuFt,  MrtMCuFt, HrvMCuFt, LiveMCuFt - HrvMCuFt as RsdMCuFt,
 LiveBdFt,   MrtBdFt,  HrvBdFt,  LiveBdFt  - HrvBdFt  as RsdBdFt, 
 CaseID from temp.StdStk1;                                     
drop table if exists CmpStdStk;                               
create table CmpStdStk as 
select MgmtID,Year,Species,DBHClass,
    round(sum(LiveTPA  *SamplingWt)/sum(SamplingWt),2) as CmpLiveTPA,
    round(sum(MrtTPA   *SamplingWt)/sum(SamplingWt),2) as CmpMrtTPA,
    round(sum(HrvTPA   *SamplingWt)/sum(SamplingWt),2) as CmpHrvTPA,
    round(sum(RsdTPA   *SamplingWt)/sum(SamplingWt),2) as CmpRsdTPA,
    round(sum(LiveBA   *SamplingWt)/sum(SamplingWt),2) as CmpLiveBA,
    round(sum(MrtBA    *SamplingWt)/sum(SamplingWt),2) as CmpMrtBA,
    round(sum(HrvBA    *SamplingWt)/sum(SamplingWt),2) as CmpHrvBA,
    round(sum(RsdBA    *SamplingWt)/sum(SamplingWt),2) as CmpRsdBA,
    round(sum(LiveTCuFt*SamplingWt)/sum(SamplingWt),2) as CmpLiveTCuFt,
    round(sum(MrtTCuFt *SamplingWt)/sum(SamplingWt),2) as CmpMrtTCuFt,
    round(sum(HrvTCuFt *SamplingWt)/sum(SamplingWt),2) as CmpHrvTCuFt,
    round(sum(RsdTCuFt *SamplingWt)/sum(SamplingWt),2) as CmpRsdTCuFt,
    round(sum(LiveMCuFt*SamplingWt)/sum(SamplingWt),2) as CmpLiveMCuFt,
    round(sum(MrtMCuFt *SamplingWt)/sum(SamplingWt),2) as CmpMrtMCuFt,
    round(sum(HrvMCuFt *SamplingWt)/sum(SamplingWt),2) as CmpHrvMCuFt,
    round(sum(RsdMCuFt *SamplingWt)/sum(SamplingWt),2) as CmpRsdMCuFt,
    round(sum(LiveBdFt *SamplingWt)/sum(SamplingWt),2) as CmpLiveBdFt,
    round(sum(MrtBdFt  *SamplingWt)/sum(SamplingWt),2) as CmpMrtBdFt,
    round(sum(HrvBdFt  *SamplingWt)/sum(SamplingWt),2) as CmpHrvBdFt,
    round(sum(RsdBdFt  *SamplingWt)/sum(SamplingWt),2) as CmpRsdBdFt  
  from (select * from StdStk where CaseID in (select CaseID from temp.Cases))
  join FVS_Cases using (CaseID)
  group by MgmtID,Year,Species,DBHClass;"

Create_CmpSummary = "
drop table if exists CmpSummary;
create table CmpSummary as 
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
  from (select * from FVS_Summary where CaseID in (select CaseID from temp.Cases))
  join FVS_Cases using (CaseID)
  group by MgmtID,Year;"

Create_CmpSummary_East = "
drop table if exists CmpSummary_East;
create table CmpSummary_East as 
select MgmtID,Year,
    round(sum(Age    *SamplingWt)/sum(SamplingWt),2) as CmpAge,
    round(sum(Tpa    *SamplingWT)/sum(SamplingWt),2) as CmpTpa,    
    round(sum(BA     *SamplingWT)/sum(SamplingWt),2) as CmpBA,
    round(sum(SDI    *SamplingWT)/sum(SamplingWt),2) as CmpSDI,    
    round(sum(CCF    *SamplingWT)/sum(SamplingWt),2) as CmpCCF,
    round(sum(TopHt  *SamplingWT)/sum(SamplingWt),2) as CmpTopHt,
    round(sum(QMD    *SamplingWT)/sum(SamplingWt),2) as CmpQMD,
    round(sum(MCuFt  *SamplingWT)/sum(SamplingWt),2) as CmpMCuFt,
    round(sum(SCuFt  *SamplingWT)/sum(SamplingWt),2) as CmpSCuFt,
    round(sum(SBdFt  *SamplingWT)/sum(SamplingWt),2) as CmpSBdFt,
    round(sum(RTpa   *SamplingWT)/sum(SamplingWt),2) as CmpRTpa,  
    round(sum(RMCuFt *SamplingWT)/sum(SamplingWt),2) as CmpRMCuFt,      
    round(sum(RSCuFt *SamplingWT)/sum(SamplingWt),2) as CmpRSCuFt,
    round(sum(RSBdFt *SamplingWT)/sum(SamplingWt),2) as CmpRSBdFt,
    round(sum(ATBA   *SamplingWT)/sum(SamplingWt),2) as CmpATBA,
    round(sum(ATSDI  *SamplingWT)/sum(SamplingWt),2) as CmpATSDI,
    round(sum(ATCCF  *SamplingWT)/sum(SamplingWt),2) as CmpATCCF,
    round(sum(ATTopHt*SamplingWT)/sum(SamplingWt),2) as CmpATTopHt,
    round(sum(ATQMD  *SamplingWT)/sum(SamplingWt),2) as CmpATQMD,
    round(sum(SamplingWt                        ),2) as CmpSamplingWt
  from (select * from FVS_Summary_East where CaseID in (select CaseID from temp.Cases))
  join FVS_Cases using (CaseID)
  group by MgmtID,Year;"

  
Create_CmpCompute = "
drop table if exists CmpCompute;
create table CmpCompute as
  select MgmtID,Year,subExpression,
  round(sum(SamplingWt),2) as CmpSamplingWt
  from (select * from FVS_Compute where CaseID in (select CaseID from temp.Cases))
  join FVS_Cases using (CaseID)
  group by MgmtID,Year;" 
