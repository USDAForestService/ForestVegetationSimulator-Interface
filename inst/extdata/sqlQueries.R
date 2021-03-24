# $Id: sqlQueries.R 3017 2020-03-24 12:26:57Z nickcrookston $

exqury = function (dbcon,x,subExpression=NULL,asSpecies=NULL) 
{
  # return value: TRUE=worked, FALSE=error
  if (!is.null(subExpression)) x = gsub("subExpression",subExpression,x)
  if (!is.null(asSpecies))     x = gsub("asSpecies",paste0(asSpecies," as Species"),x)
  for (qry in scan(text=gsub("\n"," ",x),sep=";",what="",quote="",quiet=TRUE))
  {
#cat ("exqury qry1=",qry,"\n")
    res = if (nchar(qry) > 5) try(dbExecute(dbcon,qry)) else NULL
    if (!is.null(res) && class(res) == "try-error") 
    {
#cat ("exqury qry2=",qry,"\n")
      qry = gsub(paste0(asSpecies," as Species")," Species ",qry)
      res = try(dbExecute(dbcon,qry))
      if (class(res) == "try-error") return(FALSE)
    }
  }
  return(TRUE)
}   

mkdbhCase = function (stpdbh=4,lgdbh=40)
{
  stpdbh=if (is.na(stpdbh) || as.numeric(stpdbh)==0 || 
             as.character(stpdbh)=="") 4 else ceiling(stpdbh)
  if (is.na(lgdbh ) || as.numeric(lgdbh )==0 || as.character(lgdbh )=="") lgdbh =40
  if (stpdbh<1)       stpdbh=1
  if (lgdbh<stpdbh*4) lgdbh=stpdbh*4
  lb = seq(stpdbh-(stpdbh/2),lgdbh+stpdbh,stpdbh)
  lb[1] = 0
  classes = seq(stpdbh,lgdbh,stpdbh)
  nc = nchar(as.character(classes[length(classes)]))
  chrclasses = sprintf(paste0("%",nc,".",nc,"d"),classes)
  subExpression = paste0("case when (dbh <",lb[2],") then '", 
                       chrclasses[1],"'")  
  for (i in 2:(length(classes)))
  {
    subExpression = paste0(subExpression," when (dbh >= ",lb[i]," and dbh < ",
     lb[i+1],") then '", chrclasses[i],"'")
  }
  subExpression = paste0(subExpression," else '",lb[length(lb)],"+' end ")
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
 order by RunTitle, RunDateTime;"

Create_StdStkDBHSp = "
drop table if exists temp.StdStkDBHSp; 
drop table if exists temp.StdStkAllDBH; 
drop table if exists temp.StdStkAllSp; 
drop table if exists temp.StdStkAllAll; 
create table temp.StdStkDBHSp as 
  select CaseID,Year,asSpecies, 
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
  select CaseID,Year,asSpecies, 
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
 CaseID from temp.StdStk1;"                                    
                                                                                    
Create_CmpStdStk = "
drop table if exists CmpSmpWt;                               
drop table if exists CmpStdStk;                               
drop table if exists temp.CmpStdStkDBHSp;                  
drop table if exists temp.CmpStdStkAllDBH; 
drop table if exists temp.CmpStdStkAllSp; 
drop table if exists temp.CmpStdStkAllAll;
create table temp.CmpSmpWt as
  select MgmtID,sum(SamplingWt) as CmpSmpWt from FVS_Cases where
  CaseID in (select CaseID from temp.Cases)
  group by MgmtID;    
create table temp.CmpStdStkDBHSp as 
  select MgmtID,Year,Species,DBHClass,
    sum(LiveTPA  *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpLiveTPA,
    sum(MrtTPA   *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpMrtTPA,
    sum(HrvTPA   *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpHrvTPA,
    sum(RsdTPA   *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpRsdTPA,
    sum(LiveBA   *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpLiveBA,
    sum(MrtBA    *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpMrtBA,
    sum(HrvBA    *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpHrvBA,
    sum(RsdBA    *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpRsdBA,
    sum(LiveTCuFt*SamplingWt)/CmpSmpWt.CmpSmpWt as CmpLiveTCuFt,
    sum(MrtTCuFt *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpMrtTCuFt,
    sum(HrvTCuFt *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpHrvTCuFt,
    sum(RsdTCuFt *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpRsdTCuFt,
    sum(LiveMCuFt*SamplingWt)/CmpSmpWt.CmpSmpWt as CmpLiveMCuFt,
    sum(MrtMCuFt *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpMrtMCuFt,
    sum(HrvMCuFt *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpHrvMCuFt,
    sum(RsdMCuFt *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpRsdMCuFt,
    sum(LiveBdFt *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpLiveBdFt,
    sum(MrtBdFt  *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpMrtBdFt,
    sum(HrvBdFt  *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpHrvBdFt,
    sum(RsdBdFt  *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpRsdBdFt  
  from (select * from StdStk where Species != 'All' and DBHClass != 'All' and
        CaseID in (select CaseID from temp.Cases))
  join FVS_Cases using (CaseID)
  join temp.CmpSmpWt using (MgmtID)
  group by MgmtID,Year,Species,DBHClass;
create table temp.CmpStdStkAllDBH as
  select MgmtID,Year,Species,'All' as DBHClass,
    round(sum(CmpLiveTPA),2)   as CmpLiveTPA,
    round(sum(CmpMrtTPA),2)    as CmpMrtTPA,
    round(sum(CmpHrvTPA),2)    as CmpHrvTPA,
    round(sum(CmpRsdTPA),2)    as CmpRsdTPA,
    round(sum(CmpLiveBA),2)    as CmpLiveBA,
    round(sum(CmpMrtBA),2)     as CmpMrtBA,
    round(sum(CmpHrvBA),2)     as CmpHrvBA,
    round(sum(CmpRsdBA),2)     as CmpRsdBA,
    round(sum(CmpLiveTCuFt),2) as CmpLiveTCuFt,
    round(sum(CmpMrtTCuFt),2)  as CmpMrtTCuFt,
    round(sum(CmpHrvTCuFt),2)  as CmpHrvTCuFt,
    round(sum(CmpRsdTCuFt),2)  as CmpRsdTCuFt,
    round(sum(CmpLiveMCuFt),2) as CmpLiveMCuFt,
    round(sum(CmpMrtMCuFt),2)  as CmpMrtMCuFt,
    round(sum(CmpHrvMCuFt),2)  as CmpHrvMCuFt,
    round(sum(CmpRsdMCuFt),2)  as CmpRsdMCuFt,
    round(sum(CmpLiveBdFt),2)  as CmpLiveBdFt,
    round(sum(CmpMrtBdFt),2)   as CmpMrtBdFt,
    round(sum(CmpHrvBdFt),2)   as CmpHrvBdFt,
    round(sum(CmpRsdBdFt),2)   as CmpRsdBdFt 
  from temp.CmpStdStkDBHSp
  group by MgmtID,Year,Species;          
create table temp.CmpStdStkAllSp as
  select MgmtID,Year,'All' as Species,DBHClass,
    round(sum(CmpLiveTPA),2)   as CmpLiveTPA,
    round(sum(CmpMrtTPA),2)    as CmpMrtTPA,
    round(sum(CmpHrvTPA),2)    as CmpHrvTPA,
    round(sum(CmpRsdTPA),2)    as CmpRsdTPA,
    round(sum(CmpLiveBA),2)    as CmpLiveBA,
    round(sum(CmpMrtBA),2)     as CmpMrtBA,
    round(sum(CmpHrvBA),2)     as CmpHrvBA,
    round(sum(CmpRsdBA),2)     as CmpRsdBA,
    round(sum(CmpLiveTCuFt),2) as CmpLiveTCuFt,
    round(sum(CmpMrtTCuFt),2)  as CmpMrtTCuFt,
    round(sum(CmpHrvTCuFt),2)  as CmpHrvTCuFt,
    round(sum(CmpRsdTCuFt),2)  as CmpRsdTCuFt,
    round(sum(CmpLiveMCuFt),2) as CmpLiveMCuFt,
    round(sum(CmpMrtMCuFt),2)  as CmpMrtMCuFt,
    round(sum(CmpHrvMCuFt),2)  as CmpHrvMCuFt,
    round(sum(CmpRsdMCuFt),2)  as CmpRsdMCuFt,
    round(sum(CmpLiveBdFt),2)  as CmpLiveBdFt,
    round(sum(CmpMrtBdFt),2)   as CmpMrtBdFt,
    round(sum(CmpHrvBdFt),2)   as CmpHrvBdFt,
    round(sum(CmpRsdBdFt),2)   as CmpRsdBdFt 
  from temp.CmpStdStkDBHSp
  group by MgmtID,Year,DBHClass;
create table temp.CmpStdStkAllAll as  
  select MgmtID,Year,'All' as Species,'All' as DBHClass,
    round(sum(CmpLiveTPA),2)   as CmpLiveTPA,
    round(sum(CmpMrtTPA),2)    as CmpMrtTPA,
    round(sum(CmpHrvTPA),2)    as CmpHrvTPA,
    round(sum(CmpRsdTPA),2)    as CmpRsdTPA,
    round(sum(CmpLiveBA),2)    as CmpLiveBA,
    round(sum(CmpMrtBA),2)     as CmpMrtBA,
    round(sum(CmpHrvBA),2)     as CmpHrvBA,
    round(sum(CmpRsdBA),2)     as CmpRsdBA,
    round(sum(CmpLiveTCuFt),2) as CmpLiveTCuFt,
    round(sum(CmpMrtTCuFt),2)  as CmpMrtTCuFt,
    round(sum(CmpHrvTCuFt),2)  as CmpHrvTCuFt,
    round(sum(CmpRsdTCuFt),2)  as CmpRsdTCuFt,
    round(sum(CmpLiveMCuFt),2) as CmpLiveMCuFt,
    round(sum(CmpMrtMCuFt),2)  as CmpMrtMCuFt,
    round(sum(CmpHrvMCuFt),2)  as CmpHrvMCuFt,
    round(sum(CmpRsdMCuFt),2)  as CmpRsdMCuFt,
    round(sum(CmpLiveBdFt),2)  as CmpLiveBdFt,
    round(sum(CmpMrtBdFt),2)   as CmpMrtBdFt,
    round(sum(CmpHrvBdFt),2)   as CmpHrvBdFt,
    round(sum(CmpRsdBdFt),2)   as CmpRsdBdFt 
  from temp.CmpStdStkDBHSp
  group by MgmtID,Year; 
create table CmpStdStk as 
  select MgmtID,Year,Species,DBHClass,
    round(CmpLiveTPA  ,2) as CmpLiveTPA,
    round(CmpMrtTPA   ,2) as CmpMrtTPA,
    round(CmpHrvTPA   ,2) as CmpHrvTPA,
    round(CmpRsdTPA   ,2) as CmpRsdTPA,
    round(CmpLiveBA   ,2) as CmpLiveBA,
    round(CmpMrtBA    ,2) as CmpMrtBA,
    round(CmpHrvBA    ,2) as CmpHrvBA,
    round(CmpRsdBA    ,2) as CmpRsdBA,
    round(CmpLiveTCuFt,2) as CmpLiveTCuFt,
    round(CmpMrtTCuFt ,2) as CmpMrtTCuFt,
    round(CmpHrvTCuFt ,2) as CmpHrvTCuFt,
    round(CmpRsdTCuFt ,2) as CmpRsdTCuFt,
    round(CmpLiveMCuFt,2) as CmpLiveMCuFt,
    round(CmpMrtMCuFt ,2) as CmpMrtMCuFt,
    round(CmpHrvMCuFt ,2) as CmpHrvMCuFt,
    round(CmpRsdMCuFt ,2) as CmpRsdMCuFt,
    round(CmpLiveBdFt ,2) as CmpLiveBdFt,
    round(CmpMrtBdFt  ,2) as CmpMrtBdFt,
    round(CmpHrvBdFt  ,2) as CmpHrvBdFt,
    round(CmpRsdBdFt  ,2) as CmpRsdBdFt  
  from temp.CmpStdStkDBHSp;
insert into CmpStdStk select * from temp.CmpStdStkAllDBH; 
insert into CmpStdStk select * from temp.CmpStdStkAllSp; 
insert into CmpStdStk select * from temp.CmpStdStkAllAll;"   
  
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

Create_CmpSummary2 = "
drop table if exists temp.CmpSummary2A;
create table temp.CmpSummary2A as 
 select MgmtID,Year,RmvCode,
    round(sum(Age      *SamplingWt)/sum(SamplingWt),2) as CmpAge,
    round(sum(Tpa      *SamplingWT)/sum(SamplingWt),2) as CmpTpa,    
    round(sum(TprdTpa  *SamplingWT)/sum(SamplingWt),2) as CmpTprdTpa,    
    round(sum(BA       *SamplingWT)/sum(SamplingWt),2) as CmpBA,
    round(sum(SDI      *SamplingWT)/sum(SamplingWt),2) as CmpSDI,    
    round(sum(CCF      *SamplingWT)/sum(SamplingWt),2) as CmpCCF,
    round(sum(TopHt    *SamplingWT)/sum(SamplingWt),2) as CmpTopHt,
    round(sum(QMD      *SamplingWT)/sum(SamplingWt),2) as CmpQMD,
    round(sum(TCuFt    *SamplingWT)/sum(SamplingWt),2) as CmpTCuFt,
    round(sum(TprdTCuFt*SamplingWT)/sum(SamplingWt),2) as CmpTprdTCuFt,
    round(sum(MCuFt    *SamplingWT)/sum(SamplingWt),2) as CmpMCuFt,
    round(sum(TprdMCuFt*SamplingWT)/sum(SamplingWt),2) as CmpTprdMCuFt,
    round(sum(BdFt     *SamplingWT)/sum(SamplingWt),2) as CmpBdFt,
    round(sum(TprdBdFt *SamplingWT)/sum(SamplingWt),2) as CmpTPrdBdFt,
    round(sum(RTpa     *SamplingWT)/sum(SamplingWt),2) as CmpRTpa,  
    round(sum(RTCuFt   *SamplingWT)/sum(SamplingWt),2) as CmpRTCuFt,      
    round(sum(RMCuFt   *SamplingWT)/sum(SamplingWt),2) as CmpRMCuFt,
    round(sum(RBdFt    *SamplingWT)/sum(SamplingWt),2) as CmpRBdFt,
    round(sum(SamplingWt                          ),2) as CmpSamplingWt
  from (select * from FVS_Summary2 where CaseID in (select CaseID from temp.Cases))
  join FVS_Cases using (CaseID)   
  group by MgmtID,Year,RmvCode order by MgmtID,Year,RmvCode;
 
drop table if exists temp.CmpSummary2B;
create table temp.CmpSummary2B as 
 select MgmtID,Year,RmvCode,max(RmvCode) as newRmv,
    round(sum(CmpAge      *CmpSamplingWt)/sum(CmpSamplingWt),2) as CmpAge,
    round(sum(CmpTpa      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTpa,    
    round(sum(CmpTprdTpa  *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdTpa,    
    round(sum(CmpBA       *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpBA,
    round(sum(CmpSDI      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpSDI,    
    round(sum(CmpCCF      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpCCF,
    round(sum(CmpTopHt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTopHt,
    round(sum(CmpQMD      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpQMD,
    round(sum(CmpTCuFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTCuFt,
    round(sum(CmpTprdTCuFt*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdTCuFt,
    round(sum(CmpMCuFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpMCuFt,
    round(sum(CmpTprdMCuFt*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdMCuFt,
    round(sum(CmpBdFt     *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpBdFt,
    round(sum(CmpTprdBdFt *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTPrdBdFt,
    round(sum(CmpRTpa     *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRTpa,  
    round(sum(CmpRTCuFt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRTCuFt,      
    round(sum(CmpRMCuFt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRMCuFt,
    round(sum(CmpRBdFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRBdFt,
    round(sum(CmpSamplingWt                          ),2) as CmpSamplingWt
  from temp.CmpSummary2A where RmvCode in (0,1)
  group by MgmtID,Year order by MgmtID,Year;

insert into temp.CmpSummary2B 
 select MgmtID,Year,RmvCode,max(RmvCode) as newRmv,
    round(sum(CmpAge      *CmpSamplingWt)/sum(CmpSamplingWt),2) as CmpAge,
    round(sum(CmpTpa      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTpa,    
    round(sum(CmpTprdTpa  *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdTpa,    
    round(sum(CmpBA       *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpBA,
    round(sum(CmpSDI      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpSDI,    
    round(sum(CmpCCF      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpCCF,
    round(sum(CmpTopHt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTopHt,
    round(sum(CmpQMD      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpQMD,
    round(sum(CmpTCuFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTCuFt,
    round(sum(CmpTprdTCuFt*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdTCuFt,
    round(sum(CmpMCuFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpMCuFt,
    round(sum(CmpTprdMCuFt*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdMCuFt,
    round(sum(CmpBdFt     *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpBdFt,
    round(sum(CmpTprdBdFt *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTPrdBdFt,
    round(sum(CmpRTpa     *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRTpa,  
    round(sum(CmpRTCuFt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRTCuFt,      
    round(sum(CmpRMCuFt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRMCuFt,
    round(sum(CmpRBdFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRBdFt,
    round(sum(CmpSamplingWt                          ),2) as CmpSamplingWt
  from temp.CmpSummary2A where RmvCode in (0,2)
  group by MgmtID,Year order by MgmtID,Year;
  
drop table if exists CmpSummary2;
create table CmpSummary2 as 
  select distinct MgmtID,Year,newRmv as RmvCode,CmpAge,CmpTpa,CmpTprdTpa,  
    CmpBA,CmpSDI,CmpCCF,CmpTopHt,CmpQMD,CmpTCuFt,
    CmpTprdTCuFt,CmpMCuFt,CmpTprdMCuFt,CmpBdFt,CmpTPrdBdFt,
    CmpRTpa,CmpRTCuFt,CmpRMCuFt,CmpRBdFt,CmpSamplingWt
 from temp.CmpSummary2B
 order by MgmtID,Year,RmvCode;
drop table if exists temp.CmpSummary2A;
drop table if exists temp.CmpSummary2B;"

Create_CmpSummary2_East = "
drop table if exists temp.CmpSummary2_EastA;
create table temp.CmpSummary2_EastA as 
 select MgmtID,Year,RmvCode,
    round(sum(Age      *SamplingWt)/sum(SamplingWt),2) as CmpAge,
    round(sum(Tpa      *SamplingWT)/sum(SamplingWt),2) as CmpTpa,    
    round(sum(TprdTpa  *SamplingWT)/sum(SamplingWt),2) as CmpTprdTpa,    
    round(sum(BA       *SamplingWT)/sum(SamplingWt),2) as CmpBA,
    round(sum(SDI      *SamplingWT)/sum(SamplingWt),2) as CmpSDI,    
    round(sum(CCF      *SamplingWT)/sum(SamplingWt),2) as CmpCCF,
    round(sum(TopHt    *SamplingWT)/sum(SamplingWt),2) as CmpTopHt,
    round(sum(QMD      *SamplingWT)/sum(SamplingWt),2) as CmpQMD,
    round(sum(MCuFt    *SamplingWT)/sum(SamplingWt),2) as CmpMCuFt,
    round(sum(TprdMCuFt*SamplingWT)/sum(SamplingWt),2) as CmpTprdMCuFt,
    round(sum(SCuFt    *SamplingWT)/sum(SamplingWt),2) as CmpSCuFt,
    round(sum(TprdSCuFt*SamplingWT)/sum(SamplingWt),2) as CmpTprdSCuFt,
    round(sum(SBdFt    *SamplingWT)/sum(SamplingWt),2) as CmpSBdFt,
    round(sum(TprdSBdFt*SamplingWT)/sum(SamplingWt),2) as CmpTPrdSBdFt,
    round(sum(RTpa     *SamplingWT)/sum(SamplingWt),2) as CmpRTpa,  
    round(sum(RMCuFt   *SamplingWT)/sum(SamplingWt),2) as CmpRMCuFt,      
    round(sum(RSCuFt   *SamplingWT)/sum(SamplingWt),2) as CmpRSCuFt,
    round(sum(RSBdFt   *SamplingWT)/sum(SamplingWt),2) as CmpRSBdFt,
    round(sum(SamplingWt                          ),2) as CmpSamplingWt
  from (select * from FVS_Summary2_East where CaseID in (select CaseID from temp.Cases))
  join FVS_Cases using (CaseID) 
  group by MgmtID,Year,RmvCode order by MgmtID,Year,RmvCode;
 
drop table if exists temp.CmpSummary2_EastB;
create table temp.CmpSummary2_EastB as 
 select MgmtID,Year,RmvCode,
    max(RmvCode) as newRmv,
    round(sum(CmpAge      *CmpSamplingWt)/sum(CmpSamplingWt),2) as CmpAge,
    round(sum(CmpTpa      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTpa,    
    round(sum(CmpTprdTpa  *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdTpa,    
    round(sum(CmpBA       *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpBA,
    round(sum(CmpSDI      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpSDI,    
    round(sum(CmpCCF      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpCCF,
    round(sum(CmpTopHt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTopHt,
    round(sum(CmpQMD      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpQMD,
    round(sum(CmpMCuFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpMCuFt,
    round(sum(CmpTprdMCuFt*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdMCuFt,
    round(sum(CmpSCuFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpSCuFt,
    round(sum(CmpTprdSCuFt*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdSCuFt,
    round(sum(CmpSBdFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpSBdFt,
    round(sum(CmpTprdSBdFt*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTPrdSBdFt,
    round(sum(CmpRTpa     *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRTpa,  
    round(sum(CmpRMCuFt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRMCuFt,      
    round(sum(CmpRSCuFt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRSCuFt,
    round(sum(CmpRSBdFt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRSBdFt,
    round(sum(CmpSamplingWt                          ),2) as CmpSamplingWt
  from temp.CmpSummary2_EastA where RmvCode in (0,1)
  group by MgmtID,Year order by MgmtID,Year;

insert into temp.CmpSummary2_EastB 
 select MgmtID,Year,RmvCode,max(RmvCode) as newRmv,
    round(sum(CmpAge      *CmpSamplingWt)/sum(CmpSamplingWt),2) as CmpAge,
    round(sum(CmpTpa      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTpa,    
    round(sum(CmpTprdTpa  *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdTpa,    
    round(sum(CmpBA       *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpBA,
    round(sum(CmpSDI      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpSDI,    
    round(sum(CmpCCF      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpCCF,
    round(sum(CmpTopHt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTopHt,
    round(sum(CmpQMD      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpQMD,
    round(sum(CmpMCuFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpMCuFt,
    round(sum(CmpTprdMCuFt*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdMCuFt,
    round(sum(CmpSCuFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpSCuFt,
    round(sum(CmpTprdSCuFt*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdSCuFt,
    round(sum(CmpSBdFt    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpSBdFt,
    round(sum(CmpTprdSBdFt*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTPrdSBdFt,
    round(sum(CmpRTpa     *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRTpa,  
    round(sum(CmpRMCuFt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRMCuFt,      
    round(sum(CmpRSCuFt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRSCuFt,
    round(sum(CmpRSBdFt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRSBdFt,
    round(sum(CmpSamplingWt                          ),2) as CmpSamplingWt
  from temp.CmpSummary2_EastA where RmvCode in (0,2)
  group by MgmtID,Year order by MgmtID,Year;
  
drop table if exists CmpSummary2_East;
create table CmpSummary2_East as 
  select distinct MgmtID,Year,newRmv as RmvCode,CmpAge,CmpTpa,CmpTprdTpa,  
    CmpBA,CmpSDI,CmpCCF,CmpTopHt,CmpQMD,CmpMCuFt,
    CmpTprdMCuFt,CmpSCuFt,CmpTprdSCuFt,CmpSBdFt,CmpTPrdSBdFt,
    CmpRTpa,CmpRMCuFt,CmpRSCuFt,CmpRSBdFt,CmpSamplingWt
 from temp.CmpSummary2_EastB
  order by MgmtID,Year,RmvCode;
drop table if exists temp.CmpSummary2_EastA;
drop table if exists temp.CmpSummary2_EastB;"


Create_CmpCompute = "
drop table if exists CmpCompute;
create table CmpCompute as
  select MgmtID,Year,subExpression,
  round(sum(SamplingWt),2) as CmpSamplingWt
  from (select * from FVS_Compute where CaseID in (select CaseID from temp.Cases))
  join FVS_Cases using (CaseID)
  group by MgmtID,Year;" 
  
Create_View_DWN_Required = c("FVS_Down_Wood_Cov","FVS_Down_Wood_Vol")
Create_View_DWN = "
drop view if exists View_DWN;
create temp view View_DWN as
select FVS_Down_Wood_Cov.CaseID,FVS_Down_Wood_Cov.StandID,
  FVS_Down_Wood_Cov.Year,
  DWD_Cover_Total_Hard, DWD_Cover_Total_Soft, 
  DWD_Cover_Total_Hard+DWD_Cover_Total_Soft as DWD_Total_Cover,
  DWD_Volume_Total_Hard, DWD_Volume_Total_Soft, 
  DWD_Volume_Total_Hard+DWD_Volume_Total_Soft as DWD_Total_Volume
from FVS_Down_Wood_Cov inner join FVS_Down_Wood_Vol 
  on FVS_Down_Wood_Cov.CaseID = FVS_Down_Wood_Vol.CaseID and
     FVS_Down_Wood_Cov.Year = FVS_Down_Wood_Vol.Year;"

