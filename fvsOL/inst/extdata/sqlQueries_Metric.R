# $Id$

exqury = function (dbcon,x,subExpression=NULL,asSpecies=NULL) 
{
  # return value: TRUE=worked, FALSE=error
  if (!is.null(subExpression)) x = gsub("subExpression",subExpression,x)
  if (!is.null(asSpecies))     x = gsub("asSpecies",paste0(asSpecies," as Species"),x)
  for (qry in scan(text=gsub("\n"," ",x),sep=";",what="",quote="",quiet=TRUE))
  {
cat ("exqury qry1=",qry,"\n")
    res = if (nchar(qry) > 5) try(dbExecute(dbcon,qry)) else NULL
    if (!is.null(res) && class(res) == "try-error") 
    {
cat ("exqury qry2=",qry,"\n")
      qry = gsub(paste0(asSpecies," as Species")," Species ",qry)
      res = try(dbExecute(dbcon,qry))
      if (class(res) == "try-error") return(FALSE)
    }
  }
  return(TRUE)
}   

mkdbhCase = function (stpdbh=5,lgdbh=100)
{
  stpdbh=if (is.na(stpdbh) || as.numeric(stpdbh)==0 || 
             as.character(stpdbh)=="") 5 else ceiling(stpdbh)
  if (is.na(lgdbh ) || as.numeric(lgdbh )==0 || as.character(lgdbh )=="") lgdbh =100
  if (stpdbh<1)       stpdbh=1
  if (lgdbh<stpdbh*5) lgdbh=stpdbh*5
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
    sum(TPH)      as LiveTPH, 
    sum(DBH*DBH*7.853982E-05*TPH) as LiveBA, 
    sum(TCuM*TPH) as LiveTCuM, 
    sum(MCuM*TPH) as LiveMCuM, 
    sum(MortPH)   as MrtTPH, 
    sum(DBH*DBH*7.853982E-05*MortPH) as MrtBA, 
    sum(TCuM*MortPH) as MrtTCuM, 
    sum(MCuM*MortPH) as MrtMCuM
  from FVS_TreeList_Metric 
  where CaseID in (select CaseID from temp.Cases)
  group by CaseID,Year,DBHClass,Species
  order by CaseID,Year,DBHClass,Species;
create table temp.StdStkAllDBH as 
  select CaseID,Year,Species,'All' as DBHClass,
    sum(LiveTPH)  as LiveTPH, 
    sum(LiveBA)   as LiveBA, 
    sum(LiveTCuM) as LiveTCuM, 
    sum(LiveMCuM) as LiveMCuM, 
    sum(MrtTPH)   as MrtTPH, 
    sum(MrtBA)    as MrtBA, 
    sum(MrtTCuM)  as MrtTCuM, 
    sum(MrtMCuM)  as MrtMCuM
  from temp.StdStkDBHSp 
  group by CaseID,Year,Species
  order by CaseID,Year,Species,DBHClass;
create table temp.StdStkAllSp as 
  select CaseID,Year,'All' as Species, DBHClass,
    sum(LiveTPH)  as LiveTPH, 
    sum(LiveBA)   as LiveBA, 
    sum(LiveTCuM) as LiveTCuM, 
    sum(LiveMCuM) as LiveMCuM, 
    sum(MrtTPH)   as MrtTPH, 
    sum(MrtBA)    as MrtBA, 
    sum(MrtTCuM)  as MrtTCuM, 
    sum(MrtMCuM)  as MrtMCuM
  from temp.StdStkDBHSp 
  group by CaseID,Year,DBHClass
  order by CaseID,Year,Species,DBHClass;
create table temp.StdStkAllAll as 
  select CaseID,Year,'All' as Species, 'All' as DBHClass,
    sum(LiveTPH)  as LiveTPH, 
    sum(LiveBA)   as LiveBA,           
    sum(LiveTCuM) as LiveTCuM, 
    sum(LiveMCuM) as LiveMCuM,
    sum(MrtTPH)   as MrtTPH, 
    sum(MrtBA)    as MrtBA, 
    sum(MrtTCuM)  as MrtTCuM, 
    sum(MrtMCuM)  as MrtMCuM
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
    sum(TPH)          as HrvTPH,
    sum(DBH*DBH*7.853982E-05*TPH) as HrvBA, 
    sum(TCuM*TPH)    as HrvTCuM,
    sum(MCuM*TPH)    as HrvMCuM
  from FVS_CutList_Metric 
  where CaseID in (select CaseID from temp.Cases)
  group by CaseID,Year,Species,DBHClass;
create table temp.HrvStdStkAllDBH as 
  select CaseID,Year,Species,'All' as DBHClass,
    sum(HrvTPH)   as HrvTPH,
    sum(HrvBA)    as HrvBA,
    sum(HrvTCuM)  as HrvTCuM,
    sum(HrvMCuM)  as HrvMCuM
  from temp.HrvStdStk 
  group by CaseID,Year,Species;
create table temp.HrvStdStkAllSp as 
  select CaseID,Year,'All' as Species, DBHClass,
    sum(HrvTPH)   as HrvTPH,
    sum(HrvBA)    as HrvBA,
    sum(HrvTCuM)  as HrvTCuM,
    sum(HrvMCuM)  as HrvMCuM
  from temp.HrvStdStk 
  group by CaseID,Year,DBHClass;
create table temp.HrvStdStkAllAll as 
  select CaseID,Year,'All' as Species, 'All' as DBHClass,
    sum(HrvTPH)   as HrvTPH,
    sum(HrvBA)    as HrvBA,
    sum(HrvTCuM)  as HrvTCuM,
    sum(HrvMCuM)  as HrvMCuM
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
 LiveTPH, LiveBA, LiveTCuM, LiveMCuM,  
 case when HrvTPH  is not null then HrvTPH  else 0 end as HrvTPH, 
 case when HrvBA   is not null then HrvBA   else 0 end as HrvBA, 
 case when HrvTCuM is not null then HrvTCuM else 0 end as HrvTCuM, 
 case when HrvMCuM is not null then HrvMCuM else 0 end as HrvMCuM, 
 MrtTPH, MrtBA, MrtTCuM, MrtMCuM, CaseID
from temp.StdStk2;"                                                                         
  
Create_StdStk1NoHrv = "
drop table if exists temp.StdStk1; 
create table temp.StdStk1 as 
select Year,Species,DBHClass,
 LiveTPH, LiveBA, LiveTCuM, LiveMCuM,                               
 0 as HrvTPH, 0 as HrvBA, 0 as HrvTCuM, 0 as HrvMCuM, 
 MrtTPH, MrtBA, MrtTCuM, MrtMCuM, CaseID
from temp.StdStkDBHSp;"                                 
                                                                                    
Create_StdStkFinal = "
drop table if exists StdStk_Metric;
create table StdStk_Metric as select Year, Species, DBHClass, 
 LiveTPH,   MrtTPH,  HrvTPH,  LiveTPH  - HrvTPH  as RsdTPH,
 LiveBA,    MrtBA,   HrvBA,   LiveBA   - HrvBA   as RsdBA,
 LiveTCuM,  MrtTCuM, HrvTCuM, LiveTCuM - HrvTCuM as RsdTCuM,
 LiveMCuM,  MrtMCuM, HrvMCuM, LiveMCuM - HrvMCuM as RsdMCuM,
 CaseID from temp.StdStk1;"                                    
                                                                                    
Create_CmpStdStk = "
drop table if exists CmpSmpWt;                               
drop table if exists CmpStdStk_Metric;                               
drop table if exists temp.CmpStdStkDBHSp;                  
drop table if exists temp.CmpStdStkDBHAll; 
drop table if exists temp.CmpStdStkAllSp; 
drop table if exists temp.CmpStdStkAllAll;
create table temp.CmpSmpWt as
  select MgmtID,sum(SamplingWt) as CmpSmpWt from FVS_Cases where
  CaseID in (select CaseID from temp.Cases)
  group by MgmtID;    
create table temp.CmpStdStkDBHSp as 
  select MgmtID,Year,Species,DBHClass,
    sum(LiveTPH *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpLiveTPH,
    sum(MrtTPH  *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpMrtTPH,
    sum(HrvTPH  *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpHrvTPH,
    sum(RsdTPH  *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpRsdTPH,
    sum(LiveBA  *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpLiveBA,
    sum(MrtBA   *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpMrtBA,
    sum(HrvBA   *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpHrvBA,
    sum(RsdBA   *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpRsdBA,
    sum(LiveTCuM*SamplingWt)/CmpSmpWt.CmpSmpWt as CmpLiveTCuM,
    sum(MrtTCuM *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpMrtTCuM,
    sum(HrvTCuM *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpHrvTCuM,
    sum(RsdTCuM *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpRsdTCuM,
    sum(LiveMCuM*SamplingWt)/CmpSmpWt.CmpSmpWt as CmpLiveMCuM,
    sum(MrtMCuM *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpMrtMCuM,
    sum(HrvMCuM *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpHrvMCuM,
    sum(RsdMCuM *SamplingWt)/CmpSmpWt.CmpSmpWt as CmpRsdMCuM 
  from (select * from StdStk_Metric where Species != 'All' and DBHClass != 'All' and
        CaseID in (select CaseID from temp.Cases))
  join FVS_Cases using (CaseID)
  join temp.CmpSmpWt using (MgmtID)
  group by MgmtID,Year,Species,DBHClass;
create table temp.CmpStdStkDBHAll as
  select MgmtID,Year,Species,'All' as DBHClass,
    round(sum(CmpLiveTPH),2)  as CmpLiveTPH,
    round(sum(CmpMrtTPH),2)   as CmpMrtTPH,
    round(sum(CmpHrvTPH),2)   as CmpHrvTPH,
    round(sum(CmpRsdTPH),2)   as CmpRsdTPH,
    round(sum(CmpLiveBA),2)   as CmpLiveBA,
    round(sum(CmpMrtBA),2)    as CmpMrtBA,
    round(sum(CmpHrvBA),2)    as CmpHrvBA,
    round(sum(CmpRsdBA),2)    as CmpRsdBA,
    round(sum(CmpLiveTCuM),2) as CmpLiveTCuM,
    round(sum(CmpMrtTCuM),2)  as CmpMrtTCuM,
    round(sum(CmpHrvTCuM),2)  as CmpHrvTCuM,
    round(sum(CmpRsdTCuM),2)  as CmpRsdTCuM,
    round(sum(CmpLiveMCuM),2) as CmpLiveMCuM,
    round(sum(CmpMrtMCuM),2)  as CmpMrtMCuM,
    round(sum(CmpHrvMCuM),2)  as CmpHrvMCuM,
    round(sum(CmpRsdMCuM),2)  as CmpRsdMCuM
  from temp.CmpStdStkDBHSp
  group by MgmtID,Year,Species;          
create table temp.CmpStdStkAllSp as
  select MgmtID,Year,'All' as Species,DBHClass,
    round(sum(CmpLiveTPH),2)  as CmpLiveTPH,
    round(sum(CmpMrtTPH),2)   as CmpMrtTPH,
    round(sum(CmpHrvTPH),2)   as CmpHrvTPH,
    round(sum(CmpRsdTPH),2)   as CmpRsdTPH,
    round(sum(CmpLiveBA),2)   as CmpLiveBA,
    round(sum(CmpMrtBA),2)    as CmpMrtBA,
    round(sum(CmpHrvBA),2)    as CmpHrvBA,
    round(sum(CmpRsdBA),2)    as CmpRsdBA,
    round(sum(CmpLiveTCuM),2) as CmpLiveTCuM,
    round(sum(CmpMrtTCuM),2)  as CmpMrtTCuM,
    round(sum(CmpHrvTCuM),2)  as CmpHrvTCuM,
    round(sum(CmpRsdTCuM),2)  as CmpRsdTCuM,
    round(sum(CmpLiveMCuM),2) as CmpLiveMCuM,
    round(sum(CmpMrtMCuM),2)  as CmpMrtMCuM,
    round(sum(CmpHrvMCuM),2)  as CmpHrvMCuM,
    round(sum(CmpRsdMCuM),2)  as CmpRsdMCuM
  from temp.CmpStdStkDBHSp
  group by MgmtID,Year,DBHClass;
create table temp.CmpStdStkAllAll as  
  select MgmtID,Year,'All' as Species,'All' as DBHClass,
    round(sum(CmpLiveTPH),2)  as CmpLiveTPH,
    round(sum(CmpMrtTPH),2)   as CmpMrtTPH,
    round(sum(CmpHrvTPH),2)   as CmpHrvTPH,
    round(sum(CmpRsdTPH),2)   as CmpRsdTPH,
    round(sum(CmpLiveBA),2)   as CmpLiveBA,
    round(sum(CmpMrtBA),2)    as CmpMrtBA,
    round(sum(CmpHrvBA),2)    as CmpHrvBA,
    round(sum(CmpRsdBA),2)    as CmpRsdBA,
    round(sum(CmpLiveTCuM),2) as CmpLiveTCuM,
    round(sum(CmpMrtTCuM),2)  as CmpMrtTCuM,
    round(sum(CmpHrvTCuM),2)  as CmpHrvTCuM,
    round(sum(CmpRsdTCuM),2)  as CmpRsdTCuM,
    round(sum(CmpLiveMCuM),2) as CmpLiveMCuM,
    round(sum(CmpMrtMCuM),2)  as CmpMrtMCuM,
    round(sum(CmpHrvMCuM),2)  as CmpHrvMCuM,
    round(sum(CmpRsdMCuM),2)  as CmpRsdMCuM
  from temp.CmpStdStkDBHSp
  group by MgmtID,Year; 
create table CmpStdStk_Metric as 
  select MgmtID,Year,Species,DBHClass,
    round(CmpLiveTPH ,2) as CmpLiveTPH,
    round(CmpMrtTPH  ,2) as CmpMrtTPH,
    round(CmpHrvTPH  ,2) as CmpHrvTPH,
    round(CmpRsdTPH  ,2) as CmpRsdTPH,
    round(CmpLiveBA  ,2) as CmpLiveBA,
    round(CmpMrtBA   ,2) as CmpMrtBA,
    round(CmpHrvBA   ,2) as CmpHrvBA,
    round(CmpRsdBA   ,2) as CmpRsdBA,
    round(CmpLiveTCuM,2) as CmpLiveTCuM,
    round(CmpMrtTCuM ,2) as CmpMrtTCuM,
    round(CmpHrvTCuM ,2) as CmpHrvTCuM,
    round(CmpRsdTCuM ,2) as CmpRsdTCuM,
    round(CmpLiveMCuM,2) as CmpLiveMCuM,
    round(CmpMrtMCuM ,2) as CmpMrtMCuM,
    round(CmpHrvMCuM ,2) as CmpHrvMCuM,
    round(CmpRsdMCuM ,2) as CmpRsdMCuM
  from temp.CmpStdStkDBHSp;
insert into CmpStdStk_Metric select * from temp.CmpStdStkAllSp; 
insert into CmpStdStk_Metric select * from temp.CmpStdStkDBHAll; 
insert into CmpStdStk_Metric select * from temp.CmpStdStkAllAll;"   
  
Create_CmpSummary2 = "
drop table if exists temp.CmpSummary2A;
create table temp.CmpSummary2A as 
 select MgmtID,Year,RmvCode,
    round(sum(Age     *SamplingWt)/sum(SamplingWt),2) as CmpAge,
    round(sum(TPH     *SamplingWT)/sum(SamplingWt),2) as CmpTPH,    
    round(sum(TprdTPH *SamplingWT)/sum(SamplingWt),2) as CmpTprdTPH,    
    round(sum(BA      *SamplingWT)/sum(SamplingWt),2) as CmpBA,
    round(sum(SDI     *SamplingWT)/sum(SamplingWt),2) as CmpSDI,    
    round(sum(CCF     *SamplingWT)/sum(SamplingWt),2) as CmpCCF,
    round(sum(TopHt   *SamplingWT)/sum(SamplingWt),2) as CmpTopHt,
    round(sum(QMD     *SamplingWT)/sum(SamplingWt),2) as CmpQMD,
    round(sum(TCuM    *SamplingWT)/sum(SamplingWt),2) as CmpTCuM,
    round(sum(TprdTCuM*SamplingWT)/sum(SamplingWt),2) as CmpTprdTCuM,
    round(sum(MCuM    *SamplingWT)/sum(SamplingWt),2) as CmpMCuM,
    round(sum(TprdMCuM*SamplingWT)/sum(SamplingWt),2) as CmpTprdMCuM,
    round(sum(RTPH    *SamplingWT)/sum(SamplingWt),2) as CmpRTPH,  
    round(sum(RTCuM   *SamplingWT)/sum(SamplingWt),2) as CmpRTCuM,      
    round(sum(RMCuM   *SamplingWT)/sum(SamplingWt),2) as CmpRMCuM,
    round(sum(SamplingWt                          ),2) as CmpSamplingWt
  from (select * from FVS_Summary2_Metric where CaseID in (select CaseID from temp.Cases))
  join FVS_Cases using (CaseID)   
  group by MgmtID,Year,RmvCode order by MgmtID,Year,RmvCode;
 
drop table if exists temp.CmpSummary2B;
create table temp.CmpSummary2B as 
 select MgmtID,Year,RmvCode,max(RmvCode) as newRmv,
    round(sum(CmpAge     *CmpSamplingWt)/sum(CmpSamplingWt),2)  as CmpAge,
    round(sum(CmpTPH     *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpTPH,    
    round(sum(CmpTprdTPH *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpTprdTPH,    
    round(sum(CmpBA      *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpBA,
    round(sum(CmpSDI     *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpSDI,    
    round(sum(CmpCCF     *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpCCF,
    round(sum(CmpTopHt   *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpTopHt,
    round(sum(CmpQMD     *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpQMD,
    round(sum(CmpTCuM    *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpTCuM,
    round(sum(CmpTprdTCuM*CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpTprdTCuM,
    round(sum(CmpMCuM    *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpMCuM,
    round(sum(CmpTprdMCuM*CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpTprdMCuM,
    round(sum(CmpRTPH    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRTPH,  
    round(sum(CmpRTCuM   *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpRTCuM,      
    round(sum(CmpRMCuM   *CmpSamplingWT)/sum(CmpSamplingWt),2)  as CmpRMCuM,
    round(sum(CmpSamplingWt                          ),2) as CmpSamplingWt
  from temp.CmpSummary2A where RmvCode in (0,1)
  group by MgmtID,Year order by MgmtID,Year;

insert into temp.CmpSummary2B 
 select MgmtID,Year,RmvCode,max(RmvCode) as newRmv,
    round(sum(CmpAge     *CmpSamplingWt)/sum(CmpSamplingWt),2) as CmpAge,
    round(sum(CmpTPH     *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTPH,    
    round(sum(CmpTprdTPH *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdTPH,    
    round(sum(CmpBA      *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpBA,
    round(sum(CmpSDI     *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpSDI,    
    round(sum(CmpCCF     *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpCCF,
    round(sum(CmpTopHt   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTopHt,
    round(sum(CmpQMD     *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpQMD,
    round(sum(CmpTCuM    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTCuM,
    round(sum(CmpTprdTCuM*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdTCuM,
    round(sum(CmpMCuM    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpMCuM,
    round(sum(CmpTprdMCuM*CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpTprdMCuM,
    round(sum(CmpRTPH    *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRTPH,  
    round(sum(CmpRTCuM   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRTCuM,      
    round(sum(CmpRMCuM   *CmpSamplingWT)/sum(CmpSamplingWt),2) as CmpRMCuM,
    round(sum(CmpSamplingWt                          ),2) as CmpSamplingWt
  from temp.CmpSummary2A where RmvCode in (0,2)
  group by MgmtID,Year order by MgmtID,Year;
  
drop table if exists CmpSummary2_Metric;
create table CmpSummary2_Metric as 
  select distinct MgmtID,Year,newRmv as RmvCode,CmpAge,CmpTPH,CmpTprdTPH,  
    CmpBA,CmpSDI,CmpCCF,CmpTopHt,CmpQMD,CmpTCuM,
    CmpTprdTCuM,CmpMCuM,CmpTprdMCuM,
    CmpRTPH,CmpRTCuM,CmpRMCuM,CmpSamplingWt
 from temp.CmpSummary2B
 order by MgmtID,Year,RmvCode;
drop table if exists temp.CmpSummary2A;
drop table if exists temp.CmpSummary2B;"


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

