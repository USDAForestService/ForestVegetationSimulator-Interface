mkStdSel <- function (dbGlb)
{
  if (length(dbGlb$sids) > 1000) return(renderUI(NULL))
  dbGlb$rowSelOn <- TRUE
  renderUI(selectInput("rowSelector",
    "Select stand(s)", choices  = dbGlb$sids, multiple = TRUE, 
     selectize=FALSE, size=10))
}
      
fixEmptyTable <- function (dbGlb)
{
cat("in fixEmptyTable, dbGlb$tblName=",dbGlb$tblName,"\n")
  qry = paste0("select count(*) from ",dbGlb$tblName)
  tmp = dbGetQuery(dbGlb$dbIcon,qry)
  if (tmp[1,1] > 0) return()    
  tmp = dbReadTable(dbGlb$dbIcon,dbGlb$tblName)
  tmp[1,] = tmp[1,]  
  dbWriteTable(dbGlb$dbIcon,dbGlb$tblName,tmp,overwrite=TRUE)
  dbGlb$tbl  <- dbGetQuery(dbGlb$dbIcon,paste0("select _ROWID_,* from ",dbGlb$tblName))
  rownames(dbGlb$tbl) = dbGlb$tbl$rowid
  dbGlb$rows = c(1,1)
  dbGlb$tbl$Delete = FALSE
}

    
checkMinColumnDefs <- function(dbGlb)
{
  #this routine may need to be rebuilt. One issue is that the Stand_CN may not be
  # in the TreeInit table. That is not checked in this code.
cat ("in checkMinColumnDefs\n")
  fields = try(dbListFields(dbGlb$dbIcon,"FVS_StandInit"))
  # if this is an error, then FVS_StandInit does not exist and this is an error
  # where the standard fixup in this case is to try recovery of the database.
  if (class(fields) == "try-error")
  {
    if (file.exists("FVS_Data.db")) file.remove("FVS_Data.db")
    if (file.exists("FVS_Data.db.backup"))
    {
      file.rename("FVS_Data.db.backup","FVS_Data.db")
      checkMinColumnDefs(dbGlb$dbOcon)
    } else {
      file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
      unlink("FVS_Data.db.backup")
    }
    return()
  }
  modStarted = FALSE
  sID = FALSE
  sCN = FALSE
  grp = FALSE
  # make sure groups are defined, if missing set one to "All"
  if (length(grep("Groups",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbGlb$dbIcon)}
    dbSendQuery(dbGlb$dbIcon,
      "alter table FVS_StandInit add column Groups text not null default 'All_Stands'")
    grp = TRUE
  }
  # make sure Stand_ID is defined
  if (length(grep("Stand_ID",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbGlb$dbIcon)}
    dbSendQuery(dbGlb$dbIcon,
      "alter table FVS_StandInit add column Stand_ID text")      
    sID = TRUE
  }
  # make sure Stand_CN is defined
  if (length(grep("Stand_CN",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbGlb$dbIcon)}
    dbSendQuery(dbGlb$dbIcon,
      "alter table FVS_StandInit add column Stand_CN text")
    sCN = TRUE
  }
  # make sure Inv_Year is defined
  if (length(grep("Inv_Year",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbGlb$dbIcon)}
    year=substring(as.character(Sys.time()),1,4)
    dbSendQuery(dbGlb$dbIcon,paste0(
      "alter table FVS_StandInit add column Inv_Year integer not null default ",year))      
  }
  # make sure FVSKeywords is defined
  if (length(grep("FVSKeywords",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbGlb$dbIcon)}
    year=substring(as.character(Sys.time()),1,4)
    dbSendQuery(dbGlb$dbIcon,
      "alter table FVS_StandInit add column FVSKeywords text")
  }
cat ("in checkMinColumnDefs, modStarted=",modStarted," sID=",sID,
     " sCN=",sCN,"\n")
  if (modStarted)
  {                                             
    dbCommit(dbGlb$dbIcon)
    if (sID || sCN) 
    {
      fvsInit = dbReadTable(dbGlb$dbIcon,"FVS_StandInit")
      if (nrow(fvsInit))
      {
        if (sID) fvsInit$Stand_ID = 
          if (sCN) paste0("Stand",1:nrow(fvsInit)) else fvsInit$Stand_CN
        if (sCN) fvsInit$Stand_CN = fvsInit$Stand_ID
        dbWriteTable(dbGlb$dbIcon,"FVS_StandInit",fvsInit,overwrite=TRUE)
      }
    }
  }
  # check groups
  if (!grp)
  {
    grps = dbGetQuery(dbGlb$dbIcon,"select Groups from FVS_StandInit")
    names(grps) = toupper(names(grps))
    if (is.null(grps$GROUPS) || any(is.na(grps$GROUPS)) || any(grps$GROUPS == "")) 
      dbSendQuery(dbGlb$dbIcon,
        "update FVS_StandInit set Groups = 'All_Stands' where Groups = ''")
  }
  # check on FVS_GroupAddFilesAndKeywords, if present, assume it is correct
  gtab = try(dbReadTable(dbGlb$dbIcon,"FVS_GroupAddFilesAndKeywords"))
  need = class(gtab) == "try-error"
  if (!need) need = nrow(gtab) == 0
  names(gtab) = toupper(names(gtab))
  if (!need) need = all(is.na(gtab$FVSKEYWORDS))
  if (!need) need = all(gtab$FVSKEYWORDS == "")
  if (need)
  {
    dfin = data.frame(Groups = "All All_Stands",Addfiles = "",
      FVSKeywords = paste0("Database\nDSNIn\nFVS_Data.db\nStandSQL\n",
        "SELECT * FROM FVS_StandInit\nWHERE Stand_CN= '%Stand_CN%'\n",
        "EndSQL\nTreeSQL\nSELECT * FROM FVS_TreeInit\n", 
        "WHERE Stand_CN= '%Stand_CN%'\nEndSQL\nEND")    
    )
    dbWriteTable(dbGlb$dbIcon,name="FVS_GroupAddFilesAndKeywords",value=dfin,overwrite=TRUE)
  }
}


fixFVSKeywords <- function(dbGlb)
{
  tbs <- dbListTables(dbGlb$dbIcon)
  for (tb in tbs)
  {
cat ("in fixFVSKeywords, tb=",tb,"\n")
    flds <- dbListFields(dbGlb$dbIcon, tb)
    kwdsIdxs <- grep ("keywords",flds,ignore.case = TRUE)
    if (length(kwdsIdxs) == 0) next
    for (kwdname in flds[kwdsIdxs])
    {    
      qry = paste0("select _ROWID_,",kwdname," from ",tb,
        " where ",kwdname," is not null and ",kwdname," != '';")
cat ("qry=",qry,"\n")              
      res <- dbSendQuery(dbGlb$dbIcon,qry)
      kwdf <- dbFetch(res, n=-1)
cat ("result nrow=",nrow(kwdf),"\n")      
      dbClearResult(dbGlb$dbIcon)
      if (nrow(kwdf))
      {
        for (row in 1:nrow(kwdf))
        {
          if (nchar(kwdf[row,2]) < 2) {kwdf[row,1] = -1; next}
          one <- gsub ("\r","",kwdf[row,2])
          dsnin <- grep ("dsnin",one,ignore.case = TRUE)
          if (length(dsnin) == 0) {kwdf[row,1] = -1; next}
          one <- scan(text=one,sep="\n",what="character",quiet=TRUE)
          dsnin <- grep ("dsnin",one,ignore.case = TRUE)+1
          one[dsnin] <- "FVS_Data.db"
          kwdf[row,2] <- paste0(paste0(one,collapse="\n"),"\n")
        }
        kwdf = subset(kwdf,rowid > 0)
        if (nrow(kwdf) > 0)
        {
          dbBegin(dbGlb$dbIcon)
          for (row in 1:nrow(kwdf))
          {
            qut <- if (length(grep("'",kwdf[row,2],fixed=TRUE))) "\"" else "'"
            qry <- paste0("update ",tb," set ",kwdname," = ",qut,
              kwdf[row,2],qut," where _ROWID_ = ",kwdf[row,1],";")
cat ("qry=",qry,"\n")              
             dbSendQuery(dbGlb$dbIcon,qry)              
          }
          dbCommit(dbGlb$dbIcon)
        }
      }
    }
  }
cat ("exit fixFVSKeywords\n")
}    
