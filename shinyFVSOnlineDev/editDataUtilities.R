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

    
checkMinColumnDefs <- function(dbGlb,progress=NULL)
{
  #this routine may need to be rebuilt. One issue is that the Stand_CN may not be
  # in the TreeInit table. That is not checked in this code.
cat ("in checkMinColumnDefs\n")
  stdInit <- NULL
  for (i in 1:length(dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[,1])){
    if (!is.na(match(toupper(dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[[1]][i]), toupper("FVS_StandInit")))){
      stdInit <- dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[[1]][i]
    }
  }
  stdInit_cond <- NULL
  for (i in 1:length(dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[,1])){
    if (!is.na(match(toupper(dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[[1]][i]), toupper("FVS_StandInit_Cond")))){
      stdInit_cond <- dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[[1]][i]
    }
  }
  if (!is.null(stdInit_cond))return("Uploaded database installed")
  if (is.null(stdInit))   
  {
    file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
    return("FVS_StandInit not found, training data installed.")
  }
  fields = try(dbListFields(dbGlb$dbIcon,stdInit))
  # if this is an error, then FVS_StandInit does not exist and this is an error
  # where the standard fixup in this case is to try recovery of the database.
  if (class(fields) == "try-error")
  {
    file.copy("FVS_Data.db.default","FVS_Data.db",overwrite=TRUE)
    return("FVS_StandInit not found, training data installed.")
  }
  modStarted = FALSE
  sID = FALSE
  sCN = FALSE
  grp = FALSE
  # make sure groups are defined, if missing set one to "All"
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = 2, detail = "Groups")
  if (length(grep("Groups",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbGlb$dbIcon)}
    dbExecute(dbGlb$dbIcon,paste0("alter table ",stdInit,
       " add column Groups text not null default 'All_Stands'"))
    grp = TRUE
  }
  # make sure Stand_ID is defined
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = 3, detail = "Stand_ID")
  if (length(grep("Stand_ID",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbGlb$dbIcon)}
    dbExecute(dbGlb$dbIcon,paste0("alter table ",stdInit,
      " add column Stand_ID text"))      
    sID = TRUE
  }
  # make sure Stand_CN is defined
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = 4, detail = "Stand_CN")
  if (length(grep("Stand_CN",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbGlb$dbIcon)}
    dbExecute(dbGlb$dbIcon,paste0("alter table ",stdInit,
      " add column Stand_CN text"))
    sCN = TRUE
  }
  # make sure Inv_Year is defined
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = 5, detail = "Inv_Year ")
  if (length(grep("Inv_Year",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbGlb$dbIcon)}
    year=substring(as.character(Sys.time()),1,4)
    dbExecute(dbGlb$dbIcon,paste0(paste0("alter table ",stdInit,
      " add column Inv_Year integer not null default ",year)))      
  }
  # make sure FVSKeywords is defined
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = 6, detail = "FVSKeywords ")
  if (length(grep("FVSKeywords",fields,ignore.case=TRUE)) == 0)
  {
    if (!modStarted) {modStarted=TRUE; dbBegin(dbGlb$dbIcon)}
    year=substring(as.character(Sys.time()),1,4)
    dbExecute(dbGlb$dbIcon,paste0("alter table ",stdInit,
      " add column FVSKeywords text"))
  }
cat ("in checkMinColumnDefs, modStarted=",modStarted," sID=",sID,
     " sCN=",sCN,"\n")
  if (modStarted)
  {                                             
    dbCommit(dbGlb$dbIcon)
    if (sID || sCN) 
    {
      fvsInit = dbReadTable(dbGlb$dbIcon,stdInit)
      if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
        value = 7, detail = "Stand_ID and Stand_CN consistent")
      if (nrow(fvsInit))
      {
        isCN = grep("Stand_CN",names(fvsInit),ignore.case=TRUE)
        if (sID) fvsInit$Stand_ID = 
          if (sCN) paste0("Stand",1:nrow(fvsInit)) else fvsInit[,isCN]
        isID = grep("Stand_ID",names(fvsInit),ignore.case=TRUE)
        if (sCN) 
        {
          isCN = grep("Stand_CN",names(fvsInit),ignore.case=TRUE)
          fvsInit[,isCN] = fvsInit[,isID]
          isID = grep("Stand_ID",names(fvsInit),ignore.case=TRUE)
          fvsInit[,isCN] = fvsInit[,isID]
        }
        dbWriteTable(dbGlb$dbIcon,stdInit,fvsInit,overwrite=TRUE)
      }
    }
  }
  # check groups
  if (!grp)
  {
    if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
        value = 8, detail = "Groups content")
    grps = dbGetQuery(dbGlb$dbIcon,paste0("select Groups from ",stdInit))
    names(grps) = toupper(names(grps))
    if (is.null(grps$GROUPS) || any(is.na(grps$GROUPS)) || any(grps$GROUPS == "")) 
      dbExecute(dbGlb$dbIcon,paste0("update ",stdInit,
         " set Groups = 'All_Stands' where Groups = ''"))
  }
  # check on FVS_GroupAddFilesAndKeywords, if present, assume it is correct
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
      value = 9, detail = "FVS_GroupAddFilesAndKeywords")
  addkeys = getTableName(dbGlb$dbIcon,"FVS_GroupAddFilesAndKeywords")
  if (is.null(addkeys)) need = TRUE else
  {
    gtab = try(dbReadTable(dbGlb$dbIcon,addkeys))
    need = class(gtab) == "try-error"
    if (!need) need = nrow(gtab) == 0
    names(gtab) = toupper(names(gtab))
    if (!need) need = all(is.na(gtab$FVSKEYWORDS))
    if (!need) need = all(gtab$FVSKEYWORDS == "")
  }
  if (need)
  {
    treeInit = getTableName(dbGlb$dbIcon,"FVS_TreeInit")
    if (is.null(treeInit)) treeInit = "FVS_TreeInit"
    dfin = data.frame(Groups = "All All_Stands",Addfiles = "",
      FVSKeywords = paste0("Database\nDSNIn\nFVS_Data.db\nStandSQL\n",
        "SELECT * FROM ",stdInit,"\nWHERE Stand_ID= '%StandID%'\n",
        "EndSQL\nTreeSQL\nSELECT * FROM ",treeInit,"\n", 
        "WHERE Stand_ID= '%StandID%'\nEndSQL\nEND")    
    )
    dbWriteTable(dbGlb$dbIcon,"FVS_GroupAddFilesAndKeywords",value=dfin,overwrite=TRUE)
  } 
  return("Uploaded database installed")  
}


fixFVSKeywords <- function(dbGlb,progress=NULL)
{
  tbs <- dbGetQuery(dbGlb$dbIcon,"select name from sqlite_master where type='table';")[,1]
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
      kwdf <- dbGetQuery(dbGlb$dbIcon,qry)
cat ("result nrow=",nrow(kwdf),"\n")      
      if (!is.null(progress)) progress$set(detail = 
        paste0("Table=",tb," Field=",kwdname," Rows to process=",nrow(kwdf)))
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
             dbExecute(dbGlb$dbIcon,qry)              
          }
          dbCommit(dbGlb$dbIcon)
        }
      }
    }
  }
cat ("exit fixFVSKeywords\n")
}    

mkInserts <- function(inputTbl,tblName,tbsCTypes)
{
  inserts=NULL
  colnames(inputTbl) = toupper(colnames(inputTbl))
  names   (tbsCTypes)= toupper(   names(tbsCTypes))
  for (i in 1:nrow(inputTbl))
  {
    line = na.omit(inputTbl[i,inputTbl[i,] != ""])
    if (length(line) > 0)
    {
      vars = paste0(names(line),collapse=",")
      charVars = na.omit(tbsCTypes[names(line)])
      if (length(charVars)) line[charVars] = paste0("'",line[charVars],"'")
      vals = paste0(line,collapse=",")
      ins  = paste0("insert into ",tblName," (",vars,") values (",vals,");")
      inserts = c(inserts,ins)
    }
  }
  inserts
}
    
