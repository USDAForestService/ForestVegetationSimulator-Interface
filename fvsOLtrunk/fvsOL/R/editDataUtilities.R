# $Id$

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
  qry = paste0("select count(*) from '",dbGlb$tblName,"'")
cat("in fixEmptyTable, qry=",qry,"\n")
  tmp = try(dbGetQuery(dbGlb$dbIcon,qry))
  if (class(tmp)=="try-error") return()
  if (tmp[1,1] > 0) return()    
  tmp = dbReadTable(dbGlb$dbIcon,dbGlb$tblName)
  tmp[1,] = tmp[1,]  
  dbWriteTable(dbGlb$dbIcon,dbGlb$tblName,tmp,overwrite=TRUE)
  qry=paste0("select _ROWID_,* from '",dbGlb$tblName,"'")
cat(" qry=",qry,"\n")
  tmp = try(dbGetQuery(dbGlb$dbIcon,qry))
  if (class(tmp)=="try-error") return()
  dbGlb$tbl  <- tmp
  rownames(dbGlb$tbl) = dbGlb$tbl$rowid
  dbGlb$rows = c(1,1)
  dbGlb$tbl$Delete = FALSE
}

checkMinColumnDefs <- function(dbo,progress=NULL,pn=0)
{
cat ("in checkMinColumnDefs\n")
  for (initnm in c("FVS_StandInit","FVS_PlotInit","FVS_StandInit_Cond"))
  {
    stdInit = getTableName(dbo,initnm)
    if (!is.null(stdInit)) break
  }
cat ("stdInit=",stdInit,"\n")
  if (is.null(stdInit)) return("No standinit table was found.")
  fields = try(dbListFields(dbo,stdInit))
  # if this is an error, then FVS_StandInit does not exist and this is an error
  # where the standard fixup in this case is to try recovery of the database.
  if (class(fields) == "try-error") return("Geting column names from StandInit table failed.")
  sID = FALSE
  sCN = FALSE
  grp = FALSE
  # make sure groups are defined, if missing set one to "All_Stands"
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = pn+1, detail = "Groups")
  if (length(grep("Groups",fields,ignore.case=TRUE)) == 0)
  {
    qt = try(dbExecute(dbo,paste0("alter table '",stdInit,
       "' add column Groups text default 'All All_Stands'")))
    if (class(qt)=="try-error") return ("Adding group 'All All_Stands' to StandInit failed.")
    grp = TRUE
  }
  # make sure Stand_ID is defined
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = pn+2, detail = "Stand_ID")
  if (length(grep("Stand_ID",fields,ignore.case=TRUE)) == 0)
  {
    qt = try(dbExecute(dbo,paste0("alter table '",stdInit,
      "' add column Stand_ID text")))      
    if (class(qt)=="try-error") return ("Adding 'Stand_ID' to StandInit failed.")
    sID = TRUE
  }
  # make sure Stand_CN is defined
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = pn+3, detail = "Stand_CN")
  if (length(grep("Stand_CN",fields,ignore.case=TRUE)) == 0)
  {
    qt = try(dbExecute(dbo,paste0("alter table '",stdInit,
      "' add column Stand_CN text")))
    if (class(qt)=="try-error") return ("Adding 'Stand_CN' to StandInit failed.")
    sCN = TRUE
  }
  # make sure Inv_Year is defined
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = pn+4, detail = "Inv_Year ")
  if (length(grep("Inv_Year",fields,ignore.case=TRUE)) == 0)
  {
    year=substring(as.character(Sys.time()),1,4)
    qt=try((dbExecute(dbo,paste0(paste0("alter table '",stdInit,
      "' add column Inv_Year integer default ",year)))))    
    if (class(qt)=="try-error") return ("Adding 'Inv_Year' to StandInit failed.")
  }
  # make sure FVSKeywords is defined
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = pn+5, detail = "FVSKeywords ")
  if (length(grep("FVSKeywords",fields,ignore.case=TRUE)) == 0)
  {
    year=substring(as.character(Sys.time()),1,4)
    qt=try(dbExecute(dbo,paste0("alter table '",stdInit,
      "' add column FVSKeywords text")))
    if (class(qt)=="try-error") return ("Adding 'FVSKeywords' to StandInit failed.")
  }
  # make sure Sam_Wt is defined
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
    value = pn+6, detail = "Sam_Wt")
  if (length(grep("Sam_Wt",fields,ignore.case=TRUE)) == 0)
  {
    qt=try(dbExecute(dbo,paste0("alter table ",stdInit,
      " add column Sam_Wt real")))
    if (class(qt)=="try-error") return ("Adding 'Sam_Wt' to StandInit failed.")
  }
cat ("in checkMinColumnDefs sID=",sID," sCN=",sCN,"\n")
  if (sID || sCN) 
  {
    fvsInit = try(dbReadTable(dbo,stdInit))
    if (class(fvsInit)=="try-error") return ("Can not read StandInit.")
    if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
      value = pn+7, detail = "Stand_ID and Stand_CN consistent")
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
      dbWriteTable(dbo,stdInit,fvsInit,overwrite=TRUE)
    }
  }
  # check groups
  if (!grp)
  {
    if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
        value = pn+8, detail = "Groups content")
    grps = try(dbGetQuery(dbo,paste0("select Groups from '",stdInit,"'"))) 
    if (class(grps)=="try-error") return ("Can not read Groups from StandInit.")
    names(grps) = toupper(names(grps))
    if (is.null(grps$GROUPS) || any(is.na(grps$GROUPS)) || any(grps$GROUPS == ""))
    {
      qt =try(dbExecute(dbo,paste0("update '",stdInit,
         " set Groups = 'All_Stands' where Groups = ''")))
      if (class(qt)=="try-error") return ("Failure updating 'Groups' in StandInit.")  
    }
  }
  # check on FVS_GroupAddFilesAndKeywords, if present, assume it is correct
  if (!is.null(progress)) progress$set(message = paste0("Checking ",stdInit), 
      value = pn+9, detail = "FVS_GroupAddFilesAndKeywords")
  addkeys = getTableName(dbo,"FVS_GroupAddFilesAndKeywords")
  if (is.null(addkeys)) need = TRUE else
  {
    gtab = try(dbReadTable(dbo,addkeys))
    need = class(gtab) == "try-error"
    if (!need) need = nrow(gtab) == 0
    names(gtab) = toupper(names(gtab))
    if (!need) need = all(is.na(gtab$FVSKEYWORDS))
    if (!need) need = all(gtab$FVSKEYWORDS == "")
  }
  if (need)
  { 
    treeInit = getTableName(dbo,"FVS_TreeInit")
    if (is.null(treeInit)) return("Needed FVS_GroupAddFilesAndKeywords not added.")
    dfinstand=NULL
    grps = list("FVS_StandInit"="All All_Stands",
                "FVS_PlotInit"="All All_Plots",
                "FVS_StandInit_Cond"="All All_Conds")
    for (std in names(grps))
    {
      stdInit = getTableName(dbo,std)
      if (is.null(stdInit)) next
      linkID = if(stdInit=="FVS_PlotInit") "StandPlot_ID" else "Stand_ID"
      dfinstand = rbind(dfinstand,
        data.frame(Groups = grps[[std]],Addfiles = "",
          FVSKeywords = paste0("Database\nDSNIn\nFVS_Data.db\nStandSQL\n",
            "SELECT * FROM ",stdInit,"\nWHERE ",linkID,"= '%StandID%'\n",
            "EndSQL\nTreeSQL\nSELECT * FROM ",treeInit,"\n", 
            "WHERE ",linkID,"= '%StandID%'\nEndSQL\nEND")))
    }
    dbWriteTable(dbo,"FVS_GroupAddFilesAndKeywords",value=dfinstand,overwrite=TRUE)
  }
  return(NULL)
}


fixFVSKeywords <- function(dbo)
{
  tbs <- dbGetQuery(dbo,"select name from sqlite_master where type='table';")[,1]
  for (tb in tbs)
  {
cat ("in fixFVSKeywords, tb=",tb,"\n")
    flds <- dbListFields(dbo, tb)
    kwdsIdxs <- grep ("keywords",flds,ignore.case = TRUE)
    if (length(kwdsIdxs) == 0) next
    for (kwdname in flds[kwdsIdxs])
    {
      qry = paste0("select _ROWID_,",kwdname," from '",tb,
        "' where ",kwdname," is not null and ",kwdname," != '';")
cat ("qry=",qry,"\n")              
      kwdf <- try(dbGetQuery(dbo,qry))                                                              
      if (class(kwdf)=="try-error") return (paste0("Error in ",qry))
cat ("result nrow=",nrow(kwdf),"\n")                                  
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
          for (row in 1:nrow(kwdf))
          {
            qut <- if (length(grep("'",kwdf[row,2],fixed=TRUE))) "\"" else "'"
            qry <- paste0("update '",tb,"' set ",kwdname," = ",qut,
              kwdf[row,2],qut," where _ROWID_ = ",kwdf[row,1],";")
cat ("qry=",qry,"\n")              
             qry <- try(dbExecute(dbo,qry))
             if (class(qry)=="try-error")                                                                  
               return (paste0("Error in ",qry))             
          }
        }
      }
    }
  }
cat ("exit fixFVSKeywords\n")
  return(NULL)
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
    
