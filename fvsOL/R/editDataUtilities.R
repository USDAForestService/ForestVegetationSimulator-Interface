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
    
