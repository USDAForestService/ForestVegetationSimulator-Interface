# $Id$

library(shiny)                                 
library(RSQLite)
library(zip)
library(openxlsx)              

# set shiny.trace=T for reactive tracing (lots of output)
options(shiny.maxRequestSize=10000*1024^2,shiny.trace = FALSE,
        rgl.inShiny=TRUE) 

trim <- function (x) gsub("^\\s+|\\s+$","",x)
myListTables <- function(db)
{
  dbGetQuery(db,"select name from sqlite_master where type = 'table';")[,1]
}

shinyServer(function(input, output, session) 
{
  if (!interactive()) 
  {
    if (file.exists("FVSDataConvert.log")) 
    {
      unlink("FVSDataConvert.older.log")
      file.rename("FVSDataConvert.log","FVSDataConvert.older.log")
    }
    #make sure the sink stack is empty
    while (sink.number()) sink()
    sink("FVSOnline.log")
  }

cat ("FVSDataConvert server start.\n")
  output$actionMsg <- NULL

  ## Upload new database
  observe({
    if (is.null(input$uploadNewDB)) return()
    fext = tools::file_ext(basename(input$uploadNewDB$name))
cat ("fext=",fext,"\n")
    if (! (fext %in% c("accdb","mdb","xlsx","zip"))) 
    {
      output$actionMsg  = renderText('Uploaded file is not "accdb","mdb","xlsx", or "zip".')
      session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
      unlink(input$uploadNewDB$datapath)
      return()
    } 
    fdir = dirname(input$uploadNewDB$datapath)
    output$actionMsg = renderUI(NULL)
    progress <- shiny::Progress$new(session,min=1,max=20)
    if (fext == "zip") 
    {
      progress$set(message = "Unzip data", value = 1)
      unzip(input$uploadNewDB$datapath, junkpaths = TRUE, exdir = fdir)
      unlink(input$uploadNewDB$datapath)
      fname = dir(dirname(input$uploadNewDB$datapath))
      if (length(fname)>1) 
      {
        output$actionMsg = renderText(".zip contains more than one file.")
        lapply (dir(dirname(input$uploadNewDB$datapath),full.names=TRUE),unlink)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
        progress$close()
        return()
      } else if (length(fname) == 0) {
        output$actionMsg = renderText(".zip was empty.")
        session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
        progress$close()
        return()
      } else fname=paste0(paste0,dirname(input$uploadNewDB$datapath),"/",fname)
      fext = tools::file_ext(fname)
      if (! (fext %in% c("accdb","mdb","xlsx"))) 
      {
        output$actionMsg = renderText('.zip did not contain "accdb","mdb", or "xlsx".')
        lapply (dir(dirname(input$uploadNewDB$datapath),full.names=TRUE),unlink)
        session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
        progress$close()
        return()
      }
    } else fname = input$uploadNewDB$datapath
    dbOutFile = paste0(fdir,"/FVS_Data.db")
cat ("fext=",fext," fname=",fname," fdir=",fdir," dbOutFile=",dbOutFile,"\n")
    if (fext %in% c("accdb","mdb"))
    {
      progress$set(message = "Process schema", value = 2)
      fnschema = paste0(fdir,"/schema")
      cmd = if (.Platform$OS.type == "windows") 
        shQuote(paste0("C:/FVS/mdbtools/mdb-schema ",fname)) else
        paste0(paste0("mdb-schema ",fname))
cat ("cmd=",cmd,"\n")
      schema = if (.Platform$OS.type == "windows") try(shell(cmd,intern=TRUE)) else 
                                                   try(system(cmd,intern=TRUE))
      if (class(schema)=="try-error" || length(schema) < 2 || 
          schema[1] =="Unknown Jet version.") 
      {
        progress$close()  
        output$actionMsg = renderText(if (schema[1] =="Unknown Jet version.") 
          "Unknown Jet version. Possible corrupt database." else
          "Error when attempting to extract data from Access database.")
        session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
        return()
      }
      sqlImp = paste0(fdir,"/sqlite3.import")
      tbls = grep ("CREATE TABLE",schema,ignore.case=TRUE)
      schema = schema[tbls[1]:length(schema)]
      schema = gsub("\t"," ",schema,fixed=TRUE)   
      schema = gsub("[","]",schema,fixed=TRUE) 
      tbls = grep ("CREATE TABLE",schema,ignore.case=TRUE)
      tbln=unlist(lapply(schema[tbls],function(x) if (length(grep("]",x,fixed=TRUE)))
        scan(text=x,what="character",sep="]",quiet=TRUE)[2] else
        scan(text=x,what="character",quiet=TRUE)[3]))
      schema = gsub(" Long Integer"," Integer",schema,ignore.case=TRUE)
      schema = gsub(" Int"," Integer",schema,ignore.case=TRUE)
      schema = gsub(" Integereger"," Integer",schema,ignore.case=TRUE)
      schema = gsub(" Memo.*)"," Text",schema,ignore.case=TRUE)
      schema = gsub(" Memo"," Text",schema,ignore.case=TRUE)
      schema = gsub(" Text.*)"," Text",schema,ignore.case=TRUE)
      schema = gsub(" Double"," Real",schema,ignore.case=TRUE)
      schema = gsub(" SHORT_DATE_TIME,"," Text,",schema,ignore.case=TRUE)
      schema = gsub(" FLOAT,"," Real,",schema,ignore.case=TRUE)
      schema = gsub(" NOT NULL"," ",schema,,ignore.case=TRUE)
      schema = gsub(" Single"," Real",schema)                    
      schema = gsub("]",'"',schema,fixed=TRUE) 
      cat ("begin;\n",file=sqlImp)
      cat (paste0(schema,"\n"),file=sqlImp,append=TRUE)
      cat ("commit;\n",file=sqlImp,append=TRUE)
      progress$set(message = "Extract data", value = 3)  
      for (tab in tbln) 
      {
        progress$set(message = paste0("Export table ",tab), value = 3)
        cat ("begin;\n",file=sqlImp,append=TRUE)
        cmd = paste0 (if (.Platform$OS.type == "windows") "C:/FVS/mdbtools/" else "",
                       "mdb-export -I sqlite ",fname,' "',tab,'" >> ',sqlImp)
        cat ("cmd=",cmd,"\n")
        result = if (.Platform$OS.type == "windows") shell(cmd,intern=TRUE) else system(cmd,intern=TRUE)
        cat ("commit;\n",file=sqlImp,append=TRUE)
      }
      cat (".quit\n",file=sqlImp,append=TRUE)
      progress$set(message = "Import data to Sqlite3", value = 4) 
      cmd = paste0(if (.Platform$OS.type == "windows")
         "C:/FVS/SQLite/" else "","sqlite3 ",dbOutFile," < ",sqlImp)
cat ("cmd=",cmd,"\n")
      if (.Platform$OS.type == "windows") shell(cmd) else system(cmd)
cat ("cmd done.\n")
      dbo = dbConnect(dbDriver("SQLite"),dbOutFile)
    } else if (fext == "xlsx") 
    {
      progress$set(message = "Get data sheets", value = 3)
      sheets = getSheetNames(fname)
      sheetsU <- toupper(sheets)
      normNames = c("FVS_GroupAddFilesAndKeywords","FVS_PlotInit",                
                    "FVS_StandInit","FVS_TreeInit")
      dbo = dbConnect(dbDriver("SQLite"),dbOutFile)
      dbdis="databaseDescription.xlsx"
      standNT = try(read.xlsx(xlsxFile=dbdis,sheet="FVS_StandInit"))
      standNT = if (class(standNT) == "try-error") NULL else apply(standNT[,c(1,3)],2,toupper)
      treeNT = try(read.xlsx(xlsxFile=dbdis,sheet="FVS_TreeInit"))
      treeNT = if (class(treeNT) == "try-error") NULL else apply(treeNT[,c(1,3)],2,toupper)
      plotNT = try(read.xlsx(xlsxFile=dbdis,sheet="FVS_PlotInit"))
      plotNT = if (class(plotNT) == "try-error") NULL else apply(plotNT[,c(1,3)],2,toupper)
      i = 3
      for (sheet in sheets)
      {
        i = i+1
cat ("sheet = ",sheet," i=",i,"\n")
        progress$set(message = paste0("Processing sheet ",i," name=",sheet), value=i)
        sdat = read.xlsx(xlsxFile=fname,sheet=sheet)
        sdat[[3]] <- gsub("_x000D_", "", sdat[[3]])
        im = grep(sheet,normNames,ignore.case=TRUE)
        if (length(im)) sheet = normNames[im]
        NT = switch(sheet,"FVS_StandInit"=standNT,"FVS_TreeInit"=treeNT,
                          "FVS_PlotInit"=plotNT,NULL) 
        if (!is.null(NT))
        {
          std = pmatch(toupper(names(sdat)),NT[,1])        
          for (icol in 1:length(sdat))
          {
            if (!is.na(std[icol])) sdat[,icol] = 
              switch(NT[std[icol],2],
              "TEXT"    = as.character(sdat[,icol]),
              "REAL"    = as.numeric  (sdat[,icol]),
              "INTEGER" = as.integer  (sdat[,icol]))
          }
        }
        dbWriteTable(conn=dbo,name=sheet,value=sdat)
      }
    }
    tabs = myListTables(dbo)
    rowCnts = unlist(lapply(tabs,function (x) dbGetQuery(dbo,
      paste0("select count(*) as '",x,"' from '",x,"';"))))
    msg = lapply(names(rowCnts),function(x) paste0(x," (",rowCnts[x]," rows)"))
    msg = paste0("<b>Uploaded data:</b><br>",paste0(msg,collapse="<br>"),
          "<h5>To use these data in FVS, first download the database and then ",
          "uploaded them into a project (use the <i>Import Data</i> and then ",
          "<i>Upload inventory database</i> tabs in the FVS interface). ",
          "Please do not simply copy the data into a project directory.")   
    output$actionMsg = renderUI(HTML(msg))
    dbDisconnect(dbo)
    session$sendCustomMessage(type = "resetFileInputHandler","uploadNewDB")
    progress$close()
  })
  
  output$dbDownload <- downloadHandler(filename="FVS_Data.db",
     content = function (tf = tempfile())
      {
        sfile = paste0(dirname(input$uploadNewDB$datapath),"/FVS_Data.db")
        if (file.exists(sfile)) file.copy(sfile,tf) else
        {
          output$actionMsg = renderUI(HTML("<h4>FVS-Ready Sqlite3 database was not created."))
        }
      }, contentType=NULL)

})

