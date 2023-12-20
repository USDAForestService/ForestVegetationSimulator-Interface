checkMinColumnDefs <- function(dbo, progress = NULL, pn = 0) {
  cat("In checkMinColumnDefs\n")
  # Set up list of column names that are automatic database rejection
  reject_if_missing <- c("Variant", "Stand_ID", "Inv_Year")

  # Set up data frame to dub in required columns if missing
  StdInitColumnReq <- data.frame(
    ColumnName = c("Groups", "FVSKeywords", "Sam_Wt"),
    DataType = c("text", "text", "real"),
    Default = c("'All_Stands'", NA, NA)
  )

  FVS_StandInit = FALSE
  valid_table_found = FALSE
  for (initnm in c("FVS_StandInit", "FVS_PlotInit", 
                   "FVS_StandInit_Plot", "FVS_PlotInit_Plot", "FVS_StandInit_Cond")) {
    initnm_exists = FALSE
    tryCatch(
      # Try to read table from db connection
      {
        stdInit <- dbReadTable(dbo, initnm)
        initnm_exists = TRUE
        valid_table_found = TRUE
      },

      # if an error . . .
      error = function(e) {
        cat(paste0("Table: ", initnm, " not found in database\n"))
      }
    )
    if (initnm_exists) {
      if (initnm == "FVS_StandInit") FVS_StandInit = TRUE

      # get list of column names in table
      fields <- tolower(names(stdInit))
      reject = reject_if_missing
    
      if (initnm == 'FVS_PlotInit' || initnm == 'FVS_PlotInit_Plot') {
        plotInit = c("StandPlot_ID")
        reject = c(reject_if_missing, plotInit)
      }


      # Check for missing required columns that would reject database
      for (e in reject) {
        if (!is.null(progress)) {
          pn = pn+1
          progress$set(message = paste0("Checking ", initnm), value = pn, 
            detail = e)
        }

        if (!(tolower(e) %in% fields)) {
            return(paste0("<h4>Input database invalid.<br>",
              initnm, " table Missing column: '", e, "'</h4>"))
        }

        # Add logic to check for blank entries
        # Note: Sqlite extensions 'math', 'regexp', 'series', 'csv' not enabled by default
        RSQLite::initExtension(dbo, extension = c('regexp'))
        q <- paste0("SELECT COUNT(*) FROM ", initnm, " WHERE ", e,
           " NOT REGEXP '[A-Za-z0-9_]' OR ", e, " IS NULL")
        
        if (tolower(e) == 'inv_year'){
          q <- paste0("SELECT COUNT (*) FROM ", initnm, " WHERE INV_YEAR <= 0 OR INV_YEAR IS NULL")
        }
           tryCatch(
           {
             result <- dbGetQuery(dbo, q)
             if (result > 0) {
               return(paste0("<h4>Input database invalid.<br>",
               initnm, " Column ", e, " contains a blank or missing value</h4>"))
             }
           },
            error = function(e) {
              return(paste0("Attempt to read column: ", e, "Failed."))
            }
          )
      }

      for (reqColumn in StdInitColumnReq$ColumnName) {
        type = StdInitColumnReq$DataType[
                 StdInitColumnReq$ColumnName == reqColumn]

        default = StdInitColumnReq$Default[
                    StdInitColumnReq$ColumnName == reqColumn]

        if (reqColumn == "Groups" &&
              (initnm == "FVS_PlotInit" || initnm == "FVS_PlotInit_Plot")) {
          default = "'All_Plots'"
        }

        if (!is.null(progress)) {
            pn = pn + 1
            progress$set(message = paste0("Checking ", initnm),
              value = pn, detail = reqColumn)
        }

        if (!(tolower(reqColumn) %in% fields)) {

          tryCatch(
            {
              if(!is.na(default)){
                dbExecute(dbo, paste0("alter table ", initnm, " add column ", 
                  reqColumn, " ", type, " default ", default, ";"))
              }
              else{
                dbExecute(dbo, paste0("alter table ", initnm, " add column ", 
                  reqColumn, " ", type, ";"))
              }
                
            },
            error = function(e) {
              return(paste0("Attempt to add column: ", reqColumn, "Failed."))
            }
          )
        }
      }
    }
  }
  if (!valid_table_found) {
    return(paste0("<h4>Input database invalid.<br>",
               "No valid StandInit or PlotInit tables found</h4>"))
  }
  return(NULL)
}