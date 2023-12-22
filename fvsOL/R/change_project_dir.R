####################################################################################
# change_project_dir - r.20230711
#
# Accepts path to directory
#
# Opens new interface instance with last modified project in chosen directory
# 
# makes heavy usage of code from server.R for input input$PrjOpen
#
# Function does not verify user access to chosen directory,
# must be handled prior to function call
####################################################################################


change_project_dir <- function(new_proj_dir) {
  cat(paste0("User Specified Dir: ", new_proj_dir))

  if (dir.exists(new_proj_dir)) {
    if (isLocal()) {
      if (exists("RscriptLocation")) {
        rscript= RscriptLocation
      } else {
        exe_file = normalizePath(commandArgs(trailingOnly = FALSE)[1])

        if (.Platform$OS.type == "windows") {
          bin = regexpr("\\\\bin\\\\", exe_file)
        } else {
          bin = regexpr("/bin/", exe_file)
        }

        bin = substr(exe_file, 1, bin + attr(bin, "match.length") - 2)

        if (.Platform$OS.type == "windows") {
          file.path(bin,"Rscript.exe")
        } else {
          file.path(bin,"Rscript")
        }
      }

      rscript = gsub("\\\\", "/", rscript)
      defs = paste0("RscriptLocation='", rscript, "';")

      if (exists("mdbToolsDir")) {
        defs = paste0(defs, "mdbToolsDir='", mdbToolsDir, "';")
      }

      if (exists("sqlite3exe")) {
        defs = paste0(defs, "sqlite3exe='", sqlite3exe, "';")
      }

      cat(".libPaths=", unlist(.libPaths()), "\n")

      if (exists("RscriptLocation")) {
        Rlib2Use <- 
        paste0(dirname(dirname(dirname(RscriptLocation))), "/library")
        defs = paste0(defs, ".libPaths('", Rlib2Use, "');")
      }

      # Get list of projects in supplied Directory
      prjs = list()
      dirs = dir(new_proj_dir)
      for (dir in dirs) {
        if (file.exists(paste0(new_proj_dir, "/", dir, "/projectId.txt"))){
          prjs = append(prjs, paste0(new_proj_dir, "/", dir))
          prjs <- as.character((prjs))
        }
      }

      if (!length(prjs)) {
        if (file.exists(paste0(new_proj_dir, "/Project_1/projectId.txt"))) {
            #Display notice of locked project
          } else {
          #Create new project_1 directory and launch
          dir.create(paste0(new_proj_dir, "/Project_1"))
          write(file = paste0(new_proj_dir, "/Project_1/projectId.txt"),
            "title= Project_1")
            prjs = append(prjs, paste0(new_proj_dir, "/Project_1"))
        }
      }

      ord = sort(unlist(lapply(prjs, function(x) as.integer(file.mtime(x)))),
        decreasing = TRUE, index.return = TRUE)$ix

      cmd = paste0("$",rscript,"$ --vanilla -e $", defs, "require(fvsOL)",
                    ";fvsOL(prjDir='", prjs[ord[1]], "',fvsBin='", fvsBin, "');
                    quit()$")
      cmd = gsub('$', '\"', cmd, fixed=TRUE)

      if (.Platform$OS.type == "unix") {
        cmd = paste0("nohup ", cmd, " >> /dev/null")
      }
      rtn=try(system(cmd, wait=FALSE))
      cat("cmd for launch project=", cmd, "\nrtn=", rtn, "\n")
     }
  }
}