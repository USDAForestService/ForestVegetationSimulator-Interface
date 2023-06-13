####################################################################################
# change_project_dir
#
# Accepts path to existing project directory.
#
# Opens new interface instance with specified path as project
# 
# makes heavy usage of code from server.R for input input$PrjOpen
#
####################################################################################


change_project_dir <- function(newPrjDir){
  cat("Open project function called")
  projectIsLocked= file.exists(paste0(newPrjDir,"/projectIsLocked.txt"))
  cat("PrjOpen to=",newPrjDir," dir.exists(newPrj)=",dir.exists(newPrjDir),
      " locked=",projectIsLocked,"\n")

  if (dir.exists(newPrjDir)){
    if (isLocal()){
      if (exists("RscriptLocation")){
        rscript= RscriptLocation
      }
      else{
        exefile=normalizePath(commandArgs(trailingOnly=FALSE)[1])
        
        if(.Platform$OS.type == "windows"){
          bin=  regexpr("\\\\bin\\\\",exefile)
        }
        else{
          bin= regexpr("/bin/",exefile)
        } 
        
        bin = substr(exefile,1,bin+attr(bin,"match.length")-2)
        
        if(.Platform$OS.type == "windows"){
          file.path(bin,"Rscript.exe")
        }
        else{
          file.path(bin,"Rscript")
        } 
      }
      rscript=gsub("\\\\","/",rscript)
      defs=paste0("RscriptLocation='",rscript,"';")
      
      if (exists("mdbToolsDir")){
        defs=paste0(defs,"mdbToolsDir='",mdbToolsDir,"';")  
      } 
      
      if (exists("sqlite3exe")){
        defs=paste0(defs,"sqlite3exe='",sqlite3exe,"';")  
      }  
      
      cat(".libPaths=",unlist(.libPaths()),"\n")
      
      if (exists("RscriptLocation")) {
        Rlib2Use <- paste0(dirname(dirname(dirname(RscriptLocation))),"/library")
        defs=paste0(defs,".libPaths('",Rlib2Use,"');")
      }
      
      cmd =  paste0("$",rscript,"$ --vanilla -e $",defs,"require(fvsOL)",";fvsOL(prjDir='",newPrjDir,"',fvsBin='",fvsBin,"');quit()$")
      cmd = gsub('$','\"',cmd,fixed=TRUE)    
      
      if (.Platform$OS.type == "unix"){
        cmd = paste0("nohup ",cmd," >> /dev/null")
      } 
      rtn=try(system (cmd,wait=FALSE))
      cat ("cmd for launch project=",cmd,"\nrtn=",rtn,"\n")
    }
    Sys.sleep(5)
  }
}