####################################################################################
# validateProjectOpen
#
# Accepts object containing members $name and $datapath.
#
# Verifies filename is "projectId.txt", and that the title of the project 
#   listed in the file matches the name of the folder it is contained in.
#
# Returns 1 if improper naming found, returns null if successful
#
####################################################################################


validateProjectOpen<- function(fileObj){
  if(fileObj$name != "projectId.txt"){
    cat("Files selected not a valid project")
    return(1)
  }
 # print(fileObj)
  fileConts<- readLines(fileObj$datapath)
  # Get information from line after "title="
  projectName<- substr(fileConts, 7, nchar(fileConts))
  # Remove leading whitespace
  projectName<- trimws(projectName)
  prjPathName = substr(fileObj$datapath, nchar(fileObj$datapath)- (nchar(projectName)+ 13), nchar(fileObj$datapath)- 14)
  
  if(projectName != prjPathName){
    cat("File mismatch between projectID.txt and path name\n")
    cat(paste("project txt: ", projectName,"\n"))
    cat(paste("Path ID: ", prjPathName,"\n"))
    return(1)
  }
  return()
}