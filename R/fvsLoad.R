fvsLoad <-
function(fvsProgram,bin="../../trunk/bin")
{
  if (missing(fvsProgram)) stop("fvsProgram is required.")
  # strip program suffix if it is present
  fvsProgram=strsplit(fvsProgram,".",fixed=TRUE)[[1]][1]
  # add the suffix that is consistent for the platform
  fvsProgram=paste0(fvsProgram,.Platform$dynlib.ext)
  # if the last char of the bin is not a file separator, add one.
  if (substring(bin,nchar(bin)) != .Platform$file.sep) bin=paste(bin,.Platform$file.sep,sep="")
  
  ldfile = paste0(bin,fvsProgram)
  if (exists(".FVSLOADEDLIBRARY",envir=.GlobalEnv)) 
  {
    loaded=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)
    remove(".FVSLOADEDLIBRARY",envir=.GlobalEnv)
    lapply (loaded,dyn.unload)
  }
  if (file.exists(ldfile))
  {
    load = try(dyn.load(ldfile,local=TRUE,now=TRUE))
    if (class(load) == "try-error") stop (paste (ldfile,"was not loaded."))

    # this is a list of the routines needed by the API. Normally, all will
    # be present or all absent, if some are absent, the DLL is unloaded.
    neededRoutines = c("CfvsEvmonAttr","CfvsFFEAttrs","CfvsSVSObjData",
      "CfvsSetCmdLine","CfvsSpeciesAttr","CfvsSpeciesCode","CfvsStandID",
      "CfvsTreeAttr","CfvsUnitConversion",
      tolower(c("fvs","fvsAddActivity","fvsAddTrees","fvsDimSizes",
      "fvsGetRestartCode","fvsSVSDimSizes",
      "fvsSetStoppointCodes","fvsSummary")))
    found=unlist(lapply(neededRoutines,is.loaded))
    if (!all(found)) 
    {
      try(dyn.unload(ldfile))
      stop(paste0(paste0(neededRoutines[!found],collapse=","),
      ": needed routines that are not available in ",ldfile,
      " (maybe they were not exported when the library was built)"))
    }       
    assign(".FVSLOADEDLIBRARY",list("pgm"=ldfile),envir=.GlobalEnv)
  } else stop(paste0(ldfile," does not exist."))
  invisible(ldfile)                   
}
