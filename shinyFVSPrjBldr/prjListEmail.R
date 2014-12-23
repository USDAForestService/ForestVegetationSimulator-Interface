prjListEmail <- function (queryEmail,ndays=60,sendEmail=TRUE)
{
  ## This funciton needs to be customized a noted below!
  
  # Send an Email of the projects associated with an email address 
  # to the email address. The Email address must be an exact match to 
  # the one sepecified when the project was created. 

  if (missing(queryEmail)) stop("queryEmail must be specified")
  # insure a single token, no blanks
  queryEmail = scan(text=queryEmail,what="character",quiet=TRUE)
  if (length(queryEmail) > 1) stop ("queryEmail string contains white space")
  
  workDirs = list.dirs("../FVSwork",recursive = FALSE)
  ids = lapply(workDirs,function (x) 
     {  
       fn = paste0(x,"/projectId.txt")
       id = NULL
       if (file.exists(fn)) 
       {
         id = scan(file=fn,what="character",
               sep="\n",quiet=TRUE) 
         if (!is.null(id)) 
         { 
           info = file.info(x)
           attr(id,"ctime") = info[1,"ctime"]
           info = file.info(fn)
           attr(id,"mtime") = info[1,"mtime"]
         }
       }
       id
     })
##The following line needs to be edited and uncommented to replace the
##base web address string in the substitution
#  names (ids) = sub("../FVSwork",
#                    "http://forest.moscowfsl.wsu.edu:3838/FVSwork",workDirs)
  
  rptFile = tempfile()
  con = file(rptFile,"w")
  
  cat (file=con,"\n Projects and links for Email:",queryEmail,"\n")
  nprjs = 0
  for (i in 1:length(ids))
  {
    id = unlist(ids[i])
    nam = names(ids[i])
    if (is.null(id)) next
    email = scan(text=id[1],what="character",quiet=TRUE)[2]
    if (email == queryEmail)
    {  
      nprjs = nprjs+1
      cat (file=con,"\n",id[1],"\n",id[2],"\n")
      tt = format(attr(ids[i][[1]],"ctime"),usetz=TRUE)
      cat (file=con," created at   = ",tt,"\n")
      tt = format(attr(ids[i][[1]],"mtime"),usetz=TRUE)
      cat (file=con," last modified= ",tt,"\n")
      tt = format(attr(ids[i][[1]],"mtime")+(86400*ndays),usetz=TRUE)#86400=seconds/day
      cat (file=con," auto removal = ",tt,"\n")
      cat (file=con," project link = ",nam,"\n")
    }
  }
  
  if (nprjs == 0) cat (file=con,"\n There are no projects under this Email address.\n")
  if (nprjs == 1) cat (file=con,"\n There is one project under this Email address.\n")
  if (nprjs  > 1) cat (file=con,"\n There are",nprjs,
                       "projects under this Email address.\n")
  close(con)

##Edit and uncomment the mailCmd as necessary for a given installation  
#  mailCmd = paste('mailx -t',queryEmail,'-a "From: FVSOnline"',
#   '-a "Subject: Active projects"',
#   '-a "Reply-To: Nicholas Crookston <ncrookston.fs@gmail.com>"',
#   '-a "Cc: ncrookston.fs@gmail.com" < ',rptFile)
  
  if (sendEmail) system (mailCmd) else system(paste("cat",rptFile))
  unlink (rptFile)
  nprjs
}

