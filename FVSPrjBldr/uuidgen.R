uuidgen <- function (n=1)
{
# generate a version 4 uuid using R's random number generator and a
# special seed/status.
# example: "36d3054f-553b-4f52-ac3f-b1a028f3dfa8"

# designed to make it almost impossible to cause this generator
# to generate a duplicate...even when the user first calls set.seed
# and does not have package digest.

# NLCrookston, Dec 2014.

  ss <- if (exists(".Random.seed",envir=.GlobalEnv)) 
        get(".Random.seed",envir=.GlobalEnv) else NULL
  cp <- Sys.getpid()
  if (exists(".uuid.seedpid",envir=.GlobalEnv) &&
         get(".uuid.seedpid",envir=.GlobalEnv) == cp &&
      exists(".uuid.seed",   envir=.GlobalEnv))
    .Random.seed <<- get(".uuid.seed",envir=.GlobalEnv) else 
  {
    .uuid.seedpid <<- cp
    if (file.exists("/dev/random")) {
      rn <- file ("/dev/random",open="rb",raw=TRUE)
      set.seed(readBin(rn,"integer"))
      close(rn)
    } else 
    {
      if (require (digest,quietly=TRUE)) { 
        i <- as.integer(runif(1,min=1,max=32-7))
        dig <- paste0("0x",substring(digest(Sys.getenv()),i,i+6))
        dig <- strtoi(dig) * if (strtoi(substring(dig,1,3)) %% 2) 1 else -1
      } else dig <- 1
      set.seed(as.integer(Sys.time())+cp+as.integer(runif(1)*1000)+dig)
    }
  }   
  uuid <- vector("character",n)
  for (i in 1:n) {
    rnum = runif(4)
    rstr <- substring(sprintf("%.8a",rnum),5,12)
    uuid[i] <- sprintf("%s-%s-4%s%s%s-%s%s",rstr[1],substring(rstr[2],1,4),
               substring(rstr[2],5,7),
               c("-8","-9","-a","-b","-8")[as.integer(rnum[1]*4)+1],
               substring(rstr[3],1,3),substring(rstr[3],4,7),rstr[4])
  }  
  .uuid.seed <<- .Random.seed
  if (is.null(ss)) rm(.Random.seed,envir=.GlobalEnv) else .Random.seed <<- ss
  uuid
}

