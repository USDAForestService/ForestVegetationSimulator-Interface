rdparms <- function(pdir="parms") 
{
#  options(warn=2)
  prms = list()
  files = dir(pdir)
  for (fn in files)
  {
    if (fn %in% c("mods.txt","mkprm","ppekeys.kwd","tutorial.prm")) next
    file = paste0(pdir,"/",fn)
    cat ("processing file=",file,"\n")    
    raw  = scan(file=file,sep="\n",what="character",
                blank.lines.skip = FALSE,quiet=TRUE)
    strs = grep("^//start",raw)
    if (length(strs))
    {
      for (i in 1:length(strs))
      {
        p = strs[i]
        n = scan(text=raw[p],what="character",quiet=TRUE)[2]
        p = p+1
        e = if (i == length(strs)) length(raw) else strs[i+1]-1
        sec = raw[p:e]
        e = grep("^//end",sec)
        if (!is.na(e)) sec=sec[1:(e-1)]
        prms[[n]] = paste(sec,collapse="\n")
      }
    }
  }
  prms
}

mkpkeys = function (mstext)
{
  # maybe this one is already parsed!
  if (class(mstext)=="list") return(mstext)
  cat("mstext=",names(get("X",envir=parent.frame()))[get("i",envir=parent.frame())],"\n") 
  pkeys = list()
  pkeqs  = list() # pkeys that are "equal" other pkeys.
  state = 1
  buffer = ""
  lineNumber=1
  for (i in 1:nchar(mstext))
  {
    thechar = substr(mstext,i,i)
    if (thechar == '\n') lineNumber=lineNumber+1
##  if (state!= 15) cat ("state=",state," lineNumber=",lineNumber," thechar=",thechar,"\n")
    if (state == 1)
    {
      # looking for the beginning of a pkey or a
      # comment (starts with !, ends with newline)

      if (thechar == " " || thechar == "\t" || thechar == "\n") next
   
      if(thechar == "!")     # we have a comment.
      {
        state = 15
      } 
      else
      {
        state=2
        buffer = paste0(buffer,thechar)
      }
      next
    }
    else if (state == 2)  # looking for the end of a pkey.
    {
      # if a printing char but not { : = the char is part of pkey.
    
      if (thechar != '{' && thechar != '=' && 
          thechar != ':' && thechar != ' ')
      {
         buffer = paste0(buffer,thechar)         
         next
      }
    
      # otherwise, we found the end of the pkey...save the buffer as pkey
      # unless it has zero length.
  
      if (nchar(buffer) == 0)
      {
         state = 1
         next
      }
## cat("end of pkey, buffer=",buffer,"\n")
      pkey=buffer
      buffer = ""
      ns = switch (thechar,
          "{" = 4,
          ":" = 7,
          "=" = 11,
          " " = 3,
          99)
      
      if (ns == 99) 
      {
        cat("Error: expecting { : = and found ", thechar,
          " mstext=",mstext," line=",lineNumber," state=",state,"\n")
## stop("error")
        state=1
      } else state = ns
      next
    }
    else if (state == 3) # looking for beginning of ATList or Pstring
    {
      if (thechar == ' ' || thechar == "\t" || thechar == "\n") next;
## cat ("thechar1=",thechar," buffer=",buffer," pkey=",pkey,"\n")  
      ns = switch (thechar,
          "{" = 4,
          ":" = 7,
          "=" = 11,
          99)
      
      if (ns == 99) 
      {
        cat("Error: expecting { : = and found ", thechar,
            " nearby=",substr(mstext,max(1,i-20),min(i+20,nchar(mstext))),
            " line=",lineNumber," state=",state,"\n")
## stop("error")
        state=1
      } else state = ns
      next
    }
    else if (state == 4) # looking for the end of the ATList.
    { 
      if (thechar == "\n") next
      if (thechar == "\t") thechar=" "
      if (thechar != '}')
      {
         state = 5;
         buffer = paste0(buffer,thechar)
      }
      else if (thechar == '}') # we found the end of the atlist...before is started!
      {
         atlist = ""
         buffer = ""
         state = 6
      }
      next
    }  
    else if (state == 5)  # looking for the end of the ATList.
    {
      if (thechar == '}') # we found the end of the atlist.
      {
         attr(pkey,"atlist") = scan (text=buffer,what=" ",quiet=TRUE)
         buffer = ""
         state = 6;
      }
      else
      {
         buffer = paste0(buffer,thechar)
      }
      next
    }   
    else if (state == 6) # looking for : =
    {
      if (thechar == ' ' || thechar == "\t" || thechar == "\n") next;
      ns = switch (thechar,
          ":" = 7,
          "=" = 11,
          99)
      if (ns == 99)
      {
        cat("Error: expecting : = and found ", thechar,
            " nearby=",substr(mstext,max(1,i-20),min(i+20,nchar(mstext))),
            " line=",lineNumber," state=",state,"\n")
## stop("error")
        state=1
      } else state = ns
      next
    }
    else if (state == 7) # looking for the beginning of the pstring.
    {
      if (thechar == ' ' || thechar == "\t" || thechar == "\n") next; 
      if (thechar == '{')
      {
         state = 9
         buffer = ""
         next
      } 
      else
      {
         state = 8;
         buffer = thechar
      }
      next
    }
    else if (state == 8)  # looking for the end of the single-token pstring.
    {
      if (thechar != ' ' && thechar != "\t" && thechar != "\n" &&
          i != nchar(mstext))
      {
        buffer = paste0(buffer,thechar)
      } 
      else
      {
## cat ("state=",state," buffer=",buffer,"\n")
        attr(pkey,"pstring") = buffer
        pkeys[[length(pkeys)+1]] = pkey
        buffer=""
        state = 1
      }
      next
    }
    else if (state == 9) # looking for the end of the pstring...keep all blanks.
                         # use the backslash char to escape the following char.
    {      
      if (thechar == "\\") 
      {
        state=10
      } 
      else if (thechar == '}') # we found the end of the pstring.
      {
## cat ("state=",state," buffer=",buffer,"\n")
        attr(pkey,"pstring") = buffer
        pkeys[[length(pkeys)+1]] = pkey
        buffer = ""
        state = 1
      } else
      {
         buffer = paste0(buffer,thechar)
      }
      next
    }
    else if (state == 10) # last char was a backslash
    {
      if (thechar == 'n') # we have a new line requested.
      {
         buffer = paste0(buffer,'\n')
      } 
      else if (thechar == 't') # we have a horizontal tab requesed.
      {
         buffer = paste0(buffer,'\t')
      } 
      else if (thechar != '\n') # if the char is not newline, save it.
      {
         buffer = paste0(buffer,thechar)
      }                    # otherwise it will be skipped.  
      state=9
      next
    } 
    else if (state == 11) # found equal sign, looking for pkey_r
    {
      if (thechar == ' ' || thechar == "\t" || thechar == "\n") next; 
      state=12
      buffer = paste0(buffer,thechar);
      next
    } 
    else if (state == 12) # looking for the end of the pkey_r
    {
## cat ("state=",state," buffer=",buffer,"\n")
      if (thechar == ' ' || thechar == "\t") next
      if (thechar == "\n"|| i == nchar(mstext))
      {  
        pkeqs[[length(pkeqs)+1]] = paste(pkey,buffer)
        buffer = ""
        state=1
        next
      } 
      else if (thechar == '{') 
      {
        thechar = " "
        state = 13
      }
      buffer = paste0(buffer,thechar)
      next
    } 

    else if (state == 13) # looking for the end of the atlist for the pkey_r
    {
## cat ("state=",state," buffer=",buffer," pkey=",pkey,"\n")
      if (thechar == "\n" || thechar == "\t") thechar == " "
      if (thechar == '}')
      {
        pkeqs[[length(pkeqs)+1]] = paste(pkey,buffer)
        buffer = ""
        state = 1
      }
      else
      {
        buffer = paste0(buffer,thechar);
      }
      next
    } 

    else if (state == 15)  # looking for the end of a comment.
    {
      if (thechar == '\n' || i == nchar(mstext)) state = 1
      next
    }
  }
  if (length(pkeqs) > 0)
  {
    for (pkeq in pkeqs)
    {
      toks = scan (text=pkeq,what=" ",quiet=TRUE)
      pkey = toks[1]; pkr = toks[2]
      atl = if (length(toks) > 2) toks[3:length(toks)] else NULL
      for (i in 1:length(pkeys))
      {
        if (pkr == pkeys[[i]])
        {
          if ((is.null(atl) && is.null(attr(pkeys[[i]],"atlist"))) ||
              length(intersect(atl,attr(pkeys[[i]],"atlist"))) > 0)
          {
            attributes(pkey) = attributes(pkeys[[i]])
            pkeys[[length(pkeys)+1]] = pkey
            break
          }
        }
      }   
    } 
  }
  pkeys
}
 
prms = rdparms(pdir="~/open-suppose/trunk/parms")
prms = lapply(prms,mkpkeys)
save(file="prms.RData",prms)




