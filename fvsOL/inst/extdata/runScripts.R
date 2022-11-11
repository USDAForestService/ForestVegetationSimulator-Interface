
# List custom run scripts that are available here.

# The string displayed in the selectInput list (on the Run tab) is on the left, 
# the script "name" is on the right. 


# The "name" map to scripts named: "customRun_[name].R" 
# where name is a left-hand side in the list. If a script by that 
# name (case sensitive) is not found, then the list item is removed 
# prior to the server.R script startup (that is, only "available" 
# optional scripts will be listed).

# NOTE: it is very important NOT to allow users to upload their own scripts when
# the system is running in a client/server environment. On "local" it is just
# fine!

customRunScripts=list(
  "AcadianGY (Weiskittel et al.) normally run with FVSne" = "fvsRunAcadian",
  "AdirondackGY (Weiskittel et al.) normally run with FVSne" = "fvsRunAdirondack"
  )


