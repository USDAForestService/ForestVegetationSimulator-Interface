sdir = "../R"
for (rf in dir (sdir)) source (paste(sdir,rf,sep="/"))

# load the FVS library
fvsLoad("FVSie")

# specify the keyword file

fvsSetCmdLine("--keywordfile=iet01.key")

# define a function that will be called after the first call to the Event
# Monitor. This function returns a data frame that contains the dbh, ht, and 
# species of each tree. It takes one argument which is a vector of years 
# corresponding to the years the data frame is desired. NULL is returned
# in all other years. 

fetchTrees <- function (captureYears) 
{
  curYear <- fvsGetEventMonitorVariables("year")
   
  if (is.na(match(curYear,captureYears))) NULL else 
      fvsGetTreeAttrs(c("dbh","ht","species","id"))  
}

# run the simulation and capture the data. Note that the first argument
# is a string of R code that is evaluated at each stop point and the
# second is a function that contains no arguments. 

output <- fvsInteractRun(AfterEM1="fetchTrees(c(2020,2070))",
                         SimEnd=fvsGetSummary)


# separate the tree list information from the summary statistics so that 
# the following code is easier to follow.

treeLists    <- output[-grep("SimEnd",names(output))]
summaryStats <- output[ grep("SimEnd",names(output))]

# find global values for axes limits. Note that the default name
# given to the value returned from  is "AfterEM1"
xlim <- range(unlist(lapply(treeLists,function (x) range(x[["AfterEM1"]][,"dbh"]))))
ylim <- range(unlist(lapply(treeLists,function (x) range(x[["AfterEM1"]][,"ht"]))))

# there are 6 plots, arrange them in 3 rows, 2 cols 
# for saving plot: 
# png(file="rFVS_ex2A.png", pointsize=8,height=3.5, width=2.5, units="in", res=200) 
 X11(height=3.5,width=2.5,pointsize=8)
par(mar=c(3,3.8,3,1)+.1)
layout(mat=matrix(1:6,3,2,byrow=TRUE))

for (caseID in names(treeLists))
{
  atts <- treeLists[[caseID]][["AfterEM1"]]
  plot(atts[,c("dbh","ht")],col=atts[,"species"],xlim=xlim,ylim=ylim,main=caseID,
       xlab="D.B.H. (inches)",ylab="Height (feet)")
}
# dev.off() #for saving plot

# set up the summary statistics tables for plotting
sumToPlot <- lapply(summaryStats,fvsSetupSummary)

# find global range
yrRange    <- range(unlist(lapply(sumToPlot,function (x) range(x[,"Year"]))))
tcuftRange <- range(unlist(lapply(sumToPlot,function (x) range(x[,"TPrdTCuFt"]))))

# for saving plot: 
# png(file="rFVS_ex2B.png",pointsize=8,height=1.5,width=4,units="in",res=200)
 X11(height=1.5,width=4,pointsize=8)
par(mar=c(3,3.8,3,1)+.1,mfcol=c(1,3))

for (caseID in names(sumToPlot))
{
  mainLab <- sub(":SimEnd","",caseID) #remove SimEnd for a plot title.
  plot(TPrdTCuFt ~ Year, data=sumToPlot[[caseID]], xlim=yrRange, ylim=tcuftRange, 
       type="b",main=mainLab,col="green",ylab="Total cubic volume per acre")
  lines(TCuFt    ~ Year, data=sumToPlot[[caseID]], xlim=yrRange, ylim=tcuftRange, 
       type="b",col="red")
}
#for saving plot: 
# dev.off()


