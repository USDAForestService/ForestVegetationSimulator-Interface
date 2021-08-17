all: fvsOL/data/prms.RData fvsOLmadeTag rFVSmadeTag 

fvsOL/data/prms.RData: fvsOL/parms/*
	rm fvsOLmadeTag 
	Rscript fvsOL/parms/mkpkeys.R

fvsOLmadeTag: fvsOL/DESCRIPTION fvsOL/R/* fvsOL/inst/extdata/* fvsOL/inst/extdata/www/* fvsOL/data/*
	R -e require\(devtools\)\;devtools::document\(pkg=\"fvsOL\"\)
	R -e require\(devtools\)\;devtools::build\(pkg=\"fvsOL\"\)                                                  
	R -e require\(devtools\)\;devtools::install\(pkg=\"fvsOL\",type=\"source\",dependencies=TRUE,repos=NULL\)
	touch fvsOLmadeTag
                 
rFVSmadeTag: rFVS/R/* rFVS/DESCRIPTION
	R -e require\(devtools\)\;devtools::document\(pkg=\"rFVS\"\)
	R -e require\(devtools\)\;devtools::build\(pkg=\"rFVS\"\)
	R -e require\(devtools\)\;devtools::install\(pkg=\"rFVS\",type=\"source\",dependencies=TRUE,repos=NULL\)
	touch rFVSmadeTag
                                                                          
clean:
	rm fvsOL/data/prms.RData fvsOLmadeTag rFVSmadeTag                                                                                   