all: fvsOL/data/prms.RData fvsOLmadeTag rFVSmadeTag 

fvsOL/data/prms.RData: fvsOL/parms/*
	Rscript fvsOL/parms/mkpkeys.R

fvsOLmadeTag: fvsOL/DESCRIPTION fvsOL/R/* fvsOL/inst/extdata/* fvsOL/inst/extdata/www/* fvsOL/data/*
	Rscript --default-packages=devtools -e 'devtools::document(pkg="fvsOL")'
	Rscript --default-packages=devtools -e 'devtools::build(pkg="fvsOL")'
	Rscript --default-packages=devtools -e 'devtools::install(pkg="fvsOL",type="source",repos=NULL)'
	touch fvsOLmadeTag
                 
rFVSmadeTag: rFVS/R/* rFVS/DESCRIPTION                         
	Rscript --default-packages=devtools -e 'devtools::document(pkg="rFVS")'
	Rscript --default-packages=devtools -e 'devtools::build(pkg="rFVS")'
	Rscript --default-packages=devtools -e 'devtools::install(pkg="rFVS",type="source",repos=NULL)'
	touch rFVSmadeTag
                                                                          
clean:
	rm fvsOL/data/prms.RData fvsOLmadeTag rFVSmadeTag                                                                                   
