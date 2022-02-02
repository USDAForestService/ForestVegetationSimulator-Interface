all: data/prms.RData fvsOLdevMadeTag 

data/prms.RData: parms/*
	Rscript parms/mkpkeys.R

fvsOLdevMadeTag: makefile DESCRIPTION R/* inst/extdata/* inst/extdata/www/* data/*
	cd .. && Rscript --default-packages=devtools -e "devtools::document(pkg='fvsOLdev')"
	cd .. && Rscript --default-packages=devtools -e "devtools::build(pkg='fvsOLdev')"
	cd .. && Rscript --default-packages=devtools -e "devtools::install(pkg='fvsOLdev',type='source',repos=NULL)"
	touch fvsOLdevMadeTag
                 
clean:
	rm data/prms.RData fvsOLdevMadeTag                                                                               
	
