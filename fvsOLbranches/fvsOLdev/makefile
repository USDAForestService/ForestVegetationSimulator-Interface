all: data/prms.RData fvsOLdevMadeTag 

data/prms.RData: parms/*
	Rscript parms/mkpkeys.R
	
data/fvsOnlineHelpRender.RData: inst/extdata/mkhelp.R inst/extdata/fvsOnlineHelp.html
	Rscript inst/extdata/mkhelp.R

fvsOLdevMadeTag: makefile DESCRIPTION R/* inst/extdata/* inst/extdata/www/* data/*
	cd .. && Rscript --default-packages=devtools -e "devtools::document(pkg='fvsOLdev')"
	cd .. && Rscript --default-packages=devtools -e "devtools::build(pkg='fvsOLdev')"
	cd .. && Rscript --default-packages=devtools -e "devtools::install(pkg='fvsOLdev',type='source',repos=NULL)"
	touch fvsOLdevMadeTag
                 
clean:
	rm data/prms.RData data/fvsOnlineHelpRender.RData fvsOLdevMadeTag                                                                               
	
