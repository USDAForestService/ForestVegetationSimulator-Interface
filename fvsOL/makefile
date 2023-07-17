all: data/fvsOnlineHelpRender.RData data/prms.RData fvsOLmadeTag

data/prms.RData: parms/*
	Rscript parms/mkpkeys.R

data/fvsOnlineHelpRender.RData: inst/extdata/mkhelp.R inst/extdata/fvsOnlineHelp.html inst/extdata/databaseDescription.xlsx
	Rscript inst/extdata/mkhelp.R

fvsOLmadeTag: makefile DESCRIPTION R/* inst/extdata/* inst/extdata/www/* data/*
	cd .. && Rscript --default-packages=devtools -e "devtools::document(pkg='fvsOL')"
	cd .. && Rscript --default-packages=devtools -e "devtools::build(pkg='fvsOL')"
	cd .. && Rscript --default-packages=devtools -e "devtools::install(pkg='fvsOL',type='source')"
	touch fvsOLmadeTag
                 
clean:
	rm data/prms.RData data/fvsOnlineHelpRender.RData fvsOLmadeTag
	