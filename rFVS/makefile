all: rFVSmadeTag 

rFVSmadeTag: makefile DESCRIPTION R/* 
	cd .. && Rscript --default-packages=devtools -e "devtools::document(pkg='rFVS')"
	cd .. && Rscript --default-packages=devtools -e "devtools::build(pkg='rFVS')"
	cd .. && Rscript --default-packages=devtools -e ".libPaths('~/R-dev');devtools::install(pkg='rFVS',type='source',repos=NULL)"
	touch rFVSmadeTag
                                                                          
clean:
	rm rFVSmadeTag                                                                                   
	
