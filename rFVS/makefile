all: rFVSmadeTag 

rFVSmadeTag: makefile DESCRIPTION R/* 
	cd .. && Rscript --default-packages=devtools -e "devtools::document(pkg='rFVS')"
	cd .. && Rscript --default-packages=devtools -e "devtools::build(pkg='rFVS')"
	# We don't install system wide because we want to use it with renv in FCAT
	# cd .. && Rscript --default-packages=devtools -e "devtools::install(pkg='rFVS',type='source',repos=NULL)"
	touch rFVSmadeTag
                                                                          
clean:
	rm rFVSmadeTag                                                                                   
	
