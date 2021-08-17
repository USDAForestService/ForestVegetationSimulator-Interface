all: fvsOLmadeTag rFVSmadeTag

fvsOL/data/prms.RData: $(shell find fvsOL/parms)
	R -e source\(\"fvsOL/parms/mkpkeys.R\"\)

fvsOLmadeTag: $(shell find fvsOL | grep -F \. | grep  -v -F /\. | grep  -v -F man/)
	R -e devtools::document\(pkg=\"fvsOL\"\)
	R -e devtools::build\(pkg=\"fvsOL\"\)
	R -e devtools::install\(pkg=\"fvsOL\",type=\"source\",dependencies=TRUE,repos=NULL\)
	touch fvsOLmadeTag

rFVSmadeTag: $(shell find rFVS | grep -F \. | grep  -v -F /\. | grep  -v -F man/)
	R -e devtools::document\(pkg=\"rFVS\"\)
	R -e devtools::build\(pkg=\"rFVS\"\)
	R -e devtools::install\(pkg=\"rFVS\",type=\"source\",dependencies=TRUE,repos=NULL\)
	touch rFVSmadeTag
                                                                          
