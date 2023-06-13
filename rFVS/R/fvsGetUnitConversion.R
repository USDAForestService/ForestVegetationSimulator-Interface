#' Get FVS Units Conversion Factor
#'
#' @param name a character string name of the desired conversion factor, where:
#'  \tabular{cl}{
#'    CMtoIN         \tab cm to inches \cr
#'    CMtoFT         \tab cm to feet \cr
#'    MtoIN          \tab m to inches \cr
#'    MtoFT          \tab m to feet \cr
#'    KMtoMI         \tab km to miles \cr
#'    M2toFT2        \tab square m to square feet \cr
#'    HAtoACR        \tab hectares to acres \cr
#'    M3toFT3        \tab cubic meters to cubic feet \cr
#'    KGtoLB         \tab kg to pounds (lbs) \cr
#'    TMtoTI         \tab Tonnes metric (1,000 kg) to Tons Imperial (2,000 lbs) \cr
#'    CtoF1          \tab Slope in the Celsius to Fahrenheit conversion \cr
#'    CtoF2          \tab Intercept in the Celsius to Fahrenheit conversion \cr
#'    INtoCM         \tab inches to cm \cr
#'    FTtoCM         \tab feet to cm \cr
#'    INtoM          \tab inches to m \cr
#'    FTtoM          \tab feet to m \cr
#'    MItoKM         \tab miles to km \cr
#'    FT2toM2        \tab square feet to square m \cr
#'    ACRtoHA        \tab acres to hectares \cr
#'    FT3toM3        \tab cubic feet to cubic m \cr
#'    LBtoKG         \tab pounds (lbs) to kg \cr
#'    TItoTM         \tab Tons Imperial (2,000 lbs) to Tonnes metric (1,000 kg) \cr
#'    FtoC1          \tab Slope in the Fahrenheit to Celsius conversion \cr
#'    FtoC2          \tab Intercept in the Fahrenheit to Celsius conversion \cr
#'    BTUtoKJ        \tab BTU to kilojoules \cr
#'    M2pHAtoFT2pACR \tab square m per hectare to square feet per acre \cr
#'    M3pHAtoFT3pACR \tab cubic m per hectare \cr
#'    FT2pACRtoM2pHA \tab square feet per acre to square m per hectare \cr
#'    FT3pACRtoM3pHA \tab cubic feet per acre to cubic m per hectare \cr}
#' @return the numeric value, or NA if FVS does not contain the desired factor.
#' @examples
#'    #edit fvsLoad to reflect where FVSbin is stored on your system. 
#'    fvsLoad(bin="FVSbin",fvsProgram="FVSie")
#'    fvsUnitConversion("BTUtoKJ")
#'    fvsUnitConversion("M2pHAtoFT2pACR")
#'    fvsUnitConversion("M2pHAtoFT2pACR")*fvsUnitConversion("FT2pACRtoM2pHA")
#'    # [1] 0.9999998
#' @export
fvsUnitConversion <-
function(name)
{
  nch =nchar(name)
  ans = .C("CfvsUnitConversion",name,nch,as.numeric(0),as.integer(0),
        PACKAGE=get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)$pgm)
  if (ans[[4]] == 0) return(ans[[3]]) else return(NA)
}

