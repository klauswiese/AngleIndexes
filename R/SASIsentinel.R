#Función SASI para Sentinel
SASIsentinel <- function(NIR, SWIR, SWIR2){
  betaSWIR <- overlay(NIR, SWIR, SWIR2, fun=function(x,y,z){
    a <- (sqrt((1373.5-835.1)^2+(y-x)^2))
    b <- (sqrt((1373.5-1613.7)^2+(y-z)^2))
    c <- (sqrt((1613.7-835.1)^2+(z-x)^2))
    indice <- acos((a^2+b^2-c^2)/(2*a*b))
    return(indice)
  })
  
  Slope <- overlay(NIR, SWIR2, fun=function(x,y){y-x})
  
  SASI <- overlay(betaSWIR, Slope, fun=function(x,y){x*y})
  
  return(SASI)
}
