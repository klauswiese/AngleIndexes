#Función SASI para MODIS
SASImodis <- function(NIR, SWIR, SWIR2){
  betaSWIR <- overlay(NIR, SWIR, SWIR2, fun=function(x,y,z){
    a <- (sqrt((1240-858)^2+(y-x)^2))
    b <- (sqrt((1240-1640)^2+(y-z)^2))
    c <- (sqrt((1640-858)^2+(z-x)^2))
    indice <- acos((a^2+b^2-c^2)/(2*a*b))
    return(indice)
  })
  
  Slope <- overlay(NIR, SWIR2, fun=function(x,y){y-x})
  
  SASI <- overlay(betaSWIR, Slope, fun=function(x,y){x*y})
  
  return(SASI)
}