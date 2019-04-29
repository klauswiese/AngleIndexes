#Función SASI para MODIS
SASImodis <- function(NIR, SWIR, SWIR2){
  betaSWIR <- overlay(NIR, SWIR, SWIR2, fun=function(x,y,z){
    a <- (sqrt((1.240-0.858)^2+(y-x)^2))
    b <- (sqrt((1.240-1.640)^2+(y-z)^2))
    c <- (sqrt((1.640-0.858)^2+(z-x)^2))
    d <- (a^2+b^2-c^2)/(2*a*b)
    d2 <- ifelse(d > 1, 1, ifelse(d < -1, -1, d))
    ind <- acos(d2)
    return(ind)
  })
  
  Slope <- overlay(NIR, SWIR2, fun=function(x,y){y-x})
  
  SASI <- overlay(betaSWIR, Slope, fun=function(x,y){x*y})
  
  return(SASI)
}
