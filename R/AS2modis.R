#Función AS1 para MODIS
AS2modis <- function(SWIR, SWIR2, SWIR3){
  AS2Index <- overlay(SWIR, SWIR2, SWIR3, fun=function(x,y,z){
    a <- (sqrt((1640-1240)^2+(y-x)^2))
    b <- (sqrt((2130-1640)^2+(y-z)^2))
    c <- (sqrt((2130-1240)^2+(z-x)^2))
    indice <- acos((a^2+b^2-c^2)/(2*a*b))
    return(indice)
  })
  return(AS2Index)
}
