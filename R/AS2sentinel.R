#Función AS2 para Sentinel 2
AS2sentinel <- function(SWIR, SWIR2, SWIR3){
  AS2Index <- overlay(SWIR, SWIR2, SWIR3, fun=function(x,y,z){
    a <- (sqrt((1612.7-1373.7)^2+(y-x)^2))
    b <- (sqrt((2202-1612.7)^2+(y-z)^2))
    c <- (sqrt((2202-1373.7)^2+(z-x)^2))
    indice <- acos((a^2+b^2-c^2)/(2*a*b))
    return(indice)
  })
  return(AS2Index)
}
