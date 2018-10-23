#Función AS1 para Sentinel 2
AS1sentinel <- function(NIR, SWIR, SWIR2){
  AS1Index <- overlay(NIR, SWIR, SWIR2, fun=function(x,y,z){
    a <- (sqrt((1373.5-835.1)^2+(y-x)^2))
    b <- (sqrt((1373.5-1613.7)^2+(y-z)^2))
    c <- (sqrt((1613.7-835.1)^2+(z-x)^2))
    indice <- acos((a^2+b^2-c^2)/(2*a*b))
    return(indice)
  })
  return(AS1Index)
}
