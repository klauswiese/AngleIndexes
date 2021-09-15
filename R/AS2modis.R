#Función AS1 para MODIS
AS2modis <- function(SWIR, SWIR2, SWIR3){
  AS2Index <- overlay(SWIR, SWIR2, SWIR3, fun=function(x,y,z){
    a <- (sqrt((1.640-1.240)^2+(y-x)^2))
    b <- (sqrt((2.130-1.640)^2+(y-z)^2))
    c <- (sqrt((2.130-1.240)^2+(z-x)^2))
    d <- (a^2+b^2-c^2)/(2*a*b)
    d2 <- ifelse(d > 1, 1, ifelse(d < -1, -1, d))#revisar
    ind <- acos(d2)
    return(ind)
  })
  return(AS2Index)
}
