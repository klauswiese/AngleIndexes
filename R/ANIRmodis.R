#Función ANIR para MODIS
ANIRmodis <- function(Red, NIR, SWIR){
  ANIRIndex <- overlay(Red, NIR, SWIR, fun=function(x,y,z){
    a <- (sqrt((858-648)^2+(y-x)^2))
    b <- (sqrt((1240-858)^2+(z-y)^2))
    c <- (sqrt((1240-648)^2+(z-x)^2))
    ind <- acos((a^2+b^2-c^2)/(2*a*b))
    return(ind)
  })
  return(ANIRIndex)
}
