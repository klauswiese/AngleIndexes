#Función ANIR para Sentinel 2
ANIRsentinel <- function(Red, NIR, SWIR){
  ANIRIndex <- overlay(Red, NIR, SWIR, fun=function(x,y,z){
    a <- (sqrt((835.1-664.5)^2+(y-x)^2))
    b <- (sqrt((1373.5-835.1)^2+(z-y)^2))
    c <- (sqrt((1373.5-664.5)^2+(z-x)^2))
    ind <- acos((a^2+b^2-c^2)/(2*a*b))
    return(ind)
  })
  return(ANIRIndex)
}
