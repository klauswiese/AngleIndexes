#Función ANIR para Sentinel 2
ANIRsentinel <- function(Red, NIR, SWIR){
  ANIRIndex <- overlay(Red, NIR, SWIR, fun=function(x,y,z){
    a <- (sqrt((0.835-0.6645)^2+(y-x)^2))
    b <- (sqrt((1.6137-0.835)^2+(z-y)^2))
    c <- (sqrt((1.6137-0.664)^2+(z-x)^2))
    d <- (a^2+b^2-c^2)/(2*a*b)
    d2 <- ifelse(d > 1, 1, ifelse(d < -1, -1, d))
    #arco coseno
    ind <- acos(d2)
    return(ind)
  })
  return(ANIRIndex)
}

#SWIR = 1.612
