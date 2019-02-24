#Función AR para Sentinel 2
ARsentinel <- function(Green, Red, NIR){
  ARIndex <- overlay(Green, Red, NIR, fun=function(x,y,z){
    a <- (sqrt((0.664-0.560)^2+(y-x)^2))
    b <- (sqrt((0.835-0.664)^2+(z-y)^2))
    c <- (sqrt((0.835-0.560)^2+(z-x)^2))
    ind <- acos((a^2+b^2-c^2)/(2*a*b))
    return(ind)
  })
  return(ARIndex)
}