

#Función AR para MODIS
ARmodis <- function(Green, Red, NIR){
  ARIndex <- overlay(Green, Red, NIR, fun=function(x,y,z){
    a <- (sqrt((648-555)^2+(y-x)^2))
    b <- (sqrt((858-648)^2+(z-y)^2))
    c <- (sqrt((858-555)^2+(z-x)^2))
    ind <- acos((a^2+b^2-c^2)/(2*a*b))
    return(ind)
  })
  return(ARIndex)
}
