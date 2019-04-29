#Función ARE1 para Sentinel 2
ARE1sentinel <- function(Red, RedEdge1, RedEdge2){
  ARE1Index <- overlay(Red, RedEdge1, RedEdge2, fun=function(x,y,z){
    a <- (sqrt((0.705-0.665)^2+(y-x)^2))
    b <- (sqrt((0.740-0.705)^2+(z-y)^2))
    c <- (sqrt((0.740-0.665)^2+(z-x)^2))
    d <- (a^2+b^2-c^2)/(2*a*b)
    d2 <- ifelse(d > 1, 1, ifelse(d < -1, -1, d))
    ind <- acos(d2)
    return(ind)
  })
  return(ARE1Index)
}
