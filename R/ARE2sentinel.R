#Función ARE2 para Sentinel 2
ARE2sentinel <- function(RedEdge1, RedEdge2, RedEdge3){
  ARE2Index <- overlay(RedEdge1, RedEdge2, RedEdge3, fun=function(x,y,z){
    a <- (sqrt((0.740-0.705)^2+(y-x)^2))
    b <- (sqrt((0.783-0.740)^2+(z-y)^2))
    c <- (sqrt((0.783-0.705)^2+(z-x)^2))
    d <- (a^2+b^2-c^2)/(2*a*b)
    d2 <- ifelse(d > 1, 1, ifelse(d < -1, -1, d))
    ind <- acos(d2)
    return(ind)
  })
  return(ARE2Index)
}
