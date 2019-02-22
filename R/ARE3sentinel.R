#Función ARE3 para Sentinel 2
ARE3sentinel <- function(RedEdge2, RedEdge3, NIR){
  ARE3Index <- overlay(RedEdge2, RedEdge3, NIR, fun=function(x,y,z){
    a <- (sqrt((783-740)^2+(y-x)^2))
    b <- (sqrt((842-783)^2+(z-y)^2))
    c <- (sqrt((842-740)^2+(z-x)^2))
    ind <- acos((a^2+b^2-c^2)/(2*a*b))
    return(ind)
  })
  return(ARE3Index)
}
