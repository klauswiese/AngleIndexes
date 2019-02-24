#Función ARE3 para Sentinel 2
ARE3sentinel <- function(RedEdge2, RedEdge3, NIR){
  ARE3Index <- overlay(RedEdge2, RedEdge3, NIR, fun=function(x,y,z){
    a <- (sqrt((0.783-0.740)^2+(y-x)^2))
    b <- (sqrt((0.842-0.783)^2+(z-y)^2))
    c <- (sqrt((0.842-0.740)^2+(z-x)^2))
    ind <- acos((a^2+b^2-c^2)/(2*a*b))
    return(ind)
  })
  return(ARE3Index)
}
