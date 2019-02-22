#Función ARE2 para Sentinel 2
ARE2sentinel <- function(RedEdge1, RedEdge2, RedEdge3){
  ARE2Index <- overlay(RedEdge1, RedEdge2, RedEdge3, fun=function(x,y,z){
    a <- (sqrt((740-705)^2+(y-x)^2))
    b <- (sqrt((783-740)^2+(z-y)^2))
    c <- (sqrt((783-705)^2+(z-x)^2))
    ind <- acos((a^2+b^2-c^2)/(2*a*b))
    return(ind)
  })
  return(ARE2Index)
}
