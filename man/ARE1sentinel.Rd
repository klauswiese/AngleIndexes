\name{ARE1sentinel}
\alias{ARE1sentinel}
\title{
  Angle on Red Edge 1 for Sentinel 2
}
\description{
  Calculate the angle on red edge 1 relate with the value of red and red edge 2}
\usage{
  ANIRsentinel(Red, RedEdge1, RedEdge2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Red}{Raster of red band from Sentinel 2}
  \item{RedEdge1}{Raster of red edge 1 band from Sentinel 2}
  \item{RedEdge2}{Raster of red edge 2 band from Sentinel 2}
}

\value{Angle formed in the triangle formed by Red - RedEdge1 - RedEdge2 in the spectral curve}

\author{Klaus Wolfgang Wiese Acosta}


\examples{
  #load bands
  Red <- raster("B4Sentinel.tif")
  RedEdge1 <- raster("B5Sentinel.tif")
  RedEdge2 <- raster("B6Sentinel.tif")
  
  #excute command
  ARE1sentinel(Red = Red, RedEdge1 = RedEdge1, RedEdge2 = RedEdge2)
  
}

