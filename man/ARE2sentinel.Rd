\name{ARE2sentinel}
\alias{ARE2sentinel}

\title{
  Angle on Red Edge 2 for Sentinel 2
}
\description{
  Calculate the angle on red edge 2 relate with the value of red edge 1 and red edge 3}
\usage{
  ANIRsentinel(RedEdge1, RedEdge2, RedEdge3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RedEdge1}{Raster of red edge 1 band from Sentinel 2}
  \item{RedEdge2}{Raster of red edge 2 band from Sentinel 2}
  \item{RedEdge3}{Raster of red edge 3 band from Sentinel 2}
}

\value{Angle formed in the triangle formed by RedEdge1 - RedEdge2 - RedEdge3 in the spectral curve}

\author{Klaus Wolfgang Wiese Acosta}


\examples{
  #load bands
    RedEdge1 <- raster("B5Sentinel.tif")
  RedEdge2 <- raster("B6Sentinel.tif")
  RedEdge3 <- raster("B7Sentinel.tif")
  #excute command
  ARE2sentinel(RedEdge1 = RedEdge1, RedEdge2 = RedEdge2, RedEdge3 = RedEdge3)
  
}

