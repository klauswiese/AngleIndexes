\name{ARE3sentinel}
\alias{ARE3sentinel}

\title{
  Angle on Red Edge 3 for Sentinel 2
}
\description{
  Calculate the angle on red edge 3 relate with the value of red edge 2 and NIR}
\usage{
  ANIRsentinel(RedEdge2, RedEdge3, NIR)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RedEdge2}{Raster of red edge 2 band from Sentinel 2}
  \item{RedEdge3}{Raster of red edge 3 band from Sentinel 2}
  \item{NIR}{Raster of near infrared band from Sentinel 2}
}

\value{Angle formed in the triangle formed by RedEdge2 - RedEdge3 - NIR in the spectral curve}

\author{Klaus Wolfgang Wiese Acosta}


\examples{
  #load bands
  RedEdge2 <- raster("B6Sentinel.tif")
  RedEdge3 <- raster("B7Sentinel.tif")
  NIR <- raster("B8Sentinel.tif")
  #excute command
  ARE2sentinel(RedEdge2 = RedEdge2, RedEdge3 = RedEdge3, NIR = NIR)
  
}

