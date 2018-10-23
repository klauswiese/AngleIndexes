\name{AS1sentinel}
\alias{AS1sentinel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Angle on SWIR 1 for Sentinel 2
}
\description{
Calculate the angle on SWIR 1 value relate with the value of SWIR 2 and near infrared}
\usage{
AS1sentinel(NIR, SWIR, SWIR2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NIR}{Raster of near infrared band from Sentinel 2}
  \item{SWIR}{Raster of SWIR 1 band from Sentinel 2}
  \item{SWIR2}{Raster of SWIR 2 band from Sentinel 2}
}

\value{Angle formed in the triangle formed by NIR - SWIR - SWIR2 in the spectral curve}
\references{Khanna, S.; Palacios-Orueta, A.; Whiting, M. L.; Ustin, S. L.; Riaño, D. & Litago, J. Development of angle indexes for soil moisture estimation, dry matter detection and land-cover discrimination Remote Sensing of Environment, 2007, 109, 154-165}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#load bands, need to resample from original image, NIR = 10m, B10 = 60m, B11 = 20m
NIR <- raster("B8Sentinel.tif")
SWIR <- raster("B10Sentinel.tif")
SWIR2 <- raster("B11Sentinel.tif")

#excute command
AS1sentinel(NIR = NIR, SWIR = SWIR, SWIR2 = SWIR2)

}

