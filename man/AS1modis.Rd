\name{AS1modis}
\alias{AS1modis}

\title{
Angle on SWIR 1 for MODIS
}
\description{
Calculate the angle on SWIR 1 value relate with the value of SWIR 2 and near infrared}
\usage{
AS1modis(NIR, SWIR, SWIR2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NIR}{Raster of near infrared band from MODIS}
  \item{SWIR}{Raster of SWIR 1 band from MODIS}
  \item{SWIR2}{Raster of SWIR 2 band from MODIS}
}

\value{Angle formed in the triangle formed by NIR - SWIR - SWIR2 in the spectral curve}
\references{Khanna, S.; Palacios-Orueta, A.; Whiting, M. L.; Ustin, S. L.; Riaño, D. & Litago, J. Development of angle indexes for soil moisture estimation, dry matter detection and land-cover discrimination Remote Sensing of Environment, 2007, 109, 154-165}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#load bands
NIR <- raster("B4MODIS.tif")
SWIR <- raster("B5MODIS.tif")
SWIR2 <- raster("B5MODIS.tif")

#excute command
AS1modis(NIR = NIR, SWIR = SWIR, SWIR2 = SWIR2)

}

