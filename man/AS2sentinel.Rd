\name{AS2sentinel}
\alias{AS2sentinel}
\title{
Angle on SWIR 2 for Sentinel 2
}
\description{
Calculate the angle on SWIR 2 value relate with the value of SWIR 2 and SWIR 3}
\usage{
AS2sentinel(SWIR, SWIR2, SWIR3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SWIR}{Raster of SWIR 1 band from MODIS}
  \item{SWIR2}{Raster of SWIR 2 band from MODIS}
  \item{SWIR3}{Raster of SWIR 3 band from MODIS}
}

\value{Angle formed in the triangle formed by SWIR - SWIR2 - SWIR3 in the spectral curve}
\references{Khanna, S.; Palacios-Orueta, A.; Whiting, M. L.; Ustin, S. L.; Riaño, D. & Litago, J. Development of angle indexes for soil moisture estimation, dry matter detection and land-cover discrimination Remote Sensing of Environment, 2007, 109, 154-165}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#load bands
SWIR <- raster("B10sentinel.tif")
SWIR2 <- raster("B11sentinel.tif")
SWIR3 <- raster("B12sentinel.tif")

#excute command
AS2sentinel(SWIR = SWIR, SWIR2 = SWIR2, SWIR3 = SWIR3)

}

