\name{SASImodis}
\alias{SASImodis}
\title{
Shortwave Angle Slope Index for MODIS
}
\description{
Combination of NIR, SWIR1 and SWIR2 MODIS bands. SASI emulates the general shape of this part of the spectrum. To accomplish this, we evaluate a triangle with vertices at R(NIR), R(SWIR1) and R(SWIR2), where R is the reflectance at broad bands of the NIR, SWIR1 and SWIR2.}
\usage{
SASImodis(NIR, SWIR, SWIR2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NIR}{Raster of near infrared band from MODIS}
  \item{SWIR}{Raster of SWIR 1 band from MODIS}
  \item{SWIR2}{Raster of SWIR 2 band from MODIS}
}

\value{Shortwave Angle Slope Index for MODIS}
\references{Khanna, S.; Palacios-Orueta, A.; Whiting, M. L.; Ustin, S. L.; Riaño, D. & Litago, J. Development of angle indexes for soil moisture estimation, dry matter detection and land-cover discrimination Remote Sensing of Environment, 2007, 109, 154-165}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#load bands
NIR <- raster("B4MODIS.tif")
SWIR <- raster("B5MODIS.tif")
SWIR2 <- raster("B5MODIS.tif")

#excute command
SASImodis(NIR = NIR, SWIR = SWIR, SWIR2 = SWIR2)

}

