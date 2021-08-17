\name{ANIRmodis}
\alias{ANIRmodis}

\title{
Angle on near infrared for MODIS
}
\description{
Calculate the angle on near infrared relate with the value of red and swir}
\usage{
ANIRmodis(Red, NIR, SWIR)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Red}{Raster of red band from MODIS}
  \item{NIR}{Raster of near infrared band from MODIS}
  \item{SWIR}{Raster of SWIR band from MODIS}
}

\value{Angle formed in the triangle formed by Red - NIR - SWIR in the spectral curve}
\references{Khanna, S.; Palacios-Orueta, A.; Whiting, M. L.; Ustin, S. L.; Riaño, D. & Litago, J. Development of angle indexes for soil moisture estimation, dry matter detection and land-cover discrimination Remote Sensing of Environment, 2007, 109, 154-165}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#load bands
Red <- raster("MODISB3.tif")
NIR <- raster("MODISB4.tif")
SWIR <- raster("MODISB5.tif")

#excute command
ANIRmodis(Red = Red, NIR = NIR, SWIR = SWIR)

}

