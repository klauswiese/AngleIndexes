\name{ANIRsentinel}
\alias{ANIRsentinel}
\title{
Angle on near infrared for Sentinel 2
}
\description{
Calculate the angle on near infrared relate with the value of red and swir}
\usage{
ANIRsentinel(Red, NIR, SWIR)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Red}{Raster of red band from Sentinel 2}
  \item{NIR}{Raster of near infrared band from Sentinel 2}
  \item{SWIR}{Raster of SWIR band from Sentinel 2}
}

\value{Angle formed in the triangle formed by Red - NIR - SWIR in the spectral curve}
\references{Khanna, S.; Palacios-Orueta, A.; Whiting, M. L.; Ustin, S. L.; Riaño, D. & Litago, J. Development of angle indexes for soil moisture estimation, dry matter detection and land-cover discrimination Remote Sensing of Environment, 2007, 109, 154-165}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#load bands
Red <- raster("B4Sentinel.tif")
NIR <- raster("B8Sentinel.tif")
SWIR <- raster("B10Sentinel.tif")

#excute command
ANIRsentinel(Red = Red, NIR = NIR, SWIR = SWIR)

}

