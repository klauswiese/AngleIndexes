\name{ARsentinel}
\alias{ARsentinel}
\title{
Angle on Red for Sentinel 2
}
\description{
Calculate the angle on Red value relate with the value of green and near infrared}
\usage{
ARsentinel(Green, Red, NIR)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Green}{Raster of green band from Sentinel 2}
  \item{Red}{Raster of Red band from Sentinel 2}
  \item{NIR}{Raster of near infrared band from Sentinel 2}
}

\value{Angle formed in the triangle formed by Green - Red - NIR in the spectral curve}
\references{Khanna, S.; Palacios-Orueta, A.; Whiting, M. L.; Ustin, S. L.; Riaño, D. & Litago, J. Development of angle indexes for soil moisture estimation, dry matter detection and land-cover discrimination Remote Sensing of Environment, 2007, 109, 154-165}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#load bands
Green <- raster("SentinelB3.tif")
Red <- raster("SentinelB4.tif")
NIR <- raster("SentinelB8.tif")

#execute command
ARsentinel(Green = Green, Red = Red, NIR = NIR)

}

