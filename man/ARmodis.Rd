\name{ARmodis}
\alias{ARmodis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Angle on Red for MODIS
}
\description{
Calculate the angle on Red value relate with the value of green and near infrared}
\usage{
ARmodis(Green, Red, NIR)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Green}{Raster of green band from MODIS}
  \item{Red}{Raster of Red band from MODIS}
  \item{NIR}{Raster of near infrared band from MODIS}
}

\value{Angle formed in the triangle formed by Green - Red - NIR in the spectral curve}
\references{Khanna, S.; Palacios-Orueta, A.; Whiting, M. L.; Ustin, S. L.; Riaño, D. & Litago, J. Development of angle indexes for soil moisture estimation, dry matter detection and land-cover discrimination Remote Sensing of Environment, 2007, 109, 154-165}
\author{Klaus Wolfgang Wiese Acosta}


\examples{
#load bands
Green <- raster("MODISB2.tif")
Red <- raster("MODISB3.tif")
NIR <- raster("MODISB4.tif")

#excute command
ARmodis(Green = Green, Red = Red, NIR = NIR)

}

