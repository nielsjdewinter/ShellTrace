\name{Oyster_import_TIF}
\alias{Oyster_import_TIF}
\title{Function that imports a TIF file}
\description{Function that imports an image in TIF or TIFF format of an XRF map.
  de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_import_TIF(file_name)
}
\arguments{
  \item{file_name}{String of full name of the TIF(F) that needs to be imported,
     including extension}
}
\details{Requires "tiff" package to work}
\value{
\item{TIF}{"TIF" file representing the TIF(F) image in R session}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}
\seealso{"Oyster_av_thickness"}