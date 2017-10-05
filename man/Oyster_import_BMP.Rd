\name{Oyster_import_BMP}
\alias{Oyster_import_BMP}
\title{Function that imports a BMP of an XRF map}
\description{Function that imports a BMP file into R.
  de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_import_BMP(file_name)
}
\arguments{
  \item{file_name}{String of full name of the BMP that needs to be imported,
     excluding the extension}
}
\details{Requires "bmp" package to work}
\value{
\item{BMP}{"BMP" file representing the BMP image in R session}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the ShellTrace
   model}
\seealso{"Oyster_av_thickness"}
\source{
\href{https://github.com/nielsjdewinter/ShellTrace}{GitHub} \cr
\href{https://doi.org/10.5194/gmd-2017-137}{Manuscript} \cr
\href{https://doi.org/10.5194/gmd-2017-137-supplement}{Supplementary data} \cr
\href{http://nidewint.wixsite.com/nielsdewinter}{Author website}
}