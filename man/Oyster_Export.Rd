\name{Oyster_Export}
\alias{Oyster_Export}
\title{Function that exports data of the growth model}
\description{Function that exports the results of the bivalve growth model as
   tables in the form of XLSX files.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_Export(subincr_matrix, name_file)
}
\arguments{
  \item{subincr_matrix}{Data frame that contains characteristics of every
     sub-increment}
  \item{name_file}{String containing the name of the file to be exported}
}
\details{Results are exported as "<name file>.xlsx?" in the working directory}

\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}
\seealso{"Oyster_av_thickness"}

\examples{
  \dontrun{
    Oyster_Export(subincr_matrix6, "test_export")
  }
}
