\name{Oyster_phase_matrix_BMP}
\alias{Oyster_phase_matrix_BMP}
\title{Function that calculates phase matrix}
\description{Function that calculates a matrix of phase numbers for every pixel
   in the imported BMP file.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_phase_matrix_BMP(BMP, phases)
}
\arguments{
  \item{BMP}{"BMP" file representing the BMP image in R session}
  \item{phases}{Matrix containing colour, density and trace element data of the
    phases in the XRF phase map}
}
\details{This function compares the colour data from the "BMP" object with
   colour codes of phases in "phases" table to assign a phase to every pixel
     in the BMP}
\value{
\item{phasemat}{matrix of phases of each pixel sorted by X- and Y-coordinate
   of the pixel}
}
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
    phasemat<-Oyster_phase_matrix_BMP(BMP, O1_phase)
  }
}
