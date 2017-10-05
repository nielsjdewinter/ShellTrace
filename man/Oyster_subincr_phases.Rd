\name{Oyster_subincr_phases}
\alias{Oyster_subincr_phases}
\title{Calculates proportion of phases in each sub-increment}
\description{Function that takes the matrix of sub-increment positions and the
   matrix of phases and calculates the amount of pixels of each phase that is
     contained in each sub-increment based on pixelsize and phase characteristics.
      de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_subincr_phases(IncG, phasemat, pixelsize, phases)
}
\arguments{
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated sub-increments}
  \item{phasemat}{matrix of phases of each pixel sorted by X- and Y-coordinate
     of the pixel}
  \item{pixelsize}{size of pixels in the phase map in millimeter}
  \item{phases}{Matrix containing colour and trace element data of the phases
     in the XRF phase map}
}
\details{For every sub-increment, all pixels that cover area in the sub-increment
   are identified based on the pixel size of the phase map and the X- and
     Y-positions of the sub-increments. The amount of pixels of each phase in the
       sub-increment is saved in a new matrix of phases per sub-increment}
\value{
A matrix of the amount of pixels for each phase found in every sub-increment
\item{phase_mat}{Matrix of amounts of pixels of each phase per sub-increment}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}
\seealso{"Oyster_Suby_av_thickness"}

\examples{
  \dontrun{
    phase_mat<-Oyster_subincr_phases(IncG, phasemat, pixelsize, O1_phase)
  }
}
