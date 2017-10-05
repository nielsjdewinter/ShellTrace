\name{Oyster_phase_stats}
\alias{Oyster_phase_stats}
\title{Function that exports phase statistics}
\description{Function that searches through the matrix of phases per pixel and
   exports the statistics of representation of different phases in the map as
     well as the bulk composition of pixels in the map.
      de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_phase_stats(phasemat, phases)
}
\arguments{
  \item{phasemat}{matrix of phases of each pixel sorted by X- and Y-coordinate
     of the pixel}
  \item{phases}{Matrix containing colour and trace element data of the phases
     in the XRF phase map}
}
\details{Phase statistics are calculated by looping through the matrix of phases
   created from the XRF phase map and comparing with the "phases" statistics
     table}
\value{
\item{phase_stat}{matrix of statistics of trace elements and phases in the
   total map}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}
\seealso{"Oyster_phase_matrix_BMP"}
\source{
\href{https://github.com/nielsjdewinter/ShellTrace}{GitHub} \cr
\href{https://doi.org/10.5194/gmd-2017-137}{Manuscript} \cr
\href{https://doi.org/10.5194/gmd-2017-137-supplement}{Supplementary data} \cr
\href{http://nidewint.wixsite.com/nielsdewinter}{Author website}
}
\examples{
  phase_stat<-Oyster_phase_stats(phasemat, O1_phase)
}
