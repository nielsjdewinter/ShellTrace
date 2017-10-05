\name{Oyster_el_time}
\alias{Oyster_el_time}
\title{Calculate concentrations of trace elements per sub-increment}
\description{Function that takes the matrix of phase pixels per sub-increment
   together with the matrix of concentrations per phase to calculate the
     concentration of each trace element in every sub-increment.
      de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_el_time(phase_mat, phases)
}
\arguments{
  \item{phase_mat}{Matrix of amounts of pixels of each phase per sub-increment}
  \item{phases}{Matrix containing colour and trace element data of the phases
     in the XRF phase map}
}
\details{For every sub-increment, the relative contribution of phases is
   multiplied with the trace element concentrations of the phases to calculate
     the average concentration of trace elements in each sub-increment}
\value{
Matrix of trace element concentrations per sub-increment
\item{el_time}{Matrix of trace element concentrations through time}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}
\seealso{"Oyster_Volumes"}

\examples{
  el_time<-Oyster_el_time(phase_mat, O1_phase)
}
