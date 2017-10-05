\name{Oyster_incr_area}
\alias{Oyster_incr_area}
\title{Area between shell increments}
\description{Calculates the area between this shell increment and the
       previous increment and the cumulative shell cross section area at the
         moment of deposition of the current increment and adds these to
         the previously created increment matrix.
          de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_incr_area(cross_section, incr_matrix)
}
\arguments{
  \item{cross_section}{Digitized cross section of the shell with shell top,
     bottom and growth increments relative to a common X-axis}
  \item{incr_matrix}{Matrix containing ages and calibrated ages for each shell
    increment}
}
\details{Areas between shell increments are calculated by iterating through
   increments in X-direction (dx) and adding differences in Y-values between
     increments}
\value{
\item{incr_matrix}{Matrix containing ages and calibrated ages for each shell
    increment}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}

\examples{
  incr_matrix1<-Oyster_incr_area(cross_section, incr_matrix0)
}
