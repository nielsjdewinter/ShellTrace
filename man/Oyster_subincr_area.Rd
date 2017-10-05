\name{Oyster_subincr_area}
\alias{Oyster_subincr_area}
\title{Formula that calculates area between sub-increments}
\description{Formula that calculates cross section area between each sub-increment and
 the previous sub-increment.
  de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_subincr_area(IncG, subincr_matrix, Xstep)
}
\arguments{
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated sub-increments}
  \item{subincr_matrix}{Data frame that contains characteristics of every
   sub-increment}
  \item{Xstep}{Step value in X-direction for the interpolation of sub-increments}
}
\details{Areas between sub-increments are calculated by averaging the difference
 in Y-values between sub-increments and multiplying them with the step in X-values
  (dx)}
\value{
\item{subincr_matrix}{Revised version of the "incr_matrix" data frame that
 contains cross section area recalculated for every sub-increment}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling growth
 and trace element uptake in marine bivalve shells: Model verification on pacific
   oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
     https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the ShellTrace
   model}
\seealso{"Oyster_Shell_length"}

\examples{
  subincr_matrix1<-Oyster_subincr_area(IncG, subincr_matrix0, Xstep=1)
}
