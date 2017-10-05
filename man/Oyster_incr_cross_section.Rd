\name{Oyster_incr_cross_section}
\alias{Oyster_incr_cross_section}
\title{Formula that interpolates shell increments}
\description{Formula that takes the coordinates of digitized shell increments
   and interpolates a number of sub-increments between them to increase
     the time resolution of the growth model. The number of interpolated
     shell increments as well as the relative thickness of these increments
     is determined by a sinusoidal seasonal model that simulates seasonal
     variations in shell growth rate.
     de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_incr_cross_section(incr_matrix, cross_section, season_length,
  Tstep=1, Xstep=0.1)
}
\arguments{
  \item{incr_matrix}{Matrix listing characteristics of each digitized
     increment}
  \item{cross_section}{Digitized cross section of the shell with shell top,
     bottom and growth increments relative to a common X-axis}
  \item{season_length}{Length (in days) of the growth season of the
    studied bivalve}
  \item{Tstep}{The step size (dt) in time (days) used to interpolate
     the existing shell increments}
  \item{Xstep}{The step size (dx) in X-direction used to interpolate
     coordinates of shell increments}
}
\details{Sub-increments are reconstructed by interpolating Y-values between
   the digitized shell increments}
\value{
List of two data frames:
\item{IncG}{Matrix of X- and Y-coordinates of all interpolated sub-increments}
\item{subincr_matrix}{Revised version of the "incr_matrix" data frame that
   contains characteristics (sub-increment number, X-value of start of
     increment) recalculated for every sub-increment}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}

\examples{
  Lsub<-Oyster_incr_cross_section(incr_matrix3, cross_section, season_length=250,
  Tstep=1, Xstep=1)
}

