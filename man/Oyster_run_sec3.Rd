\name{Oyster_run_sec3}
\alias{Oyster_run_sec3}
\title{Runs complete Step 3 of the growth model}
\description{Function that combines all functions in Step 3 of the bivalve
   growth model and runs them consecutively given the right input.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_run_sec3(cross_section, incr_matrix, season_length=250, Xstep=0.1,
  Tstep=1, Oyster_height, Oyster_length)
}
\arguments{
  \item{cross_section}{Digitized cross section of the shell with shell top,
     bottom and growth increments relative to a common X-axis}
  \item{incr_matrix}{Matrix listing characteristics of each digitized
     increment}
  \item{season_length}{Length (in days) of the growth season of the
    studied bivalve}
  \item{Xstep}{The step size (dx) in X-direction used to interpolate
     coordinates of shell increments}
  \item{Tstep}{The step size (dt) in time (days) used to interpolate
     the existing shell increments}
  \item{Oyster_height}{Measured maximum height of the shell}
  \item{Oyster_length}{Measured maximum length of the shell in anterio-posterior direction}
}
\details{This function runs the functions Oyster_Increment_cross_section,
   Oyster_plot_Inc_CS, Oyster_Inc_fill, Oyster_subincr_area,
     Oyster_subincr_shell_height, Oyster_subincr_av_thickness and
       Oyster_ellipse_parameters consecutively}
\value{
List of two items:
\item{IncG}{Matrix of X- and Y-coordinates of all interpolated
   sub-increments}
\item{subincr_matrix}{Revised version of the "yearly_matrix" data
   frame that contains characteristics (sub-increment number, X-value
     of start of increment) recalculated for every sub-increment}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for
   modelling growth and trace element uptake in marine bivalve shells:
     Model verification on pacific oyster shells (Crassostrea gigas),
       Geosci. Model Dev. Discuss., https://doi.org/10.5194/gmd-2017-137,
         in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with
   the ShellTrace model}

\examples{
  List3<-Oyster_run_sec3(cross_section, incr_matrix3, season_length=250, Xstep=1,
  Tstep=1, Oyster_height, Oyster_length)
}
