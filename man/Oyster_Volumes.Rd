\name{Oyster_Volumes}
\alias{Oyster_Volumes}
\title{Formula that calculates volume of the shell through time}
\description{Formula that calculates the volume of the bivalve shell during the
   time of deposition of each sub-increment.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_Volumes(subincr_matrix, Z_mat, IncG, Xstep = 0.1)
}
\arguments{
  \item{subincr_matrix}{Data frame that contains characteristics of every
     sub-increment}
  \item{Z_mat}{Matrix of Z-values for each X-value and each sub-increment}
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated sub-increments}
  \item{Xstep}{Step value in X-direction for the interpolation of sub-increments}
}
\details{Volume is calculated for each sub-increment and each X-value based on
   cross sections perpendicular to the XY-plane. Area of the shell in this
     cross section is calculated by constructing a circle section through the
       intercept with the base ellipse and the top of the shell sub-increment.
         See paper de Winter, GMD (in review) for details and illustrations}
\value{
A list of two data frames:
\item{subincr_matrix}{Updated data frame that contains characteristics of every
   sub-increment with modelled shell volumes added to the matrix}
\item{IncGAnet}{Matrix of areas of cross sections in YZ-directions sorted by
   X-values and by sub-increment}
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
    diagL<-Oyster_Volumes(subincr_matrix4, Z_mat, IncG, Xstep = 1)
  }
}
