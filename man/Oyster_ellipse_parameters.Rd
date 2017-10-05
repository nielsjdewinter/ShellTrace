\name{Oyster_ellipse_parameters}
\alias{Oyster_ellipse_parameters}
\title{Function that calculates the parameters of the base ellipse used for
   bivalve growth modelling}
\description{Calculates the parameters a and b of the ellipse that forms
   the base of the shell in growth modelling.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_ellipse_parameters(subincr_matrix, IncG, Oyster_height, Oyster_length)
}
\arguments{
  \item{subincr_matrix}{Data frame that contains characteristics of every
     sub-increment}
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated
     sub-increments}
  \item{Oyster_height}{Measured maximum height of the shell}
  \item{Oyster_length}{Measured maximum length of the shell in anterio-posterior direction}
}
\details{Parameters of the base ellipse of the shell are calculated by
   calculating the ratio between measured shell height and length and the
     endpoints of all sub-increments}
\value{
\item{subincr_matrix}{Updated data frame that contains characteristics
   of every sub-increment, with ellipse parameters added}
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
  subincr_matrix4<-Oyster_ellipse_parameters(subincr_matrix3,IncG,Oyster_height,Oyster_height)
}
