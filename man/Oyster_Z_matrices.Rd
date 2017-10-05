\name{Oyster_Z_matrices}
\alias{Oyster_Z_matrices}
\title{Function that calculates matrices of Z-values for all sub-increments and
   all X-values}
\description{Function that calculates Z-values that form the edge of the shell
   in terms of distance from the X-axis in direction of the width of the shell.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_Z_matrices(IncG, subincr_matrix)
}
\arguments{
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated sub-increments}
  \item{subincr_matrix}{Data frame that contains characteristics of every
     sub-increment}
}
\details{Z-values are calculated using the standard formulae of an ellipse, the
   parameters calculated in "Oyster_Oval_parameters" and the X-coordinates of
     each sub-increment
}
\value{
\item{Z-mat}{Matrix of Z-values for each X-value and each sub-increment}
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
  Z_mat<-Oyster_Z_matrices(IncG, subincr_matrix4)
}
