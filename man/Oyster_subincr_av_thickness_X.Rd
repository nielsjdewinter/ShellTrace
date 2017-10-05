\name{Oyster_subincr_av_thickness_X}
\alias{Oyster_subincr_av_thickness_X}
\title{Formula that calculates average shell thickness through time}
\description{Formula that calculates average shell thickness at the moment of
 deposition of each shell sub-increment from X- and Y-coordinates of the shell
  increments and adds the result to the matrix of increment characteristics.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_subincr_av_thickness_X(IncG, subincr_matrix)
}
\arguments{
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated sub-increments}
  \item{subincr_matrix}{Data frame that contains characteristics of every
   sub-increment}
}
\details{Shell thickness is calculated as the average difference in Y-values
 between the shell sub-increment and the top of the shell (Increment 0)}
\value{
\item{subincr_matrix}{Revised version of the "incr_matrix" data frame that
 contains average shell thickness recalculated for every sub-increment}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
 growth and trace element uptake in marine bivalve shells: Model verification
  on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
   https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
 ShellTrace model}
\seealso{"Oyster_av_thickness"}
\source{
\href{https://github.com/nielsjdewinter/ShellTrace}{GitHub} \cr
\href{https://doi.org/10.5194/gmd-2017-137}{Manuscript} \cr
\href{https://doi.org/10.5194/gmd-2017-137-supplement}{Supplementary data} \cr
\href{http://nidewint.wixsite.com/nielsdewinter}{Author website}
}
\examples{
  subincr_matrix3<-Oyster_subincr_av_thickness_X(IncG, subincr_matrix2)
}
