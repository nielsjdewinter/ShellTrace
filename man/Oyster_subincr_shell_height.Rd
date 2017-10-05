\name{Oyster_subincr_shell_height}
\alias{Oyster_subincr_shell_height}
\title{A fomula to calculate shell height per sub-increment}
\description{Formula that calculates shell height at the time of deposition
   of each sub-increment.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_subincr_shell_height(subincr_matrix, IncG, Xstep)
}
\arguments{
  \item{subincr_matrix}{Data frame that contains characteristics of every
     sub-increment}
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated
     sub-increments}
  \item{Xstep}{Step value in X-direction for the interpolation of
     sub-increments}
}
\details{Shell height is calculated via the Pythagorean Theorem using the
   X- and Y-coordinates of both ends of the shell sub-increment with extreme
     X-values}
\value{
\item{subincr_matrix}{Revised version of the "incr_matrix" data frame
   that contains shell height recalculated for every sub-increment}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}
\seealso{"Oyster_Shell_height"}
\source{
\href{https://github.com/nielsjdewinter/ShellTrace}{GitHub} \cr
\href{https://doi.org/10.5194/gmd-2017-137}{Manuscript} \cr
\href{https://doi.org/10.5194/gmd-2017-137-supplement}{Supplementary data} \cr
\href{http://nidewint.wixsite.com/nielsdewinter}{Author website}
}
\examples{
  subincr_matrix2<-Oyster_subincr_shell_height(subincr_matrix1,IncG,Xstep=1)
}
