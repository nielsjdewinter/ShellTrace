\name{Oyster_Shell_height}
\alias{Oyster_Shell_height}
\title{Formula that calculates shell height through time}
\description{Formula that calculates shell height at the moment of deposition of
   each shell increment from X- and Y-coordinates of the shell increments and
     adds the result to the matrix of increment characteristics.
      de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_Shell_height(cross_section, incr_matrix)
}
\arguments{
  \item{incr_matrix}{Matrix listing characteristics of each digitized
     increment}
  \item{cross_section}{Digitized cross section of the shell with shell top,
     bottom and growth increments relative to a common X-axis}
}
\details{Shell height is calculated via the Pythagorean Theorem using the
   X- and Y-coordinates of both ends of the shell increment with extreme
     X-values}
\value{
\item{incr_matrix}{Updated matrix listing characteristics of each digitized
   increment, shell height values as well as the coordinates of both ends of
     the shell increments are added}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}
\source{
\href{https://github.com/nielsjdewinter/ShellTrace}{GitHub} \cr
\href{https://doi.org/10.5194/gmd-2017-137}{Manuscript} \cr
\href{https://doi.org/10.5194/gmd-2017-137-supplement}{Supplementary data} \cr
\href{http://nidewint.wixsite.com/nielsdewinter}{Author website}
}
\examples{
  incr_matrix3<-Oyster_Shell_height(cross_section, incr_matrix2)
}
