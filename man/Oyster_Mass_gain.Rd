\name{Oyster_Mass_gain}
\alias{Oyster_Mass_gain}
\title{Formula that calculates mass increase of bivalve shell with time}
\description{Formula that takes the modelled volume of a bivalve shell by
   sub-increment and calculates mass increase using variable shell density.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_Mass_gain(subincr_matrix, phase_mat, phases)
}
\arguments{
  \item{subincr_matrix}{Data frame that contains characteristics of
     every sub-increment}
  \item{phase_mat}{Matrix of amounts of pixels of each phase per sub-increment}
  \item{phases}{Matrix containing colour, density and trace element data of the
    phases in the XRF phase map}
}
\details{
Calculates mass gain from modelled changes in volume based on the shell
   density
}
\value{
\item{subincr_matrix}{Updated data frame that contains characteristics
   of every sub-increment with modelled shell mass calculations added to
     the matrix}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for
   modelling growth and trace element uptake in marine bivalve shells:
     Model verification on pacific oyster shells (Crassostrea gigas),
       Geosci. Model Dev. Discuss., https://doi.org/10.5194/gmd-2017-137,
         in review, 2017.}
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
  subincr_matrix6<-Oyster_Mass_gain(subincr_matrix5, phase_mat, O1_phase)
}
