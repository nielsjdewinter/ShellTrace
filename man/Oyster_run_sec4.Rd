\name{Oyster_run_sec4}
\alias{Oyster_run_sec4}
\title{Runs complete Step 4 of the growth model}
\description{Function that combines all functions in Step 4 of the bivalve growth
   model and runs them consecutively given the right input.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_run_sec4(IncG, subincr_matrix, Xstep = 0.1)
}
\arguments{
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated sub-increments}
  \item{subincr_matrix}{Revised version of the "yearly_matrix" data frame that
     contains characteristics calculated for every sub-increment}
  \item{Xstep}{The step size (dx) in X-direction used to interpolate coordinates
     of shell increments}
}
\details{This function runs the functions Oyster_Z_matrices and Oyster_Volumes
  consecutively}
\value{
A list of two items:
\item{subincr_matrix}{Revised version of the "incr_matrix" data frame that
   contains characteristics calculated for every sub-increment}
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
\source{
\href{https://github.com/nielsjdewinter/ShellTrace}{GitHub} \cr
\href{https://doi.org/10.5194/gmd-2017-137}{Manuscript} \cr
\href{https://doi.org/10.5194/gmd-2017-137-supplement}{Supplementary data} \cr
\href{http://nidewint.wixsite.com/nielsdewinter}{Author website}
}
\examples{
  \dontrun{
    List4<-Oyster_run_sec4(IncG, subincr_matrix4, Xstep = 1)
  }
}
