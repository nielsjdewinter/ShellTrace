\name{Oyster_plot_incr_CS}
\alias{Oyster_plot_incr_CS}
\title{Plot the result of interpolation of sub-increments}
\description{Formula that plots the result of the interpolation of digitized
   shell growth increments to reconstruct sub-increments to provide a check
     on the progress of the model.
      de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_plot_incr_CS(IncG, incr_matrix, Tstep)
}
\arguments{
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated
     sub-increments}
  \item{incr_matrix}{Matrix listing characteristics of each digitized
     increment}
  \item{Tstep}{The step size (dt) in time (days) used to interpolate
     the existing shell increments}
}
\details{In order to prevent overcrowding the plot area and slowing the
   plotting process, only original shell increments and sub-increments
     halfway between original increments are plotted. Sub-increments are
       colored blue}
\value{
No data is exported, but a plot showing interpolated sub-increments is
   given in a separate window.
}
\references{de Winter, N. J.: ShellTrace v1.0 - A new approach for modelling
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
  Oyster_plot_incr_CS(IncG,incr_matrix3,Tstep=1)
}
