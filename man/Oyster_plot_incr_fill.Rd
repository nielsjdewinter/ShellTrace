\name{Oyster_plot_incr_fill}
\alias{Oyster_plot_incr_fill}
\title{Plot the result of interpolation of sub-increments with a coloured
   area plot}
\description{Formula that plots the result of the interpolation of digitized
   shell growth increments to reconstruct sub-increments to provide a check
     on the progress of the model. Areas between sub-increments are coloured
       in direction of growth using the heat colour palette.
        de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_plot_incr_fill(IncG)
}
\arguments{
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated
     sub-increments}
}
\details{Areas between sub-increments are represented by coloured polygons,
   while digitized increments are plotted using black lines. High numbers
     of interpolated sub-increments can cause plotting to become slow}
\value{
No data is exported, but a coloured plot showing interpolated sub-increments
   is given in a separate window.
}
\references{de Winter, N. J.: ShellTrace v1.0 - A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}

\examples{
  Oyster_plot_incr_fill(IncG)
}
