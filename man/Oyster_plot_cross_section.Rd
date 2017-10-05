\name{Oyster_plot_cross_section}
\alias{Oyster_plot_cross_section}
\title{Plot the converted shell cross section}
\description{Simple function that returns a plot of the shell cross section
   after it has been converted to a common X-axis.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_plot_cross_section(cross_section)
}
\arguments{
  \item{cross_section}{Digitized cross section of the shell with shell top,
     bottom and growth increments relative to a common X-axis}
}
\details{Plotting of digitized cross section after first modelling step to
   verify the correct digitization of the shell increments}
\value{Opens a new plotting window to plot the shell cross section based on
   its X- and Y-coordinates
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model
     verification on pacific oyster shells (Crassostrea gigas), Geosci.
       Model Dev. Discuss., https://doi.org/10.5194/gmd-2017-137, in review,
         2017.}
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
  Oyster_plot_cross_section(cross_section)
}
