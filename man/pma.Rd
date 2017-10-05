\name{pma}
\alias{pma}
\title{Function to do an n-point moving average}
\description{Performs a moving average smoothing on a data series.
  de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
pma(x, i, n)
}

\arguments{
  \item{x}{A numeric data frame containing the data set to be smoothed, X-values should be in the first column}
  \item{i}{The index of the column that contains the Y-values}
  \item{n}{Integer N-value determining the window size of the moving average smoothing}
}
\value{
A numeric data frame containing three columns: One with X-values, one with Y-values and one with smoothed Y-values
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling growth and trace element uptake in marine bivalve shells: Model verification on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss., https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the ShellTrace model}
\source{
\href{https://github.com/nielsjdewinter/ShellTrace}{GitHub} \cr
\href{https://doi.org/10.5194/gmd-2017-137}{Manuscript} \cr
\href{https://doi.org/10.5194/gmd-2017-137-supplement}{Supplementary data} \cr
\href{http://nidewint.wixsite.com/nielsdewinter}{Author website}
}
\examples{
Nile<-as.data.frame(Nile)
Nile<-cbind(rownames(Nile),Nile)
Nile_5pma<-pma(Nile,2,5)
}
