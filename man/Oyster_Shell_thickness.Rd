\name{Oyster_Shell_thickness}
\alias{Oyster_Shell_thickness}
\title{Formula that calculates average shell thickness through time}
\description{Formula that calculates average shell thickness at the moment of
   deposition of each shell increment from X- and Y-coordinates of the shell
     increments and adds the result to the matrix of increment characteristics.
      de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_Shell_thickness(cross_section, incr_matrix)
}
\arguments{
  \item{incr_matrix}{Matrix listing characteristics of each digitized
     increment}
  \item{cross_section}{Digitized cross section of the shell with shell top,
     bottom and growth increments relative to a common X-axis}
}
\details{Shell thickness is calculated as the average difference in Y-values
   between the shell increment and the top of the shell (Increment 0)}
\value{
\item{incr_matrix}{Updated matrix listing characteristics of each digitized
   increment, shell thickness values are added}
A plot of the change in shell thickness with shell age based on the digitized
   growth increments is produced in a new window
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}

\examples{
  incr_matrix2<-Oyster_Shell_thickness(cross_section, incr_matrix1)
}
