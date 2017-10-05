\name{Oyster_accumulation}
\alias{Oyster_accumulation}
\title{Calculate trace element accumulation rates}
\description{Function that combines the concentrations of trace elements per
   sub-increment with a smoothed record of mass accumulation with time to
     calculate the rate of accumulation of each element through the lifetime
       of the bivalve.
        de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_accumulation(el_time, subincr_matrix, npma = 10)
}
\arguments{
  \item{el_time}{Matrix of trace element concentrations through time}
  \item{subincr_matrix}{Data frame that contains characteristics of
     every sub-increment}
  \item{npma}{Integer n-value determining the window size of the moving
     average smoothing of the mass accumulation record}
}
\details{A record of mass accumulation of the shell is smoothed using a
   moving average. This mass accumulation record is multiplied with the
     records of trace element concentrations per sub-increment to obtain
       a record of mass accumulation of each trace element through time
         and a record of cumulative trace element accumulation through
           time}
\value{
Matrices of trace element accumulation per sub-increment and cumulative
   trace element accumulation
\item{M_el_mat}{Matrix of mass accumulation per trace element}
\item{M_el_mat_c}{Matrix of cumulative mass accumulation per trace
   element}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for
   modelling growth and trace element uptake in marine bivalve shells:
     Model verification on pacific oyster shells (Crassostrea gigas),
       Geosci. Model Dev. Discuss., https://doi.org/10.5194/gmd-2017-137,
         in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with
   the ShellTrace model}
\seealso{"Oyster_el_time"}

\examples{
  AccL<-Oyster_accumulation(el_time, subincr_matrix6, npma = 10)
}