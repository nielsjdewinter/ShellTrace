\name{Oyster_phase_export}
\alias{Oyster_phase_export}
\title{Export results of trace element model}
\description{A function that takes all the matrices with results of the trace
   element model and exports them as XLSX files.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_phase_export(phase_stat, el_time, M_el_mat, M_el_mat_c, name_shell)
}
\arguments{
  \item{phase_stat}{matrix of statistics of trace elements and phases in
     the total map}
  \item{el_time}{Matrix of trace element concentrations through time}
  \item{M_el_mat}{Matrix of mass accumulation per trace element}
  \item{M_el_mat_c}{Matrix of cumulative mass accumulation per trace element}
  \item{name_shell}{Name of the shell used in the model to be incorporated
     into the file names}
}
\details{All matrices fed to the function are exported as XLSX files in the
   working directory using the " write.xlsx"  function of the "xlsx" package}
\value{XLSX files of all result matrices of the trace element model}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}
\seealso{"Oyster_Export"}

\examples{
  \dontrun{
    Oyster_phase_export(phase_stat, el_time, M_el_mat, M_el_mat_c, "test")
  }
}
