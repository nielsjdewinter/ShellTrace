\name{Oyster_run_sec6}
\alias{Oyster_run_sec6}
\title{Runs complete Step 6 of the trace element model}
\description{Function that combines all functions in Step 6 of the bivalve trace
   element model and runs them consecutively given the right input.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_run_sec6(phasemat, IncG, pixelsize, phases, subincr_matrix, npma,
  name_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phasemat}{matrix of phases of each pixel sorted by X- and Y-coordinate
     of the pixel}
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated
     sub-increments}
  \item{pixelsize}{size of pixels in the phase map in micrometer}
  \item{phases}{Matrix containing colour and trace element data of
     the phases in the XRF phase map}
  \item{subincr_matrix}{Data frame that contains characteristics of
     every sub-increment}
  \item{npma}{Integer n-value determining the window size of the moving
     average smoothing of the mass accumulation record}
  \item{name_file}{string indicating the name of the exported results file}
}
\details{This function runs the functions Oyster_subincr_phases, Oyster_Mass_gain
  Oyster_el_time and Oyster_accumulation consecutively}
\value{
List of three items:
\item{el_time}{Matrix of trace element concentrations through time}
\item{M_el_mat}{Matrix of mass accumulation per trace element}
\item{M_el_mat_c}{Matrix of cumulative mass accumulation per trace element}
\item{subincr_matrix}{Data frame that contains characteristics of
     every sub-increment}
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
    List6<-Oyster_run_sec6(phasemat, IncG, pixelsize, O1_phase, subincr_matrix5, npma=10,
    "test_export")
  }
}
