\name{Oyster_phase_run}
\alias{Oyster_phase_run}
\title{Runs the entire trace element model}
\description{Function that runs all functions contained in Step 5 and 6 of the
   trace element model.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_phase_run(LOG=T, phases_name, image_name, image_ext, IncG,
  pixelsize, subincr_matrix, npma=10, name_shell, name_file)
}
\arguments{
  \item{LOG}{Boolean parameter specifying if a LOG should be printed
    detailing the parameters entered into this function}
  \item{phases_name}{String of full name (including extension!) of the CSV
     file that contains information about the phases in the map}
  \item{image_name}{String of full name of the BMP that needs to be imported,
     excluding extension}
  \item{image_ext}{Extention of phase map file ("BMP" or "TIF")}
  \item{IncG}{Matrix of X- and Y-coordinates of all interpolated
     sub-increments}
  \item{pixelsize}{Size of pixels in phase map in micrometeres}
  \item{subincr_matrix}{Revised version of the "yearly_matrix" data
     frame that contains characteristics calculated for every sub-increment}
  \item{npma}{Window size of the moving average used to smooth mass gain
     record from the bivalve growth model}
  \item{name_shell}{string indicating the name that should be added to all
     exported model results}
  \item{name_file}{string indicating the name of the exported results file}
}
\details{Runs Oyser_run_sec5, Oyser_run_sec6 and Oyster_phase_export}
\value{
XLSX files of all result matrices of the trace element model as well as a
   list containing these matrices:
\item{phase_stat}{matrix of statistics of trace elements and phases in
   the total map}
\item{el_time}{Matrix of trace element concentrations through time}
\item{M_el_mat}{Matrix of mass accumulation per trace element}
\item{M_el_mat_c}{Matrix of cumulative mass accumulation per trace element}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model
     verification on pacific oyster shells (Crassostrea gigas),
       Geosci. Model Dev. Discuss., https://doi.org/10.5194/gmd-2017-137,
         in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}