\name{Oyster_run_sec5}
\alias{Oyster_run_sec5}
\title{Runs complete Step 5 of the trace element model}
\description{Function that combines all functions in Step 5 of the bivalve
   trace element model and runs them consecutively given the right input.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_run_sec5(phases_name, image_name, image_ext)
}
\arguments{
  \item{phases_name}{String of full name (including extension!) of the CSV
     file that contains information about the phases in the map}
  \item{image_name}{String of full name of the BMP that needs to be imported,
     excluding extension}
  \item{image_ext}{Extention of phase map file ("BMP" or "TIF")}
}
\details{This function runs the functions Oyster_import_phases,
  Oyster_phase_matrix_BMP, Oyster_phase_matrix_TIF and Oyster_phase_stat consecutively}
\value{
List of two items:
\item{phasemat}{matrix of phases of each pixel sorted by X- and
   Y-coordinate of the pixel}
\item{phase_stat}{matrix of statistics of trace elements and phases
   in the total map}
\item{phases}{Matrix containing colour, density and trace element data of the
  phases in the XRF phase map}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for
   modelling growth and trace element uptake in marine bivalve shells:
     Model verification on pacific oyster shells (Crassostrea gigas),
       Geosci. Model Dev. Discuss., https://doi.org/10.5194/gmd-2017-137,
         in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with
   the ShellTrace model}