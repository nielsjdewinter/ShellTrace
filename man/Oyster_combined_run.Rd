\name{Oyster_combined_run}
\alias{Oyster_combined_run}
\title{Runs the growth and trace element model}
\description{Function that runs the bivalve growth and trace element model from
   start to finish, combining Step 2-6.
   de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_combined_run(raw_data, image_length, season_length=250, Xstep=0.1, Tstep=1,
Oyster_height, Oyster_length, name_file="Oyster_growth_model", phases_name,
image_name, image_ext, npma=10, name_shell)
}
\arguments{
  \item{raw_data}{Numeric data frame containing the X- and Y-coordinates
     digitized in Adobe Illustrator or another image processing software}
  \item{image_length}{Measured maximum length of the area of the cross
     section that is represented in "raw_data"}
  \item{season_length}{Length (in days) of the growth season of the
    studied bivalve}
  \item{Xstep}{The step size (dx) in X-direction used to interpolate
     coordinates of shell increments}
  \item{Tstep}{The step size (dt) in time (days) used to interpolate
     the existing shell increments}
  \item{Oyster_height}{Measured maximum height of the shell}
  \item{Oyster_length}{Measured maximum length of the shell in anterio-posterior direction}
  \item{name_file}{string indicating the name that should be added to
     all exported model results}
  \item{phases_name}{String of full name (including extension!) of the CSV
     file that contains information about the phases in the map}
  \item{image_name}{String of full name of the BMP that needs to be imported,
     excluding extension}
  \item{image_ext}{Extention of phase map file ("BMP" or "TIF")}
  \item{npma}{Window size of the moving average used to smooth mass gain
     record from the bivalve growth model}
  \item{name_shell}{string indicating the name that should be added to all
     exported model results}
}
\details{Oyster_growth_run and Oyster_phase_run, bundling and exporting
  the model results}
\value{
Exports matrices containing oyster growth parameters for each sub-increment
   as well as matrices containing trace element accumulation rates and
     concentration changes through the shells life time resulting from the
       trace element model.
All these matrices are also exported as a list containing:
\item{subincr_matrix}{Revised version of the "incr_matrix" data frame
   that contains characteristics calculated for every sub-increment}
\item{phase_stat}{matrix of statistics of trace elements and phases in the
   total map}
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
\seealso{"Oyster_growth_run" and "Oyster_phase_run"}
\source{
\href{https://github.com/nielsjdewinter/ShellTrace}{GitHub} \cr
\href{https://doi.org/10.5194/gmd-2017-137}{Manuscript} \cr
\href{https://doi.org/10.5194/gmd-2017-137-supplement}{Supplementary data} \cr
\href{http://nidewint.wixsite.com/nielsdewinter}{Author website}
}