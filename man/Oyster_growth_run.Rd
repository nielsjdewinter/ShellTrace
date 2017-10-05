\name{Oyster_growth_run}
\alias{Oyster_growth_run}
\title{Runs the entire growth model}
\description{Function that runs all functions contained in Step 2-4 of the
   growth model.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_growth_run(LOG=T, raw_data, image_length, season_length=250,
  Xstep=0.1, Tstep=1, Oyster_height, Oyster_length,
  name_file="Oyster_growth_model")
}
\arguments{
  \item{LOG}{Boolean parameter specifying if a LOG should be printed
    detailing the parameters entered into this function}
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
}
\details{Runs Oyser_run_sec2, Oyser_run_sec3, Oyser_run_sec4 and
   Oyster_Export}
\value{
Matrix containing all parameters of the shell calculated per sub-increment
   and CSV file containing this matrix
\item{subincr_matrix}{Revised version of the "incr_matrix" data frame
   that contains characteristics calculated for every sub-increment}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model
     verification on pacific oyster shells (Crassostrea gigas),
       Geosci. Model Dev. Discuss., https://doi.org/10.5194/gmd-2017-137,
         in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with
   the ShellTrace model}
\source{
\href{https://github.com/nielsjdewinter/ShellTrace}{GitHub} \cr
\href{https://doi.org/10.5194/gmd-2017-137}{Manuscript} \cr
\href{https://doi.org/10.5194/gmd-2017-137-supplement}{Supplementary data} \cr
\href{http://nidewint.wixsite.com/nielsdewinter}{Author website}
}
\examples{
  \dontrun{
    GList<-Oyster_growth_run(TRUE, O1_input, image_length, season_length=250,
    Xstep=1, Tstep=1, Oyster_height, Oyster_length,name_file="test_export")
  }
}
