\name{Oyster_run_sec2}
\alias{Oyster_run_sec2}
\title{Runs complete Step 2 of the growth model}
\description{Function that combines all functions in Step 2 of the bivalve
   growth model and runs them consecutively given the right input.
    de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_run_sec2(raw_data, image_length, Xstep)
}
\arguments{
  \item{raw_data}{Numeric data frame containing the X- and Y-coordinates
     digitized in Adobe Illustrator or another image processing software}
  \item{image_length}{Measured maximum length of the area of the cross
     section that is represented in raw_data}
  \item{Xstep}{The step size (dx) in X-direction used to interpolate
     coordinates of shell increments}
}
\details{This function runs the functions Oyster_Convert_cross_section,
   Oyster_plot_cross_section, Oyster_incr_area, Oyster_Shell_thickness
     and Oyster_Shell_height consecutively}
\value{
List of three items:
\item{cross_section}{Digitized cross section of the shell with shell top,
   bottom and growth increments relative to a common X-axis}
\item{incr_matrix}{Matrix listing characteristics of each digitized
   increment}
\item{lengthfactor}{Factor of actual shell length relative to shell
   length in cross section, used to constrain pixelsize in phase map}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for
   modelling growth and trace element uptake in marine bivalve shells:
     Model verification on pacific oyster shells (Crassostrea gigas),
       Geosci. Model Dev. Discuss., https://doi.org/10.5194/gmd-2017-137,
         in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with
   the ShellTrace model}

\examples{
  List2<-Oyster_run_sec2(O1_input, image_length, Xstep=1)
}

