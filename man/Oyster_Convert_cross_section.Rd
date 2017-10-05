\name{Oyster_Convert_cross_section}
\alias{Oyster_Convert_cross_section}
\title{Function that converts XY-data of digitized cross section}
\description{Takes XY data of digitized growth increments in a shell cross section
   and converts them to a common X-axis with the correct lengths in millimeters.
   de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_Convert_cross_section(raw_data, image_length, Xstep = 0.1)
}
\arguments{
  \item{raw_data}{Numeric data frame containing the X- and Y-coordinates digitized
     in Adobe Illustrator or another image processing software}
  \item{image_length}{Measured maximum length of the area of the cross section that
     is represented in "raw_data"}
  \item{Xstep}{The step size (dx) in X-direction used to interpolate coordinates
     of shell increments}
}
\details{First step in growth modelling: Converting XY data of increments to a common
   X-axis}
\value{
List of two data sets and one value:
\item{cross_section}{Digitized cross section of the shell with shell top, bottom and
   growth increments relative to a common X-axis}
\item{year_trace}{Digitized cross section of shell increments without addition of top
   and bottom of the shell to the increment Y-values}
\item{lengthfactor}{Factor of actual shell length relative to shell length in cross
   section}
\item{incr_matrix}{Matrix containing ages and calibrated ages for each shell increment}
}
\references{de Winter, N. J.: ShellTrace v1.0 - A new approach for modelling growth
   and trace element uptake in marine bivalve shells: Model verification on pacific
     oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https:\\doi.org\\10.5194\\gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the ShellTrace model}

\examples{
  Llist<-Oyster_Convert_cross_section(O1_input, image_length, Xstep=1)
}
