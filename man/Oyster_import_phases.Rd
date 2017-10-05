\name{Oyster_import_phases}
\alias{Oyster_import_phases}
\title{Function that imports phase data}
\description{Function that imports a table (in CSV format) containing the
   specifics of phases that make up the phase XRF map used in the trace
     element model.
      de Winter, N. J. (2017) <doi:10.5194/gmd-2017-137>}
\usage{
Oyster_import_phases(file_name)
}
\arguments{
  \item{file_name}{String containing the name of the CSV file to be imported,
     including its extension}
}
\details{This funcion imports a table containing trace element concentration
   and RGB colour data. Table needs to be of the same format as the example
     shown in de Winter, N.J., GMD, in review. The amount of phases represented
       in the table byrows is unlimited)
}
\value{
\item{phases}{Matrix containing colour, density and trace element data of the
  phases in the XRF phase map}
}
\references{de Winter, N. J.: ShellTrace v1.0 ? A new approach for modelling
   growth and trace element uptake in marine bivalve shells: Model verification
     on pacific oyster shells (Crassostrea gigas), Geosci. Model Dev. Discuss.,
       https://doi.org/10.5194/gmd-2017-137, in review, 2017.}
\author{Niels J. de Winter}
\note{Please cite Geoscientific Model Development paper dealing with the
   ShellTrace model}
\seealso{"Oyster_av_thickness"}