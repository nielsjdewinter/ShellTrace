#' Matrix containing data calculated for each growth band.
#'
#' A dataset containing specific parameters calculated for all digitized growth
#' increments from the Crassostre gigas #1 oyster used as an example in de Winter
#' (2017) sorted per increment.
#'
#' @docType data
#'
#' @usage data(incr_matrix2)
#'
#' @format A data frame with 8 rows and 6 variables:
#'
#' \describe{
#'   \item{growth band}{name of the growth increment}
#'   \item{age (days)}{Age associated with the deposition of the growth increment}
#'   \item{age_cal (days)}{Age associated with the deposition of the growth increment, calibrated to the seasonal cycle}
#'   \item{incr_area}{area between subsequent increments}
#'   \item{incr_cumarea}{area between increment and the top of the shell}
#'   \item{av_thickness}{average thickness of area between increment and the top of the shell}
#' }
#' @source \url{https://doi.org/10.5194/gmd-2017-137-supplement}
"incr_matrix2"
