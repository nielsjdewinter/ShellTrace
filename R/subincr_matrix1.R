#' Matrix containing data calculated for each growth band.
#'
#' A dataset containing specific parameters calculated for all interpolated
#' subincrements from the Crassostrea gigas #1 oyster used as an example in de Winter
#' (2017) sorted per increment.
#'
#' @docType data
#'
#' @usage data(subincr_matrix1)
#'
#' @format A data frame with 1291 rows and 5 variables:
#'
#' \describe{
#'   \item{Age}{age (in days) of the subincrement}
#'   \item{p1xs}{X-value of the first (leftmost) point in the subincrement}
#'   \item{p2xs}{X-value of the last (rightmost) point in the subincrement}
#'   \item{areaY}{Area between subsequent subincrements}
#'   \item{areaC}{Area between subincrement and top of shell}
#' }
#' @source \url{https://doi.org/10.5194/gmd-2017-137-supplement}
"subincr_matrix1"
