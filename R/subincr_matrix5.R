#' Matrix containing data calculated for each growth band.
#'
#' A dataset containing specific parameters calculated for all interpolated
#' subincrements from the Crassostrea gigas #1 oyster used as an example in de Winter
#' (2017) sorted per increment.
#'
#' @docType data
#'
#' @usage data(subincr_matrix5)
#'
#' @format A data frame with 1291 rows and 17 variables:
#'
#' \describe{
#'   \item{Age}{age (in days) of the subincrement}
#'   \item{p1xs}{X-value of the first (leftmost) point in the subincrement}
#'   \item{p2xs}{X-value of the last (rightmost) point in the subincrement}
#'   \item{areaY}{Area between subsequent subincrements}
#'   \item{areaC}{Area between subincrement and top of shell}
#'   \item{p1y}{Y-value of the first (leftmost) point in the subincrement}
#'   \item{p2y}{Y-value of the last (rightmost) point in the subincrement}
#'   \item{shell_height}{Height of shell during deposition of the subincrement}
#'   \item{firstl}{Row number in IncG of first (leftmost) data point belonging to the subincrement}
#'   \item{lastl}{Row number in IncG of last (rightmost) data point belonging to the subincrement}
#'   \item{av_thickness}{Average thickness during deposition of the subincrement}
#'   \item{W_ellipse}{Length of the short axis of the base ellipse of the oyster during deposition of the subincrement}
#'   \item{L_ellipse_acc}{Length of the long axis of the base ellipse of the oyster during deposition of the subincrement projected on the X-axis}
#'   \item{a_ellipse}{Half the length of the long axis of the base ellipse of the oyster during deposition of the subincrement}
#'   \item{b_ellipse}{Half the length of the short axis of the base ellipse of the oyster during deposition of the subincrement}
#'   \item{VolI}{Volume between subsequent subincrements}
#'   \item{VolC}{Volume between subincrement and top of shell}
#' }
#' @source \url{https://doi.org/10.5194/gmd-2017-137-supplement}
"subincr_matrix5"
