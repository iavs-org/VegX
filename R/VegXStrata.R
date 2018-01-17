#' S4 class for Veg-X strata definition
#'
#' @slot method VegXMethod.
#' @slot strata list.
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @export
#'
#' @examples
setClass("VegXStrata",slots=c(
  method = "VegXMethod",
  strata = "list"))
