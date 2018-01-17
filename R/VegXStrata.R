#' S4 class for Veg-X strata definition
#'
#' @slot method VegXMethod.
#' @slot strata list.
#'
#' @return
#' @export
#'
#' @examples
setClass("VegXStrata",slots=c(
  method = "VegXMethod",
  strata = "list"))
