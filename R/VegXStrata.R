#' S4 class for strata definition in Veg-X
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
