#' S4 class for Veg-X surface cover definition
#'
#' @slot method VegXMethod.
#' @slot strata list.
#'
#' @export
#'
#' @examples
#' showClass("VegXSurfaceCoverDefinition")
#'
setClass("VegXSurfaceCoverDefinition",slots=c(
  method = "VegXMethod",
  surfaceCovers = "list"))
