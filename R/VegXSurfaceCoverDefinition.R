#' S4 class for Veg-X surface cover definition
#'
#' @slot method An object of class \code{\linkS4class{VegXMethod}}.
#' @slot surfaceCovers A list of surface covers.
#'
#' @export
#'
#' @examples
#' showClass("VegXSurfaceCoverDefinition")
#'
setClass("VegXSurfaceCoverDefinition",slots=c(
  method = "VegXMethod",
  surfaceCovers = "list"))
