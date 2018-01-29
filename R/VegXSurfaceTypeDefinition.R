#' S4 class for Veg-X surface type definition
#'
#' @slot method An object of class \code{\linkS4class{VegXMethod}}.
#' @slot surfaceTypes A list of surface types.
#'
#' @export
#'
#' @examples
#' showClass("VegXSurfaceTypeDefinition")
#'
setClass("VegXSurfaceTypeDefinition",slots=c(
  method = "VegXMethod",
  surfaceTypes = "list"))
