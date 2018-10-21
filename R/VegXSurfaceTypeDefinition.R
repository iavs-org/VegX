#' S4 class for Veg-X surface type definition
#'
#' @slot method An object of class \code{\linkS4class{VegXMethodDefinition}}.
#' @slot surfaceTypes A list of surface types.
#'
#' @examples
#' showClass("VegXSurfaceTypeDefinition")
#'
setClass("VegXSurfaceTypeDefinition",slots=c(
  method = "VegXMethodDefinition",
  surfaceTypes = "list"))
