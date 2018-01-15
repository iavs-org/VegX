#' S4 class for measurement method
#'
#' @slot name Name of the measurement method.
#' @slot description Description of the measurement method.
#' @slot attributeClass Kind of attribute measured (e.g. cover).
#' @slot attributeType Either "quantitative", "ordinal" or "qualitative".
#' @slot attributes List of attribute values
#'
#' @return
#' @export
#'
#' @examples
setClass("VegXMethod",slots=c(
                         name = "character",
                         description="character",
                         attributeClass = "character",
                         attributeType = "character",
                         attributes = "list"))
