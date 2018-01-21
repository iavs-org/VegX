#' S4 class for Veg-X measurement methods
#'
#' @slot name Name of the measurement method.
#' @slot description Description of the measurement method.
#' @slot citation A string with the bibliographic reference for the method.
#' @slot attributeClass Kind of attribute measured (e.g. cover).
#' @slot attributeType Either "quantitative", "ordinal" or "qualitative".
#' @slot attributes List of attribute values
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @export
#'
#' @examples
#' showClass("VegXMethod")
#'
setClass("VegXMethod",slots=c(
                         name = "character",
                         description="character",
                         citation = "character",
                         attributeClass = "character",
                         attributeType = "character",
                         attributes = "list"))
