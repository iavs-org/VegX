#' S4 class for Veg-X strata definition
#'
#' @slot method An object of class \code{\linkS4class{VegXMethodDefinition}}.
#' @slot strata A list of vegetation strata.
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @examples
#' showClass("VegXStrataDefinition")
#'
setClass("VegXStrataDefinition",slots=c(
  method = "VegXMethodDefinition",
  strata = "list"))
