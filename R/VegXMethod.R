#' Title
#'
#' @slot description character.
#' @slot name character.
#' @slot attributeClass character.
#' @slot attributeType character.
#' @slot attributes list.
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
