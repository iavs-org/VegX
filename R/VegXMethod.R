#' Title
#'
#' @slot description character.
#' @slot name character.
#' @slot type character.
#' @slot attributes list.
#'
#' @return
#' @export
#'
#' @examples
setClass("VegXMethod",slots=c(
                         name = "character",
                         description="character",
                         type = "character",
                         attributes = "list"))
