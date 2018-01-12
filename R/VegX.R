#' Veg-X S4 class
#'
#' Implements the minimum subset of Veg-X elements for importing and
#' combining vegetation data
#'
#' @slot plots list.
#' @slot plotObservations list.
#' @slot individualObservations list.
#' @slot aggregatedObservations list.
#' @slot stratumObservations list.
#' @slot strata list.
#' @slot individualOrganisms list.
#' @slot taxonNames list.
#' @slot attributes list.
#'
#' @return
#' @export
#'
#' @examples
setClass("VegX",slots=c(plots="list",
                        plotObservations = "list",
                        individualObservations = "list",
                        aggregatedObservations = "list",
                        stratumObservations = "list",
                        strata = "list",
                        individualOrganisms = "list",
                        taxonNames = "list",
                        attributes = "list"))
