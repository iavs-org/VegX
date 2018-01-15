#' Veg-X S4 class
#'
#' Implements the minimum subset of Veg-X elements for importing and
#' combining vegetation data
#'
#' @slot plots list.
#' @slot plotObservations list.
#' @slot taxonNames list.
#' @slot taxonNameUsageConcepts list.
#' @slot individualObservations list.
#' @slot aggregatedObservations list.
#' @slot stratumObservations list.
#' @slot strata list.
#' @slot individualOrganisms list.
#' @slot attributes list.
#'
#' @return
#' @export
#'
#' @examples
setClass("VegX",slots=c(plots="list",
                        plotObservations = "list",
                        taxonNames = "list",
                        taxonNameUsageConcepts = "list",
                        individualObservations = "list",
                        aggregatedObservations = "list",
                        stratumObservations = "list",
                        strata = "list",
                        individualOrganisms = "list",
                        methods = "list",
                        attributes = "list"))
