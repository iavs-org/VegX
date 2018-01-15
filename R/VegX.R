#' Veg-X S4 class
#'
#' Implements the minimum subset of Veg-X elements for importing and
#' combining vegetation data
#'
#' @slot projects list of research projects underpinning data collection
#' @slot plots list of vegetation plots where measurements were made.
#' @slot plotObservations list of vegetation plot observations.
#' @slot taxonNames list of scientific taxon names referenced in the data.
#' @slot taxonNameUsageConcepts list of usages of taxon names.
#' @slot individualObservations list of observations made on individual organisms (e.g. diameter or height values).
#' @slot aggregatedObservations list of observations made on aggregated organisms (e.g. plant abundance by taxa or stratum).
#' @slot stratumObservations list of observations made on strata (e.g. herb or shrub cover).
#' @slot strata list of stratum definitions.
#' @slot individualOrganisms list of individual organisms.
#' @slot methods list of measurement methods (e.g. cover scales).
#' @slot attributes list of attribute value description.
#'
#' @return
#' @export
#'
#' @examples
setClass("VegX",slots=c(projects = "list",
                        plots="list",
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
