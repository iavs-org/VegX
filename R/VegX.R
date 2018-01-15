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
setMethod("summary", signature=c("VegX"), definition = function(object, ...) {
  cat(paste0("============================================================\n"))
  cat(paste0("                  VegX object (ver 1.5.3)                  \n"))
  cat(paste0("------------------------------------------------------------\n"))
  cat(paste0(" ", length(object@plotObservations)," plot observations"))
  cat(paste0(" made in ", length(object@plots)," plots, from ",length(object@projects)," projects.\n"))
  cat(paste0("\n"))
  cat(paste0("   Individual organisms: ", length(object@individualOrganisms),"\n"))
  cat(paste0("   Vegetation strata: ", length(object@strata),"\n"))
  cat(paste0("   Taxon names: ", length(object@taxonNames),"\n"))
  cat(paste0("   Taxon name usage concepts: ", length(object@taxonNameUsageConcepts),"\n"))
  cat(paste0("\n"))
  cat(paste0("   Individual organism observations: ", length(object@individualObservations),"\n"))
  cat(paste0("   Aggregated organism observations: ", length(object@aggregatedObservations),"\n"))
  cat(paste0("   Stratum observations: ", length(object@stratumObservations),"\n"))
  cat(paste0("\n"))
  cat(paste0("   Measurement methods: ", length(object@methods),"\n"))
  cat(paste0("============================================================\n"))
})
