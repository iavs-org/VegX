#' S4 class for Veg-X documents
#'
#' Implements the minimum subset of Veg-X elements needed for importing and combining vegetation data in this package.
#' Only main elements of Veg-X are included here as lists (subelements are not specified in the class definition).
#' Other main elements ('taxonConcept', 'taxonDetermination', 'communityConcept', 'communityDetermination') are not yet implemented.
#'
#' @slot projects list of research projects underpinning data collection
#' @slot plots list of vegetation plots where measurements were made.
#' @slot plotObservations list of vegetation plot observations.
#' @slot taxonNameUsageConcepts list of usages of taxon names as made by the observer.
#' @slot individualObservations list of observations made on individual organisms (e.g. diameter or height values).
#' @slot aggregatedObservations list of observations made on aggregated organisms (e.g. plant abundance by taxa or stratum).
#' @slot individualOrganisms list of individual organisms.
#' @slot abioticObservations list of abiotic observations made plots.
#' @slot strata list of stratum definitions.
#' @slot individualOrganisms list of individual organisms.
#' @slot methods list of measurement methods (e.g. cover scales).
#' @slot attributes list of attribute value description.
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @seealso \code{\link{newVegX}}
#'
#' @examples
#' showClass("VegX")
#'
setClass("VegX",slots=c(projects = "list",
                        plots="list",
                        plotObservations = "list",
                        taxonNameUsageConcepts = "list",
                        individualObservations = "list",
                        aggregatedObservations = "list",
                        stratumObservations = "list",
                        abioticObservations = "list",
                        strata = "list",
                        individualOrganisms = "list",
                        methods = "list",
                        attributes = "list"))
