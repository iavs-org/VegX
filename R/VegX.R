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
#' @slot strata list of stratum definitions.
#' @slot stratumObservations list of observations made on strata (e.g. overall tree cover).
#' @slot aggregateObservations list of observations made on aggregate organisms (e.g. plant abundance by taxa or stratum).
#' @slot individualOrganisms list of individual organisms.
#' @slot individualObservations list of observations made on individual organisms (e.g. diameter or height values).
#' @slot abioticObservations list of abiotic observations made plots.
#' @slot vegetationObservations list of observations applying to the whole stand (e.g. stand maturity)
#' @slot surfaceCovers list of surface cover definitions.
#' @slot surfaceCoverObservations list of surface cover observations.
#' @slot ancillaryObservations list of ancillary observations (e.g. land use, management regime, legal protection, ...)
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
                        strata = "list",
                        stratumObservations = "list",
                        aggregateObservations = "list",
                        individualOrganisms = "list",
                        individualObservations = "list",
                        vegetationObservations = "list",
                        surfaceCovers = "list",
                        surfaceCoverObservations = "list",
                        abioticObservations = "list",
                        ancillaryObservations = "list",
                        methods = "list",
                        attributes = "list"))

