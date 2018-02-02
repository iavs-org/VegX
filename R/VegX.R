#' S4 class for Veg-X documents
#'
#' Implements the minimum subset of Veg-X elements needed for importing and combining vegetation data in this package.
#' Only main elements of Veg-X are included here as lists (subelements are not specified in the class definition).
#' Other main elements ('taxonConcept', 'taxonDetermination', 'communityConcept', 'communityDetermination') are not yet implemented.
#'
#' @slot VegXVersion A string to indicate the version of the Veg-X standard.
#' @slot projects A list of research projects underpinning data collection.
#' @slot plots A list of vegetation plots where measurements were made.
#' @slot plotObservations A list of vegetation plot observations.
#' @slot organismIdentities A list of organism identities.
#' @slot strata A list of stratum definitions.
#' @slot stratumObservations A list of observations made on strata (e.g. overall tree cover).
#' @slot aggregateObservations A list of observations made on aggregate organisms (e.g. plant abundance by taxa or stratum).
#' @slot individualOrganisms A list of individual organisms.
#' @slot individualObservations A list of observations made on individual organisms (e.g. diameter or height values).
#' @slot siteObservations A list of observations that apply to the site (e.g., abiotic measurements, land use, management regime, legal protection, ...).
#' @slot communityObservations list of observations applying to the whole plant community (e.g. stand age or species richness)
#' @slot surfaceTypes A list of surface class definitions.
#' @slot surfaceCoverObservations list of surface cover observations.
#' @slot methods A list of measurement methods (e.g. cover scales).
#' @slot attributes A list of attribute value description.
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @seealso \code{\link{newVegX}}, \code{\link{showElementTable}}
#'
#' @examples
#' showClass("VegX")
#'
setClass("VegX",slots=c(VegXVersion = "character",
                        projects = "list",
                        plots="list",
                        plotObservations = "list",
                        organismIdentities = "list",
                        strata = "list",
                        stratumObservations = "list",
                        aggregateObservations = "list",
                        individualOrganisms = "list",
                        individualObservations = "list",
                        communityObservations = "list",
                        surfaceTypes = "list",
                        surfaceCoverObservations = "list",
                        siteObservations = "list",
                        methods = "list",
                        attributes = "list"))

