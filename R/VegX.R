#' S4 class for Veg-X documents
#'
#' Implements the minimum subset of Veg-X elements needed for importing and combining vegetation data in this package.
#' Only main elements of Veg-X are included here as lists (sub-elements are not specified in the class definition).
#' Other main Veg-X elements ('taxonDetermination', 'communityConcept', 'communityDetermination') are not yet implemented.
#'
#' @slot VegXVersion A string to indicate the version of the Veg-X standard.
#' @slot projects A list of research projects underpinning data collection.
#' @slot parties A list of persons, organisations cited in the data set.
#' @slot literatureCitations A list of literature citations.
#' @slot methods A list of measurement methods (e.g. cover scales).
#' @slot attributes A list of attribute value description.
#' @slot organismNames A list of organism names used in the data set.
#' @slot taxonConcepts A list of taxon concepts (i.e. taxon name + citation) used in the data set.
#' @slot organismIdentities A list of organism identities.
#' @slot strata A list of stratum definitions.
#' @slot surfaceTypes A list of surface class definitions.
#' @slot plots A list of vegetation plots where measurements were made.
#' @slot individualOrganisms A list of individual organisms.
#' @slot plotObservations A list of vegetation plot observations.
#' @slot individualObservations A list of observations made on individual organisms (e.g. diameter or height values).
#' @slot aggregateObservations A list of observations made on aggregate organisms (e.g. plant abundance by taxa or stratum).
#' @slot stratumObservations A list of observations made on strata (e.g. overall tree cover).
#' @slot communityObservations list of observations applying to the whole plant community (e.g. stand age or species richness)
#' @slot siteObservations A list of observations that apply to the site (e.g., abiotic measurements, land use, management regime, legal protection, ...).
#' @slot surfaceCoverObservations list of surface cover observations.
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
                        parties = "list",
                        literatureCitations = "list",
                        methods = "list",
                        attributes = "list",
                        organismNames = "list",
                        taxonConcepts = "list",
                        organismIdentities = "list",
                        strata = "list",
                        surfaceTypes = "list",
                        plots="list",
                        individualOrganisms = "list",
                        plotObservations = "list",
                        individualObservations = "list",
                        aggregateObservations = "list",
                        stratumObservations = "list",
                        communityObservations = "list",
                        siteObservations = "list",
                        surfaceCoverObservations = "list"))

