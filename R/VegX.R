#' S4 class for Veg-X documents
#'
#' Implements the minimum subset of Veg-X elements needed for importing and combining vegetation data in this package.
#' Only main elements of Veg-X are included here as lists (sub-elements are not specified in the class definition).
#' Other main Veg-X elements ('taxonDetermination', 'communityConcept', 'communityDetermination') are not yet implemented.
#'
#' @slot VegXVersion A string to indicate the version of the Veg-X standard.
#' @slot parties A list of persons, organisations mentioned in the data set.
#' @slot literatureCitations A list of literature citations used in the data set.
#' @slot methods A list of measurement methods (e.g. cover scales) mentioned in the data set.
#' @slot attributes A list of attribute value descriptions used in the data set.
#' @slot strata A list of stratum definitions used in the data set.
#' @slot surfaceTypes A list of surface class definitions used in the data set.
#' @slot organismNames A list of organism names used in the data set.
#' @slot taxonConcepts A list of taxon concepts (i.e. taxon name + citation) used in the data set.
#' @slot organismIdentities A list of organism identities used in the data set.
#' @slot projects A list of research projects underpinning plot data collection.
#' @slot plots A list of vegetation plots where measurements were made.
#' @slot individualOrganisms A list of individual organisms found in vegetation plots.
#' @slot plotObservations A list of observations (i.e. sampling events) made on vegetation plots.
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
#' @exportClass VegX
#' 
setClass("VegX",slots=c(VegXVersion = "character",
                        parties = "list",
                        literatureCitations = "list",
                        methods = "list",
                        attributes = "list",
                        strata = "list",
                        surfaceTypes = "list",
                        organismNames = "list",
                        taxonConcepts = "list",
                        organismIdentities = "list",
                        projects = "list",
                        plots="list",
                        individualOrganisms = "list",
                        plotObservations = "list",
                        individualObservations = "list",
                        aggregateObservations = "list",
                        stratumObservations = "list",
                        communityObservations = "list",
                        siteObservations = "list",
                        surfaceCoverObservations = "list"))

