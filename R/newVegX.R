#' New VegX document
#'
#' Creates a new (empty) VegX document (ver. 2.0.0)
#'
#'
#' @return An empty object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @examples
#'
#' # Creates new Veg-X document
#' newVegX()
#'
newVegX<-function() {
  return(new("VegX",
             VegXVersion = "2.0.0",
             projects = list(),
             parties = list(),
             literatureCitations = list(),
             methods = list(),
             attributes = list(),
             organismNames = list(),
             taxonConcepts = list(),
             organismIdentities = list(),
             surfaceTypes = list(),
             strata = list(),
             plots=list(),
             plotObservations = list(),
             individualObservations = list(),
             aggregateObservations = list(),
             stratumObservations = list(),
             individualOrganisms = list(),
             communityObservations = list(),
             surfaceCoverObservations = list(),
             siteObservations = list()))
}
