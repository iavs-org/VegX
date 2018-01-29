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
#' # Create new Veg-X document
#' newVegX()
#'
newVegX<-function() {
  return(new("VegX",
             VegXVersion = "2.0.0",
             projects = list(),
             plots=list(),
             plotObservations = list(),
             taxonNameUsageConcepts = list(),
             individualObservations = list(),
             aggregateObservations = list(),
             stratumObservations = list(),
             strata = list(),
             individualOrganisms = list(),
             communityObservations = list(),
             surfaceCovers = list(),
             surfaceCoverObservations = list(),
             siteObservations = list(),
             methods = list(),
             attributes = list()))
}
