#' New VegX document
#'
#' Creates a new (empty) VegX document
#'
#'
#' @return an empty object of class \code{\linkS4class{VegX}}
#' @export
#'
#' @examples
#'
#' # Create new Veg-X document
#' newVegX()
#'
newVegX<-function() {
  return(new("VegX",
             projects = list(),
             plots=list(),
             plotObservations = list(),
             taxonNameUsageConcepts = list(),
             individualObservations = list(),
             aggregateObservations = list(),
             stratumObservations = list(),
             strata = list(),
             individualOrganisms = list(),
             methods = list(),
             attributes = list(),
             siteObservations = list()))
}
