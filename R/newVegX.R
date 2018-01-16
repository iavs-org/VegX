#' New VegX document
#'
#' Creates a new (empty) VegX document
#'
#'
#' @return an empty object of class \code{\linkS4class{VegX}}
#' @export
#'
#' @examples
newVegX<-function() {
  #project
  return(new("VegX",
             projects = list(),
             plots=list(),
             plotObservations = list(),
             taxonNameUsageConcepts = list(),
             individualObservations = list(),
             aggregatedObservations = list(),
             stratumObservations = list(),
             strata = list(),
             individualOrganisms = list(),
             methods = list(),
             attributes = list()))
}
