#' New VegX document
#'
#' Creates a new (empty) VegX document
#'
#' @param projectTitle a character string to identify the project title
#'
#' @return an object of class \code{\linkS4class{VegX}}
#' @export
#'
#' @examples
newVegX<-function(projectTitle) {
  #project
  projVector = vector("list", 1)
  names(projVector) = "1"
  projVector[[1]] = list("title" = projectTitle)
  return(new("VegX",
             projects = projVector,
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
