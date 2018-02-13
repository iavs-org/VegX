#' Add taxon concepts
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one organism identity, given by a column that can be mapped to \code{originalOrganismName}.
#' @param mapping A named list whose elements are strings that correspond to column names in \code{x}. Names of the list should be:
#'  \itemize{
#'    \item{\code{originalOrganismName} - A string with the original name given by the author of the data set (required).}
#'    \item{\code{taxonConceptName} - A string with the taxon name forming the taxon concept, if different from \code{originalOrganismName} (optional).}
#'    \item{\code{taxonConceptCitation} - A string with the bibliographic citation forming the taxon concept (optional).}
#'  }
#' @param citationStringAll A string with the bibliographic citation to be applied to all organism identities of the VegX object (using the original organism names as taxon names),
#' or to all original organism names listed in \code{x}.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#' 
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family add functions
#'
#' @examples
addTaxonConcepts<-function(target, citationStringAll = "", x = NULL, mapping = list()) {
  
  return(target)
}