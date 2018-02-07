#' Applies a preferred taxon nomenclature to the organism identities of a data set using a lookup table
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to a different value of 'originalOrganismName'.
#' @param mapping A named list with element names 'originalOrganismName', 'preferredTaxonName'
#' @param verbose A boolean flag to indicate console output of the nomenclatural change process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @family transform functions
#'
#' @examples
#'
transformTaxonNomenclature<-function(target, x, mapping,
                                     verbose = TRUE) {



}
