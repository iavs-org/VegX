#' Add tree observation records
#'
#' Adds tree observation records to a VegX object from a data table,
#' using a mapping to identify columns: plot, observation date, taxon name and diameter.
#' Additional mappings can be used to specify a stratum where the tree is located.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one tree observation. Columns can be varied.
#' @param projectTitle A string to identify the project title, which can be the same as one of the currently defined in \code{target}.
#' @param mapping A list with element names 'plotName', 'obsStartDate', 'taxonAuthorName' and 'diameter', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#'                Additional optional mappings are: 'subPlot', 'obsEndDate', 'individual', 'height' and 'stratumName'.
#' @param diameterMethod
#' @param heightMethod
#' @param stratumDefinition An object of class \code{\linkS4class{VegXStrata}} indicating the definition of strata.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @examples
addTreeObservationRecords<-function(target, x, projectTitle,
                                    mapping,
                                    diameterMethod,
                                    heightMethod = NULL,
                                    stratumDefinition = NULL,
                                    verbose = TRUE) {
}
