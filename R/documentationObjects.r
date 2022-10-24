#' Objects to Support Data Mapping
#'
#' @description
#' A named list of different objects used to support the process of mapping the
#' names of fields and methods into the internal standard required by the
#' *VegX* function `prepareMapping()` (see the function help for other
#' details).
#'
#' Only the networks or databases that agreed to provide the equivalencies of
#' their datasets to the standards required are included here. If you want to
#' provide a map or method to be included as a default map within *VegX*,
#' please let us know \href{https://github.com/iavs-org/VegX/issues}{here}.
#'
#' @details
#' The list of supporting objects is:
#'
#' \itemize{
#'
#' \item{`reference_map`: a named list of empty vectors which works as a
#' reference for all fields included in the internal standard.}
#'
#' \item{`predefined_fields`: a vector of field names that are potentially
#' present in the internal file containing the standard used for mapping.}
#'
#' \item{`available_projects`: the list of selected networks or databases with
#' predefined mapping currently available in *VegX*.}
#'
#' \item{`predefined_map`: a data frame containing the predefined mapping
#' itself.}
#'
#' \item{`available_methods`: a vector of predefined method names that are
#' currently available in *VegX*.}
#'
#' }
#'
#' @name supporting_info
#' @docType data
#' @keywords data
#' @format A Named List
#' @source \url{INCLUDELINKHERE}
NULL

