#' List of predefined quantitative methods
#'
#' This data frame contains the definition of predefined quantitative methods available for Veg-X. 
#' These are used by calling function \code{\link{predefinedMeasurementMethod}} or directly by their name. 
#' When a string refers to a subject, the default method for that subject is used.
#'
#' \itemize{
#'  \item subject The description of an attribute class for comparative purposes.
#'  \item name The name of the measurement method (normally includes units)
#'  \item description Short description of the measurement method (subject and units)
#'  \item unit Measurement units
#'  \item lowerLimit Lower limit of the quantitative scale, if defined
#'  \item upperLimit Upper limit of the quantitative scale, if defined
#' }
#'
#' @name quantitative_methods
#' @aliases quantitative_methods
#' @docType data
#' @author Veg-X team.
#' @keywords data
#' @seealso \code{\link{predefinedMeasurementMethod}}, \code{\link{defineQuantitativeScaleMethod}}
#' @examples
#' 
#' # To explore available methods
#' data("quantitative_methods")
#' 
#' #To use one of them within Veg-X
#' predefinedMeasurementMethod("Plant cover/%")
NULL

