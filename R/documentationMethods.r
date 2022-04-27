#' @title List of predefined quantitative methods
#'
#' @description This data frame contains the definition of predefined quantitative methods
#' available for Veg-X. These are used by calling function
#' \code{\link{predefinedMeasurementMethod}} or directly by their name. When a
#' string refers to a subject, the default method for that subject is used.
#' 
#' The main columns contained in this data frame are:
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

#' @title List of predefined qualitative methods
#'
#' @description  This list contains the definition of predefined qualitative
#'   methods available for Veg-X.
#' 
#' The main elements contained in this list of methods are:
#' \itemize{
#'  \item name The name of the method
#'  \item description Short description of the method
#'  \item subject The description of an attribute class for comparative purposes.
#'  \item citationString The bibliographic reference containing the method
#'  description (if available)
#'  \item code The class codes of the qualitative variable
#'  \item definition The definitions of the class codes of the qualitative variable
#' }
#'
#' @name qualitative_methods
#' @aliases qualitative_methods
#' @docType data
#' @author Veg-X team.
#' @keywords data
#' @seealso \code{\link{defineQualitativeScaleMethod}}
#' @examples
#' 
#' # To explore available methods
#' names(qualitative_methods)
#' 
NULL

#' @title List of predefined ordinal methods
#'
#' @description  This list contains the definition of predefined ordinal methods
#'   available for Veg-X.
#' 
#' The main elements contained in this list of methods are:
#' \itemize{
#'  \item name The name of the method
#'  \item description Short description of the method
#'  \item subject The description of an attribute class for comparative
#'  purposes.
#'  \item citationString The bibliographic reference containing the method
#'  description (if available)
#'  \item code The ordinal codes of the ordinal variable
#'  \item definition The definitions of the codes of the ordinal variable
#'  \item lowerLimit, upperLimit and midPoint The limits and midpoint of the
#'  ordinal codes
#'  
#' }
#'
#' @name ordinal_methods
#' @aliases ordinal_methods
#' @docType data
#' @author Veg-X team.
#' @keywords data
#' @seealso \code{\link{defineOrdinalScaleMethod}}
#' @examples
#' 
#' # To explore available methods
#' names(ordinal_methods)
#' 
NULL