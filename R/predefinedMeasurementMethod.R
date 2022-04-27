#' Predefined measurement methods
#'
#' Creates a measurement method for Veg-X documents from a set of predefined options
#'
#' @param name A string with the desired measurement method or a subject (in this case, the default method is returned). 
#' The list of available predefined methods and subjects is found in \code{\link{quantitative_methods}}.
#'
#'
#' @return an object of class \code{\linkS4class{VegXMethodDefinition}}
#'
#' @family define measurement functions
#'
#' @examples
#' # Create a method for plant percent cover
#' predefinedMeasurementMethod("Plant cover/%")
#'
#' @export
predefinedMeasurementMethod <- function(name) {
  quantitative_methods = get("quantitative_methods")

  m = NULL
  l = -1
  if (name %in% quantitative_methods$name) {
    l = which(quantitative_methods$name == name)
  } else if(name %in% quantitative_methods$subject) {
    s = which(quantitative_methods$subject == name)
    l = s[which(quantitative_methods$default[s] == "Yes")]
  } 
  if (l != -1) {
    m = defineQuantitativeScaleMethod(
          name = quantitative_methods$name[l],
          description = quantitative_methods$description[l],
          subject = quantitative_methods$subject[l],
          unit = quantitative_methods$unit[l],
          lowerLimit = as.numeric(quantitative_methods$lowerLimit[l]),
          upperLimit = as.numeric(quantitative_methods$upperLimit[l]),
          citationString = ifelse(is.na(quantitative_methods$citationString[l]),
                                  "", quantitative_methods$citationString[l]), 
          DOI = ifelse(is.na(quantitative_methods$DOI[l]), 
                       "", quantitative_methods$DOI[l])
    )
    return(m)
  } else {
    stop(paste0(name, " is not among the predefined methods."))
  }
}
