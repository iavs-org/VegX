#' Quantitative scale method definition
#'
#' A general function to define a method for measurements in a quantitative scale of a given subject
#'
#' @param name String with the name of the method.
#' @param description String describing the method.
#' @param subject A string to identify the subject.
#' @param unit A character describing measurement units.
#' @param citationString A string with the bibliographic reference for the method.
#' @param DOI A string with the DOI the resource related to \code{citationString}
#' @param lowerLimit The lower limit of the quantitative scale, if defined.
#' @param upperLimit The upper limit of the quantitative scale, if defined.
#'
#' @return an object of class \code{\linkS4class{VegXMethodDefinition}}
#' @export
#'
#' @family define measurement functions
#'
#' @examples
#'
#' defineQuantitativeScaleMethod(name = "Percent cover",
#'                               description = "Quantitative plant percent cover",
#'                               unit = "%",
#'                               subject = "plant cover",
#'                               lowerLimit = 0, upperLimit = 100)
#'
defineQuantitativeScaleMethod<-function(name, description, subject, unit,
                                        citationString = "", DOI = "",
                                        lowerLimit = -Inf,
                                        upperLimit = Inf) {
  attributes = list(
    list(type="quantitative",
         unit = unit,
         lowerLimit = lowerLimit,
         upperLimit = upperLimit)
  )
  names(attributes) = 1
  return(new("VegXMethodDefinition",
             name = name,
             description = description,
             subject = subject,
             citationString = citationString,
             DOI = DOI,
             attributeType = "quantitative",
             attributes = attributes))
}
