#' Ordinal scale definition
#'
#' Defines an oridnal scale for a given subject (measurement)
#'
#' @param name String with the name of the ordinal scale
#' @param description String describing the ordinal scale
#' @param subject A string to identify the subject
#' @param values A character vector of class codes
#' @param citation A string with the bibliographic reference for the method.
#' @param breaks A vector of break points (for ordinal scales with class limits)
#' @param midPoints A vector of class midpoints (for ordinal scales that can be translated into quantitative values)
#'
#' @return an object of class \code{\linkS4class{VegXMethod}}
#' @export
#'
defineOrdinalScale<-function(name, description, subject, values,
                             citation = "",
                             breaks = NULL,
                             midPoints = NULL) {
  nvals = length(values)
  attributes = vector("list", nvals)
  for(i in 1:nvals) {
    attributes[[i]] = list(type = "ordinal",
                           code = values[i],
                           order = i)
    if(!is.null(breaks)) {
      attributes[[i]]$lowerLimit = breaks[i]
      attributes[[i]]$upperLimit = breaks[i+1]
    }
    if(!is.null(midPoints)) {
      attributes[[i]]$midPoints = midPoints[i]
    }
  }
  names(attributes) = 1:nvals
  return(new("VegXMethod",
             name = name,
             description = description,
             citation = citation,
             subject = "subject",
             attributeType = "ordinal",
             attributes = attributes))
}
