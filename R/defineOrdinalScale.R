#' Ordinal scale definition
#'
#' A general function to define an ordinal scale for a given subject (measurement)
#'
#' @param name String with the name of the ordinal scale
#' @param description String describing the ordinal scale
#' @param subject A string to identify the subject
#' @param values A character vector of class codes
#' @param citation A string with the bibliographic reference for the method.
#' @param quantifiableValues A string vector with the subset of \code{values} that have correspondence in a quantitative scale. These values
#' can be transformed to midPoints (if supplied in \code{midPoints}) and, correspondingly, values in the quantitative scale can be transformed to ordinal values
#' using class limits (if supplied in \code{breaks}).
#' @param breaks A vector of break points (for ordinal scales with class limits)
#' @param midPoints A vector of class midpoints (for ordinal scales that can be translated into quantitative values)
#' @param definitions A character vector of class definitions
#'
#' @return an object of class \code{\linkS4class{VegXMethod}}
#' @export
#'
#' @family define measurement functions
#'
#' @examples
#'
#' #Ordinal scale with three levels
#' defineOrdinalScale("scale1", "Description for scale1", "subject1",
#'                    values = c("first", "second","third"))
#'
#' #Braun-Blanquet plant cover scale with five levels that can be translated to cover values
#' defineOrdinalScale(name = "Braun-Blanquet",
#'                    description = "Five-level Braun-Blanquet cover scale",
#'                    subject = "plant cover",
#'                    values = as.character(1:5),
#'                    quantifiableValues = as.character(1:5),
#'                    breaks = c(0,5,25,50,75,100),
#'                    midPoints = c(2.5,17.5, 37.5, 62.5, 87.5))
#'
#'
#' #Ordinal plant cover scale with seven levels where six can be translated to a quantitative scale
#' defineOrdinalScale(name = "Recce cover scale",
#'                    description = "Recce recording method by Hurst/Allen",
#'                    subject = "plant cover",
#'                    citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                    values = c("P","1","2","3", "4", "5", "6"),
#'                    quantifiableValues = c("1","2","3", "4", "5", "6"),
#'                    breaks = c(0, 1, 5, 25, 50, 75, 100),
#'                    midPoints = c(0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                    definitions = c("Presence", "<1%", "1-5%","6-25%", "26-50%", "51-75%", "76-100%"))
#'
defineOrdinalScale<-function(name, description, subject, values,
                             citation = "",
                             quantifiableValues = character(0),
                             breaks = NULL,
                             midPoints = NULL,
                             definitions = NULL) {
  nvals = length(values)
  attributes = vector("list", nvals)

  if(!is.null(breaks)) if(length(breaks)!= (length(quantifiableValues)+1)) stop("'breaks' has to have a length equal to the length of 'quantifiableValues' plus one.")
  if(!is.null(midPoints)) if(length(midPoints)!= length(quantifiableValues)) stop("'midPoints' has to be of the same length as 'quantifiableValues'.")
  if(sum(quantifiableValues %in% values)< length(quantifiableValues)) stop("'quantifiableValues' has to be a subset of 'values'")

  if(!is.null(definitions)) if(length(definitions)!= length(values)) stop("'definitions' has to be of the same length as 'values'.")

  cnt = 1
  for(i in 1:nvals) {
    attributes[[i]] = list(type = "ordinal",
                           code = values[i],
                           order = i)
    if(!is.null(definitions)) attributes[[i]]$definition = definitions[[i]]
    if(values[i] %in% quantifiableValues) {
      if(!is.null(breaks)) {
        attributes[[i]]$lowerLimit = breaks[cnt]
        attributes[[i]]$upperLimit = breaks[cnt+1]
      }
      if(!is.null(midPoints)) {
        attributes[[i]]$midPoints = midPoints[cnt]
      }
      cnt = cnt + 1
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
