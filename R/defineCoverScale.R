#' Cover scale definition
#'
#' Defines a cover scale for aggregated organism observations or stratum observations
#'
#' @param name String with the name of the cover scale
#' @param description String describing the cover scale
#' @param breaks A vector of cover break points
#' @param midPoints A vector of cover class midpoints
#' @param values A character vector of class codes
#'
#' @return an object of class \code{\linkS4class{VegXMethod}}
#' @export
#'
#' @examples
defineCoverScale<-function(name = "Braun-Blanquet", description = "Five-level Braun-Blanquet cover scale",
                     breaks = c(0,5,25,50,75,100),
                     midPoints = c(2.5,17.5, 37.5, 62.5, 87.5),
                     values = as.character(1:5)) {
  nvals = length(values)
  attributes = vector("list", nvals)
  for(i in 1:nvals) attributes[[i]] = list(type = "ordinal", code = values[i], order = i, lowerLimit = breaks[i],
                                           upperLimit = breaks[i+1], midPoint = midPoints[i])
  names(attributes) = 1:nvals
  return(new("VegXMethod",
             name = name,
             description = description,
             attributeClass = "cover",
             attributeType = "ordinal",
             attributes = attributes))
}
