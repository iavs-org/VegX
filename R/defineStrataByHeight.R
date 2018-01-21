#' Stratum definition by height
#'
#' Height-based strata definition
#'
#' @param name A string to identify the stratum definition.
#' @param description A string describing how strata are defined.
#' @param citation A string with the bibliographic reference for the method.
#' @param heightBreaks A numeric vector with height limits between strata (of length equal to the number of strata plus one).
#' @param stratumNames A numeric vector of stratum codes (of length equal to the number of strata).
#' @param heightUnit A string to identify height units.
#'
#' @return an object of class \code{\linkS4class{VegXStrata}}
#' @export
#'
#' @examples
#' strataDef = defineStrataByHeight(name = "Recce strata",
#'                                 description = "Standard Recce stratum definition",
#'                                 citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                                 heightBreaks = c(0, 0.3,2.0,5, 12, 25,50, 100),
#'                                 stratumNames = paste0("Tier ",1:7))
#'
defineStrataByHeight<-function(name = "Strata by height",
                               description = "Vegetation strata defined by height in m",
                               citation = "",
                               heightBreaks = c(0,1,3,5),
                               stratumNames = c("s1", "s2", "s3"), heightUnit = "m") {
  attributes = list(
    list(type="quantitative",
         unit = heightUnit,
         lowerBound = 0)
  )
  names(attributes) = 1
  defMethod = new("VegXMethod",
             name = name,
             description = description,
             citation = citation,
             attributeClass = "stratum height",
             attributeType = "quantitative",
             attributes = attributes)

  strata = list()
  for(i in 1:(length(heightBreaks)-1)) {
     strata[[as.character(i)]] = list(stratumName = stratumNames[i],
                        stratumSequence = i,
                        lowerBound = heightBreaks[i],
                        upperBound = heightBreaks[i+1])
  }
  return(new("VegXStrata",
             method = defMethod,
             strata = strata))
}

#' Strata definition by simple category
#'
#' @param name A string to identify the stratum definition.
#' @param description A string describing how strata are defined.
#' @param citation A string with the bibliographic reference for the method.
#' @param stratumNames A numeric vector of stratum codes (of length equal to the number of strata).
#'
#' @return an object of class \code{\linkS4class{VegXStrata}}
defineStratumCategories<-function(name = "Strata by categories",
                                  description = "Vegetation categorical strata",
                                  citation = "",
                                  stratumNames = c("s1", "s2", "s3")) {
  defMethod = new("VegXMethod",
                  name = name,
                  description = description,
                  citation = citation,
                  attributeClass = "stratum category",
                  attributeType = "qualitative",
                  attributes = list())

  strata = list()
  for(i in 1:length(stratumNames)) {
    strata[[as.character(i)]] = list(stratumName = stratumNames[i],
                                     stratumSequence = i)
  }
  return(new("VegXStrata",
             method = defMethod,
             strata = strata))
}

#' Surface cover definition by simple category
#'
#' @param name A string to identify the surface cover definition.
#' @param description A string describing how surface covers are defined.
#' @param citation A string with the bibliographic reference for the method.
#' @param surfaceNames A numeric vector of surface codes (of length equal to the number of strata).
#'
#' @return an object of class \code{\linkS4class{VegXStrata}}
defineSurfaceCategories<-function(name = "Surface covers",
                                  description = "Four simple surface categories",
                                  citation = "",
                                  surfaceNames = c("bare soil", "water", "rock", "vegetation")) {
  defMethod = new("VegXMethod",
                  name = name,
                  description = description,
                  citation = citation,
                  attributeClass = "surface category",
                  attributeType = "qualitative",
                  attributes = list())

  surfaceCovers = list()
  for(i in 1:length(surfaceNames)) {
    surfaceCovers[[as.character(i)]] = list(surfaceNames = surfaceNames[i])
  }
  return(new("VegXSurfaceCoverDefinition",
             method = defMethod,
             surfaceCovers = surfaceCovers))
}

