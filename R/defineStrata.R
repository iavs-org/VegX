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
defineHeightStrata<-function(name = "Strata by height",
                               description = "Vegetation strata defined by height in m",
                               citation = "",
                               heightBreaks = c(0,1,3,5),
                               stratumNames = c("s1", "s2", "s3"), heightUnit = "m") {
  attributes = list(
    list(type="quantitative",
         unit = heightUnit,
         lowerBound = 0,
         upperBound = Inf)
  )
  names(attributes) = 1
  defMethod = new("VegXMethod",
             name = name,
             description = description,
             citation = citation,
             subject = "stratum height",
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
defineCategoricalStrata<-function(name = "Strata by categories",
                                  description = "Vegetation categorical strata",
                                  citation = "",
                                  stratumNames = c("s1", "s2", "s3")) {
  defMethod = new("VegXMethod",
                  name = name,
                  description = description,
                  citation = citation,
                  subject = "stratum category",
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

#' Mixed stratum definiton
#'
#' Define some strata by height and other by category
#'
#' @param name A string to identify the stratum definition.
#' @param description A string describing how strata are defined.
#' @param citation A string with the bibliographic reference for the method.
#' @param heightStrataBreaks A numeric vector with height limits between height strata (of length equal to the number of height strata plus one).
#' @param heightStrataNames A numeric vector of stratum codes (of length equal to the number of height strata).
#' @param heightStrataUnit A string to identify height units.
#' @param categoryStrataNames A numeric vector of categorical stratum codes (of length equal to the number of categorical strata).
#' @param order A numeric vector to specify order strata (indices starting from height strata and continuing with category strata).
#'
#' @return an object of class \code{\linkS4class{VegXStrata}}
#' @export
#'
#' @examples
#' strataDef = defineMixedStrata(name = "Recce strata",
#'                               description = "Standard Recce stratum definition",
#'                               citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                               heightStrataBreaks = c(0, 0.3,2.0,5, 12, 25, 50),
#'                               heightStrataNames = paste0("Tier ",1:6),
#'                               categoryStrataNames = "Tier 7")
defineMixedStrata<-function(name = "Strata by height or category",
                            description = "Vegetation strata defined by height in m and other strata defined by category",
                            citation = "",
                            heightStrataBreaks = c(0,1,3,5),
                            heightStrataNames = c("s1", "s2", "s3"),
                            heightStrataUnit = "m",
                            categoryStrataNames = "s4",
                            order = NULL) {
  attributes = list(
    list(type="quantitative",
         unit = heightStrataUnit,
         lowerBound = 0,
         upperBound = Inf)
  )
  names(attributes) = 1
  defMethod = new("VegXMethod",
                  name = name,
                  description = description,
                  citation = citation,
                  subject = "stratum mixed",
                  attributeType = "quantitative",
                  attributes = attributes)

  nhstr = length(heightStrataNames)
  ncstr = length(categoryStrataNames)
  nstr =nhstr + ncstr
  strata = list()
  if(is.null(order)) order = 1:nstr
  for(i in 1:nhstr) {
    strata[[as.character(i)]] = list(stratumName = heightStrataNames[i],
                                     stratumSequence = order[i],
                                     lowerBound = heightStrataBreaks[i],
                                     upperBound = heightStrataBreaks[i+1])
  }
  for(i in 1:ncstr) {
    strata[[as.character(nhstr+i)]] = list(stratumName = categoryStrataNames[i],
                                           stratumSequence = order[nhstr+i])
  }
  return(new("VegXStrata",
             method = defMethod,
             strata = strata))
}
