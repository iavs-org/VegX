#' Stratum definition by height
#'
#' Height-based strata definition
#'
#' @param name A string to identify the stratum definition.
#' @param description A string describing how strata are defined.
#' @param citationString A string with the bibliographic reference for the method.
#' @param DOI A string with the DOI of a resource describing the method.
#' @param heightBreaks A numeric vector with height limits between strata (of length equal to the number of strata plus one).
#' @param heightUnit A string to identify height units.
#' @param strataNames A character vector of stratum codes (of length equal to the number of strata).
#' @param strataDefinitions A character vector with strata definitions
#'
#' @return An object of class \code{\linkS4class{VegXStrataDefinition}}
#'
#' @family define strata functions
#'
defineHeightStrata<-function(name = "Strata by height",
                             description = "Vegetation strata defined by height in m",
                             citationString = "", DOI = "",
                             heightBreaks = c(0,1,3,5),
                             heightUnit = "m",
                             strataNames = c("s1", "s2", "s3"),
                             strataDefinitions = NULL) {
  attributes = list(
    list(type="quantitative",
         unit = heightUnit,
         lowerLimit = 0,
         upperLimit = Inf)
  )
  names(attributes) = 1
  defMethod = new("VegXMethodDefinition",
             name = name,
             description = description,
             citationString = citationString,
             DOI = DOI,
             subject = "stratum height",
             attributeType = "quantitative",
             attributes = attributes)

  strata = list()
  for(i in 1:(length(heightBreaks)-1)) {
     strata[[as.character(i)]] = list(stratumName = strataNames[i],
                                      order = i,
                                      lowerLimit = heightBreaks[i],
                                      upperLimit = heightBreaks[i+1])
     if(!is.null(strataDefinitions)) strata[[as.character(i)]]$definition = strataDefinitions[i]
  }
  return(new("VegXStrataDefinition",
             method = defMethod,
             strata = strata))
}

#' Strata definition by simple category
#'
#' @param name A string to identify the stratum definition.
#' @param description A string describing how strata are defined.
#' @param citationString A string with the bibliographic reference for the method.
#' @param DOI A string with the DOI of a resource describing the method.
#' @param strataNames A character vector of stratum codes (of length equal to the number of strata).
#' @param strataDefinitions A character vector with stratum definitions
#'
#' @return An object of class \code{\linkS4class{VegXStrataDefinition}}
#' @family define strata functions
#'
defineCategoricalStrata<-function(name = "Strata by categories",
                                  description = "Vegetation categorical strata",
                                  citationString = "", DOI = "",
                                  strataNames = c("s1", "s2", "s3"),
                                  strataDefinitions = NULL) {
  defMethod = new("VegXMethodDefinition",
                  name = name,
                  description = description,
                  citationString = citationString,
                  DOI = DOI,
                  subject = "stratum category",
                  attributeType = "qualitative",
                  attributes = list())

  strata = list()
  for(i in 1:length(strataNames)) {
    strata[[as.character(i)]] = list(stratumName = strataNames[i],
                                     order = i)
    if(!is.null(strataDefinitions)) strata[[as.character(i)]]$definition = strataDefinitions[i]
  }
  return(new("VegXStrataDefinition",
             method = defMethod,
             strata = strata))
}

#' Mixed stratum definiton
#'
#' Define some strata by height and other by category
#'
#' @param name A string to identify the stratum definition.
#' @param description A string describing how strata are defined.
#' @param citationString A string with the bibliographic reference for the method.
#' @param DOI A string with the DOI of a resource describing the method.
#' @param heightStrataBreaks A numeric vector with height limits between height strata (of length equal to the number of height strata plus one).
#' @param heightStrataUnit A string to identify height units.
#' @param heightStrataNames A numeric vector of stratum codes (of length equal to the number of height strata).
#' @param categoryStrataNames A numeric vector of categorical stratum codes (of length equal to the number of categorical strata).
#' @param heightStrataDefinitions A character vector with height strata definitions
#' @param categoryStrataDefinitions A character vector with category strata definitions
#' @param order A numeric vector to specify order strata (indices starting from height strata and continuing with category strata).
#'
#' @return An object of class \code{\linkS4class{VegXStrataDefinition}}
#'
#' @family define strata functions
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
                            citationString = "", DOI = "",
                            heightStrataBreaks = c(0,1,3,5),
                            heightStrataUnit = "m",
                            heightStrataNames = c("s1", "s2", "s3"),
                            categoryStrataNames = "s4",
                            heightStrataDefinitions = NULL,
                            categoryStrataDefinitions = NULL,
                            order = NULL) {
  attributes = list(
    list(type="quantitative",
         unit = heightStrataUnit,
         lowerLimit = 0,
         upperLimit = Inf)
  )
  names(attributes) = 1
  defMethod = new("VegXMethodDefinition",
                  name = name,
                  description = description,
                  citationString = citationString,
                  DOI = DOI,
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
                                     order = order[i],
                                     lowerLimit = heightStrataBreaks[i],
                                     upperLimit = heightStrataBreaks[i+1])
    if(!is.null(heightStrataDefinitions)) strata[[as.character(i)]]$definition = heightStrataDefinitions[i]
  }
  for(i in 1:ncstr) {
    strata[[as.character(nhstr+i)]] = list(stratumName = categoryStrataNames[i],
                                           order = order[nhstr+i])
    if(!is.null(categoryStrataDefinitions)) strata[[as.character(nhstr+i)]]$definition = categoryStrataDefinitions[i]
  }
  return(new("VegXStrataDefinition",
             method = defMethod,
             strata = strata))
}
