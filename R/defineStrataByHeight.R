#' Stratum definition by height
#'
#' User-defined strata based on height
#'
#' @param heightBreaks A numeric vector with height limits between strata (of length equal to the number of strata plus one).
#' @param stratumNames A numeric vector of stratum codes (of length equal to the number of strata).
#' @param heightUnit A string to identify height units.
#'
#' @return an object of class \code{\linkS4class{VegXStrata}}
#' @export
#'
#' @examples
defineStrataByHeight<-function(name = "Strata by height",
                                    description = "Vegetation strata defined by height in m",
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
