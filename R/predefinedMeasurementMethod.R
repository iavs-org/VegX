#' Predefined measurement methods
#'
#' @param name A string with the desired measurement method. Current options are:
#' \code{"Percent cover"}, \code{"Individual counts"}, \code{"Frequency of occurrence"}
#'
#'
#' @return an object of class \code{\linkS4class{VegXMethod}}
#' @export
#'
#' @examples
predefinedMeasurementMethod<-function(name) {

  if(name=="Percent cover") {
    attributes = list(
      list(type="quantitative",
           unit = "%",
           upperBound = 100,
           lowerBound = 0)
    )
    names(attributes) = 1
    return(new("VegXMethod",
               name = "Percent cover",
               description = "Quantitative plant percent cover",
               attributeClass = "cover",
               attributeType = "quantitative",
               attributes = attributes))
  }
  else if (name=="Individual counts") {
    attributes = list(
      list(type="quantitative",
           unit = "individuals",
           lowerBound = 0,
           upperBound = Inf)
    )
    names(attributes) = 1
    return(new("VegXMethod",
               name = "Individual counts",
               description = "Number of individuals in the (sub)plot",
               attributeClass = "count",
               attributeType = "quantitative",
               attributes = attributes))
  }
  else if (name=="Frequency of occurrence") {
    attributes = list(
      list(type="quantitative",
           unit = "%",
           lowerBound = 0,
           upperBound = 100)
    )
    names(attributes) = 1
    return(new("VegXMethod",
               name = "Frequency of occurrence",
               description = "Frequency of occurrence in subunits of the sampled area",
               attributeClass = "frequency",
               attributeType = "quantitative",
               attributes = attributes))
  }
  stop(paste0(name, " is not among the predefined methods."))
}
