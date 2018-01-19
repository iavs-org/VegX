#' Predefined measurement methods
#'
#' Creates a measurement method for Veg-X documents from a set of predefined options
#'
#' @param name A string with the desired measurement method. Current accepted options are:
#' \code{"Percent cover"}, \code{"Individual counts"}, \code{"Frequency of occurrence"},
#' \code{"Plant height"}, \code{"DBH"}, \code{"pH"}, \code{"Slope degrees"}, \code{"Aspect degrees"},
#' \code{"Elevation meters"}.
#'
#'
#' @return an object of class \code{\linkS4class{VegXMethod}}
#' @export
#'
#' @examples
#' # Create a method for plant percent cover
#' predefinedMeasurementMethod("Percent cover")
#'
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
               attributeClass = "plant cover",
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
               attributeClass = "plant count",
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
               attributeClass = "plant frequency",
               attributeType = "quantitative",
               attributes = attributes))
  }
  else if (name=="Plant height") {
    attributes = list(
      list(type="quantitative",
           unit = "m",
           lowerBound = 0,
           upperBound = Inf)
    )
    names(attributes) = 1
    return(new("VegXMethod",
               name = "Plant height",
               description = "Plant height in meters",
               attributeClass = "plant height",
               attributeType = "quantitative",
               attributes = attributes))
  }
  else if (name=="DBH") {
    attributes = list(
      list(type="quantitative",
           unit = "cm",
           lowerBound = 0,
           upperBound = Inf)
    )
    names(attributes) = 1
    return(new("VegXMethod",
               name = "DBH",
               description = "Diameter at breast height, in cm",
               attributeClass = "plant diameter",
               attributeType = "quantitative",
               attributes = attributes))
  }
  else if (name=="pH") {
    attributes = list(
      list(type="quantitative",
           unit = "pH",
           lowerBound = 0,
           upperBound = 14)
    )
    names(attributes) = 1
    return(new("VegXMethod",
               name = "pH",
               description = "pH scale",
               attributeClass = "pH",
               attributeType = "quantitative",
               attributes = attributes))
  }
  else if (name=="Slope degrees") {
    attributes = list(
      list(type="quantitative",
           unit = "degrees",
           lowerBound = 0,
           upperBound = 360)
    )
    names(attributes) = 1
    return(new("VegXMethod",
               name = "Slope degrees",
               description = "Slope measured using degrees",
               attributeClass = "slope",
               attributeType = "quantitative",
               attributes = attributes))
  }
  else if (name=="Aspect degrees") {
    attributes = list(
      list(type="quantitative",
           unit = "degrees",
           lowerBound = 0,
           upperBound = 360)
    )
    names(attributes) = 1
    return(new("VegXMethod",
               name = "Aspect degrees",
               description = "Aspect measured using degrees",
               attributeClass = "aspect",
               attributeType = "quantitative",
               attributes = attributes))
  }
  else if (name=="Elevation meters") {
    attributes = list(
      list(type="quantitative",
           unit = "m",
           lowerBound = 0,
           upperBound = Inf)
    )
    names(attributes) = 1
    return(new("VegXMethod",
               name = "Elevation meters",
               description = "Elevation measured using meters above sea level",
               attributeClass = "elevation",
               attributeType = "quantitative",
               attributes = attributes))
  }
  stop(paste0(name, " is not among the predefined methods."))
}
