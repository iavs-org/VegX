#' Predefined measurement methods
#'
#' Creates a measurement method for Veg-X documents from a set of predefined options
#'
#' @param name A string with the desired measurement method. Current accepted options are:
#' \code{"Percent cover"}, \code{"Individual counts"}, \code{"Frequency of occurrence"},
#' \code{"Stratum height/m"},
#' \code{"Plant height/m"}, \code{"Plant height/cm"}, \code{"DBH"}, \code{"pH"}, \code{"Slope/degrees"}, \code{"Aspect/degrees"},
#' \code{"Elevation/m"}.
#'
#'
#' @return an object of class \code{\linkS4class{VegXMethod}}
#' @export
#'
#' @family define measurement functions
#'
#' @examples
#' # Create a method for plant percent cover
#' predefinedMeasurementMethod("Plant cover/%")
#'
predefinedMeasurementMethod<-function(name) {

  name = match.arg(name, c("Plant cover/%", "Individual counts", "Frequency of occurrence",
                           "Stratum height/m",
                           "Plant height/m", "Plant height/cm", "DBH/cm",
                           "pH",
                           "Slope/degrees", "Aspect/degrees", "Elevation/m"))
  if(name=="Plant cover/%") {
    return(defineQuantitativeScaleMethod(name = "Plant cover/%",
                                         description = "Plant cover as percentage of ground covered by the projection.",
                                         subject = "plant cover",
                                         unit = "%",
                                         lowerLimit = 0,
                                         upperLimit = 100))
  }
  else if (name=="Individual counts") {
    return(defineQuantitativeScaleMethod(name = "Individual counts",
                                         description = "Number of individuals in the plot",
                                         subject = "plant count",
                                         unit = "individuals",
                                         lowerLimit = 0))
  }
  else if (name=="Frequency of occurrence") {
    return(defineQuantitativeScaleMethod(name = "Frequency of occurrence",
                                         description = "Frequency of occurrence in fixed subunits of the plot",
                                         subject = "plant frequency",
                                         unit = "%",
                                         lowerLimit = 0,
                                         upperLimit = 100))
  }
  else if (name=="Stratum height/m") {
    return(defineQuantitativeScaleMethod(name = "Stratum height/m",
                                         description = "Stratum height in meters above the ground",
                                         subject = "stratum height",
                                         unit = "m",
                                         lowerLimit = 0))
  }
  else if (name=="Plant height/m") {
    return(defineQuantitativeScaleMethod(name = "Plant height/m",
                                         description = "Plant height in meters above the ground",
                                         subject = "plant height",
                                         unit = "m",
                                         lowerLimit = 0))
  }
  else if (name=="Plant height/cm") {
    return(defineQuantitativeScaleMethod(name = "Plant height/cm",
                                         description = "Plant height in cm above the ground",
                                         subject = "plant height",
                                         unit = "cm",
                                         lowerLimit = 0))
  }
  else if (name=="DBH/cm") {
    return(defineQuantitativeScaleMethod(name = "DBH/cm",
                                         description = "Diameter at breast height, in cm",
                                         subject = "plant diameter",
                                         unit = "cm",
                                         lowerLimit = 0))
  }
  else if (name=="pH") {
    return(defineQuantitativeScaleMethod(name = "pH",
                                         description = "pH scale from 0 to 14",
                                         subject = "pH",
                                         unit = "",
                                         lowerLimit = 0,
                                         upperLimit = 14))
  }
  else if (name=="Slope/degrees") {
    return(defineQuantitativeScaleMethod(name = "Slope/degrees",
                                         description = "Slope measured using degrees",
                                         subject = "slope",
                                         unit = "degrees",
                                         lowerLimit = 0,
                                         upperLimit = 90))
  }
  else if (name=="Aspect/degrees") {
    return(defineQuantitativeScaleMethod(name = "Aspect/degrees",
                                         description = "Aspect measured using degrees from North",
                                         subject = "aspect",
                                         unit = "degrees",
                                         lowerLimit = 0,
                                         upperLimit = 360))
  }
  else if (name=="Elevation meters") {
    return(defineQuantitativeScaleMethod(name = "Elevation/m",
                                         description = "Elevation measured using meters above sea level",
                                         subject = "elevation",
                                         unit = "m"))
  }
  stop(paste0(name, " is not among the predefined methods."))
}
