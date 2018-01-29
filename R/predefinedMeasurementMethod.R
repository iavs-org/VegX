#' Predefined measurement methods
#'
#' Creates a measurement method for Veg-X documents from a set of predefined options
#'
#' @param name A string with the desired measurement method. Current accepted options are:
#' \itemize{
#'  \item{\code{"Plant cover/\%"}: Plant cover as percentage of ground covered by the projection.}
#'  \item{\code{"Plant counts"}: Number of individuals in the plot.}
#'  \item{\code{"Plant frequency/\%"}: Frequency of occurrence in fixed subunits of the plot.}
#'  \item{\code{"Stratum height/m"}: Stratum height in meters above the ground.}
#'  \item{\code{"Stratum height/cm"}: Stratum height in cm above the ground.}
#'  \item{\code{"Plant height/m"}: Plant height in meters above the ground.}
#'  \item{\code{"Plant height/cm"}: Plant height in cm above the ground.}
#'  \item{\code{"DBH/cm"}: Diameter at breast height, in cm.}
#'  \item{\code{"pH"}: pH scale from 0 to 14.}
#'  \item{\code{"Slope/degrees"}: Slope measured using degrees.}
#'  \item{\code{"Aspect/degrees"}: Aspect measured using degrees from North.}
#'  \item{\code{"Elevation/m"}: Elevation measured using meters above sea level.}
#' }
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

  name = match.arg(name, c("Plant cover/%", "Plant counts", "Plant frequency/%",
                           "Stratum height/m", "Stratum height/cm",
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
  else if (name=="Plant counts") {
    return(defineQuantitativeScaleMethod(name = "Individual counts",
                                         description = "Number of individuals in the plot",
                                         subject = "plant count",
                                         unit = "individuals",
                                         lowerLimit = 0))
  }
  else if (name=="Plant frequency/%") {
    return(defineQuantitativeScaleMethod(name = "Plant frequency/%",
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
  else if (name=="Stratum height/cm") {
    return(defineQuantitativeScaleMethod(name = "Stratum height/cm",
                                         description = "Stratum height in cm above the ground",
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
