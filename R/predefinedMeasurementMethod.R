#' Predefined measurement methods
#'
#' Creates a measurement method for Veg-X documents from a set of predefined options
#'
#' @param name A string with the desired measurement method. Current accepted options are:
#' \itemize{
#'  \item{\code{"Plot area/m2"}: Plot area measured in squared meters.}
#'  \item{\code{"Plot area/cm2"}: Plot area measured in squared centimeters.}
#'  \item{\code{"Plot dimension/m"}: Plot dimension (radius, length, width) measured in meters.}
#'  \item{\code{"Plot dimension/cm"}: Plot dimension (radius, length, width) measured in centimeters.}
#'  \item{\code{"Plant cover/\%"}: Plant cover as percentage of ground covered by the projection.}
#'  \item{\code{"Plant counts"}: Number of plant individuals.}
#'  \item{\code{"Plant frequency/\%"}: Frequency of occurrence in fixed subunits of the plot.}
#'  \item{\code{"Basal area/m2*ha-1"}: Basal area in square meters per hectare.}
#'  \item{\code{"Stratum height/m"}: Stratum height in meters above the ground.}
#'  \item{\code{"Stratum height/cm"}: Stratum height in cm above the ground.}
#'  \item{\code{"Plant height/m"}: Plant height in meters above the ground.}
#'  \item{\code{"Plant height/cm"}: Plant height in cm above the ground.}
#'  \item{\code{"DBH/cm"}: Diameter at breast height, in cm.}
#'  \item{\code{"Surface cover/\%"}: Surface covered by a surface type as percentage of ground covered by the projection.}
#'  \item{\code{"pH/0-14"}: pH scale from 0 to 14.}
#'  \item{\code{"Slope/degrees"}: Slope measured using degrees.}
#'  \item{\code{"Aspect/degrees"}: Aspect measured using degrees from North.}
#'  \item{\code{"Elevation/m"}: Elevation measured using meters above sea level.}
#' }
#'
#'
#' @return an object of class \code{\linkS4class{VegXMethodDefinition}}
#' @export
#'
#' @family define measurement functions
#'
#' @examples
#' # Create a method for plant percent cover
#' predefinedMeasurementMethod("Plant cover/%")
#'
predefinedMeasurementMethod<-function(name) {

  name = match.arg(name, c("Plot area/m2", "Plot area/cm2","Plot dimension/m","Plot dimension/cm",
                           "Plant cover/%", "Plant counts", "Plant frequency/%",
                           "Basal area/m2*ha-1", 
                           "Stratum height/m", "Stratum height/cm",
                           "Plant height/m", "Plant height/cm", "DBH/cm",
                           "Surface cover/%",
                           "pH/0-14",
                           "Slope/degrees", "Aspect/degrees", "Elevation/m"))
  if(name=="Plot area/m2") {
    return(defineQuantitativeScaleMethod(name = "Plot area/m2",
                                         description = "Plot area measured in squared meters.",
                                         subject = "plot area",
                                         unit = "m2",
                                         lowerLimit = 0,
                                         upperLimit = Inf))
  }
  else if(name=="Plot area/cm2") {
    return(defineQuantitativeScaleMethod(name = "Plot area/cm2",
                                         description = "Plot area measured in squared centimeters.",
                                         subject = "plot area",
                                         unit = "cm2",
                                         lowerLimit = 0,
                                         upperLimit = Inf))
  }
  else if(name=="Plot dimension/m") {
    return(defineQuantitativeScaleMethod(name = "Plot dimension/m",
                                         description = "Plot dimension (radius, length, width) measured in meters.",
                                         subject = "plot dimension",
                                         unit = "m",
                                         lowerLimit = 0,
                                         upperLimit = Inf))
  }
  else if(name=="Plot dimension/cm") {
    return(defineQuantitativeScaleMethod(name = "Plot dimension/cm",
                                         description = "Plot dimension (radius, length, width) measured in centimeters.",
                                         subject = "plot dimension",
                                         unit = "cm",
                                         lowerLimit = 0,
                                         upperLimit = Inf))
  }
  else if(name=="Plant cover/%") {
    return(defineQuantitativeScaleMethod(name = "Plant cover/%",
                                         description = "Plant cover as percentage of ground covered by the projection.",
                                         subject = "plant cover",
                                         unit = "%",
                                         lowerLimit = 0,
                                         upperLimit = 100))
  }
  else if (name=="Plant counts") {
    return(defineQuantitativeScaleMethod(name = "Individual counts",
                                         description = "Number of plant individuals",
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
  else if (name=="Basal area/m2*ha-1") {
    return(defineQuantitativeScaleMethod(name = "Basal area/m2*ha-1",
                                         description = "Basal area in square meters per hectare",
                                         subject = "basal area",
                                         unit = "m2*ha-1",
                                         lowerLimit = 0,
                                         upperLimit = Inf))
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
  else if (name=="Surface cover/%") {
    return(defineQuantitativeScaleMethod(name = "Surface cover/%",
                                         description = "Surface covered by a surface type as percentage of ground covered by the projection.",
                                         subject = "surface cover",
                                         unit = "%",
                                         lowerLimit = 0,
                                         upperLimit = 100))
  }
  else if (name=="pH/0-14") {
    return(defineQuantitativeScaleMethod(name = "pH/0-14",
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
  else if (name=="Elevation/m") {
    return(defineQuantitativeScaleMethod(name = "Elevation/m",
                                         description = "Elevation measured using meters above sea level",
                                         subject = "elevation",
                                         unit = "m"))
  }
  stop(paste0(name, " is not among the predefined methods."))
}
