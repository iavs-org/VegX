#' Add surface cover observation records
#'
#' Adds surface cover observation records to a VegX object from a data table
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one surface cover observation (e.g. bare rock percent cover). Columns can be varied.
#' @param mapping A list with element names 'plotName',  'obsStartDate', 'surfaceName' and 'coverMeasurement'
#' are used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Additional optional mappings are: 'subPlotName'.
#' @param coverMethod A method definition for surface cover measurements (an object of class \code{\linkS4class{VegXMethodDefinition}}).
#' @param surfaceTypeDefinition An object of class \code{\linkS4class{VegXSurfaceTypeDefinition}} indicating the definition of surface types.
#' @param missing.values A character vector of values that should be considered as missing observations/measurements.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @details Missing 'plotName', 'obsStartDate' or 'surfaceName' values are interpreted as if the previous non-missing value has to be used to define plot observation.
#' Missing subPlotName values are interpreted in that observation refers to the parent plotName.
#' Missing measurements are simply not added to the Veg-X document.
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family add functions
#'
#' @examples
#' # Load source data
#' data(mtfyffe)
#'
#' # Examine surface cover data
#' head(mtfyffe_groundcover)
#'
#' # Define mapping
#' mapping = list(plotName = "Plot", obsStartDate = "PlotObsStartDate",
#'                surfaceName = "PlotGroundCover", coverMeasurement = "Value")
#'
#'
#' # Get cover measurement method from predefined list
#' coverMethod = predefinedMeasurementMethod("Surface cover/%")
#'
#' # Define surface types from the data
#' unique(mtfyffe_groundcover$PlotGroundCover)
#' surfaceTypes = defineSurfaceTypes(name = "Default surface types",
#'                                   description = "Five surface categories",
#'                                   surfaceNames = c("Vegetation", "Moss", "Litter", "Exposed Soil", "Rock"))
#'
#' # Create new Veg-X document with surface cover observations
#' x = addSurfaceCoverObservations(newVegX(), mtfyffe_groundcover, mapping,
#'                                 coverMethod, surfaceTypes)
#'
#' # Examine results
#' head(showElementTable(x, "surfaceCoverObservation"))
#'
#'
#' # Another example with different surface types
#' data(takitimu)
#' head(taki_groundcover)
#'
#' unique(taki_groundcover$PlotGroundCover)
#' surfaceTypes = defineSurfaceTypes(name = "Default surface types",
#'                               description = "Five surface categories",
#'                               surfaceNames = c("Vegetation", "Soil", "Erosion Pavement", "Litter","Rock"))
#'
#' x = addSurfaceCoverObservations(newVegX(), taki_groundcover, mapping,
#'                                 coverMethod, surfaceTypes)
#'
#' head(showElementTable(x, "surfaceCoverObservation"))
#'
addSurfaceCoverObservations<-function(target, x, mapping,
                                      coverMethod, surfaceTypeDefinition,
                                      missing.values = c(NA, ""),
                                      verbose = TRUE) {

  if(is.null(surfaceTypeDefinition)) stop("Surface type definition must be supplied to map cover observations.")
  if(is.null(coverMethod)) stop("Cover measurement method must be supplied to map cover observations.")

  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0


  #Check columns exist
  for(i in 1:length(mapping)) {
    if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]))
  surfaceNameData = as.character(x[[mapping[["surfaceName"]]]])

  #Optional mappings
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }


  #Check duplicate records
  surfaceObservationMappingsAvailable = c("plotName", "obsStartDate", "subPlotName", "surfaceName")
  mapcols = as.character(mapping[surfaceObservationMappingsAvailable[c(T,T,subPlotFlag,T)]])
  xstrings = apply(x[, mapcols],1, paste, collapse=" ")
  us = length(unique(xstrings))
  if(us<nrow(x)) warning(paste0(nrow(x)-us," duplicate records found!"))

  #covermeasurement
  coverValues = as.character(x[[mapping[["coverMeasurement"]]]])
  methods = list(coverMeasurement = coverMethod)

  #add methods
  methodIDs = character(0)
  methodCodes = list()
  methodAttIDs = list()
  for(m in names(methods)) {
    method = methods[[m]]
    nmtid = .newMethodIDByName(target,method@name)
    methodID = nmtid$id
    methodIDs[[m]] = methodID
    methodCodes[[m]] = character(0)
    methodAttIDs[[m]] = character(0)
    if(nmtid$new) {
      target@methods[[methodID]] = list(name = method@name,
                                        description = method@description,
                                        subject = method@subject,
                                        attributeType = method@attributeType)
      if(verbose) cat(paste0(" Measurement method '", method@name,"' added for '",m,"'.\n"))
      # add literature citation if necessary
      if(method@citationString!="") {
        ncitid = .newLiteratureCitationIDByCitationString(target, method@citationString)
        if(ncitid$new) {
          target@literatureCitations[[ncitid$id]] = list(citationString =method@citationString)
          if(method@DOI!="")  target@literatureCitations[[ncitid$id]]$DOI = method@DOI
        }
        target@methods[[methodID]]$citationID = ncitid$id
      }
      # add attributes if necessary
      methodAttIDs[[m]] = character(length(method@attributes))
      methodCodes[[m]] = character(length(method@attributes))
      for(i in 1:length(method@attributes)) {
        attid = .nextAttributeID(target)
        target@attributes[[attid]] = method@attributes[[i]]
        target@attributes[[attid]]$methodID = methodID
        methodAttIDs[[m]][i] = attid
        if(method@attributes[[i]]$type != "quantitative") methodCodes[[m]][i] = method@attributes[[i]]$code
      }
    } else {
      methodCodes[[m]] = .getAttributeCodesByMethodID(target,methodID)
      methodAttIDs[[m]] = .getAttributeIDsByMethodID(target,methodID)
      if(verbose) cat(paste0(" Measurement method '", method@name,"' for '",m,"' already included.\n"))
    }
  }


  # surface type definition
  surfDefMethod = surfaceTypeDefinition@method
  snmtid = .newMethodIDByName(target,surfDefMethod@name)
  smethodID = snmtid$id
  if(snmtid$new) {
    target@methods[[smethodID]] = list(name = surfDefMethod@name,
                                       description = surfDefMethod@description,
                                       subject = surfDefMethod@subject,
                                       attributeType = surfDefMethod@attributeType)
    if(verbose) cat(paste0(" Surface type definition method '", surfDefMethod@name,"' added.\n"))
    # add attributes if necessary
    if(length(surfDefMethod@attributes)>0) {
      for(i in 1:length(surfDefMethod@attributes)) {
        attid = .nextAttributeID(target)
        target@attributes[[attid]] = surfDefMethod@attributes[[i]]
        target@attributes[[attid]]$methodID = strmethodID
      }
    }
    # add surface types (beware of new surface types)
    orinst = length(target@surfaceTypes)
    nst = length(surfaceTypeDefinition@surfaceTypes)
    surfaceIDs = character(0)
    surfaceNames = character(0)
    for(i in 1:nst) {
      stid = .nextSurfaceTypeID(target)
      surfaceIDs[i] = stid
      surfaceNames[i] = surfaceTypeDefinition@surfaceTypes[[i]]$surfaceName
      target@surfaceTypes[[stid]] = surfaceTypeDefinition@surfaceTypes[[i]]
      target@surfaceTypes[[stid]]$methodID = smethodID
    }
    finnst = length(target@surfaceTypes)
    if(verbose) {
      cat(paste0(" ", finnst-orinst, " new surface type definitions added.\n"))
    }
  }
  else { #Read surface type IDs from selected method
    if(verbose) cat(paste0(" Surface type definition '", surfaceTypeDefinition@name,"' already included.\n"))
    surfaceIDs = .getSurfaceTypeIDsByMethodID(target,strmethodID)
    surfaceNames = .getSurfaceTypeNamesByMethodID(target,strmethodID)
  }

  orinplots = length(target@plots)
  orinplotobs = length(target@plotObservations)
  orinscobs = length(target@surfaceCoverObservations)
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
  parsedPlotObs = character(0)
  parsedPlotObsIDs = character(0)
  parsedSCObs = character(0)
  parsedSCObsIDs = character(0)
  #Record parsing loop
  for(i in 1:nrecords) {
    #plot
    if(!(plotNames[i] %in% missing.values)) {# If plotName is missing take the previous one
      plotName = plotNames[i]
    }
    if(!(plotName %in% parsedPlots)) {
      npid = .newPlotIDByName(target, plotNames[i]) # Get the new plot ID (internal code)
      plotID = npid$id
      if(npid$new) target@plots[[plotID]] = list("plotName" = plotName)
      parsedPlots = c(parsedPlots, plotName)
      parsedPlotIDs = c(parsedPlotIDs, plotID)
    } else { #this access should be faster
      plotID = parsedPlotIDs[which(parsedPlots==plotName)]
    }
    #subplot (if defined)
    if(subPlotFlag){
      if(!(subPlotNames[i] %in% missing.values)) {# If subPlotName is missing use parent plot ID
        subPlotCompleteName = paste0(plotNames[i],"_", subPlotNames[i])
        if(!(subPlotCompleteName %in% parsedPlots)) {
          parentPlotID = plotID
          npid = .newPlotIDByName(target, subPlotCompleteName) # Get the new subplot ID (internal code)
          plotID = npid$id
          if(npid$new) target@plots[[plotID]] = list("plotName" = subPlotCompleteName,
                                                     "parentPlotID" = parentPlotID)
          parsedPlots = c(parsedPlots, subPlotCompleteName)
          parsedPlotIDs = c(parsedPlotIDs, plotID)
        } else { #this access should be faster
          plotID = parsedPlotIDs[which(parsedPlots==subPlotCompleteName)]
        }
      }
    }
    #plot observation
    if(!(obsStartDates[i] %in% missing.values)) {# If observation date is missing take the previous one
      obsStartDate = obsStartDates[i]
    }
    pObsString = paste(plotID, obsStartDate) # plotID+Date
    if(!(pObsString %in% parsedPlotObs)) {
      npoid = .newPlotObsIDByDate(target, plotID, obsStartDate) # Get the new plot observation ID (internal code)
      plotObsID = npoid$id
      if(npoid$new) {
        target@plotObservations[[plotObsID]] = list("plotID" = plotID,
                                                    "obsStartDate" = obsStartDate)
      }
      parsedPlotObs = c(parsedPlotObs, pObsString)
      parsedPlotObsIDs = c(parsedPlotObsIDs, plotObsID)
    } else {
      plotObsID = parsedPlotObsIDs[which(parsedPlotObs==pObsString)]
    }

    # surface cover observations
    if(!(surfaceNameData[i] %in% missing.values)) {# If surface name is missing take the previous one
      surfaceName = surfaceNameData[i]
    }
    if(!(surfaceName %in% surfaceNames)) stop(paste0(surfaceName," not found within surface type names. Revise surface type definition or data."))
    stID = surfaceIDs[which(surfaceNames==surfaceName)]
    scObsString = paste(plotObsID, stID) # plotObsID+stID
    if(!(scObsString %in% parsedSCObs)) {
      nstoid = .newSurfaceCoverObsIDByIDs(target, plotObsID, stID) # Get the new surface type observation ID (internal code)
      scObsID = nstoid$id
      if(nstoid$new) target@surfaceCoverObservations[[scObsID]] = list("plotObservationID" = plotObsID,
                                                                   "surfaceTypeID" = stID)
      parsedSCObs = c(parsedSCObs, scObsString)
      parsedSCObsIDs = c(parsedSCObsIDs, scObsID)
    } else {
      scObsID = parsedSCObsIDs[which(parsedSCObs==scObsString)]
    }
    scObs = target@surfaceCoverObservations[[scObsID]]

    # cover measurements
    if("coverMeasurement" %in% names(mapping)) {
      method = methods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      value = as.character(coverValues[i])
      if(!(value %in% as.character(missing.values))) {
        if(method@attributeType== "quantitative") {
          value = as.numeric(value)
          if(value> method@attributes[[1]]$upperLimit) {
            stop(paste0("Cover '", value,"' larger than upper limit of measurement definition. Please revise scale or data."))
          }
          else if(value < method@attributes[[1]]$lowerLimit) {
            stop(paste0("Cover '", value,"' smaller than lower limit of measurement definition. Please revise scale or data."))
          }
          scObs[[m]] = list("attributeID" = attIDs[1], "value" = value)
        } else {
          ind = which(codes==value)
          if(length(ind)==1) {
            scObs[[m]] = list("attributeID" = attIDs[ind], "value" = value)
          }
          else stop(paste0("Value '", value,"' not found in cover measurement definition. Please revise cover classes or data."))
        }
      } else {
        nmissing = nmissing + 1
      }
    }
    #Store value in target
    target@surfaceCoverObservations[[scObsID]] = scObs
  }
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnscobs = length(target@surfaceCoverObservations)
  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new added.\n"))
    cat(paste0(" " , length(parsedPlotObs)," plot observation(s) parsed, ", finnplotobs-orinplotobs, " new added.\n"))
    cat(paste0(" ", nrecords," record(s) parsed, ", finnscobs-orinscobs, " new surface cover observation(s) added.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " measurement(s) with missing value(s) not added.\n"))
  }


  return(target)
}
