#' Add community observation records
#'
#' Adds community observation records to a VegX object from a data table where rows are plot observations.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param x A data frame where each row corresponds to one plot observation. Columns can be varied.
#' @param mapping A list with element names 'plotName', 'obsStartDate', used to specify the mapping of data columns (specified using strings for column names) onto these variables.
#' Additional optional mappings are: 'subPlotName'.
#' @param methods A named list of objects of class \code{\linkS4class{VegXMethodDefinition}} with the measurement method
#' for each of the community measurements listed in \code{mapping}. List names should be the same as subject measurement variables.
#' Alternatively, methods can be specified using strings if predefined methods exist (see \code{\link{predefinedMeasurementMethod}}).
#' @param date.format A character string specifying the input format of dates (see \code{\link{as.Date}}).
#' @param missing.values A character vector of values that should be considered as missing observations/measurements.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @details Missing value policy:
#' \itemize{
#'   \item{Missing 'plotName' or 'obsStartDate' values are interpreted as if the previous non-missing value has to be used to define plot observation.}
#'   \item{Missing 'subPlotName' values are interpreted in that observation refers to the parent plotName.}
#'   \item{Missing measurements are simply not added to the Veg-X document.}
#' }
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family add functions
#'
#' @examples 
#' data(Mokihinui)
#' 
#' # Simulate measurement of basal area
#' moki_site$BA = pmax(0, rnorm(nrow(moki_site), 10, 5))
#' 
#' # Define mapping
#' mapping = list(plotName = "Plot", subPlotName = "Subplot",
#'                obsStartDate = "PlotObsStartDate", basal_area = "BA")
#'
#' 
#' x = addCommunityObservations(newVegX(), moki_site, mapping = mapping,
#'                         methods = list(basal_area = "Basal area/m2*ha-1"))
#'                         
#' # Inspect the result
#' head(showElementTable(x, "communityObservation"))
addCommunityObservations<-function(target, x,
                              mapping,
                              methods = list(),
                              date.format = "%Y-%m-%d",
                              missing.values = c(NA,""),
                              verbose = TRUE) {

  if(class(target)!="VegX") stop("Wrong class for 'target'. Should be an object of class 'VegX'")
  x = as.data.frame(x)
  nrecords = nrow(x)
  nmissing = 0


  #check mappings
  mappingsAvailable = c("plotName", "obsStartDate", "subPlotName")
  communityValues = list()
  for(i in 1:length(mapping)) {
    if(!(names(mapping)[i] %in% mappingsAvailable)) {
      if(!(names(mapping)[i] %in% names(methods))) stop(paste0("Measurement method should be provided corresponding to mapping '", names(mapping)[i], "'."))
      if(!(mapping[i] %in% names(x))) stop(paste0("Variable '", mapping[i],"' not found in column names. Revise mapping or data."))
      communityValues[[names(mapping)[i]]] = as.character(x[[mapping[[i]]]])
    }
  }
  plotNames = as.character(x[[mapping[["plotName"]]]])
  obsStartDates = as.Date(as.character(x[[mapping[["obsStartDate"]]]]), format = date.format)
  
  #Optional mappings
  subPlotFlag = ("subPlotName" %in% names(mapping))
  if(subPlotFlag) {
    subPlotNames = as.character(x[[mapping[["subPlotName"]]]])
  }

  #check methods for community variables
  if(length(methods)>0) {
    for(i in 1:length(methods)) {
      if(!(names(methods)[i] %in% names(mapping))) stop(paste0("Mapping should be defined corresponding to measurement method '", names(methods)[i], "'."))
    }
  }


  #add methods
  methodIDs = character(0)
  methodCodes = list()
  methodAttIDs = list()
  for(m in names(methods)) {
    method = methods[[m]]
    if(class(method)=="character") {
      method = predefinedMeasurementMethod(method)
      methods[[m]] = method
    }
    else if (class(method) != "VegXMethodDefinition") stop(paste("Wrong class for method: ",m ,"."))
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


  orinplots = length(target@plots)
  orinplotobs = length(target@plotObservations)
  orinabioobs = length(target@communityObservations)
  parsedPlots = character(0)
  parsedPlotIDs = character(0)
  parsedPlotObs = character(0)
  parsedPlotObsIDs = character(0)
  #Record parsing loop
  for(i in 1:nrecords) {
    #plot
    if(!(plotNames[i] %in% missing.values)) plotName = plotNames[i] # Missing plot means we keep with plot of the previous record
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
      if(!(subPlotNames[i] %in% missing.values)) { # Missing subplot means that we add information to the plot
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
    if(!(obsStartDates[i] %in% missing.values)) obsStartDate = obsStartDates[i] # Missing date means we keep with date of the previous record
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
      plotObsID = parsedPlotIDs[which(parsedPlotObs==pObsString)]
    }

    #community observations
    coid = .newCommunityObservationIDByID(target, plotObsID)
    commObsID = coid$id
    if(coid$new) {
      commObs = list("plotObservationID" = plotObsID)
      target@plotObservations[[plotObsID]]$communityObservationID = commObsID # Set a one-to-one link
    } else {
      commObs = target@communityObservations[[commObsID]]
    }
    for(m in names(methods)) {
      value = communityValues[[m]][i]
      method = methods[[m]]
      attIDs = methodAttIDs[[m]]
      codes = methodCodes[[m]]
      if(!(value %in% as.character(missing.values))) {
        if(!("communityMeasurements" %in% names(commObs))) commObs$communityMeasurements = list()
        mesID = as.character(length(commObs$communityMeasurements)+1)
        commObs$communityMeasurements[[mesID]] = list()
        if(method@attributeType== "quantitative") {
          value = as.numeric(value)
          if(value > method@attributes[[1]]$upperLimit) {
            stop(paste0("Value '", value,"' for '", m, "' larger than upper limit of measurement definition. Please revise scale or data."))
          }
          else if(value < method@attributes[[1]]$lowerLimit) {
            stop(paste0("Value '", value,"' for '", m, "' smaller than lower limit of measurement definition. Please revise scale or data."))
          }
          commObs$communityMeasurements[[mesID]] = list("attributeID" = attIDs[[1]],
                                                        "value" = value)
        } 
        else {
          ind = which(codes==as.character(value))
          if(length(ind)==1) {
            commObs$communityMeasurements[[mesID]] = list("attributeID" = attIDs[[ind]],
                                                     "value" = value)
          }
          else stop(paste0("Value '", value,"' for '", m, "' not found in measurement definition. Please revise classes or data."))
        }
      } 
      else {
        nmissing = nmissing + 1
      }

    }
    target@communityObservations[[commObsID]] = commObs
  }
  finnplots = length(target@plots)
  finnplotobs = length(target@plotObservations)
  finnabioobs = length(target@communityObservations)
  if(verbose) {
    cat(paste0(" " , length(parsedPlots)," plot(s) parsed, ", finnplots-orinplots, " new plot(s) added.\n"))
    cat(paste0(" " , length(parsedPlotObs)," plot observation(s) parsed, ", finnplotobs-orinplotobs, " new plot observation(s) added.\n"))
    cat(paste0(" ", nrecords," record(s) parsed, ", finnabioobs-orinabioobs, " new community observation(s) added.\n"))
    if(nmissing>0) cat(paste0(" ", nmissing, " community measurement(s) with missing value(s) not added.\n"))
  }

  return(target)
}
