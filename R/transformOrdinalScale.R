#' Transform ordinal scale
#'
#' Replaces all the values in a VegX object made using an ordinal scale into the corresponding scale midpoint values.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified.
#' @param method An integer (index) or a name of an ordinal scale method.
#' @param newMethod An integer (index) or a name of a quantitative method existing in the initial object,
#' or an object of class \code{\linkS4class{VegXMethod}}.
#' @param verbose A boolean flag to indicate console output of the data transformation process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @examples
#' data(mokihinui)
#'
#' # Create Veg-X document
#' target = newVegX()
#' mapping = list(plotName = "Plot", obsStartDate = "obsDate", taxonAuthorName = "PreferredSpeciesName",
#'               stratumName = "Tier", value = "Category")
#' scale = definePlantCoverScale(name = "Recce cover scale", description = "Recce recording method by Allen",
#'                          citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                          breaks = c(0, 0.1, 1, 5, 25, 50, 75, 100),
#'                          midPoints = c(0.01, 0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                          values = c("P","1","2","3", "4", "5", "6"))
#' strataDef = defineMixedStrata(name = "Recce strata",
#'                               description = "Standard Recce stratum definition",
#'                               citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                               heightStrataBreaks = c(0, 0.3,2.0,5, 12, 25, 50),
#'                               heightStrataNames = paste0("Tier ",1:6),
#'                               categoryStrataNames = "Tier 7")
#' x = addTaxonObservations(target, tcv, "Mokihinui",
#'                         mapping = mapping,
#'                         abundanceMethod = scale,
#'                         stratumDefinition = strataDef)
#'
#' summary(x)
#'
#' # Transform from "Recce cover scale" to "Percent cover"
#' percentScale = predefinedMeasurementMethod("Percent cover")
#' y = transformOrdinalScale(x, "Recce cover scale", percentScale)
#'
transformOrdinalScale<-function(target, method, newMethod, verbose = TRUE) {
  if(length(target@methods)==0) stop("VegX object has no methods")
  methodID = NULL
  if(is.numeric(method)) {
    methodID = as.character(method)
  }
  else {
    for(i in 1:length(target@methods)) {
      if(target@methods[[i]]$name==method) methodID = names(target@methods)[i]
    }
  }
  if(is.null(methodID)) stop("Target method not found.")
  if(verbose) cat(paste0(" Target method: '",target@methods[[methodID]]$name,"'\n"))
  attIDs = .getAttributeIDsByMethodID(target, methodID)
  if(verbose) cat(paste0(" Number of attributes: ", length(attIDs),"\n"))
  mapping = list()
  lowerBound = 9999999999
  upperBound = -9999999999

  for(i in 1:length(attIDs)) {
    att = target@attributes[[attIDs[i]]]
    if(att$type=="ordinal") {
      if("midPoint" %in% names(att)) {
        mapping[[attIDs[i]]] = att$midPoint
        if("lowerLimit" %in% names(att)) lowerBound = min(as.numeric(att$lowerLimit), lowerBound)
        if("upperLimit" %in% names(att)) upperBound = max(as.numeric(att$upperLimit), upperBound)
      }
    }
  }
  if(verbose) cat(paste0(" Number of attributes with midpoints: ", length(mapping),"\n"))
  if(verbose) cat(paste0(" Boundaries of the new attribute: [", lowerBound,", ",upperBound ,"]\n"))
  if(length(mapping)==0) stop("The selected method cannot be transformed.")

  # New method and attribute
  newMethodID = NULL
  if(is.numeric(newMethod)) {
    newMethodID = as.character(newMethod)
    if(!(newMethodID %in% names(target@methods))) stop("New method not found in Veg-X object.")
  }
  else if(is.character(newMethod)) {
    for(i in 1:length(target@methods)) {
      if(target@methods[[i]]$name==newMethod) methodID = names(target@methods)[i]
    }
    if(is.null(newMethodID)) stop("New method not found in Veg-X object.")
  }
  else if(class(newMethod)=="VegXMethod") {
    nmtid = .newMethodIDByName(target,newMethod@name)
    newMethodID = nmtid$id
    if(nmtid$new) { # add new method
      target@methods[[newMethodID]] = list(name = newMethod@name,
                                        description = newMethod@description,
                                        subject = newMethod@subject,
                                        attributeType = newMethod@attributeType)
      if(verbose) cat(paste0(" Measurement method '", newMethod@name,"' added.\n"))
      # add attributes if necessary
      cnt = length(target@attributes)+1
      for(i in 1:length(newMethod@attributes)) {
        nattid = as.character(length(target@attributes)+1)
        target@attributes[[nattid]] = newMethod@attributes[[i]]
        target@attributes[[nattid]]$methodID = newMethodID
        cnt = cnt + 1
      }
    } else { # method already existed
      if(verbose) cat(paste0(" Measurement method '", newMethod@name,"' already included.\n"))
    }
  } else {
    stop("Wrong class for 'newMethod'. Should be either a character, an integer or an object 'VegXMethod'")
  }
  newAttID = .getAttributeIDsByMethodID(target,newMethodID)
  if(length(newAttID)!=1) stop("New method has the wrong number of attributes.")
  if(target@attributes[[newAttID]]$type!="quantitative") stop("The attribute of the new method should be quantitative.")

  # Apply mapping on aggregated organism observations
  naggtransf = 0
  if(length(target@aggregateObservations)>0) {
    for(i in 1:length(target@aggregateObservations)) {
      if(target@aggregateObservations[[i]]$attributeID %in% names(mapping)) {
        target@aggregateObservations[[i]]$value = mapping[[target@aggregateObservations[[i]]$attributeID]]
        target@aggregateObservations[[i]]$attributeID = newAttID
        naggtransf = naggtransf + 1
      }
    }
  }
  if(verbose && naggtransf > 0) cat(paste0(" ", naggtransf, " transformation(s) were applied on aggregate organism observations.\n"))

  # Apply mapping on individual organism observations
  nindtransf = 0
  if(length(target@individualObservations)>0) {
    for(i in 1:length(target@individualObservations)) {
      if(target@individualObservations[[i]]$attributeID %in% names(mapping)) {
        target@individualObservations[[i]]$diameterValue = mapping[[target@individualObservations[[i]]$attributeID]]
        target@individualObservations[[i]]$diameterAttributeID = newAttID
        nindtransf = nindtransf + 1
      }
    }
  }
  if(verbose && nindtransf > 0) cat(paste0(" ", nindtransf, " transformation(s) were applied on individual organism observations.\n"))

  # Return the modified document
  return(target)
}
