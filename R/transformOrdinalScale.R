#' Transform ordinal scale
#'
#' Replaces all the values made using an ordinal scale to their corresponding midpoint quantitative values
#' (e.g. cover scale values to percent cover).
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified
#' @param method An integer (index) or a name of an ordinal scale method
#' @param verbose A boolean flag to indicate console output of the data transformation process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @examples
transformOrdinalScale<-function(target, method, newUnits, verbose = TRUE) {
  if(length(target@methods)==0) stop("VegX object has no methods")
  methodID = NULL
  if(is.numeric(method)) methodID = as.character(method)
  else {
    for(i in 1:length(target@methods)) {
      if(target@methods[[i]]$name==method) methodID = names(target@methods)[i]
    }
  }
  if(is.null(methodID)) stop("Method not found.")
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

  # Creates the transform method and attribute
  newMethodID = as.character(length(target@methods)+1)
  target@methods[[newMethodID]] = list(name = paste0(target@methods[[methodID]]$name, " [transformed]"),
                                       description = paste0(target@methods[[methodID]]$description, " [transformed]"),
                                       attributeClass = target@methods[[methodID]]$attributeClass,
                                       attributeType = "quantitative")
  if(verbose) cat(paste0(" New method '", target@methods[[newMethodID]]$name,"' added.\n"))

  newAttID = as.character(length(target@attributes)+1)
  target@attributes[[newAttID]] = list(type = "quantitative",
                                       unit = newUnits,
                                       lowerBound = lowerBound,
                                       upperBound = upperBound,
                                       methodID = newMethodID)
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
  if(verbose && naggtransf > 0) cat(paste0(" ", naggtransf, " aggregate organism transformation(s) were modified.\n"))

  # Return the modified document
  return(target)
}
