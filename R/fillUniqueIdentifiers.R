#' Title
#'
#' @param target 
#' @param element
#' @param replace.existing 
#'
#' @return
#' @export
#'
#' @examples
fillUniqueIdentifiers<-function(target, element = "plot", replace.existing = FALSE) {
  if(class(target)!="VegX") stop("Wrong class for 'target'. Should be an object of class 'VegX'")
  if(element=="plot") {
    if(length(target@plots)>0) {
      for(i in 1:length(target@plots)) {
        uuid = UUIDgenerate()
        if(replace.existing) target@plots[[i]]$plotUniqueIdentifier = uuid
        else {
          if(!("plotUniqueIdentifier" %in% names(target@plots[[i]]))) {
            target@plots[[i]]$plotUniqueIdentifier = uuid
          } else {
            if(target@plots[[i]]$plotUniqueIdentifier=="") target@plots[[i]]$plotUniqueIdentifier = uuid
          }
        }
      }
    }
  }
  if(element=="plotObservation") {
    if(length(target@plotObservations)>0) {
      for(i in 1:length(target@plotObservations)) {
        uuid = UUIDgenerate()
        if(replace.existing) target@plotObservations[[i]]$plotObservationUniqueIdentifier = uuid
        else {
          if(!("plotObservationUniqueIdentifier" %in% names(target@plotObservations[[i]]))) {
            target@plotObservations[[i]]$plotObservationUniqueIdentifier = uuid
          } else {
            if(target@plotObservations[[i]]$plotObservationUniqueIdentifier=="") target@plotObservations[[i]]$plotObservationUniqueIdentifier = uuid
          }
        }
      }
    }
  }
  return(target)
}
