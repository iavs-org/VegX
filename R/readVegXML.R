#' Reads a Veg-X XML file
#'
#' @param file the filename of an XML file following the Veg-X standard (ver. 2.0)
#'
#' @return An object of class \code{\linkS4class{VegX}}
#' @export
#' 
readVegXML<-function(file) {
  target = newVegX()
  veg=xmlRoot(xmlTreeParse(file, useInternalNodes = T))
  
  #read projects
  .readProject.2.0.0 = function(x) {
    project = list()
    project$title = xmlValue(x[["title"]])
    return(project)
  }
  target@projects = xmlApply(veg[["projects"]], .readProject.2.0.0)
  #read plots
  .readPlot.2.0.0 = function(x) {
    plot = list()
    plot$plotName = xmlValue(x[["plotName"]])
    return(plot)
  }
  target@plots = xmlApply(veg[["plots"]], .readPlot.2.0.0)
  rm(veg)
  return(target)
}