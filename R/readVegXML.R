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

  .parseVegXMeasurement.2.0.0 = function(x) {
    return(list(value = xmlValue(x[["value"]]), attributeID = xmlValue(x[["attributeID"]])))
  }

  #read projects
  .readProject.2.0.0 = function(x) {
    project = list()
    project$title = xmlValue(x[["title"]])
    return(project)
  }
  target@projects = xmlApply(veg[["projects"]], .readProject.2.0.0)
  names(target@projects) = xmlApply(veg[["projects"]], xmlAttrs)
  #read plots
  .readPlot.2.0.0 = function(x) {
    plot = list()
    plot$plotName = xmlValue(x[["plotName"]])
    n = names(x)
    if("relatedPlot" %in% names(x)) {
      rp = x[["relatedPlot"]]
      if(xmlValue(rp[["plotRelationship"]])=="subplot") {
        plot$parentPlotID = xmlValue(rp[["relatedPlotID"]])
      } else {
        warning("Plot relationship could not be parsed!")
      }
    }
    if("location" %in% names(x)) {
      loc = x[["location"]]
      plot$location = list()
      if("geospatial" %in% names(loc)) {
        geo = loc[["geospatial"]]
        if("DecimalLongitude" %in% names(geo)) plot$location$DecimalLongitude = xmlValue(geo[["DecimalLongitude"]])
        if("DecimalLatitude" %in% names(geo)) plot$location$DecimalLatitude = xmlValue(geo[["DecimalLatitude"]])
        if("GeodeticDatum" %in% names(geo)) plot$location$GeodeticDatum = xmlValue(geo[["GeodeticDatum"]])
      }
    }
    if("topography" %in% names(x)) {
      topo = x[["topography"]]
      plot$topography = list()
      if("slope" %in% names(topo)) plot$topography$slope = .parseVegXMeasurement.2.0.0(topo[["slope"]])
      if("aspect" %in% names(topo)) plot$topography$aspect = .parseVegXMeasurement.2.0.0(topo[["aspect"]])
    }
    return(plot)
  }
  target@plots = xmlApply(veg[["plots"]], .readPlot.2.0.0)
  names(target@plots) = xmlApply(veg[["plots"]], xmlAttrs)
  rm(veg)
  return(target)
}
