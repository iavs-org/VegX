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

  .readVegXMeasurement.2.0.0 = function(x) {
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
      if("slope" %in% names(topo)) plot$topography$slope = .readVegXMeasurement.2.0.0(topo[["slope"]])
      if("aspect" %in% names(topo)) plot$topography$aspect = .readVegXMeasurement.2.0.0(topo[["aspect"]])
    }
    return(plot)
  }
  target@plots = xmlApply(veg[["plots"]], .readPlot.2.0.0)
  names(target@plots) = xmlApply(veg[["plots"]], xmlAttrs)

  #read methods
  .readMethod.2.0.0 = function(x) {
    met = list()
    met$name = xmlValue(x[["name"]])
    n = names(x)
    if("description" %in% n) met$description = xmlValue(x[["description"]])
    if("subject" %in% n) met$subject = xmlValue(x[["subject"]])
    if("citationString" %in% n) met$citationString = xmlValue(x[["citationString"]])
    return(met)
  }
  target@methods = xmlApply(veg[["methods"]], .readMethod.2.0.0)
  names(target@methods) = xmlApply(veg[["methods"]], xmlAttrs)

  #read attributes
  .readAttribute.2.0.0 = function(x) {
    n = names(x)
    if("quantitative" %in% n) {
      a = x[["quantitative"]]
      att = list(type = "quantitative")
      att$methodID = xmlValue(a[["methodID"]])
      if("unit" %in% names(a)) att$unit = xmlValue(a[["unit"]])
      if("lowerLimit" %in% names(a)) att$lowerLimit = xmlValue(a[["lowerLimit"]])
      if("upperLimit" %in% names(a)) att$upperLimit = xmlValue(a[["upperLimit"]])
      return(att)
    }
    else if("ordinal" %in% n) {
      a = x[["ordinal"]]
      att = list(type = "ordinal")
      att$methodID = xmlValue(a[["methodID"]])
      if("code" %in% names(a)) att$code = xmlValue(a[["code"]])
      if("definition" %in% names(a)) att$definition = xmlValue(a[["definition"]])
      if("lowerLimit" %in% names(a)) att$lowerLimit = xmlValue(a[["lowerLimit"]])
      if("upperLimit" %in% names(a)) att$upperLimit = xmlValue(a[["upperLimit"]])
      if("order" %in% names(a)) att$order = xmlValue(a[["order"]])
      return(att)
    }
    else if("qualitative" %in% n) {
      a = x[["qualitative"]]
      att = list(type = "qualitative")
      att$methodID = xmlValue(a[["methodID"]])
      if("code" %in% names(a)) att$code = xmlValue(a[["code"]])
      if("definition" %in% names(a)) att$definition = xmlValue(a[["definition"]])
      return(att)
    }
    stop("Wrong attribute type")
  }
  target@attributes = xmlApply(veg[["attributes"]], .readAttribute.2.0.0)
  names(target@attributes) = xmlApply(veg[["attributes"]], xmlAttrs)
  for(att in target@attributes) target@methods[[att$methodID]]$attributeType = att$type #Sets method type from attribute type

  rm(veg)
  return(target)
}
