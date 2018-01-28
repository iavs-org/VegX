#' Reads a Veg-X XML file
#'
#' @param file the filename of an XML file following the Veg-X standard (ver. 2.0)
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return An object of class \code{\linkS4class{VegX}}
#' @export
#'
readVegXML<-function(file, verbose = TRUE) {
  target = newVegX()
  veg=xmlRoot(xmlTreeParse(file, useInternalNodes = T))
  vegnames = names(veg)

  .readVegXMeasurement.2.0.0 = function(x) {
    return(list(value = xmlValue(x[["value"]]), attributeID = xmlValue(x[["attributeID"]])))
  }

  #read projects
  .readProject.2.0.0 = function(x) {
    project = list()
    project$title = xmlValue(x[["title"]])
    return(project)
  }
  if("projects" %in% vegnames) {
    target@projects = xmlApply(veg[["projects"]], .readProject.2.0.0)
    names(target@projects) = xmlApply(veg[["projects"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@projects), " project(s) read.\n"))
  }

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
  if("plots" %in% vegnames) {
    target@plots = xmlApply(veg[["plots"]], .readPlot.2.0.0)
    names(target@plots) = xmlApply(veg[["plots"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@plots), " plot(s) read.\n"))
  }

  #read plot observations
  .readPlotObservation.2.0.0 = function(x) {
    plotObs = list(plotID = xmlValue(x[["plotID"]]),
                   obsStartDate = as.Date(xmlValue(x[["obsStartDate"]]), format = "%Y-%m-%d"))
    n = names(x)
    if("projectID" %in% n) plotObs$projectID = xmlValue(x[["projectID"]])
    if("obsEndDate" %in% n) plotObs$obsEndDate = as.Date(xmlValue(x[["obsEndDate"]]), format = "%Y-%m-%d")
    if("siteObservationID" %in% n) plotObs$siteObservationID = xmlValue(x[["siteObservationID"]])
    return(plotObs)
  }
  if("plotObservations" %in% vegnames) {
    target@plotObservations = xmlApply(veg[["plotObservations"]], .readPlotObservation.2.0.0)
    names(target@plotObservations) = xmlApply(veg[["plotObservations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@plotObservations), " plot observation(s) read.\n"))
  }

  #read taxon name usage concepts
  .readTNUC.2.0.0 = function(x) {
    tnuc = list(authorTaxonName = xmlValue(x[["authorTaxonName"]]))
    return(tnuc)
  }
  if("taxonNameUsageConcepts" %in% vegnames) {
    target@taxonNameUsageConcepts = xmlApply(veg[["taxonNameUsageConcepts"]], .readTNUC.2.0.0)
    names(target@taxonNameUsageConcepts) = xmlApply(veg[["taxonNameUsageConcepts"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@taxonNameUsageConcepts), " taxon name usage concept(s) read.\n"))
  }

  #read strata
  .readStratum.2.0.0 = function(x) {
    str = list(stratumName = xmlValue(x[["stratumName"]]))
    n = names(x)
    if("methodID" %in% n) str$methodID = xmlValue(x[["methodID"]])
    if("definition" %in% n) str$definition = xmlValue(x[["definition"]])
    if("lowerLimit" %in% n) str$lowerLimit = xmlValue(x[["lowerLimit"]])
    if("upperLimit" %in% n) str$upperLimit = xmlValue(x[["upperLimit"]])
    if("order" %in% n) str$order = xmlValue(x[["order"]])
    return(str)
  }
  if("strata" %in% vegnames) {
    target@strata = xmlApply(veg[["strata"]], .readStratum.2.0.0)
    names(target@strata) = xmlApply(veg[["strata"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@strata), " strata read.\n"))
  }

  #read stratum observations
  .readStratumObservation.2.0.0 = function(x) {
    strObs = list(stratumID = xmlValue(x[["stratumID"]]),
                  plotObservationID = xmlValue(x[["plotObservationID"]]))
    n = names(x)
    for(nm in n) {
      if(nm=="lowerLimitMeasurement") strObs$lowerLimitMeasurement = .readVegXMeasurement.2.0.0(x[[nm]])
      else if(nm=="upperLimitMeasurement") strObs$upperLimitMeasurement = .readVegXMeasurement.2.0.0(x[[nm]])
      else if(nm=="stratumMeasurement") {
        if(!("stratumMeasurements" %in% names(strObs))) strObs$stratumMeasurements = list()
        mesid = as.character(length(strObs$stratumMeasurements)+1)
        strObs$stratumMeasurements[[mesid]] = .readVegXMeasurement.2.0.0(x[[nm]])
      }
    }
    return(strObs)
  }
  if("stratumObservations" %in% vegnames) {
    target@stratumObservations = xmlApply(veg[["stratumObservations"]], .readStratumObservation.2.0.0)
    names(target@stratumObservations) = xmlApply(veg[["stratumObservations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@stratumObservations), " stratum observation(s) read.\n"))
  }

  #read aggregate organism observations
  .readAggregateOrganismObservation.2.0.0 = function(x) {
    aggObs = list(plotObservationID = xmlValue(x[["plotObservationID"]]),
                  taxonNameUsageConceptID = xmlValue(x[["taxonNameUsageConceptID"]]))
    n = names(x)
    if("stratumObservationID" %in% n) aggObs$stratumObservationID = xmlValue(x[["stratumObservationID"]])
    for(nm in n) {
      if(nm=="heightMeasurement") aggObs$heightMeasurement = .readVegXMeasurement.2.0.0(x[[nm]])
      else if(nm=="aggregateOrganismMeasurement") {
        if(!("aggregateOrganismMeasurements" %in% names(aggObs))) aggObs$aggregateOrganismMeasurements = list()
        mesid = as.character(length(aggObs$aggregateOrganismMeasurements)+1)
        aggObs$aggregateOrganismMeasurements[[mesid]] = .readVegXMeasurement.2.0.0(x[[nm]])
      }
    }

    return(aggObs)
  }
  if("aggregateOrganismObservations" %in% vegnames) {
    target@aggregateObservations = xmlApply(veg[["aggregateOrganismObservations"]], .readAggregateOrganismObservation.2.0.0)
    names(target@aggregateObservations) = xmlApply(veg[["aggregateOrganismObservations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@aggregateObservations), " aggregate organism observation(s) read.\n"))
  }


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
  if("methods" %in% vegnames) {
    target@methods = xmlApply(veg[["methods"]], .readMethod.2.0.0)
    names(target@methods) = xmlApply(veg[["methods"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@methods), " method(s) read.\n"))
  }

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
  if("attributes" %in% vegnames) {
    target@attributes = xmlApply(veg[["attributes"]], .readAttribute.2.0.0)
    names(target@attributes) = xmlApply(veg[["attributes"]], xmlAttrs)
    for(att in target@attributes) target@methods[[att$methodID]]$attributeType = att$type #Sets method type from attribute type
    if(verbose) cat(paste0(" ", length(target@attributes), " attribute(s) read.\n"))
  }

  rm(veg)
  return(target)
}
