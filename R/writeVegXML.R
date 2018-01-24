#' Write VegX XML file
#'
#' Assembles and writes an XML file on the disk
#' following the Veg-X XML schema standard (ver 1.6.0)
#'
#' @param x an object of class \code{\linkS4class{VegX}}
#' @param file the file name to be written
#'
#' @return A string of the file written
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @examples
#' \dontrun{
#'   target = newVegX()
#'   writeVegXML(target, "foo.xml")
#' }
#'
writeVegXML<-function(x, file) {
  # Top XML node
  top = newXMLNode("VegX")

  #Project elements
  if(length(x@projects)>0){
    prjs = newXMLNode("projects", parent = top)
    for(i in 1:length(x@projects)){
      prj = newXMLNode("project",
                       attrs = c("id"=names(x@projects)[i]),
                       parent = prjs)
      newXMLNode("title", x@projects[[i]]$title, parent=prj)
    }
  }
  #Plot elements
  if(length(x@plots)>0){
    plots = newXMLNode("plots", parent = top)
    for(i in 1:length(x@plots)){
      p = newXMLNode("plot",
                     attrs = c("id"=names(x@plots)[i]),
                     parent = plots)
      newXMLNode("plotName", x@plots[[i]]$plotName, parent=p)
      if("parentPlotID" %in% names(x@plots[[i]])) { #Add parent plot information (it is a subplot)
        rp = newXMLNode("relatedPlot", parent=p)
        newXMLNode("relatedPlotID", x@plots[[i]]$parentPlotID, parent=rp)
        newXMLNode("plotRelationship", "subplot", parent=rp)
      }
      #Add location information
      if("location" %in% names(x@plots[[i]])) {
        pl = newXMLNode("location", parent=p)
        gs = newXMLNode("geospatial", parent=pl)
        if("DecimalLongitude" %in% names(x@plots[[i]]$location)) newXMLNode("DecimalLongitude", x@plots[[i]]$location$DecimalLongitude, parent=gs)
        if("DecimalLatitude" %in% names(x@plots[[i]]$location)) newXMLNode("DecimalLatitude", x@plots[[i]]$location$DecimalLatitude, parent=gs)
        if("GeodeticDatum" %in% names(x@plots[[i]]$location)) newXMLNode("GeodeticDatum", x@plots[[i]]$location$GeodeticDatum, parent=gs)
      }
      if("topography" %in% names(x@plots[[i]])) {
        topo = newXMLNode("topography", parent=p)
        if("slope" %in% names(x@plots[[i]]$topography)) { #Add slope information
          sl = newXMLNode("slope", parent=topo)
          newXMLNode("value", x@plots[[i]]$topography$slope$value, parent=sl)
          newXMLNode("attributeID", x@plots[[i]]$topography$slope$attributeID, parent=sl)
        }
        if("aspect" %in% names(x@plots[[i]]$topography)) { #Add aspect information
          as = newXMLNode("aspect", parent=topo)
          newXMLNode("value", x@plots[[i]]$topography$aspect$value, parent=as)
          newXMLNode("attributeID", x@plots[[i]]$topography$aspect$attributeID, parent=as)
        }
      }
    }
  }
  #PlotObservation elements
  if(length(x@plotObservations)>0){
    plotObservations = newXMLNode("plotObservations", parent = top)
    for(i in 1:length(x@plotObservations)){
      po = newXMLNode("plotObservation",
                      attrs = c("id" = names(x@plotObservations)[i]),
                      parent = plotObservations)
      newXMLNode("plotID", x@plotObservations[[i]]$plotID, parent=po)
      newXMLNode("obsStartDate", as.character(x@plotObservations[[i]]$obsStartDate), parent=po)
      if("obsEndDate" %in% names(x@plotObservations[[i]])) newXMLNode("obsEndDate", as.character(x@plotObservations[[i]]$obsEndDate), parent=po)
      if("projectID" %in% names(x@plotObservations[[i]])) newXMLNode("projectID", x@plotObservations[[i]]$projectID, parent=po)
      if("siteObservationID" %in% names(x@plotObservations[[i]])) newXMLNode("siteObservationID", x@plotObservations[[i]]$siteObservationID, parent=po)
    }
  }
  #TaxonNameUsageConcept elements
  if(length(x@taxonNameUsageConcepts)>0) {
    taxonNameUsageConcepts = newXMLNode("taxonNameUsageConcepts", parent = top)
    for(i in 1:length(x@taxonNameUsageConcepts)){
      tnuc = newXMLNode("taxonNameUsageConcept",
                        attrs = c(id=names(x@taxonNameUsageConcepts)[i]),
                        parent = taxonNameUsageConcepts)
      newXMLNode("authorName", x@taxonNameUsageConcepts[[i]]$authorName,
                 # attrs = c(type = "usage name"),
                 parent=tnuc)
    }
  }
  #AggregateOrganismObservation elements
  if(length(x@aggregateObservations)>0) {
    aggregatedOrganismObservations = newXMLNode("aggregateOrganismObservations", parent = top)
    for(i in 1:length(x@aggregateObservations)){
      aoo = newXMLNode("aggregateOrganismObservation",
                       attrs = c(id = names(x@aggregateObservations)[i]),
                       parent = aggregatedOrganismObservations)
      newXMLNode("plotObservationID", x@aggregateObservations[[i]]$plotObservationID, parent=aoo)
      newXMLNode("taxonNameUsageConceptID", x@aggregateObservations[[i]]$taxonNameUsageConceptID, parent=aoo)
      if("stratumObservationID" %in% names(x@aggregateObservations[[i]]))
        if(x@aggregateObservations[[i]]$stratumObservationID != "") newXMLNode("stratumObservationID", x@aggregateObservations[[i]]$stratumObservationID, parent=aoo)
      aggval = newXMLNode("aggregateValue", parent=aoo)
      newXMLNode("value", x@aggregateObservations[[i]]$value, parent=aggval)
      newXMLNode("attributeID", x@aggregateObservations[[i]]$attributeID, parent=aggval)
    }
  }
  #stratum elements
  if(length(x@strata)>0) {
    strata = newXMLNode("strata", parent = top)
    for(i in 1:length(x@strata)){
      str = newXMLNode("stratum",
                       attrs = c(id = names(x@strata)[i]),
                       parent = strata)
      newXMLNode("stratumName", x@strata[[i]]$stratumName, parent=str)
      if("methodID" %in% names(x@strata[[i]])) newXMLNode("methodID", x@strata[[i]]$methodID, parent=str)
      if("stratumSequence" %in% names(x@strata[[i]])) newXMLNode("stratumSequence", x@strata[[i]]$stratumSequence, parent=str)
      if("lowerBound" %in% names(x@strata[[i]])) newXMLNode("lowerBound", x@strata[[i]]$lowerBound, parent=str)
      if("upperBound" %in% names(x@strata[[i]])) newXMLNode("upperBound", x@strata[[i]]$upperBound, parent=str)
    }
  }
  #StratumObservation elements
  if(length(x@stratumObservations)>0) {
    stratumObservations = newXMLNode("stratumObservations", parent = top)
    for(i in 1:length(x@stratumObservations)){
      stro = newXMLNode("stratumObservation",
                        attrs = c(id = names(x@stratumObservations)[i]),
                        parent = stratumObservations)
      newXMLNode("plotObservationID", x@stratumObservations[[i]]$plotObservationID, parent=stro)
      newXMLNode("stratumID", x@stratumObservations[[i]]$stratumID, parent=stro)
    }
  }
  #Individual Organism elements
  if(length(x@individualOrganisms)>0) {
    individualOrganisms = newXMLNode("individualOrganisms", parent = top)
    for(i in 1:length(x@individualOrganisms)){
      io = newXMLNode("individualOrganism",
                      attrs = c(id = names(x@individualOrganisms)[i]),
                      parent = individualOrganisms)
      newXMLNode("plotID", x@individualOrganisms[[i]]$plotID, parent=io)
      newXMLNode("identificationLabel", x@individualOrganisms[[i]]$identificationLabel, parent=io)
      newXMLNode("taxonNameUsageConceptID", x@individualOrganisms[[i]]$taxonNameUsageConceptID, parent=io)
    }
  }
  #individualOrganismObservation elements
  if(length(x@individualObservations)>0) {
    individualOrganismObservations = newXMLNode("individualOrganismObservations", parent = top)
    for(i in 1:length(x@individualObservations)){
      ioo = newXMLNode("individualOrganismObservation",
                       attrs = c(id = names(x@individualObservations)[i]),
                       parent = individualOrganismObservations)
      newXMLNode("plotObservationID", x@individualObservations[[i]]$plotObservationID, parent=ioo)
      newXMLNode("individualOrganismID", x@individualObservations[[i]]$individualOrganismID, parent=ioo)
      if("stratumObservationID" %in% names(x@individualObservations[[i]])) newXMLNode("stratumObservationID", x@aggregateObservations[[i]]$stratumObservationID, parent=ioo)
      if("diameterValue" %in% names(x@individualObservations[[i]])) {
        diamval = newXMLNode("diameter", parent=ioo)
        newXMLNode("value", x@individualObservations[[i]]$diameterValue, parent=diamval)
        if("diameterAttributeID" %in% names(x@individualObservations[[i]])) newXMLNode("diameterID", x@individualObservations[[i]]$diameterAttributeID, parent=diamval)
      }
    }
  }
  #siteObservation elements
  if(length(x@siteObservations)>0) {
    siteObservations = newXMLNode("siteObservations", parent = top)
    for(i in 1:length(x@siteObservations)){
      abio = newXMLNode("siteObservation",
                        attrs = c(id = names(x@siteObservations)[i]),
                        parent = siteObservations)
      if("soil" %in% names(x@siteObservations[[i]])) {
        soil = newXMLNode("soil", parent=abio)
        if("pH" %in% names(x@siteObservations[[i]]$soil)) { #Add pH information
          pH = newXMLNode("pH", parent=soil)
          newXMLNode("value", x@siteObservations[[i]]$soil$pH$value, parent=pH)
          newXMLNode("attributeID", x@siteObservations[[i]]$soil$pH$attributeID, parent=pH)
        }
      }
    }
  }
  #surfaceCover elements
  if(length(x@surfaceCovers)>0) {
    surfaceCovers = newXMLNode("surfaceCovers", parent = top)
    for(i in 1:length(x@surfaceCovers)){
      sc = newXMLNode("surfaceCover",
                       attrs = c(id = names(x@strata)[i]),
                       parent = surfaceCovers)
      newXMLNode("surfaceName", x@surfaceCovers[[i]]$surfaceName, parent=str)
      newXMLNode("methodID", x@surfaceCovers[[i]]$methodID, parent=str)
    }
  }
  #Attribute elements
  if(length(x@attributes)>0) {
    attributes = newXMLNode("attributes", parent = top)
    for(i in 1:length(x@attributes)){
      att = newXMLNode("attribute",
                       attrs = c(id=names(x@attributes)[i]),
                       parent = attributes)
      atttype = newXMLNode(x@attributes[[i]]$type, parent=att)
      if(x@attributes[[i]]$type=="qualitative") {
        newXMLNode("code", x@attributes[[i]]$code, parent=atttype)
        if("definition" %in% names(x@attributes[[i]])) newXMLNode("definition", x@attributes[[i]]$order, parent=atttype)
      }
      else if(x@attributes[[i]]$type=="ordinal") {
        newXMLNode("code", x@attributes[[i]]$code, parent=atttype)
        if("definition" %in% names(x@attributes[[i]])) newXMLNode("definition", x@attributes[[i]]$order, parent=atttype)
        newXMLNode("order", x@attributes[[i]]$order, parent=atttype)
        newXMLNode("lowerLimit", x@attributes[[i]]$lowerLimit, parent=atttype)
        newXMLNode("upperLimit", x@attributes[[i]]$upperLimit, parent=atttype)
        newXMLNode("midPoint", x@attributes[[i]]$midPoint, parent=atttype)
      }
      else if(x@attributes[[i]]$type=="quantitative") {
        newXMLNode("unit", x@attributes[[i]]$unit, parent=atttype)
        newXMLNode("upperBound", x@attributes[[i]]$upperBound, parent=atttype)
        newXMLNode("lowerBound", x@attributes[[i]]$lowerBound, parent=atttype)
      }
    }
  }
  #Methods
  if(length(x@methods)>0) {
    methods = newXMLNode("methods", parent = top)
    for(i in 1:length(x@methods)){
      met = newXMLNode("method",
                       attrs = c(id=names(x@methods)[i]),
                       parent = methods)
      newXMLNode("name", x@methods[[i]]$name, parent=met)
      newXMLNode("description", x@methods[[i]]$description, parent=met)
      if("citation" %in% names(x@methods[[i]])) if(x@methods[[i]]$citation != "") newXMLNode("citationString", x@methods[[i]]$citation, parent=met)
      newXMLNode("subject", x@methods[[i]]$subject, parent=met)
      newXMLNode("attributeType", x@methods[[i]]$attributeType, parent=met)
    }
  }
  #XML document
  doc = newXMLDoc(top)
  saveXML(doc, file = file)
}
