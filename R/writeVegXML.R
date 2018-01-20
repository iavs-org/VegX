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
  #AggregatedOrganismObservation elements
  if(length(x@aggregatedObservations)>0) {
    aggregatedOrganismObservations = newXMLNode("aggregatedOrganismObservations", parent = top)
    for(i in 1:length(x@aggregatedObservations)){
      aoo = newXMLNode("aggregatedOrganismObservation",
                       attrs = c(id = names(x@aggregatedObservations)[i]),
                       parent = aggregatedOrganismObservations)
      newXMLNode("plotObservationID", x@aggregatedObservations[[i]]$plotObservationID, parent=aoo)
      newXMLNode("taxonNameUsageConceptID", x@aggregatedObservations[[i]]$taxonNameUsageConceptID, parent=aoo)
      if("stratumObservationID" %in% names(x@aggregatedObservations[[i]])) newXMLNode("stratumObservationID", x@aggregatedObservations[[i]]$stratumObservationID, parent=aoo)
      aggval = newXMLNode("aggregateValue", parent=aoo)
      newXMLNode("value", x@aggregatedObservations[[i]]$value, parent=aggval)
      newXMLNode("attributeID", x@aggregatedObservations[[i]]$attributeID, parent=aggval)
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
      newXMLNode("stratumSequence", x@strata[[i]]$stratumSequence, parent=str)
      newXMLNode("lowerBound", x@strata[[i]]$lowerBound, parent=str)
      newXMLNode("upperBound", x@strata[[i]]$upperBound, parent=str)
      newXMLNode("methodID", x@strata[[i]]$methodID, parent=str)
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
      if("stratumObservationID" %in% names(x@individualObservations[[i]])) newXMLNode("stratumObservationID", x@aggregatedObservations[[i]]$stratumObservationID, parent=ioo)
      if("diameterValue" %in% names(x@individualObservations[[i]])) {
        diamval = newXMLNode("diameter", parent=ioo)
        newXMLNode("value", x@individualObservations[[i]]$diameterValue, parent=diamval)
        if("diameterID" %in% names(x@individualObservations[[i]])) newXMLNode("diameterID", x@individualObservations[[i]]$diameterID, parent=diamval)
      }
    }
  }
  #AbioticObservation elements
  if(length(x@abioticObservations)>0) {
    abioticObservations = newXMLNode("abioticObservations", parent = top)
    for(i in 1:length(x@abioticObservations)){
      abio = newXMLNode("abioticObservation",
                        attrs = c(id = names(x@abioticObservations)[i]),
                        parent = abioticObservations)
      if("soil" %in% names(x@abioticObservations[[i]])) {
        soil = newXMLNode("soil", parent=abio)
        if("pH" %in% names(x@abioticObservations[[i]]$soil)) { #Add pH information
          pH = newXMLNode("pH", parent=soil)
          newXMLNode("value", x@abioticObservations[[i]]$soil$pH$value, parent=pH)
          newXMLNode("attributeID", x@abioticObservations[[i]]$soil$pH$attributeID, parent=pH)
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
      newXMLNode("lowerBound", x@attributes[[i]]$lowerBound, parent=atttype)
      if(x@attributes[[i]]$type=="ordinal") {
        newXMLNode("code", x@attributes[[i]]$code, parent=atttype)
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
      newXMLNode("attributeClass", x@methods[[i]]$attributeClass, parent=met)
      newXMLNode("attributeType", x@methods[[i]]$attributeType, parent=met)
    }
  }
  #XML document
  doc = newXMLDoc(top)
  saveXML(doc, file = file)
}
