#' Write VegX XML file (ver 1.5.3)
#'
#' @param x an object of class \code{\linkS4class{VegX}}
#' @param file the file name to be written
#'
#' @return
#' @export
#'
#' @examples
writeVegXML<-function(x, file) {
  # Top XML node
  top = newXMLNode("VegX")

  #Plot elements
  if(length(x@plots)>0){
    plots = newXMLNode("plots", parent = top)
    for(i in 1:length(x@plots)){
      p = newXMLNode("plot", parent = plots)
      newXMLNode("plotUniqueIdentifier", names(x@plots)[i], parent=p)
      newXMLNode("plotName", x@plots[[i]]$plotName, parent=p)
    }
  }
  #PlotObservation elements
  if(length(x@plotObservations)>0){
    plotObservations = newXMLNode("plotObservations", parent = top)
    for(i in 1:length(x@plotObservations)){
      po = newXMLNode("plotObservation",
                      attrs = c("id" = names(x@plotObservations)[i]),
                      parent = plotObservations)
      newXMLNode("plotUniqueIdentifier", x@plotObservations[[i]]$plotID, parent=po)
      newXMLNode("obsStartDate", as.character(x@plotObservations[[i]]$obsStartDate), parent=po)
    }
  }
  #TaxonName elements
  if(length(x@taxonNames)>0) {
    taxonNames = newXMLNode("taxonNames", parent = top)
    for(i in 1:length(x@taxonNames)){
      tn = newXMLNode("ScientificName", parent = taxonNames)
      newXMLNode("Simple", x@taxonNames[[i]]$taxonName, parent=tn)
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
  #individualOrganismObservation elements
  if(length(x@individualObservations)>0) {
    individualOrganismObservations = newXMLNode("individualOrganismObservations", parent = top)
  }
  #AggregatedOrganismObservation elements
  if(length(x@aggregatedObservations)>0) {
    aggregatedOrganismObservations = newXMLNode("aggregatedOrganismObservations", parent = top)
    for(i in 1:length(x@aggregatedObservations)){
      aoo = newXMLNode("aggregatedOrganismObservation", parent = aggregatedOrganismObservations)
      newXMLNode("plotObservationID", x@aggregatedObservations[[i]]$plotObservationID, parent=aoo)
      newXMLNode("taxonNameUsageConceptID", x@aggregatedObservations[[i]]$taxonNameUsageConceptID, parent=aoo)
      aggval = newXMLNode("aggregateValue", parent=aoo)
      newXMLNode("value", x@aggregatedObservations[[i]]$value, parent=aggval)
      newXMLNode("attributeID", x@aggregatedObservations[[i]]$attributeID, parent=aggval)
    }
  }
  #StratumObservation elements
  if(length(x@stratumObservations)>0) {
    stratumObservations = newXMLNode("stratumObservations", parent = top)
  }
  #stratum elements
  if(length(x@strata)>0) {
    strata = newXMLNode("strata", parent = top)
  }
  #Attribute elements
  if(length(x@attributes)>0) {
    attributes = newXMLNode("attributes", parent = top)
    for(i in 1:length(x@attributes)){
      att = newXMLNode("attribute",
                       attrs = c(id=names(x@attributes)[i]),
                       parent = attributes)
      atttype = newXMLNode(x@attributes[[i]]$type, parent=att)
      if(x@attributes[[i]]$type=="ordinal") {
        newXMLNode("code", x@attributes[[i]]$code, parent=atttype)
        newXMLNode("order", x@attributes[[i]]$order, parent=atttype)
        newXMLNode("lowerLimit", x@attributes[[i]]$lowerLimit, parent=atttype)
        newXMLNode("upperLimit", x@attributes[[i]]$upperLimit, parent=atttype)
        newXMLNode("midPoint", x@attributes[[i]]$midPoint, parent=atttype)
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
