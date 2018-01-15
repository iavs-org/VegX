#' Write VegX XML file
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
  #List elements
  plots = newXMLNode("plots", parent = top)
  plotObservations = newXMLNode("plotObservations", parent = top)
  taxonNames = newXMLNode("taxonNames", parent = top)
  taxonConcepts = newXMLNode("taxonConcepts", parent = top)
  taxonNameUsageConcepts = newXMLNode("taxonNameUsageConcepts", parent = top)
  individualOrganismObservations = newXMLNode("individualOrganismObservations", parent = top)
  aggregatedOrganismObservations = newXMLNode("aggregatedOrganismObservations", parent = top)
  stratumObservations = newXMLNode("stratumObservations", parent = top)
  strata = newXMLNode("strata", parent = top)
  methods = newXMLNode("methods", parent = top)
  attributes = newXMLNode("attributes", parent = top)

  #Plot elements
  for(i in 1:length(x@plots)){
    p = newXMLNode("plot", parent = plots)
    newXMLNode("plotUniqueIdentifier", names(x@plots)[i], parent=p)
    newXMLNode("plotName", x@plots[[i]]$plotName, parent=p)
  }
  #PlotObservation elements
  for(i in 1:length(x@plotObservations)){
    po = newXMLNode("plotObservation",
                    attrs = c("id" = names(x@plotObservations)[i]),
                    parent = plotObservations)
    newXMLNode("plotUniqueIdentifier", x@plotObservations[[i]]$plotID, parent=po)
    newXMLNode("obsStartDate", as.character(x@plotObservations[[i]]$obsStartDate), parent=po)
  }
  #TaxonName elements
  if(length(x@taxonNames)>0) {
    for(i in 1:length(x@taxonNames)){
      tn = newXMLNode("ScientificName", parent = taxonNames)
      newXMLNode("Simple", x@taxonNames[[i]]$taxonName, parent=tn)
    }
  }
  #TaxonNameUsageConcept elements
  for(i in 1:length(x@taxonNameUsageConcepts)){
    tnuc = newXMLNode("taxonNameUsageConcept",
                      attrs = c(id=names(x@taxonNameUsageConcepts)[i]),
                      parent = taxonNameUsageConcepts)
    newXMLNode("authorName", x@taxonNameUsageConcepts[[i]]$authorName,
               # attrs = c(type = "usage name"),
               parent=tnuc)
  }
  #AggregatedOrganismObservations elements
  if(length(x@aggregatedObservations)>0) {
    for(i in 1:length(x@aggregatedObservations)){
      aoo = newXMLNode("aggregatedOrganismObservation", parent = aggregatedOrganismObservations)
      newXMLNode("plotObservationID", x@aggregatedObservations[[i]]$plotObservationID, parent=aoo)
      newXMLNode("taxonNameUsageConceptID", x@aggregatedObservations[[i]]$taxonNameUsageConceptID, parent=aoo)
      aggval = newXMLNode("aggregateValue", parent=aoo)
      newXMLNode("value", x@aggregatedObservations[[i]]$value, parent=aggval)
      newXMLNode("attributeID", x@aggregatedObservations[[i]]$attributeID, parent=aggval)
    }
  }

  #XML document
  doc = newXMLDoc(top)
  saveXML(doc, file = file)
}
