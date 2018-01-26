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
  doc = newXMLDoc()
  top = newXMLNode(name = "VegX",
                   namespaceDefinitions = c(acc="eml://ecoinformatics.org/access-2.0.1",
                                  cov="eml://ecoinformatics.org/coverage-2.0.1",
                                  doc="eml://ecoinformatics.org/documentation-2.0.1",
                                  party="eml://ecoinformatics.org/party-2.0.1",
                                  proj="eml://ecoinformatics.org/project-2.0.1",
                                  prot="eml://ecoinformatics.org/protocol-2.0.1",
                                  res="eml://ecoinformatics.org/resource-2.0.1",
                                  lit="eml://ecoinformatics.org/literature-2.0.1",
                                  txt="eml://ecoinformatics.org/text-2.0.1",
                                  tcs="http://www.tdwg.org/schemas/tcs/1.01",
                                  dwe="http://rs.tdwg.org/dwc/dwelement",
                                  dwg="http://rs.tdwg.org/dwc/geospatial/",
                                  veg="https://github.com/miquelcaceres/VegX/tree/master/vegxschema/veg-2.0.0",
                                  obs="https://github.com/miquelcaceres/VegX/tree/master/vegxschema/veg-misc-2.0.0",
                                  org="https://github.com/miquelcaceres/VegX/tree/master/vegxschema/veg-organismobservation-2.0.0",
                                  plot="https://github.com/miquelcaceres/VegX/tree/master/vegxschema/veg-plot-2.0.0",
                                  plotobs="https://github.com/miquelcaceres/VegX/tree/master/vegxschema/veg-plotobservation-2.0.0",
                                  userdef="https://github.com/miquelcaceres/VegX/tree/master/vegxschema/veg-userdefined-2.0.0",
                                  xsi="http://www.w3.org/2001/XMLSchema-instance"))
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
      newXMLNode("authorTaxonName", x@taxonNameUsageConcepts[[i]]$authorTaxonName,
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
      if("heightMeasurement" %in% names(x@aggregateObservations[[i]])) {
        hm = newXMLNode("heightMeasurement", parent=aoo)
        newXMLNode("value", x@aggregateObservations[[i]]$heightMeasurement$value, parent=hm)
        newXMLNode("attributeID", x@aggregateObservations[[i]]$heightMeasurement$attributeID, parent=hm)
      }
      if("aggregateOrganismMeasurements" %in% names(x@aggregateObservations[[i]])) {
        if(length(x@aggregateObservations[[i]]$aggregateOrganismMeasurements)>0) {
          for(j in 1:length(x@aggregateObservations[[i]]$aggregateOrganismMeasurements)) {
            aom = newXMLNode("aggregateOrganismMeasurement", parent=aoo)
            newXMLNode("value", x@aggregateObservations[[i]]$aggregateOrganismMeasurements[[j]]$value, parent=aom)
            newXMLNode("attributeID", x@aggregateObservations[[i]]$aggregateOrganismMeasurements[[j]]$attributeID, parent=aom)
          }
        }
      }
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
      if("definition" %in% names(x@strata[[i]])) newXMLNode("definition", x@strata[[i]]$definition, parent=str)
      if("order" %in% names(x@strata[[i]])) newXMLNode("order", x@strata[[i]]$order, parent=str)
      if("lowerLimit" %in% names(x@strata[[i]])) newXMLNode("lowerLimit", x@strata[[i]]$lowerLimit, parent=str)
      if("upperLimit" %in% names(x@strata[[i]])) newXMLNode("upperLimit", x@strata[[i]]$upperLimit, parent=str)
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
      if("lowerLimitMeasurement" %in% names(x@stratumObservations[[i]])) {
        llm = newXMLNode("lowerLimitMeasurement", parent=stro)
        newXMLNode("value", x@stratumObservations[[i]]$lowerLimitMeasurement$value, parent=llm)
        newXMLNode("attributeID", x@stratumObservations[[i]]$lowerLimitMeasurement$attributeID, parent=llm)
      }
      if("upperLimitMeasurement" %in% names(x@stratumObservations[[i]])) {
        ulm = newXMLNode("upperLimitMeasurement", parent=stro)
        newXMLNode("value", x@stratumObservations[[i]]$upperLimitMeasurement$value, parent=ulm)
        newXMLNode("attributeID", x@stratumObservations[[i]]$upperLimitMeasurement$attributeID, parent=ulm)
      }
      if("stratumMeasurements" %in% names(x@stratumObservations[[i]])) {
        if(length(x@stratumObservations[[i]]$stratumMeasurements)>0) {
          for(j in 1:length(x@stratumObservations[[i]]$stratumMeasurements)) {
            sm = newXMLNode("stratumMeasurement", parent=stro)
            newXMLNode("value", x@stratumObservations[[i]]$stratumMeasurements[[j]]$value, parent=sm)
            newXMLNode("attributeID", x@stratumObservations[[i]]$stratumMeasurements[[j]]$attributeID, parent=sm)
          }
        }
      }
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
      if("plotObservationID" %in% names(x@siteObservations[[i]])) newXMLNode("plotObservationID", x@siteObservations[[i]]$plotObservationID, parent=abio)
      if("soilMeasurements" %in% names(x@siteObservations[[i]])) {
        soilMeasurements = x@siteObservations[[i]]$soilMeasurements
        for(j in 1:length(soilMeasurements)) {
          sm = newXMLNode("soilMeasurement", parent=abio)
          newXMLNode("value", soilMeasurements[[j]]$value, parent=sm)
          newXMLNode("attributeID", soilMeasurements[[j]]$attributeID, parent=sm)
        }
      }
      if("climateMeasurements" %in% names(x@siteObservations[[i]])) {
        climateMeasurements = x@siteObservations[[i]]$climateMeasurements
        for(j in 1:length(climateMeasurements)) {
          cm = newXMLNode("climateMeasurement", parent=abio)
          newXMLNode("value", climateMeasurements[[j]]$value, parent=cm)
          newXMLNode("attributeID", climateMeasurements[[j]]$attributeID, parent=cm)
        }
      }
      if("waterMassMeasurements" %in% names(x@siteObservations[[i]])) {
        waterMassMeasurements = x@siteObservations[[i]]$waterMassMeasurements
        for(j in 1:length(waterMassMeasurements)) {
          cm = newXMLNode("waterMassMeasurement", parent=abio)
          newXMLNode("value", waterMassMeasurements[[j]]$value, parent=cm)
          newXMLNode("attributeID", waterMassMeasurements[[j]]$attributeID, parent=cm)
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
        newXMLNode("methodID", x@attributes[[i]]$methodID, parent=atttype)
        newXMLNode("code", x@attributes[[i]]$code, parent=atttype)
        if("definition" %in% names(x@attributes[[i]])) newXMLNode("definition", x@attributes[[i]]$order, parent=atttype)
      }
      else if(x@attributes[[i]]$type=="ordinal") {
        newXMLNode("methodID", x@attributes[[i]]$methodID, parent=atttype)
        newXMLNode("code", x@attributes[[i]]$code, parent=atttype)
        if("definition" %in% names(x@attributes[[i]])) newXMLNode("definition", x@attributes[[i]]$order, parent=atttype)
        newXMLNode("order", x@attributes[[i]]$order, parent=atttype)
        newXMLNode("lowerLimit", x@attributes[[i]]$lowerLimit, parent=atttype)
        newXMLNode("upperLimit", x@attributes[[i]]$upperLimit, parent=atttype)
        newXMLNode("midPoint", x@attributes[[i]]$midPoint, parent=atttype)
      }
      else if(x@attributes[[i]]$type=="quantitative") {
        newXMLNode("methodID", x@attributes[[i]]$methodID, parent=atttype)
        newXMLNode("unit", x@attributes[[i]]$unit, parent=atttype)
        if("precision" %in% names(x@attributes[[i]])) newXMLNode("precision", x@attributes[[i]]$unit, parent=atttype)
        newXMLNode("lowerLimit", x@attributes[[i]]$lowerLimit, parent=atttype)
        newXMLNode("upperLimit", x@attributes[[i]]$upperLimit, parent=atttype)
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
      newXMLNode("subject", x@methods[[i]]$subject, parent=met)
      if("citation" %in% names(x@methods[[i]])) if(x@methods[[i]]$citation != "") newXMLNode("citationString", x@methods[[i]]$citation, parent=met)
    }
  }
  #XML document
  doc = newXMLDoc(top)
  saveXML(doc, file = file)
}
