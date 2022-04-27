#' Write VegX XML file
#'
#' Assembles and writes an XML file on the disk
#' following the Veg-X XML schema standard (ver 2.0.1)
#'
#' @param x an object of class \code{\linkS4class{VegX}}
#' @param file the file name to be written
#' @param verbose A boolean flag to indicate console output of the XML tree building process.
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @importFrom XML newXMLDoc newXMLNode saveXML
#' 
#' @examples
#' \dontrun{
#'   target = newVegX()
#'   writeVegXML(target, "foo.xml")
#' }
#'
#' @export
writeVegXML <- function(x, file, verbose = TRUE) {
  if(class(x)!="VegX") 
    stop("Wrong class for 'x'. Should be an object of class 'VegX'")

  # Top XML node
  doc = newXMLDoc()
  top = newXMLNode(name = "VegX",
                   namespaceDefinitions = c(veg="http://iavs.org/vegx/veg-2.0.0",
                                            plot="http://iavs.org/vegx/veg-plot-2.0.0",
                                            plotobs="http://iavs.org/vegx/veg-plotobservation-2.0.0",
                                            org="http://iavs.org/vegx/veg-organism-2.0.0",
                                            com="http://iavs.org/vegx/veg-community-2.0.0",
                                            misc="http://iavs.org/vegx/veg-misc-2.0.0",
                                            userdef="http://iavs.org/vegx/veg-userdefined-2.0.0",
                                            xsi="http://www.w3.org/2001/XMLSchema-instance"),
                  doc = doc)
  #party elements
  if(length(x@parties)>0){
    prts = newXMLNode("parties", parent = top)
    for(i in 1:length(x@parties)){
      prt = newXMLNode("party",
                       attrs = c("id"=names(x@parties)[i]),
                       parent = prts)
      if(x@parties[[i]]$partyType=="individual") newXMLNode("individualName", x@parties[[i]]$name, parent=prt)
      else if(x@parties[[i]]$partyType=="organization") newXMLNode("organizationName", x@parties[[i]]$name, parent=prt)
      else if(x@parties[[i]]$partyType=="position") newXMLNode("positionName", x@parties[[i]]$name, parent=prt)
      if("address" %in% names(x@parties[[i]])) newXMLNode("address", x@parties[[i]]$address, parent=prt)
      if("phone" %in% names(x@parties[[i]])) newXMLNode("phone", x@parties[[i]]$phone, parent=prt)
      if("electronicMailAddress" %in% names(x@parties[[i]])) newXMLNode("electronicMailAddress", x@parties[[i]]$electronicMailAddress, parent=prt)
      if("onlineURL" %in% names(x@parties[[i]])) newXMLNode("onlineURL", x@parties[[i]]$onlineURL, parent=prt)
    }
    if(verbose) cat(paste0(" ", length(x@parties), " party(ies) added to XML tree.\n"))
  }
  #literatureCitation elements
  if(length(x@literatureCitations)>0){
    lits = newXMLNode("literatureCitations", parent = top)
    for(i in 1:length(x@literatureCitations)){
      lit = newXMLNode("literatureCitation",
                       attrs = c("id"=names(x@literatureCitations)[i]),
                       parent = lits)
      if("citationString" %in% names(x@literatureCitations[[i]])) newXMLNode("citationString", x@literatureCitations[[i]]$citationString, parent=lit)
      if("DOI" %in% names(x@literatureCitations[[i]])) newXMLNode("DOI", x@literatureCitations[[i]]$DOI, parent=lit)
    }
    if(verbose) cat(paste0(" ", length(x@literatureCitations), " literature citation(s) added to XML tree.\n"))
  }
  #method elements
  if(length(x@methods)>0) {
    methods = newXMLNode("methods", parent = top)
    for(i in 1:length(x@methods)){
      met = newXMLNode("method",
                       attrs = c(id=names(x@methods)[i]),
                       parent = methods)
      newXMLNode("name", x@methods[[i]]$name, parent=met)
      newXMLNode("description", x@methods[[i]]$description, parent=met)
      newXMLNode("subject", x@methods[[i]]$subject, parent=met)
      if("citationID" %in% names(x@methods[[i]])) newXMLNode("citationID", x@methods[[i]]$citationID, parent=met)
    }
    if(verbose) cat(paste0(" ", length(x@methods), " method(s) added to XML tree.\n"))
  }
  #attribute elements
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
        if("definition" %in% names(x@attributes[[i]])) newXMLNode("definition", x@attributes[[i]]$definition, parent=atttype)
      }
      else if(x@attributes[[i]]$type=="ordinal") {
        newXMLNode("methodID", x@attributes[[i]]$methodID, parent=atttype)
        newXMLNode("code", x@attributes[[i]]$code, parent=atttype)
        if("definition" %in% names(x@attributes[[i]])) newXMLNode("definition", x@attributes[[i]]$definition, parent=atttype)
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
    if(verbose) cat(paste0(" ", length(x@attributes), " attribute(s) added to XML tree.\n"))
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
    if(verbose) cat(paste0(" ", length(x@strata), " strata added to XML tree.\n"))
  }
  #surfaceType elements
  if(length(x@surfaceTypes)>0) {
    surfaceTypes = newXMLNode("surfaceTypes", parent = top)
    for(i in 1:length(x@surfaceTypes)){
      surf = newXMLNode("surfaceType",
                        attrs = c(id = names(x@surfaceTypes)[i]),
                        parent = surfaceTypes)
      newXMLNode("surfaceName", x@surfaceTypes[[i]]$surfaceName, parent=surf)
      if("methodID" %in% names(x@surfaceTypes[[i]])) newXMLNode("methodID", x@surfaceTypes[[i]]$methodID, parent=surf)
      if("definition" %in% names(x@surfaceTypes[[i]])) newXMLNode("definition", x@surfaceTypes[[i]]$definition, parent=surf)
    }
    if(verbose) cat(paste0(" ", length(x@surfaceTypes), " surface types to XML tree.\n"))
  }
  #organismName elements (TO BE CHECKED)
  if(length(x@organismNames)>0){
    orgnms = newXMLNode("organismNames", parent = top)
    for(i in 1:length(x@organismNames)){
      orgnm = newXMLNode("organismName", x@organismNames[[i]]$name,
                       attrs = c("id"=names(x@organismNames)[i],
                                 "taxonName" = x@organismNames[[i]]$taxon),
                       parent = orgnms)
    }
    if(verbose) cat(paste0(" ", length(x@organismNames), " organism name(s) added to XML tree.\n"))
  }
  #taxonConcept elements (TO BE CHECKED)
  if(length(x@taxonConcepts)>0){
    txcpts = newXMLNode("taxonConcepts", parent = top)
    for(i in 1:length(x@taxonConcepts)){
      txcpt = newXMLNode("taxonConcept",
                         attrs = c("id"=names(x@organismNames)[i]),
                         parent = txcpts)
      if("organismNameID" %in% names(x@taxonConcepts[[i]])) newXMLNode("organismNameID", x@taxonConcepts[[i]]$organismNameID, parent=txcpt)
      if("accordingToCitationID" %in% names(x@taxonConcepts[[i]])) newXMLNode("accordingToCitationID", x@taxonConcepts[[i]]$accordingToCitationID, parent=txcpt)
    }
    if(verbose) cat(paste0(" ", length(x@taxonConcepts), " taxon concept(s) added to XML tree.\n"))
  }
  #organismIdentity elements (TO BE FINISHED)
  if(length(x@organismIdentities)>0) {
    organismIdentities = newXMLNode("organismIdentities", parent = top)
    for(i in 1:length(x@organismIdentities)){
      oi = newXMLNode("organismIdentity",
                      attrs = c(id=names(x@organismIdentities)[i]),
                      parent = organismIdentities)
      newXMLNode("originalOrganismNameID", x@organismIdentities[[i]]$originalOrganismNameID,
                 parent=oi)
      if("originalIdentificationConcept" %in% names(x@organismIdentities[[i]])) {
         ocid = newXMLNode("originalIdentificationConcept", parent=oi)
         newXMLNode("taxonConceptID", x@organismIdentities[[i]]$originalIdentificationConcept$taxonConceptID,
                    parent=ocid)
         if("assertionDate" %in% names(x@organismIdentities[[i]]$originalIdentificationConcept)) 
           newXMLNode("assertionDate", as.character(x@organismIdentities[[i]]$originalIdentificationConcept$assertionDate), parent=ocid)
         if("assertionPartyID" %in% names(x@organismIdentities[[i]]$originalIdentificationConcept)) 
           newXMLNode("assertionPartyID", x@organismIdentities[[i]]$originalIdentificationConcept$assertionPartyID, parent=ocid)
      }
      if("preferredTaxonNomenclature" %in% names(x@organismIdentities[[i]])) {
        ptn = newXMLNode("preferredTaxonNomenclature", parent=oi)
        newXMLNode("preferredTaxonNameID", x@organismIdentities[[i]]$preferredTaxonNomenclature$preferredTaxonNameID,
                   parent=ptn)
        if("interpretationDate" %in% names(x@organismIdentities[[i]]$preferredTaxonNomenclature)) 
          newXMLNode("interpretationDate", as.character(x@organismIdentities[[i]]$preferredTaxonNomenclature$interpretationDate), parent=ptn)
        if("interpretationSource" %in% names(x@organismIdentities[[i]]$preferredTaxonNomenclature)) 
          newXMLNode("interpretationSource", x@organismIdentities[[i]]$preferredTaxonNomenclature$interpretationSource, parent=ptn)
        if("interpretationCitationID" %in% names(x@organismIdentities[[i]]$preferredTaxonNomenclature)) 
          newXMLNode("interpretationCitationID", x@organismIdentities[[i]]$preferredTaxonNomenclature$interpretationCitationID, parent=ptn)
        if("interpretationPartyID" %in% names(x@organismIdentities[[i]]$preferredTaxonNomenclature)) 
          newXMLNode("interpretationPartyID", x@organismIdentities[[i]]$preferredTaxonNomenclature$interpretationPartyID, parent=ptn)
      }
    }
    if(verbose) cat(paste0(" ", length(x@organismIdentities), " organism identitie(s) added to XML tree.\n"))
  }
  #project elements
  if(length(x@projects)>0){
    prjs = newXMLNode("projects", parent = top)
    for(i in 1:length(x@projects)){
      prj = newXMLNode("project",
                       attrs = c("id"=names(x@projects)[i]),
                       parent = prjs)
      newXMLNode("title", x@projects[[i]]$title, parent=prj)
      if("personnel" %in% names(x@projects[[i]])) {
        if(length(x@projects[[i]]$personnel)>0) {
          for(j in 1:length(x@projects[[i]]$personnel)){
            prjpers = newXMLNode("personnel", parent=prj)
            newXMLNode("role", names(x@projects[[i]]$personnel)[j],parent=prjpers)
            newXMLNode("partyID", x@projects[[i]]$personnel[[j]], parent=prjpers)
          }
        }
      }
      if("abstract" %in% names(x@projects[[i]])) newXMLNode("abstract", x@projects[[i]]$abstract, parent=prj)
      if("funding" %in% names(x@projects[[i]])) newXMLNode("funding", x@projects[[i]]$funding, parent=prj)
      if("studyAreaDescription" %in% names(x@projects[[i]])) newXMLNode("studyAreaDescription", x@projects[[i]]$studyAreaDescription, parent=prj)
      if("designDescription" %in% names(x@projects[[i]])) newXMLNode("designDescription", x@projects[[i]]$designDescription, parent=prj)
      if("documentCitationID" %in% names(x@projects[[i]])) newXMLNode("documentCitationID", x@projects[[i]]$documentCitationID, parent=prj)
    }
    if(verbose) cat(paste0(" ", length(x@projects), " project(s) added to XML tree.\n"))
  }
  #plot elements
  if(length(x@plots)>0){
    plots = newXMLNode("plots", parent = top)
    for(i in 1:length(x@plots)){
      p = newXMLNode("plot",
                     attrs = c("id"=names(x@plots)[i]),
                     parent = plots)
      newXMLNode("plotName", x@plots[[i]]$plotName, parent=p)
      if("plotUniqueIdentifier" %in% names(x@plots[[i]])) newXMLNode("plotUniqueIdentifier", x@plots[[i]]$plotUniqueIdentifier, parent=p)
      if("placementPartyID" %in% names(x@plots[[i]])) newXMLNode("placementPartyID", x@plots[[i]]$placementPartyID, parent=p)
      if("parentPlotID" %in% names(x@plots[[i]])) { #Add parent plot information (it is a subplot)
        rp = newXMLNode("relatedPlot", parent=p)
        newXMLNode("relatedPlotID", x@plots[[i]]$parentPlotID, parent=rp)
        newXMLNode("plotRelationship", "subplot", parent=rp)
      }
      #Add location information
      if("location" %in% names(x@plots[[i]])) {
        location = x@plots[[i]]$location
        pl = newXMLNode("location", parent=p)
        if("horizontalCoordinates" %in% names(location)) {
          hc = newXMLNode("horizontalCoordinates", parent=pl)
          if("coordinates" %in% names(location$horizontalCoordinates)) {
            cc = newXMLNode("coordinates", parent=hc)
            newXMLNode("valueX", location$horizontalCoordinates$coordinates$valueX, parent=cc)
            newXMLNode("valueY", location$horizontalCoordinates$coordinates$valueY, parent=cc)
            newXMLNode("spatialReference", location$horizontalCoordinates$coordinates$spatialReference, parent=cc)
            if("attributeID" %in% names(location$horizontalCoordinates$coordinates)) {
              newXMLNode("spatialReference", location$horizontalCoordinates$coordinates$attributeID, parent=cc)
            }
          }
        }
        if("verticalCoordinates" %in% names(location)) {
          vc = newXMLNode("verticalCoordinates", parent=pl)
          if("elevation" %in% names(location$verticalCoordinates)) {
            mes = newXMLNode("elevation", parent=vc)
            newXMLNode("value", location$verticalCoordinates$elevation$value, parent=mes)
            newXMLNode("attributeID", location$verticalCoordinates$elevation$attributeID, parent=mes)
          }
        }
      }
      if("geometry" %in% names(x@plots[[i]])) {
        gm = newXMLNode("geometry", parent=p)
        if("area" %in% names(x@plots[[i]]$geometry)) { #Add area information
          mes = newXMLNode("area", parent=gm)
          newXMLNode("value", x@plots[[i]]$geometry$area$value, parent=mes)
          newXMLNode("attributeID", x@plots[[i]]$geometry$area$attributeID, parent=mes)
        }
        if("shape" %in% names(x@plots[[i]]$geometry)) {
          newXMLNode("shape", x@plots[[i]]$geometry$shape, parent=gm)
        }
        if("radius" %in% names(x@plots[[i]]$geometry)) { #Add radius information
          mes = newXMLNode("radius", parent=gm)
          newXMLNode("value", x@plots[[i]]$geometry$radius$value, parent=mes)
          newXMLNode("attributeID", x@plots[[i]]$geometry$radius$attributeID, parent=mes)
        }
        if("width" %in% names(x@plots[[i]]$geometry)) { #Add length information
          mes = newXMLNode("width", parent=gm)
          newXMLNode("value", x@plots[[i]]$geometry$width$value, parent=mes)
          newXMLNode("attributeID", x@plots[[i]]$geometry$width$attributeID, parent=mes)
        }
        if("length" %in% names(x@plots[[i]]$geometry)) { #Add length information
          mes = newXMLNode("length", parent=gm)
          newXMLNode("value", x@plots[[i]]$geometry$length$value, parent=mes)
          newXMLNode("attributeID", x@plots[[i]]$geometry$length$attributeID, parent=mes)
        }
        if("bandWidth" %in% names(x@plots[[i]]$geometry)) { #Add length information
          mes = newXMLNode("width", parent=gm)
          newXMLNode("value", x@plots[[i]]$geometry$bandWidth$value, parent=mes)
          newXMLNode("attributeID", x@plots[[i]]$geometry$bandWidth$attributeID, parent=mes)
        }
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
    if(verbose) cat(paste0(" ", length(x@plots), " plot(s) added to XML tree.\n"))
  }
  #individualOrganism elements
  if(length(x@individualOrganisms)>0) {
    individualOrganisms = newXMLNode("individualOrganisms", parent = top)
    for(i in 1:length(x@individualOrganisms)){
      io = newXMLNode("individualOrganism",
                      attrs = c(id = names(x@individualOrganisms)[i]),
                      parent = individualOrganisms)
      newXMLNode("plotID", x@individualOrganisms[[i]]$plotID, parent=io)
      newXMLNode("individualOrganismLabel", x@individualOrganisms[[i]]$individualOrganismLabel, parent=io)
      newXMLNode("organismIdentityID", x@individualOrganisms[[i]]$organismIdentityID, parent=io)
    }
    if(verbose) cat(paste0(" ", length(x@individualOrganisms), " individual organism(s) added to XML tree.\n"))
  }
  #plotObservation elements
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
      if("plotObservationUniqueIdentifier" %in% names(x@plotObservations[[i]])) newXMLNode("plotObservationUniqueIdentifier", x@plotObservations[[i]]$plotObservationUniqueIdentifier, parent=po)
      if("siteObservationID" %in% names(x@plotObservations[[i]])) newXMLNode("siteObservationID", x@plotObservations[[i]]$siteObservationID, parent=po)
      if("communityObservationID" %in% names(x@plotObservations[[i]])) newXMLNode("communityObservationID", x@plotObservations[[i]]$communityObservationID, parent=po)
      if("observationPartyID" %in% names(x@plotObservations[[i]])) newXMLNode("observationPartyID", x@plotObservations[[i]]$observationPartyID, parent=po)
    }
    if(verbose) cat(paste0(" ", length(x@plotObservations), " plot observation(s) added to XML tree.\n"))
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
      if("diameterMeasurement" %in% names(x@individualObservations[[i]])) {
        dm = newXMLNode("diameterMeasurement", parent=ioo)
        newXMLNode("value", x@individualObservations[[i]]$diameterMeasurement$value, parent=dm)
        newXMLNode("attributeID", x@individualObservations[[i]]$diameterMeasurement$attributeID, parent=dm)
      }
      if("heightMeasurement" %in% names(x@individualObservations[[i]])) {
        hm = newXMLNode("heightMeasurement", parent=ioo)
        newXMLNode("value", x@individualObservations[[i]]$heightMeasurement$value, parent=hm)
        newXMLNode("attributeID", x@individualObservations[[i]]$heightMeasurement$attributeID, parent=hm)
      }
      if("individualOrganismMeasurements" %in% names(x@individualObservations[[i]])) {
        if(length(x@individualObservations[[i]]$individualOrganismMeasurements)>0) {
          for(j in 1:length(x@individualObservations[[i]]$individualOrganismMeasurements)) {
            iom = newXMLNode("individualOrganismMeasurement", parent=ioo)
            newXMLNode("value", x@individualObservations[[i]]$individualOrganismMeasurements[[j]]$value, parent=iom)
            newXMLNode("attributeID", x@individualObservations[[i]]$individualOrganismMeasurements[[j]]$attributeID, parent=iom)
          }
        }
      }
    }
    if(verbose) cat(paste0(" ", length(x@individualObservations), " individual organism observation(s) added to XML tree.\n"))
  }
  #aggregateOrganismObservation elements
  if(length(x@aggregateObservations)>0) {
    aggregatedOrganismObservations = newXMLNode("aggregateOrganismObservations", parent = top)
    for(i in 1:length(x@aggregateObservations)){
      aoo = newXMLNode("aggregateOrganismObservation",
                       attrs = c(id = names(x@aggregateObservations)[i]),
                       parent = aggregatedOrganismObservations)
      newXMLNode("plotObservationID", x@aggregateObservations[[i]]$plotObservationID, parent=aoo)
      newXMLNode("organismIdentityID", x@aggregateObservations[[i]]$organismIdentityID, parent=aoo)
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
    if(verbose) cat(paste0(" ", length(x@aggregateObservations), " aggregate organism observation(s) added to XML tree.\n"))
  }
  #stratumObservation elements
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
    if(verbose) cat(paste0(" ", length(x@stratumObservations), " stratum observation(s) added to XML tree.\n"))
  }
  #communityObservation elements (TO BE CHECKED)
  if(length(x@communityObservations)>0) {
    communityObservations = newXMLNode("communityObservations", parent = top)
    for(i in 1:length(x@communityObservations)){
      stro = newXMLNode("communityObservation",
                        attrs = c(id = names(x@communityObservations)[i]),
                        parent = communityObservations)
      newXMLNode("plotObservationID", x@communityObservations[[i]]$plotObservationID, parent=stro)
      if("communityMeasurements" %in% names(x@communityObservations[[i]])) {
        if(length(x@communityObservations[[i]]$communityMeasurements)>0) {
          for(j in 1:length(x@communityObservations[[i]]$communityMeasurements)) {
            sm = newXMLNode("communityMeasurement", parent=stro)
            newXMLNode("value", x@communityObservations[[i]]$communityMeasurements[[j]]$value, parent=sm)
            newXMLNode("attributeID", x@communityObservations[[i]]$communityMeasurements[[j]]$attributeID, parent=sm)
          }
        }
      }
    }
    if(verbose) cat(paste0(" ", length(x@communityObservations), " community observation(s) added to XML tree.\n"))
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
      if("waterBodyMeasurements" %in% names(x@siteObservations[[i]])) {
        waterBodyMeasurements = x@siteObservations[[i]]$waterBodyMeasurements
        for(j in 1:length(waterBodyMeasurements)) {
          cm = newXMLNode("waterBodyMeasurement", parent=abio)
          newXMLNode("value", waterBodyMeasurements[[j]]$value, parent=cm)
          newXMLNode("attributeID", waterBodyMeasurements[[j]]$attributeID, parent=cm)
        }
      }
    }
    if(verbose) cat(paste0(" ", length(x@siteObservations), " site observation(s) added to XML tree.\n"))
  }
  #surfaceCoverObservation elements
  if(length(x@surfaceCoverObservations)>0) {
    surfaceCoverObservations = newXMLNode("surfaceCoverObservations", parent = top)
    for(i in 1:length(x@surfaceCoverObservations)){
      sco = newXMLNode("surfaceCoverObservation",
                       attrs = c(id = names(x@surfaceCoverObservations)[i]),
                       parent = surfaceCoverObservations)
      newXMLNode("plotObservationID", x@surfaceCoverObservations[[i]]$plotObservationID, parent=sco)
      newXMLNode("surfaceTypeID", x@surfaceCoverObservations[[i]]$surfaceTypeID, parent=sco)
      if("coverMeasurement" %in% names(x@surfaceCoverObservations[[i]])) {
        cm = newXMLNode("coverMeasurement", parent=sco)
        newXMLNode("value", x@surfaceCoverObservations[[i]]$coverMeasurement$value, parent=cm)
        newXMLNode("attributeID", x@surfaceCoverObservations[[i]]$coverMeasurement$attributeID, parent=cm)
      }
    }
    if(verbose) cat(paste0(" ", length(x@surfaceCoverObservations), " surface cover observation(s) added to XML tree.\n"))
  }
  #XML document
  saveXML(doc, file = file)
  if(verbose) cat(paste0(" XML file '", file, "' written. \n"))
  rm(doc)
}
