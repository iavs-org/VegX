#' Reads a Veg-X XML file
#'
#' @param file the filename of an XML file following the Veg-X standard (ver. 2.0.1)
#' @param verbose A boolean flag to indicate console output of the reading process.
#'
#' @return An object of class \code{\linkS4class{VegX}}
#'
readVegXML<-function(file, verbose = TRUE) {
  target = newVegX()
  veg=xmlRoot(xmlTreeParse(file, useInternalNodes = T))
  vegnames = names(veg)

  #Auxiliary functions
  .readVegXMeasurement.2.0.X = function(x) {
    return(list(value = xmlValue(x[["value"]]), attributeID = xmlValue(x[["attributeID"]])))
  }

  #read parties
  .readParty.2.0.X = function(x) {
    party = list()
    n = names(x)
    if("individualName" %in% n) {
      party$name = xmlValue(x[["individualName"]])
      party$partyType = "individual"
    }
    else if("organizationName" %in% n) {
      party$name = xmlValue(x[["organizationName"]])
      party$partyType = "organization"
    }
    else if("positionName" %in% n) {
      party$name = xmlValue(x[["positionName"]])
      party$partyType = "position"
    }
    if("address" %in% n) party$address = xmlValue(x[["address"]])
    if("phone" %in% n) party$phone = xmlValue(x[["phone"]])
    if("electronicMailAddress" %in% n) party$electronicMailAddress = xmlValue(x[["electronicMailAddress"]])
    if("onlineURL" %in% n) party$onlineURL = xmlValue(x[["onlineURL"]])

    return(party)
  }
  if("parties" %in% vegnames) {
    target@parties = xmlApply(veg[["parties"]], .readParty.2.0.X)
    names(target@parties) = xmlApply(veg[["parties"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@parties), " party(ies) read.\n"))
  }

  #read literatureCitations
  .readLiteratureCitation.2.0.X = function(x) {
    literatureCitation = list()
    n = names(x)
    if("citationString" %in% n) literatureCitation$citationString = xmlValue(x[["citationString"]])
    if("DOI" %in% n) literatureCitation$DOI = xmlValue(x[["DOI"]])
    return(literatureCitation)
  }
  if("literatureCitations" %in% vegnames) {
    target@literatureCitations = xmlApply(veg[["literatureCitations"]], .readLiteratureCitation.2.0.X)
    names(target@literatureCitations) = xmlApply(veg[["literatureCitations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@literatureCitations), " literature citation(s) read.\n"))
  }

  #read methods
  .readMethod.2.0.X = function(x) {
    met = list()
    met$name = xmlValue(x[["name"]])
    n = names(x)
    if("description" %in% n) met$description = xmlValue(x[["description"]])
    if("subject" %in% n) met$subject = xmlValue(x[["subject"]])
    if("citationID" %in% n) met$citationID = xmlValue(x[["citationID"]])
    return(met)
  }
  if("methods" %in% vegnames) {
    target@methods = xmlApply(veg[["methods"]], .readMethod.2.0.X)
    names(target@methods) = xmlApply(veg[["methods"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@methods), " method(s) read.\n"))
  }

  #read attributes
  .readAttribute.2.0.X = function(x) {
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
    target@attributes = xmlApply(veg[["attributes"]], .readAttribute.2.0.X)
    names(target@attributes) = xmlApply(veg[["attributes"]], xmlAttrs)
    for(att in target@attributes) target@methods[[att$methodID]]$attributeType = att$type #Sets method type from attribute type
    if(verbose) cat(paste0(" ", length(target@attributes), " attribute(s) read.\n"))
  }

  #read strata
  .readStratum.2.0.X = function(x) {
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
    target@strata = xmlApply(veg[["strata"]], .readStratum.2.0.X)
    names(target@strata) = xmlApply(veg[["strata"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@strata), " strata read.\n"))
  }

  #read surface types
  .readSurfaceType.2.0.X = function(x) {
    str = list(surfaceName = xmlValue(x[["surfaceName"]]))
    n = names(x)
    if("methodID" %in% n) str$methodID = xmlValue(x[["methodID"]])
    if("definition" %in% n) str$definition = xmlValue(x[["definition"]])
    return(str)
  }
  if("surfaceTypes" %in% vegnames) {
    target@surfaceTypes = xmlApply(veg[["surfaceTypes"]], .readSurfaceType.2.0.X)
    names(target@surfaceTypes) = xmlApply(veg[["surfaceTypes"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@surfaceTypes), " surface types read.\n"))
  }

  #read organism names
  .readOrganismName.2.0.X = function(x) {
    orgName = list()
    orgName$name = xmlValue(x)
    orgName$taxon = as.logical(xmlAttrs(x)[["taxonName"]])
    return(orgName)
  }
  if("organismNames" %in% vegnames) {
    target@organismNames = xmlApply(veg[["organismNames"]], .readOrganismName.2.0.X)
    names(target@organismNames) = xmlApply(veg[["organismNames"]], function(x) {return(xmlAttrs(x)[[1]])})
    if(verbose) cat(paste0(" ", length(target@organismNames), " organism name(s) read.\n"))
  }

  #read taxon concepts
  .readTaxonConcept.2.0.X = function(x) {
    txCpt = list()
    n = names(x)
    if("organismNameID" %in% n)  txCpt$organismNameID = xmlValue(x[["organismNameID"]])
    if("accordingToCitationID" %in% n)  txCpt$accordingToCitationID = xmlValue(x[["accordingToCitationID"]])
    return(txCpt)
  }
  if("taxonConcepts" %in% vegnames) {
    target@taxonConcepts = xmlApply(veg[["taxonConcepts"]], .readTaxonConcept.2.0.X)
    names(target@taxonConcepts) = xmlApply(veg[["taxonConcepts"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@taxonConcepts), " taxon concept(s) read.\n"))
  }

  #read organism identities (TO BE FINISHED)
  .readOrganismIdentity.2.0.X = function(x) {
    orgId = list()
    n = names(x)
    if("originalOrganismNameID" %in% n)  orgId$originalOrganismNameID = xmlValue(x[["originalOrganismNameID"]])
    if("originalIdentificationConcept" %in% n)  {
      ocid = x[["originalIdentificationConcept"]]
      orgId$originalIdentificationConcept = list(taxonConceptID = xmlValue(ocid[["taxonConceptID"]]))
      
      if("assertionDate" %in% names(ocid)) orgId$originalIdentificationConcept$assertionDate = as.Date(xmlValue(ocid[["assertionDate"]]), format = "%Y-%m-%d")
      if("assertionPartyID" %in% names(ocid)) orgId$originalIdentificationConcept$assertionPartyID = xmlValue(ocid[["assertionPartyID"]])
    }
    if("preferredTaxonNomenclature" %in% n)  {
      ptn = x[["preferredTaxonNomenclature"]]
      orgId$preferredTaxonNomenclature = list(preferredTaxonNameID = xmlValue(ptn[["preferredTaxonNameID"]]))
      if("interpretationDate" %in% names(ptn)) orgId$preferredTaxonNomenclature$interpretationDate = as.Date(xmlValue(ptn[["interpretationDate"]]), format = "%Y-%m-%d")
      if("interpretationPartyID" %in% names(ptn)) orgId$preferredTaxonNomenclature$interpretationPartyID = xmlValue(ptn[["interpretationPartyID"]])
      if("interpretationSource" %in% names(ptn)) orgId$preferredTaxonNomenclature$interpretationSource = xmlValue(ptn[["interpretationSource"]])
      if("interpretationCitationID" %in% names(ptn)) orgId$preferredTaxonNomenclature$interpretationCitationID = xmlValue(ptn[["interpretationCitationID"]])
    }
    return(orgId)
  }
  if("organismIdentities" %in% vegnames) {
    target@organismIdentities = xmlApply(veg[["organismIdentities"]], .readOrganismIdentity.2.0.X)
    names(target@organismIdentities) = xmlApply(veg[["organismIdentities"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@organismIdentities), " organism identitie(s) read.\n"))
  }


  #read projects
  .readProject.2.0.X = function(x) {
    project = list()
    n = names(x)
    project$title = xmlValue(x[["title"]])

    for(i in 1:length(n)) {
      if(n[i]=="personnel") {
        project$personnel[[xmlValue(x[[i]][["role"]])]] = xmlValue(x[[i]][["partyID"]])
      }
      else if(n[i] == "abstract") project$abstract = xmlValue(x[[i]])
      else if(n[i] == "funding") project$funding = xmlValue(x[[i]])
      else if(n[i] == "studyAreaDescription") project$studyAreaDescription = xmlValue(x[[i]])
      else if(n[i] == "designDescription") project$designDescription = xmlValue(x[[i]])
      else if(n[i] == "documentCitationID") project$documentCitationID = xmlValue(x[[i]])
    }
    return(project)
  }
  if("projects" %in% vegnames) {
    target@projects = xmlApply(veg[["projects"]], .readProject.2.0.X)
    names(target@projects) = xmlApply(veg[["projects"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@projects), " project(s) read.\n"))
  }

  #read plots
  .readPlot.2.0.X = function(x) {
    plot = list()
    plot$plotName = xmlValue(x[["plotName"]])
    n = names(x)
    if("plotUniqueIdentifier" %in% n) plot$plotUniqueIdentifier = xmlValue(x[["plotUniqueIdentifier"]])
    if("placementPartyID" %in% n) plot$placementPartyID = xmlValue(x[["placementPartyID"]])
    if("relatedPlot" %in% n) {
      rp = x[["relatedPlot"]]
      if(xmlValue(rp[["plotRelationship"]])=="subplot") {
        plot$parentPlotID = xmlValue(rp[["relatedPlotID"]])
      } else {
        warning("Plot relationship could not be parsed!")
      }
    }
    if("location" %in% n) {
      loc = x[["location"]]
      plot$location = list()
      if("horizontalCoordinates" %in% names(loc)) {
        plot$location$horizontalCoordinates = list()
        hc = loc[["horizontalCoordinates"]]
        if("coordinates" %in% names(hc)) {
          cc = hc[["coordinates"]]
          plot$location$horizontalCoordinates$coordinates$valueX = xmlValue(cc[["valueX"]])
          plot$location$horizontalCoordinates$coordinates$valueY = xmlValue(cc[["valueY"]])
          plot$location$horizontalCoordinates$coordinates$spatialReference = xmlValue(cc[["spatialReference"]])
          if("attributeID" %in% names(cc)) {
            plot$location$horizontalCoordinates$coordinates$attributeID = xmlValue(cc[["attributeID"]])
          }
        }
      }
      if("verticalCoordinates" %in% names(loc)) {
        plot$location$verticalCoordinates = list()
        vc = loc[["verticalCoordinates"]]
        if("elevation" %in% names(vc)) {
          elev = vc[["elevation"]]
          plot$location$verticalCoordinates$elevation = .readVegXMeasurement.2.0.X(elev)
        }
      }
    }
    if("geometry" %in% n) {
      gm = x[["geometry"]]
      plot$geometry = list()
      if("area" %in% names(gm)) plot$geometry$area = .readVegXMeasurement.2.0.X(gm[["area"]])
      if("shape" %in% names(gm)) {
        plot$geometry$shape = xmlValue(gm[["shape"]])
      }
      if("radius" %in% names(gm)) plot$geometry$radius = .readVegXMeasurement.2.0.X(gm[["radius"]])
      if("length" %in% names(gm)) plot$geometry$length = .readVegXMeasurement.2.0.X(gm[["length"]])
      if("width" %in% names(gm)) plot$geometry$width = .readVegXMeasurement.2.0.X(gm[["width"]])
      if("bandWidth" %in% names(gm)) plot$geometry$bandWidth = .readVegXMeasurement.2.0.X(gm[["bandWidth"]])
    }
    if("topography" %in% n) {
      topo = x[["topography"]]
      plot$topography = list()
      if("slope" %in% names(topo)) plot$topography$slope = .readVegXMeasurement.2.0.X(topo[["slope"]])
      if("aspect" %in% names(topo)) plot$topography$aspect = .readVegXMeasurement.2.0.X(topo[["aspect"]])
    }
    return(plot)
  }
  if("plots" %in% vegnames) {
    target@plots = xmlApply(veg[["plots"]], .readPlot.2.0.X)
    names(target@plots) = xmlApply(veg[["plots"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@plots), " plot(s) read.\n"))
  }

  #read individual organisms
  .readIndividualOrganism.2.0.X = function(x) {
    ind = list(plotID = xmlValue(x[["plotID"]]),
               individualOrganismLabel = xmlValue(x[["individualOrganismLabel"]]))
    n = names(x)
    if("organismIdentityID" %in% n) ind$organismIdentityID = xmlValue(x[["organismIdentityID"]])
    return(ind)
  }
  if("individualOrganisms" %in% vegnames) {
    target@individualOrganisms = xmlApply(veg[["individualOrganisms"]], .readIndividualOrganism.2.0.X)
    names(target@individualOrganisms) = xmlApply(veg[["individualOrganisms"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@individualOrganisms), " individual organism(s) read.\n"))
  }

  #read plot observations
  .readPlotObservation.2.0.X = function(x) {
    plotObs = list(plotID = xmlValue(x[["plotID"]]),
                   obsStartDate = as.Date(xmlValue(x[["obsStartDate"]]), format = "%Y-%m-%d"))
    n = names(x)
    if("plotObservationUniqueIdentifier" %in% n) plotObs$plotObservationUniqueIdentifier = xmlValue(x[["plotObservationUniqueIdentifier"]])
    if("projectID" %in% n) plotObs$projectID = xmlValue(x[["projectID"]])
    if("obsEndDate" %in% n) plotObs$obsEndDate = as.Date(xmlValue(x[["obsEndDate"]]), format = "%Y-%m-%d")
    if("siteObservationID" %in% n) plotObs$siteObservationID = xmlValue(x[["siteObservationID"]])
    if("communityObservationID" %in% n) plotObs$communityObservationID = xmlValue(x[["communityObservationID"]])
    if("observationPartyID" %in% n) plotObs$observationPartyID = xmlValue(x[["observationPartyID"]])

    return(plotObs)
  }
  if("plotObservations" %in% vegnames) {
    target@plotObservations = xmlApply(veg[["plotObservations"]], .readPlotObservation.2.0.X)
    names(target@plotObservations) = xmlApply(veg[["plotObservations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@plotObservations), " plot observation(s) read.\n"))
  }

  #read individual organism observations
  .readIndividualOrganismObservation.2.0.X = function(x) {
    indObs = list(plotObservationID = xmlValue(x[["plotObservationID"]]),
                  individualOrganismID = xmlValue(x[["individualOrganismID"]]))
    n = names(x)
    if("stratumObservationID" %in% n) indObs$stratumObservationID = xmlValue(x[["stratumObservationID"]])
    for(nm in n) {
      if(nm=="heightMeasurement") indObs$heightMeasurement = .readVegXMeasurement.2.0.X(x[[nm]])
      else if(nm=="diameterMeasurement") indObs$diameterMeasurement = .readVegXMeasurement.2.0.X(x[[nm]])
      else if(nm=="individualOrganismMeasurement") {
        if(!("individualOrganismMeasurements" %in% names(indObs))) indObs$individualOrganismMeasurements = list()
        mesid = as.character(length(indObs$individualOrganismMeasurements)+1)
        indObs$individualOrganismMeasurements[[mesid]] = .readVegXMeasurement.2.0.X(x[[nm]])
      }
    }
    return(indObs)
  }
  if("individualOrganismObservations" %in% vegnames) {
    target@individualObservations = xmlApply(veg[["individualOrganismObservations"]], .readIndividualOrganismObservation.2.0.X)
    names(target@individualObservations) = xmlApply(veg[["individualOrganismObservations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@individualObservations), " individual organism observation(s) read.\n"))
  }

  #read aggregate organism observations
  .readAggregateOrganismObservation.2.0.X = function(x) {
    aggObs = list(plotObservationID = xmlValue(x[["plotObservationID"]]),
                  organismIdentityID = xmlValue(x[["organismIdentityID"]]))
    n = names(x)
    if("stratumObservationID" %in% n) aggObs$stratumObservationID = xmlValue(x[["stratumObservationID"]])
    for(nm in n) {
      if(nm=="heightMeasurement") aggObs$heightMeasurement = .readVegXMeasurement.2.0.X(x[[nm]])
      else if(nm=="aggregateOrganismMeasurement") {
        if(!("aggregateOrganismMeasurements" %in% names(aggObs))) aggObs$aggregateOrganismMeasurements = list()
        mesid = as.character(length(aggObs$aggregateOrganismMeasurements)+1)
        aggObs$aggregateOrganismMeasurements[[mesid]] = .readVegXMeasurement.2.0.X(x[[nm]])
      }
    }

    return(aggObs)
  }
  if("aggregateOrganismObservations" %in% vegnames) {
    target@aggregateObservations = xmlApply(veg[["aggregateOrganismObservations"]], .readAggregateOrganismObservation.2.0.X)
    names(target@aggregateObservations) = xmlApply(veg[["aggregateOrganismObservations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@aggregateObservations), " aggregate organism observation(s) read.\n"))
  }

  #read stratum observations
  .readStratumObservation.2.0.X = function(x) {
    strObs = list(stratumID = xmlValue(x[["stratumID"]]),
                  plotObservationID = xmlValue(x[["plotObservationID"]]))
    n = names(x)
    for(nm in n) {
      if(nm=="lowerLimitMeasurement") strObs$lowerLimitMeasurement = .readVegXMeasurement.2.0.X(x[[nm]])
      else if(nm=="upperLimitMeasurement") strObs$upperLimitMeasurement = .readVegXMeasurement.2.0.X(x[[nm]])
      else if(nm=="stratumMeasurement") {
        if(!("stratumMeasurements" %in% names(strObs))) strObs$stratumMeasurements = list()
        mesid = as.character(length(strObs$stratumMeasurements)+1)
        strObs$stratumMeasurements[[mesid]] = .readVegXMeasurement.2.0.X(x[[nm]])
      }
    }
    return(strObs)
  }
  if("stratumObservations" %in% vegnames) {
    target@stratumObservations = xmlApply(veg[["stratumObservations"]], .readStratumObservation.2.0.X)
    names(target@stratumObservations) = xmlApply(veg[["stratumObservations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@stratumObservations), " stratum observation(s) read.\n"))
  }

  #read community observations
  .readCommunityObservation.2.0.X = function(x) {
    strObs = list(plotObservationID = xmlValue(x[["plotObservationID"]]))
    n = names(x)
    for(nm in n) {
      if(nm=="communityMeasurement") {
        if(!("communityMeasurements" %in% names(strObs))) strObs$communityMeasurements = list()
        mesid = as.character(length(strObs$communityMeasurements)+1)
        strObs$communityMeasurements[[mesid]] = .readVegXMeasurement.2.0.X(x[[nm]])
      }
    }
    return(strObs)
  }
  if("communityObservations" %in% vegnames) {
    target@communityObservations = xmlApply(veg[["communityObservations"]], .readCommunityObservation.2.0.X)
    names(target@communityObservations) = xmlApply(veg[["communityObservations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@communityObservations), " community observation(s) read.\n"))
  }

  #read site observations
  .readSiteObservation.2.0.X = function(x) {
    siteObs = list(plotObservationID = xmlValue(x[["plotObservationID"]]))
    n = names(x)
    for(nm in n) {
      if(nm=="soilMeasurement") {
        if(!("soilMeasurements" %in% names(siteObs))) siteObs$soilMeasurements = list()
        mesid = as.character(length(siteObs$soilMeasurements)+1)
        siteObs$soilMeasurements[[mesid]] = .readVegXMeasurement.2.0.X(x[[nm]])
      }
      else if(nm=="climateMeasurement") {
        if(!("climateMeasurements" %in% names(siteObs))) siteObs$climateMeasurements = list()
        mesid = as.character(length(siteObs$climateMeasurements)+1)
        siteObs$climateMeasurements[[mesid]] = .readVegXMeasurement.2.0.X(x[[nm]])
      }
      else if(nm=="waterBodyMeasurement") {
        if(!("waterBodyMeasurements" %in% names(siteObs))) siteObs$waterBodyMeasurements = list()
        mesid = as.character(length(siteObs$waterBodyMeasurements)+1)
        siteObs$waterBodyMeasurements[[mesid]] = .readVegXMeasurement.2.0.X(x[[nm]])
      }
    }
    return(siteObs)
  }
  if("siteObservations" %in% vegnames) {
    target@siteObservations = xmlApply(veg[["siteObservations"]], .readSiteObservation.2.0.X)
    names(target@siteObservations) = xmlApply(veg[["siteObservations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@siteObservations), " site observation(s) read.\n"))
  }

  #read surface cover observations
  .readSurfaceCoverObservation.2.0.X = function(x) {
    scObs = list(surfaceTypeID = xmlValue(x[["surfaceTypeID"]]),
                 plotObservationID = xmlValue(x[["plotObservationID"]]))
    n = names(x)
    for(nm in n) {
      if(nm=="coverMeasurement") scObs$coverMeasurement = .readVegXMeasurement.2.0.X(x[[nm]])
    }
    return(scObs)
  }
  if("surfaceCoverObservations" %in% vegnames) {
    target@surfaceCoverObservations = xmlApply(veg[["surfaceCoverObservations"]], .readSurfaceCoverObservation.2.0.X)
    names(target@surfaceCoverObservations) = xmlApply(veg[["surfaceCoverObservations"]], xmlAttrs)
    if(verbose) cat(paste0(" ", length(target@surfaceCoverObservations), " surface cover observation(s) read.\n"))
  }


  rm(veg)
  return(target)
}
