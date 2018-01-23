#' Coearces VegX elements into a data frame
#'
#' Coerces part of the information of a Veg-X object into a data frame
#'
#' @param x An object of class \code{\linkS4class{VegX}}
#' @param element The name of the main elements to be coerced: 'plot', 'plotObservation', 'taxonNameUsageConcept',
#' 'stratum', 'stratumObservation', 'aggregateOrganismObservation', 'individualOrganism', 'individualOrganismObservation',
#' 'method', 'attribute'.
#' @param includeIDs A boolean flag to indicate whether internal identifiers should be included in the output
#'
#' @return a data frame
#' @export
#'
#' @examples
#' data(mokihinui)
#'
#' # Create document 'x' with aggregate taxon observations
#' taxmapping = list(plotName = "Plot", obsStartDate = "obsDate", taxonAuthorName = "PreferredSpeciesName",
#'               stratumName = "Tier", value = "Category")
#' scale = defineCoverScale(name = "Recce cover scale", description = "Recce recording method by Allen",
#'                          citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                          breaks = c(0, 0.1, 1, 5, 25, 50, 75, 100),
#'                          midPoints = c(0.01, 0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                          values = c("P","1","2","3", "4", "5", "6"))
#' strataDef = defineStrataByHeight(name = "Recce strata",
#'                                 description = "Standard Recce stratum definition",
#'                                 citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                                 heightBreaks = c(0, 0.3,2.0,5, 12, 25,50, 100),
#'                                 stratumNames = paste0("Tier ",1:7))
#' x = addTaxonObservations(newVegX(), tcv, "Mokihinui",
#'                         mapping = taxmapping,
#'                         abundanceMethod = scale,
#'                         stratumDefinition = strataDef)
#'
#' # Summary
#' summary(x)
#'
#' # show plot information
#' showElementTable(x, "plot")
#'
#' # show plot observation information
#' showElementTable(x, "plotObservation")
#'
#' # show methods and attributes
#' showElementTable(x, "method")
#' showElementTable(x, "attribute")
#'
#' # show aggregate organism observations
#' showElementTable(x, "aggregateOrganismObservation")
showElementTable<-function(x, element = "plot", includeIDs = FALSE) {

  element = match.arg(element, c("plot", "plotObservation", "taxonNameUsageConcept",
                                 "stratum", "stratumObservation", "aggregateOrganismObservation",
                                 "individualOrganism", "individualOrganismObservation",
                                 "method", "attribute"))
  res = NULL
  if(element=="plot") {
    res = data.frame(plotName = rep(NA, length(x@plots)), row.names = names(x@plots))
    if(length(x@plots)>0) {
      for(i in 1:length(x@plots)){
        res[i,"plotName"] = x@plots[[i]]$plotName
        if("parentPlotID" %in% names(x@plots[[i]])) { #Add parent plot information (it is a subplot)
          if(includeIDs) res[i,"relatedPlotID"] = x@plots[[i]]$parentPlotID
          res[i,"relatedPlotName"] = x@plots[[x@plots[[i]]$parentPlotID]]$plotName
          res[i,"plotRelationship"] = "subplot"
        }
        # Add location information
        if("location" %in% names(x@plots[[i]])) {
          res[i,"DecimalLongitude"] = x@plots[[i]]$location$DecimalLongitude
          res[i,"DecimalLatitude"] = x@plots[[i]]$location$DecimalLatitude
          res[i,"GeodeticDatum"] = x@plots[[i]]$location$GeodeticDatum
        }
        if("topography" %in% names(x@plots[[i]])) {
          if("slope" %in% names(x@plots[[i]]$topography)) { #Add slope information
            res[i,"slope_value"] = x@plots[[i]]$topography$slope$value
            res[i,"slope_attributeID"] = x@plots[[i]]$topography$slope$attributeID
          }
          if("aspect" %in% names(x@plots[[i]]$topography)) { #Add aspect information
            res[i,"aspect_value"] = x@plots[[i]]$topography$aspect$value
            res[i,"aspect_attributeID"] = x@plots[[i]]$topography$aspect$attributeID
          }
        }
      }
    }
  }
  else if(element=="plotObservation") {
    if(includeIDs) {
      res = data.frame(plotID = rep(NA, length(x@plotObservations)),
                       plotName = rep(NA, length(x@plotObservations)),
                       obsStartDate = rep(NA, length(x@plotObservations)),
                       row.names = names(x@plotObservations))
    } else {
      res = data.frame(plotName = rep(NA, length(x@plotObservations)),
                       obsStartDate = rep(NA, length(x@plotObservations)),
                       row.names = names(x@plotObservations))
    }
    if(length(x@plotObservations)>0){
      for(i in 1:length(x@plotObservations)){
        if(includeIDs) {
          res[i, "plotID"] = x@plotObservations[[i]]$plotID
        }
        res[i,"plotName"] = x@plots[[x@plotObservations[[i]]$plotID]]
        res[i,"obsStartDate"] = as.character(x@plotObservations[[i]]$obsStartDate)
        if("obsEndDate" %in% names(x@plotObservations[[i]])) {
          res[i, "obsEndDate"] = as.character(x@plotObservations[[i]]$obsEndDate)
        }
        if("projectID" %in% names(x@plotObservations[[i]])) {
          if(includeIDs) {
            res[i, "projectID"] = x@plotObservations[[i]]$projectID
          }
          res[i, "projectTitle"] = x@projects[[x@plotObservations[[i]]$projectID]]$title
        }
      }
    }
  }
  else if(element=="taxonNameUsageConcept") {
    res = data.frame(authorName = rep(NA, length(x@taxonNameUsageConcepts)),
                     row.names = names(x@taxonNameUsageConcepts))
    if(length(x@taxonNameUsageConcepts)>0){
      for(i in 1:length(x@taxonNameUsageConcepts)){
        res[i, "authorName"] = x@taxonNameUsageConcepts[[i]]$authorName
      }
    }
  }
  else if(element=="aggregateOrganismObservation") {
    if(includeIDs) {
      res = data.frame(plotObservationID = rep(NA, length(x@aggregateObservations)),
                     plotName = rep(NA, length(x@aggregateObservations)),
                     obsStartDate = rep(NA, length(x@aggregateObservations)),
                     taxonNameUsageConceptID = rep(NA, length(x@aggregateObservations)),
                     authorName = rep(NA, length(x@aggregateObservations)),
                     row.names = names(x@aggregateObservations))
    } else {
      res = data.frame(plotName = rep(NA, length(x@aggregateObservations)),
                       obsStartDate = rep(NA, length(x@aggregateObservations)),
                       authorName = rep(NA, length(x@aggregateObservations)),
                       row.names = names(x@aggregateObservations))
    }
    if(length(x@aggregateObservations)>0){
      for(i in 1:length(x@aggregateObservations)){
        if(includeIDs) {
          res[i, "plotObservationID"] = x@aggregateObservations[[i]]$plotObservationID
        }
        res[i, "plotName"] = x@plots[[x@plotObservations[[x@aggregateObservations[[i]]$plotObservationID]]$plotID]]$plotName
        res[i, "obsStartDate"] = as.character(x@plotObservations[[x@aggregateObservations[[i]]$plotObservationID]]$obsStartDate)
        if(includeIDs) {
          res[i, "taxonNameUsageConceptID"] = x@aggregateObservations[[i]]$taxonNameUsageConceptID
        }
        res[i, "authorName"] = x@taxonNameUsageConcepts[[x@aggregateObservations[[i]]$taxonNameUsageConceptID]]$authorName
        if("stratumObservationID" %in% names(x@aggregateObservations[[i]])){
          if(x@aggregateObservations[[i]]$stratumObservationID != "") {
            if(includeIDs) {
              res[i, "stratumObservationID"] = x@aggregateObservations[[i]]$stratumObservationID
              res[i, "stratumID"] = x@stratumObservations[[x@aggregateObservations[[i]]$stratumObservationID]]$stratumID
            }
            res[i, "stratumName"] = x@strata[[x@stratumObservations[[x@aggregateObservations[[i]]$stratumObservationID]]$stratumID]]$stratumName
          }
        }
        res[i, "aggregateValue_value"] = x@aggregateObservations[[i]]$value
        res[i, "aggregateValue_method"] = x@methods[[x@attributes[[x@aggregateObservations[[i]]$attributeID]]$methodID]]$name
      }
    }
  }
  else if(element=="stratum") {
    res = data.frame(stratumName = rep(NA, length(x@strata)),
                     row.names = names(x@strata))
    if(length(x@strata)>0){
      for(i in 1:length(x@strata)){
        res[i, "stratumName"] = x@strata[[i]]$stratumName
        if("methodID" %in% names(x@strata[[i]])) {
          if(includeIDs) {
            res[i, "methodID"] = x@strata[[i]]$methodID
          }
          res[i, "methodName"] = x@methods[[x@strata[[i]]$methodID]]$name
        }
        if("stratumSequence" %in% names(x@strata[[i]])) res[i, "stratumSequence"] = x@strata[[i]]$stratumSequence
        if("lowerBound" %in% names(x@strata[[i]])) res[i, "lowerBound"] = x@strata[[i]]$lowerBound
        if("upperBound" %in% names(x@strata[[i]])) res[i, "upperBound"] = x@strata[[i]]$upperBound
      }
    }
  }
  else if(element=="stratumObservation") {
    if(includeIDs) {
      res = data.frame(plotObservationID = rep(NA, length(x@stratumObservations)),
                       plotName = rep(NA, length(x@stratumObservations)),
                       obsStartDate = rep(NA, length(x@stratumObservations)),
                       stratumID = rep(NA, length(x@stratumObservations)),
                       stratumName = rep(NA, length(x@stratumObservations)),
                       row.names = names(x@stratumObservations))
    } else {
      res = data.frame(plotName = rep(NA, length(x@stratumObservations)),
                       obsStartDate = rep(NA, length(x@stratumObservations)),
                       stratumName = rep(NA, length(x@stratumObservations)),
                       row.names = names(x@stratumObservations))
    }
    if(length(x@stratumObservations)>0){
      for(i in 1:length(x@stratumObservations)){
        if(includeIDs) {
          res[i, "plotObservationID"] = x@stratumObservations[[i]]$plotObservationID
        }
        res[i, "plotName"] = x@plots[[x@plotObservations[[x@stratumObservations[[i]]$plotObservationID]]$plotID]]
        res[i, "obsStartDate"] = as.character(x@plotObservations[[x@stratumObservations[[i]]$plotObservationID]]$obsStartDate)
        if(includeIDs) {
          res[i, "stratumID"] = x@stratumObservations[[i]]$stratumID
        }
        res[i, "stratumName"] = x@strata[[x@stratumObservations[[i]]$stratumID]]$stratumName
        if("value" %in% names(x@stratumObservations[[i]])) res[i, "stratumValue_value"] = x@stratumObservations[[i]]$value
        if("attributeID" %in% names(x@stratumObservations[[i]])) res[i, "stratumValue_method"] = x@methods[[x@attributes[[x@stratumObservations[[i]]$attributeID]]$methodID]]$name
      }
    }
  }
  else if(element=="individualOrganism") {
    if(includeIDs) {
      res = data.frame(plotID = rep(NA, length(x@individualOrganisms)),
                       plotName = rep(NA, length(x@individualOrganisms)),
                       identificationLabel = rep(NA, length(x@individualOrganisms)),
                       taxonNameUsageConceptID = rep(NA, length(x@individualOrganisms)),
                       authorName = rep(NA, length(x@individualOrganisms)),
                       row.names = names(x@individualOrganisms))
    } else {
      res = data.frame(plotName = rep(NA, length(x@individualOrganisms)),
                       identificationLabel = rep(NA, length(x@individualOrganisms)),
                       authorName = rep(NA, length(x@individualOrganisms)),
                       row.names = names(x@individualOrganisms))
    }
    if(length(x@individualOrganisms)>0){
      for(i in 1:length(x@individualOrganisms)){
        if(includeIDs) {
          res[i, "plotID"] = x@individualOrganisms[[i]]$plotID
        }
        res[i, "plotName"] = x@plots[[x@individualOrganisms[[i]]$plotID]]$plotName
        if(includeIDs) {
          res[i, "taxonNameUsageConceptID"] = x@individualOrganisms[[i]]$taxonNameUsageConceptID
        }
        res[i, "authorName"] = x@taxonNameUsageConcepts[[x@individualOrganisms[[i]]$taxonNameUsageConceptID]]$authorName
        res[i, "identificationLabel"] = x@individualOrganisms[[i]]$identificationLabel
      }
    }
  }
  else if(element=="individualOrganismObservation") {
    if(includeIDs) {
      res = data.frame(plotObservationID = rep(NA, length(x@individualObservations)),
                       plotName = rep(NA, length(x@individualObservations)),
                       obsStartDate = rep(NA, length(x@individualObservations)),
                       individualOrganismID = rep(NA, length(x@individualObservations)),
                       identificationLabel = rep(NA, length(x@individualObservations)),
                       authorName = rep(NA, length(x@individualObservations)),
                       row.names = names(x@individualObservations))
    } else {
      res = data.frame(plotName = rep(NA, length(x@individualObservations)),
                       obsStartDate = rep(NA, length(x@individualObservations)),
                       identificationLabel = rep(NA, length(x@individualObservations)),
                       authorName = rep(NA, length(x@individualObservations)),
                       row.names = names(x@individualObservations))
    }
    if(length(x@individualObservations)>0){
      for(i in 1:length(x@individualObservations)){
        if(includeIDs) {
          res[i, "plotObservationID"] = x@individualObservations[[i]]$plotObservationID
        }
        res[i, "plotName"] = x@plots[[x@plotObservations[[x@individualObservations[[i]]$plotObservationID]]$plotID]]$plotName
        res[i, "obsStartDate"] = as.character(x@plotObservations[[x@individualObservations[[i]]$plotObservationID]]$obsStartDate)
        if(includeIDs) {
          res[i, "individualOrganismID"] = x@individualObservations[[i]]$individualOrganismID
        }
        res[i, "identificationLabel"] = x@individualOrganisms[[x@individualObservations[[i]]$individualOrganismID]]$identificationLabel
        res[i, "authorName"] = x@taxonNameUsageConcepts[[x@individualOrganisms[[x@individualObservations[[i]]$individualOrganismID]]$taxonNameUsageConceptID]]$authorName
        if("stratumObservationID" %in% names(x@individualObservations[[i]])){
          if(x@individualObservations[[i]]$stratumObservationID != "") {
            if(includeIDs) {
              res[i, "stratumObservationID"] = x@individualObservations[[i]]$stratumObservationID
              res[i, "stratumID"] = x@stratumObservations[[x@individualObservations[[i]]$stratumObservationID]]$stratumID
            }
            res[i, "stratumName"] = x@strata[[x@stratumObservations[[x@individualObservations[[i]]$stratumObservationID]]$stratumID]]$stratumName
          }
        }
        res[i, "diameter_value"] = x@individualObservations[[i]]$diameterValue
        res[i, "diameter_method"] = x@methods[[x@attributes[[x@individualObservations[[i]]$diameterAttributeID]]$methodID]]$name
      }
    }
  }
  else if(element=="method") {
    res = data.frame(name = rep(NA, length(x@methods)),
                     description = rep(NA, length(x@methods)),
                     subject = rep(NA, length(x@methods)),
                     attributeType = rep(NA, length(x@methods)),
                     attributeNumber = rep(NA, length(x@methods)),
                     row.names = names(x@methods))
    if(length(x@methods)>0){
      for(i in 1:length(x@methods)){
        res[i, "name"] = x@methods[[i]]$name
        res[i, "description"] = x@methods[[i]]$description
        res[i, "subject"] = x@methods[[i]]$subject
        res[i, "attributeType"] = x@methods[[i]]$attributeType
        res[i, "attributeNumber"] = length(.getAttributeIDsByMethodID(x, names(x@methods)[i]))
      }
    }
  }
  else if(element=="attribute") {
    resQuantitative = data.frame()
    resOrdinal = data.frame()
    resQualitative = data.frame()
    if(length(x@attributes)>0){
      cntQuant = 0
      cntOrd = 0
      cntQual = 0
      for(i in 1:length(x@attributes)){
        if(x@attributes[[i]]$type=="quantitative") {
          cntQuant = cntQuant + 1
          if(includeIDs) resQuantitative[cntQuant, "methodID"] = x@attributes[[i]]$methodID
          resQuantitative[cntQuant, "methodName"] = x@methods[[x@attributes[[i]]$methodID]]$name
          resQuantitative[cntQuant, "methodSubject"] = x@methods[[x@attributes[[i]]$methodID]]$subject
          resQuantitative[cntQuant, "unit"] = x@attributes[[i]]$unit
          if("lowerBound" %in% names(x@attributes[[i]])) resQuantitative[cntQuant, "lowerBound"] = x@attributes[[i]]$lowerBound
          if("upperBound" %in% names(x@attributes[[i]])) resQuantitative[cntQuant, "upperBound"] = x@attributes[[i]]$upperBound
          rownames(resQuantitative)[cntQuant] = names(x@attributes)[i]
        }
        else if(x@attributes[[i]]$type=="ordinal") {
          cntOrd = cntOrd + 1
          if(includeIDs) resOrdinal[cntOrd, "methodID"] = x@attributes[[i]]$methodID
          resOrdinal[cntOrd, "methodName"] = x@methods[[x@attributes[[i]]$methodID]]$name
          resOrdinal[cntOrd, "methodSubject"] = x@methods[[x@attributes[[i]]$methodID]]$subject
          resOrdinal[cntOrd, "code"] = x@attributes[[i]]$code
          if("definition" %in% names(x@attributes[[i]])) resOrdinal[cntOrd, "definition"] = x@attributes[[i]]$definition
          if("order" %in% names(x@attributes[[i]])) resOrdinal[cntOrd, "order"] = x@attributes[[i]]$order
          if("lowerLimit" %in% names(x@attributes[[i]])) resOrdinal[cntOrd, "lowerLimit"] = x@attributes[[i]]$lowerLimit
          if("upperLimit" %in% names(x@attributes[[i]])) resOrdinal[cntOrd, "upperLimit"] = x@attributes[[i]]$upperLimit
          if("midPoint" %in% names(x@attributes[[i]])) resOrdinal[cntOrd, "midPoint"] = x@attributes[[i]]$midPoint
          rownames(resOrdinal)[cntOrd] = names(x@attributes)[i]
        }
        else if(x@attributes[[i]]$type=="qualitative") {
          cntQual = cntQual + 1
          if(includeIDs) resQualitative[cntQual, "methodID"] = x@attributes[[i]]$methodID
          resQualitative[cntQual, "methodName"] = x@methods[[x@attributes[[i]]$methodID]]$name
          resQualitative[cntQual, "methodSubject"] = x@methods[[x@attributes[[i]]$methodID]]$subject
          resQualitative[cntQual, "code"] = x@attributes[[i]]$code
          if("definition" %in% names(x@attributes[[i]])) resQualitative[cntQual, "definition"] = x@attributes[[i]]$definition
          rownames(resQualitative)[cntQual] = names(x@attributes)[i]
        }
      }
    }
    res = list(quantitative = resQuantitative, ordinal = resOrdinal, qualitative = resQualitative)
  }
  return(res)
}
