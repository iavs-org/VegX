#' Coearces VegX elements into a data frame
#'
#' Coerces part of the information of a Veg-X object into a data frame
#'
#' @param x An object of class \code{\linkS4class{VegX}}
#' @param element The name of the main elements to be coerced: 'plot', 'plotObservation', 'aggregateOrganismObservation',
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
#' showVegXElement(x, "plot")
#'
#' # show plot observation information
#' showVegXElement(x, "plotObservation")
#'
#' # show methods and attributes
#' showVegXElement(x, "method")
#' showVegXElement(x, "attribute")
#'
showVegXElement<-function(x, element = "plot", includeIDs = FALSE) {

  element = match.arg(element, c("plot", "plotObservation", "aggregateOrganismObservation", "method", "attribute"))
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
        res[i, "plotName"] = x@plots[[x@plotObservations[[x@aggregateObservations[[i]]$plotObservationID]]$plotID]]
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
        res[i, "subject"] = x@methods[[i]]$attributeClass
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
          resQuantitative[cntQuant, "methodSubject"] = x@methods[[x@attributes[[i]]$methodID]]$attributeClass
          resQuantitative[cntQuant, "unit"] = x@attributes[[i]]$unit
          if("lowerBound" %in% names(x@attributes[[i]])) resQuantitative[cntQuant, "lowerBound"] = x@attributes[[i]]$lowerBound
          if("upperBound" %in% names(x@attributes[[i]])) resQuantitative[cntQuant, "upperBound"] = x@attributes[[i]]$upperBound
          rownames(resQuantitative)[cntQuant] = names(x@attributes)[i]
        }
        else if(x@attributes[[i]]$type=="ordinal") {
          cntOrd = cntOrd + 1
          if(includeIDs) resOrdinal[cntOrd, "methodID"] = x@attributes[[i]]$methodID
          resOrdinal[cntOrd, "methodName"] = x@methods[[x@attributes[[i]]$methodID]]$name
          resOrdinal[cntOrd, "methodSubject"] = x@methods[[x@attributes[[i]]$methodID]]$attributeClass
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
          resQualitative[cntQual, "methodSubject"] = x@methods[[x@attributes[[i]]$methodID]]$attributeClass
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
