#' Coerces VegX elements into a data frame
#'
#' Coerces part of the information of a Veg-X object into a data frame
#'
#' @param x An object of class \code{\linkS4class{VegX}}
#' @param element The name of the main elements to be coerced: 'plot', 'plotObservation', 'taxonNameUsageConcept',
#' 'stratum', 'stratumObservation', 'surfaceType', 'surfaceCoverObservation',
#' 'aggregateOrganismObservation', 'individualOrganism', 'individualOrganismObservation',
#' 'siteObservation', 'method', 'attribute'.
#' @param IDs A boolean flag to indicate whether internal identifiers should be included in the output.
#' @param subjects A boolean flag to indicate whether method subjects should be included in the output.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' data(mokihinui)
#'
#' # Create document 'x' with aggregate organism observations
#' mapping = list(plotName = "Plot", obsStartDate = "obsDate", authorTaxonName = "PreferredSpeciesName",
#'               stratumName = "Tier", cover = "Category")
#' coverscale = defineOrdinalScaleMethod(name = "Recce cover scale",
#'                    description = "Recce recording method by Hurst/Allen",
#'                    subject = "plant cover",
#'                    citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                    codes = c("P","1","2","3", "4", "5", "6"),
#'                    quantifiableCodes = c("1","2","3", "4", "5", "6"),
#'                    breaks = c(0, 1, 5, 25, 50, 75, 100),
#'                    midPoints = c(0.05, 0.5, 15, 37.5, 62.5, 87.5),
#'                    definitions = c("Presence", "<1%", "1-5%","6-25%", "26-50%", "51-75%", "76-100%"))
#' strataDef = defineMixedStrata(name = "Recce strata",
#'                               description = "Standard Recce stratum definition",
#'                               citation = "Hurst, JM and Allen, RB. (2007) The Recce method for describing New Zealand vegetation – Field protocols. Landcare Research, Lincoln.",
#'                               heightStrataBreaks = c(0, 0.3,2.0,5, 12, 25, 50),
#'                               heightStrataNames = paste0("Tier ",1:6),
#'                               categoryStrataNames = "Tier 7",
#'                               categoryStrataDefinition = "Epiphytes")
#' x = addAggregateOrganismObservations(newVegX(), moki_tcv,
#'                         mapping = mapping,
#'                         methods = c(cover=coverscale),
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
showElementTable<-function(x, element = "plot", IDs = FALSE, subjects = FALSE) {

  element = match.arg(element, c("plot", "plotObservation", "taxonNameUsageConcept",
                                 "stratum", "stratumObservation",
                                 "surfaceType", "surfaceCoverObservation",
                                 "aggregateOrganismObservation",
                                 "individualOrganism", "individualOrganismObservation", "siteObservation",
                                 "method", "attribute"))
  res = NULL
  if(element=="plot") {
    res = data.frame(plotName = rep(NA, length(x@plots)), row.names = names(x@plots))
    if(length(x@plots)>0) {
      for(i in 1:length(x@plots)){
        res[i,"plotName"] = x@plots[[i]]$plotName
        if("parentPlotID" %in% names(x@plots[[i]])) { #Add parent plot information (it is a subplot)
          if(IDs) res[i,"relatedPlotID"] = x@plots[[i]]$parentPlotID
          res[i,"relatedPlotName"] = x@plots[[x@plots[[i]]$parentPlotID]]$plotName
          res[i,"plotRelationship"] = "subplot"
        }
        if("plotUniqueIdentifier"%in% names(x@plots[[i]])) {
          res[i,"plotUniqueIdentifier"] = x@plots[[i]]$plotUniqueIdentifier
        }
        # Add location information
        if("location" %in% names(x@plots[[i]])) {
          res[i,"DecimalLongitude"] = x@plots[[i]]$location$DecimalLongitude
          res[i,"DecimalLatitude"] = x@plots[[i]]$location$DecimalLatitude
          res[i,"GeodeticDatum"] = x@plots[[i]]$location$GeodeticDatum
        }
        if("topography" %in% names(x@plots[[i]])) {
          if("slope" %in% names(x@plots[[i]]$topography)) { #Add slope information
            res[i,"slope_method"] = x@methods[[x@attributes[[x@plots[[i]]$topography$slope$attributeID]]$methodID]]$name
            res[i,"slope_value"] = x@plots[[i]]$topography$slope$value
            if(IDs) res[i,"slope_attributeID"] = x@plots[[i]]$topography$slope$attributeID
          }
          if("aspect" %in% names(x@plots[[i]]$topography)) { #Add aspect information
            res[i,"aspect_method"] = x@methods[[x@attributes[[x@plots[[i]]$topography$aspect$attributeID]]$methodID]]$name
            res[i,"aspect_value"] = x@plots[[i]]$topography$aspect$value
            if(IDs) res[i,"aspect_attributeID"] = x@plots[[i]]$topography$aspect$attributeID
          }
        }
      }
    }
  }
  else if(element=="plotObservation") {
    if(IDs) {
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
        if(IDs) {
          res[i, "plotID"] = x@plotObservations[[i]]$plotID
        }
        res[i,"plotName"] = x@plots[[x@plotObservations[[i]]$plotID]]$plotName
        res[i,"obsStartDate"] = as.character(x@plotObservations[[i]]$obsStartDate)
        if("obsEndDate" %in% names(x@plotObservations[[i]])) {
          res[i, "obsEndDate"] = as.character(x@plotObservations[[i]]$obsEndDate)
        }
        if("plotObservationUniqueIdentifier"%in% names(x@plotObservations[[i]])) {
          res[i,"plotObservationUniqueIdentifier"] = x@plotObservations[[i]]$plotObservationUniqueIdentifier
        }
        if("projectID" %in% names(x@plotObservations[[i]])) {
          if(IDs) {
            res[i, "projectID"] = x@plotObservations[[i]]$projectID
          }
          res[i, "projectTitle"] = x@projects[[x@plotObservations[[i]]$projectID]]$title
        }
        if("siteObservationID" %in% names(x@plotObservations[[i]])) {
          if(IDs) {
            res[i, "siteObservationID"] = x@plotObservations[[i]]$siteObservationID
          }
        }
        if("communityObservationID" %in% names(x@plotObservations[[i]])) {
          if(IDs) {
            res[i, "communityObservationID"] = x@plotObservations[[i]]$communityObservationID
          }
        }
      }
    }
  }
  else if(element=="taxonNameUsageConcept") {
    res = data.frame(authorTaxonName = rep(NA, length(x@taxonNameUsageConcepts)),
                     row.names = names(x@taxonNameUsageConcepts))
    if(length(x@taxonNameUsageConcepts)>0){
      for(i in 1:length(x@taxonNameUsageConcepts)){
        res[i, "authorTaxonName"] = x@taxonNameUsageConcepts[[i]]$authorTaxonName
      }
    }
  }
  else if(element=="aggregateOrganismObservation") {
    if(IDs) {
      res = data.frame(plotObservationID = rep(NA, length(x@aggregateObservations)),
                     plotName = rep(NA, length(x@aggregateObservations)),
                     obsStartDate = rep(NA, length(x@aggregateObservations)),
                     taxonNameUsageConceptID = rep(NA, length(x@aggregateObservations)),
                     authorTaxonName = rep(NA, length(x@aggregateObservations)),
                     row.names = names(x@aggregateObservations))
    } else {
      res = data.frame(plotName = rep(NA, length(x@aggregateObservations)),
                       obsStartDate = rep(NA, length(x@aggregateObservations)),
                       authorTaxonName = rep(NA, length(x@aggregateObservations)),
                       row.names = names(x@aggregateObservations))
    }
    if(length(x@aggregateObservations)>0){
      for(i in 1:length(x@aggregateObservations)){
        if(IDs) {
          res[i, "plotObservationID"] = x@aggregateObservations[[i]]$plotObservationID
        }
        res[i, "plotName"] = x@plots[[x@plotObservations[[x@aggregateObservations[[i]]$plotObservationID]]$plotID]]$plotName
        res[i, "obsStartDate"] = as.character(x@plotObservations[[x@aggregateObservations[[i]]$plotObservationID]]$obsStartDate)
        if(IDs) {
          res[i, "taxonNameUsageConceptID"] = x@aggregateObservations[[i]]$taxonNameUsageConceptID
        }
        res[i, "authorTaxonName"] = x@taxonNameUsageConcepts[[x@aggregateObservations[[i]]$taxonNameUsageConceptID]]$authorTaxonName
        if("stratumObservationID" %in% names(x@aggregateObservations[[i]])){
          if(x@aggregateObservations[[i]]$stratumObservationID != "") {
            if(IDs) {
              res[i, "stratumObservationID"] = x@aggregateObservations[[i]]$stratumObservationID
              res[i, "stratumID"] = x@stratumObservations[[x@aggregateObservations[[i]]$stratumObservationID]]$stratumID
            }
            res[i, "stratumName"] = x@strata[[x@stratumObservations[[x@aggregateObservations[[i]]$stratumObservationID]]$stratumID]]$stratumName
          }
        }
        if("heightMeasurement" %in% names(x@aggregateObservations[[i]])) {
          if(IDs) {
            res[i, "height_attID"] = x@aggregateObservations[[i]]$heightMeasurement$attributeID
          }
          res[i, "height_method"] = x@methods[[x@attributes[[x@aggregateObservations[[i]]$heightMeasurement$attributeID]]$methodID]]$name
          res[i, "height_value"] = x@aggregateObservations[[i]]$heightMeasurement$value
        }
        if("aggregateOrganismMeasurements" %in% names(x@aggregateObservations[[i]])) {
          measurements = x@aggregateObservations[[i]]$aggregateOrganismMeasurements
          if(length(measurements)>0) {
            for(j in 1:length(measurements)) {
              attID = measurements[[j]]$attributeID
              strAttID = paste0("agg_", names(measurements)[j],"_attID")
              if(IDs) {
                res[i, strAttID] = measurements[[j]]$attributeID
              }
              aggMethod = paste0("agg_", names(measurements)[j],"_method")
              res[i, aggMethod] = x@methods[[x@attributes[[attID]]$methodID]]$name
              if(subjects) {
                aggSubject = paste0("agg_", names(measurements)[j],"_subject")
                res[i, aggSubject] = x@methods[[x@attributes[[attID]]$methodID]]$subject
              }
              aggVal = paste0("agg_", names(measurements)[j],"_value")
              res[i, aggVal] = measurements[[j]]$value
            }
          }
        }
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
          if(IDs) {
            res[i, "methodID"] = x@strata[[i]]$methodID
          }
          res[i, "methodName"] = x@methods[[x@strata[[i]]$methodID]]$name
        }
        if("order" %in% names(x@strata[[i]])) res[i, "order"] = x@strata[[i]]$order
        if("lowerLimit" %in% names(x@strata[[i]])) res[i, "lowerLimit"] = x@strata[[i]]$lowerLimit
        if("upperLimit" %in% names(x@strata[[i]])) res[i, "upperLimit"] = x@strata[[i]]$upperLimit
      }
    }
  }
  else if(element=="stratumObservation") {
    if(IDs) {
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
        if(IDs) {
          res[i, "plotObservationID"] = x@stratumObservations[[i]]$plotObservationID
        }
        res[i, "plotName"] = x@plots[[x@plotObservations[[x@stratumObservations[[i]]$plotObservationID]]$plotID]]$plotName
        res[i, "obsStartDate"] = as.character(x@plotObservations[[x@stratumObservations[[i]]$plotObservationID]]$obsStartDate)
        if(IDs) {
          res[i, "stratumID"] = x@stratumObservations[[i]]$stratumID
        }
        res[i, "stratumName"] = x@strata[[x@stratumObservations[[i]]$stratumID]]$stratumName
        if("lowerLimitMeasurement" %in% names(x@stratumObservations[[i]])) {
          if(IDs) {
            res[i, "lowerLimit_attID"] = x@stratumObservations[[i]]$lowerLimitMeasurement$attributeID
          }
          res[i, "lowerLimit_method"] = x@methods[[x@attributes[[x@stratumObservations[[i]]$lowerLimitMeasurement$attributeID]]$methodID]]$name
          res[i, "lowerLimit_value"] = x@stratumObservations[[i]]$lowerLimitMeasurement$value
        }
        if("upperLimitMeasurement" %in% names(x@stratumObservations[[i]])) {
          if(IDs) {
            res[i, "upperLimit_attID"] = x@stratumObservations[[i]]$upperLimitMeasurement$attributeID
          }
          res[i, "upperLimit_method"] = x@methods[[x@attributes[[x@stratumObservations[[i]]$upperLimitMeasurement$attributeID]]$methodID]]$name
          res[i, "upperLimit_value"] = x@stratumObservations[[i]]$upperLimitMeasurement$value
        }
        if("stratumMeasurements" %in% names(x@stratumObservations[[i]])) {
          measurements = x@stratumObservations[[i]]$stratumMeasurements
          if(length(measurements)>0) {
            for(j in 1:length(measurements)) {
              attID = measurements[[j]]$attributeID
              strAttID = paste0("str_", names(measurements)[j],"_attID")
              if(IDs) {
                res[i, strAttID] = measurements[[j]]$attributeID
              }
              strMethod = paste0("str_", names(measurements)[j],"_method")
              res[i, strMethod] = x@methods[[x@attributes[[attID]]$methodID]]$name
              if(subjects) {
                strSubject = paste0("str_", names(measurements)[j],"_subject")
                res[i, strSubject] = x@methods[[x@attributes[[attID]]$methodID]]$subject
              }
              strVal = paste0("str_", names(measurements)[j],"_value")
              res[i, strVal] = measurements[[j]]$value
            }
          }
        }
      }
    }
  }
  else if(element=="surfaceType") {
    res = data.frame(surfaceName = rep(NA, length(x@surfaceTypes)),
                     row.names = names(x@surfaceTypes))
    if(length(x@surfaceTypes)>0){
      for(i in 1:length(x@surfaceTypes)){
        res[i, "surfaceName"] = x@surfaceTypes[[i]]$surfaceName
        if("methodID" %in% names(x@surfaceTypes[[i]])) {
          if(IDs) {
            res[i, "methodID"] = x@surfaceTypes[[i]]$methodID
          }
          res[i, "methodName"] = x@methods[[x@surfaceTypes[[i]]$methodID]]$name
        }
      }
    }
  }
  else if(element=="surfaceCoverObservation") {
    if(IDs) {
      res = data.frame(plotObservationID = rep(NA, length(x@surfaceCoverObservations)),
                       plotName = rep(NA, length(x@surfaceCoverObservations)),
                       obsStartDate = rep(NA, length(x@surfaceCoverObservations)),
                       surfaceTypeID = rep(NA, length(x@surfaceCoverObservations)),
                       surfaceName = rep(NA, length(x@surfaceCoverObservations)),
                       row.names = names(x@surfaceCoverObservations))
    } else {
      res = data.frame(plotName = rep(NA, length(x@surfaceCoverObservations)),
                       obsStartDate = rep(NA, length(x@surfaceCoverObservations)),
                       surfaceName = rep(NA, length(x@surfaceCoverObservations)),
                       row.names = names(x@surfaceCoverObservations))
    }
    if(length(x@surfaceCoverObservations)>0){
      for(i in 1:length(x@surfaceCoverObservations)){
        if(IDs) {
          res[i, "plotObservationID"] = x@surfaceCoverObservations[[i]]$plotObservationID
        }
        res[i, "plotName"] = x@plots[[x@plotObservations[[x@surfaceCoverObservations[[i]]$plotObservationID]]$plotID]]$plotName
        res[i, "obsStartDate"] = as.character(x@plotObservations[[x@surfaceCoverObservations[[i]]$plotObservationID]]$obsStartDate)
        if(IDs) {
          res[i, "surfaceTypeID"] = x@surfaceCoverObservations[[i]]$surfaceTypeID
        }
        res[i, "surfaceName"] = x@surfaceTypes[[x@surfaceCoverObservations[[i]]$surfaceTypeID]]$surfaceName
        if("coverMeasurement" %in% names(x@surfaceCoverObservations[[i]])) {
          if(IDs) {
            res[i, "cover_attID"] = x@surfaceCoverObservations[[i]]$coverMeasurement$attributeID
          }
          res[i, "cover_method"] = x@methods[[x@attributes[[x@surfaceCoverObservations[[i]]$coverMeasurement$attributeID]]$methodID]]$name
          res[i, "cover_value"] = x@surfaceCoverObservations[[i]]$coverMeasurement$value
        }
      }
    }
  }
  else if(element=="individualOrganism") {
    if(IDs) {
      res = data.frame(plotID = rep(NA, length(x@individualOrganisms)),
                       plotName = rep(NA, length(x@individualOrganisms)),
                       identificationLabel = rep(NA, length(x@individualOrganisms)),
                       taxonNameUsageConceptID = rep(NA, length(x@individualOrganisms)),
                       authorTaxonName = rep(NA, length(x@individualOrganisms)),
                       row.names = names(x@individualOrganisms))
    } else {
      res = data.frame(plotName = rep(NA, length(x@individualOrganisms)),
                       identificationLabel = rep(NA, length(x@individualOrganisms)),
                       authorTaxonName = rep(NA, length(x@individualOrganisms)),
                       row.names = names(x@individualOrganisms))
    }
    if(length(x@individualOrganisms)>0){
      for(i in 1:length(x@individualOrganisms)){
        if(IDs) {
          res[i, "plotID"] = x@individualOrganisms[[i]]$plotID
        }
        res[i, "plotName"] = x@plots[[x@individualOrganisms[[i]]$plotID]]$plotName
        if(IDs) {
          res[i, "taxonNameUsageConceptID"] = x@individualOrganisms[[i]]$taxonNameUsageConceptID
        }
        res[i, "authorTaxonName"] = x@taxonNameUsageConcepts[[x@individualOrganisms[[i]]$taxonNameUsageConceptID]]$authorTaxonName
        res[i, "identificationLabel"] = x@individualOrganisms[[i]]$identificationLabel
      }
    }
  }
  else if(element=="individualOrganismObservation") {
    if(IDs) {
      res = data.frame(plotObservationID = rep(NA, length(x@individualObservations)),
                       plotName = rep(NA, length(x@individualObservations)),
                       obsStartDate = rep(NA, length(x@individualObservations)),
                       individualOrganismID = rep(NA, length(x@individualObservations)),
                       identificationLabel = rep(NA, length(x@individualObservations)),
                       authorTaxonName = rep(NA, length(x@individualObservations)),
                       row.names = names(x@individualObservations))
    } else {
      res = data.frame(plotName = rep(NA, length(x@individualObservations)),
                       obsStartDate = rep(NA, length(x@individualObservations)),
                       identificationLabel = rep(NA, length(x@individualObservations)),
                       authorTaxonName = rep(NA, length(x@individualObservations)),
                       row.names = names(x@individualObservations))
    }
    if(length(x@individualObservations)>0){
      for(i in 1:length(x@individualObservations)){
        if(IDs) {
          res[i, "plotObservationID"] = x@individualObservations[[i]]$plotObservationID
        }
        res[i, "plotName"] = x@plots[[x@plotObservations[[x@individualObservations[[i]]$plotObservationID]]$plotID]]$plotName
        res[i, "obsStartDate"] = as.character(x@plotObservations[[x@individualObservations[[i]]$plotObservationID]]$obsStartDate)
        if(IDs) {
          res[i, "individualOrganismID"] = x@individualObservations[[i]]$individualOrganismID
        }
        res[i, "identificationLabel"] = x@individualOrganisms[[x@individualObservations[[i]]$individualOrganismID]]$identificationLabel
        res[i, "authorTaxonName"] = x@taxonNameUsageConcepts[[x@individualOrganisms[[x@individualObservations[[i]]$individualOrganismID]]$taxonNameUsageConceptID]]$authorTaxonName
        if("stratumObservationID" %in% names(x@individualObservations[[i]])){
          if(x@individualObservations[[i]]$stratumObservationID != "") {
            if(IDs) {
              res[i, "stratumObservationID"] = x@individualObservations[[i]]$stratumObservationID
              res[i, "stratumID"] = x@stratumObservations[[x@individualObservations[[i]]$stratumObservationID]]$stratumID
            }
            res[i, "stratumName"] = x@strata[[x@stratumObservations[[x@individualObservations[[i]]$stratumObservationID]]$stratumID]]$stratumName
          }
        }
        if("diameterMeasurement" %in% names(x@individualObservations[[i]])) {
          if(IDs) {
            res[i, "diameter_attID"] = x@individualObservations[[i]]$diameterMeasurement$attributeID
          }
          res[i, "diameter_method"] = x@methods[[x@attributes[[x@individualObservations[[i]]$diameterMeasurement$attributeID]]$methodID]]$name
          res[i, "diameter_value"] = x@individualObservations[[i]]$diameterMeasurement$value
        }
        if("heightMeasurement" %in% names(x@individualObservations[[i]])) {
          if(IDs) {
            res[i, "height_attID"] = x@individualObservations[[i]]$heightMeasurement$attributeID
          }
          res[i, "height_method"] = x@methods[[x@attributes[[x@individualObservations[[i]]$heightMeasurement$attributeID]]$methodID]]$name
          res[i, "height_value"] = x@individualObservations[[i]]$heightMeasurement$value
        }
        if("individualOrganismMeasurements" %in% names(x@individualObservations[[i]])) {
          measurements = x@individualObservations[[i]]$individualOrganismMeasurements
          if(length(measurements)>0) {
            for(j in 1:length(measurements)) {
              attID = measurements[[j]]$attributeID
              strAttID = paste0("ind_", names(measurements)[j],"_attID")
              if(IDs) {
                res[i, strAttID] = measurements[[j]]$attributeID
              }
              indMethod = paste0("ind_", names(measurements)[j],"_method")
              res[i, indMethod] = x@methods[[x@attributes[[attID]]$methodID]]$name
              if(subjects) {
                indSubject = paste0("ind_", names(measurements)[j],"_subject")
                res[i, indSubject] = x@methods[[x@attributes[[attID]]$methodID]]$subject
              }
              indVal = paste0("ind_", names(measurements)[j],"_value")
              res[i, indVal] = measurements[[j]]$value
            }
          }
        }
      }
    }
  }
  else if(element=="siteObservation") {
    if(IDs) {
      res = data.frame(plotObservationID = rep(NA, length(x@siteObservations)),
                       plotName = rep(NA, length(x@siteObservations)),
                       obsStartDate = rep(NA, length(x@siteObservations)),
                       row.names = names(x@siteObservations))
    } else {
      res = data.frame(plotName = rep(NA, length(x@siteObservations)),
                       obsStartDate = rep(NA, length(x@siteObservations)),
                       row.names = names(x@siteObservations))
    }
    if(length(x@siteObservations)>0){
      for(i in 1:length(x@siteObservations)){
        if(IDs) {
          res[i, "plotObservationID"] = x@siteObservations[[i]]$plotObservationID
        }
        res[i, "plotName"] = x@plots[[x@plotObservations[[x@siteObservations[[i]]$plotObservationID]]$plotID]]$plotName
        res[i, "obsStartDate"] = as.character(x@plotObservations[[x@siteObservations[[i]]$plotObservationID]]$obsStartDate)
        for(mesType in c("soilMeasurements", "climateMeasurements", "waterMassMeasurements")) {
          if(mesType %in% names(x@siteObservations[[i]])) {
            measurements = x@siteObservations[[i]][[mesType]]
            for(j in 1:length(measurements)) {
              attID = measurements[[j]]$attributeID
              soilAttID = paste0("soil", names(measurements)[j],"_attributeID")
              if(IDs) {
                res[i, soilAttID] = measurements[[j]]$attributeID
              }
              soilMethod = paste0("soil_", names(measurements)[j],"_method")
              res[i, soilMethod] = x@methods[[x@attributes[[attID]]$methodID]]$name
              if(subjects) {
                soilSubject = paste0("soil_", names(measurements)[j],"_subject")
               res[i, soilSubject] = x@methods[[x@attributes[[attID]]$methodID]]$subject
              }
              soilVal = paste0("soil_", names(measurements)[j],"_value")
              res[i, soilVal] = measurements[[j]]$value
            }
          }
        }
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
          if(IDs) resQuantitative[cntQuant, "methodID"] = x@attributes[[i]]$methodID
          resQuantitative[cntQuant, "methodName"] = x@methods[[x@attributes[[i]]$methodID]]$name
          resQuantitative[cntQuant, "methodSubject"] = x@methods[[x@attributes[[i]]$methodID]]$subject
          resQuantitative[cntQuant, "unit"] = x@attributes[[i]]$unit
          if("lowerLimit" %in% names(x@attributes[[i]])) resQuantitative[cntQuant, "lowerLimit"] = x@attributes[[i]]$lowerLimit
          if("upperLimit" %in% names(x@attributes[[i]])) resQuantitative[cntQuant, "upperLimit"] = x@attributes[[i]]$upperLimit
          rownames(resQuantitative)[cntQuant] = names(x@attributes)[i]
        }
        else if(x@attributes[[i]]$type=="ordinal") {
          cntOrd = cntOrd + 1
          if(IDs) resOrdinal[cntOrd, "methodID"] = x@attributes[[i]]$methodID
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
          if(IDs) resQualitative[cntQual, "methodID"] = x@attributes[[i]]$methodID
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
