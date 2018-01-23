#' Coearces VegX elements into a data frame
#'
#' Coerces part of the information of a Veg-X object into a data frame
#'
#' @param x An object of class \code{\linkS4class{VegX}}
#' @param element The name of the main elements to be coerced.
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
#' scale = defineCoverScale(name = "Standard Recce (Allen)", description = "Recce recording method by Allen",
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
showVegXElement<-function(x, element = "plot") {

  element = match.arg(element, c("plot", "plotObservation"))
  if(element=="plot") {
    res = data.frame(plotName = rep(NA, length(x@plots)), row.names = names(x@plots))
    for(i in 1:length(x@plots)){
      res[i,"plotName"] = x@plots[[i]]$plotName
      if("parentPlotID" %in% names(x@plots[[i]])) { #Add parent plot information (it is a subplot)
        res[i,"relatedPlotID"] = x@plots[[i]]$parentPlotID
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
    return(res)
  }
  return(NULL)
}
