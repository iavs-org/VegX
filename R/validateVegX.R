#' VegX XML file validation/check
#'
#' Validates an XML file against VegX schema and checks its internal logical consistency
#'
#' @param file VegX xml file to be checked
#'
#' @export
#'
#' @author Sebastian Schmidtlein
#'
#' @importFrom curl nslookup
#' @importFrom xml2 read_xml xml_children xml_child xml_text xml_validate xml_attr
#' 
#' @examples
#' \dontrun{
#' # Load source data
#' data(mokihinui)
#'
#' # Define mapping
#' mapping = list(projectTitle = "Project", plotName = "Plot", subPlotName = "Subplot",
#'                obsStartDate = "PlotObsStartDate", obsEndDate = "PlotObsStopDate")
#'
#' # Create a new Veg-X document with projects, plots and plot observations (no data)
#' x = addPlotObservations(newVegX(), moki_site, mapping = mapping)
#'
#' # Save file
#' writeVegXML(x, "foo.xml")
#'
#' # Validate created XML file
#' validateVegX("foo.xml")
#'
#' }
validateVegX <- function(file) {

  # Helper functions

  # missingTargets: Get a vector of id's where references
  # point to. After that, check if the respective target elements exist.
  #
  # linkName      One unambiguous string such as "<methodID> (with brackets!)"
  #               defining the link
  # targetName    One unambiguous string such as "method id" defining the
  #               target

  missingTargets <- function(linkName, targetName) {

    # Find links
    links <- vegx[grep(linkName, vegx)]
    patternID <- paste(".*", linkName, "|", "<.*", sep = "")
    ids <- sort(unique(gsub(patternID, "", links)))

    # Find targets
    occurrencesTarget <- grep(targetName, vegx)
    patternTar <- paste(".*", "id=\"", "|", "\".*", sep = "")
    targets <- sub(" .*$", "", gsub(patternTar, "", vegx[occurrencesTarget]))

    # Find missing targets
    nonexisting <- ids[which(!ids %in% targets)]

    # Rows with missing targets
    linkidx <- which(vegx %in% links)
    missingString <- paste(linkName, nonexisting, "</", sep = "")
    problemRow <- linkidx[grep(missingString, vegx[linkidx])]

    if (sum(!ids %in% targets) > 0) {
      cat("------------------------\n")
      cat("'", linkName, "' points nowhere:\n", sep = "")
      cat(paste("'", nonexisting, "'", sep = ""), "\n")
      cat("Row(s):", problemRow, "\n")
      errorlog <<- errorlog + 1
    }
  }

  # orphanTargets: Check if there are orphan elements (i.e. elements
  # that should be target of some link, which are not targeted)
  #
  # targetName  One unambiguous string defining the target lines such as "method id"
  # linkNames   Vector with one or more unambiguous strings such as
  #             c("<methodID>", "<qualityAssessmentMethodID>") defining the
  #             linking lines

  orphanTargets <- function(targetName, linkNames) {

    # Find target
    targetLines <- vegx[grep(targetName, vegx)]
    patternTar <- paste(".*", "id=\"", "|", "\".*", sep = "")
    targets <- sub(" .*$", "", gsub(patternTar, "", targetLines))

    # Find links
    linkNamesString <- paste(linkNames, collapse = "|")
    linkLines <- vegx[grep(linkNamesString, vegx)] # Lines with occurrences of link strings
    patternLink <- paste(paste(".*", linkNames, "|", "<.*", sep = ""), collapse = "|") # pattern used for isolating actual id's
    ids <- sort(unique(gsub(patternLink, "", linkLines)))

    # Find orphans
    nonexisting <- targets[which(!targets %in% ids)]

    # Rows with orphans
    targetIdx <- which(vegx %in% targetLines)
    missingString <- paste(targetName, "=\"", nonexisting, "\">", sep = "")
    problemRow <- grep(paste(missingString, collapse="|"), vegx)


    if (sum(!targets %in% ids) > 0) {
      cat("------------------------\n")
      cat("'", targetName, "' has nothing pointing to it:\n", sep = "")
      cat(paste("'", nonexisting, "'", sep = ""), "\n")
      cat("Row(s):", problemRow, "\n")
      errorlog <<- errorlog + 1

      # Comments related to specific nodes
      # stratumObservation
      if (targetName == "stratumObservation id"){
        strObs <- xml_children(xml_child(doc, "stratumObservations"))
        ids <- xml_attr(strObs, "id")
        missedIds <- which(ids %in% nonexisting)
        problemObs <- strObs[missedIds]
        noProblem <- ids[missedIds[grep("stratumMeasurement", problemObs)]]
        if (sum(nonexisting %in% noProblem) == length(nonexisting)) {
          cat("This is probably not a problem because these observations\n")
          cat("do all contain stratumMeasurements, which are self-sufficient\n")
        }
        if (sum(nonexisting %in% noProblem) < length(nonexisting)) {
          cat("\nWhile stratumMeasurements are self-sufficient, the following elements\n")
          cat("do not contain stratumMeasurements and are therefore problematic:\n")
          cat("ID:", nonexisting[!nonexisting %in% noProblem], "\n")
        }
      }
      # stratumID
      if (targetName == "stratum id") {
        usedStrata <- targets[which(targets %in% ids)]
        cat("This may be intentional if an entire layer system is\n")
        cat("included while not all layers are used in the dataset.\n")
        if (length(usedStrata) == 1) {
          cat("One other layer is used so this could be the case here.\n")
        }
        if (length(usedStrata) > 1) {
          cat(length(usedStrata), "other layers are used so this could be the case here.\n")
        }
        if (length(usedStrata) == 0) {
          cat("However, in this case no stratum is used at all.\n")
        }
      }
    }
  }

  # Check if there is an internet connection
  hasNet <- !is.null(nslookup("kit.edu", error = FALSE))

  # If there is an internet connection, validate with latest schema
  if (hasNet) {

    # download
    downloadVegXschema(tempdir())

    # Read data
    schema <- read_xml(file.path(tempdir(),"veg.xsd"))  # as xml document
    doc <- read_xml(file)                     # as xml document

    # Report
    kid <- as.character(xml_children(schema)[1])
    patternVersion <- paste(".*", "plot-", "|", "\"/>", sep = "")
    cat(" version", gsub(patternVersion, "", kid), "...\n")
    cat("Checking file structure ...\n")

    # Validation using the xml2 validation function
    if (xml_validate(doc, schema) == TRUE) {
      cat("The basic structure is fine.\n")
    }
    else stop("XML file does not match the latest schema")
  }

  if (!hasNet) cat("No validation with schema.\n
                    Checking element relationships\n")

  vegx <- readLines(file, encoding = "utf-8")  # as character


  # Checking consistency of element relationships (the "database" structure)
  errorlogA <- errorlog <- 0
  # First check if all target ID's exist
  missingTargets("<methodID>",                      "method id")
  missingTargets("<qualityAssessmentMethodID>",     "method id")
  missingTargets("<attributeID>",                   "attribute id")
  missingTargets("<qualitativeAttributeID>",        "attribute id")
  missingTargets("<protocolID>",                    "protocol id")
  missingTargets("<citationID>",                    "literatureCitation id")
  missingTargets("<accordingToCitationID>",         "literatureCitation id")
  missingTargets("<interpretationCitationID>",      "literatureCitation id")
  missingTargets("<documentCitationID>",            "literatureCitation id")
  missingTargets("<organismNameID>",                "organismName id")
  missingTargets("<originalOrganismNameID>",        "organismName id")
  missingTargets("<preferredOrganismNameID>",       "organismName id")
  missingTargets("<organismIdentityID>",            "organismIdentity id")
  missingTargets("<partyID>",                       "party id")
  missingTargets("<determinationPartyID>",          "party id")
  missingTargets("<originalIdentificationPartyID>", "party id")
  missingTargets("<conceptAssertionPartyID>",       "party id")
  missingTargets("<interpretationPartyID>",         "party id")
  missingTargets("<placementPartyID>",              "party id")
  missingTargets("<locationPartyID>",               "party id")
  missingTargets("<elevationPartyID>",              "party id")
  missingTargets("<depthPartyID>",                  "party id")
  missingTargets("<observationPartyID>",            "party id")
  missingTargets("<taxonConceptID>",                "taxonConcept id")
  missingTargets("<communityConceptID>",            "communityConcept id")
  missingTargets("<projectID>",                     "project id")
  missingTargets("<relatedProjectID>",              "project id")
  missingTargets("<plotID>",                        "plot id")
  missingTargets("<relatedPlotID>",                 "plot id")
  missingTargets("<relatedItemID>",                 "individualOrganism id")
  missingTargets("<stratumObservationID>",          "stratumObservation id")
  missingTargets("<communityObservationID>",        "communityObservation id")
  missingTargets("<siteObservationID>",             "siteObservation id")
  missingTargets("<plotObservationID>",             "plotObservation id")
  missingTargets("<previousPlotObservationID>",     "plotObservation id")
  missingTargets("<observationGroupingID>",         "observationGrouping id")
  missingTargets("<stratumID>",                     "stratum id")
  missingTargets("<surfaceTypeID>",                 "surfaceType id")
  errorlogB <- errorlog


  # Check for orphans
  # Linking element required for:
  orphanTargets("party id", c("<partyID>",
                               "<determinationPartyID>",
                               "<originalIdentificationPartyID>",
                               "<conceptAssertionPartyID>",
                               "<interpretationPartyID>",
                               "<placementPartyID>",
                               "<locationPartyID>",
                               "<elevationPartyID>",
                               "<depthPartyID>",
                               "<observationPartyID>"))
  orphanTargets("literatureCitation id", c("<citationID>",
                               "<accordingToCitationID>",
                               "<interpretationCitationID>",
                               "<documentCitationID>"))
  orphanTargets("method id",  c("<methodID>",
                               "<qualityAssessmentMethodID>"))
  orphanTargets("protocol id", "<protocolID>")
  orphanTargets("organismIdentity id", "<organismIdentityID>")
  orphanTargets("taxonConcept id", "<taxonConceptID>")

  orphanTargets("communityConcept id", "<communityConceptID>")
  orphanTargets("project id", c("<projectID>",
                               "<relatedProjectID>"))
  orphanTargets("plot id", c("<plotID>",
                               "<relatedPlotID>"))
  orphanTargets("communityObservation id", "<communityObservationID>")
  orphanTargets("siteObservation id", "<siteObservationID>")
  orphanTargets("plotObservation id", c("<plotObservationID>",
                               "<previousPlotObservationID>"))
  orphanTargets("observationGrouping id", "<observationGroupingID>")
  orphanTargets("surfaceType id", "<surfaceTypeID>")
  orphanTargets("individualOrganism id", c("<relatedItemID>",
                               "<individualOrganismID>"))
  orphanTargets("organismName id", c("<organismNameID>",
                               "<originalOrganismNameID>",
                               "<preferredOrganismNameID>"))

  ############ schema-wise there is nothing pointing to these elements: #############
  # "taxonDetermination id"
  # "communityDetermination id"
  # "aggregateOrganismObservation id"


  ## Stratum observations can be orphans because direct measurements
  ## on strata are covered by stratumObservationID + attribute
  orphanTargets("stratumObservation id", "<stratumObservationID>")
  ## If stratum definitions are reported but not used this may
  ## result in non-linked elements but that's just fine.
  orphanTargets("stratum id", "<stratumID>")
  ## Attributes can be similar. For example if cover codes
  ## of a system are included for sake of completeness but not used.
  orphanTargets("attribute id", c("<attributeID>",
                               "<qualitativeAttributeID>"))

  errorlogC <- errorlog

  ## Check whether stratum/plot combinations of stratumObservations are unique
  stratO <- xml_text(xml_child(xml_children(xml_child(doc, "stratumObservations")), "stratumID"))
  plotO <- xml_text(xml_child(xml_children(xml_child(doc, "stratumObservations")), "plotObservationID"))
  if (length(paste(stratO, plotO)) != length(unique(paste(stratO, plotO)))) {
    cat("------------------------\n")
    cat("stratumObservations are not unique regarding plot-stratum combinations\n")
    errorlog <- errorlog + 1
  }



  ## Checking whether there are stratumTypes with a conflicting order
  ## TBD

  ## Checking whether there are stratumTypes with a conflicting definition
  ## TBD

  ## Duplicated species

  ## plotOrigin: If the horizontalCoordinates > locationInPlot has value "plot origin",
  ## this element needs to be present

  # Final reporting
  cat("------------------------\n")
  if (errorlog == 0) cat("Impressed, this file has no structural problems.\n")
  else cat("Finished with", errorlog, "potential issue(s)\n")

}
