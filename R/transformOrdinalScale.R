#' Transform ordinal scale
#'
#' Replaces all the values in a VegX object made using an ordinal scale into the corresponding scale midpoint values.
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified.
#' @param method An integer (index) or a name of an ordinal scale method.
#' @param newMethod An integer (index) or a name of a quantitative method existing in the initial object,
#' or an object of class \code{\linkS4class{VegXMethod}}.
#' @param replaceValues A boolean flag to indicate that values of the new scale should replace the old ones. 
#' For some measurements transformations will not be possible if replacement is not forced using this flag.
#' @param verbose A boolean flag to indicate console output of the data transformation process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @examples
#' data(mokihinui)
#'
#' # Create Veg-X document with aggregate organism observations with ordinal cover scale
#' taxmapping = list(plotName = "Plot", obsStartDate = "obsDate", authorTaxonName = "PreferredSpeciesName",
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
#' x = addAggregateOrganismObservations(newVegX(), tcv, "Mokihinui",
#'                         mapping = taxmapping,
#'                         methods = c(cover=coverscale),
#'                         stratumDefinition = strataDef)
#'
#' #Add stratum observations with ordinal cover scale
#' mapping = list(plotName = "Plot", obsStartDate = "obsDate", stratumName = "Tier",
#'                cover = "CoverClass")
#'
#' x = addStratumObservations(x, tier, "Mokihinui",
#'                         mapping = mapping,
#'                         methods = list(cover=coverscale),
#'                         stratumDefinition = strataDef)
#'
#'
#' # Transform from "Recce cover scale" to "Plant cover/%"
#' percentScale = predefinedMeasurementMethod("Plant cover/%")
#' y = transformOrdinalScale(x, "Recce cover scale", percentScale)
#'
transformOrdinalScale<-function(target, method, newMethod, replaceValues = FALSE, verbose = TRUE) {
  if(length(target@methods)==0) stop("VegX object has no methods")
  methodID = NULL
  if(is.numeric(method)) {
    methodID = as.character(method)
  }
  else {
    for(i in 1:length(target@methods)) {
      if(target@methods[[i]]$name==method) methodID = names(target@methods)[i]
    }
  }
  if(is.null(methodID)) stop("Target method not found.")
  if(verbose) cat(paste0(" Target method: '",target@methods[[methodID]]$name,"'\n"))
  attIDs = .getAttributeIDsByMethodID(target, methodID)
  if(verbose) cat(paste0(" Number of attributes: ", length(attIDs),"\n"))
  mapping = list()
  lowerLimit = Inf
  upperLimit = -Inf

  for(i in 1:length(attIDs)) {
    att = target@attributes[[attIDs[i]]]
    if(att$type=="ordinal") {
      if("midPoint" %in% names(att)) {
        mapping[[attIDs[i]]] = att$midPoint
        if("lowerLimit" %in% names(att)) lowerLimit = min(as.numeric(att$lowerLimit), lowerLimit)
        if("upperLimit" %in% names(att)) upperLimit = max(as.numeric(att$upperLimit), upperLimit)
      }
    }
  }
  if(verbose) cat(paste0(" Number of quantifiable attributes with midpoints: ", length(mapping),"\n"))
  if(verbose) cat(paste0(" Limits of the new attribute: [", lowerLimit,", ",upperLimit ,"]\n"))
  if(length(mapping)==0) stop("The selected method cannot be transformed.")

  # New method and attribute
  newMethodID = NULL
  if(is.numeric(newMethod)) {
    newMethodID = as.character(newMethod)
    if(!(newMethodID %in% names(target@methods))) stop("New method not found in Veg-X object.")
  }
  else if(is.character(newMethod)) {
    for(i in 1:length(target@methods)) {
      if(target@methods[[i]]$name==newMethod) methodID = names(target@methods)[i]
    }
    if(is.null(newMethodID)) stop("New method not found in Veg-X object.")
  }
  else if(class(newMethod)=="VegXMethod") {
    nmtid = .newMethodIDByName(target,newMethod@name)
    newMethodID = nmtid$id
    if(nmtid$new) { # add new method
      target@methods[[newMethodID]] = list(name = newMethod@name,
                                        description = newMethod@description,
                                        subject = newMethod@subject,
                                        attributeType = newMethod@attributeType)
      if(verbose) cat(paste0(" Measurement method '", newMethod@name,"' added.\n"))
      # add attributes if necessary
      cnt = length(target@attributes)+1
      for(i in 1:length(newMethod@attributes)) {
        nattid = as.character(length(target@attributes)+1)
        target@attributes[[nattid]] = newMethod@attributes[[i]]
        target@attributes[[nattid]]$methodID = newMethodID
        cnt = cnt + 1
      }
    } else { # method already existed
      if(verbose) cat(paste0(" Measurement method '", newMethod@name,"' already included.\n"))
    }
  } else {
    stop("Wrong class for 'newMethod'. Should be either a character, an integer or an object 'VegXMethod'")
  }
  newAttID = .getAttributeIDsByMethodID(target,newMethodID)
  if(length(newAttID)!=1) stop("New method has the wrong number of attributes.")
  if(target@attributes[[newAttID]]$type!="quantitative") stop("The attribute of the new method should be quantitative.")
  if(target@methods[[methodID]]$subject!=target@methods[[newMethodID]]$subject) stop("The two methods should apply to the same subject. Aborting.")
  
  # Apply mapping on aggregated organism observations
  naggtransf = 0
  nfruaggtransf = 0
  if(length(target@aggregateObservations)>0) {
    for(i in 1:length(target@aggregateObservations)) {
      if("aggregateOrganismMeasurements" %in% names(target@aggregateObservations[[i]])) {
        if(length(target@aggregateObservations[[i]]$aggregateOrganismMeasurements)>0) {
          for(j in 1:length(target@aggregateObservations[[i]]$aggregateOrganismMeasurements)) {
            mes = target@aggregateObservations[[i]]$aggregateOrganismMeasurements[[j]]
            if(mes$attributeID %in% names(mapping)) {
              m = list(attributeID = newAttID,
                       value = mapping[[mes$attributeID]])
              if(replaceValues){
                target@aggregateObservations[[i]]$aggregateOrganismMeasurements[[j]] = m
              } else {
                newmesID = as.character(length(target@aggregateObservations[[i]]$aggregateOrganismMeasurements)+1)
                target@aggregateObservations[[i]]$aggregateOrganismMeasurements[[newmesID]] = m
              }
              naggtransf = naggtransf + 1
            }
          }
        }
      }
    }
  }
  if(verbose && nfruaggtransf > 0) cat(paste0(" ", nfruaggtransf, " transformation(s) could not be applied on aggregate organism observations (see 'replaceValues').\n"))
  if(verbose && naggtransf > 0) cat(paste0(" ", naggtransf, " transformation(s) were applied on aggregate organism observations.\n"))

  # Apply mapping on individual organism observations
  nindtransf = 0
  if(length(target@individualObservations)>0) {
    for(i in 1:length(target@individualObservations)) {
      if("individualOrganismMeasurements" %in% names(target@individualObservations[[i]])) {
        if(length(target@individualObservations[[i]]$individualOrganismMeasurements)>0) {
          for(j in 1:length(target@individualObservations[[i]]$individualOrganismMeasurements)) {
            mes = target@individualObservations[[i]]$individualOrganismMeasurements[[j]]
            if(mes$attributeID %in% names(mapping)) {
              m = list(attributeID = newAttID,
                       value = mapping[[mes$attributeID]])
              if(replaceValues) {
                target@individualObservations[[i]]$individualOrganismMeasurements[[j]] = m
              }
              else {
                newmesID = as.character(length(target@individualObservations[[i]]$individualOrganismMeasurements)+1)
                target@individualObservations[[i]]$individualOrganismMeasurements[[newmesID]] = m
              }
              nindtransf = nindtransf + 1
            }
          }
        }
      }
    }
  }
  if(verbose && nindtransf > 0) cat(paste0(" ", nindtransf, " transformation(s) were applied on individual organism observations.\n"))

  # Apply mapping on stratum observations
  nstrtransf = 0
  nfrustrtransf = 0
  if(length(target@stratumObservations)>0) {
    for(i in 1:length(target@stratumObservations)) {
      if("heightMeasurement" %in% names(target@stratumObservations[[i]])){
        mes = target@stratumObservations[[i]]$heightMeasurement
        if(mes$attributeID %in% names(mapping)) {
          if(replaceValues) {
            m = list(attributeID = newAttID,
                     value = mapping[[mes$attributeID]])
            target@stratumObservations[[i]]$heightMeasurement = m
            nstrtransf = nstrtransf + 1
          } else {
            nfrustrtransf = nfrustrtransf + 1
          }
        }
      }
      if("stratumMeasurements" %in% names(target@stratumObservations[[i]])) {
        if(length(target@stratumObservations[[i]]$stratumMeasurements)>0) {
          for(j in 1:length(target@stratumObservations[[i]]$stratumMeasurements)) {
            mes = target@stratumObservations[[i]]$stratumMeasurements[[j]]
            if(mes$attributeID %in% names(mapping)) {
              m = list(attributeID = newAttID,
                       value = mapping[[mes$attributeID]])
              if(replaceValues) {
                target@stratumObservations[[i]]$stratumMeasurements[[j]] = m
              } else {
                newmesID = as.character(length(target@stratumObservations[[i]]$stratumMeasurements)+1)
                target@stratumObservations[[i]]$stratumMeasurements[[newmesID]] = m
              }
              nstrtransf = nstrtransf + 1
            }
          }
        }
      }
    }
  }
  if(verbose && nfrustrtransf > 0) cat(paste0(" ", nfrustrtransf, " transformation(s) could not be applied on stratum observations (see 'replaceValues').\n"))
  if(verbose && nstrtransf > 0) cat(paste0(" ", nstrtransf, " transformation(s) were applied on stratum observations.\n"))


  # Apply mapping on site observations
  nsitetransf = 0
  if(length(target@siteObservations)>0) {
    for(i in 1:length(target@siteObservations)) {
      for(m in c("soilMeasurements", "climateMeasurements", "waterMassMeasurements")) {
        if(m %in% names(target@siteObservations[[i]])) {
          if(length(target@siteObservations[[i]][[m]])>0) {
            for(j in 1:length(target@siteObservations[[i]][[m]])) {
              mes = target@siteObservations[[i]][[m]][[j]]
              if(mes$attributeID %in% names(mapping)) {
                m = list(attributeID = newAttID,
                         value = mapping[[mes$attributeID]])
                if(replaceValues) {
                  target@siteObservations[[i]][[m]][[j]] = m
                } 
                else {
                  newmesID = as.character(length(target@siteObservations[[i]][[m]])+1)
                  target@siteObservations[[i]][[m]][[newmesID]] = m
                }
                nsitetransf = nsitetransf + 1
              }
            }
          }
        }
      }
    }
  }
  if(verbose && nsitetransf > 0) cat(paste0(" ", nsitetransf, " transformation(s) were applied on site observations.\n"))

  # Return the modified document
  return(target)
}
