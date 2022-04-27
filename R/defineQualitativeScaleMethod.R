#' Qualitative scale method definition
#'
#' A general function to define a method for measurements of a given subject in a qualitative (i.e. categorical) scale.
#'
#' @param name String with the name of the method.
#' @param description String describing the method.
#' @param subject A string to identify the subject.
#' @param codes A character vector of class codes.
#' @param citationString A string with the bibliographic reference for the method.
#' @param DOI A string with the DOI the resource related to \code{citationString}.
#' @param definitions A character vector of class definitions.
#'
#' @return an object of class \code{\linkS4class{VegXMethodDefinition}}
#'
#' @family define measurement functions
#'
#' @examples
#'
#' #Qualitative scale with three levels
#' defineQualitativeScaleMethod("scale1", "Description for scale1", "subject1",
#'                              codes = c("A", "B", "C"))
#'
#' @importFrom methods new
#'
#' @export
defineQualitativeScaleMethod<-function(name, description, subject, codes,
                             citationString = "", DOI = "",
                             definitions = NULL) {
  ncodes = length(codes)
  attributes = vector("list", ncodes)


  if(!is.null(definitions)) if(length(definitions)!= length(codes)) stop("'definitions' has to be of the same length as 'codes'.")

  for(i in 1:ncodes) {
    attributes[[i]] = list(type = "qualitative",
                           code = codes[i])
    if(!is.null(definitions)) attributes[[i]]$definition = definitions[[i]]
  }

  names(attributes) = 1:ncodes
  return(methods::new("VegXMethodDefinition",
             name = name,
             description = description,
             citationString = citationString,
             DOI = DOI,
             subject = "subject",
             attributeType = "qualitative",
             attributes = attributes))
}
