
#' Surface cover class definition by simple category
#'
#' @param name A string to identify the surface cover class definition.
#' @param description A string describing how surface cover classes are defined.
#' @param surfaceNames A character vector of codes for surface cover classes.
#' @param surfaceDefinitions A character vector of definition of surface cover classes.
#' @param citation A string with the bibliographic reference for the method.
#'
#' @return an object of class \code{\linkS4class{VegXSurfaceCoverDefinition}}
#'
#' @examples
#'
#' defineSurfaceCategories(name = "Surface covers",
#'                         description = "Four simple surface cover categories",
#'                         surfaceNames = c("bare soil", "water", "rocks", "vegetation"))
#'
defineSurfaceCategories<-function(name, description,
                                  surfaceNames,
                                  surfaceDefinitions = NULL,
                                  citation = "") {
  defMethod = new("VegXMethod",
                  name = name,
                  description = description,
                  citation = citation,
                  subject = "surface category",
                  attributeType = "qualitative",
                  attributes = list())

  surfaceCovers = list()
  for(i in 1:length(surfaceNames)) {
    surfaceCovers[[as.character(i)]] = list(surfaceNames = surfaceNames[i])
    if(!is.null(surfaceDefinitions)) surfaceCovers[[as.character(i)]]$definition = surfaceDefinitions[i]
  }
  return(new("VegXSurfaceCoverDefinition",
             method = defMethod,
             surfaceCovers = surfaceCovers))
}

