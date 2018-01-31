
#' Surface type definition
#'
#' @param name A string to identify the surface type definition.
#' @param description A string describing how surface types are defined.
#' @param surfaceNames A character vector of names for surface types.
#' @param definitions A character vector of definition of surface types.
#' @param citation A string with the bibliographic reference for the method.
#'
#' @return an object of class \code{\linkS4class{VegXSurfaceTypeDefinition}}
#'
#' @examples
#'
#' defineSurfaceTypes(name = "Default Surface types",
#'                    description = "Five simple surface categories",
#'                    surfaceNames = c("Vegetation", "Moss", "Litter", "Exposed Soil", "Rock"))
#'
defineSurfaceTypes<-function(name, description,
                             surfaceNames,
                             definitions = NULL,
                             citation = "") {
  defMethod = new("VegXMethod",
                  name = name,
                  description = description,
                  citation = citation,
                  subject = "surface category",
                  attributeType = "qualitative",
                  attributes = list())

  surfaceTypes = list()
  for(i in 1:length(surfaceNames)) {
    surfaceTypes[[as.character(i)]] = list(surfaceNames = surfaceNames[i])
    if(!is.null(definitions)) surfaceTypes[[as.character(i)]]$definition = definitions[i]
  }
  return(new("VegXSurfaceTypeDefinition",
             method = defMethod,
             surfaceTypes = surfaceTypes))
}

