
#' Surface cover definition by simple category
#'
#' @param name A string to identify the surface cover definition.
#' @param description A string describing how surface covers are defined.
#' @param citation A string with the bibliographic reference for the method.
#' @param surfaceNames A numeric vector of surface codes (of length equal to the number of strata).
#'
#' @return an object of class \code{\linkS4class{VegXStrata}}
defineSurfaceCategories<-function(name = "Surface covers",
                                  description = "Four simple surface categories",
                                  citation = "",
                                  surfaceNames = c("bare soil", "water", "rock", "vegetation")) {
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
  }
  return(new("VegXSurfaceCoverDefinition",
             method = defMethod,
             surfaceCovers = surfaceCovers))
}

