#' Download VegX schema
#'
#' Downloads VegX schema, storing files in the indicated folder
#'
#' @param folder The latest schema will be downloaded to this folder
#'
#' @export
#'
#' @author Sebastian Schmidtlein
#' 
#' @importFrom curl nslookup
#' @importFrom xml2 download_xml
#'
#' @examples
#' \dontrun{
#' #Downloads schema to the current folder
#' downloadVegXschema()
#' }
downloadVegXschema <- function(folder = getwd()) {

  # If required, set directory for storing the schema
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(folder)

  # Check if there is an internet connection
  hasNet <- !is.null(nslookup("kit.edu", error = FALSE))

  # If there is an internet connection, validate with latest schema
  if (hasNet) {

    cat("Downloading latest VegX schema")

    # Download latest schema
    download_xml(url = "https://raw.githubusercontent.com/iavs-org/vegx-standard/master/veg.xsd")
    download_xml(url = "https://raw.githubusercontent.com/iavs-org/vegx-standard/master/veg-community.xsd")
    download_xml(url = "https://raw.githubusercontent.com/iavs-org/vegx-standard/master/veg-misc.xsd")
    download_xml(url = "https://raw.githubusercontent.com/iavs-org/vegx-standard/master/veg-organism.xsd")
    download_xml(url = "https://raw.githubusercontent.com/iavs-org/vegx-standard/master/veg-plot.xsd")
    download_xml(url = "https://raw.githubusercontent.com/iavs-org/vegx-standard/master/veg-plotobservation.xsd")
    download_xml(url = "https://raw.githubusercontent.com/iavs-org/vegx-standard/master/veg-userdefined.xsd")
    
  } else {
    cat("No connection")
  }
  setwd(wd)
}
