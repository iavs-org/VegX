#' Map Field Names
#' 
#' Provides different options to map equivalencies of the fields and the methods
#' used in a given dataset to standard field and method names.
#' 
#' @param project a character. The acronym or name of a network or project to
#'   obtain pre-defined field maps (see details).
#' @param equivalencies a data frame. A user-defined map of a given dataset to
#'   the to standard field and method names (see details).
#' @param user.map a named list. The user-defined mapping of the dataset to
#'   standard field and method names (see details).
#' @param user.column a character. The name of the column in
#'   \code{equivalencies} containing the mapping. Defaults to 'YourFieldNames'.
#'
#' @return A named list, with named vectors as elements.
#' 
#' @details Users can map their dataset to the standard field names and methods
#'   in three different ways:
#'   
#'   \itemize{
#'   \item{Option 1: provide to the argument \code{project} the acronym of
#'   one of the network, groups or databases of vegetation data with available
#'   pre-defined mapping to the field and method standards. The currently
#'   available options are available in the internal object
#'   \code{\link{supporting_info}}.}
#'    
#'   \item{Option 2: provide to the argument \code{equivalencies} a data
#'   frame containing a user-defined mapping of the dataset. Users can download
#'   and complete this data frame using the model and explanations available
#'   \href{INCLUDELINKHERE}{here}.}
#'
#'   \item{Option 3: provide to the argument \code{user.map} a named list
#'   containing a user-defined mapping of the dataset. This named list must
#'   follow the standard names which are in the internal object
#'   \code{\link{supporting_info}}. More information on the 
#'   meaning of each field are available \href{INCLUDELINKHERE}{here}.}
#'   }
#'   
#'   If the user chose for Option 2, than the use should also provide the name
#'   of the column containing the mapping. If the name of the column is not
#'   present in the input data frame, the function assumes the 4th column to be
#'   the one containing the mapping. If even the 4th column is not different
#'   from the predefined fields of the internal standard, then the function uses
#'   the next column which is not a predefined field as a final try.
#'   
#' @examples
#' # Example of the Option 1
#'  mapping <- prepareMapping(project = "NVS") 
#'
#' # Example of the Option 3
#'  my_map <- list(plot.info = c(plotName = "Plot",  
#'                              samplingEffort = "PlotArea", 
#'                              samplingEffortMethod = "Plot area/m2",
#'                              censusDateStart = "PlotObsStartDate", 
#'                              censusDateStop = "PlotObsStopDate"),
#'               plot.location = c(plotLongitude = "Longitude", 
#'                                 plotLatitude = "Latitude",
#'                                 elevationSite = "Altitude",
#'                                 elevationSiteMethod = "Elevation/m"),
#'               individual.data = c(plotNameSample = "Plot",
#'                                   obsStartDateSample = "PlotObsStartDate",
#'                                   fieldTag = "Identifier",
#'                                   organismNameOriginal = "NVSSpeciesName",
#'                                   diameter = "Diameter",
#'                                   diameterMethod = "DBH/cm"))
#'  
#'  mapping <- prepareMapping(user.map = my_map)
#'  
#' @family highlevel functions
#'
#' @export prepareMapping
#'
prepareMapping <- function(project = NULL,
                           equivalencies = NULL,
                           user.map = NULL,
                           user.column = "YourFieldNames")
{
  
  # Option 1: Using the pre-defined mappings validated by the networks
  if (!is.null(project)) {
    
    # Check input
    if (class(project) != "character")
      stop("Wrong class for 'project'. Should be an object of class 'character'")
    
    project.orig <- project
    available_projects <- supporting_info$available_projects
    project <- try(.matchArgExtended(project, available_projects, 
                                     ignore.case = TRUE),  TRUE)
    if (class(project) == "try-error")
      project <- try(.matchArgExtended(project.orig, available_projects, 
                                ignore.case = TRUE, method = "fuzzy"), TRUE)
    
    if (class(project) == "try-error") {
      project <- NULL
    } else {
      # Loading and filtering the internal VegX map for the pre-defined networks
      map <- supporting_info$predefined_map
      
      # Checking the input network name and columns
      if (project %in% colnames(map)) {
        cols <- c("Group", "Field", "Use", project)
        map <- map[ , match(cols, colnames(map), nomatch = 0)]
        # miss.cols <- map$Field[is.na(map[[project]]) & 
        #                           map$Use %in% "required"]
        # miss.cols.rec <- map$Field[is.na(map[[project]]) & 
        #                               map$Use %in% "recommended"]
        # miss.cols.opt <- map$Field[is.na(map[[project]]) & 
        #                               map$Use %in% "optional"]
        
      } else {
        stop(gettextf("Mapping is currently possible only for: %s", 
                      paste(dQuote(available_projects), collapse = ", ")), 
             domain = NA)
      }
      
      # Defining the object that will contain the mapping
      tables <- unique(map$Group)
      mapping <- vector("list", length(tables))
      names(mapping) <- tables
      
      # Defining the mapping for each type of table
      for (i in seq_len(length(tables))) {
        ids <- map$Group %in% tables[i]
        mapping[[i]] <- .setNames(map[[project]][ids], map[["Field"]][ids])
      }
      
      return(mapping)
    }
  }  
  
  # Option 2: user-provided data frame with the mapping
  if (!is.null(equivalencies)) {
    
    if (!class(equivalencies)[1] == "data.frame")
      stop("Wrong class for 'equivalencies'. Should be an object of class 'data frame'")
    
    if (dim(equivalencies)[1] == 0)
      stop("Input data frame is empty")
    
    map <- equivalencies

    if (user.column %in% colnames(map)) {
      user.col <- user.column
      cols <- c("Group", "Field", "Use", user.col)
      map <- map[ , match(cols, colnames(map), nomatch = 0)]
      # if ("Use" %in% colnames(map)) {
      #   miss.cols <- map$Field[is.na(map[[user.col]]) & 
      #                             map$Use %in% "required"]
      #   miss.cols.rec <- map$Field[is.na(map[[user.col]]) & 
      #                                 map$Use %in% "recommended"]
      #   miss.cols.opt <- map$Field[is.na(map[[user.col]]) & 
      #                                 map$Use %in% "optional"]
      # }
      
    } else {
      user.col <- colnames(map)[4]
      cols <- supporting_info$predefined_fields
      if (user.col %in% cols) {
        cols <- c("Group", "Field", "Use", user.col)
        map <- map[ , match(cols, colnames(map), nomatch = 0)]
        # if ("Use" %in% colnames(map)) {
        #   miss.cols <- map$Field[is.na(map[[user.col]]) & 
        #                             map$Use %in% "required"]
        #   miss.cols.rec <- map$Field[is.na(map[[user.col]]) & 
        #                                 map$Use %in% "recommended"]
        #   miss.cols.opt <- map$Field[is.na(map[[user.col]]) & 
        #                                 map$Use %in% "optional"]
        # }
      } else {
        user.column <- colnames(map)[!colnames(map) %in% cols][1]
        if (!is.na(user.column)) {
          cols <- c("Group", "Field", "Use", user.col)
          map <- map[ , match(cols, colnames(map), nomatch = 0)]
          # if ("Use" %in% colnames(map)) {
          #   miss.cols <- map$Field[is.na(map[[user.col]]) & 
          #                             map$Use %in% "required"]
          #   miss.cols.rec <- map$Field[is.na(map[[user.col]]) & 
          #                                 map$Use %in% "recommended"]
          #   miss.cols.opt <- map$Field[is.na(map[[user.col]]) & 
          #                                 map$Use %in% "optional"]
          # }
          
        } else {
          stop("No other column found in `equivalencies` besides the predefined fields")    
        }
      }
    }

    min.cols <- c("Group", "Field")
    if (!all(min.cols %in% colnames(map)))
      stop(gettextf("For mapping, the following columns are needed: %s", 
                    paste(dQuote(min.cols), collapse = ", ")), 
           domain = NA)
    
    # Defining the object that will contain the mapping
    tables <- unique(map$Group)
    reference_map <- supporting_info$reference_map
    tables <- tables[tables %in% names(reference_map)]
    mapping <- vector("list", length(tables))
    names(mapping) <- tables
    
    # Defining the mapping for each type of table 
    for (i in seq_len(length(tables))) {
      ids <- map$Group %in% tables[i]
      mapping[[i]] <- .setNames(map[[user.column]][ids], map[["Field"]][ids])
    }
    
    return(mapping)
  }  
  
  # Option 3: User-provided mapping using the functions arguments
  if (is.null(project) & is.null(equivalencies) & !is.null(user.map)) {
    
    reference_map <- supporting_info$reference_map
    mapping <- .mapToReference(user.map, reference_map)
    
    return(mapping)
  } 
  else {
    stop("Please provide some information to one of the arguments")
  }
}
