#' Prepare Data to the VegX Format
#' 
#' @description This function ...
#' 
#' @param plot.info a data frame. General information of the plots or invariant
#'   inventory site characteristics (e.g. plot or site codes, names and
#'   identifiers, methods, location (place names), geographical coordinates,
#'   elevation, aspect, slope). If the site characteristic can vary between
#'   census, it should be provided as `site.data`.
#' @param plot.obs a data frame. Observations or census information related to
#'   the plots or inventories (e.g. census dates, data generator, citation,
#'   licenses, taxonomic quality checks)
#' @param site.data a data frame. Observations or measurements associated with
#'   the inventory site that may vary in time (soil and climate conditions,
#'   legal protection, land use, vegetation type)
#' @param surface.data a data frame. Surface cover measurements associated with
#'   the plot at a given time period.
#' @param community.data  a data frame. Observations or measurements associated
#'   with the entire vegetation, i.e. observations at the stand or community
#'   level (e.g. density, biomass, species diversity, canopy cover, vegetation
#'   height, successional status)
#' @param species.data a data frame. The aggregated observations per species
#'   (e.g. species name, count of individuals, total biomass, frequency)
#' @param individual.data a data frame. The observations per plant individual
#'   (e.g. species name, field tag, diameter, height, coordinates within the
#'   plot)
#' @param project.info a data frame. The information about the project that
#'   generated/compiled the vegetation data (e.g. abstract, funding, citation
#'   and short descriptions of the studied area and the typical project design)
#' @param people.info a data frame. The information about the people involved in
#'   the project that generated/compiled the vegetation data (e.g. full names,
#'   roles, addresses, email)
#' @param citation.info a data frame. The information about the literature
#'   associated with the vegetation data (e.g. citation ID, authors, year,
#'   title, etc.)
#' @param mapping a named list with no default. Usually the output of the VegX
#'   function function `prepareMapping()`.
#' @param project.title a character with the project name/title. Only needed if
#'   the user wants to use a different name than the one provided in the
#'   argument `mapping`.
#' @param miss.fields logical. Should any missing fields be auto-completed in
#'   the field mapping from matching the names of the columns in the input data?
#'   Defaults to TRUE.
#' @param fixed.date date. A fixed date to be added for projects or plots
#'   without census date information. Defaults to the current system data (i.e.
#'   `Sys.Date()`). For dates other than the default, preferably use the
#'   following format: "YYYY-MM-DD".
#' @param spp.names character vector. The priority order for defining the
#'   working organism name for building the VegX object and for calculating
#'   species diversity indices. Defaults to the following order:
#'   'organismNameMorpho', 'organismNameOriginal' and 'organismName'.
#' @param summarise.data logical. Should individual or species plot data be
#'   aggregated at higher levels of information (i.e. species and community
#'   levels)? Defaults to TRUE.
#'
#' @return A list with ...
#' 
#' @details This function
#' 
#' @importFrom anytime anydate
#' 
#' @family high-level functions
#' 
#' @export prepareInputData
#'
#'
prepareInputData <- function(plot.info = NULL,
                             plot.obs = NULL,
                             site.data = NULL,
                             surface.data = NULL,
                             community.data = NULL,
                             species.data = NULL,
                             individual.data = NULL,
                             project.info = NULL,
                             people.info = NULL,
                             citation.info = NULL,
                             # species.info = NULL,
                             # env.data = NULL,
                             mapping = NULL,
                             project.title = NULL,
                             miss.fields = TRUE,
                             fixed.date = Sys.Date(),
                             spp.names = c("organismNameMorpho", 
                                           "organismNameOriginal", 
                                           "organismName"),
                             summarise.data = TRUE) {
  
  # Check the input info
  if (is.null(plot.info) & (is.null(community.data) | 
                              is.null(species.data) | 
                                is.null(individual.data)))
    stop("Please provide the data frames containing the plot information, community, species and/or individual data")

  if (is.null(mapping))
    stop("Please provide the mapping of the fields of your vegetation data")

  if (!is.list(mapping))
    stop("Please provide the mapping of your vegetation data as a list")

  if (is.list(mapping) & is.null(names(mapping)))
    stop("Please provide the mapping of your vegetation data as a named list")

  # Check the input mapping ------------------------------------------------
  # Detect missing fields and replaced them by fields present in the data
  if (miss.fields) {

    table.names <- names(mapping)
    tabelas <- table.names[-1]
    excep.tabs  <- c("plot.obs", "site.data", "community.data")
    tabelas <- tabelas[!tabelas %in% excep.tabs]
    for (i in seq_along(tabelas))
      if (is.null(get(tabelas[i])))
        table.names <- table.names[!table.names %in% tabelas[i]]

    for (i in seq_along(table.names)) {
      name.i <- table.names[i]
      if (name.i %in% excep.tabs) {
        if (is.null(get(name.i))) {
          replace_these <- is.na(mapping[[name.i]]) & 
            names(mapping[[name.i]]) %in% colnames(plot.info) 
        } else {
          replace_these <- is.na(mapping[[name.i]]) & 
            names(mapping[[name.i]]) %in% colnames(get(name.i)) 
        }
      } else {
        replace_these <- is.na(mapping[[name.i]]) & 
          names(mapping[[name.i]]) %in% colnames(get(name.i))
      }
      
      if (any(replace_these))
        mapping[[name.i]][replace_these] <- 
          names(mapping[[name.i]])[replace_these]
    }
  }

  # Defining the required fields -------------------------------------------
  ## Plot name, Plot ID and Subplot names
  plotName <- mapping$plot.info[["plotName"]]
  plotID <- mapping$plot.info[["plotID"]]
  
  if (is.na(plotName)) {
    if (is.na(plotID)) {
      stop(gettextf("The column containing the plot names or IDs is required and was not found in the mapping"), 
           domain = NA)
    } else {
      plotName <- plotID
    }
  }
  
  if (is.na(mapping$species.data[["plotNameSample"]])) {
    plotNameSample.sp <- plotName
  } else {
    plotNameSample.sp <- mapping$species.data[["plotNameSample"]]
  }
    
  if (is.na(mapping$individual.data[["plotNameSample"]])) {
    plotNameSample.ind <- plotName
  } else {
    plotNameSample.ind <- mapping$individual.data[["plotNameSample"]]
  }
  
  #### CHECK HERE: ALLOW SPECIES/INDIVIDUAL DATA TO STORE SUBPLOT NAMES UNDER A DIFFERENT COLUMN
  # if (is.na(mapping$species.data[["subplotNameSample"]]))
  #   subplotNameSample <- subplotName
  
  if (!plotName %in% colnames(species.data) & !is.null(species.data))
    stop("The names or IDs of the samples (e.g. plots) must be present in both the plot info and species data")

  if (!plotName %in% colnames(individual.data) & !is.null(individual.data))
    stop("The names or IDs of the samples (e.g. plots) must be present in both the plot info and individual data")
  
    
  ## Project title/name
  # making sure that the data contains a project name
  if (is.null(project.title)) {
    project1 <- mapping$project.info[["title"]]
    project2 <- mapping$plot.obs[["projectTitle"]]
    
    if (is.na(project1) & !is.na(project2) |
          !is.na(project1) & !is.na(project2)) {
      project.title <- project2
      mapping$project.info[["title"]] <- project.title
    }
    
    if (!is.na(project1) & is.na(project2)) {
      project.title <- project1
      mapping$plot.obs[["projectTitle"]] <- project.title
    }
    
    if (is.na(project1) & is.na(project2)) {
      tempo <- Sys.Date()
      project.title <- paste0("(undefined project created in ",
                              tempo,")")
      mapping$project.info[["title"]] <- project.title
      mapping$plot.obs[["projectTitle"]] <- project.title
    }
  } else {
    mapping$project.info[["title"]] <- project.title
    mapping$plot.obs[["projectTitle"]] <- project.title
  }
  
  # Organizing the available info ------------------------------------------
  all.data <- list(plot.info = plot.info,
                   plot.obs = plot.obs, 
                   site.data = site.data, 
                   community.data = community.data, 
                   species.data = species.data,
                   individual.data = individual.data,
                   project.info = project.info,
                   people.info = people.info,
                   citation.info = citation.info)
  
  for (i in seq_along(all.data))  {
    if (!is.null(all.data[[i]])) {
      nomes.dados <- names(all.data[[i]])
      nomes.mapa <- mapping[[names(all.data)[[i]]]]
      nomes.mapa <- nomes.mapa[!is.na(nomes.mapa)]
      nomes.dados <- nomes.dados[nomes.dados %in% nomes.mapa]
      if ("projectTitle" %in% names(nomes.mapa))
        nomes.dados <- c("projectTitle", nomes.dados)
      new.dados <- all.data[[i]][ ,nomes.dados]
      
      nomes.mapa <- nomes.mapa[!nomes.mapa %in% names(new.dados)]
      check_these <- !grepl("projectTitle|Method$", names(nomes.mapa), perl = TRUE)
      if (any(check_these))
        new.dados[, names(nomes.mapa)[check_these]] <- NA_character_
      
      all.data[[i]] <- new.dados
    } else {
      if (names(all.data)[i] %in% c("plot.obs", "site.data", 
                                    "community.data", "citation.info")){
        if (!is.null(plot.info)) {
          nomes.dados <- names(plot.info)
          nomes.mapa <- mapping[[names(all.data)[[i]]]]
          nomes.mapa <- nomes.mapa[!is.na(nomes.mapa)]
          nomes.dados <- unique(c(plotName, plotID,
                              nomes.dados[nomes.dados %in% nomes.mapa]))
          new.dados <- plot.info[, nomes.dados]
      
          nomes.mapa <- nomes.mapa[!nomes.mapa %in% names(new.dados)]
          check_these <- !grepl("projectTitle|Method$", names(nomes.mapa), perl = TRUE)
          if (any(check_these))
            new.dados[, names(nomes.mapa)[check_these]] <- NA_character_
          
          all.data[[i]] <- new.dados
        }  
      }
    }
  }
  
  # adding the variable network/project name to the plot.obs and project.info
  all.data$plot.obs[["projectTitle"]] <- project.title
  all.data$project.info[["title"]] <- project.title
  
  # Imputing missing fields -------------------------------------------
  ## Missing census date?
  census.date.start <- mapping$plot.obs[["censusDateStart"]]
  census.date.stop <- mapping$plot.obs[["censusDateStop"]]
    
  fixed.date <- anytime::anydate(fixed.date)
  if (is.na(census.date.start) & is.na(census.date.stop)) {
    all.data[["plot.obs"]]["PlotObsStartDate.tmp"] <- fixed.date
    mapping$plot.obs[["censusDateStart"]] <- 
      census.date.start <- "PlotObsStartDate.tmp"
  }
    
  ## Formatting sampling dates
  if (!is.na(census.date.start)) 
    all.data[["plot.obs"]][, census.date.start] <- 
        anytime::anydate(as.character(
          all.data[["plot.obs"]][, census.date.start]))
    
  # Any missing starting dates
  check_these <- is.na(all.data[["plot.obs"]][, census.date.start])
  if (any(check_these))
    all.data[["plot.obs"]][, census.date.start][check_these] <- fixed.date
    
  if (!is.na(census.date.stop)) {
    all.data[["plot.obs"]][, census.date.stop] <-
      anytime::anydate(as.character(all.data[["plot.obs"]][, census.date.stop]))
    check_these <- is.na(all.data[["plot.obs"]][, census.date.stop])
    if (any(check_these))
      all.data[["plot.obs"]][, census.date.stop][check_these] <- fixed.date
  } else {
    all.data[["plot.obs"]]["PlotObsStopDate.tmp"] <- 
      all.data[["plot.obs"]][,census.date.start]
    mapping$plot.obs[["censusDateStop"]] <- 
      census.date.stop <- "PlotObsStopDate.tmp"
  }
    
  # Site, community, species or individual data without date? Add start census date
  #### CHECK HERE: what to do when there is >1 census (i.e. >1 "PlotObsStartDate")? For now just returnig a warning ####
  
  data.vector <- c("site.data", "community.data", "species.data", 
                   "individual.data")
  no.dates <- NULL 
  for (i in seq_along(data.vector)) {
    data.vec <- data.vector[i]
    if (!is.null(all.data[[data.vec]])){
      if (is.na(mapping[[data.vec]][["obsStartDateSample"]])) {
        tmp <- all.data[[data.vec]]
        tmp$ordem.tmp <- seq_len(dim(tmp)[1])
        tmp.census.date.start <- mapping$plot.obs[["censusDateStart"]]
        tmp.plot.info <- 
          all.data[["plot.obs"]][,c(plotName, tmp.census.date.start)]
        tmp <- merge(tmp, tmp.plot.info, 
                     by = plotName, all.x = TRUE, sort = FALSE)
        tmp <- tmp[order(tmp$ordem.tmp), ]
        all.data[[data.vec]] <- 
          tmp[, match(c(colnames(all.data[[data.vec]]), 
                        tmp.census.date.start), colnames(tmp),  nomatch = 0)]
        mapping[[data.vec]][["obsStartDateSample"]] <- tmp.census.date.start
        if (is.null(no.dates)) no.dates <- data.vec else no.dates <- c(no.dates, data.vec)
      }  
    }
  }

  if (!is.null(no.dates))
    warning(paste0("Assuming the same sampling date for measurements in: ",
                   paste0(no.dates, collapse = ", "),
                   ". If any plot has multiple measurements, please declare their dates explicitly"), 
          domain = NA, call. = FALSE)
  
  # Creating the combined strings with taxon/organism names -----------------
  ## Species-level plot data
  if (!is.null(species.data)) {
      if (is.na(mapping$species.data[["organismNameOriginal"]])) {
          orig.cols <- c("organismGenusOriginal", "organismEpitethOriginal", 
              "organismInfraEpitethOriginal", "organismAuthorshipOriginal")
          cols <- 
            mapping$species.data[names(mapping$species.data) %in% orig.cols]
          cols <- cols[!is.na(cols)]
          if (any(grepl("organismGenus", names(cols))) & 
                any(grepl("organismEpiteth", names(cols)))) {
            tmp <- all.data[["species.data"]]
            tmp$organismNameOriginal <- do.call(paste, c(tmp[, cols]))
            tmp$organismNameOriginal <- 
              gsub(" NA$", "", tmp$organismNameOriginal, perl = TRUE)
            tmp$organismNameOriginal <- 
              gsub( "^ | $", "", gsub("\\s+", " ", tmp$organismNameOriginal, 
                         perl = TRUE), perl = TRUE)
            all.data[["species.data"]] <- tmp
            mapping$species.data[["organismNameOriginal"]] <- 
              "organismNameOriginal"
          } else {
            all.data[["species.data"]]["organismNameOriginal"] <- NA_character_
          }
          
      } else {
        all.data[["species.data"]][ , mapping$species.data[["organismNameOriginal"]]] <- 
          gsub( "^ | $", "", gsub("\\s+", " ", 
              all.data[["species.data"]][ , mapping$species.data[["organismNameOriginal"]]], 
                  perl = TRUE), perl = TRUE)
      }
        
      if (is.na(mapping$species.data[["organismNameMorpho"]])) {
        orig.cols <- c("organismGenusMorpho", "organismEpitethMorpho", 
              "organismInfraEpitethMorpho", "organismAuthorshipMorpho")
        cols <- 
          mapping$species.data[names(mapping$species.data) %in% orig.cols]
        cols <- cols[!is.na(cols)]
        if (any(grepl("organismGenus", names(cols))) & 
              any(grepl("organismEpiteth", names(cols)))) {
          tmp <- all.data[["species.data"]]
          tmp$organismNameMorpho <- do.call(paste, c(tmp[, cols]))
          tmp$organismNameMorpho <- 
              gsub( " NA$", "", tmp$organismNameMorpho, perl = TRUE)
          tmp$organismNameMorpho <- 
              gsub( "^ | $", "", gsub("\\s+", " ", tmp$organismNameMorpho, 
                  perl = TRUE), perl = TRUE)
          all.data[["species.data"]] <- tmp
          mapping$species.data[["organismNameMorpho"]] <- "organismNameMorpho"
        } else {
          all.data[["species.data"]]["organismNameMorpho"] <- NA_character_
        }
          
      } else {
        all.data[["species.data"]][ , mapping$species.data[["organismNameMorpho"]]] <- 
            gsub( "^ | $", "", gsub("\\s+", " ", 
                all.data[["species.data"]][ , mapping$species.data[["organismNameMorpho"]]], 
                  perl = TRUE), perl = TRUE)
      }   
        
      if (is.na(mapping$species.data[["organismName"]])) {
        orig.cols <-  c("organismGenus", "organismEpiteth", 
              "organismInfraEpiteth", "organismAuthorship")
        cols <- 
            mapping$species.data[names(mapping$species.data) %in% orig.cols]
        cols <- cols[!is.na(cols)]
        if (any(grepl("organismGenus", names(cols))) & 
              any(grepl("organismEpiteth", names(cols)))) {
          tmp <- all.data[["species.data"]]
          tmp$organismName <- do.call(paste, c(tmp[, cols]))
          tmp$organismName <- gsub( " NA$", "", tmp$organismName, perl = TRUE)
          tmp$organismName <- 
              gsub( "^ | $", "", gsub("\\s+", " ", tmp$organismName, 
                  perl = TRUE), perl = TRUE)
          all.data[["species.data"]] <- tmp
          mapping$species.data[["organismName"]] <- "organismName"
        } else {
          all.data[["species.data"]]["organismName"] <- NA_character_
        }
          
      } else {
        all.data[["species.data"]][ , mapping$species.data[["organismName"]]] <- 
            gsub( "^ | $", "", gsub("\\s+", " ", 
                all.data[["species.data"]][ , mapping$species.data[["organismName"]]], 
                    perl = TRUE), perl = TRUE)
      }
    
    spp.data <- mapping$species.data[spp.names]
    organism.name.spp.data <- spp.data[!is.na(spp.data)][1]
  }
 
  ## Individual-level plot data
  if (!is.null(individual.data)) {
    if (is.na(mapping$individual.data[["organismNameOriginal"]])) {
      orig.cols <- c("organismGenusOriginal", "organismEpitethOriginal", 
                     "organismInfraEpitethOriginal", "organismAuthorshipOriginal")
      cols <- 
        mapping$individual.data[names(mapping$individual.data) %in% orig.cols]
      cols <- cols[!is.na(cols)]
      if (any(grepl("organismGenus", names(cols))) & 
          any(grepl("organismEpiteth", names(cols)))) {
        tmp <- all.data[["individual.data"]]
        tmp$organismNameOriginal <- do.call(paste, c(tmp[, cols]))
        tmp$organismNameOriginal <- 
          gsub(" NA$", "", tmp$organismNameOriginal, perl = TRUE)
        tmp$organismNameOriginal <- 
          gsub( "^ | $", "", gsub("\\s+", " ", tmp$organismNameOriginal, 
                                  perl = TRUE), perl = TRUE)
        all.data[["individual.data"]] <- tmp
        mapping$individual.data[["organismNameOriginal"]] <- 
          "organismNameOriginal"
      } else {
        all.data[["individual.data"]]["organismNameOriginal"] <- NA_character_
      }
      
    } else {
      all.data[["individual.data"]][ , mapping$individual.data[["organismNameOriginal"]]] <- 
        gsub( "^ | $", "", gsub("\\s+", " ", 
                                all.data[["individual.data"]][ , mapping$individual.data[["organismNameOriginal"]]], 
                                perl = TRUE), perl = TRUE)
    }
    
    if (is.na(mapping$individual.data[["organismNameMorpho"]])) {
      orig.cols <- c("organismGenusMorpho", "organismEpitethMorpho", 
                     "organismInfraEpitethMorpho", "organismAuthorshipMorpho")
      cols <- 
        mapping$individual.data[names(mapping$individual.data) %in% orig.cols]
      cols <- cols[!is.na(cols)]
      if (any(grepl("organismGenus", names(cols))) & 
          any(grepl("organismEpiteth", names(cols)))) {
        tmp <- all.data[["individual.data"]]
        tmp$organismNameMorpho <- do.call(paste, c(tmp[, cols]))
        tmp$organismNameMorpho <- 
          gsub( " NA$", "", tmp$organismNameMorpho, perl = TRUE)
        tmp$organismNameMorpho <- 
          gsub( "^ | $", "", gsub("\\s+", " ", tmp$organismNameMorpho, 
                                  perl = TRUE), perl = TRUE)
        all.data[["individual.data"]] <- tmp
        mapping$individual.data[["organismNameMorpho"]] <- "organismNameMorpho"
      } else {
        all.data[["individual.data"]]["organismNameMorpho"] <- NA_character_
      }
      
    } else {
      all.data[["individual.data"]][ , mapping$individual.data[["organismNameMorpho"]]] <- 
        gsub( "^ | $", "", gsub("\\s+", " ", 
                                all.data[["individual.data"]][ , mapping$individual.data[["organismNameMorpho"]]], 
                                perl = TRUE), perl = TRUE)
    }   
    
    if (is.na(mapping$individual.data[["organismName"]])) {
      orig.cols <-  c("organismGenus", "organismEpiteth", 
                      "organismInfraEpiteth", "organismAuthorship")
      cols <- 
        mapping$individual.data[names(mapping$individual.data) %in% orig.cols]
      cols <- cols[!is.na(cols)]
      if (any(grepl("organismGenus", names(cols))) & 
          any(grepl("organismEpiteth", names(cols)))) {
        tmp <- all.data[["individual.data"]]
        tmp$organismName <- do.call(paste, c(tmp[, cols]))
        tmp$organismName <- gsub( " NA$", "", tmp$organismName, perl = TRUE)
        tmp$organismName <- 
          gsub( "^ | $", "", gsub("\\s+", " ", tmp$organismName, 
                                  perl = TRUE), perl = TRUE)
        all.data[["individual.data"]] <- tmp
        mapping$individual.data[["organismName"]] <- "organismName"
      } else {
        all.data[["individual.data"]]["organismName"] <- NA_character_
      }
      
    } else {
      all.data[["individual.data"]][ , mapping$individual.data[["organismName"]]] <- 
        gsub( "^ | $", "", gsub("\\s+", " ", 
                                all.data[["individual.data"]][ , mapping$individual.data[["organismName"]]], 
                                perl = TRUE), perl = TRUE)
    } 
    
    spp.data <- mapping$individual.data[spp.names]
    organism.name.ind.data <- spp.data[!is.na(spp.data)][1]
  }
     
  # Removing fields for which there is no predefined method available --------
  all_fields <- cbind.data.frame(Field = names(unlist(mapping)), 
                                   Equiv = unlist(mapping))
  methods <- all_fields[grepl("Method$", all_fields$Field), ]
  fields <- all_fields[!grepl("Method$", all_fields$Field) & 
                         !is.na(all_fields$Equiv), ]
  keep_these <- !is.na(methods$Equiv) | 
                  methods$Field %in% paste0(fields$Field, "Method")
  methods <- methods[keep_these, ]
  # methods <- methods[!is.na(methods$Equiv), ]
  methods$Group <- sapply(methods$Field, 
                          function (x) paste(
                            strsplit(x, split = "\\.")[[1]][1:2], 
                            collapse =  "."))
  methods$Method <- gsub(".*\\.", "", methods$Field, perl = TRUE)
  methods$Field <- gsub("Method$", "", methods$Method)
  check_these <- !methods$Equiv %in% supporting_info$available_methods
  removed_fields <- NULL
  if (any(check_these)) {
    removed_fields <- methods$Field[check_these]
    for (i in seq_along(removed_fields)) {
      campo <- removed_fields[i]
      grupo <- methods$Group[methods$Field %in% campo]
      metodo <- methods$Method[methods$Field %in% campo]
      mapping[[grupo]][[campo]] <- NA_character_
      mapping[[grupo]][[metodo]] <- NA_character_
    }
  miss.fields <- paste0(removed_fields, " (", methods$Equiv[check_these], ")")
  warning(gettextf("The methods for the following fields are missing or are currently not available: %s. They were removed from the map", 
                     paste(dQuote(miss.fields), collapse = ", ")), 
            domain = NA, call. = FALSE)
  }
    
  ## Organizing essential information --------------------------------------
  essential.info <- c(plotName = plotName)
  if (exists("plotID"))
    essential.info <- c(essential.info, plotID = plotID)
  if (exists("plotNameSample.sp"))
    essential.info <- c(essential.info, 
                        plotName.SpeciesData = plotNameSample.sp)
  if (exists("plotNameSample.ind"))
    essential.info <- c(essential.info, 
                        plotName.IndividualData = plotNameSample.ind)
  # if (exists("subplotNameSample"))
  #   essential.info <- c(essential.info, 
  #                       subplotNameSample = subplotNameSample)
  if (exists("census.date.start"))
    essential.info <- c(essential.info, censusDateStart = census.date.start)
  if (exists("census.date.stop"))
    essential.info <- c(essential.info, censusDateStop = census.date.stop)
  if (exists("organism.name.spp.data"))
    essential.info <- c(essential.info, organismName.SpeciesData = 
                          as.character(organism.name.spp.data))
  if (exists("organism.name.ind.data"))
    essential.info <- c(essential.info, organismName.IndividualData = 
                          as.character(organism.name.ind.data))
  # if (exists("project.title"))
  #   essential.info <- c(essential.info, projectTitle = project.title)

  ## Getting higher level summaries ---------------------------------------
  if (summarise.data) {
    if (!is.null(all.data[['individual.data']])) {
      #### CHECK HERE: INCLUDE SUMMARY FROM Individuals -> Species level ####
    }
    
    if (!is.null(all.data[['species.data']])) {
      metrics <- c("individuals", "density", "basalArea", "AGB",
                   "richness", "shannon", "simpson", "pielou")
      miss.metrics <- metrics[!metrics %in%
                                names(mapping[['community.data']][!is.na(mapping[['community.data']])])]
      
      if (is.null(all.data[['community.data']]) | 
            !identical(miss.metrics, character(0))) {

        spp.by <- mapping[["species.data"]][spp.names]
        spp.by <- spp.by[!is.na(spp.by)][1]
        
        plot.info <- c("plotNameSample", "obsStartDateSample") 
        plot.by <- mapping[["species.data"]][plot.info]
        group.by <- all.data[['species.data']][, plot.by]
        if (length(dim(group.by)) > 1) {
          group.by$combo <- apply(group.by, 1, paste, collapse = "___")
        } else {
          group.by <- data.frame(combo = group.by) 
        }
        
        field.names <- c("countsMeasurement", "basalAreaMeasurement", 
                       "biomassMeasurement")
        fields <- mapping[["species.data"]][field.names]
        fields <- fields[!is.na(fields)]

        # stand/commnunity metrics
        tmp <- NULL
        if (any(field.names %in% names(fields))) {
          tmp <- aggregate(x = all.data[['species.data']][, fields],
                           by = group.by[, "combo", drop = FALSE], 
                           sum, na.rm = TRUE)
          names(tmp) <- c("group.by", names(fields))
        }
          
        # stand/commnunity species richness
        if (any(miss.metrics %in% c("richness", "pielou"))) {
          tmp.sp <- aggregate(x = all.data[['species.data']][, spp.by], 
                              by = group.by[, "combo", drop = FALSE], 
                              function(x) length(unique(x)))
          names(tmp.sp) <- c("group.by", "speciesRichness")
          if (is.null(tmp)) tmp <- tmp.sp else tmp$speciesRichness <- 
                                                  tmp.sp$speciesRichness 
        }
        
        # stand/commnunity species diversity/heterogenity indices
        if (any(miss.metrics %in% 
                c("shannon", "simpson", "pielou", "inv.simpson"))) {
          if ("countsMeasurement" %in% names(fields)) {
            contagem <- 
              all.data[['species.data']][, fields[["countsMeasurement"]]]
              
            shannon <- .getSpeciesDiversity(contagem, "shannon", group.by$combo)
            simpson <- .getSpeciesDiversity(contagem, "simpson", group.by$combo)
            # inv.simpson <- .getSpeciesDiversity(contagem, "inv.simpson", group.by)
            pielou <- .getSpeciesDiversity(contagem, "pielou", group.by$combo)
            tmp.div <- cbind.data.frame(shannon, pielou, simpson, 
                                        inv.simpson = 1/(1 - simpson))
            if (is.null(tmp)) { 
                tmp <- cbind.data.frame(group.by = sort(unique(group.by$combo)),
                                        tmp.div) 
              } else {
                tmp <- cbind.data.frame(tmp, tmp.div) 
              }  
          } else {
            warning("Counts of individuals per species not found to calculate the diversity metrics")
          }
        }
        
        # editing and writing the missing metrics and their methods
        if (!is.null(tmp)) {
          tmp1 <- do.call(rbind.data.frame, 
                          strsplit(tmp$group.by, "___", fixed = TRUE))
          names(tmp1) <- names(group.by[, -which(names(group.by) %in% "combo")])
          tmp2 <- cbind.data.frame(group.by = tmp$group.by, tmp1)
          tmp2 <- merge(tmp2, tmp, by = "group.by", all.x = TRUE, sort = FALSE)

          if (!is.na(mapping[["plot.info"]]["samplingEffort"])) {
            effort <- mapping[["plot.info"]]["samplingEffort"]
            plot.effort <- 
              all.data[["plot.info"]][, c(plot.by[1], effort)]
            
            effortMethod <- mapping[["plot.info"]]["samplingEffortMethod"]
            if (effortMethod %in% "Plot area/m2")
              plot.effort$PlotSize <- plot.effort$PlotSize*0.0001 
            
            if (effortMethod %in% "Plot area/cm2")
              plot.effort$PlotSize <- plot.effort$PlotSize*1e-8 
            
            tmp2 <- merge(tmp2, plot.effort, all.x = TRUE, sort = FALSE)
            if ("countsMeasurement" %in% names(tmp2))
              tmp2$density <- tmp2$countsMeasurement/
                                tmp2[, effort]
            if ("basalAreaMeasurement" %in% names(tmp2))
              tmp2$basalArea <- tmp2$basalAreaMeasurement/
                                  tmp2[, effort]
            if ("biomassMeasurement" %in% names(tmp2))
              tmp2$AGB <- tmp2$biomassMeasurement/
                                  tmp2[, effort]
          } else { effortMethod <- NULL }

          names(tmp2) <- gsub("countsMeasurement", "individuals", 
                              names(tmp2), fixed = TRUE)
          names(tmp2) <- gsub("speciesRichness", "richness", 
                              names(tmp2), fixed = TRUE)
          tmp.final <- 
            tmp2[ , c(names(tmp1), names(tmp2)[names(tmp2) %in% miss.metrics])]
          
          final.metrics <- miss.metrics[miss.metrics %in% names(tmp.final)]
          final.metrics <- final.metrics[final.metrics %in%
                                           names(mapping[['community.data']])]
          final.methods <- .setNames(rep(NA, length(final.metrics)), 
                              paste0(final.metrics, "Method"))
          all.methods <- c(individualsMethod = "Plant count/individuals",
                           densityMethod = "Density/individuals*ha-1",
                           basalAreaMethod = "Basal area/m2*ha-1",
                           AGBMethod = "Above ground biomass/Mg*ha-1", 
                           richnessMethod = "Richness/species", 
                           shannonMethod = "Shannon/nats", 
                           simpsonMethod = "Simpson/0-1",
                           pielouMethod = "Pielou/0-1")
          final.methods <- 
            all.methods[match(names(final.methods), names(all.methods), 
                              nomatch = 0)]
          
          if (is.null(all.data[['community.data']])) {
            all.data[['community.data']] <- tmp.final
            mapping[['community.data']][final.metrics] <- final.metrics
            mapping[['community.data']][names(final.methods)] <- final.methods
          }
          
          if (!identical(miss.metrics, character(0))) {
            prev.meths <- mapping[['community.data']][names(final.methods)]
            prev.meths <- prev.meths[is.na(prev.meths)]
            mapping[['community.data']][names(prev.meths)] <- 
              final.methods[names(prev.meths)]

            new.fields <- gsub("Method$", "", names(prev.meths))
            mapping[['community.data']][new.fields] <- new.fields 
            
            tmp.final1 <- tmp.final[ , c(plot.by, new.fields)]
            prev.data <- all.data[['community.data']]
            prev.data <- merge(prev.data, tmp.final1, 
                                all.x = TRUE, sort = FALSE)
            all.data[['community.data']] <- prev.data
          }  
        }
      }
    } 
  }
  
  output <- list(essential.info = essential.info,
                 mapping = mapping,
                 data = all.data)
  return(output)
}
