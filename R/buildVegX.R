#' Convert Data to a VegX Object
#'
#' @description This function ...
#'
#' @param input.info a named list. The output of the VegX high-level function
#'   function `prepareInputData()`.
#' @param add.site logical. Should the information about the site
#'   characteristics (e.g. slope, aspect, parent material, soil, vegetation
#'   type) be added to the VegX object? Defaults to TRUE.
#' @param add.community logical. Should the aggregated observations at the
#'   stand/community (e.g. density, total biomass) level be added to the VegX
#'   object? Defaults to TRUE.
#' @param add.species logical. Should the aggregated observations at the
#'   species-level (e.g. number of individuals and biomass per species) be added
#'   to the VegX object? Defaults to TRUE.
#' @param add.individual logical. Should the observations at the
#'   individual-level (e.g. stem diameter and height) information about the xxx
#'   be added to the VegX object? Defaults to TRUE.
#' @param add.project logical. Should the information about the project be
#'   added to the VegX object? Defaults to TRUE.
#' @param add.people logical. Should the information about the people involved
#'   in the project be added to the VegX object? Defaults to TRUE.
#' @param add.citation logical. Should the information about the literature
#'   associated with the vegetation data be added to the VegX object? Defaults
#'   to TRUE. (Not implemented yet)
#' @param proj.string a string with the projection attributes of the plot/site
#'   geographical coordinates to be passed to the function `addPlotLocations()`
#'   (see `proj4string` of package `sp`).
#'
#' @return The ...
#'
#' @details To be done ...
#'
#' @return A list with ...
#'
#' @details This function ...
#'
#' @family high-level functions
#'
#' @importFrom plantR formatLoc fixLoc strLoc getLoc
#'
#' @export buildVegX
#'
buildVegX <- function(input.info = NULL,
                      add.site = TRUE,
                      add.community = TRUE,
                      add.species = TRUE,
                      add.individual = TRUE,
                      add.project = TRUE,
                      add.people = TRUE,
                      add.citation = TRUE,
                      proj.string = "+proj=longlat +datum=WGS84") {

  # Check the input info
  if (is.null(input.info))
    stop("Please provide the input information related to the vegetation data")

  if (add.project & is.null(input.info$data[["project.info"]])) {
    add.project <- FALSE
    warning("No data frame with project information. Setting `add.project` to FALSE",
            call. = FALSE)
  }

  if (add.people & is.null(input.info$data[["people.info"]])) {
    add.people <- FALSE
    warning("No data frame with people information. Setting `fill.people` to FALSE",
            call. = FALSE)
  }

  if (add.citation & is.null(input.info$data[["citation.info"]])) {
    add.citation <- FALSE
    warning("No data frame with citation information. Setting `add.citation` to FALSE",
            call. = FALSE)
  }

  # Creating the Veg-X object -----------------------------------------------
  vegx <- newVegX()

  # Add plot information ----------------------------------------------------
  ## project, plot names, dates, plot PI, etc.
  map <- list(projectTitle = "projectTitle",
              plotName = input.info$essential.info[["plotName"]],
              obsStartDate = input.info$essential.info[["censusDateStart"]],
              obsEndDate = input.info$essential.info[["censusDateStop"]])
  if (!is.na(input.info$mapping$plot.obs[["plotID"]]))
    map <- c(map, plotUniqueIdentifier =
               input.info$mapping$plot.obs[["plotID"]])
  if (!is.na(input.info$mapping$plot.obs[["subplotName"]]))
    map <- c(map, subPlotName =
               input.info$mapping$plot.obs[["subplotName"]])
  if (!is.na(input.info$mapping$plot.obs[["dataGenerator"]]))
    map <- c(map, observationParty =
               input.info$mapping$plot.obs[["dataGenerator"]])

  # cat("\033[0;32mAdding general plot information:\033[0m")
  # cat("\033[0;33Adding general plot information:\033[0m")
  # cat("\033[0;34mAdding general plot information:\033[0m")
  message("Adding general plot information:")
  vegx <- addPlotObservations(vegx,
                              input.info$data$plot.obs, mapping = map)

  # Add project information ------------------------------------------------
  if (add.project) {
    if (!is.null(input.info$data$project.info)) {
      info <- input.info$data$project.info
      if (length(info) == 1 & names(info)[1] == "title") {

        if (info$title %in% network_info$GroupCode) {
          project.title <- info$title
          info <- as.data.frame(network_info[network_info$GroupCode
                                             %in% project.title, ])

          if (add.people) {

            # check: add names directly from the people sheets
            # info.people <- as.data.frame(people_info[people_info$GroupCode %in% network, ])

            pessoas <- list(info[, c("ManagerName")],
                            info[, c("CoordinatorName")])
            names(pessoas) <- c("manager", "pointOfContact")
            pessoas <- pessoas[!is.na(pessoas)]
            if (length(pessoas) == 0) pessoas <- NA_character_

          } else {
            pessoas <- NA_character_
          }

          if (add.citation) {
            citacao <- info[, grepl("Reference", colnames(info))]
            citacao <- citacao[, !is.na(citacao)]

            if (dim(citacao)[2] == 0) {
              citacao <- NA_character_
            } else {
              citacao <- paste(citacao, collapse = " | ")
            }
          } else {
            citacao <- NA_character_
          }

          if (is.na(pessoas)) {
            message("Adding general project/network/database information:")
            vegx <- fillProjectInformation(vegx,
                            title = project.title,
                            abstract = ifelse(is.na(info$SmallAbstract), "", info$SmallAbstract),
                            studyAreaDescription = ifelse(is.na(info$StudyAreaDescription), "", info$StudyAreaDescription),
                            designDescription = ifelse(is.na(info$DesignDescription), "", info$DesignDescription),
                            # funding = ifelse(is.na(info$funding), "", info$funding),
                            citationString = ifelse(is.na(citacao), "", citacao))
          } else {
            message("Adding general project/network/database information:")
            vegx <- fillProjectInformation(vegx,
                            title = project.title,
                            personnel = pessoas,
                            abstract = ifelse(is.na(info$SmallAbstract), "", info$SmallAbstract),
                            studyAreaDescription = ifelse(is.na(info$StudyAreaDescription), "", info$StudyAreaDescription),
                            designDescription = ifelse(is.na(info$DesignDescription), "", info$DesignDescription),
                            # funding = ifelse(is.na(info$funding), "", info$funding),
                            citationString = ifelse(is.na(citacao), "", citacao))
          }

        } else {
          # message("Adding general project/network/database information:")
          project.title <- info$title
          vegx <- fillProjectInformation(vegx,
                                         title = project.title)
        }

      } else {
        ids <- match(names(info), names(input.info$mapping$project.info),
                     nomatch = 0)
        names(info) <- names(input.info$mapping$project.info)[ids]
        project.title <- input.info$data$project.info$title

        if (add.people) {
          if (!is.null(input.info$data$people.info)) {
            if ("names" %in% colnames(input.info$data$people.info)) {
              pessoas <- as.list(input.info$data$people.info$names)
              names(pessoas) <- input.info$data$people.info$roles
            } else {
              pessoas <- NA_character_
            }
          } else {
            pessoas <- NA_character_
          }

          if (any(is.na(pessoas))) {
            message("Adding general project/network/database information:")
            vegx <- fillProjectInformation(vegx,
                            title = project.title,
                            abstract = ifelse(is.na(info$abstract), "", info$abstract),
                            studyAreaDescription = ifelse(is.na(info$studyArea), "", info$studyArea),
                            designDescription = ifelse(is.na(info$design), "", info$design),
                            funding = ifelse(is.na(info$funding), "", info$funding),
                            citationString = ifelse(is.na(info$citation), "", info$citation))
          } else {
            message("Adding general project/network/database information:")
            vegx <- fillProjectInformation(vegx,
                            title = project.title,
                            personnel = pessoas,
                            abstract = ifelse(is.na(info$abstract), "", info$abstract),
                            studyAreaDescription = ifelse(is.na(info$studyArea), "", info$studyArea),
                            designDescription = ifelse(is.na(info$design), "", info$design),
                            funding = ifelse(is.na(info$funding), "", info$funding),
                            citationString = ifelse(is.na(info$citation), "", info$citation))
          }

        } else {
          message("Adding general project/network/database information:")
          vegx <- fillProjectInformation(vegx,
                            title = project.title,
                            abstract = ifelse(is.na(info$abstract), "", info$abstract),
                            studyAreaDescription = ifelse(is.na(info$studyArea), "", info$studyArea),
                            designDescription = ifelse(is.na(info$design), "", info$design),
                            funding = ifelse(is.na(info$funding), "", info$funding),
                            citationString = ifelse(is.na(info$citation), "", info$citation))
        }
      }
    }
  }

  # Add people information -------------------------------------------------
  if (add.people) {
    if (dim(showElementTable(vegx, "party"))[1] > 0) {
      if (!is.null(input.info$data$people.info)) {
        info <- input.info$data$people.info
        ids <- match(names(info), names(input.info$mapping$people.info),
                     nomatch = 0)
        names(info) <- names(input.info$mapping$people.info)[ids]
        project.title <- input.info$data$project.info$title
        info.i <- info[info$group %in% project.title, ]

        message("Adding project/network/database people information:")
        for (i in seq_len(dim(info.i)[1])) {
          info.i <- info[i, , drop = FALSE]
          vegx <- fillPartyInformation(vegx,
                            name = info.i$name,
                            type = "individual",
                            address = ifelse(is.na(info.i$ad), "", info.i$ad),
                            phone = ifelse(is.na(info.i$phone), "", info.i$phone),
                            electronicMailAddress = ifelse(is.na(info.i$email), "", info.i$email))
        }
        cat(paste0(" Information added to ", i, " party(ies)/individual(s).\n"))
      } else {

        #### CHECK HERE: GET INFO FROM THE IN-BUILD NETWORK PEOPLE INFORMATION
        # info.people <-
        #   as.data.frame(people_info[people_info$GroupCode %in% network,])

      }
    }
  }

  # Add plot coordinates, elevation and place names -------------------------
  locations <- input.info$mapping$plot.info[!is.na(input.info$mapping$plot.info)]
  if (any(grepl("plotLat|plotLong", names(locations)))) {
    map <- c(plotName = input.info$essential.info[["plotName"]],
             x = locations[["plotLongitude"]],
             y = locations[["plotLatitude"]])
    message("Adding plot spatial coordinates:")
    vegx <- addPlotLocations(vegx, input.info$data$plot.info, map,
                             proj4string = proj.string)
  }

  if (!is.na(input.info$mapping$plot.info[["elevationSite"]])) {
    map <- list(plotName = input.info$essential.info[["plotName"]],
                elevation = input.info$mapping$plot.info[["elevationSite"]])
    message("Adding plot elevation:")
    vegx <- addPlotLocations(vegx, input.info$data$plot.info, map,
                             methods = list(elevation = input.info$mapping$plot.info[["elevationSiteMethod"]])
    )
  }

  #### CHECK HERE: HOW TO ADD MULTIPLE PLACE NAMES FOR THE SAME PLOT SEPARATELY?
  # Putting all place names into a single column
  all.places <- c("continent", "country", "stateProvince", "county",
                  "municipality", "locality", "sublocality")
  if (any(all.places %in% names(locations))) {
    locations1 <- locations[names(locations) %in% all.places]
    plot.locations <-
      input.info$data$plot.info[, c(input.info$essential.info[["plotName"]],
                                    locations1), drop = FALSE]
    names(plot.locations) <-
      c(input.info$essential.info[["plotName"]], names(locations1))
    plot.locations$placeName <-
      apply(plot.locations[ , rev(names(locations1)), drop = FALSE],
            1, paste, collapse = " | ")
    plot.locations$placeType <-
      paste(rev(names(locations1)), collapse = " | ")
    levels.locations <- names(plot.locations)[names(plot.locations) %in%
                                                c("country", "stateProvince",
                                                  "municipality", "locality")]

    loc.check <- try(plantR::formatLoc(plot.locations,
                          loc.levels = levels.locations, scrap = TRUE)$loc.correct,
                  TRUE)
    if (class(loc.check) == "try-error") {
      map <- list(plotName = input.info$essential.info[["plotName"]],
                  placeName = "placeName",
                  placeType = "placeType")
    } else {
      plot.locations$loc.check <- loc.check
      map <- list(plotName = input.info$essential.info[["plotName"]],
                  placeName = "placeName",
                  placeType = "placeType",
                  locationNarrative = "loc.check")
    }
    message("Adding plot place names:")
    vegx <- addPlotLocations(vegx, plot.locations, map)
  }

  map <- list(plotName = input.info$essential.info[["plotName"]])
  if ("authorLocation" %in% names(locations))
    map <- c(map,
             authorLocation = input.info$mapping$plot.info[["authorLocation"]])
  if ("locationNotes" %in% names(locations))
    map <- c(map,
             locationNarrative = input.info$mapping$plot.info[["locationNotes"]])
  if (length(map) > 1)
    vegx <- addPlotLocations(vegx, input.info$data$plot.info, map)


  # Add plot methods -------------------------------------------------------
  map <- list(plotName = input.info$essential.info[["plotName"]])
  methods.ls <- list()
  # subPlotName is only on plot.obs now (put in plot.info as well?)
  # if (!is.na(input.info$mapping$plot.info[["subplotName"]]))
  #   map <- c(map, subPlotName = input.info$mapping$plot.info[["subplotName"]])
  map.names <- names(map)

  methods <- c("samplingEffortMethod", "radiusMethod", "plotLengthMethod",
               "plotWidthMethod")
  methods.eq <- c("area", "radius", "length", "width")
  fields <- gsub("Method", "", methods)

  for (i in seq_along(fields)) {
    field.i <- input.info$mapping$plot.info[[fields[i]]]
    if (!is.na(field.i)) {
      map.i <- list(field.i)
      method.i <- list(input.info$mapping$plot.info[[methods[i]]])
      names(map.i) <- names(method.i) <- methods.eq[i]
      map <- c(map, map.i)
      methods.ls <- c(methods.ls, method.i)
    }
  }

  if (!is.na(input.info$mapping$plot.info[["plotShape"]]))
    map <- c(map, shape = input.info$mapping$plot.info[["plotShape"]])

  if (any(!names(map) %in% map.names)) {
    message("Adding plot methods:")
    vegx <- addPlotGeometries(vegx, input.info$data$plot.info, map, methods.ls)
  }

  # Add invariant site characteristics -------------------------------------
  if (add.site) {
    map <- list(plotName = input.info$essential.info[["plotName"]])
    methods.ls <- list()
    # if (!is.na(input.info$mapping$plot.info[["subplotName"]]))
    #   map <- c(map, subPlotName = input.info$mapping$plot.info[["subplotName"]])
    map.names <- names(map)

    methods <- c("slopeSiteMethod", "aspectSiteMethod", "landformMethod",
                 "parentMaterialMethod")
    methods.eq <- c("slope", "aspect", "landform", "parentMaterial")
    fields <- gsub("Method", "", methods)

    for (i in seq_along(fields)) {
      field.i <- input.info$mapping$plot.info[[fields[i]]]
      if (!is.na(field.i)) {
        map.i <- list(field.i)
        method.i <- list(input.info$mapping$plot.info[[methods[i]]])
        names(map.i) <- names(method.i) <- methods.eq[i]
        map <- c(map, map.i)
        methods.ls <- c(methods.ls, method.i)
      }
    }

    # Any qualitative/ordinal method?
    methods.ls <- .getNonQuantitativeMethods(methods.ls)

    if (any(!names(map) %in% map.names)) {
      message("Adding invariant site characteristics:")
      vegx <-
        addSiteCharacteristics(vegx, input.info$data$plot.info, map, methods.ls)
    }
  }

  # Add aggregate organism observations ------------------------------------
  if (add.species) {
    if (!is.null(input.info$data$species.data)) {
      map <- list(plotName = input.info$essential.info[["plotName.SpeciesData"]],
                  obsStartDate = input.info$essential.info[["censusDateStart"]],
                  organismName = input.info$essential.info[["organismName.SpeciesData"]])
      methods.ls <- list()

      if (!is.na(input.info$mapping$species.data[["subplotNameSample"]]))
        map <- c(map,
                 subPlotName = input.info$mapping$species.data[["subplotNameSample"]])

      if (!is.na(input.info$mapping$species.data[["stratumName"]])) {
        map <- c(map,
                 stratumName = input.info$mapping$species.data[["stratumName"]])
        strata.name <- input.info$mapping$species.data[["stratumNameMethod"]]
        strataDef <- input.info$stratum.info[[strata.name]]
      } else { strataDef <- NULL}
      map.names <- names(map)

      #### CHECK HERE: WHERE TO STORE THE FOREST HEIGHT? ####

      # Adding methods
      map.list <- .getMethods(input.info$mapping$species.data, map, methods.ls)
      map <- map.list$map
      methods.ls <- map.list$method.list

      if (any(!names(map) %in% map.names)) {
        message("Adding aggregate (i.e. species-level) measurements:")
        vegx <- addAggregateOrganismObservations(vegx,
                                                 input.info$data$species.data,
                                                 stratumDefinition = strataDef,
                                                 map, methods.ls)
      }
    }
  }

  # Add individual organism observations -----------------------------------
  if (add.individual) {
    if (!is.null(input.info$data$individual.data)) {
      map <- list(plotName = input.info$essential.info[["plotName.IndividualData"]],
                  obsStartDate = input.info$essential.info[["censusDateStart"]],
                  organismName = input.info$essential.info[["organismName.IndividualData"]])
      methods.ls <- list()
      if (!is.na(input.info$mapping$individual.data[["subplotNameSample"]]))
        map <- c(map, subPlotName =
                   input.info$mapping$individual.data[["subplotNameSample"]])

      if (!is.na(input.info$mapping$individual.data[["fieldTag"]]))
        map <- c(map, individualOrganismLabel =
                   input.info$mapping$individual.data[["fieldTag"]])

      if (!is.na(input.info$mapping$individual.data[["stratumName"]])) {
        map <- c(map,
                 stratumName = input.info$mapping$individual.data[["stratumName"]])
        strata.name <- input.info$mapping$individual.data[["stratumNameMethod"]]
        strataDef <- input.info$stratum.info[[strata.name]]
      } else { strataDef <- NULL}

      map.names <- names(map)

      # Adding methods
      map.list <-
        .getMethods(input.info$mapping$individual.data, map, methods.ls)
      map <- map.list$map
      methods.ls <- map.list$method.list

      # all.fields <- names(input.info$mapping$individual.data)
      # methods <- all.fields[grepl("Method", all.fields)]
      # fields <- gsub("Method", "", methods)
      #
      # for (i in seq_len(length(fields))) {
      #   field.i <- input.info$mapping$individual.data[[fields[i]]]
      #   if (!is.na(field.i)) {
      #     map.i <- list(field.i)
      #     method.i <- list(input.info$mapping$individual.data[[methods[i]]])
      #     names(map.i) <- names(method.i) <- fields[i]
      #     map <- c(map, map.i)
      #     methods.ls <- c(methods.ls, method.i)
      #   }
      # }
      #
      # # Any qualitative/ordinal method?
      # methods.ls <- .getNonQuantitativeMethods(methods.ls)

      if (any(!names(map) %in% map.names)) {
        message("Adding tree (i.e. individual-level) measurements:")
        vegx <- addIndividualOrganismObservations(vegx,
                                                  input.info$data$individual.data,
                                                  stratumDefinition = strataDef,
                                                  map, methods.ls)
      }
    }
  }

  # Add community observations ---------------------------------------------
  if (add.community) {
    if (!is.null(input.info$data$community.data)) {
      map <- list(plotName = input.info$essential.info[["plotName"]],
                  obsStartDate = input.info$essential.info[["censusDateStart"]])
      methods.ls <- list()

      if (!is.na(input.info$mapping$community.data[["stratumName"]])) {
        #### CHECK HERE: VegX currently does not allow for data per stratum in Community Observations
        #### CODES BELOW SHOULD BE REMOVED ONCE FIXED
        drop <- input.info$mapping$community.data[["stratumName"]]
        drop <- "stratumName"
        mapa <- as.character(unlist(map))
        input.info$data$community.data <-
          .aggregateRows(input.info$data$community.data, mapa, drop)

        col.dados <- unique(colnames(input.info$data$community.data))
        col.dados <- unique(c(col.dados,
                              names(input.info$mapping$community.data)[grepl(paste(col.dados, collapse = "|^"),
                                                                             input.info$mapping$community.data,
                                                                             perl = TRUE)]))
        keep_cols <- paste(col.dados, collapse = "|^")
        input.info$mapping$community.data <-
          input.info$mapping$community.data[grepl(keep_cols, input.info$mapping$community.data, perl = TRUE) |
                                              grepl(keep_cols, names(input.info$mapping$community.data), perl = TRUE)]
      #   map <- c(map,
      #            stratumName = input.info$mapping$community.data[["stratumName"]])
      #   strata.name <- input.info$mapping$community.data[["stratumNameMethod"]]
      #   strataDef <- input.info$stratum.info[[strata.name]]
      } else { strataDef <- NULL}
      map.names <- names(map)

      # Adding methods
      map.list <- .getMethods(input.info$mapping$community.data, map, methods.ls)
      map <- map.list$map
      methods.ls <- map.list$method.list

      # all.fields <- names(input.info$mapping$community.data)
      # methods <- all.fields[grepl("Method", all.fields) &
      #                               !grepl("stratumName", all.fields)]
      # fields <- gsub("Method", "", methods)
      # #length(fields) == length(methods)
      #
      # for (i in seq_len(length(fields))) {
      #   field.i <- input.info$mapping$community.data[[fields[i]]]
      #   if (!is.na(field.i)) {
      #     map.i <- list(field.i)
      #     method.i <- list(input.info$mapping$community.data[[methods[i]]])
      #     names(map.i) <- names(method.i) <- fields[i]
      #     map <- c(map, map.i)
      #     methods.ls <- c(methods.ls, method.i)
      #   }
      # }
      #
      # # Any qualitative/ordinal method?
      # methods.ls <- .getNonQuantitativeMethods(methods.ls)

      if (any(!names(map) %in% map.names)) {
        message("Adding community (i.e. stand-level) measurements:")
        vegx <-
          addCommunityObservations(vegx, input.info$data$community.data,
                                   #stratumDefinition = strataDef,
                                   map, methods.ls)
      }
    }
  }

  # Add site observations --------------------------------------------------
  if (add.site) {
    if (!is.null(input.info$data$site.data)) {
      map <- list(plotName = input.info$essential.info[["plotName"]],
                  obsStartDate = input.info$essential.info[["censusDateStart"]])
      methods.ls <- list()

      # Soil measurements
      soil.measures <- input.info$mapping$site.data[grepl("soilMeasurement",
                                                          names(input.info$mapping$site.data))]
      if (any(!is.na(soil.measures))) {
        all.fields <- soil.measures[!is.na(soil.measures)]
        fields <- list(all.fields[!grepl("Method", names(all.fields))])
        methods <- list(all.fields[grepl("Method", names(all.fields))])
        names(fields) <- names(methods) <- letters[seq_len(length(fields))]
        message("Adding soil measurements:")
        vegx <- addSiteObservations(vegx, input.info$data$site.data,
                                    plotObservationMapping = map,
                                    soilMeasurementMapping = fields,
                                    soilMeasurementMethods = methods)
      }

      #### CHECK HERE: do the same for climateMeasurements?  ####

      # Soil type
      if (!is.na(input.info$mapping$site.data[["soilType"]])) {
        message("Adding soil type:")
        field <- list(soilType = input.info$mapping$site.data[["soilType"]])
        method <- list(soilType = input.info$mapping$site.data[["soilTypeMethod"]])
        method[["soilType"]] <- qualitative_methods[[unlist(method)]]
        vegx <- addSiteObservations(vegx, input.info$data$site.data,
                                    plotObservationMapping = map,
                                    soilMeasurementMapping = field,
                                    soilMeasurementMethods = method)
      }

      #### ADD HERE: vegetationType  ####


    }
  }

  # Add stratum observations -----------------------------------------------
  # if (add.stratum) {}
  ## TO BE IMPLEMENTED
  #### CHECK TUTORIAL ####

  # Add surface cover observations -----------------------------------------
  # if (add.surface) {}
  ## TO BE IMPLEMENTED
  #### CHECK TUTORIAL ####

  # Returning the VegX object ----------------------------------------------
  return(vegx)
}
