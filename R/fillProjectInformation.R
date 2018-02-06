#' Fills the information for a given research project
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified.
#' @param title A string with the title of the project. It can match one of the existing project or a new one will be defined.
#' @param personnel A named list where element names are roles and values are party names (e.g. \code{list(contributor = "John Smith")}).
#' @param abstract A string summarizing the aims and findings of the project.
#' @param funding A string with information about funding agencies.
#' @param studyAreaDescription A string describing the physical area associated with the research project, potentially including coverage, climate, geology, distrubances, etc..
#' @param designDescription A string describing the overall plot placement design.
#' @param verbose A boolean flag to indicate console output of the data integration process.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family fill functions
#'
#' @examples
#'
#' x = fillProjectInformation(newVegX(), "MOKIHINUI HYDRO PROPOSAL - LOWER GORGE 2011",
#'                            personnel = c(contributor = "Susan K. Wiser"),
#'                            abstract = paste("Characterise the forest and riparian vegetation in the lower Mokihinui gorge,",
#'                                        "and compare this with the vegetation in (a) North Branch gorge of Mokihinui",
#'                                        "and (b) Karamea catchment."),
#'                            studyAreaDescription = "Mokihinui and Karamea catchments. Forest riparian habitat.")
#'
fillProjectInformation<-function(target, title,
                                 personnel = list(),
                                 abstract = "", funding = "",
                                 studyAreaDescription = "",
                                 designDescription = "",
                                 verbose = TRUE) {

  nprid = .newProjectIDByTitle(target, title) # Get the new project ID (internal code)
  projectID = nprid$id
  if(nprid$new) project = list("title" = title)
  else project = target@projects[[projectID]]

  if(length(personnel)>0) {
    orinparties = length(target@parties)
    project$personnel = list()
    for(i in 1:length(personnel)) {
      role = names(personnel)[i]
      npid = .newPartyIDByName(target, personnel[[i]])
      partyID = npid$id
      if(npid$new) target@parties[[partyID]] = list(name = personnel[[i]],
                                                    partyType = "individual")
      project$personnel[[role]] = partyID
    }
    finnparties = length(target@parties)
    if(verbose) {
      if(finnparties > orinparties) cat(paste0(" " , finnparties-orinparties, " new partie(s) were added to the document as individuals. Consider providing party information.\n"))
    }
  }

  if(abstract!="") project$abstract = abstract
  if(funding!="") project$funding = funding
  if(studyAreaDescription!="") project$studyAreaDescription = studyAreaDescription
  if(designDescription!="") project$designDescription = designDescription

  target@projects[[projectID]] = project
  return(target)
}
