#' Fills the information for a given research project
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified.
#' @param title A string with the title of the project. It can match one of the existing project or a new one will be defined.
#' @param personnel A character vector with party names.
#' @param abstract A string summarizing the aims and findings of the project.
#' @param funding A string with information about funding agencies.
#' @param studyAreaDescription A string describing the study area succinctly.
#' @param designDescription A string describing the overall plot placement design.
#'
#' @return The modified object of class \code{\linkS4class{VegX}}.
#' @export
#'
#' @references Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK (2011). Veg-X - an exchange standard for plot-based vegetation data
#'
#' @family fill functions
#'
#' @examples
fillProjectInformation<-function(target, title,
                                 personnel = c(),
                                 abstract = "", funding = "",
                                 studyAreaDescription = "",
                                 designDescription = "") {

  nprid = .newProjectIDByTitle(target, title) # Get the new project ID (internal code)
  projectID = nprid$id
  if(nprid$new) project = list("title" = title)
  else project = target@projects[[projectID]]

  if(length(personnel)>0) {
    if("personnel" %in% names(project)) project$personnel = list()
    for(i in 1:length(personnel)) project$personnel[[i]] = as.character(personnel[i])
  }
  if(abstract!="") project$abstract = abstract
  if(funding!="") project$funding = funding
  if(studyAreaDescription!="") project$studyAreaDescription = studyAreaDescription
  if(designDescription!="") project$designDescription = designDescription

  target@projects[[projectID]] = project
  return(target)
}
