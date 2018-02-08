#' Fills the information for a given party
#'
#' @param target The initial object of class \code{\linkS4class{VegX}} to be modified.
#' @param name A string with the full name of the person/organization/position being described.
#' @param address A string that identifies the party type: "individual", "organization" or "position".
#' @param phone A string summarizing the aims and findings of the project.
#' @param electronicMailAddress A string with information about funding agencies.
#' @param onlineURL A string describing the study area succinctly.
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
#' x = fillPartyInformation(newVegX(), name = "Susan K. Wiser")
#'
fillPartyInformation<-function(target, name, type = "individual",
                               address = "", phone = "",
                               electronicMailAddress = "",
                               onlineURL = "") {

  nprid = .newPartyIDByName(target, name) # Get the new project ID (internal code)
  partyID = nprid$id
  if(nprid$new) party = list(name = name, partyType = "individual")
  else party = target@parties[[partyID]]


  if(address!="") party$address = address
  if(phone!="") party$phone = phone
  if(electronicMailAddress!="") party$electronicMailAddress = electronicMailAddress
  if(onlineURL!="") party$onlineURL = onlineURL

  target@parties[[partyID]] = party
  return(target)
}
