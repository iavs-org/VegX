# Returns the projectID for a new project in the data set or the ID of an existing project with the same name
.newProjectIDByTitle<-function(target, projectTitle) {
  if(length(target@projects)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@projects)) {
    if(target@projects[[i]]$title==projectTitle) return(list(id=names(target@projects)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@projects)+1), new = TRUE))
}
# Returns the plotID for a new plot in the data set or the ID of an existing plot with the same name
.newPlotIDByName<-function(target, plotName) {
  if(length(target@plots)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@plots)) {
    if(target@plots[[i]]$plotName==plotName) return(list(id = names(target@plots)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@plots)+1), new = TRUE))
}
# Returns the plotID for a new plot in the data set or the ID of an existing plot with the same name
.newMethodIDByName<-function(target, methodName) {
  if(length(target@methods)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@methods)) {
    if(target@methods[[i]]$name==methodName) return(list(id = names(target@methods)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@methods)+1), new = TRUE))
}
# Returns the ID for a new taxon name usage concept in the data set or the ID of an existing taxon name usage concept with the same name
.newTaxonNameUsageConceptIDByName<-function(target, authorName) {
  if(length(target@taxonNameUsageConcepts)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@taxonNameUsageConcepts)) {
    if(target@taxonNameUsageConcepts[[i]]$authorName==authorName) return(list(id = names(target@taxonNameUsageConcepts)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@taxonNameUsageConcepts)+1), new = TRUE))
}
