# Returns the plotID for a new plot in the data set or the ID of an existing plot with the same name
.newPlotIDByName<-function(target, plotName) {
  if(length(target@plots)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@plots)) {
    if(target@plots[[i]]$plotName==plotName) return(list(id = names(target@plots)[i], new = FALSE))
  }
  return(list(id = as.character(length(target@plots)+1), new = TRUE))
}

# Returns the projectID for a new project in the data set or the ID of an existing project with the same name
.newProjectIDByTitle<-function(target, projectTitle) {
  if(length(target@projects)==0) return(list(id="1", new = TRUE))
  for(i in 1:length(target@projects)) {
    if(target@projects[[i]]$title==projectTitle) return(list(id=names(target@projects)[i], new = FALSE))
  }
  return(list(id = as.character(length(projects@plots)+1), new = TRUE))
}
