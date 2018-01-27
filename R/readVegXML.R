readVegXML<-function(file) {
  target = newVegX()
  veg=xmlRoot(xmlTreeParse(file, useInternalNodes = T))
  
  #read projects
  .readProject.2.0.0 = function(x) {
    project = list()
    project$title = xmlValue(x[["title"]])
    return(project)
  }
  target@projects = xmlApply(veg[["projects"]], .readProject.2.0.0)
  #read plots
  .readPlot.2.0.0 = function(x) {
    plot = list()
    plot$plotName = xmlValue(x[["plotName"]])
    return(plot)
  }
  target@plots = xmlApply(veg[["plots"]], .readPlot.2.0.0)
  rm(veg)
  return(target)
}