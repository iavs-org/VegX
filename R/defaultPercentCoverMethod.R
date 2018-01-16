defaultPercentCoverMethod<-function() {
   attributes = list(
     list(type="quantitative",
          unit = "%",
          upperBound = 100,
          lowerBound = 0)
   )
   names(attributes) = 1
   return(new("VegXMethod",
              name = "Percent cover",
              description = "Quantitative plant percent cover",
              attributeClass = "cover",
              attributeType = "quantitative",
              attributes = attributes))
}
defaultPlantCountMethod<-function() {
  attributes = list(
    list(type="quantitative",
         unit = "individuals",
         lowerBound = 0,
         upperBound = Inf)
  )
  names(attributes) = 1
  return(new("VegXMethod",
             name = "Individual counts",
             description = "Number of individuals in the (sub)plot",
             attributeClass = "count",
             attributeType = "quantitative",
             attributes = attributes))
}
coverScale<-function(name = "Braun-Blanquet", description = "Five-level Braun-Blanquet cover scale",
                     breaks = c(0,5,25,50,75,100),
                     midPoints = c(2.5,17.5, 37.5, 62.5, 87.5),
                     values = 1:5) {
   nvals = length(values)
   attributes = vector("list", nvals)
   for(i in 1:nvals) attributes[[i]] = list(type = "ordinal", code = values[i], order = i, lowerLimit = breaks[i],
                                            upperLimit = breaks[i+1], midPoint = midPoints[i])
   names(attributes) = 1:nvals
   return(new("VegXMethod",
              name = name,
              description = description,
              attributeClass = "cover",
              attributeType = "ordinal",
              attributes = attributes))
}
