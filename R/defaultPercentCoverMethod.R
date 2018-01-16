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
defaultPlantFrequencyMethod<-function() {
  attributes = list(
    list(type="quantitative",
         unit = "%",
         lowerBound = 0,
         upperBound = 100)
  )
  names(attributes) = 1
  return(new("VegXMethod",
             name = "Frequency of occurrence",
             description = "Frequency of occurrence in subunits of the sampled area",
             attributeClass = "frequency",
             attributeType = "quantitative",
             attributes = attributes))
}
