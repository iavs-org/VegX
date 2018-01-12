defaultPercentCoverMethod<-function() {
   attributes = list(
     list(type="quantitative",
          units = "%",
          upperBound = 100,
          lowerBound = 0)
   )
   return(new("VegXMethod",
              name = "Percent cover",
              description = "Quantitative plant percent cover",
              attributes = attributes))
}
