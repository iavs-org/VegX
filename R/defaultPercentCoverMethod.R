defaultPercentCoverMethod<-function() {
   attributes = list(
     list(type="quantitative",
          units = "%",
          upperBound = 100,
          lowerBound = 0)
   )
   return(new("VegXMethod",
              name = "PercentCover",
              description = "Quantitative percent cover",
              attributes = attributes))
}
