library(readxl)
quantitative_methods <- as.data.frame(read_xlsx("data-raw/PredefinedMethods.xlsx", sheet = "quantitative"), stringsAsFactors=FALSE)

devtools::use_data(quantitative_methods, internal = FALSE, overwrite = TRUE)
rm(quantitative_methods)

#REBUILD!!