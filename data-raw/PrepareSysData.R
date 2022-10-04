#### GENERATING THE VegX SUPPORTING OBJECTS ####

if (!require("googlesheets4"))
  install.packages("googlesheets4")

if (!require("usethis"))
  install.packages("usethis")

if (!require("readxl"))
  install.packages("readxl")

if (!require("xlsx"))
  install.packages("xlsx")


# REFERENCE AND PREDEFINED FIELD MAPS ---------------------------------------
# Downloading and saving the predefined equivalencies
## Set authentication token to be stored in a folder called `.secrets`
options(gargle_oauth_cache = ".secrets")
## Authenticate manually
googlesheets4::gs4_auth()

## If successful, the previous step stores a token file. Check that a file that stores a token exists with:
# googlesheets4::gs4_auth(cache = ".secrets", 
#                         email = "raflima@usp.br")
link <- "https://docs.google.com/spreadsheets/d/1GKRClW7DNeRLK-CBnHAkW0M191845mKuvNQEfPYAkGo/edit?usp=sharing"
predefined_map <- as.data.frame(googlesheets4::read_sheet(link))

# Editing the downloaded file
predefined_map <- predefined_map[!grepl("^TO|^DONT", predefined_map$Group) , ]
predefined_map <- predefined_map[!is.na(predefined_map$Group) , ]
predefined_map <- predefined_map[!is.na(predefined_map$Field) , ]


#### CHECK HERE ####
## SAVING THE DOWNLOADED FILES INTO THE DATA-RAW FOLDER

## Getting the list of available network/databases
tmp <- colnames(predefined_map)
predefined_fields <- c("Order", "FieldID", "Group", "Field", "Documentation", 
                       "Type", "Example", "VegX_schema", "Status", "Use", "Notes")
tmp1 <- tmp[!tmp %in% predefined_fields]
tmp2 <- predefined_map[, tmp1]
tmp2 <- tmp2[, !apply(tmp2, 2, function(x) all(is.na(x)))]
available_projects <- sort(colnames(tmp2))

# Getting the reference (empty) mapping 
tables <- unique(predefined_map$Group)
reference_map <- vector("list", length(tables))
names(reference_map) <- tables
tmp <- table(predefined_map$Group)
numb.fields <- tmp[match(tables, names(tmp))]

for (i in seq_len(length(tables))) {
  ids <- predefined_map$Group %in% tables[i]
  reference_map[[i]] <- setNames(rep(NA_character_, numb.fields[i]), 
                                 predefined_map[["Field"]][ids])
}

# Editting the objects map before saving
cols <- c("Order", "Group", "Field", "Use", available_projects)
predefined_map <- predefined_map[, colnames(predefined_map) %in% cols]
predefined_map[predefined_map$Field %in% "projectTitle", available_projects] <-
  colnames(predefined_map[predefined_map$Field %in% "projectTitle", available_projects])
xlsx::write.xlsx(predefined_map, file = "./data-raw/files-raw/temp_map.xlsx",
                 row.names = FALSE)
objects <- c("reference_map", 
             "predefined_fields", 
             "available_projects",
             "predefined_map")
supporting_info <- vector("list", length(objects))
supporting_info <- lapply(objects, get)
names(supporting_info) <- objects 

# NETWORK INFORMATION ------------------------------------------------------
link <- "https://docs.google.com/spreadsheets/d/1mqd2RzgQh6_6ytbVFUO3epUUtXxdcknmcue78SnF2ZU/edit#gid=1426544088"
network_info <- as.data.frame(googlesheets4::read_sheet(link))

network_info <- 
  network_info[!apply(network_info[,-1], 1, function(x) all(is.na(x))), ]
network_info <- 
  network_info[, c(TRUE, !apply(network_info[,-1], 2, function(x) all(is.na(x))))]
network_info <- network_info[network_info$GroupCode %in% available_projects,]

# QUANTITATIVE SCALE METHODS -----------------------------------------------
# predefined.path <- "data-raw/PredefinedMethods.xlsx"
predefined.path <- "data-raw/files-raw/PredefinedMethods.xlsx"
quantitative_methods <-
  as.data.frame(readxl::read_xlsx(predefined.path, sheet = "quantitative"),
                stringsAsFactors = FALSE)
quantitative_methods$lowerLimit <- as.numeric(quantitative_methods$lowerLimit)
quantitative_methods$upperLimit <- as.numeric(quantitative_methods$upperLimit)

# QUALITATIVE SCALE METHODS -----------------------------------------------
qualitative_names <-
  as.data.frame(readxl::read_xlsx(predefined.path, sheet = "qualitative"),
                stringsAsFactors = FALSE)
qualitative_desc <-
  as.data.frame(readxl::read_xlsx(predefined.path, sheet = "qualitative_description"),
                stringsAsFactors = FALSE)

methods <- intersect(qualitative_names$name, qualitative_desc$name)
qualitative_methods <- vector("list", length(methods))
names(qualitative_methods) <- methods
for(i in seq_along(methods)) {
  method <- names(qualitative_methods)[i]
  meth <- qualitative_names[qualitative_names$name %in% method, ]
  desc <- qualitative_desc[qualitative_desc$name %in% method, ]
  obj <- VegX::defineQualitativeScaleMethod(
    name = meth$name,
    description = meth$description,
    subject = meth$subject,
    citationString = ifelse(is.na(meth$citationString), "", meth$citationString), 
    DOI = ifelse(is.na(meth$DOI), "", meth$DOI), 
    codes = desc$codes,
    definitions = desc$definitions)
  qualitative_methods[[i]] <- obj
}


# ORDINAL SCALE METHODS -----------------------------------------------------
ordinal_names <-
  as.data.frame(readxl::read_xlsx(predefined.path, sheet = "ordinal"),
                stringsAsFactors = FALSE)
ordinal_desc <-
  as.data.frame(readxl::read_xlsx(predefined.path, sheet = "ordinal_description"),
                stringsAsFactors = FALSE)
ordinal_desc$breaks <- as.numeric(ordinal_desc$breaks)
ordinal_desc$midPoints <- as.numeric(ordinal_desc$midPoints)

methods <- intersect(ordinal_names$name, ordinal_desc$name)
ordinal_methods <- vector("list", length(methods))
names(ordinal_methods) <- methods
for(i in seq_along(methods)) {
  method <- names(ordinal_methods)[i]
  meth <- ordinal_names[ordinal_names$name %in% method, ]
  desc <- ordinal_desc[ordinal_desc$name %in% method, ]
  
  codes <- desc$codes[!is.na(desc$codes)]
  defs <- desc$definitions[!is.na(desc$definitions)]  
  quant.codes <- desc$codes[!is.na(desc$codes) & 
                              desc$quantifiableCodes %in% "yes"]
  brk.values <- desc$breaks[!is.na(desc$breaks)]
  mids <- desc$midPoints[!is.na(as.numeric(desc$midPoints))]  
  
  obj <- VegX::defineOrdinalScaleMethod(
    name = meth$name,
    description = meth$description,
    subject = meth$subject,
    citationString = ifelse(is.na(meth$citationString), "", meth$citationString), 
    DOI = ifelse(is.na(meth$DOI), "", meth$DOI), 
    codes = codes,
    definitions = defs,
    quantifiableCodes = quant.codes,
    breaks = brk.values,
    midPoints = mids)
  ordinal_methods[[i]] <- obj
}

# THE LIST OF ALL AVAILABLE METHODS --------------------------------------
available_methods <- c(quantitative_methods$name,
                       names(qualitative_methods),
                       names(ordinal_methods))
supporting_info <- append(supporting_info, 
                          list(available_methods = available_methods))

# SAVING THE SYSDATA -----------------------------------------------------
#### CHECK HERE: SAVE ALL AS INTERNAL (R/sysdata.rda), EXTERNAL (data/file1.rda, data/file2.rda, etc) OR BOTH? DOING BOTH NOW ####
usethis::use_data(supporting_info,
                  qualitative_methods,
                  ordinal_methods,
                  network_info,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz")
usethis::use_data(quantitative_methods,
                  overwrite = TRUE,
                  internal = FALSE,
                  compress = "xz")
rm(supporting_info, quantitative_methods, qualitative_methods, ordinal_methods,
   network_info)

