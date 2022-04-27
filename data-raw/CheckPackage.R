# # CHECK PACKAGE DEPENDENCIES ----------------------------------------------
# ## which are the R base packages
# all.pckg <- installed.packages()
# base.pckg <- as.data.frame(all.pckg[ all.pckg[,"Priority"] %in%
#                                        c("base","recommended"), c("Package", "Priority")])
# base.pckgs <- base.pckg[base.pckg$Priority %in% "base","Package"]
# 
# # ##Creating the list of plantR dependencies from package DESCRIPTION
# plantr.description <- desc::desc(package = "VegX")$get_deps()
# import <- plantr.description[plantr.description$type %in% "Imports", "package"]
# suggest <- plantr.description[plantr.description$type %in% "Suggests", "package"]
# 
# # Getting the list of non-base dependencies
# deps.import <- vector("list", length(import))
# names(deps.import) <- import
# deps.suggest <- vector("list", length(suggest))
# names(deps.suggest) <- suggest
# 
# for(i in 1:length(import)) {
#   lista <- sort(tools::package_dependencies(package= import[i], recursive=TRUE)[[import[i]]])
#   lista <- lista[!lista %in% base.pckgs]
#   deps.import[[i]] <- lista
# }
# 
# for(i in 1:length(suggest)) {
#   lista <- 
#     sort(tools::package_dependencies(package= suggest[i], recursive=TRUE)[[suggest[i]]])
#   lista <- lista[!lista %in% base.pckgs]
#   deps.suggest[[i]] <- lista
# }
# 
# ## Inpsecting the package direct and recursive dependencies
# deps.import
# deps.suggest
# 
# ## Checking the minimum R versions necessary for each dependency
# ap <- available.packages()
# any(!import %in% rownames(ap))
# ap[ap[,"Package"] %in% import, "Depends"]
# 
# 
# # LOCATING FUNCTIONS FROM OTHER PACKAGES ----------------------------------
# ## Localizando em quais funções estão as dependências a remover
# library(mvbutils)
# library(VegX)
# foodweb( find.funs("package:VegX"), prune="UUIDgenerate")
# foodweb( find.funs("package:VegX"), prune="nslookup")
# foodweb( find.funs("package:VegX"), prune="newXMLDoc")
# foodweb( find.funs("package:VegX"), prune="newXMLNode")
# foodweb( find.funs("package:VegX"), prune="xmlTreeParse")
# foodweb( find.funs("package:VegX"), prune="xmlValue")
# foodweb( find.funs("package:VegX"), prune="xmlAttrs")
# foodweb( find.funs("package:VegX"), prune="saveXML")
# foodweb( find.funs("package:VegX"), prune="xmlApply")
# foodweb( find.funs("package:VegX"), prune="xmlRoot")
# foodweb( find.funs("package:VegX"), prune="download_xml")
# foodweb( find.funs("package:VegX"), prune="read_xml")
# foodweb( find.funs("package:VegX"), prune="xml_children")
# foodweb( find.funs("package:VegX"), prune="xml_child")
# foodweb( find.funs("package:VegX"), prune="xml_text")
# foodweb( find.funs("package:VegX"), prune="xml_validate")
# foodweb( find.funs("package:VegX"), prune="xml_attr")
# foodweb( find.funs("package:VegX"), prune="SpatialPoints")
# foodweb( find.funs("package:VegX"), prune="spTransform")
# foodweb( find.funs("package:VegX"), prune="CRS")
# 
# ## List of licenses per dependencies (ideally, should use the same)
# licensas <- vector("list", length(import))
# for (i in seq_along(import)) {
#   licensas[[i]] <- packageDescription(import[i], fields = "License")
# }
# cbind(import, do.call(rbind, licensas))
# 
# 
# # CHECKS, TESTS AND SPELLING ----------------------------------------------
# 
# #Checking
devtools::run_examples()
# devtools::check()
# 
# #Testing
# #ROpenSci: Test coverage below 75% will likely require additional tests or
# #explanation before being sent for review.
# devtools::test()
# 
# #Spelling
# spelling::update_wordlist(pkg = ".", vignettes = FALSE, confirm = TRUE)
# spelling::get_wordlist(pkg = ".")
# toto <- spelling::spell_check_package(vignettes = FALSE)
# toto <- spelling::spell_check_package(vignettes = TRUE)
# 
# #Checking by word
# toto1 <- lengths(toto$found)
# names(toto1) <- toto$word
# sort(toto1)
# 
# palavras <- c("colunm", "containig", "definiton", "Donwload", "rellevant",
#               "indvidual", "landcareresearch", "nomenclatural",
#               "accomodate", "availble", "characteirstics", "depeding",
#               "elliposid", "encompasing", "nomenclarure", "distrubances")
# palavras <- c("Mokihinui")
# toto2 <- toto$found[match(palavras, toto$word)]
# names(toto2) <- palavras
# toto2
# 
# #Checking by file
# # tail(sort(table(unlist(toto$found))), 10)
# spelling::spell_check_files("./vignettes/plantr_introduction.Rmd",
#                             lang = "en_US")
# spelling::spell_check_files("./vignettes/articles/plantr_tutorial.Rmd",
#                             lang = "en_US")
# spelling::spell_check_files("./vignettes/articles/atualiza_duplicatas.Rmd",
#                             lang = "pt_BR")
# 
# #Good practices
# goodpractice::gp()
# goodpractice::gp(".", checks = c("rcmdcheck_tests_pass"))
# 
# # In 03/10/2021: 57% of code lines were covered by test cases
# # Larger functions pending tests: checkList, formatDwc, prepDup,
# #readData, rgibif2, rspeciesLink, saveData, summaryData, summaryFlags
# 
# ## Checking non-ascii characters in the code
# arquivos <- list.files("R/", full.names = TRUE)
# arquivos <- arquivos[!arquivos %in% "R/sysdata.rda"]
# for (i in seq_along(arquivos)) {
#   cat(arquivos[i], "\n")
#   non.ascii <- tools::showNonASCIIfile(arquivos[i])
# }




