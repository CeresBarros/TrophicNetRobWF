## ------------------------------------------------------------------
## EUROPEAN TROPHIC WEBS
## SPECIES IUCN STATUS
## SUPPORTING SCRIPTS - For workflow demo
##
## Ceres: Aug 2024
## ------------------------------------------------------------------------

## this script aims at obtaining the IUCN status of each species in the metawed
## partly based on Louise O'Connors scrip, but more generic and attempts to find synonyms

output.dir <- "Spp_traits_habs/IUCN/"
dir.create(output.dir, recursive = TRUE)

## --------------------------------------------------
## OUR SPECIES LIST
## use spp x habitat table
sppIDs <-  prepInputs(url = "https://zenodo.org/api/records/13345395/files-archive",
                      archive = "13345395.zip",
                      targetFile = "BARM_sppcodes.csv",
                      destinationPath = "data/",
                      fun = read.csv(targetFile, header = TRUE, stringsAsFactors = FALSE))
sppIDs <- as.data.table(sppIDs)

## --------------------------------------------------
## GET IUCN DATA
URL <- "https://www.eea.europa.eu/data-and-maps/data/european-red-lists-7/european-red-list/european-red-list-csv-files/at_download/file"
speciesListIUCN <- prepInputs(targetFile = "European_Red_List_2017_December.csv",
                              archive = "European_Red_List_2017_December_csv.zip",
                              destinationPath = "data/",
                              url = URL,
                              fun = "read.csv")
speciesListIUCN <- as.data.table(speciesListIUCN)
if (any(grepl("?..", names(speciesListIUCN), fixed = TRUE)))
  setnames(speciesListIUCN, names(speciesListIUCN),
           sub("?..", "", names(speciesListIUCN), fixed = TRUE))

cols <- c("taxonomicRankClass", "taxonomicRankOrder", "taxonomicRankFamily",
          "taxonomicRankGenus", "taxonomicRankSpecies", "taxonomicRankSubspecies",
          "scientificName", "endemicToEurope", "europeanRegionalRedListCategory",
          "endemicToEu", "euRegionalRedListCategory")

speciesListIUCN <- speciesListIUCN[, ..cols]

URL <- "https://www.eea.europa.eu/data-and-maps/data/european-red-lists-7/tables-metadata/tables-metadata-csv-files/at_download/file"
IUCNdefinitions <- prepInputs(targetFile = "European_Red_List_2017_December_TableDefinitions.csv",
                              destinationPath = "data/",
                              url = URL,
                              fun = "read.csv")
IUCNdefinitions <- as.data.table(IUCNdefinitions)
if (any(grepl("?..", names(IUCNdefinitions), fixed = TRUE)))
  setnames(IUCNdefinitions, names(IUCNdefinitions),
           sub("?..", "", names(IUCNdefinitions), fixed = TRUE))

## subset data
sppIUCNstatus <- speciesListIUCN[taxonomicRankClass %in% c("AMPHIBIA", "AVES", "REPTILIA", "MAMMALIA")]

## checks and corrections
IUCNlevels <- IUCNdefinitions[Field.name == "euRegionalRedListCategory", Codes]
IUCNlevels <- unlist(strsplit(IUCNlevels, "; "))
IUCNlevels <- sub("([[:alpha:]]{2}) -.*", "\\1", IUCNlevels)

sppIUCNstatus[, euRegionalRedListCategory := as.character(euRegionalRedListCategory)]
sppIUCNstatus[is.na(euRegionalRedListCategory), euRegionalRedListCategory := "NA"]

differentLevels <- setdiff(sppIUCNstatus$euRegionalRedListCategory, IUCNlevels)
if (length(differentLevels)) {
  message("These levels in `euRegionalRedListCategory` are not in list of accepted codes:")
  message(paste0(differentLevels, collapse = ", "))
}

IUCNlevels <- IUCNdefinitions[Field.name == "europeanRegionalRedListCategory", Codes]
IUCNlevels <- unlist(strsplit(IUCNlevels, "; "))
IUCNlevels <- sub("([[:alpha:]]{2}) -.*", "\\1", IUCNlevels)

sppIUCNstatus[, europeanRegionalRedListCategory := as.character(europeanRegionalRedListCategory)]
sppIUCNstatus[is.na(europeanRegionalRedListCategory), europeanRegionalRedListCategory := "NA"]

differentLevels <- setdiff(sppIUCNstatus$europeanRegionalRedListCategory, IUCNlevels)
if (length(differentLevels)) {
  message("These levels in `europeanRegionalRedListCategory` are not in list of accepted codes:")
  message(paste0(differentLevels, collapse = ", "))
}

## known issues with IUCN levels
sppIUCNstatus[, euRegionalRedListCategory := sub("/PE", "", euRegionalRedListCategory, fixed = TRUE)]
sppIUCNstatus[, euRegionalRedListCategory := sub("lc", "LC", euRegionalRedListCategory)]

## fix species names:
sppIUCNstatus[taxonomicRankClass == "MAMMALIA", Spp := gsub(" ", "_", scientificName)]
sppIUCNstatus[taxonomicRankClass != "MAMMALIA", Spp := paste(taxonomicRankGenus, taxonomicRankSpecies, sep = "_")]

## to match sppIDs$Spp:
sppIUCNstatus[, Spp := gsub(x = Spp, pattern = "Hierophis_cypriensis", replacement = "Dolichophis_cypriensis")]
sppIUCNstatus[, Spp := gsub(x = Spp, pattern = "Clanga_clanga", replacement = "Aquila_clanga")]
sppIUCNstatus[, Spp := gsub(x = Spp, pattern = "Mareca_penelope", replacement = "Anas_penelope")]
sppIUCNstatus[, Spp := gsub(x = Spp, pattern = "Spatula_querquedula", replacement = "Anas_querquedula")]
sppIUCNstatus[, Spp := gsub(x = Spp, pattern = "Vanellus_spinosus", replacement = "Hoplopterus_spinosus")]
sppIUCNstatus[, Spp := gsub(x = Spp, pattern = "Calidris_pugnax", replacement = "Philomachus_pugnax")]
sppIUCNstatus[, Spp := gsub(x = Spp, pattern = "Turnix_sylvaticus", replacement = "Turnix_sylvatica")]
sppIUCNstatus[, Spp := gsub(x = Spp, pattern = "Hydrobates_leucorhous", replacement = "Oceanodroma_leucorhoa")]
sppIUCNstatus[, Spp := gsub(x = Spp, pattern = "Bubo_scandiacus", replacement = "Nyctea_scandiaca")]

setdiff(sppIDs$Spp, sppIUCNstatus$Spp)

## add species codes keeping all species in our list
sppIUCNstatus <- sppIUCNstatus[sppIDs, on = "Spp"]
sppIUCNstatus$ID <- as.character(sppIUCNstatus$ID)

## for species missing a status use "europe" region from rredlist
## this region is NOT a simple subset of "global", see e.g.:
if (FALSE) {
  rl_search("Lyciasalamandra luschani", region = "global")$result["category"]
  rl_search("Lyciasalamandra luschani", region = "europe")$result["category"]
  rl_search("Lyciasalamandra luschani", region = "mediterranean")$result["category"]
}

if (any(is.na(sppIUCNstatus$europeanRegionalRedListCategory))) {
  message("Found missing IUCN status for:")
  message(paste(sum(is.na(sppIUCNstatus$europeanRegionalRedListCategory)), "species"))
  message("Trying to fill in with `rredlist::rl_search(..., region = 'europe')`")

  missingSpp  <- sppIUCNstatus[is.na(europeanRegionalRedListCategory), Spp]
  missingSpp <- sub("_", " ", missingSpp)
  missingStatus <- Cache(findIUCNStatus,
                         species = missingSpp,
                         region = "europe")
  missingStatus <- data.table(Spp = missingSpp, status = missingStatus)

  ## find synonyms for not found
  missingSpp <- missingStatus[status %in% c("NoAssessment", NA), Spp]
  missingStatus2 <- Cache(findIUCNAcceptedName,
                          species = missingSpp)
  missingStatus2 <- data.table(Spp = names(unlist(missingStatus2)), Synonyms = unlist(missingStatus2))
  missingStatus2[, Spp := sub("(.*)[[:digit:]]{1}$", "\\1", Spp)]
  missingStatus2 <- unique(missingStatus2)
  missingStatus2 <- missingStatus2[Spp != Synonyms]

  ## try to find status for synonyms
  missingStatus2$status <- Cache(findIUCNStatus,
                                 species = missingStatus2$Synonyms,
                                 region = "europe")
  missingStatus2 <- unique(missingStatus2[, .(Spp, status)])
  if (any(duplicated(missingStatus2)))
    stop("Duplicated species!")

  missingStatus <- missingStatus[!Spp %in% missingStatus2$Spp]
  missingStatus <- rbind(missingStatus, missingStatus2)

  missingStatus[, Spp := sub(" ", "_", Spp)]
  setnames(missingStatus, "status", "europeanRegionalRedListCategory")


  sppIUCNstatus1 <- sppIUCNstatus[!Spp %in% missingStatus$Spp]
  sppIUCNstatus2 <- sppIUCNstatus[Spp %in% missingStatus$Spp]
  sppIUCNstatus2[, europeanRegionalRedListCategory := NULL]
  sppIUCNstatus2 <- missingStatus[sppIUCNstatus2, on = "Spp"]
  sppIUCNstatus <- rbind(sppIUCNstatus1, sppIUCNstatus2, fill = TRUE)
}

cols <- c("ID", "Spp", "endemicToEurope", "europeanRegionalRedListCategory", "endemicToEu", "euRegionalRedListCategory")
sppIUCNstatus <- sppIUCNstatus[, ..cols]

if (any(is.na(sppIUCNstatus$ID)))
  stop("Bug!")

## save:
write.table(sppIUCNstatus, file = file.path(output.dir, "BARM_IUCNstatus.txt"))
