## --------------------------------------------------
## PREY DISTRIBUTIONS PER SPP ACROSS BASELINE NETWORKS
## 10KM
## SUPPORTING SCRIPTS - For workflow demo
##
## Ceres Aug 2024
## --------------------------------------------------

## getting the 10%, 25%, 75%, and 90% quantile valaues and median of the number of prey for each species across all baselin webs
## to be used as species' specific thresholds of extinction

## list local BL network files
files <- list.files(results.dir, pattern = "spp10kWdietFUND_", full.names = TRUE)

## loading diet categories
BARMdiet.binary <- prepInputs(url = "https://zenodo.org/api/records/13345395/files-archive",
                              archive = "13345395.zip",
                              targetFile = "BARMdiet_binFUNDLINKS_50_nocann.RData",
                              destinationPath = "data/",
                              fun = "load")
## "unlist"
BARMdiet.binary <- BARMdiet.binary$BARMdiet.binary_nocann
dietcat <- grep("^[A|B|M|R][[:digit:]]", rownames(BARMdiet.binary), invert = TRUE, value = TRUE)

## define quantiles to use - note that min and median are added by calc.extThresh
quants <- c(0.1, 0.25, 0.5, 0.75, 0.90)

cat(paste("***************\n", "Calculating extinction thresholds:\n", date(), "\n"))
plan(multisession, workers = noCPUs)
future_lapply(files, calc.extThresh, quants = quants, out.dir = results.dir, dietcat = dietcat)
future:::ClusterRegistry("stop")
cat(paste0("*done!\n", date(), "\n***************\n"))

