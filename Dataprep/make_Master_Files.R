## MAKING MASTER FILES ------------------------------

## these are files that contain species P/A data compiled from
## the output local networks (baseline or scenario)

if (exists("redo")) {
  redo <- FALSE
}

if (exists("save")) {
  save <- TRUE
}

if (exists("returnMaster")) {
  returnMaster <- FALSE
}

## diet categories
BARMdiet.binary_nocann <- prepInputs(url = "https://zenodo.org/api/records/13345395/files-archive",
                                     archive = "13345395.zip",
                                     targetFile = "BARMdiet_binFUNDLINKS_50_nocann.RData",
                                     destinationPath = "data/",
                                     fun = "load")
BARMdiet.binary_nocann <- BARMdiet.binary_nocann$BARMdiet.binary_nocann

dietcat <- rownames(BARMdiet.binary_nocann)[1:11]
rm(BARMdiet.binary_nocann); for (i in 1:10) gc()

## baseline webs
file.ls <- list.files(file.path(bl.dir, paste0("ExtTrsh_", quant)),
                      pattern = "spp10k", full.names = TRUE)

if (!requireNamespace("future")) {
  stop("Install 'future' package")
}
if (!requireNamespace("future.apply")) {
  stop("Install 'future.apply' package")
}

if (length(file.ls) > 1) {
  future::plan(future::multisession, gc = TRUE, workers = 2)
  masterBL.files <- future.apply::future_sapply(file.ls, makeAndSaveMasterBL,
                                                dietcat = dietcat,
                                                redo = redo, returnMaster = returnMaster, save = save)
  future:::ClusterRegistry("stop")
} else {
  masterBL.files <- sapply(file.ls, makeAndSaveMasterBL, dietcat = dietcat,
                           redo = redo, returnMaster = returnMaster, save = save)
}

## scenario webs
file.ls <- do.call(c, lapply(list.files(scen.dir, full.names = TRUE), function(x) {
  do.call(c, lapply(list.files(x, pattern = quant, full.names = TRUE), function(xx) {
    list.files(xx, pattern = "spp10k", full.names = TRUE)
  }))
}))

future::plan(future::multisession, gc = TRUE, workers = 2)
masterScen.files <- future.apply::future_sapply(file.ls, makeAndSaveMasterScen,
                                                dietcat = dietcat,
                                                redo = redo, returnMaster = returnMaster, save = save)
future:::ClusterRegistry("stop")
