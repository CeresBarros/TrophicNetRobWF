## --------------------------------------------------
## NETWORK ROBUSTNESS TO CC AND LUC
## 10KM
## SUPPORTING SCRIPTS - For workflow demo
## 
## Load species-related data
## Ceres Aug 2024
## --------------------------------------------------

## --------------------------------------------------
## SPP INTERATIONS ----------------------------------
## load spp x spp data - binary with diet categories
load("Spp_traits_habs/BARMdiet_binFUNDLINKS_50_nocann.RData")     ## version with no cannibalistic links 
## change name and clean ws
BARMdiet.binary <- BARMdiet.binary_nocann; rm(BARMdiet.binary_nocann); gc(reset = TRUE)

## spp with too few obsrvations (<10). To remove from sppXspp table - note that B511 only has distributions and no other data
spp_fewObs <- read.table(file.path("../SDM_newclimLUKE", "spp_NoOcc.txt"), header = TRUE, row.names = 1)
spp_fewObs$SP <- sub("mm", "M", spp_fewObs$SP)
spp_fewObs$SP <- sub("mb", "B", spp_fewObs$SP)
spp_fewObs$SP <- sub("mr", "R", spp_fewObs$SP)
spp_fewObs$SP <- sub("ma", "A", spp_fewObs$SP)
spp_fewObs <- spp_fewObs[spp_fewObs$No_occ < 20,]

BARMdiet.binary <- BARMdiet.binary[!row.names(BARMdiet.binary) %in% spp_fewObs$SP, !colnames(BARMdiet.binary) %in% spp_fewObs$SP]

## --------------------------------------------------
## EXTINCTION THRESHOLDS BY SPP    
if (length(list.files("NetworkSims/Baseline_SDMpres_GlobCover/No_ext_thresh/", pattern = "Ext_thresh_BLwebs")) > 0) {
  if (grepl("GAM", params[["CC"]])) {
    fileEXT <- grep("GAM",
                    list.files("NetworkSims/Baseline_SDMpres_GlobCover/No_ext_thresh/", pattern = "Ext_thresh_BLwebs", full.names = TRUE),
                    value = TRUE)
  } else if (grepl("RF", params[["CC"]])) {
    fileEXT <- grep("RF",
                    list.files("NetworkSims/Baseline_SDMpres_GlobCover/No_ext_thresh/", pattern = "Ext_thresh_BLwebs", full.names = TRUE),
                    value = TRUE)
  }
  ext.thresh <- read.table(fileEXT)
  cat(paste("\nUsing", basename(fileEXT), "as min. prey thresh. file"))
}

## --------------------------------------------------
## SPP PRESENCES/ABSENCES 

## Get current presences/absences - always necessary
if (grepl("GAM", params[["CC"]])) {
  fileBL <- "pixXspp_10k_current_wm_bin_GAM_allhab_tresh0.Rdata"
} else if (grepl("RF", params[["CC"]])) {
  fileBL <- "pixXspp_10k_current_wm_bin_RF_allhab_tresh0.Rdata"
}

master.SDMpres <- prepInputs(url = "https://zenodo.org/api/records/13345395/files-archive",
                              archive = "13345395.zip",
                              targetFile = fileBL,
                              destinationPath = "data/",
                              fun = "load")
master.SDMpres <- master.SDMpres$master.SDMpres

cat(paste("\nUsing", basename(fileBL), "as current spp P/A file"))

## remove pixels that are not predicted
master.SDMpres <- master.SDMpres[as.character(master.SDMpres$PAGENAME) %in% as.character(orig.pixhabs$PAGENAME),]

## Get presences absences necessary for the scenario (coming from parameter file)
if (doCC) {
  fileCC <- paste0("pixXspp_10k_", params[["CC"]], "_", file.suff, ".Rdata")
  master.fut <- prepInputs(url = "https://zenodo.org/api/records/13345395/files-archive",
                           archive = "13345395.zip",
                           targetFile = fileCC,
                           destinationPath = "data/",
                           fun = "load")
  master.fut <- master.fut$master.fut
  cat(paste("\nUsing", basename(fileCC), "as future spp P/A file"))
} else if (doLUC | doIUCN) {
  master.fut <- master.SDMpres
  cat(paste("\nUsing", basename(fileBL), "as future spp P/A file"))
}

## Remove species based on IUCN status from scenario presences/absences
if (doIUCN) {
  sppIUCNstatus <- read.table("Spp_traits_habs/IUCN/BARM_IUCNstatus.txt", stringsAsFactors = FALSE, header = TRUE)
  sppIUCNstatus <- as.data.table(sppIUCNstatus)
  
  IUCNlevels <- unlist(strsplit(params["IUCN"], "_"))
  sppToRm <- sppIUCNstatus[europeanRegionalRedListCategory %in% IUCNlevels, ID]
  sppToRm <- intersect(sppToRm, colnames(master.fut))
  master.fut <- mutate(master.fut, across(all_of(sppToRm), ~ ifelse(!is.na(.x), 0, .x)))
  cat(paste("\nRemoving species with IUCN status", paste(IUCNlevels, collapse = ","), 
            "from future spp P/A"))
}


if (exists("master.fut")) {
  ## remove pixels that are not predicted 
  master.fut <- if (doLUC) {
    master.fut[as.character(master.fut$PAGENAME) %in% as.character(fut.pixhabs$PAGENAME),]
  } else {
    master.fut[as.character(master.fut$PAGENAME) %in% as.character(orig.pixhabs$PAGENAME),]
  }

  ## remove species that were not present in any baseline web - this will prevent spp from outside EU countries colonizing EU pixels
  master.fut <- master.fut[, c("PAGENAME", rownames(ext.thresh))]
}
