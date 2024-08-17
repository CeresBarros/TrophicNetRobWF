## ------------------------------------------------------------------
## EUROPEAN TROPHIC WEBS
## SPECIES PRESENCES AND ABSENCES BY PIXEL
## SUPPORTING SCRIPTS - For workflow demo
##
## By Ceres: Aug 2024
## -------------------------------------------------------------------

## script to produce binary matrices of pixelXspp

## -----------------------------------------------------
## CURRENT SPECIES DISTRIBUTIONS FROM LUIGI MAIORANOS DISTS 
## -----------------------------------------------------

## define directories
sppdist.dir <- "FoodWebs_robustness/Spp_distributions/10K/LuigiMaiorano_dists/"
outputs.dir <- "FoodWebs_robustness/Spp_distributions/10K/pixXspp_matrices/fromLuigiMaiorano_dists"

## LOAD DISTRIBUTION DATA ------------------------------
sppDists <- list.files(sppdist.dir, full.names = T,pattern = ".RData$")

master <- do.PAmaster(sppDists[1:100], opt = opt)

out.filenm <- paste0("Spp_traits_habs/MASTER.bin10000", file.suff, k, ".Rdata")
save(master, file = file.path(outputs.dir, out.filenm))

## -----------------------------------------------------
## CURRENT SPECIES DISTRIBUTIONS FROM SDMs 
## -----------------------------------------------------

## define directories
sppdist.dir <- "SDM_newclimLUKE"
outputs.dir <- "FoodWebs_robustness/Spp_distributions/10K/pixXspp_matrices/fromSDMs/IPCC5_RF_GAM"
dir.create(outputs.dir)

## define type of ensemble averaging to use
sdmtype <- "wm_bin"

## list of model ensembles to iterate through
m <- c("GAM", "RF")

folder <- list.files(sppdist.dir, recursive = TRUE, pattern = "_results.zip", full.names = TRUE)
sppDists <- grep(".img", unzip(zipfile = folder, list = TRUE)$Name, value = TRUE)

## make vector of combinations
combs <- apply(expand.grid("current", sdmtype, m), 1, paste, collapse = "_")

lapply(combs, FUN = function(x){
  sppDists_sub <- grep(x, sppDists, value = TRUE)
  
  ## removing spp with too few observations
  ## reloding data to match spp IDs
  spp_fewObs <- read.table(file.path(sppdist.dir, "spp_NoOcc.txt"), header = TRUE, row.names = 1)
  spp_fewObs <- as.character(spp_fewObs$SP[spp_fewObs$No_occ < 20])
  spp_fewObs <- paste0(dirname(sppDists_sub[1]), "/", spp_fewObs, "_", x, ".img")
  sppDists_sub <- sppDists_sub[!sppDists_sub %in% spp_fewObs]
  
  ## Build pixXspp matrix
  master.SDMpres <- do.PAmaster(sppDists_sub)
  
  ## save
  out.filenm <- paste0("pixXspp_10k_", x, file.suff, k, ".Rdata")
  save(master.SDMpres, file = file.path(outputs.dir, out.filenm))
})


## -----------------------------------------------------
## FUTURE SPECIES DISTRIBUTIONS FROM SDMs 
## -----------------------------------------------------

## define directories
sppdist.dir <- "../SDM_newclimLUKE"
outputs.dir <- "Spp_distributions/10K/pixXspp_matrices/fromSDMs/IPCC5_RF_GAM"
dir.create(outputs.dir)

folder <- list.files(sppdist.dir, recursive = TRUE, pattern = "_results.zip", full.names = TRUE)
sppDists <- grep(".img", unzip(zipfile = folder, list = TRUE)$Name, value = TRUE)
# folder <- "../SDM_newclimLUKE/SDM_newclimLUKE/_results/"
# sppDists <- list.files(folder, pattern = ".img$", recursive = TRUE)

## define type of ensemble averaging to use
sdmtype <- "wm_bin"

## list of statistical models, GCMs and CC scenarios to iterate through
m <- c("GAM", "RF")
mm <- c("gs", "ip", "hd", "he", "no")
scen <- c("rcp45", "rcp60", "rcp85")

## make combinations to run through
combs <- apply(expand.grid(mm, scen, sdmtype, m), 1, paste, collapse = "_")

lapply(combs, FUN = function(x){
  cat(paste("Making spp P/A matrix for", x))
  sppDists_sub <- grep(x, sppDists, value = TRUE)
  
  ## removing spp with too few observations
  ## reloding data to match spp IDs
  spp_fewObs <- read.table(file.path(sppdist.dir, "spp_NoOcc.txt"), header = TRUE, row.names = 1)
  spp_fewObs <- as.character(spp_fewObs$SP[spp_fewObs$No_occ < 20])
  spp_fewObs <- paste0(dirname(sppDists_sub[1]), "/", spp_fewObs, "_", x, ".img")
  sppDists_sub <- sppDists_sub[!sppDists_sub %in% spp_fewObs]
  
  ## Build pixXspp matrix
  master.fut <- do.PAmaster(sppDists_sub)
  
  ## save
  out.filenm <- paste0("pixXspp_10k_", x, file.suff, k, ".Rdata")
  save(master.fut, file = file.path(outputs.dir, out.filenm))
  cat(paste0("\nDone!\n"))
})



