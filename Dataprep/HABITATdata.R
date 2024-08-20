## --------------------------------------------------
## NETWORK ROBUSTNESS TO CC AND LUC
## 10KM
## SUPPORTING SCRIPTS - For workflow demo
## 
## Load habitat data
## Ceres Aug 2024
## --------------------------------------------------

## this script aims at calculating the area of each GlobCover (v2.2) habitat category (300m2)
## per pixel of a grid at 10km2 and build pixel X habitat matrices used in network simulations

## LOAD DATA -------------------------------------------
## habitats raster
## habitats
habRaster <- prepInputs(url = "https://zenodo.org/api/records/13345395/files-archive",
                        archive = "13345395.zip",
                        targetFile = "GLOBCOVER.img",
                        destinationPath = "data/",
                        fun = "raster::raster")

## spp x habitat table - with GlobCover classes
BARM.habs <- prepInputs(url = "https://zenodo.org/api/records/13345395/files-archive",
                        archive = "13345395.zip",
                        targetFile = "BARM_allhabs.csv",
                        destinationPath = "data/",
                        fun = read.csv(targetFile, header = TRUE, row.names = "ID"))

BARM.habs <- as.matrix(BARM.habs[, which(colnames(BARM.habs) != "SPPname")])       ## remove spp names column (spp codes are row.names)

## checks
if (!compareRaster(lakes, mask10k, stopiffalse = FALSE)) {
  lakes <- Cache(postProcess,
                 x = lakes, 
                 rasterToMatch = mask10kSHP)
}

sppHabs <- as.numeric(sub("X", "", colnames(BARM.habs)))
rasHabs <- unique(getValues(habRaster))
notInSpp <- setdiff(rasHabs, sppHabs)

if (length(notInSpp)) {
  message("Habitats: ")
  message(paste(notInSpp, collapse = ", "))
  message("not found in spp X hab matrix. Will be removed from pixel X habitat matrix")
}


## COUNT HABITATS IN LARGER GRID ----------------------
## project polygons for no info loss
if (!compareCRS(mask10kSHP, habRaster)) {
  mask10kSHP <- Cache(spTransform,
                   x = mask10kSHP, 
                   CRSobj = crs(habRaster))
}

## crop for faster extraction
habRaster <- crop(habRaster, extent(mask10kSHP))

## extract
mask10kSHP <- st_as_sf(mask10kSHP)
orig.pixhabs2List <- Cache(exact_extract,
                      x = habRaster, 
                      y = mask10kSHP)
names(orig.pixhabs2List) <- mask10kSHP$PageName

## sum the fractions per habitat type
orig.pixhabs2List <- lapply(orig.pixhabs2List, FUN = function(DT) {
  DT <- as.data.table(DT)
  DT <- DT[, list(coverage_fraction = sum(coverage_fraction)),
           by = value]
  DT
})

## bind
orig.pixhabs <- rbindlist(orig.pixhabs2List, idcol = "PAGENAME")

## REMOVE LAKES ----------------------------------------
lakePAGENAMES <- as.character(mask10kID[mask10k[!is.na(lakes[])], "PageName"])
cols <- grep("PAGENAME", names(orig.pixhabs), invert = TRUE)
orig.pixhabs[PAGENAME %in% lakePAGENAMES, (cols) := NA]

## extend table
orig.pixhabs <- dcast.data.table(orig.pixhabs, formula = PAGENAME ~ value, fun.aggregate = sum, 
                             value.var = "coverage_fraction")
orig.pixhabs[, "NA" := NULL]

cols <- c("PAGENAME", setdiff(names(orig.pixhabs), "PAGENAME"))
setcolorder(orig.pixhabs, cols)

cols <- setdiff(names(orig.pixhabs), "PAGENAME")
setnames(orig.pixhabs, old = cols, new = paste0("X", cols))

## REMOVE HABITATS NOT IN SPP X HABITATS MATRIX
cols <- setdiff(names(orig.pixhabs), paste0("X", notInSpp))
orig.pixhabs <- orig.pixhabs[, ..cols]

## save
write.table(orig.pixhabs, file = file.path(output.dir, "tabulateGLC_10Km_GC_FILTRD_NOLAKES.txt"))

## --------------------------------------------------
## HABITATS -----------------------------------------

## before converting attribute NA's to spp whose habitats are all "optimal"
BARM.habs[rowSums(BARM.habs, na.rm = TRUE) == 2*ncol(BARM.habs),] <- NA

## TODO: put data in Zenodo and download from there
fileHAB <- "Habitats/Habitat_pixel/GlobCover/tabulateGLC_10Km_GC_FILTRD_NOLAKES.txt"
orig.pixhabs <- read.table(fileHAB, header = TRUE)
orig.pixhabs$PAGENAME <- as.character(orig.pixhabs$PAGENAME)
rownames(orig.pixhabs) <- orig.pixhabs$PAGENAME

## checks
if (!any(colnames(BARM.habs) %in% colnames(orig.pixhabs))) {
  stop("Can't find any of the species' preferred habitats in the pixel X habitat matrix")
}

if (!any(colnames(orig.pixhabs) %in% colnames(BARM.habs))) {
  stop("Can't find any of habitats in habitats layer (pixel X habitat matrix) in the species' X habitat matrix")
}

cat(paste("\nUsing", basename(fileHAB), "habitats file"))
