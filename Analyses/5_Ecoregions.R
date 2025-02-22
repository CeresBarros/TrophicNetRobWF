source("Analyses/0_loadPackages.R")

## Extract results

# Load previous figure
fig.dir <- "NetworkSims/IUCN_CC_sims_GlobCover/Analyses/Figs/pubFigs"
robMap2 <- qs::qread(file.path(fig.dir, "robMap2.qs")) # lighter to load, all data
# plotOut <- qs::qread(file.path(fig.dir, "plotOut.qs")) # Larger, not needed

# Extract robustness data from figures
scen_clim <- tibble(robMap2$data)  %>%
  filter(Scen == "hd_rcp85_noLUC_Invs_noIUCNext") %>%
  dplyr::select(x, y, invRobust)
scen_iucn <- tibble(robMap2$data)  %>%
  filter(Scen == "noCC_noCC_noLUC_noInvs_CR_EN_VU") %>%
  dplyr::select(x, y, invRobust)


## Convert to layer format

# Load mask layer
mask10k <- prepInputs(url = "https://zenodo.org/api/records/13345395/files-archive",
                      archive = "13345395.zip",
                      targetFile = "reference_grid_10km.img",
                      destinationPath = "data/",
                      fun = "raster::raster")
mask10k[] <- mask10k[]

# Convert to raster
rast_clim <- terra::rast(scen_clim, crs = raster::crs(mask10k, asText = TRUE))
rast_iucn <- terra::rast(scen_iucn, crs = raster::crs(mask10k, asText = TRUE))


## Get ecoregions data

# Download ecoregions data
ecodir <- "Ecoregions"
ecozip <- "Ecoregions.zip"
if (!dir.exists(ecodir)) {
  if (!file.exists(ecozip)) {
    utils::download.file(url = "https://storage.googleapis.com/teow2016/Ecoregions2017.zip",
                       destfile = ecozip,
                       mode = "wb")
  }
  unzip(ecozip, exdir = ecodir, overwrite = TRUE, junkpaths = TRUE)
  file.remove(ecozip)
}

# Load ecoregions
ecoregions_shp <- terra::vect("Ecoregions/Ecoregions2017.shp")
ecoregions_vec <- terra::project(ecoregions_shp, rast_iucn)
# terra::mask(crop(ecoregions_vec, mask10k), as(terra::rast(mask10k), "SpatVector"))

# Crop to mask extent
ecoregions_vec <- crop(ecoregions_vec, mask10k)


## Summarize results by ecoregion

# Extract median per ecoregion
ecoregions_poly <- sf::st_as_sf(ecoregions_vec)
ecoregions_poly$medianRobustnessClim <- exact_extract(rast_clim, ecoregions_poly, "median")
ecoregions_poly$medianRobustnessIUCN <- exact_extract(rast_iucn, ecoregions_poly, "median")

# Remove ecoregions without any data
ecoregions_noNA <- filter(ecoregions_poly, !is.na(medianRobustnessClim))
plot(ecoregions_noNA["medianRobustnessClim"])
plot(ecoregions_noNA["medianRobustnessIUCN"])

# Rescale values below 0.7
ecoregions_resc <- ecoregions_noNA
ecoregions_resc$medianRobustnessClim <-
  with(ecoregions_resc, ifelse(medianRobustnessClim > 0.7, medianRobustnessClim, 0.7))
ecoregions_resc$medianRobustnessIUCN <-
  with(ecoregions_resc, ifelse(medianRobustnessIUCN > 0.7, medianRobustnessIUCN, 0.7))

# Rasterize ecoregions
rast_clim_eco <- terra::rasterize(ecoregions_resc, mask10k, "medianRobustnessClim")
rast_iucn_eco <- terra::rasterize(ecoregions_resc, mask10k, "medianRobustnessIUCN")


## Plot results

# Plot
t1 <- "Climate Change"
t2 <- "IUCN extinctions"
gg1 <- ggplot() +
  geom_sf(data = ecoregions_resc, aes(fill = medianRobustnessClim)) +
  scale_fill_viridis_c(direction=-1, limits = c(0.7, 1.0), name = "Median\nEcoregion\nRobustness") +
  labs(title = t1)
gg2 <- ggplot() +
  geom_sf(data = ecoregions_resc, aes(fill = medianRobustnessIUCN)) +
  scale_fill_viridis_c(direction=-1, limits = c(0.7, 1.0), name = "Median\nEcoregion\nRobustness") +
  labs(title = t1)
(gg3 <- gg1 + gg2)

# Export figures
ggsave(plot = gg1, filename = file.path(fig.dir, "rob_ecoregion_clim.png"), dpi = 300)
ggsave(plot = gg2, filename = file.path(fig.dir, "rob_ecoregion_iucn.png"), dpi = 300)
ggsave(plot = gg3, filename = file.path(fig.dir, "rob_ecoregion_both.png"),
       dpi = 300, height = 10, width = 16)
