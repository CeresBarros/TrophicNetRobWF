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

# Load mask polygon
mask10kSHP <- prepInputs(url = "https://zenodo.org/api/records/13345395/files-archive",
                         archive = "13345395.zip",
                         targetFile = "grid_10Km.shp",
                         destinationPath = "data/")
# mask10kSHP <- as_Spatial(mask10kSHP)
mask10kSHP <- as(mask10kSHP, "SpatVector")
mask10kSHP_agg <- aggregate(mask10kSHP)

# Crop to mask extent
ecoregions_mask <- mask(crop(ecoregions_vec, mask10kSHP_agg), mask10kSHP_agg)

## Summarize results by ecoregion

# Extract median per ecoregion
ecoregions_poly <- sf::st_as_sf(ecoregions_mask)
ecoregions_poly$medianRobustnessClim <- exact_extract(rast_clim, ecoregions_poly, "median")
ecoregions_poly$medianRobustnessIUCN <- exact_extract(rast_iucn, ecoregions_poly, "median")


# Rescale values below arbitrary threshold
ecoregions_resc <- ecoregions_poly
rv <- 0.8
ecoregions_resc$medianRobustnessClim <-
  with(ecoregions_resc, ifelse(medianRobustnessClim > rv, medianRobustnessClim, rv))
ecoregions_resc$medianRobustnessIUCN <-
  with(ecoregions_resc, ifelse(medianRobustnessIUCN > rv, medianRobustnessIUCN, rv))

# Rasterize ecoregions
rast_clim_eco <- terra::rasterize(ecoregions_resc, mask10k, "medianRobustnessClim")
rast_iucn_eco <- terra::rasterize(ecoregions_resc, mask10k, "medianRobustnessIUCN")


## Plot results

# Plot
t1 <- "(A) Climate Change Scenario"
t2 <- "(B) IUCN Extinctions Scenario"
gg1 <- ggplot() +
  geom_sf(data = ecoregions_resc, aes(fill = medianRobustnessClim)) +
  scale_fill_viridis_c(direction=-1, limits = c(rv, 1.0), guide = "none") +
  labs(title = t1) +
  theme_void()
gg2 <- ggplot() +
  geom_sf(data = ecoregions_resc, aes(fill = medianRobustnessIUCN)) +
  scale_fill_viridis_c(direction=-1, limits = c(rv, 1.0),
                      name = "Median\nEcoregion\nRobustness",
                      breaks = seq(rv, 1.0, by=0.05),
                      labels = c(paste0("< ", format(rv, nsmall = 2)),
                                 format(seq(rv, 1.0, by=0.05)[-1], nsmall = 2)),
                      guide = guide_legend(label.hjust = 1)
                      ) +
  labs(title = t2) +
  theme_void()
(ggh <- gg1 + gg2 +
  theme(legend.title = element_text(vjust = 4),
        legend.text = element_text(hjust = 1),
        legend.text.position = "left",
        legend.key.width = unit(0.5, "lines"),
        legend.key.height = unit(2.0, "lines")
        )
)
(ggv <- gg1 /
          (gg2 +
           scale_fill_viridis_c(direction=-1, limits = c(rv, 1.0),
                                name = "Median Ecoregion Robustness",
                                breaks = seq(rv, 1.0, by=0.05),
                                labels = c(paste("<", format(rv, nsmall = 2)),
                                           format(seq(rv, 1.0, by=0.05)[-1], nsmall = 2))
                                ) +
           theme(legend.position="bottom",
                 legend.title = element_text(hjust = 0.5),
                 legend.title.position="bottom",
                 legend.key.width = unit(5.0, "lines"),
                 legend.key.height = unit(0.7, "lines")
                 )
            )
)

hist1 <- ggplot(data = ecoregions_resc, aes(x = medianRobustnessClim)) +
  geom_histogram(aes(fill = after_stat(x)),
                 binwidth=0.05, boundary = rv) +
  scale_fill_viridis_c(direction = -1)
hist2 <- ggplot(data = ecoregions_resc, aes(x = medianRobustnessIUCN)) +
  geom_histogram(aes(fill = after_stat(x)),
                 binwidth=0.05, boundary = rv) +
  scale_fill_viridis_c(direction = -1)
(hists <- hist1 / hist2)

# # All pixel data
# ggplot(data = scen_clim, aes(x = invRobust)) +
#   geom_histogram(aes(fill = after_stat(x))) +
#   scale_fill_viridis_c(direction = -1)


# Export figures
ggsave(plot = ggh, filename = "Figures/rob_ecoregion_void.png",
       dpi = 300, height = 6, width = 14)
ggsave(plot = ggv, filename = "Figures/rob_ecoregion_void_v.png",
       dpi = 300, height = 14, width = 6)
ggsave(plot = hists, filename = "Figures/rob_ecoregion_hists.png",
       dpi = 300, height = 14, width = 6)

# Export for manuscript
ggsave(plot = ggv, filename = "Figures/fig2.jpg", # for submitted manuscript
       dpi = 200, height = 14, width = 6)
ggsave(plot = ggv, filename = "Figures/fig2.pdf", # for preprint
       dpi = 200, height = 14, width = 6)


# Reduce file while maintaining proportions
# ragg::agg_jpeg("Figures/fig2_mini.jpg", height = 7, width = 3, units = "in", res = 300, scaling = 0.5)
# ggv
# dev.off()
