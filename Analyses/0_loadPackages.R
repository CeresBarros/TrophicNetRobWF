## Package loading

pkgfolder <- "packages/"
dir.create(pkgfolder)

## package installation
.libPaths(pkgfolder)
options(repos = c("predictiveecology.r-universe.dev", getOption("repos")))

if (!"Require" %in% installed.packages())
  install.packages("Require", lib = "packages/")

Require::Require(c(
  "cheddar",
  "colorspace",
  "data.table",
  "dplyr",
  "exactextractr",
  "foreign",
  "future",
  "future.apply",
  "ggExtra",
  "ggplot2",
  "ggpubr",
  "patchwork",
  "reproducible",
  "raster",
  "rasterVis",
  "rredlist",
  "RSQLite",
  "sf",
  "CeresBarros/ToolsCB@master (HEAD)"
),
libPaths = pkgfolder)

## install, but don't load
Require::Require(c(
  "qs",
  "SpaDES.tools",
  "terra"
), libPaths = pkgfolder, require = FALSE)

options(future.globals.maxSize = 3e+9,
        reproducible.useNewDigestAlgorithm = 2,
        reproducible.cachePath = "cache/",
        reproducible.useGDAL = FALSE,
        reproducible.rasterRead = "raster::raster",
        nwarnings = 100000000,
        max.print = 100000000)
