## -----------------------------------------------------------------
## ROBUSTNESS AND TROPHIC LEVEL
## 10KM
## SUPPORTING SCRIPTS - For workflow demo
##
## Ceres Aug 2024
## -----------------------------------------------------------------

## source script for labels
source("Analyses/labsAndCols.R")

## get spp P/A under future climate (no network filtering) -- not used by compilePextSext if only doing IUCN
sppPA.fut.file <- list.files("data", pattern = paste0("pixXspp.*", "hd_rcp85_wm_bin_RF"), full.names = TRUE)

## compile tables of Pext and Sext spps IDs
names(masterScen.files) <- NULL  ## use the actual file names to name the outFiles list
outFiles <- Map(masterScen.file = masterScen.files,
                f = compilePextSext,
                MoreArgs = list(
                  masterBL.file = masterBL.files,
                  sppPA.fut.file = sppPA.fut.file
                ))

## (Baseline) trophic level of extinct species following IUCN extinctions scenario
scenstr <- "CR_EN_VU"

PextFile <- outFiles[[grep(scenstr, basename(names(outFiles)))]] |>
  grep("Pext", x = _, value = TRUE)
SextFile <- outFiles[[grep(scenstr, basename(names(outFiles)))]] |>
  grep("Sext", x = _, value = TRUE)

## calculate trophic levels
outLs <- tlcalcFun(scenstr, PextFile, SextFile)
tlDT <- outLs$tlDT
masterPext <- outLs$masterPext
masterSext <- outLs$masterSext

rm(outLs)
for (i in 1:3) gc(reset = TRUE)

tlDT <- filterTL(tlDT = tlDT, masterPext = masterPext, masterSext = masterSext)

###  Trophic level distributions by type of extinction
# tl.model <- glm(TL ~ Ext, data = tlDT, family = "Gamma")
# summary(tl.model)
# anova(tl.model)

# opts <- par(mfrow = c(2,2))
# plot(tl.model)  ## good enough
# par(opts)

assign(paste0(scenstr, "TLPlots"), TLplotsFun(tlDT, quant, scenstr, fig.dir))
eval(parse(text = paste0(scenstr, "TLPlots")))

### Trophic level distributions by type of extinction and robustness level

assign(paste0(scenstr, "TLRobPlots"), TLRobplotsFun(res.dir, tlDT, quant, scenstr, fig.dir))
eval(parse(text = paste0(scenstr, "TLRobPlots")))   ## just prints

## save ggplot objects for later
dir.create(file.path(fig.dir, "pubFigs"), showWarnings = FALSE)
eval(parse(text = paste0(
  "qs::qsave(", scenstr, "TLRobPlots, file.path(fig.dir, 'pubFigs/", scenstr, "TLRobPlots.qs'))"
  )
  ))

## (Baseline) trophic level of extinct species following CC scenario
scenstr <- "hd_rcp85_wm_bin_RF"

PextFile <- outFiles[[grep(scenstr, basename(names(outFiles)))]] |>
  grep("Pext", x = _, value = TRUE)
SextFile <- outFiles[[grep(scenstr, basename(names(outFiles)))]] |>
  grep("Sext", x = _, value = TRUE)


outLs <- tlcalcFun(scenstr, PextFile, SextFile)
tlDT <- outLs$tlDT
masterPext <- outLs$masterPext
masterSext <- outLs$masterSext

rm(outLs)
for (i in 1:3) gc(reset = TRUE)

tlDT <- filterTL(tlDT = tlDT, masterPext = masterPext, masterSext = masterSext)

###  Trophic level distributions by type of extinction
# tl.model <- glm(TL ~ Ext, data = tlDT, family = "Gamma")
# summary(tl.model)
# anova(tl.model)

# opts <- par(mfrow = c(2,2))
# plot(tl.model)  ## good enough
# par(opts)

assign(paste0(scenstr, "TLPlots"), TLplotsFun(tlDT, scenstr, quant, fig.dir))
eval(parse(text = paste0(scenstr, "TLPlots")))


### Trophic level distributions by type of extinction and robustness level
assign(paste0(scenstr, "TLRobPlots"), TLRobplotsFun(res.dir, tlDT, quant, scenstr, fig.dir))
eval(parse(text = paste0(scenstr, "TLRobPlots")))     ## just prints

## save ggplot objects for later
eval(parse(text = paste0(
  "qs::qsave(", scenstr, "TLRobPlots, file.path(fig.dir, 'pubFigs/", scenstr, "TLRobPlots.qs'))"
)
))
