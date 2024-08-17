## -----------------------------------------------------------------
## Maps of robustness
## 10KM
## SUPPORTING SCRIPTS - For workflow demo
## 
## Ceres Aug 2024
## -----------------------------------------------------------------

## source script for labels and map plots
source("Analyses/labsAndCols.R")
source("Tools/plotFuns.R")

## MAPS OF ROBUSTNESS, PRIMARY AND SECONDARY EXTINCTIONS AND COLONIZATIONS

## get lat-long coords from points, using mask raster
coords <- data.table(xyFromCell(mask10k,  cell = which(!is.na(getValues(mask10k)))),
                     PAGENAME = mask10kID$PageName)
all_metrics <- coords[all_metrics, on = "PAGENAME", nomatch = 0]   ## note that some pixels have no networks

invsMap <- plotMetricsMaps(all_metrics, x = "x", y = "y", fill = "Invasiv",
                           wrapVar = "Scen", wrapLabeller = scenLab, 
                           title = "No. colonizations")

PextMap <- plotMetricsMaps(all_metrics, x = "x", y = "y", fill = "Pext",
                           wrapVar = "Scen", wrapLabeller = scenLab, 
                           title = "No. primary extinctions")

SextMap <- plotMetricsMaps(all_metrics, x = "x", y = "y", fill = "Sext",
                           wrapVar = "Scen", wrapLabeller = scenLab, 
                           title = "No. secondary extinctions")

robMap <- plotMetricsMaps(all_metrics, x = "x", y = "y", fill = "invRobust",
                          wrapVar = "Scen", wrapLabeller = scenLab,
                          title = "Robustness")

scenLab2 <- c("hd_rcp85_noLUC_Invs_noIUCNext" = "Climate change", 
              "noCC_noCC_noLUC_noInvs_CR_EN_VU" = "IUCN extinctions")
robMap2 <- plotMetricsMaps(all_metrics[grepl("(hd_rcp85|CR_EN_VU)", Scen)],
                           x = "x", y = "y", fill = "invRobust",
                           wrapVar = "Scen", wrapLabeller = scenLab2, 
                           title = "Robustness")
robMap2 + labs(title = "", fill = "Robustness")

## worse than previous
all_metrics[, invRobustLog := log(invRobust+1)]
robMap3 <- plotMetricsMaps(all_metrics[grepl("(hd_rcp85|CR_EN_VU)", Scen)],
                           x = "x", y = "y", fill = "invRobustLog",
                           wrapVar = "Scen", wrapLabeller = scenLab2, 
                           title = "Robustness")
## all metrics
png(file.path(fig.dir, paste0("RobPextSextInvsMaps", quant, ".png")), 
    res = 300, units = "cm", width = 30, height = 28)
ggarrange(robMap, PextMap, SextMap, invsMap,
          ncol = 2, nrow = 2, labels = "auto")
dev.off()

## individual metrics
ggsave(plot = robMap, filename = file.path(fig.dir, paste0("RobMaps", quant, ".png")),
       dpi = 300, units = "cm", width = 15, height = 14)
ggsave(plot = robMap2 + theme(plot.title = element_blank()) + labs(fill = "Robustness"), 
       filename = file.path(fig.dir, paste0("RobMaps_CC_CR_EN_VU", quant, ".png")),
       dpi = 300, units = "cm", width = 30, height = 14)
ggsave(plot = robMap3, 
       filename = file.path(fig.dir, paste0("logRobMaps_CC_CR_EN_VU", quant, ".png")),
       dpi = 300, units = "cm", width = 30, height = 14)

dir.create(file.path(fig.dir, "pubFigs"), showWarnings = FALSE)
qs::qsave(robMap2, file = file.path(fig.dir, "pubFigs", "robMap2.qs"))

