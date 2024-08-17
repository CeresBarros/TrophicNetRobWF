## --------------------------------------------------
## SENSITIVITY ANALYSIS
## SUPPORTING SCRIPTS - For workflow demo
## 
## Ceres Aug 2024
## --------------------------------------------------

## LOADING BASELINE RESULTS ----------------------------------
quants <- c("No_ext_thresh", "min", "quant10", "quant25", "median", "quant50", "quant75", "quant90")
names(quants) <- quants
all_metrics_BL <- lapply(quants, FUN = function(quant) {
  reproducible::Cache(loadResultsMetrics,
                      bl.dir = res.dir,
                      out.dir = out.dir,
                      quant = quant,
                      onlyBL = TRUE,
                      userTags = c("loadAllmetrics", "baselineOnly", quant))  
}) 

all_metrics_BL <- rbindlist(all_metrics_BL, use.names = TRUE, idcol = "ExtThresh") 

setnames(all_metrics_BL, old = names(all_metrics_BL),
         new = sub("^BL_", "", names(all_metrics_BL)))

## re-order thresholds
all_metrics_BL$ExtThresh <- as.factor(all_metrics_BL$ExtThresh)
all_metrics_BL$ExtThresh <- relevel(all_metrics_BL$ExtThresh, ref = "quant90")
all_metrics_BL$ExtThresh <- relevel(all_metrics_BL$ExtThresh, ref = "quant75")
all_metrics_BL$ExtThresh <- relevel(all_metrics_BL$ExtThresh, ref = "median")
all_metrics_BL$ExtThresh <- relevel(all_metrics_BL$ExtThresh, ref = "quant25")
all_metrics_BL$ExtThresh <- relevel(all_metrics_BL$ExtThresh, ref = "quant10")
all_metrics_BL$ExtThresh <- relevel(all_metrics_BL$ExtThresh, ref = "min")
all_metrics_BL$ExtThresh <- relevel(all_metrics_BL$ExtThresh, ref = "No_ext_thresh")
all_metrics_BL <- all_metrics_BL[ExtThresh != "quant50"]   ## same as median, not sure why it was calculated

## remove GAMs - not used anymore (Mar 2021)
all_metrics_BL <- all_metrics_BL[SDM_stat != "GAM"]

## ------------------------------------------------------------------------------------------------------------------------------------
## BOXPLOTS ---------------------------------------------------------------------------------------------------------------------------
## RICHNESS
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = S)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("S")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_S.tiff"), device = "tiff", width = 7, height = 5)

## NUMBER OF LINKS
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = L)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("L")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_L.tiff"), device = "tiff", width = 7, height = 5)

## LINK DENSITY
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = L/S)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("LD")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_LD.tiff"), device = "tiff", width = 7, height = 5)

## CONNECTANCE
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = C)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("C")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_C.tiff"), device = "tiff", width = 7, height = 5)

## OMNIVORY
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = propOmn)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("propOmn")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_Omn.tiff"), device = "tiff", width = 7, height = 5)

## PROP BASAL PREDATORS
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = propB)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("propB")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_propB.tiff"), device = "tiff", width = 7, height = 5)

## PROP INTERMEDIATE PREDATORS
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = propI)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("propI")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_propI.tiff"), device = "tiff", width = 7, height = 5)

## PROP TOP PREDATORS
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = propT)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("propT")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_propT.tiff"), device = "tiff", width = 7, height = 5)

## GENERALITY
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = normGen)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("Gen")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_Gen.tiff"), device = "tiff", width = 7, height = 5)

## VULNERABILITY
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = normVul)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("Vul")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_Vul.tiff"), device = "tiff", width = 7, height = 5)

## SD GENERALITY
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = SDnormGen)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("sdNormGen")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_sdGen.tiff"), device = "tiff", width = 7, height = 5)

## SD VULNERABILITY
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = SDnormVul)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("sdNormVul")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_sdVul.tiff"), device = "tiff", width = 7, height = 5)

## MEAN TROPHIC LEVEL
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = mean.TL)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("meanTL")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_meanTL.tiff"), device = "tiff", width = 7, height = 5)

## MAX TROPHIC LEVEL
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = max.TL)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("maxTL")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_maxTL.tiff"), device = "tiff", width = 7, height = 5)

## SD TROPHIC LEVEL
ggplot(data = all_metrics_BL, aes(x = ExtThresh, y = sd.TL)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("No_ext_thresh" = "no threshold", "min" = "min", "quant10" = "10%",
                              "quant25" = "25%", "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = expression(italic("sdTL")), x = "")
ggsave(filename = file.path(out.dir, "Boxplot_BL_sdTL.tiff"), device = "tiff", width = 7, height = 5)


## ------------------------------------------------------------------------------------------------------------------------------------
## BOXPLOTS OF NUMBER OF MINIMUM PREY SPP PER QUANTILE
## ------------------------------------------------------------------------------------------------------------------------------------
fileNames <- list.files(res.dir, pattern = "Ext_thresh_BLwebs", full.names = TRUE, recursive = TRUE)

ext.thresh <- lapply(fileNames, function(x) { 
  DT <- as.data.table(read.table(x, header = TRUE, row.names = NULL))
  DT[, SDM_stat := if (grepl("GAM", x)) "GAM" else "RF"]
  DT
})

ext.thresh <- rbindlist(ext.thresh, use.names = TRUE)
ext.thresh <- melt.data.table(ext.thresh, id.vars = c("row.names", "SDM_stat"))

## exclude GAMs - Mar 2021
ext.thresh <-  ext.thresh[SDM_stat != "GAM"]
## exclude non major quantiles:
ext.thresh<- ext.thresh[variable %in% c("No_ext_thresh", "min" = "min", "quant10", "quant25","median", "quant75", "quant90")]

ext.thresh[, variable := factor(variable, levels = c("min", "quant10", "quant25", "median", "quant75", "quant90"))]
ggplot(ext.thresh, aes(y = value, x = variable)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5) +
  scale_x_discrete(labels = c("min" = "min", "quant10" = "10%", "quant25" = "25%", 
                              "quant75" = "75%", "quant90" = "90%", "median" = "median")) +
  theme_bw() + theme_bw() + theme(legend.title = element_blank()) +
  labs(y = "No. prey", x = "Thresholds")
ggsave(filename = file.path(out.dir, "No_prey_perquantile.tiff"), device = "tiff", width = 7, height = 5)
