## --------------------------------------------------
## NETWORK ROBUSTNESS TO HABITAT CHANGES
## Simulating networks under projected habitat changes
## 10KM
##
## COMPILATION OF SIMUALTION RESULTS - NETWORK METRICS
## ALL PIXELS
## --------------------------------------------------

## this script should be sourced

## LOADING NECESSARY RASTERS ------------------------
doCache <- if (redo) "overwrite" else TRUE

all_metrics <- reproducible::Cache(loadResultsMetrics,
                                   bl.dir = bl.dir,
                                   res.dir = res.dir,
                                   out.dir = out.dir,
                                   quant = quant,
                                   useCache = doCache,
                                   userTags = "loadAllmetrics")


## transform Robustness
all_metrics[, invRobust := 1-Robust]

## calculate link density:
all_metrics[, BL_LD := BL_L/BL_S]
all_metrics[, LD := L/S]

## remove "fake networks"
all_metrics <- all_metrics[BL_S > 1]

## remove networks whose "secondary consumers" only have cannibalistic links
## e.g. a network with only 2 cannibal amphibians that eat DCs but not one another
all_metrics <- all_metrics[!(BL_propB == 0 & BL_propI == 1)]

all_metrics[BL_propOmn == 0.5]

## remove networks that had no secondary consumers to start with
all_metrics <- all_metrics[BL_No.SecCons > 0]

## make scenario(s) variable(s)
all_metrics[, Scen := paste(GCM, RCP, LUC, Invasions, IUCN, sep = "_")]
all_metrics[, ScenStat := paste(SDM_stat, GCM, RCP, LUC, Invasions, IUCN, sep = "_")]
all_metrics[, Scen := as.factor(Scen)]
all_metrics[, ScenStat := as.factor(ScenStat)]
all_metrics[, SDM_stat := as.factor(SDM_stat)]
all_metrics[, GCM := as.factor(GCM)]
all_metrics[, RCP := as.factor(RCP)]
all_metrics[, LUC := as.factor(LUC)]
all_metrics[, Invasions := as.factor(Invasions)]
all_metrics[, IUCN := as.factor(IUCN)]

all_metrics[, GCM := relevel(GCM, "noCC")]
all_metrics[, RCP := relevel(RCP, "noCC")]
all_metrics[, LUC := relevel(LUC, "noLUC")]
all_metrics[, Invasions := relevel(Invasions, "noInvs")]
all_metrics[, IUCN := relevel(IUCN, "noIUCNext")]
