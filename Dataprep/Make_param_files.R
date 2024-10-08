## --------------------------------------------------
## NETWORK ROBUSTNESS TO CC AND LUC
## 10KM
## SUPPORTING SCRIPTS - For workflow demo
##
## CREATE PARAMETER FILES
##
## Ceres Aug 2024
## --------------------------------------------------

outputs.dir <- "Param_files/workflowDemo"
dir.create(outputs.dir, recursive = TRUE)

## this script creates .txt parameter lists so that networks can be run in batch mode
## in interactive mode the script will loop through lines

## List available climate scenarios/models
CCmodels <- c("current_wm_bin_RF", "hd_rcp85_wm_bin_RF")
LUCmodels <- "noLUC"
IUCNlevels <- c("CR_EN_VU", "noIUCN")

## PARAMETERS FOR CC & SPECIES EXTINCTIONS SIMULATIONS ---------------------------------------------------------
params_CC_IUCN <- data.table(rbind(as.matrix(expand.grid(CCmodels[grepl("current", CCmodels)],
                                              LUCmodels,
                                              IUCNlevels[!grepl("noIUCN", IUCNlevels)],
                                              stringsAsFactors = FALSE)),
                        as.matrix(expand.grid(CCmodels[!grepl("current", CCmodels)],
                                              LUCmodels,
                                              IUCNlevels[grepl("noIUCN", IUCNlevels)],
                                              stringsAsFactors = FALSE))
))

## add other parameters for compatibility with scripts
params_CC_IUCN[, `:=`(doCC = !grepl("current", Var1),
                      doNOdisp = FALSE,
                      doLUC = !grepl("noLUC", Var2),
                      doPA = FALSE)]

## PARAMETERS FOR BASELINE SIMULATIONS ----------------------------------------------------------
params_BL <- data.table(expand.grid(CCmodels[grepl("current", CCmodels)],
                                    LUCmodels,
                                    IUCNlevels[grepl("noIUCN", IUCNlevels)],
                                    stringsAsFactors = FALSE))
params_BL[, `:=`(doCC = FALSE,
                 doNOdisp = FALSE,
                 doLUC = FALSE,
                 doPA = FALSE)]

## SAVE FILES IN UNIX EOL STYLE
## save
output.file <- file(file.path(outputs.dir, paste0("params_Scen_FULL.txt")), "wb")
write.table(as.matrix(params_CC_IUCN), file = output.file, row.names = FALSE, col.names = FALSE)
close(output.file)

output.file <- file(file.path(outputs.dir, paste0("params_BL.txt")), "wb")
write.table(as.matrix(params_BL), file = output.file, row.names = FALSE, col.names = FALSE)
close(output.file)
