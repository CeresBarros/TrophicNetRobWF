## --------------------------------------------------
## NETWORK ROBUSTNESS TO CC AND LUC
## 10KM
## SUPPORTING SCRIPTS
##
## Load network calculation functions
## Ceres Oct 2017
## --------------------------------------------------


## NETWORK SPATIALIZATION AND METRICS FUNCTION ------

## there are different network calc functions in this script.
## if simulating  LUC or CC, two functions are possible, one using BL networks built with a threshold of extinction
## the other without using a threshold of extinction (number of required prey)

## if NOT simulating LUC or CC (i.e. baseline networks),
network.calc <- if (doCC | doLUC | doIUCN) {
  if (useBLextthresh) {
    function(i) {
      pix <- as.character(master.SDMpres$PAGENAME[i])  ## get pix ID

      ## check whether this pixel exists in future habitat matrix, if not don't do anything.
      computeLocalWeb <- ifelse(doLUC, pix %in% fut.pixhabs$PAGENAME,
                                pix %in% orig.pixhabs2$PAGENAME)
      if (computeLocalWeb) {

        ## Creating storage matrix (each row is a pixel)
        metrics <- matrix(data = NA, nrow = 1, ncol = 21)
        metrics[1,1] <- as.character(master.SDMpres[i,1])  ## pixel names
        colnames(metrics) <- c("PAGENAME", "S", "L", "C", "Q", "dd.alpha", "dd.beta" ,
                               "normGen", "normVul", "SDnormGen", "SDnormVul",
                               "propB", "propI", "propT", "propOmn",
                               "mean.TL", "max.TL", "sd.TL",
                               "Pext", "Sext", "Invasiv")

        if (any(!is.na(master.SDMpres[i, -1])) & sum(master.SDMpres[i, -1], na.rm = TRUE) > 0) {   # skip if pixel had no spp in present
          ## CALCULATE BASELINE WEB - 2 steps, filter by habitats, then apply extinction threshold
          ## FILTER BY HABITATS
          orig.dietcat <- head(colnames(BARMdiet.binary), 11)
          orig.spp <- names(master.SDMpres)[which(master.SDMpres[i, -1]==1) + 1]          ## if not NA, then which species are present in that pixel (note that +1 is to skip pixel column name)
          orig.spp <- c(orig.dietcat, orig.spp)                           ## If spp are found in that pixel, then add the diet categories to the analysis
          orig.spp <- orig.spp[orig.spp %in% row.names(BARMdiet.binary)]  ## removing spp for which we have distributions, but that are not included on the metaweb

          bl.web <- BL_localweb(metaweb = BARMdiet.binary, SPPCODE = orig.spp,
                                SPP.HAB = BARM.habs2, PIX.HAB = orig.pixhabs2[pix,],
                                dietcat = orig.dietcat, HELP = FALSE)

          ## spp present in baseline web
          orig.spp2 <- colnames(bl.web)[!colnames(bl.web) %in% orig.dietcat]

          ## APPLY EXTINCTION THRESHOLD
          bl.web2 <- HabLoss_localweb(metaweb = BARMdiet.binary, SPPCODE = c(orig.dietcat, orig.spp2),
                                      SPP.HAB = BARM.habs2, PIX.HAB = orig.pixhabs2[pix,],
                                      EXT.TRSH = thresh, dietcat = orig.dietcat,
                                      HELP=FALSE)
          bl.web2 <- bl.web2[[1]] ## retain web only

          ## update spp present in baseline web
          orig.spp2 <- colnames(bl.web2)[!colnames(bl.web2) %in% orig.dietcat]

          ## check if baseline has spp NA - if not the pixel gets NA
          if (any(!is.na(bl.web2)) & sum(bl.web2, na.rm = TRUE) > 0) {
            ## CALCULATE WEB AFTER HABITAT AND SPECIES DISTRIB CHANGES
            ## check if there are any species predicted to exist in the future
            if (any(!is.na(master.fut[i, -1])) & sum(master.fut[i, -1], na.rm = TRUE) > 0) {
              if (doLUC) {
                if (doCC | doIUCN) {
                  if (doNOdisp) {
                    fut.spp <- names(master.fut[, -1])[which(master.fut[i, -1]==1)]    ## spp that will be present in that pixel (note that +1 is to skip pixel column name)
                    fut.spp <- fut.spp[fut.spp %in% orig.spp2]                         ## remove colonizer (new) species
                    fut.spp <- c(orig.dietcat, fut.spp)                                ## add the diet categories to the analysis

                    local.web <- SppDist_localweb(metaweb = BARMdiet.binary, ORIGSPP = orig.spp2,
                                                  SPP.HAB = BARM.habs2, PIX.HAB = fut.pixhabs[pix,],
                                                  FUTSPP = fut.spp, EXT.TRSH = thresh,
                                                  dietcat = orig.dietcat, HELP = FALSE)
                  } else {
                    fut.spp <- names(master.fut[, -1])[which(master.fut[i, -1]==1)]    ## will be present in that pixel (note that +1 is to skip pixel column name)
                    fut.spp <- c(orig.dietcat, fut.spp)                                ## add the diet categories to the analysis
                    fut.spp <- fut.spp[fut.spp %in% row.names(BARMdiet.binary)]        ## removing spp for which we have distributions, but that are not included on the metaweb

                    local.web <- SppDist_localweb(metaweb = BARMdiet.binary, ORIGSPP = orig.spp2,
                                                  SPP.HAB = BARM.habs2, PIX.HAB = fut.pixhabs[pix,],
                                                  FUTSPP = fut.spp, EXT.TRSH = thresh,
                                                  dietcat = orig.dietcat, HELP = FALSE)
                  }
                } else {
                  local.web <- HabLoss_localweb(metaweb = BARMdiet.binary, SPPCODE = c(orig.dietcat, orig.spp2),
                                                SPP.HAB = BARM.habs2, PIX.HAB = fut.pixhabs[pix,],
                                                EXT.TRSH = thresh, dietcat = orig.dietcat,
                                                HELP=FALSE)
                }
              } else {
                if (doNOdisp) {
                  fut.spp <- names(master.fut[, -1])[which(master.fut[i, -1]==1)]    ## spp that will be present in that pixel (note that +1 is to skip pixel column name)
                  fut.spp <- fut.spp[fut.spp %in% orig.spp2]                         ## remove colonizer (new) species
                  fut.spp <- c(orig.dietcat, fut.spp)                                ## add the diet categories to the analysis

                  local.web <- SppDist_localweb(metaweb = BARMdiet.binary, ORIGSPP = orig.spp2,
                                                SPP.HAB = BARM.habs2, PIX.HAB = orig.pixhabs2[pix,],
                                                FUTSPP = fut.spp, EXT.TRSH = thresh,
                                                dietcat = orig.dietcat, HELP = FALSE)
                } else {
                  fut.spp <- names(master.fut[, -1])[which(master.fut[i, -1]==1)]    ## will be present in that pixel (note that +1 is to skip pixel column name)
                  fut.spp <- c(orig.dietcat, fut.spp)                                ## add the diet categories to the analysis
                  fut.spp <- fut.spp[fut.spp %in% row.names(BARMdiet.binary)]        ## removing spp for which we have distributions, but that are not included on the metaweb

                  local.web <- SppDist_localweb(metaweb = BARMdiet.binary, ORIGSPP = orig.spp2,
                                                SPP.HAB = BARM.habs2, PIX.HAB = orig.pixhabs2[pix,],
                                                FUTSPP = fut.spp, EXT.TRSH = thresh,
                                                dietcat = orig.dietcat, HELP = FALSE)
                }
              }

              ## if the network has no species, do not calculate metrics, but still output No.SecCons prior to extinctions, Pext and Sext
              ## because network may have lost all its spp
              if (sum(dim(local.web[[1]])) == 0) {
                metrics[1, 2:ncol(metrics)] <- NA
                metrics[1, 19] <- length(local.web$Pext)
                metrics[1, 20] <- length(local.web$Sext)

                pixelXspp <- NA
              } else {

                ## diet categories are ignored to calculate S,L,C,normGen, normVul, propB, propI and propT
                ## ATTENTION: need to transpose webs to have consumers as columns
                metrics[1, 2:14] <- netw.metrics(web = t(local.web[[1]][!row.names(local.web[[1]]) %in% orig.dietcat, !colnames(local.web[[1]]) %in% orig.dietcat, drop = FALSE]),
                                                 normalise = TRUE, verbose = FALSE)

                ## Trophic level stats - dietcat should not be removed
                metrics[1, 15:18] = tlstats(web = t(local.web[[1]]), dietcat = orig.dietcat)

                ## no. of primary and secondary extinctions
                metrics[1, 19] <- length(local.web$Pext)
                metrics[1, 20] <- length(local.web$Sext)

                ## no. of invasive spp
                if (!doCC | doNOdisp) {
                  metrics[1, 21] = NA
                } else
                  metrics[1, 21] <- length(local.web$Invs)

                pixelXspp <- local.web[[1]]
              }
            } else {
              ## if no species were projected to exist, still count primary extinctions
              metrics[1, 2:ncol(metrics)] <- NA
              cols <- grep("^S$|^L$|^C$|^propB$|^propI$|^propT$|^mean.T$|^Invasiv$", colnames(metrics))
              metrics[1, cols] <- 0
              metrics[1, 19] <- length(orig.spp2)  ## Pext = BL S
              pixelXspp <- NA
            }
          } else{
            metrics[1, 2:ncol(metrics)] <- NA
            pixelXspp <- NA
          }
          write.table(metrics, file = file.path(outputs.temp, paste0("pix_metricsFUND_", pix, ".txt")))
          write.table(pixelXspp, file = file.path(outputs.temp, paste0("pix_webFUND_", pix, ".txt")))
        }
      }
    }
  } else {
    function(i) {
      pix = as.character(master.SDMpres$PAGENAME[i])  ## get pix ID

      ## check whether this pixel exists in future habitat matrix, if not don't do anything.
      computeLocalWeb <- ifelse(doLUC, pix %in% fut.pixhabs$PAGENAME,
                                pix %in% orig.pixhabs2$PAGENAME)
      if (computeLocalWeb) {

        ## Creating storage matrix (each row is a pixel)
        metrics <- matrix(data = NA, nrow = 1, ncol = 21)
        metrics[1,1] <- as.character(master.SDMpres[i,1])  ## pixel names
        colnames(metrics) = c("PAGENAME", "S", "L", "C", "Q", "dd.alpha", "dd.beta" ,
                              "normGen", "normVul", "SDnormGen", "SDnormVul",
                              "propB", "propI", "propT", "propOmn",
                              "mean.TL", "max.TL", "sd.TL",
                              "Pext", "Sext", "Invasiv")

        # skip if pixel had no spp in present
        if (any(!is.na(master.SDMpres[i, -1])) & sum(master.SDMpres[i, -1], na.rm = TRUE) > 0) {
          # CALCULATE BASELINE WEB
          orig.dietcat <- head(colnames(BARMdiet.binary), 11)
          orig.spp <- names(master.SDMpres)[which(master.SDMpres[i, -1]==1) + 1]          ## if not NA, then which species are present in that pixel (note that +1 is to skip pixel column name)
          orig.spp <- c(orig.dietcat, orig.spp)                           ## If spp are found in that pixel, then add the diet categories to the analysis
          orig.spp <- orig.spp[orig.spp %in% row.names(BARMdiet.binary)]  ## removing spp for which we have distributions, but that are not included on the metaweb

          bl.web <- BL_localweb(metaweb = BARMdiet.binary, SPPCODE = orig.spp,
                                SPP.HAB = BARM.habs2, PIX.HAB = orig.pixhabs2[pix,],
                                dietcat = orig.dietcat, HELP = FALSE)

          ## spp present in baseline web
          orig.spp2 <- colnames(bl.web)[!colnames(bl.web) %in% orig.dietcat]

          ## check if baseline has spp NA - if not the pixel gets NA
          if (any(!is.na(bl.web2)) & sum(bl.web2, na.rm = TRUE) > 0) {
            ## check if any species are predicted to exist in the future
            if (any(!is.na(master.fut[i, -1])) & sum(master.fut[i, -1], na.rm = TRUE) > 0) {
              if (doLUC) {
                if (doCC | doIUCN) {
                  if (doNOdisp) {
                    fut.spp <- names(master.fut[, -1])[which(master.fut[i, -1]==1)]    ## spp that will be present in that pixel (note that +1 is to skip pixel column name)
                    fut.spp <- fut.spp[fut.spp %in% orig.spp2]                         ## remove colonizer (new) species
                    fut.spp <- c(orig.dietcat, fut.spp)                                ## add the diet categories to the analysis

                    local.web <- SppDist_localweb(metaweb = BARMdiet.binary, ORIGSPP = orig.spp2,
                                                  SPP.HAB = BARM.habs2, PIX.HAB = fut.pixhabs[pix,],
                                                  FUTSPP = fut.spp, EXT.TRSH = thresh,
                                                  dietcat = orig.dietcat, HELP = FALSE)
                  } else {
                    fut.spp <- names(master.fut[, -1])[which(master.fut[i, -1]==1)]    ## will be present in that pixel (note that +1 is to skip pixel column name)
                    fut.spp <- c(orig.dietcat, fut.spp)                                ## add the diet categories to the analysis
                    fut.spp <- fut.spp[fut.spp %in% row.names(BARMdiet.binary)]        ## removing spp for which we have distributions, but that are not included on the metaweb

                    local.web <- SppDist_localweb(metaweb = BARMdiet.binary, ORIGSPP = orig.spp2,
                                                  SPP.HAB = BARM.habs2, PIX.HAB = fut.pixhabs[pix,],
                                                  FUTSPP = fut.spp, EXT.TRSH = thresh,
                                                  dietcat = orig.dietcat, HELP = FALSE)
                  }
                } else {
                  local.web <- HabLoss_localweb(metaweb = BARMdiet.binary, SPPCODE = c(orig.dietcat, orig.spp2),
                                                SPP.HAB = BARM.habs2, PIX.HAB = fut.pixhabs[pix,],
                                                EXT.TRSH = thresh, dietcat = orig.dietcat,
                                                HELP=FALSE)
                }
              } else {
                if (doNOdisp) {
                  fut.spp <- names(master.fut[, -1])[which(master.fut[i, -1]==1)]    ## spp that will be present in that pixel (note that +1 is to skip pixel column name)
                  fut.spp <- fut.spp[fut.spp %in% orig.spp2]                         ## remove colonizer (new) species
                  fut.spp <- c(orig.dietcat, fut.spp)                                ## add the diet categories to the analysis

                  local.web <- SppDist_localweb(metaweb = BARMdiet.binary, ORIGSPP = orig.spp2,
                                                SPP.HAB = BARM.habs2, PIX.HAB = orig.pixhabs2[pix,],
                                                FUTSPP = fut.spp, EXT.TRSH = thresh,
                                                dietcat = orig.dietcat, HELP = FALSE)
                } else {
                  fut.spp <- names(master.fut[, -1])[which(master.fut[i, -1]==1)]    ## will be present in that pixel (note that +1 is to skip pixel column name)
                  fut.spp <- c(orig.dietcat, fut.spp)                                ## add the diet categories to the analysis
                  fut.spp <- fut.spp[fut.spp %in% row.names(BARMdiet.binary)]        ## removing spp for which we have distributions, but that are not included on the metaweb

                  local.web <- SppDist_localweb(metaweb = BARMdiet.binary, ORIGSPP = orig.spp2,
                                                SPP.HAB = BARM.habs2, PIX.HAB = orig.pixhabs2[pix,],
                                                FUTSPP = fut.spp, EXT.TRSH = thresh,
                                                dietcat = orig.dietcat, HELP = FALSE)
                }
              }
              ## if the network has no species, do not calculate metrics, but still output No.SecCons prior to extinctions, Pext and Sext
              ## because network may have lost all its spp.
              ## some metrics get 0s
              if (sum(dim(local.web[[1]])) == 0) {
                metrics[1, 2:ncol(metrics)] <- NA
                cols <- grep("^S$|^L$|^C$|^propB$|^propI$|^propT$|^mean.T$|^Invasiv$", colnames(metrics))
                metrics[1, cols] <- 0
                metrics[1, 19] <- length(local.web$Pext)
                metrics[1, 20] <- length(local.web$Sext)

                pixelXspp <- NA
              } else {
                ## diet categories are ignored to calculate S,L,C,normGen, normVul, propB, propI and propT
                ## ATTENTION: need to transpose webs to have consumers as columns
                metrics[1, 2:14] <- netw.metrics(web = t(local.web[[1]][!row.names(local.web[[1]]) %in% orig.dietcat, !colnames(local.web[[1]]) %in% orig.dietcat, drop = FALSE]),
                                                 normalise = TRUE, verbose = FALSE)

                ## Trophic level stats - dietcat should not be removed
                metrics[1, 15:18] = tlstats(web = t(local.web[[1]]), dietcat = orig.dietcat)

                ## no. of primary and secondary extinctions
                metrics[1, 19] <- length(local.web$Pext)
                metrics[1, 20] <- length(local.web$Sext)

                ## no. of invasive spp
                if (!doCC | doNOdisp) {
                  metrics[1, 21] = NA
                } else
                  metrics[1, 21] <- length(local.web$Invs)

                pixelXspp <- local.web[[1]]
              }
            } else {
              ## if no species were projected in the future, still count primary extinctions
              metrics[1, 2:ncol(metrics)] <- NA
              cols <- grep("^S$|^L$|^C$|^propB$|^propI$|^propT$|^mean.T$|^Invasiv$", colnames(metrics))
              metrics[1, cols] <- 0
              metrics[1, 19] <- length(orig.spp2)  ## Pext = BL S
              pixelXspp <- NA
            }
          } else {
            metrics[1, 2:ncol(metrics)] <- NA
            pixelXspp <- NA
          }
          write.table(metrics, file = file.path(outputs.temp, paste0("pix_metricsFUND_", pix, ".txt")))
          write.table(pixelXspp, file = file.path(outputs.temp, paste0("pix_webFUND_", pix, ".txt")))
        }
      }
    }
  }
} else {
  if (useBLextthresh) {
    function(i) {
      pix = as.character(master.SDMpres$PAGENAME[i])  ## get pix ID

      ## check whether this pixel exists in the habitat matrix
      if (any(orig.pixhabs2$PAGENAME %in% pix)) {

        ## Creating storage matrix (each row is a pixel)
        metrics <- matrix(data = NA, nrow = 1, ncol = 19)
        metrics[1,1] <- pix  ## pixel names
        colnames(metrics) = c("PAGENAME", "S", "L", "C", "Q", "dd.alpha", "dd.beta" ,
                              "normGen", "normVul", "SDnormGen", "SDnormVul",
                              "propB", "propI", "propT", "propOmn",
                              "mean.TL", "max.TL", "sd.TL", "No.SecCons")
        ## skip if pixel had no spp in present
        if (any(!is.na(master.SDMpres[i, -1])) & sum(master.SDMpres[i, -1], na.rm = TRUE) > 0) {
          ## Species present from original distribution
          spp.pres <- names(master.SDMpres)[which(master.SDMpres[i, -1]==1) + 1]    ## if not NA, then which species are present in that pixel (note that +1 is to skip pixel column name)

          ## removing spp for which we have distributions, but that are not included on the metaweb
          spp.pres <- spp.pres[spp.pres %in% row.names(BARMdiet.binary)]

          ## CALCULATE INITIAL BL TO FILTER FALSE CO-OCCURRENCES
          bl.web <- BL_localweb(metaweb = BARMdiet.binary, SPPCODE = c(dietcat, spp.pres),
                                SPP.HAB = BARM.habs2, PIX.HAB = orig.pixhabs2[pix,],
                                dietcat = dietcat, HELP = FALSE)

          ## spp present in baseline web
          spp.pres2 <- colnames(bl.web)[!colnames(bl.web) %in% dietcat]

          bl.web2 <- HabLoss_localweb(metaweb = BARMdiet.binary, SPPCODE = c(dietcat, spp.pres2),
                                      SPP.HAB = BARM.habs2, PIX.HAB = orig.pixhabs2[pix,],
                                      EXT.TRSH = thresh, dietcat = dietcat,
                                      HELP=FALSE)

          bl.web2 <- bl.web2[[1]]    ## only interested in keeping the network

          ## CALCULATE METRICS -------------------------
          ## if the network has no species (because they are filtered out by hab requirements), do not calculate metrics
          if (sum(dim(bl.web2)) != 0) {
            ## diet categories are ignored to calculate S,L,C,normGen, normVul, propB, propI and propT
            ## ATTENTION: need to transpose webs to have consumers as columns
            metrics[1, 2:14] <- netw.metrics(web = t(bl.web2[!row.names(bl.web2) %in% dietcat,!colnames(bl.web2) %in% dietcat, drop = FALSE]),
                                             normalise = TRUE, verbose = FALSE)

            ## Trophic level stats - dietcat should not be removed
            metrics[1, 15:18] <- tlstats(web = t(bl.web2), dietcat = dietcat)

            ## save number of secondary consumers
            metrics[1, 19] <- sum(rowSums(bl.web2[!rownames(bl.web2) %in% dietcat, !colnames(bl.web2) %in% dietcat, drop = FALSE]) > 0)

            pixelXspp <- bl.web2
          } else{
            metrics[1, 2:ncol(metrics)] <- NA
            pixelXspp <- NA
          }
          ## save pixel results
          write.table(metrics, file = file.path(outputs.temp, paste0("pix_metricsFUND_", pix, ".txt")))
          write.table(pixelXspp, file = file.path(outputs.temp, paste0("pix_webFUND_", pix, ".txt")))
        }
      }
    }
  } else {
    function(i) {
      pix <- as.character(master.SDMpres$PAGENAME[i])  ## get pix ID

      ## check whether this pixel exists in habitat matrix
      if (any(orig.pixhabs2$PAGENAME %in% pix)) {
        ## Creating storage matrix (each row is a pixel)
        metrics <- matrix(data = NA, nrow = 1, ncol = 19)
        metrics[1,1] <- pix  ## pixel names
        colnames(metrics) <- c("PAGENAME", "S", "L", "C", "Q", "dd.alpha", "dd.beta" ,
                               "normGen", "normVul", "SDnormGen", "SDnormVul",
                               "propB", "propI", "propT", "propOmn",
                               "mean.TL", "max.TL", "sd.TL", "No.SecCons")
        ## skip if pixel has no spp in present
        if (any(!is.na(master.SDMpres[i, -1])) & sum(master.SDMpres[i, -1], na.rm = TRUE) > 0) {
          ## Species present from original distribution
          spp.pres <- names(master.SDMpres)[which(master.SDMpres[i, -1]==1) + 1]    ## if not NA, then which species are present in that pixel (note that +1 is to skip pixel column name)
          if (length(spp.pres)!= 0) spp.pres <- c(dietcat, spp.pres)         ## If spp are found in that pixel, then add the diet categories to the analysis

          ## removing spp for which we have distributions, but that are not included on the metaweb
          spp.pres <- spp.pres[spp.pres %in% row.names(BARMdiet.binary)]

          ## BUILD LOCAL WEB ---------------------------
          local.web <- BL_localweb(metaweb = BARMdiet.binary, SPPCODE = spp.pres,
                                   SPP.HAB = BARM.habs2, PIX.HAB = orig.pixhabs2[pix,],
                                   dietcat = dietcat, HELP=FALSE)

          ## CALCULATE METRICS -------------------------
          ## if the network has no species (because they are filtered out by hab requirements), do not calculate metrics
          if (sum(dim(local.web)) != 0) {
            ## diet categories are ignored to calculate S,L,C,normGen, normVul, propB, propI and propT
            ## ATTENTION: need to transpose webs to have consumers as columns
            metrics[1, 2:14] <- netw.metrics(web = t(local.web[!row.names(local.web) %in% dietcat,!colnames(local.web) %in% dietcat, drop = FALSE]),
                                             normalise = TRUE, verbose = FALSE)

            ## Trophic level stats - dietcat should not be removed
            metrics[1, 15:18] <- tlstats(web = t(local.web), dietcat = dietcat)

            ## save number of secondary consumers
            metrics[1, 19] <- sum(rowSums(local.web[!rownames(local.web) %in% dietcat, !colnames(local.web) %in% dietcat, drop = FALSE]) > 0)

            pixelXspp <- local.web
          } else{
            metrics[1, 2:ncol(metrics)] <- NA
            pixelXspp <- NA
          }
          ## save pixel results
          write.table(metrics, file = file.path(outputs.temp, paste0("pix_metricsFUND_", pix, ".txt")))
          write.table(pixelXspp, file = file.path(outputs.temp, paste0("pix_webFUND_", pix, ".txt")))
        }
      }
    }
  }
}



## PARALLELIZATION FUNCTION -------------------------
## scen is the name of the scenario (used as a folder name)
## scen is the name of the tempoary folder
## noCPUs is the number of CPUs used for parallel computing, defaults to 2
## do.pix can be "all", "missing" (calculates misisng pixels) or a vector of the pix numbers/IDs to be calculated
## parallel activates paralellisation using snowfall (WinOS) or parallel for the number of CPUs specifiec

parallelize.networkCalc <- function(outputs.dir, scen, temp.folder, do.pix = "all",
                                    parallel = FALSE, noCPUs = 2) {
  if (do.pix != "all" & do.pix != "missing" &
      (length(do.pix) < 1 | !grepl("character|integer|numeric", class(do.pix)))) {
    stop("Choose pixel numbers/IDs to calculate, or do.pix = 'all', or do.pix = 'missing'")
  }

  ## Create outputs folders (<<- puts objects in GlobalEnvironment - needed for lapply and parallel)
  outputs.folder <<- file.path(outputs.dir, scen)
  outputs.temp <<- file.path(outputs.folder, temp.folder)    ## temporary folder where pixel-level files will be stored before binding

  ## if the temporary directory was zipped, extract and delete zip folder
  if (!dir.exists(outputs.temp) & file.exists(paste0(outputs.temp, ".zip"))) {
    dir.create(outputs.temp, recursive = TRUE)
    unzip(zipfile = paste0(outputs.temp, ".zip"), junkpaths = TRUE,
          exdir = outputs.temp)
    file.remove(paste0(outputs.temp, ".zip"))
  } else {
    dir.create(outputs.temp, recursive = TRUE)
  }

  ## run for all pixels?
  if (do.pix == "all") {
    pix <-  1:nrow(master.SDMpres)
  } else if (do.pix == "missing") {
    ## if final output file exists, check it first for missing pixels
    fileNameMetrics <- list.files(outputs.folder, pattern = paste0("metrics10kWdietFUND_", outfile.suff), full.names = TRUE)
    if (length(fileNameMetrics) > 1) {
      stop(paste("Found several", paste0("metrics10kWdietFUND_", outfile.suff), "files in ",
                 outputs.folder, ". Please keep only ONE"))
    }
    if (length(fileNameMetrics)) {
      if (grepl("\\.rds$", raster::extension(basename(fileNameMetrics)))) {
        metrics <- readRDS(fileNameMetrics)
      } else if (grepl("\\.RData$|\\.Rdata$", raster::extension(basename(fileNameMetrics)))) {
        load(fileNameMetrics)
      } else stop(paste(fileNameMetrics, "needs to be .Rdata/RData/.rds"))

      pix.done <- as.character(metrics$PAGENAME)
      pix.done <- which(master.SDMpres$PAGENAME %in% pix.done)  ## convert to indices in masterSDMpres
      pix <- which(!c(1:nrow(master.SDMpres)) %in% pix.done)

      ## some pixels ended up with null networks - repeat them.
      fileNamePixelSpp <- list.files(outputs.folder, pattern = paste0("spp10kWdietFUND_", outfile.suff), full.names = TRUE)
      if (length(fileNamePixelSpp) > 1) {
        stop(paste("Found several", paste0("spp10kWdietFUND_", outfile.suff), "files in ",
                   outputs.folder, ". Please keep only ONE"))
      }
      if (grepl("\\.rds$", raster::extension(basename(fileNamePixelSpp)))) {
        pixelXspp.ls <- readRDS(fileNamePixelSpp)
      } else if (grepl("\\.RData$|\\.Rdata$", raster::extension(basename(fileNamePixelSpp)))) {
        load(fileNamePixelSpp)
      } else stop(paste(fileNamePixelSpp, "needs to be .Rdata/RData/.rds"))

      nullWebs <- sapply(pixelXspp.ls, is.null)
      nullWebs <- names(which(nullWebs))   # don't use indices
      nullWebs <- which(master.SDMpres$PAGENAME %in% nullWebs)  ## convert to indices in masterSDMpres

      if (length(nullWebs) & any(!nullWebs %in% pix)) {
        pix <- c(pix, nullWebs[!nullWebs %in% pix])
      }

      ## clean ws
      rm(metrics, pixelXspp.ls, nullWebs); for (i in 1:10) gc()
    } else {
      pix.done <- sub(".txt", "", sub(".*_", "", list.files(outputs.temp, full.names = TRUE, pattern = "pix_metricsFUND_")))
      pix.done <- which(master.SDMpres$PAGENAME %in% pix.done)  ## convert to indices in masterSDMpres
      pix <- which(!c(1:nrow(master.SDMpres)) %in% pix.done)
    }
  } else if (class(do.pix) == "character") {
    pix <- which(master.SDMpres$PAGENAME %in% do.pix)
  } else if (class(do.pix) == "numeric" | class(do.pix) == "integer") {
    pix <- do.pix
  }

  if (length(pix)) {
    cat(paste("***************\n", basename(outputs.dir), scen,
              params[1], params[2], ":\n", date(), "\n"))
    cat(paste(length(pix), " pixels to compute\n"))

    if (!parallel) {
      lapply(pix, network.calc)
    } else {
      plan(multisession, workers = noCPUs)
      tables <- future_lapply(pix, network.calc)
      future:::ClusterRegistry("stop")
      tables
    }
  } else warning("No pixels to compute")


  ## --------------------------------------------------
  ## Compiling pixel outputs
  cat(paste0("*compiling outputs and saving...\n"))
  metrics.ls <- list.files(outputs.temp, full.names = TRUE, pattern = "pix_metricsFUND_")
  pix_web.ls <- list.files(outputs.temp, full.names = TRUE, pattern = "pix_webFUND_")

  metrics <- rbindlist(lapply(metrics.ls, FUN = function(x) {
    metrics.tbl <- tryCatch(read.table(x), error = function(x) return(NULL))
    if (!is.null(metrics.tbl)) return(metrics.tbl)
  }))

  pixelXspp.ls <- lapply(pix_web.ls, FUN = function(x) {
    pixelXspp.mat <- tryCatch(Matrix::Matrix(as.matrix(read.table(x)), sparse = TRUE),
                              error = function(x) return(NULL))
    if (!is.null(pixelXspp.mat)) return(pixelXspp.mat)
  })

  names(pixelXspp.ls) <- sub(paste0(outputs.temp, "/pix_webFUND_"), "", sub(".txt", "", pix_web.ls))


  ## saving compiled results
  ## if all pixels were calculated we can save metrics directly,
  ## otherwise they will need to be appended to the existing file
  if (do.pix == "all") {
    saveRDS(metrics, file = file.path(outputs.folder, paste0("metrics10kWdietFUND_", outfile.suff, ".rds")))
    saveRDS(pixelXspp.ls, file = file.path(outputs.folder, paste0("spp10kWdietFUND_", outfile.suff, ".rds")))

  } else {
    fileNameMetrics <- list.files(outputs.folder, pattern = paste0("metrics10kWdietFUND_", outfile.suff), full.names = TRUE)
    if (length(fileNameMetrics) > 1) {
      stop(paste("Found several", paste0("metrics10kWdietFUND_", outfile.suff), "files in ",
                 outputs.folder, ". Please keep only ONE"))
    }
    if (length(fileNameMetrics)) {
      metrics2 <- metrics
      if (grepl("\\.rds$", raster::extension(basename(fileNameMetrics)))) {
        metrics <- readRDS(fileNameMetrics)
      } else if (grepl("\\.RData$|\\.Rdata$", raster::extension(basename(fileNameMetrics)))) {
        load(fileNameMetrics)
      } else stop(paste(fileNameMetrics, "needs to be .Rdata/RData/.rds"))

      ## replace pixels that might have been repeated
      pixReplace <- unique(as.character(metrics[PAGENAME %in% metrics2$PAGENAME, PAGENAME]))
      metrics[PAGENAME %in% pixReplace, names(metrics) := metrics2[PAGENAME %in% pixReplace]]

      ## now rbind (to get any other pixels) and remove duplicates
      metrics <- rbind(metrics, metrics2)
      metrics <- metrics[!duplicated(metrics),]

      if (grepl("\\.rds$", raster::extension(basename(fileNameMetrics)))) {
        saveRDS(metrics, file = file.path(outputs.folder, paste0("metrics10kWdietFUND_", outfile.suff, ".rds")))
      } else if (grepl("\\.RData$|\\.Rdata$", raster::extension(basename(fileNameMetrics)))) {
        save(metrics, file = file.path(outputs.folder, paste0("metrics10kWdietFUND_", outfile.suff, ".Rdata")))
      }
      rm(metrics2); for (i in 1:10) gc()  ## clean ws and free memory
    } else {
      saveRDS(metrics, file = file.path(outputs.folder, paste0("metrics10kWdietFUND_", outfile.suff, ".rds")))
    }

    fileNamePixelSpp <- list.files(outputs.folder, pattern = paste0("spp10kWdietFUND_", outfile.suff), full.names = TRUE)
    if (length(fileNamePixelSpp) > 1) {
      stop(paste("Found several", paste0("spp10kWdietFUND_", outfile.suff), "files in ",
                 outputs.folder, ". Please keep only ONE"))
    }
    if (length(fileNamePixelSpp)) {
      pixelXspp.ls2 <- pixelXspp.ls
      if (grepl("\\.rds$", raster::extension(basename(fileNamePixelSpp)))) {
        pixelXspp.ls <- readRDS(fileNamePixelSpp)
      } else if (grepl("\\.RData$|\\.Rdata$", raster::extension(basename(fileNamePixelSpp)))) {
        load(fileNamePixelSpp)
      } else stop(paste(fileNamePixelSpp, "needs to be .Rdata/RData/.rds"))

      ## replace pixels that might have been repeated
      pixReplace <- unique(names(pixelXspp.ls)[names(pixelXspp.ls) %in% names(pixelXspp.ls2)])
      pixelXspp.ls[pixReplace] <- pixelXspp.ls2[pixReplace]

      ## now bind (to get any other pixels) and remove duplicates
      pixelXspp.ls <- c(pixelXspp.ls, pixelXspp.ls2)
      pixelXspp.ls <- pixelXspp.ls[!duplicated(names(pixelXspp.ls))]

      if (grepl("\\.rds$", raster::extension(basename(fileNamePixelSpp)))) {
        saveRDS(pixelXspp.ls, file = file.path(outputs.folder, paste0("spp10kWdietFUND_", outfile.suff, ".rds")))
      } else if (grepl("\\.RData$|\\.Rdata$", raster::extension(basename(fileNamePixelSpp)))) {
        save(pixelXspp.ls, file = file.path(outputs.folder, paste0("spp10kWdietFUND_", outfile.suff, ".Rdata")))
      }

      rm(pixelXspp.ls2); for (i in 1:10) gc()  ## clean ws and free memory
    } else {
      saveRDS(pixelXspp.ls, file = file.path(outputs.folder, paste0("spp10kWdietFUND_", outfile.suff, ".rds")))
    }
  }

  ## deleting temporary files - need to check if pixels missing had species or not
  # rownames(master.SDMpres) = as.character(master.SDMpres$PAGENAME)
  # pix.todo = orig.pixhabs2$PAGENAME[!orig.pixhabs2$PAGENAME %in% as.character(metrics$PAGENAME)]

  # if (sum(rowSums(master.SDMpres[pix.todo,-1], na.rm = TRUE)) == 0 & nrow(metrics) == length(pixelXspp.ls)) {   ## if there are no spp in the missing pixels and outputs have the same number of pixels, delete temp files
  fileNameMetrics <- list.files(outputs.folder, pattern = paste0("metrics10kWdietFUND_", outfile.suff), full.names = TRUE)
  if (length(fileNameMetrics) > 1) {
    stop(paste("Found several", paste0("metrics10kWdietFUND_", outfile.suff), "files in ",
               outputs.folder, ". Please keep only ONE"))
  }

  fileNamePixelSpp <- list.files(outputs.folder, pattern = paste0("spp10kWdietFUND_", outfile.suff), full.names = TRUE)
  if (length(fileNamePixelSpp) > 1) {
    stop(paste("Found several", paste0("spp10kWdietFUND_", outfile.suff), "files in ",
               outputs.folder, ". Please keep only ONE"))
  }

  if (length(fileNameMetrics)) {
    if (length(fileNamePixelSpp)) {
      unlink(outputs.temp, recursive = TRUE)
    } else cat("Outputs (webs) have not been saved\n")
  } else cat("Outputs (metrics) have not been saved\n")
  # } else cat("No. rows in metrics and orighabs2 file differs\n")

  cat(paste0("*done!\n", date(), "\n***************\n"))
}
