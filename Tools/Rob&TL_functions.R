## -----------------------------------------------------------------
## ROBUSTNESS AND TROPHIC LEVEL - functions only
## 10KM
## SUPPORTING SCRIPTS - For workflow demo
##
## Ceres Aug 2024
## -----------------------------------------------------------------

futureWrap <- function(webs, out.type = "data.table", ncores = 3) {
  if (ncores <= 0 || is.null(ncores))
    stop("provide ncores >= 1")
  if (ncores == 1) {
    ## faster than using future sequentially
    tlDT.ls <- Map(web = webs,
                   f = function(web, out.type) {
                     web <- t(as.matrix(web))
                     out <- try(ToolsCB:::calcSppTL(web, out.type = out.type))
                   },
                   MoreArgs = list(out.type = out.type))
  } else {
    plan(multisession, workers = ncores)
    tlDT.ls <- future_Map(web = webs,
                          f = function(web, out.type) {
                            web <- t(as.matrix(web))
                            out <- try(ToolsCB:::calcSppTL(web, out.type = out.type))
                          },
                          MoreArgs = list(out.type = out.type))
    future:::ClusterRegistry("stop")
  }
  return(tlDT.ls)
}

tlcalcFun <- function(scenstr, PextFile, SextFile) { ## get table of primary extinctions
  ## get tables of primary and secondary extinctions
  masterPext <- readRDS(PextFile)
  masterSext <- readRDS(SextFile)

  ## get baseline web
  pixelXsppBL.ls <- readRDS(list.files(dirname(masterBL.files), "^spp", full.names = TRUE))

  ## list pixels where extinctions occurred
  tetra <- union(grep("^(A|M|B|R)[[:digit:]]{1,3}", names(masterPext), value = TRUE),
                 grep("^(A|M|B|R)[[:digit:]]{1,3}", names(masterSext), value = TRUE))

  pixWExt <- c(masterPext[["PAGENAME"]][which(rowSums(masterPext[, ..tetra]) > 0)],
               masterSext[["PAGENAME"]][which(rowSums(masterSext[, ..tetra]) > 0)]) |>
    unique()

  for (i in 1:3) gc(reset = TRUE)

  cacheExtra <- reproducible::CacheDigest(pixelXsppBL.ls[pixWExt])
  tlDT.ls <- Cache(futureWrap,
                   webs = pixelXsppBL.ls[pixWExt],
                   omitArgs = c("webs", "ncores"),
                   .cacheExtra = cacheExtra,
                   userTags = c("speciesTL", scenstr))

  for (i in 1:3) gc(reset = TRUE)

  tlDT <- rbindlist(tlDT.ls, fill = TRUE, use.names = TRUE)
  tlDT[, PAGENAME := names(tlDT.ls)]

  rm(tlDT.ls)

  ## remove diet cat
  notdiet <- setdiff(names(tlDT), dietcat)
  tlDT <- tlDT[, ..notdiet]

  for (i in 1:3) gc(reset = TRUE)
  return(list(tlDT = tlDT, masterPext = masterPext, masterSext = masterSext))
}

## filter TL measurements per Pext/Sext
filterTL <- function(tlDT, masterPext, masterSext) {
  tetra <- grep("^(A|M|B|R)[[:digit:]]{1,3}", names(tlDT), value = TRUE)

  cacheExtra <- list(summary(tlDT), colSums(masterPext[, ..tetra]))
  tlDTPext <- Cache(filterByTable,
                    metricDT = tlDT,
                    binaryDT = masterPext,
                    omitArgs = c("userTags", "metricDT", "binaryDT"),
                    .cacheExtra = cacheExtra,
                    userTags = c("filterTL", "Pext"))

  cacheExtra <- list(summary(tlDT), colSums(masterSext[, ..tetra]))
  tlDTSext <- Cache(filterByTable,
                    metricDT = tlDT,
                    binaryDT = masterSext,
                    omitArgs = c("userTags", "metricDT", "binaryDT"),
                    .cacheExtra = cacheExtra,
                    userTags = c("filterTL", "Sext"))

  ## convert to long format, rm NAs and rbind
  tlDTPext <- melt(tlDTPext, id.vars = "PAGENAME", variable.name = "spp", value = "TL")
  tlDTPext <- tlDTPext[complete.cases(tlDTPext)]
  tlDTSext <- melt(tlDTSext, id.vars = "PAGENAME", variable.name = "spp", value = "TL")
  tlDTSext <- tlDTSext[complete.cases(tlDTSext)]

  tlDT <- rbindlist(list(Pext = tlDTPext, Sext = tlDTSext), idcol = "Ext", use.names = TRUE)
  for (i in 1:3) gc(reset = TRUE)

  return(tlDT)
}
