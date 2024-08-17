## ----------------------------------------
## LABELS AND COLOURS FOR FIGURES
## ----------------------------------------
scenLab <- if (exists("all_metrics")) {
 as.character(unique(all_metrics$Scen))
} else {
  if (exists("tempBetaDiv")) {
    as.character(unique(tempBetaDiv$Scen))
  } else {
    if (exists("spaceBetaDiv")) {
      as.character(unique(spaceBetaDiv$Scen))
    }
  }
}
names(scenLab) <- scenLab
scenLab <- sub(".*rcp.*", "CC\nno IUCN ext.", scenLab)
scenLab <- gsub("_{1,}", " ", sub("(noLUC)(.*)(noInvs)", "\\2",
                                  sub(".*noCC", "no CC\n", scenLab)))
scenCols <- RColorBrewer::brewer.pal(n = 8, name = "BrBG")[c(2,6:8)]
names(scenCols) <- sort(names(scenLab))
