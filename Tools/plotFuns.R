## ggplot wrappers

## Maps of network metrics
plotMetricsMaps <- function(data, x = "x", y = "y", fill, pal = "viridis",
                            wrapVar = "Scen", wrapLabeller = scenLab,
                            title, xlab = "longitude", ylab = "latitude", fillLab = "") {
  x <- ensym(x)
  y <- ensym(y)
  fill <- ensym(fill)

  ggplot(data) +
    geom_tile(aes(x = !!x, y = !!y, fill = !!fill)) +
    scale_fill_viridis_c(direction = -1, option = pal, na.value = "white") +
    coord_equal() +
    theme_pubr(base_size = 12, legend = "right") +
    theme(panel.background = element_rect(fill = "grey90"),
          plot.margin = margin(0,0,0,0)) +
    facet_wrap(paste("~", wrapVar), labeller = labeller(Scen = as_labeller(wrapLabeller))) +
    labs(title = title, x = xlab, y = ylab, fill = fillLab)
}

## Single predictor scatterplots with/without predicted values line
scatterPlot <- function(data, x, pointY, lineY, colour, colVals, colLabs,
                        xlab = "x", ylab = "Robustness", colourlab = "Scenario",
                        linetypelab = "", drawplot = FALSE, ...) {
  x <- ensym(x)
  pointY <- ensym(pointY)
  lineY <- ensym(lineY)
  colour <- ensym(colour)

  plot <- ggplot(data,
                 aes(x = !!x)) +
    geom_point(aes(y = !!pointY, colour = !!colour), alpha = 0.5)

  if (!missing(lineY)) {
    plot <- plot +
      geom_line(aes(y = !!lineY), linewidth = 1.2, colour = "black")
  }
  plot <- plot +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_colour_manual(values = colVals, labels = colLabs) +
    theme_pubr() +
    theme(panel.grid.major.y = element_line(colour = "grey", linetype = "dotted")) +
    theme(legend.box = "vertical") +
    labs(y = ylab, x = xlab, colour = colourlab)

  if (drawplot)
    print(plot)

  if (length(list(...)))
    ggsave(plot = plot, ...)

  return(plot)
}

## Boxplots of network metrics by bins of robustness
binnedBoxplots <- function(data, x, y, colour,
                           ylims, xlabels, xlab = x, ylab = y,
                           colourlab = colour, title = "",
                           drawplot = FALSE, coordFlip = TRUE, ...) {

  if (missing(ylims)) {
    ylims <- range(data[, ..y])
  }

  if (missing(xlabels)) {
    xlabels <- unique(data[, ..x])
    names(xlabels) <- xlabels
  }

  x <- ensym(x)
  y <- ensym(y)
  colour <- ensym(colour)

  plot <- ggplot(data,
                 aes(x = !!x, y = !!y, colour = robBin)) +
    geom_point(position = position_jitterdodge(), alpha = 0.1, size = 0.4) +
    geom_boxplot(fill = NA, linewidth = 0.9, outlier.shape = NA) +
    scale_colour_discrete_sequential(palette = "Teal", nmax = 5, order = 2:5) +
    scale_y_continuous(limits = ylims) +
    scale_x_discrete(labels = xlabels) +
    theme_pubr(legend = "right") +
    theme(panel.grid.major.y = element_line(colour = "grey", linetype = "dotted")) +
    theme(legend.box = "vertical") +
    guides(colour = guide_legend(override.aes = list(linewidth = 1)))

  if (coordFlip) {
    plot <- plot +
      coord_flip() +
      labs(title = title,
           y = xlab, x = ylab, colour = colourlab)
  } else {
    plot <- plot +
      labs(title = title,
           y = ylab, x = xlab, colour = colourlab)
  }

  if (drawplot)
    print(plot)

  if (length(list(...)))
    ggsave(plot = plot, ...)

  return(plot)
}


## trophic level by extinction type (Pext/Sext)
TLplotsFun <- function(tlDT, scenstr, quant, fig.dir) {
  ## boxplot
  cols <- c(Pext = "#1993b0", Sext = "#ffbb08")
  labs <- c(Pext = "primary ext.", Sext = "secondary ext.")
  plotBP <- ggplot(tlDT, aes(x = Ext, y = TL, fill = Ext)) +
    geom_boxplot(linewidth = 0.3) +
    scale_fill_manual(labels = labs, values = cols, name = "") +
    scale_x_discrete(labels = c(Pext = "primary ext.",
                                Sext = "secondary ext."),
                     name = "")  +
    scale_y_continuous(name = "Trophic level") +
    theme_pubr()
  ggsave(plot = plotBP, width = 8, height = 5,
         file.path(fig.dir, paste("PextSextTL", scenstr, quant, "boxplots.png", sep = "_")))

  ## histogram
  plotHist <- ggplot(tlDT, aes(x = TL, fill = Ext, colour = Ext)) +
    geom_histogram(position = "identity", binwidth = 0.1, alpha = 0.3,
                   linewidth = 0.3) +
    scale_fill_manual(labels = labs, values = cols, name = "") +
    scale_colour_manual(labels = labs, values = cols, name = "") +
    scale_x_continuous(name = "Trophic level") +
    theme_pubr()
  ggsave(plot = plotHist, width = 7, height = 5,
         file.path(fig.dir,
                   paste("PextSextTL", scenstr,
                         quant, "hist.png", sep = "_")))

  ## density
  plotDens <- ggplot(tlDT, aes(x = TL, fill = Ext)) +
    geom_density(alpha = 0.5, linewidth = 0.3) +
    scale_fill_manual(labels = labs, values = cols, name = "") +
    scale_x_continuous(name = "Trophic level") +
    theme_pubr()
  ggsave(plot = plotDens, width = 7, height = 5,
         file.path(fig.dir,
                   paste("PextSextTL", scenstr,
                         quant, "density.png", sep = "_")))

  return(list(plotBP = plotBP, plotHist = plotHist, plotDens = plotDens))
}

## trophic level by extinction type (Pext/Sext) and level of robustness
TLRobplotsFun <- function(res.dir, tlDT, quant, scenstr, fig.dir) {
  cols <- c(Pext = "#1993b0", Sext = "#ffbb08")
  labs <- c(Pext = "primary ext.", Sext = "secondary ext.")

  all_metrics <- list.files(res.dir, "all_metrics", recursive = TRUE, full.names = TRUE) |>
    grep(quant, x = _, value = TRUE) |>
    readRDS()

  if (scenstr %in% all_metrics$IUCN) {
    all_metrics <- all_metrics[IUCN == scenstr]
  } else {
    scenstr2 <- unlist(strsplit(scenstr, split = "_"))
    all_metrics <- all_metrics[GCM == scenstr2[1] & RCP == scenstr2[2]]
  }

  all_metrics[, invRobust := 1-Robust]

  tlDT <- all_metrics[, .(PAGENAME, invRobust, BL_S)][tlDT, on = .(PAGENAME)]

  brks <- quantile(tlDT$invRobust, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)
  ## 0%, 5% and 25% quantiles of robustness were identical (0)
  ubrks <- unique(brks)
  ## in case some quantiles have the same value, get the highest
  names(ubrks) <- rev(names(brks))[match(ubrks, rev(brks))]

  ## keep 0s and 1s separate
  tlDT[invRobust > 0 | invRobust < 1,
       invRobustbins := cut(invRobust, breaks = ubrks, right = TRUE,
                            include.lowest = FALSE)]
  intervals <- as.numeric(unique(tlDT$invRobustbins))
  intervals <- c(1, intervals + 1, length(intervals) + 1)
  names(intervals) <- c("0", as.character(unique(tlDT$invRobustbins)), "1")
  intervals <- intervals[order(intervals)]

  tlDT[, invRobustbins := as.numeric(invRobustbins) + 1]
  tlDT[invRobust == 0, invRobustbins := 1]
  tlDT[invRobust == 1, invRobustbins := max(invRobustbins, na.rm = TRUE) + 1]
  tlDT[, invRobustbins := ordered(invRobustbins, levels = na.omit(intervals),
                                  labels = names(na.omit(intervals)))]


  labs2 <- c(bquote("0"))
  j <- 1
  for (i in 2:length(ubrks)) {
    newQ <- if (i == 2) {
      bquote(atop("("*.(round(ubrks[j], 2))*","*.(round(ubrks[i], 2))*"]",
                          ""<=.(names(ubrks[i]))))
    } else if (i == length(ubrks)) {
      bquote(atop("["*.(round(ubrks[j], 2))*","*.(round(ubrks[i], 2))*")",
                  "">=.(names(ubrks[j]))))
    } else {
      bquote(atop("["*.(round(ubrks[j], 2))*","*.(round(ubrks[i], 2))*"]",
                  .(names(ubrks[j]))*"-"*.(names(ubrks[i]))))
    }
    labs2 <- c(labs2, newQ)
    j <- j + 1
  }

  labs2 <- c(labs2, bquote("1"))
  names(labs2) <- names(na.omit(intervals))

  ## boxplots
  plotBP <- ggplot(tlDT[complete.cases(tlDT)],
                   aes(x = invRobustbins, y = TL, fill = Ext)) +
    geom_point(colour = "grey", alpha = 0.5,
               position = position_jitterdodge(jitter.width = 0.20), size = 0.7) +
    geom_boxplot(linewidth = 0.3, alpha = 0.5) +
    scale_fill_manual(labels = labs, values = cols, name = "") +
    scale_x_discrete(name = "Robustness\n(quantile breaks)", labels = labs2) +
    scale_y_continuous(name = "Trophic level") +
    theme_pubr()
  ggsave(plot = plotBP, width = 8, height = 5,
         file.path(fig.dir,
                   paste("PextSextTL_Rob", scenstr,
                         quant, "boxplots&points.png", sep = "_")))
  ## remove points and save
  plotBP2 <- plotBP
  plotBP2$layers <- plotBP2$layers[2]
  ggsave(plot = plotBP2, width = 8, height = 5,
         file.path(fig.dir,
                   paste("PextSextTL_Rob", scenstr,
                         quant, "boxplots.png", sep = "_")))


  ## violins + medians
  plotViolin <- ggplot(tlDT[complete.cases(tlDT)],
                       aes(x = invRobustbins, y = log(TL), fill = Ext, colour = Ext)) +
    geom_violin(alpha = 0.3, linewidth = 0.3) +
    stat_summary(fun = "median", geom = "point", position = position_dodge(width = 0.9)) +
    scale_fill_manual(labels = labs, values = cols, name = "") +
    scale_colour_manual(labels = labs, values = cols, name = "") +
    scale_x_discrete(name = "Robustness\n(quantile interval)",
                     labels = labs2) +
    scale_y_continuous(name = "log(trophic level)") +
    theme_pubr()
  ggsave(plot = plotViolin, width = 8, height = 5,
         file.path(fig.dir,
                   paste("PextSextTL_Rob", scenstr,
                         quant, "violins.png", sep = "_")))

  ## pointrange
  plotPoint <- ggplot(tlDT[complete.cases(tlDT)],
                      aes(x = invRobustbins, y = TL, colour = Ext)) |>
    add_summary(fun = "median_iqr", group = "Ext", error.plot = "pointrange",
                size = 0.7) +
    scale_colour_manual(labels = labs, values = cols, name = "") +
    scale_x_discrete(name = "Robustness\n(quantile interval)",
                     labels = labs2) +
    scale_y_continuous(name = "Trophic level") +
    theme_pubr()
  ggsave(plot = plotPoint, width = 8, height = 5,
         file.path(fig.dir,
                   paste("PextSextTL_Rob", scenstr,
                         quant, "points.png", sep = "_")))

  plotData <- tlDT[, list(medTL = median(TL, na.rm = TRUE),
                          iqrTL = IQR(TL, na.rm = TRUE)), by = .(PAGENAME, Ext)]
  plotData <- unique(tlDT[, .(BL_S, invRobust, invRobustbins, PAGENAME)])[plotData, on = .(PAGENAME)]
  plotPoint2 <- ggplot(plotData[complete.cases(plotData)],
                       aes(x = invRobustbins, y = medTL, colour = BL_S,
                           group = Ext, shape = Ext)) +
    geom_point(alpha = 0.5, position = position_jitterdodge(jitter.width = 0.25)) +
    scale_colour_viridis_c(name = "Initial species\nrichness", direction = -1) +
    scale_x_discrete(name = "Robustness\n(quantile interval)",
                     labels = labs2) +
    scale_shape_discrete(name = "",
                         labels = c("Pext" = "Primary extinctions",
                                    "Sext" = "Secondary extinctions")) +
    scale_y_continuous(name = "Median species trophic level") +
    theme_pubr(legend = "right")
  ggsave(plot = plotPoint2, width = 8, height = 5,
         file.path(fig.dir,
                   paste("PextSextTL_Rob_BLS", scenstr,
                         quant, "points.png", sep = "_")))


  return(list(plotBP = plotBP, plotViolin = plotViolin, plotPoint = plotPoint, plotPoint2 = plotPoint2))
}
