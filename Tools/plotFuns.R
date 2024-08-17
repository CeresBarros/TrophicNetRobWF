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