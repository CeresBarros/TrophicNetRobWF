## -----------------------------------------------------------------
## Publication figures
## 10KM
## SUPPORTING SCRIPTS - For workflow demo
## 
## Ceres Aug 2024
## -----------------------------------------------------------------

hd_rcp85_wm_bin_RFTLRobPlots <- qs::qread(file.path(fig.dir, "hd_rcp85_wm_bin_RFTLRobPlots.qs"))
CR_EN_VUTLRobPlots <- qs::qread(file.path(fig.dir, "CR_EN_VUTLRobPlots.qs"))
robMap2 <- qs::qread(file.path(fig.dir, "robMap2.qs"))

p1 <- robMap2 +
  theme_pubr(margin = FALSE, base_size = 14) +
  theme(strip.background = element_blank(),,
        strip.text = element_blank()) +
  labs(title = "Climate change", fill = "Robustness", 
       y = "Latitude", x = " ") 
p2 <- p1 + labs(title = "IUCN extinctions", 
                x = "Longitude") 
p1$data <- p1$data[Scen == "hd_rcp85_noLUC_Invs_noIUCNext"]
p2$data <- p2$data[Scen == "noCC_noCC_noLUC_noInvs_CR_EN_VU"]

p3 <- hd_rcp85_wm_bin_RFTLRobPlots$plotBP2 +
  theme_pubr(margin = FALSE, base_size = 14) +
  scale_y_continuous(limits = c(1, 5),
                     name = "Trophic level")

p4 <- CR_EN_VUTLRobPlots$plotBP2 +
  theme_pubr(margin = FALSE, base_size = 14) +
  scale_y_continuous(limits = c(1, 5),
                     name = "Trophic level") 

plotOut <- p1 + p2 + get_legend(p1) + 
  p3 + p4 + 
  get_legend(p3 + theme(legend.direction = "horizontal", legend.box = "vertical", legend.spacing = unit(0.1, "cm"))) +
  plot_layout(axis_titles = "collect_x",
              ncol = 2, design = "144
                                  255
                                  366",
              heights = c(2, 2, 0.3)) & 
  theme(legend.position = "none") 

plotOut <- plotOut +
  plot_annotation(tag_levels = list(c("a)", "b)"), ""))

ggsave(plot = plotOut, filename = file.path(fig.dir, "robMap_boxplotsTLXRobBPs.png"),
       dpi = 300, height = 9, width = 13)




textData <- hd_rcp85_wm_bin_RFTLRobPlots$plotPoint2$data
textData <- textData[, list(medTL = max(textData$medTL, na.rm = TRUE) + 0.2),
                     by = .(Ext, invRobustbins)]
textData[Ext == "Pext", `:=`(lab = "Pri.")]
textData[Ext == "Sext", `:=`(lab = "Sec.")]
  
p5 <- hd_rcp85_wm_bin_RFTLRobPlots$plotPoint2 +
  scale_y_continuous(limits = c(1, 4.5),
                     name = "Trophic level") +
  scale_color_viridis_c(name = "Initial\nspp. richness", 
                        option = "magma") +
  geom_text(data = textData, aes(x = invRobustbins, y = medTL, 
                                 label = lab, group = Ext), 
            position = position_dodge(width = 0.75),
            colour = "black") +
  guides(shape = "none")

p5$mapping$shape <- NULL

textData <- CR_EN_VUTLRobPlots$plotPoint2$data
textData <- textData[, list(medTL = max(textData$medTL, na.rm = TRUE) + 0.2),
                     by = .(Ext, invRobustbins)]
textData[Ext == "Pext", `:=`(lab = "Pri.")]
textData[Ext == "Sext", `:=`(lab = "Sec.")]
p6 <- CR_EN_VUTLRobPlots$plotPoint2 +
  scale_y_continuous(limits = c(1, 4.5),
                     name = "Trophic level")  +
  scale_color_viridis_c(name = "Initial\nspp. richness", 
                        option = "magma") +
  geom_text(data = textData, aes(x = invRobustbins, y = medTL, 
                                 label = lab, group = Ext), 
            position = position_dodge(width = 0.75),
            colour = "black") +
  guides(shape = "none")

p6$mapping$shape <- NULL

plotOut <- p1 + p2 + get_legend(p1) + 
  p5 + p6 + 
  get_legend(p5 + theme(legend.direction = "horizontal", legend.box = "vertical", legend.spacing = unit(0.1, "cm"))) +
  plot_layout(axis_titles = "collect_x",
              ncol = 2, design = "144
                                  255
                                  366",
              heights = c(2, 2, 0.3)) & 
  theme(legend.position = "none") 

plotOut <- plotOut +
  plot_annotation(tag_levels = list(c("a)", "b)"), ""))

ggsave(plot = plotOut, filename = file.path(fig.dir, "robMap_boxplotsTLXRobPoints.png"),
       dpi = 300, height = 8.5, width = 12)




p1 <- robMap2 +
  theme_pubr(margin = FALSE, base_size = 14) +
  theme(strip.background = element_blank(),,
        strip.text = element_blank()) +
  labs(title = "Climate change", fill = "Robustness", 
       y = " ", x = "Longitude") 
p2 <- p1 + labs(title = "IUCN extinctions", 
                y = "Latitude", x = "Longitude") 
p1$data <- p1$data[Scen == "hd_rcp85_noLUC_Invs_noIUCNext"]
p2$data <- p2$data[Scen == "noCC_noCC_noLUC_noInvs_CR_EN_VU"]

p3 <- hd_rcp85_wm_bin_RFTLRobPlots$plotBP2 +
  theme_pubr(margin = FALSE, base_size = 14) +
  scale_y_continuous(limits = c(1, 5),
                     name = "Trophic level")

p4 <- CR_EN_VUTLRobPlots$plotBP2 +
  theme_pubr(margin = FALSE, base_size = 14) +
  scale_y_continuous(limits = c(1, 5),
                     name = " ") 

plotOut <- p1 + p2 + get_legend(p1 + theme(legend.direction = "vertical")) + 
  p3 + p4 + 
  get_legend(p3 + theme(legend.direction = "vertical")) +
  plot_layout(axis_titles = "collect_y",
              ncol = 3, design = "11#22#3
                                  4445556",
              widths = c(2, 2, 0.1),
              heights = c(2, 1)) & 
  theme(legend.position = "none") 

plotOut <- plotOut +
  plot_annotation(tag_levels = list(c("a)", "b)"), ""))

ggsave(plot = plotOut, filename = file.path(fig.dir, "robMap_boxplotsTLXRobBPs_v.png"),
       dpi = 300, height = 10, width = 16)




textData <- hd_rcp85_wm_bin_RFTLRobPlots$plotPoint2$data
textData <- textData[, list(medTL = max(textData$medTL, na.rm = TRUE) + 0.2),
                     by = .(Ext, invRobustbins)]
textData[Ext == "Pext", `:=`(lab = "Pri.")]
textData[Ext == "Sext", `:=`(lab = "Sec.")]
  
p5 <- hd_rcp85_wm_bin_RFTLRobPlots$plotPoint2 +
  scale_y_continuous(limits = c(1, 4.5),
                     name = "Trophic level") +
  scale_color_viridis_c(name = "Initial\nspp. richness", 
                        option = "magma") +
  geom_text(data = textData, aes(x = invRobustbins, y = medTL, 
                                 label = lab, group = Ext), 
            position = position_dodge(width = 0.75),
            colour = "black") +
  guides(shape = "none")

p5$mapping$shape <- NULL

textData <- CR_EN_VUTLRobPlots$plotPoint2$data
textData <- textData[, list(medTL = max(textData$medTL, na.rm = TRUE) + 0.2),
                     by = .(Ext, invRobustbins)]
textData[Ext == "Pext", `:=`(lab = "Pri.")]
textData[Ext == "Sext", `:=`(lab = "Sec.")]
p6 <- CR_EN_VUTLRobPlots$plotPoint2 +
  scale_y_continuous(limits = c(1, 4.5),
                     name = " ")  +
  scale_color_viridis_c(name = "Initial\nspp. richness", 
                        option = "magma") +
  geom_text(data = textData, aes(x = invRobustbins, y = medTL, 
                                 label = lab, group = Ext), 
            position = position_dodge(width = 0.75),
            colour = "black") +
  guides(shape = "none")

p6$mapping$shape <- NULL

plotOut <- p1 + p2 + get_legend(p1 + theme(legend.direction = "vertical")) + 
  p5 + p6 + 
  get_legend(p5 + theme(legend.direction = "vertical")) +
  plot_layout(axis_titles = "collect_y",
              ncol = 3, design = "11#22#3
                                  4445556",
              widths = c(2, 2, 0.1),
              heights = c(2, 1)) & 
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0.2, 0, 0), units = "cm"), 
        legend.box.margin = margin(0,0,0,0)) 

plotOut <- plotOut +
  plot_annotation(tag_levels = list(c("a)", "b)"), ""))

ggsave(plot = plotOut, filename = file.path(fig.dir, "robMap_boxplotsTLXRobPoints_v.png"),
       dpi = 300, height = 10, width = 16)

