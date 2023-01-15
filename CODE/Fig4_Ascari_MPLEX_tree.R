## Plots trees and colors taxa according to lineage affinities

library(ape)
library(phangorn)
library(phytools)
library(RColorBrewer)
library(tidyverse)
library(ggtree)
library(ggrepel)
library(dplyr)
library(treeio)

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
myColors = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

#read in data
Tree1 <- read.tree("RAxML_bipartitions.outSupport")

Tree1 <- drop.tip(Tree1, c("Ei88443", "BdBar", ""))

# extract host genus names
groupInfo <- split(Tree1$tip.label, gsub(".*_", "", Tree1$tip.label)) # split(x, f) split dataset x into groups defined by f
groupInfo <- lapply(groupInfo, function(x) gsub("_.*", "", x))        # apply function(x) to each eleemtn in each group

# store genus info
Tree1$genus <- gsub(".*_", "", Tree1$tip.label)

# drop genus info from tip labels
Tree1$tip.label <- gsub("_.*", "", Tree1$tip.label)

# assign group info to tree
Tree1 <- groupOTU(Tree1, groupInfo)

# store info about nodes that define key clades
clades <- data.frame(node = c(628, 631, 482, 621, 511, 715, 370, 406, 425, 712, 688, 685, 690, 648, 471, 475, 645, 623),
                     ids = c("Ppen", "Pgris", "PoSt", "PoU1", "PoT", "PoS", "Pu", "PoT", "PoL1",
                             "PoLe","PoC1", "PoP","PoO", "PoE1/2", "PoEc", "PoEc", "PoE3", "PoM"))

# plot and save the tree
#pdf("TestFig2_AscariMPLEXtreecladeLabelFan.pdf", 22, 22)

ggtree(Tree1, layout = "fan", branch.length="branch.length", aes(label = gsub(".*_", "", label)), size = 1) +
  geom_hilight(data=clades, mapping=aes(node=node), extendto=2.72, fill = "grey", size = 0.05) +
  geom_tiplab(aes(color = group, label = paste("          ", label, "          ", sep = "")), size = 3, align = T, show.legend = F) +
  geom_cladelab(data=clades, mapping=aes(node=node, label = ids), align=T, angle = "auto", barsize=NA, horizontal=FALSE) +
  geom_tree(layout = "fan") +
  geom_point2(aes(subset=!isTip & !is.na(as.numeric(label)) & as.numeric(label) >= 50, size = as.numeric(label)/100)) +
  geom_polygon(aes(fill = group, x = 0, y = 0)) +
    theme(legend.position="top",
    legend.text= element_text(size=20, face="italic"),
    legend.title = element_text(colour="black", size=24, face="bold"),
    legend.direction = "horizontal",
    legend.justification = c(0.5, 1),
    legend.background = element_rect(fill="white",linewidth =0.3, linetype="solid",colour ="black")) +
    guides(fill = guide_legend(title = "Host\nGenus", nrow = 3, override.aes = list(label = "")))
#dev.off()



