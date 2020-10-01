# influence of coherence window visible
rm(list=ls())
library(raster)
library(lattice)
library(ggplot2)
library(sf)
library(caret)
library(CAST)
setwd("/home/petra/Downloads/BA_DATA/sync_level")

coreg_1by2 <- raster::stack("./R_results/SAR_ratio_15_4_ML_4_1__15_4_ML_4_1_.grd")

stack_1by2 <- raster::stack("/home/petra/first_study_1by2.tif")

# small
e <- c(255484, 257259, 3713800, 3717780)
colo <- viridisLite::inferno(40)
brk <- seq(0,17,0.5)
brk <- seq(0,5,0.2)
pdf("./plots/comparison1.pdf", width=5, height=10)
spplot(coreg_1by2$ratio_VV, col.regions=colo, at = brk, xlim=c(e[1], e[2]), ylim=c(e[3], e[4]), maxpixels=800000, main="coreg 1by2")
dev.off()
pdf("./plots/comparison2.pdf", width=5, height=10)
spplot(stack_1by2$layer, col.regions=colo, at = brk, xlim=c(e[1], e[2]), ylim=c(e[3], e[4]), maxpixels=20000000, main="stack 1by2")
dev.off()

plot(coh7x2_ML6x2)
spplot(deratify(coh10x3_ML6x2), #col.regions=cols,
       maxpixels=ncell(coh10x3_ML6x2))

library(ggsci)
library(tmap)
library(viridis)
library(shiny)
cols <- c("brown", "red", "blue", "green", "darkblue", "forestgreen", 
          "yellow", "lightgreen", "white")

colo <- viridisLite::inferno(15)
colo <- scale_color_aaas()

hist(coh10x3_ML6x2, breaks=100)

brk <- seq(0,3,0.2)
brk2 <- seq(4,10,1)

plot(coh10x3_ML6x2, col = colo, #stretch="lin", 
     axes=F)

colo <- viridisLite::inferno(17)
par(mfrow = c(1, 2))
plot(coh10x3_ML6x2$ratio_VV, col = colo, axes=F, ext=e, breaks=seq(0,17,1))
plot(coh20x5_ML6x2$ratio_VV, col = colo, axes=F, ext=e, breaks=seq(0,17,1))

xmin       : 256810.4 
xmax       : 260127.6 
ymin       : 3713262 
ymax       : 3717785 
par(mfrow = c(1, 2))
e <- c(252000, 260000, 3710000, 3718000)
# good
e <- c(254484, 259859, 3713000, 3718000)
# slim
e <- c(255484, 258859, 3713000, 3717780)
# small
e <- c(255484, 257259, 3713800, 3717780)
colo <- viridisLite::inferno(40)
brk <- seq(0,17,0.5)
brk <- seq(0,5,0.2)
pdf("./plots/coh20x5_ML6x2_small.pdf", width=5, height=10)
spplot(coh20x5_ML6x2$ratio_VV, col.regions=colo, at = brk, xlim=c(e[1], e[2]), ylim=c(e[3], e[4]), maxpixels=ncell(coh20x5_ML6x2$ratio_VV), main="20x5 Coherence Window")
dev.off()
pdf("./plots/coh7x2_ML6x2_small.pdf", width=5, height=10)
spplot(coh7x2_ML6x2$ratio_VV, col.regions=colo, at = brk, xlim=c(e[1], e[2]), ylim=c(e[3], e[4]), maxpixels=ncell(coh20x5_ML6x2$ratio_VV), main="7x2 Coherence Window")
dev.off()
pdf("./plots/coh10x3_ML6x2_small.pdf", width=5, height=10)
spplot(coh10x3_ML6x2$ratio_VV, col.regions=colo, at = brk, xlim=c(e[1], e[2]), ylim=c(e[3], e[4]), maxpixels=ncell(coh20x5_ML6x2$ratio_VV), main="10x3 Coherence Window")
dev.off()
pdf("./plots/coh15x4_ML6x2_small.pdf", width=5, height=10)
spplot(coh15x4_ML6x2$ratio_VV, col.regions=colo, at = brk, xlim=c(e[1], e[2]), ylim=c(e[3], e[4]), maxpixels=ncell(coh20x5_ML6x2$ratio_VV), main="10x3 Coherence Window")
dev.off()

spplot(coh20x5_ML6x2$ratio_VV, col.regions=colo, at = brk, xlim=c(255484, 257259), ylim=c(3713800, 3717780), maxpixels=ncell(coh20x5_ML6x2$ratio_VV))
spplot(coh10x3_ML6x2$ratio_VV, col.regions=colo, at = brk, xlim=c(256380, 258800), ylim=c(3713315, 3717459), maxpixels=ncell(coh20x5_ML6x2$ratio_VV))

plot1 <- crop(coh20x5_ML6x2, e)

pre_ras <- raster::stack("./R_results/SAR_ratio_7_2_ML_6_2_O_7_2_ML_6_2_O.grd")
post_ras <- raster::stack("./R_results/SAR_ratio_10_3_ML_6_2__10_3_ML_6_2_.grd")

SAR_pre <- raster::stack(paste(path,filelist[5], sep="/"))
SAR_post <- raster::stack(paste(path,filelist[17], sep="/"))

# small
e <- c(254484, 261259, 3713000, 3719580)
colo <- viridisLite::inferno(40)
color <- viridisLite::inferno(100)
brk <- seq(0,17,0.5)
brk <- seq(0,1,0.01)

pdf("./plots/plot40.pdf", width = 5, height = 6)
spplot(SAR_pre$S1B_IW_SLC_1202_1802_cohwindow_15_4_no_Topo_ML_4_1_Orb_Stack_coh_deb_ML_TC.2, col.regions=color, at = brk, #xlim=c(e[1], e[2]), ylim=c(e[3], e[4]), 
       maxpixels=ncell(coh20x5_ML6x2$ratio_VV), main="SAR coherence, calculated over \n12/02/2018 and 18/02/2018 (pre-event), VV-pol")
dev.off()
pdf("./plots/plot41.pdf", width = 5, height = 6)
spplot(SAR_post$S1B_IW_SLC_1802_0803_cohwindow_15_4_no_Topo_ML_4_1_Orb_Stack_coh_deb_ML_TC.2, col.regions=color, at = brk, #xlim=c(e[1], e[2]), ylim=c(e[3], e[4]), 
       maxpixels=ncell(coh20x5_ML6x2$ratio_VV), main="SAR coherence, calculated over \n18/02/2018 and 08/03/2018 (co-event), VV-pol")
dev.off()
rat <- SAR_pre / SAR_post

brks = seq(0,5, 0.1)
pdf("./plots/plot42.pdf", width = 5, height = 6)
spplot(rat$S1B_IW_SLC_1202_1802_cohwindow_15_4_no_Topo_ML_4_1_Orb_Stack_coh_deb_ML_TC.2, col.regions=color, at = brks, #xlim=c(e[1], e[2]), ylim=c(e[3], e[4]), 
       maxpixels=ncell(coh20x5_ML6x2$ratio_VV), main="SAR coherence ratio, \npre/co event scenes. VV-pol")
dev.off()
pdf("./plots/plot43.pdf", width = 5, height = 6)
spplot(rat$S1B_IW_SLC_1202_1802_cohwindow_15_4_no_Topo_ML_4_1_Orb_Stack_coh_deb_ML_TC.1, col.regions=color, at = brks, #xlim=c(e[1], e[2]), ylim=c(e[3], e[4]), 
       maxpixels=ncell(coh20x5_ML6x2$ratio_VV), main="SAR coherence ratio, \npre/co event scenes. VH-pol")
dev.off()
