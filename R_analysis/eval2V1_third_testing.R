# coh and ML testing + plots
rm(list=ls())
library(raster)
library(lattice)
library(ggplot2)
library(sf)
library(caret)
library(CAST)
library(RColorBrewer)
setwd("/home/petra/Downloads/BA_DATA/sync_level")

# stack and names
shape <- sf::read_sf("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 3 December 2017 and 23 February 2018 in Eastern Ghouta Area, Syria/RDA_Damascus_20180223.shp")
# band 2 of this
SAR_first_1 <- raster::stack("/home/petra/Downloads/damascus_SLC/case_study_two/first/S1A_IW_SLC__1SDV_20171202T033514_20171202T033541_019518_021202_F86A_Orb_Stack_coh_deb_ML_TC.tif")
# SAR_first_1 <- raster(SAR_first_1$S1A_IW_SLC__1SDV_20171202T033514_20171202T033541_019518_021202_F86A_Orb_Stack_coh_deb_ML_TC.2)
SAR_first_2 <- raster::stack("/home/petra/Downloads/damascus_SLC/case_study_two/stack/S1A_IW_SLC__1SDV_20171202T033514_20171202T033541_019518_021202_F86A_split_Orb_Stack_coh_deb_ML_TC.tif")
names(SAR_first_1) <- c("VH_2011_0212", "VV_2011_0212")
names(SAR_first_2) <- c("VH_0212_1412", "VH_0212_2612", 
                      "VH_0212_0701", "VH_0212_1901", 
                      "VH_0212_3101", "VH_0212_1202", 
                      "VH_0212_2402", "VV_0212_1412",
                      "VV_0212_2612", "VV_0212_0701",
                      "VV_0212_1901", "VV_0212_3101",
                      "VV_0212_1202", "VV_0212_2402")

SAR_second_1 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/second/S1A_IW_SLC__1SDV_20171214T033513_20171214T033540_019693_02177B_89FA_Orb_Stack_coh_deb_ML_TC.tif")
SAR_second_2 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/second/S1A_IW_SLC__1SDV_20171214T033513_20171214T033540_019693_02177B_89FA_SECOND_Orb_Stack_coh_deb_ML_TC.tif")

SAR_third_1 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/third_testing/S1A_IW_SLC__1SDV_20171226T033512_20171226T033540_019868_021CE5_B396_Orb_Stack_coh_deb_ML_TC.tif")
SAR_third_2 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/third_testing/S1A_IW_SLC__1SDV_20171226T033512_20171226T033540_019868_021CE5_B396_SECOND_Orb_Stack_coh_deb_ML_TC.tif")

SAR_fourth_1 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/fourth/S1A_IW_SLC__1SDV_20180107T033512_20180107T033539_020043_022267_1F4D_Orb_Stack_coh_deb_ML_TC.tif")
SAR_fourth_2 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/fourth/S1A_IW_SLC__1SDV_20180107T033512_20180107T033539_020043_022267_1F4D_SECOND_Orb_Stack_coh_deb_ML_TC.tif")

SAR_fifth_1 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/fifth/S1A_IW_SLC__1SDV_20180119T033512_20180119T033539_020218_0227F7_CF50_Orb_Stack_coh_deb_ML_TC.tif")
SAR_fifth_2 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/fifth/S1A_IW_SLC__1SDV_20180119T033512_20180119T033539_020218_0227F7_CF50_SECOND_Orb_Stack_coh_deb_ML_TC.tif")

SAR_six_1 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/six/S1A_IW_SLC__1SDV_20180131T033511_20180131T033538_020393_022D87_DADA_Orb_Stack_coh_deb_ML_TC.tif")
SAR_six_2 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/six/S1A_IW_SLC__1SDV_20180131T033511_20180131T033538_020393_022D87_DADA_SECOND_Orb_Stack_coh_deb_ML_TC.tif")

SAR_seven_1 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/seven/S1A_IW_SLC__1SDV_20180212T033511_20180212T033538_020568_023323_F103_Orb_Stack_coh_deb_ML_TC.tif")
SAR_seven_2 <- raster("/home/petra/Downloads/damascus_SLC/case_study_two/seven/S1A_IW_SLC__1SDV_20180212T033511_20180212T033538_020568_023323_F103_SECOND_Orb_Stack_coh_deb_ML_TC.tif")

SAR_first_1 <- crop(SAR_first_1, shape)
SAR_first_1 <- raster::mask(x=SAR_first_1, mask=shape)
SAR_first_2 <- crop(SAR_first_2, shape)
SAR_first_2 <- raster::mask(x=SAR_first_2, mask=shape)
SAR_second_1 <- crop(SAR_second_1, shape)
SAR_second_1 <- raster::mask(x=SAR_second_1, mask=shape)
SAR_second_2 <- crop(SAR_second_2, shape)
SAR_second_2 <- raster::mask(x=SAR_second_2, mask=shape)
SAR_third_1 <- crop(SAR_third_1, shape)
SAR_third_1 <- raster::mask(x=SAR_third_1, mask=shape)
SAR_third_2 <- crop(SAR_third_2, shape)
SAR_third_2 <- raster::mask(x=SAR_third_2, mask=shape)
SAR_fourth_1 <- crop(SAR_fourth_1, shape)
SAR_fourth_1 <- raster::mask(x=SAR_fourth_1, mask=shape)
SAR_fourth_2 <- crop(SAR_fourth_2, shape)
SAR_fourth_2 <- raster::mask(x=SAR_fourth_2, mask=shape)
SAR_fifth_1 <- crop(SAR_fifth_1, shape)
SAR_fifth_1 <- raster::mask(x=SAR_fifth_1, mask=shape)
SAR_fifth_2 <- crop(SAR_fifth_2, shape)
SAR_fifth_2 <- raster::mask(x=SAR_fifth_2, mask=shape)
SAR_six_1 <- crop(SAR_six_1, shape)
SAR_six_1 <- raster::mask(x=SAR_six_1, mask=shape)
SAR_six_2 <- crop(SAR_six_2, shape)
SAR_six_2 <- raster::mask(x=SAR_six_2, mask=shape)
SAR_seven_1 <- crop(SAR_seven_1, shape)
SAR_seven_1 <- raster::mask(x=SAR_seven_1, mask=shape)
SAR_seven_2 <- crop(SAR_seven_2, shape)
SAR_seven_2 <- raster::mask(x=SAR_seven_2, mask=shape)

SAR_1 <- SAR_first_1$VV_2011_0212
SAR_2 <- resample(SAR_first_2$VV_0212_1412, SAR_1)
SAR_3 <- resample(SAR_second_1, SAR_1)
SAR_4 <- resample(SAR_second_2, SAR_1)
SAR_5 <- resample(SAR_third_1, SAR_1)
SAR_6 <- resample(SAR_third_2, SAR_1)
SAR_7 <- resample(SAR_fourth_1, SAR_1)
SAR_8 <- resample(SAR_fourth_2, SAR_1)
SAR_9 <- resample(SAR_fifth_1, SAR_1)
SAR_10 <- resample(SAR_fifth_2, SAR_1)
SAR_11 <- resample(SAR_six_1, SAR_1)
SAR_12 <- resample(SAR_six_2, SAR_1)
SAR_13 <- resample(SAR_seven_1, SAR_1)
SAR_14 <- resample(SAR_seven_2, SAR_1)
# overall
SAR_15 <- resample(SAR_first_2$VV_0212_2402, SAR_1)

SAR_stack <- raster::stack(c(SAR_1, SAR_2, SAR_3, SAR_4, SAR_5, SAR_6, SAR_7, SAR_8, SAR_9, SAR_10, SAR_11, SAR_12, SAR_13, SAR_14, SAR_15))

# NDVI
ndvi <- raster("NDVI.tif")
ndvi <- resample(ndvi, SAR_stack)
ndvi[ndvi > 0.35] <- NA
ndvi <- crop(ndvi, shape)
ndvi <- mask(x=ndvi, mask=shape)
SAR_stack <- crop(SAR_stack, ndvi)
SAR_stack <- mask(x=SAR_stack, mask=ndvi)

names(SAR_stack) <- c("VV_2011_0212", "VV_0212_1412",
                      "21", "22", "31", "32", "41", "42",
                      "51", "52", "61", "62", "71", "72", "overall_2")

index_1 <- ( SAR_stack$VV_2011_0212 - SAR_stack$VV_0212_1412 ) / (  SAR_stack$VV_2011_0212 + SAR_stack$VV_0212_1412 )
index_2 <- ( SAR_stack$X21 - SAR_stack$X22 ) / ( SAR_stack$X21 + SAR_stack$X22 )
index_3 <- ( SAR_stack$X31 - SAR_stack$X32 ) / ( SAR_stack$X31 + SAR_stack$X32 )
index_4 <- ( SAR_stack$X41 - SAR_stack$X42 ) / ( SAR_stack$X41 + SAR_stack$X42 )
index_5 <- ( SAR_stack$X51 - SAR_stack$X52 ) / ( SAR_stack$X51 + SAR_stack$X52 )
index_6 <- ( SAR_stack$X61 - SAR_stack$X62 ) / ( SAR_stack$X61 + SAR_stack$X62 )
index_7 <- ( SAR_stack$X71 - SAR_stack$X72 ) / ( SAR_stack$X71 + SAR_stack$X72 )

index_overall <- ( SAR_stack$VV_2011_0212 - SAR_stack$overall_2 ) / (  SAR_stack$VV_2011_0212 + SAR_stack$overall_2 )


colo <- viridisLite::inferno(20)
brk <- seq(0-1,1,0.1)

pdf("./plots/indiv_scene1.pdf")
spplot(index_1, col.regions = colo, at = brk, maxpixels=ncell(index_1), main="1: Coherence Index between 20/11/17 - 02/12/17 and 02/12/17 - 14/12/17")
dev.off()
pdf("./plots/indiv_scene2.pdf")
spplot(index_2, col.regions = colo, at = brk, maxpixels=ncell(index_1), main="2: Coherence Index between 02/12/17 - 14/12/17 and 14/12/17 - 26/12/17")
dev.off()
pdf("./plots/indiv_scene3.pdf")
spplot(index_3, col.regions = colo, at = brk, maxpixels=ncell(index_1), main="3: Coherence Index between 14/12/17 - 26/12/17 and 26/12/17 - 07/01/18")
dev.off()
pdf("./plots/indiv_scene4.pdf")
spplot(index_4, col.regions = colo, at = brk, maxpixels=ncell(index_1), main="4: Coherence Index between 26/12/17 - 07/01/18 and 07/01/18 - 19/01/18")
dev.off()
pdf("./plots/indiv_scene5.pdf")
spplot(index_5, col.regions = colo, at = brk, maxpixels=ncell(index_1), main="5: Coherence Index between 07/01/18 - 19/01/18 and 19/01/18 - 31/01/18")
dev.off()
pdf("./plots/indiv_scene6.pdf")
spplot(index_6, col.regions = colo, at = brk, maxpixels=ncell(index_1), main="6: Coherence Index between 19/01/18 - 31/01/18 and 31/01/18 - 12/02/18")
dev.off()
pdf("./plots/indiv_scene7.pdf")
spplot(index_7, col.regions = colo, at = brk, maxpixels=ncell(index_1), main="7: Coherence Index between 31/01/18 - 12/02/18 and 12/02/18 - 24/02/18")
dev.off()
pdf("./plots/indiv_overall.pdf")
spplot(index_overall, col.regions = colo, at = brk, maxpixels=ncell(index_1), main="Overall Coherence Index (20/11/17 - 02/12/17 and 02/12/17 - 24/02/18)")
dev.off()

writeRaster(index_overall, "./R_results/second_case/indiv_overall.grd")
writeRaster(index_3, "./R_results/second_case/indiv_index_3.grd")
writeRaster(index_6, "./R_results/second_case/indiv_index_6.grd")


## STATS
# create raster df
index_overall <- ( SAR_stack$VV_2011_0212 - SAR_stack$overall_2 ) / (  SAR_stack$VV_2011_0212 + SAR_stack$overall_2 )
# index_overall_rat <- SAR_stack$VV_2011_0212 / SAR_stack$overall_2
df <- extract(index_overall, shape, df=TRUE)
shape$PolyID <- 1:nrow(shape)
df <- merge(df,shape,by.x="ID",by.y="PolyID")
# create shp mean dff
dff <- merge(aggregate(layer ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
dff[dff$Main_Damag == "Major damage",3] <- "Minor damage"
dff[dff$Main_Damag == "Minor damage",3] <- "Damage"
names(dff)[2] <- "index_VV"

pdf("./plots/case2_boxplot_index.pdf")
boxplot(dff$index_VV ~ dff$Main_Damag, main="Index box")
dev.off()

## ANOVA
library(rstatix)
library(ggplot2)
library(ggpubr)
head(dff)
# show random row 
dff %>% sample_n_by(Main_Damag, size=1)
# order by levels
dff$Main_Damag <- as.factor(dff$Main_Damag)
levels(dff$Main_Damag)
dff <- dff %>% reorder_levels(Main_Damag, order = c("No visible damage", "Damage"))
dff %>% group_by(Main_Damag) %>% get_summary_stats(index_VV, type="mean_sd")
# ONE extreme outliers
a <- dff %>% group_by(Main_Damag) %>% identify_outliers(index_VV)
# check normality
Model <- lm(index_VV ~ Main_Damag, data=dff)
## qq plot good and shapiro test also passed
ggqqplot(residuals(Model))
shapiro_test(residuals(Model))
# shapiro for each group
dff %>% group_by(Main_Damag) %>% shapiro_test(index_VV)
ggqqplot(dff, "index_VV", facet.by = "Main_Damag")
# ANOVA
res.aov <- dff %>% anova_test(index_VV ~ Main_Damag)
res.aov

# dff without outlier
dfff <- rbind(c(dff[1:691,]),dff[693:694,])
aov.dfff <- dfff %>% anova_test(index_VV ~ Main_Damag)
aov.dfff


##
in1 <- index_1
in2 <- index_2
in3 <- index_3
in4 <- index_4
in5 <- index_5
in6 <- index_6
in7 <- index_7
in1[in1 < 0.4] <- NA
in2[in2 < 0.4] <- NA
in3[in3 < 0.4] <- NA
in4[in4 < 0.4] <- NA
in5[in5 < 0.4] <- NA
in6[in6 < 0.4] <- NA
in7[in7 < 0.4] <- NA
spplot(in1)
