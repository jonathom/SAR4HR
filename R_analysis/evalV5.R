######## Description ########
# fifth script
# explore influence of NDVI on model calculations and Kappa
#############################

library(raster)
library(lattice)
library(ggplot2)
library(sf)
setwd("/home/petra/Downloads/BA_DATA")

ndvi <- raster("NDVI.tif")
shp <- sf::read_sf("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 23 February 2018 and 6 March 2018 in Eastern Ghouta Area, Damascus Governorate/RDA_Damascus_20180306.shp")

ndvi_ <- crop(ndvi, shp)
ndvi <- mask(x=ndvi_, mask=shp)

SAR_pre <- stack("./SAR_results/just two images no ratio or anything/S1A_IW_SLC_coreg_18_24_Orb_Stack_coh_deb_ML_TC.tif")
SAR_post <- stack("./SAR_results/just two images no ratio or anything/S1A_IW_SLC_coreg_24_08_Orb_Stack_coh_deb_ML_TC.tif")

SAR_pre <- crop(SAR_pre, shp)
SAR_pre <- mask(x=SAR_pre, mask=shp)
SAR_post <- crop(SAR_post, shp)
SAR_post <- mask(x=SAR_post, mask=shp)
names(SAR_pre) <- c("pre_VH", "pre_VV") 
names(SAR_post) <- c("post_VH", "post_VV")
SAR_ratio <- SAR_pre / SAR_post
names(SAR_ratio) <- c("ratio_VH", "ratio_VV")

ndvi <- resample(ndvi, SAR_ratio)
ndvi[ndvi > 0.2] <- NA

SAR_ratio_mask <- crop(SAR_ratio, ndvi)
SAR_ratio_mask <- mask(x=SAR_ratio, mask=ndvi)

plot(SAR_ratio)
plot(SAR_ratio_mask)

df_ratio <- extract(SAR_ratio, shp, df=TRUE)
shp$PolyID <- 1:nrow(shp)
jk <- merge(df_ratio,shp,by.x="ID",by.y="PolyID")

df_ratio_masked <- extract(SAR_ratio_mask, shp, df=TRUE)
df_ratio_masked <- merge(df_ratio_masked,shp,by.x="ID",by.y="PolyID")

dff <- merge(aggregate(ratio_VV ~ ID, data=df_ratio, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df_ratio, head, 1))
dfh <- merge(aggregate(ratio_VH ~ ID, data=df_ratio, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df_ratio, head, 1))
dff_ratio <- merge(dff, dfh, by.x = "ID", by.y = "ID")

dff <- merge(aggregate(ratio_VV ~ ID, data=df_ratio_masked, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df_ratio_masked, head, 1))
dfh <- merge(aggregate(ratio_VH ~ ID, data=df_ratio_masked, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df_ratio_masked, head, 1))
dff_ratio_masked <- merge(dff, dfh, by.x = "ID", by.y = "ID")

par(mfrow = c(1, 2))
boxplot(dff_ratio$ratio_VV ~ dff_ratio$Main_Damag.x, ylim=c(0.5,2.5))
boxplot(dff_ratio_masked$ratio_VV ~ dff_ratio_masked$Main_Damag.x, ylim=c(0.5,2.5))

ggplot(dff_ratio, aes(x = ratio_VH, y = ratio_VV, color = Main_Damag.x)) + 
  geom_point(size=2) +
  geom_smooth()
ggplot(df_ratio, aes(x = ratio_VH, y = ratio_VV, color = Main_Damag)) + geom_smooth()
ggplot(dff_ratio, aes(x = ratio_VH, y = ratio_VV, color = Main_Damag.x)) + geom_point()
ggplot(dff_ratio, aes(x = ratio_VV)) + 
  geom_density(aes(color = Main_Damag.x))

ggplot(dff_ratio_masked, aes(x = ratio_VV)) + 
  geom_density(aes(color = Main_Damag.x))
ggplot(df_ratio_masked, aes(x = ratio_VH, y = ratio_VV, color = Main_Damag)) + geom_smooth()


library(caret)
predictors <- names(SAR_ratio)
trainIDs <- createDataPartition(dff_ratio$ID,p=1,list = FALSE)
trainDat <- dff_ratio[trainIDs,]
trainDat <- trainDat[1:4]
model <- train(trainDat[,predictors],
               trainDat$Main_Damag.x,
               method="rf",
               importance=TRUE)
model

trainIDs_m <- createDataPartition(dff_ratio_masked$ID,p=1,list = FALSE)
trainDat_m <- dff_ratio_masked[trainIDs_m,]
trainDat_m <- trainDat_m[1:4]
model_masked <- train(trainDat_m[,predictors],
               trainDat_m$Main_Damag.x,
               method="rf",
               importance=TRUE)
model_masked
