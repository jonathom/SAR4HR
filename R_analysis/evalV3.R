library(raster)
library(lattice)
library(ggplot2)
setwd("/home/petra/Downloads/BA_DATA")

#####
# index and ratio calculation, boxplot and scatterplot produced
#####

# read damage evaluation shapefile
feb_mar <- sf::read_sf("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 23 February 2018 and 6 March 2018 in Eastern Ghouta Area, Damascus Governorate/RDA_Damascus_20180306.shp")
feb_mar_b <- sf::read_sf("building_shapes_ghouta_v1.shp")

# smaller area
#feb_mar <- sf::read_sf("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 23 February 2018 and 2 March 2018 in Douma, Eastern Ghouta Area, Syria/RDA_Douma_20180302.shp")
# read change detection raster
# SAR_index <- raster("./SAR_results/pre_by_post_6looks.tif")
SAR_pre <- stack("./SAR_results/just two images no ratio or anything/S1A_IW_SLC_coreg_18_24_Orb_Stack_coh_deb_ML_TC.tif")
SAR_post <- stack("./SAR_results/just two images no ratio or anything/S1A_IW_SLC_coreg_24_08_Orb_Stack_coh_deb_ML_TC.tif")
# crop and mask raster to shapefile
SAR_pre <- crop(SAR_pre, feb_mar)
SAR_pre <- mask(x=SAR_pre, mask=feb_mar)
SAR_post <- crop(SAR_post, feb_mar)
SAR_post <- mask(x=SAR_post, mask=feb_mar)
# rename
names(SAR_pre) <- c("pre_VH", "pre_VV") 
names(SAR_post) <- c("post_VH", "post_VV")

SAR_ratio <- SAR_pre / SAR_post
SAR_index <- (SAR_pre - SAR_post) / (SAR_pre + SAR_post)

names(SAR_ratio) <- c("ratio_VH", "ratio_VV")
names(SAR_index) <- c("index_VH", "index_VV")

# clip to building shape
SAR_pre <- crop(SAR_pre, feb_mar_b)
SAR_pre <- mask(x=SAR_pre, mask=feb_mar_b)
SAR_post <- crop(SAR_post, feb_mar_b)
SAR_post <- mask(x=SAR_post, mask=feb_mar_b)
plot(SAR_ratio)
# create merged dataframe
df <- extract(SAR_ratio, feb_mar, df=TRUE)
dfI <- extract(SAR_index, feb_mar, df=TRUE)
# df <- extract(SAR_stack, feb_mar, df=TRUE)
feb_mar$PolyID <- 1:nrow(feb_mar)
df <- merge(df,feb_mar,by.x="ID",by.y="PolyID")
dfI <- merge(dfI,feb_mar,by.x="ID",by.y="PolyID")
# rename columns?
# names(df)[2] <- "pre_by_post"
# save and open
#save(df,file="UN_and_pre_by_post_6looks_DF.RData")
#df <- get(load("UN_and_pre_by_post_6looks_DF.RData"))


ggplot(df, aes(x = ratio_VH, y = ratio_VV, color = Main_Damag)) + geom_point()
ggplot(df, aes(x = index_VH, y = index_VV, color = Main_Damag)) + geom_point()
# writeRaster(SAR_ratio,"./ratio_calculated_in_R_test_18_24_8.grd", overwrite=TRUE)

# look by raster cells
# max can also be an option
dff <- merge(aggregate(ratio_VV ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
dfh <- merge(aggregate(ratio_VH ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
dff <- merge(dff, dfh, by.x = "ID", by.y = "ID")

# look by raster cells
dfIf <- merge(aggregate(index_VV ~ ID, data=dfI, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
dfIh <- merge(aggregate(index_VH ~ ID, data=dfI, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=dfI, head, 1))
dfIf <- merge(dfIf, dfIh, by.x = "ID", by.y = "ID")

# VV ~ VH, points, categories, smoothed lines
ggplot(dff, aes(x = ratio_VH, y = ratio_VV, color = Main_Damag.x)) + 
  geom_point(size=2) +
  geom_smooth()

ggplot(dfIf, aes(x =index_VH, y = index_VV, color = Main_Damag.x)) + 
  geom_point(size=2) +
  geom_smooth()

dfIf$VVminusVH <- dfIf$index_VV - dfIf$index_VH
boxplot(dfIf$VVminusVH ~ dfIf$Main_Damag.x)

# useful boxplot
boxplot(dff$ratio_VV ~ dff$Main_Damag.y)
boxplot(dff$ratio_VH ~ dff$Main_Damag.y)

boxplot(dfIf$index_VV ~ dfIf$Main_Damag.y)
boxplot(dfIf$index_VH ~ dfIf$Main_Damag.y)

ggplot(dff, aes(x = ratio_VV)) +
  geom_histogram(aes(color = Main_Damag.x, fill = Main_Damag.x), 
                 position = "identity", bins = 70, alpha = 0.4) +
  scale_color_manual(values = c("red", "orange", "white")) +
  scale_fill_manual(values = c("red", "orange", "white"))

ggplot(dff, aes(x = ratio_VV)) + 
  geom_density(aes(color = Main_Damag.x), adjust=0.05) +
  geom_density(aes(color = Main_Damag.x))

ggplot(dff, aes(x = ratio_VH)) + 
  geom_density(aes(color = Main_Damag.x))

ggplot(dfI, aes(x = index_VV)) + 
  geom_density(aes(color = Main_Damag))
boxplot(dfI$index_VV ~ dfI$Main_Damag)


#### Modelltraining
# möglichkeit thresholds festlegen zu lassen
library(caret)
predictors <- names(SAR_ratio)
trainIDs <- createDataPartition(dff$ID,p=1,list = FALSE)
trainDat <- dff[trainIDs,]
trainDat <- trainDat[1:4]
model <- train(trainDat[,predictors],
               trainDat$Main_Damag.x,
               method="rf",
               importance=TRUE)
model
pred <- predict(SAR_ratio, model)
plot(pred)
writeRaster(pred,"prediction_SAR_ratio.grd",overwrite=TRUE)
