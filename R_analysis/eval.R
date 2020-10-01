library(raster)
setwd("/home/petra/Downloads/BA_DATA")

# feb_mar <- shapefile("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 23 February 2018 and 6 March 2018 in Eastern Ghouta Area, Damascus Governorate/RDA_Damascus_20180306.shp")
feb_mar <- sf::read_sf("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 23 February 2018 and 6 March 2018 in Eastern Ghouta Area, Damascus Governorate/RDA_Damascus_20180306.shp")

SAR_index <- raster("./SAR_results/pre_by_post_6looks.tif")
SAR_index <- raster("./SAR_results/S1A_B_mix_18til24_by_24til08.tif")

SAR_index_crop <- crop(SAR_index, feb_mar)
SAR_index_mask <- mask(x=SAR_index_crop, mask=feb_mar)

# add texture features
SAR_stack <- SAR_index_mask
SAR_stack$sd_5 <- focal(SAR_index_mask$pre_by_post_6looks,w=matrix(1/25,5,5), fun=sd)
SAR_stack$sd_3 <- focal(SAR_index_mask$pre_by_post_6looks,w=matrix(1/9,3,3), fun=sd)

SAR_stack$mean_5 <- focal(SAR_index_mask$pre_by_post_6looks,w=matrix(1/25,5,5), fun=mean)
SAR_stack$mean_3 <- focal(SAR_index_mask$pre_by_post_6looks,w=matrix(1/9,3,3), fun=mean)
SAR_stack$mean_25 <- focal(SAR_index_mask$pre_by_post_6looks,w=matrix(1/625,25,25), fun=mean)

crs(SAR_index_mask)
crs(feb_mar)

df <- extract(SAR_index_mask, feb_mar, df=TRUE)
# df <- extract(SAR_stack, feb_mar, df=TRUE)
feb_mar$PolyID <- 1:nrow(feb_mar)
df <- merge(df,feb_mar,by.x="ID",by.y="PolyID")

names(df)[2] <- "pre_by_post_6looks"

save(df,file="UN_and_pre_by_post_6looks_DF.RData")
# df <- get(load("UN_and_pre_by_post_6looks_DF.RData"))

library(lattice)
dotplot(df$pre_by_post_6looks~df$Main_Damag)
plot(factor(df$Main_Damag)~df$pre_by_post_6looks)

par(mar=c(8,4,1,1))
boxplot(df$pre_by_post_6looks ~ df$Main_Damag)
# boxplot(df$NDVI_sd_5 ~ df$Main_Damag)

plot(SAR_index_mask, col=gray(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)))
plot(SAR_index_mask, col=gray(c(0.3,0.5,0.7,0.9)))

hist(SAR_index_mask, breaks=100)
hist(df$pre_by_post_6looks, breaks=100)

# create categories
nodam <- df[df$Main_Damag == "No visible damage",]
mindam <- df[df$Main_Damag == "Minor damage",]
maxdam <- df[df$Main_Damag == "Major damage",]

hist(nodam$pre_by_post_6looks, breaks=100, add=T, col="green")
hist(mindam$pre_by_post_6looks, breaks=100, add=T, col="red")
hist(maxdam$pre_by_post_6looks, breaks=100, col="blue")

# look a texture
hist(nodam$NDVI_sd_5, breaks=100, col="green")
hist(mindam$NDVI_sd_5, breaks=100, add=T, col="red")
hist(maxdam$diff_mean_9, breaks=100, col="blue", add=T)

# look by raster cells
dff <- merge(aggregate(pre_by_post_6looks ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
boxplot(dff$pre_by_post_6looks ~ dff$Main_Damag)

# violplot
library(vioplot)
?vioplot
vioplot(dff$pre_by_post_6looks~dff$Main_Damag)

feb_mar$pre_by_post_6looks_Ave <- dff$pre_by_post_6looks
sf::write_sf(feb_mar, dsn="feb_mar_S1_B_mix_ave.shp")
test <- sf::read_sf("feb_mar_ave.shp")
plot(test)
plot(feb_mar)

# Intensity
feb_mar <- sf::read_sf("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 23 February 2018 and 6 March 2018 in Eastern Ghouta Area, Damascus Governorate/RDA_Damascus_20180306.shp")
INT_index <- raster("./SAR_results/pre_minus_post_Intensity_VV.tif")

INT_index_crop <- crop(INT_index, feb_mar)
INT_index_mask <- mask(x=INT_index_crop, mask=feb_mar)
hist(INT_index_mask, breaks=100, ylim=c(0,30))

df_INT <- extract(INT_index_mask, feb_mar, df=TRUE)
# df <- extract(SAR_stack, feb_mar, df=TRUE)
feb_mar$PolyID <- 1:nrow(feb_mar)
df_INT <- merge(df_INT,feb_mar,by.x="ID",by.y="PolyID")

dff_INT <- merge(aggregate(pre_minus_post_Intensity_VV ~ ID, data=df_INT, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df_INT, head, 1))
boxplot(dff_INT$pre_minus_post_Intensity_VV ~ dff_INT$Main_Damag, ylim=c(-0.2, 0.2))

plot(nodam$pre_by_post_6looks ~ nodam$pre_minus_post_Intensity_VV)
plot(mindam$pre_by_post_6looks ~ mindam$pre_minus_post_Intensity_VV, col = "green", add=T)
plot(mindam$pre_by_post_6looks ~ mindam$pre_minus_post_Intensity_VV)

nodam <- y[y$Main_Damag == "No visible damage",]
mindam <- y[y$Main_Damag == "Minor damage",]
maxdam <- y[y$Main_Damag == "Major damage",]

library(caret)
### die folgenden Zeilen nur um die Visualisierung zu optimieren!
myColors<- c("red", "blue", "yellow" )
my_settings <- list(superpose.symbol=list(col=myColors,
                                          fill= myColors))
extr_subset <- extr[createDataPartition(extr$ID,p=0.4)$Resample1,]
### jetzt der eigentliche Feature Plot:
featurePlot(y[,c("pre_by_post_6looks", "pre_minus_post_Intensity_VV")],
            factor(y$Main_Damag),plot="pairs",
            auto.key = list(columns = 2),
            par.settings=my_settings)

# extract values for single "big pixels" and try to compare to pixels with no damage
#mindam <- y[y$Main_Damag == "Minor damage",]
obj <- df[df$ID == "832",]
plot(obj$pre_by_post_6looks)
obj1 <- df[df$ID == "1",]
plot(obj1$pre_by_post_6looks, add=TRUE, col="red")
plot(mindam$pre_by_post_6looks)
