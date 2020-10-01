### al
library(raster)
library(lattice)
library(ggplot2)
library(sf)
setwd("/home/petra/Downloads/BA_DATA")
shp <- sf::read_sf("./UNOSAT_damascus_new_shapes/UNOSAT_CE20130604SYR_Syria_Damage_Assessment_2016_shp/6_Damage_Sites_Aleppo_SDA.shp")

shp <- st_transform(shp, crs=32637)

plot(shp)
inc <- shp[shp$DmgSts_4 == "Increase - damage",]
new <- shp[shp$DmgSts_4 == "New - damage",]
plot(shp)
plot(new)
plot(inc)

# keep only damage and geometry
new <- new[,c(19,20,27)]

df <- extract(SAR_ratio, new, df=TRUE)
new$PolyID <- 1:nrow(new)
df <- merge(df,new,by.x="ID",by.y="PolyID")

l1 <- SAR_pre[[1]]
l1 <- focal(l1, w=matrix(1,5,5),fun=modal)
l2 <- SAR_pre[[2]]
l2 <- focal(l2, w=matrix(1,5,5),fun=modal)

l3 <- SAR_post[[1]]
l3 <- focal(l3, w=matrix(1,5,5),fun=modal)
l4 <- SAR_post[[2]]
l4 <- focal(l4, w=matrix(1,5,5),fun=modal)

ratio_VH = l1/l3
ratio_VV = l2/l4

SAR_ratio <- stack(c(ratio_VH, ratio_VV))


ndvi <- raster("NDVI.tif")
shp <- sf::read_sf("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 23 February 2018 and 6 March 2018 in Eastern Ghouta Area, Damascus Governorate/RDA_Damascus_20180306.shp")

ndvi_ <- crop(ndvi, feb_mar)
ndvi <- mask(x=ndvi_, mask=feb_mar)

SAR_pre <- stack("./SAR_results/just two images no ratio or anything/S1A_IW_SLC_coreg_18_24_Orb_Stack_coh_deb_ML_TC.tif")
SAR_post <- stack("./SAR_results/just two images no ratio or anything/S1A_IW_SLC_coreg_24_08_Orb_Stack_coh_deb_ML_TC.tif")
SAR_pre <- stack("./SAR_results/S1A_IW_SLC_coreg_04_16_Orb_Stack_coh_deb_ML_TC.tif")
SAR_post <- stack("./SAR_results/S1A_IW_SLC_coreg_16_1609_Orb_Stack_coh_deb_ML_TC.tif")

SAR_pre <- crop(SAR_pre, shp)
SAR_pre <- mask(x=SAR_pre, mask=shp)
SAR_post <- crop(SAR_post, shp)
SAR_post <- mask(x=SAR_post, mask=shp)
names(SAR_pre) <- c("pre_VH", "pre_VV") 
names(SAR_post) <- c("post_VH", "post_VV")
SAR_ratio <- SAR_pre / SAR_post
names(SAR_ratio) <- c("ratio_VH", "ratio_VV")

ndvi <- resample(ndvi, SAR_pre)

stack <- stack(c(SAR_ratio, ndvi))

ndvi[ndvi > 0.2] <- NA
plot(stack)



df <- extract(SAR_ratio, shp, df=TRUE)
shp$PolyID <- 1:nrow(shp)
df <- merge(df,shp,by.x="ID",by.y="PolyID")

ggplot(df, aes(x = ID, y = ratio_VV, color = Main_Damag)) + geom_smooth()

# good plot here!
ggplot(df, aes(x = ratio_VH, y = ratio_VV, color = Main_Damag)) + geom_smooth()
ggplot(df, aes(x = ratio_VH, y = ratio_VV, color = Main_Damag)) + geom_point() + geom_smooth()
ggplot(df, aes(x = ratio_VH)) + geom_density(aes(color = Main_Damag))

dff <- merge(aggregate(ratio_VV ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
dfh <- merge(aggregate(ratio_VH ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
dff <- merge(dff, dfh, by.x = "ID", by.y = "ID")

ggplot(dff, aes(x = ratio_VH, y = ratio_VV, color = Main_Damag.x)) + 
  geom_point(size=2) +
  geom_smooth()
ggplot(df, aes(x = ratio_VH, y = ratio_VV, color = DmgCls_4)) + 
  geom_point(size=2) +
  geom_smooth()


boxplot(df$ratio_VV ~ df$Main_Damag, ylim=c(0.5,3))
boxplot(df$ratio_VV ~ df$DmgCls_4)

writeRaster(ndvi,"ndvi_clipped_2.grd",overwrite=TRUE)
