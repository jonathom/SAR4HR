library(raster)
library(lattice)
setwd("/home/petra/Downloads/BA_DATA")

# read damage evaluation shapefile
feb_mar <- sf::read_sf("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 23 February 2018 and 6 March 2018 in Eastern Ghouta Area, Damascus Governorate/RDA_Damascus_20180306.shp")
# read change detection raster
#SAR_index <- raster("./SAR_results/pre_by_post_6looks.tif")
SAR_index <- raster("./SAR_results/S1A_B_mix_18til24_by_24til08.tif")
# crop and mask raster to shapefile
SAR_index_crop <- crop(SAR_index, feb_mar)
SAR_index_mask <- mask(x=SAR_index_crop, mask=feb_mar)
# create merged dataframe
df <- extract(SAR_index_mask, feb_mar, df=TRUE)
# df <- extract(SAR_stack, feb_mar, df=TRUE)
feb_mar$PolyID <- 1:nrow(feb_mar)
df <- merge(df,feb_mar,by.x="ID",by.y="PolyID")
# rename columns?
names(df)[2] <- "pre_by_post"
# save and open
#save(df,file="UN_and_pre_by_post_6looks_DF.RData")
#df <- get(load("UN_and_pre_by_post_6looks_DF.RData"))

# first, not very conclusive boxplot
par(mar=c(8,4,1,1))
boxplot(df$pre_by_post ~ df$Main_Damag)
# and a histogram
hist(SAR_index_mask, breaks=100)

# look by raster cells
dff <- merge(aggregate(pre_by_post ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
# useful boxplot
boxplot(dff$pre_by_post ~ dff$Main_Damag)
# or the other way around
plot(as.factor(dff$Main_Damag)~dff$pre_by_post)

# add raster cell averages to the shapefile
feb_mar$pre_by_post_Ave <- dff$pre_by_post_6looks
# write shape
sf::write_sf(feb_mar, dsn="feb_mar_S1_B_mix_ave.shp")
# load shape
test <- sf::read_sf("feb_mar_ave.shp")
plot(test)

# try unsupervised
nr <- getValues(SAR_index_mask)
set.seed(99)
kmncluster <- kmeans(na.omit(nr), centers = 3, iter.max = 500, nstart = 5, algorithm="Lloyd")
image.df.factor <- rep(NA, length(nr))
image.df.factor[!is.na(nr)] <- kmncluster$cluster
knr <- setValues(SAR_index_mask, image.df.factor)
# You can also do it like this
# knr <- raster(SAR_index_mask)
# values(knr) <- kmncluster$cluster
# knr
plot(knr)

# tr summary
nodam <- dff[dff$Main_Damag == "No visible damage",]
mindam <- dff[dff$Main_Damag == "Minor damage",]
maxdam <- dff[dff$Main_Damag == "Major damage",]
summary(nodam)
summary(mindam)
summary(maxdam)
plot(dff$pre_by_post)
plot(as.factor(dff$Main_Damag)~dff$pre_by_post)

hist(nodam$pre_by_post, breaks=100, col="green", type="l")
hist(mindam$pre_by_post, breaks=100, add=T, col="red")
hist(maxdam$pre_by_post, breaks=100, col="blue", add=T, type="l")
?hist

ggplot(dff, aes(x = pre_by_post)) +
  geom_histogram(aes(color = Main_Damag), fill = "white",
                 position = "identity", bins = 100) +
  scale_color_manual(values = c("red", "orange", "white")) 

ggplot(dff, aes(x = pre_by_post)) +
  geom_histogram(aes(color = Main_Damag, fill = Main_Damag), 
                 position = "identity", bins = 70, alpha = 0.4) +
  scale_color_manual(values = c("red", "orange", "white")) +
  scale_fill_manual(values = c("red", "orange", "white"))
