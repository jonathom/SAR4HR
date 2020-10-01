# coh and ML testing + plots
rm(list=ls())
library(raster)
library(lattice)
library(ggplot2)
library(sf)
library(caret)
library(CAST)
setwd("/home/petra/Downloads/BA_DATA/sync_level")
# NDVI and SHP
ndvi <- raster("NDVI.tif")
shp <- sf::read_sf("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 23 February 2018 and 6 March 2018 in Eastern Ghouta Area, Damascus Governorate/RDA_Damascus_20180306.shp")
# crop NDVI
ndvi <- crop(ndvi, shp)
ndvi <- mask(x=ndvi, mask=shp)
# make TIFF list, order is 1-9 / 10-18 | coh 10x3, 15x4, 7x2 -> 10x3, 4x1, 6x2 each
filelist = list.files("/home/petra/Downloads/damascus_SLC/case_study_one/TIF")

for (i in 1:12) {
  for (j in 13:24) {

    path <- "/home/petra/Downloads/damascus_SLC/case_study_one/TIF"
    # create raster stacks
    SAR_pre <- raster::stack(paste(path,filelist[i], sep="/"))
    SAR_post <- raster::stack(paste(path,filelist[j], sep="/"))
    # extract information form filename
    date_pre <- substr(filelist[i], 12, 20)
    date_post <- substr(filelist[j], 12, 20)
    specs_pre <- paste(substr(filelist[i], 32, 35),substr(filelist[i], 44, 51),sep="")
    specs_post <- paste(substr(filelist[j], 32, 35),substr(filelist[j], 44, 51),sep="")
    
    if (res(SAR_pre)[1] == res(SAR_post)[1] && specs_pre == specs_post && date_pre < date_post) {

      # clip raster stacks
      SAR_pre <- crop(SAR_pre, shp)
      SAR_pre <- mask(x=SAR_pre, mask=shp)
      SAR_post <- crop(SAR_post, shp)
      SAR_post <- mask(x=SAR_post, mask=shp)
      # calculate ratio
      SAR_ratio <- SAR_pre / SAR_post
      # make layer names
      vh <- paste("ratio_VH")
      vv <- paste("ratio_VV")
      # rename
      names(SAR_ratio) <- c(vh, vv)
      # apply ndvi threshold
      # ndvi <- resample(ndvi, SAR_ratio)
      # ndvi[ndvi > 0.35] <- NA
      # SAR_ratio <- crop(SAR_ratio, ndvi)
      # SAR_ratio <- mask(x=SAR_ratio, mask=ndvi)
      
      # export SAR_ratio
      name <- paste(specs_pre, specs_post, sep="_")
      name <- paste("./R_results/SAR_ratio_", name, ".grd", sep="")
      # writeRaster(SAR_ratio,name,overwrite=TRUE)
      
      # create raster df
      df <- extract(SAR_ratio, shp, df=TRUE)
      shp$PolyID <- 1:nrow(shp)
      df <- merge(df,shp,by.x="ID",by.y="PolyID")
      # make name
      name <- paste(date_pre, date_post, specs_pre, specs_post, sep="_")
      namedf <- paste("df_", name, sep="")
      assign(namedf, df)
      
      # create shp mean dff
      dff <- merge(aggregate(ratio_VV ~ ID, data=df, mean, na.rm=TRUE), 
                   aggregate(Main_Damag ~ ID, data=df, head, 1))
      dfh <- merge(aggregate(ratio_VH ~ ID, data=df, mean, na.rm=TRUE), 
                   aggregate(Main_Damag ~ ID, data=df, head, 1))
      dff <- merge(dff, dfh, by.x = "ID", by.y = "ID")
      namedff <- paste("dff_", name, sep="")
      assign(namedff, dff)
      
      # paste to list of all dataframes
      if(!exists("listofframes")) {
        listofframes <- c(namedf, namedff)
      }
      else {
        listofframes <- append(listofframes, values = c(namedf, namedff))
      }
      
      # model training
      predictors <- names(SAR_ratio)
      trainDat <- dff[1:4]
      # model <- train(trainDat[,predictors],
      #                trainDat$Main_Damag.x,
      #                method="rf",
      #                importance=TRUE)
      # CV model training
      trainids <- CreateSpacetimeFolds(trainDat,spacevar="ID",class="Main_Damag.x",k=3)
      model_cv <- train(trainDat[,predictors],
                         trainDat$Main_Damag.x,
                         method="rf",
                         importance=TRUE,
                         metric="Kappa", 
                         tuneLength = 3, 
                         ntree=50,
                         trControl=trainControl(method="cv",index=trainids$index))
      # break down to damage/no damage
      # trainDat2 <- trainDat
      # trainDat2[trainDat2$Main_Damag.x == "Major damage",3] <- "Minor damage"
      # trainDat2[trainDat2$Main_Damag.x == "Minor damage",3] <- "damage"
      # model_twoclasses <- train(trainDat2[,predictors],
      #                trainDat2$Main_Damag.x,
      #                method="rf",
      #                importance=TRUE)
      
      pred <- predict(model_cv, dff)
      # pred2 <- predict(model_twoclasses, dff)
      CM <- confusionMatrix(pred,factor(dff$Main_Damag.x))
      # CM2 <- confusionMatrix(pred2,factor(trainDat2$Main_Damag.x))
      
      CMname <- paste("CM_", specs_pre, sep="")
      # CM2name <- paste("CM2_", specs_pre, sep="")
      assign(CMname, CM)
      # assign(CM2name, CM2)
      
      table <- CM$byClass
      # sensitivity major minor no damage
      # table[1,1] table[2,1] table[3,1]
      # table[1,3] table[2,3] table[3,3]
      
      if(!exists("KappaResults")) {
        KappaResults <- matrix(c(model_cv$results$Kappa, table[1,1], table[2,1], table[3,1], table[1,3], table[2,3], table[3,3], name), ncol=8)
        # KappaResults <- as.data.frame(KappaResults)
        # names(KappaResults) <- c("model_cv", "prod_maj", "proj_min", "prod_no", "us_maj", "us_min", "us_no", "specs_name")
      }
      else {
        KappaResults <- rbind(KappaResults, c(model_cv$results$Kappa, table[1,1], table[2,1], table[3,1], table[1,3], table[2,3], table[3,3], name))
      }
      
      
    }
  }
}

KappaResults_orig <- KappaResults
KappaResults <- as.data.frame(KappaResults)
names(KappaResults) <- c("model_cv", "prod_maj", "prod_min", "prod_no", "us_maj", "us_min", "us_no", "specs_name", "ID", "coherence", "multi_looking")
names(KappaResults) <- c("model_cv Kappa", "Prod. acc. - Maj. dam.", "Prod. acc. - Min. dam.", "Prod. acc. - No dam.", "User's acc. - Max. dam.", "User's acc. - Min. dam.", "User's acc. - No. dam.", "specs_name", "ID", "coherence window", "multi-looking")

KappaResults$ID <- 1:12
KappaResults$coherence_window <- c(10, 10, 10, 15, 15, 15, 20, 20, 20, 7, 7, 7)
KappaResults$multi_looking <- c(10, 4, 6, 10, 4, 6, 10, 4, 6, 10, 4, 6)

KappaResults[,1] <- as.numeric(paste(KappaResults[,1]))
KappaResults[,2] <- as.numeric(paste(KappaResults[,2]))
KappaResults[,3] <- as.numeric(paste(KappaResults[,3]))
KappaResults[,4] <- as.numeric(paste(KappaResults[,4]))
KappaResults[,5] <- as.numeric(paste(KappaResults[,5]))
KappaResults[,6] <- as.numeric(paste(KappaResults[,6]))
KappaResults[,7] <- as.numeric(paste(KappaResults[,7]))

results_table <- KappaResults
results_table1 <- results_table[,1:7]
results_table2 <- results_table[,10:11]
results_table <- cbind(results_table1, results_table2)

write.csv(results_table,"./plots/resultstable.csv", row.names = TRUE)

library(gridExtra)
pdf("./plots/plot55.pdf", width = 16.5, height = 4)
grid.table(results_table)
dev.off()

KappaResults$Mean <- rowMeans(KappaResults[,1:3])

ggplot(KappaResults, aes(x=coherence)) +
  geom_point(aes(y=prod_maj), color="red") +
  geom_smooth(aes(y=prod_maj), color="red", method="lm") +
  geom_point(aes(y=prod_maj), color="green") +
  geom_smooth(aes(y=prod_maj), color="green", method="lm") +
  geom_point(aes(y=prod_no), color="blue") +
  geom_smooth(aes(y=prod_no), color="blue", method="lm") +
  geom_point(aes(y=us_maj), color="forestgreen") +
  geom_smooth(aes(y=us_maj), color="forestgreen", method="lm") +
  geom_point(aes(y=us_min), color="purple") +
  geom_smooth(aes(y=us_min), color="purple", method="lm") +
  geom_point(aes(y=us_no), color="lightblue") +
  geom_smooth(aes(y=us_no), color="lightblue", method="lm")

