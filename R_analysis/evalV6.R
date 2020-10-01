######## Description ########
# 6
# coherence and multi-looking window influence testing 
# and all kinds of plots
#############################
rm(list=ls())
library(raster)
library(lattice)
library(ggplot2)
library(sf)
library(caret)
library(CAST)
library(plot3D)
setwd("/home/petra/Downloads/BA_DATA")
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
      writeRaster(SAR_ratio,name,overwrite=TRUE)
      
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
      model <- train(trainDat[,predictors],
                     trainDat$Main_Damag.x,
                     method="rf",
                     importance=TRUE)
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
      trainDat2 <- trainDat
      trainDat2[trainDat2$Main_Damag.x == "Major damage",3] <- "Minor damage"
      model_twoclasses <- train(trainDat2[,predictors],
                     trainDat2$Main_Damag.x,
                     method="rf",
                     importance=TRUE)
      
      if(!exists("KappaResults")) {
        KappaResults <- matrix(c(model$results$Kappa, model_cv$results$Kappa, model_twoclasses$results$Kappa, name), ncol=4)
        names(KappaResults) <- c("model", "model_cv", "model_twoclasses", "specs_name")
      }
      else {
        KappaResults <- rbind(KappaResults, c(model$results$Kappa, model_cv$results$Kappa, model_twoclasses$results$Kappa, name))
      }
      
      
    }
  }
}

KappaResults_orig <- KappaResults
KappaResults <- as.data.frame(KappaResults)
KappaResults$ID <- 1:12
names(KappaResults) <- c("model", "model_cv", "model_twoclasses", "specs_name", "ID")
KappaResults$coherence_window <- c(10, 10, 10, 15, 15, 15, 20, 20, 20, 7, 7, 7)
KappaResults$multi_looking <- c(10, 4, 6, 10, 4, 6, 10, 4, 6, 10, 4, 6)

KappaResults[,1] <- as.numeric(paste(KappaResults[,1]))
KappaResults[,2] <- as.numeric(paste(KappaResults[,2]))
KappaResults[,3] <- as.numeric(paste(KappaResults[,3]))

KappaResults$Mean <- rowMeans(KappaResults[,1:3])

ggplot(KappaResults, aes(x=multi_looking)) +
  geom_point(aes(y = model, color=coherence_window), size = 5) +
  geom_smooth(aes(y = model, color=coherence_window), color = "blue") +
  geom_point(aes(y = model_cv, color=coherence_window), size = 3) +
  geom_smooth(aes(y = model_cv, color=coherence_window), color = "red") +
  geom_point(aes(y = model_twoclasses, color=coherence_window), size = 1) +
  geom_smooth(aes(y = model_twoclasses, color=coherence_window), color = "green") +
  xlab("number of range looks") +
  ylab("Kappa of model") +
  ggtitle("model performance under influence of \nmulti-looking and coherence window size")

ggplot(KappaResults, aes(x=coherence_window, y=model_twoclasses, color=multi_looking)) +
  geom_point(size=3) +
  geom_smooth() +
  xlab("coherence-window in range") +
  ylab("Kappa of model")

pdf("./plots/plot01.pdf")
# plot Kappa against coh, smooth different models
ggplot(KappaResults, aes(x = coherence_window)) +
  ylab("Kappa of model") +
  xlab("coherence window range looks") +
  ggtitle("Model Kappa and coherence window range looks") +
  geom_point(aes(y=model_twoclasses)) +
  geom_smooth(aes(y=model_twoclasses, color="two_class_model")) +
  geom_point(aes(y=model_cv)) +
  geom_smooth(aes(y=model_cv, color="CV_model")) +
  geom_smooth(aes(y=model)) +
  geom_smooth(aes(y=model, color="simple_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="blue", two_class_model="red", CV_model="green"))
dev.off()

# plot Kappa against coherence window, size is model and color is ML
pdf("./plots/plot02.pdf")
ggplot(KappaResults, aes(x = coherence_window)) +
  ylab("Kappa of model") +
  xlab("coherence window range looks") +
  ggtitle("Model Kappa and coherence window range looks") +
  geom_point(aes(y=model, color=factor(multi_looking), size="simple_model")) +
  geom_point(aes(y=model_cv, color=factor(multi_looking), size="CV_model")) +
  geom_point(aes(y=model_twoclasses, color=factor(multi_looking), size="two_classes_model")) +
  scale_size_manual(name="Type of model", values=c(simple_model=2, CV_model=3, two_classes_model=4 ))
dev.off()

pdf("./plots/plot03.pdf")
# plot Kappa against coh, smooth different models
ggplot(KappaResults, aes(x = multi_looking)) +
  ylab("Kappa of model") +
  xlab("multi-looking range looks") +
  ggtitle("Model Kappa and multi-looking range looks") +
  geom_point(aes(y=model_twoclasses)) +
  geom_smooth(aes(y=model_twoclasses, color="two_class_model")) +
  geom_point(aes(y=model_cv)) +
  geom_smooth(aes(y=model_cv, color="CV_model")) +
  geom_smooth(aes(y=model)) +
  geom_smooth(aes(y=model, color="simple_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="blue", two_class_model="red", CV_model="green"))
dev.off()

# plot Kappa against coherence window, size is model and color is ML
pdf("./plots/plot04.pdf")
ggplot(KappaResults, aes(x = multi_looking)) +
  ylab("Kappa of model") +
  xlab("multi-looking range looks") +
  ggtitle("Model Kappa and multi-looking range looks") +
  geom_point(aes(y=model, color=factor(coherence_window), size="simple_model")) +
  geom_point(aes(y=model_cv, color=factor(coherence_window), size="CV_model")) +
  geom_point(aes(y=model_twoclasses, color=factor(coherence_window), size="two_classes_model")) +
  scale_size_manual(name="Type of model", values=c(simple_model=2, CV_model=3, two_classes_model=4 ))
dev.off()

newdata <- KappaResults
newdata <- newdata[order(-newdata$Mean),]
newdata$ID <- 1:12
newdata$Mean <- rowMeans(newdata[,1:3])

ggplot(newdata, aes(x=model)) +
  geom_point(aes(y=coherence_window, color="coherence_window")) +
  geom_smooth(aes(y=coherence_window, color="coherence_window")) +
  geom_point(aes(y=multi_looking, color="multi_looking")) +
  geom_smooth(aes(y=multi_looking, color="multi_looking")) +
  scale_color_manual(values=c(coherence_window="blue", multi_looking="green"))

ggplot(newdata, aes(x=model)) +
  geom_point(aes(y=coherence_window, color="coherence_window")) +
  geom_smooth(aes(y=coherence_window, color="coherence_window"), method="lm") +
  geom_point(aes(y=multi_looking, color="multi_looking")) +
  geom_smooth(aes(y=multi_looking, color="multi_looking"), method="lm") +
  scale_color_manual(values=c(coherence_window="blue", multi_looking="green"))

pdf("./plots/plot05.pdf")
ggplot(KappaResults) +
  ylab("Range looks") +
  xlab("model Kappa") +
  ggtitle("All Window Sizes and Model Kappa") +
  geom_point(aes(x=model, y=coherence_window, color="simple_model_coherence_window")) +
  geom_smooth(aes(x=model, y=coherence_window, color="simple_model_coherence_window")) +
  geom_point(aes(x=model, y=multi_looking, color="simple_model_multi_looking")) +
  geom_smooth(aes(x=model, y=multi_looking, color="simple_model_multi_looking")) +
  geom_point(aes(x=model_cv, y=coherence_window, color="CV_model_coherence_window")) +
  geom_smooth(aes(x=model_cv, y=coherence_window, color="CV_model_coherence_window")) + 
  geom_point(aes(x=model_cv, y=multi_looking, color="CV_model_multi_looking")) +
  geom_smooth(aes(x=model_cv, y=multi_looking, color="CV_model_multi_looking")) +
  geom_point(aes(x=model_twoclasses, y=coherence_window, color="two_classes_model_coherence_window")) +
  geom_smooth(aes(x=model_twoclasses, y=coherence_window, color="two_classes_model_coherence_window")) +
  geom_point(aes(x=model_twoclasses, y=multi_looking, color="two_classes_model_multi_looking")) +
  geom_smooth(aes(x=model_twoclasses, y=multi_looking, color="two_classes_model_multi_looking")) +
  scale_color_manual(name="Type of model and variable", values=c(simple_model_coherence_window="red", CV_model_coherence_window="orange", two_classes_model_coherence_window="yellow",
                              simple_model_multi_looking="blue", CV_model_multi_looking="lightblue", two_classes_model_multi_looking="darkblue"))
dev.off()

pdf("./plots/plot05b.pdf")
ggplot(KappaResults) +
  ylab("Range Looks") +
  xlab("Model Kappa") +
  ggtitle("All Window Sizes and Model Kappa") +
  geom_point(aes(x=model, y=coherence_window, color="simple_model_coherence_window")) +
  geom_smooth(aes(x=model, y=coherence_window, color="simple_model_coherence_window"), method="lm") +
  geom_point(aes(x=model, y=multi_looking, color="simple_model_multi_looking")) +
  geom_smooth(aes(x=model, y=multi_looking, color="simple_model_multi_looking"), method="lm") +
  geom_point(aes(x=model_cv, y=coherence_window, color="CV_model_coherence_window")) +
  geom_smooth(aes(x=model_cv, y=coherence_window, color="CV_model_coherence_window"), method="lm") + 
  geom_point(aes(x=model_cv, y=multi_looking, color="CV_model_multi_looking")) +
  geom_smooth(aes(x=model_cv, y=multi_looking, color="CV_model_multi_looking"), method="lm") +
  geom_point(aes(x=model_twoclasses, y=coherence_window, color="two_classes_model_coherence_window")) +
  geom_smooth(aes(x=model_twoclasses, y=coherence_window, color="two_classes_model_coherence_window"), method="lm") +
  geom_point(aes(x=model_twoclasses, y=multi_looking, color="two_classes_model_multi_looking")) +
  geom_smooth(aes(x=model_twoclasses, y=multi_looking, color="two_classes_model_multi_looking"), method="lm") +
  scale_color_manual(name="Type of model and variable", values=c(simple_model_coherence_window="red", CV_model_coherence_window="orange", two_classes_model_coherence_window="yellow",
                                                                 simple_model_multi_looking="blue", CV_model_multi_looking="lightblue", two_classes_model_multi_looking="darkblue"))
dev.off()

pdf("./plots/plot06.pdf")
ggplot(KappaResults) +
  ylab("Coherence Range window size") +
  xlab("Model Kappa") +
  ggtitle("Coherence Window Size and Model Kappa") +
  geom_point(aes(x=model, y=coherence_window, color="simple_model")) +
  geom_smooth(aes(x=model, y=coherence_window, color="simple_model")) +
  geom_point(aes(x=model_cv, y=coherence_window, color="CV_model")) +
  geom_smooth(aes(x=model_cv, y=coherence_window, color="CV_model")) + 
  geom_point(aes(x=model_twoclasses, y=coherence_window, color="two_classes_model")) +
  geom_smooth(aes(x=model_twoclasses, y=coherence_window, color="two_classes_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="forestgreen", CV_model="lightgreen", two_classes_model="green"))
dev.off()

pdf("./plots/plot07.pdf")
ggplot(KappaResults) +
  ylab("Multi-Looking Range window size") +
  xlab("Model Kappa") +
  ggtitle("Multi-Looking Window Size and Model Kappa") +
  geom_point(aes(x=model, y=multi_looking, color="simple_model")) +
  geom_smooth(aes(x=model, y=multi_looking, color="simple_model")) +
  geom_point(aes(x=model_cv, y=multi_looking, color="CV_model")) +
  geom_smooth(aes(x=model_cv, y=multi_looking, color="CV_model")) + 
  geom_point(aes(x=model_twoclasses, y=multi_looking, color="two_classes_model")) +
  geom_smooth(aes(x=model_twoclasses, y=multi_looking, color="two_classes_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="darkblue", CV_model="purple", two_classes_model="blue"))
dev.off()

ggplot(KappaResults, aes(x=model)) +
  geom_point(aes(y=multi_looking, color=factor(coherence_window)), size=3) +
  geom_smooth(aes(y=multi_looking))

ggplot(KappaResults, aes(x=model_cv)) +
  geom_point(aes(y=multi_looking, color=factor(coherence_window)), size=3) +
  geom_smooth(aes(y=multi_looking))

ggplot(KappaResults, aes(x=model_twoclasses)) +
  geom_point(aes(y=multi_looking, color=factor(coherence_window)), size=3) +
  geom_smooth(aes(y=multi_looking))

ggplot(KappaResults[KappaResults$multi_looking == 10,]) +
  geom_point(aes(x=coherence_window, y=model)) +
  geom_smooth(aes(x=coherence_window, y=model)) +
  geom_point(aes(x=coherence_window, y=model_cv)) +
  geom_smooth(aes(x=coherence_window, y=model_cv)) +
  geom_point(aes(x=coherence_window, y=model_twoclasses)) +
  geom_smooth(aes(x=coherence_window, y=model_twoclasses))

pdf("./plots/plot08.pdf")
ggplot(KappaResults[KappaResults$coherence_window == 7,], aes(x=multi_looking)) +
  ggtitle("Influence of Multi-Looking on Model Kappa \nwith a Coherence Window Size of 7x2") +
  xlab("Multi-Looks Range") +
  ylab("Model Kappa") +
  geom_point(aes(y=model, color="simple_model")) +
  geom_smooth(aes(y=model, color="simple_model")) +
  geom_point(aes(y=model_cv, color="CV_model")) +
  geom_smooth(aes(y=model_cv, color="CV_model")) +
  geom_point(aes(y=model_twoclasses, color="two_classes_model")) +
  geom_smooth(aes(y=model_twoclasses, color="two_classes_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="darkblue", CV_model="lightblue", two_classes_model="blue"))
dev.off()

pdf("./plots/plot09.pdf")
ggplot(KappaResults[KappaResults$coherence_window == 10,], aes(x=multi_looking)) +
  ggtitle("Influence of Multi-Looking on Model Kappa \nwith a Coherence Window Size of 10x3") +
  xlab("Multi-Looks Range") +
  ylab("Model Kappa") +
  geom_point(aes(y=model, color="simple_model")) +
  geom_smooth(aes(y=model, color="simple_model")) +
  geom_point(aes(y=model_cv, color="CV_model")) +
  geom_smooth(aes(y=model_cv, color="CV_model")) +
  geom_point(aes(y=model_twoclasses, color="two_classes_model")) +
  geom_smooth(aes(y=model_twoclasses, color="two_classes_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="darkblue", CV_model="lightblue", two_classes_model="blue"))
dev.off()

pdf("./plots/plot10.pdf")
ggplot(KappaResults[KappaResults$coherence_window == 15,], aes(x=multi_looking)) +
  ggtitle("Influence of Multi-Looking on Model Kappa \nwith a Coherence Window Size of 15x4") +
  xlab("Multi-Looks Range") +
  ylab("Model Kappa") +
  geom_point(aes(y=model, color="simple_model")) +
  geom_smooth(aes(y=model, color="simple_model")) +
  geom_point(aes(y=model_cv, color="CV_model")) +
  geom_smooth(aes(y=model_cv, color="CV_model")) +
  geom_point(aes(y=model_twoclasses, color="two_classes_model")) +
  geom_smooth(aes(y=model_twoclasses, color="two_classes_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="darkblue", CV_model="lightblue", two_classes_model="blue"))
dev.off()

pdf("./plots/plot11.pdf")
ggplot(KappaResults[KappaResults$coherence_window == 20,], aes(x=multi_looking)) +
  ggtitle("Influence of Multi-Looking on Model Kappa \nwith a Coherence Window Size of 20x5") +
  xlab("Multi-Looks Range") +
  ylab("Model Kappa") +
  geom_point(aes(y=model, color="simple_model")) +
  geom_smooth(aes(y=model, color="simple_model")) +
  geom_point(aes(y=model_cv, color="CV_model")) +
  geom_smooth(aes(y=model_cv, color="CV_model")) +
  geom_point(aes(y=model_twoclasses, color="two_classes_model")) +
  geom_smooth(aes(y=model_twoclasses, color="two_classes_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="darkblue", CV_model="lightblue", two_classes_model="blue"))
dev.off()

pdf("./plots/plot12.pdf")
ggplot(KappaResults[KappaResults$multi_looking == 4,], aes(x=coherence_window)) +
  ggtitle("Influence of Coherence Window Size on Model Kappa \nat a 4x1 Multi-Looking Window") +
  xlab("Coherence Window Range") +
  ylab("Model Kappa") +
  geom_point(aes(y=model, color="simple_model")) +
  geom_smooth(aes(y=model, color="simple_model")) +
  geom_point(aes(y=model_cv, color="CV_model")) +
  geom_smooth(aes(y=model_cv, color="CV_model")) +
  geom_point(aes(y=model_twoclasses, color="two_classes_model")) +
  geom_smooth(aes(y=model_twoclasses, color="two_classes_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="forestgreen", CV_model="lightgreen", two_classes_model="green"))
dev.off()

pdf("./plots/plot13.pdf")
ggplot(KappaResults[KappaResults$multi_looking == 6,], aes(x=coherence_window)) +
  ggtitle("Influence of Coherence Window Size on Model Kappa \nat a 6x2 Multi-Looking Window") +
  xlab("Coherence Window Range") +
  ylab("Model Kappa") +
  geom_point(aes(y=model, color="simple_model")) +
  geom_smooth(aes(y=model, color="simple_model")) +
  geom_point(aes(y=model_cv, color="CV_model")) +
  geom_smooth(aes(y=model_cv, color="CV_model")) +
  geom_point(aes(y=model_twoclasses, color="two_classes_model")) +
  geom_smooth(aes(y=model_twoclasses, color="two_classes_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="forestgreen", CV_model="lightgreen", two_classes_model="green"))
dev.off()

pdf("./plots/plot14.pdf")
ggplot(KappaResults[KappaResults$multi_looking == 10,], aes(x=coherence_window)) +
  ggtitle("Influence of Coherence Window Size on Model Kappa \nat a 10x3 Multi-Looking Window") +
  xlab("Coherence Window Range") +
  ylab("Model Kappa") +
  geom_point(aes(y=model, color="simple_model")) +
  geom_smooth(aes(y=model, color="simple_model")) +
  geom_point(aes(y=model_cv, color="CV_model")) +
  geom_smooth(aes(y=model_cv, color="CV_model")) +
  geom_point(aes(y=model_twoclasses, color="two_classes_model")) +
  geom_smooth(aes(y=model_twoclasses, color="two_classes_model")) +
  scale_color_manual(name="Type of model", values=c(simple_model="forestgreen", CV_model="lightgreen", two_classes_model="green"))
dev.off()

plot(newdata$Mean ~ newdata$ID)

pdf("./plots/plot15.pdf")
ggplot(newdata, aes(x = Mean, y = multi_looking)) +
  ggtitle("Multi-Looking Window and Mean of Model Kappa") +
  xlab("Mean of Model Kappa") +
  ylab("Multi-Looking Range Looks") +
  geom_point()+
  geom_smooth(method="lm")
dev.off()

pdf("./plots/plot16.pdf")
ggplot(newdata, aes(x = Mean, y = coherence_window)) +
  ggtitle("Coherence Window and Mean of Model Kappa") +
  xlab("Mean of Model Kappa") +
  ylab("Coherence Window") +
  geom_point()+
  geom_smooth(method="lm")
dev.off()

pdf("./plots/plot06b.pdf")
ggplot(KappaResults) +
  ylab("Coherence Range window size") +
  xlab("Model Kappa") +
  ggtitle("Coherence Window Size and Model Kappa") +
  geom_point(aes(x=model, y=coherence_window, color="simple_model")) +
  geom_smooth(aes(x=model, y=coherence_window, color="simple_model"), method="lm") +
  geom_point(aes(x=model_cv, y=coherence_window, color="CV_model")) +
  geom_smooth(aes(x=model_cv, y=coherence_window, color="CV_model"), method="lm") + 
  geom_point(aes(x=model_twoclasses, y=coherence_window, color="two_classes_model")) +
  geom_smooth(aes(x=model_twoclasses, y=coherence_window, color="two_classes_model"), method="lm") +
  scale_color_manual(name="Type of model", values=c(simple_model="forestgreen", CV_model="lightgreen", two_classes_model="green"))
dev.off()

pdf("./plots/plot07b.pdf")
ggplot(KappaResults) +
  ylab("Multi-Looking Range window size") +
  xlab("Model Kappa") +
  ggtitle("Multi-Looking Window Size and Model Kappa") +
  geom_point(aes(x=model, y=multi_looking, color="simple_model")) +
  geom_smooth(aes(x=model, y=multi_looking, color="simple_model"), method="lm") +
  geom_point(aes(x=model_cv, y=multi_looking, color="CV_model")) +
  geom_smooth(aes(x=model_cv, y=multi_looking, color="CV_model"), method="lm") + 
  geom_point(aes(x=model_twoclasses, y=multi_looking, color="two_classes_model")) +
  geom_smooth(aes(x=model_twoclasses, y=multi_looking, color="two_classes_model"), method="lm") +
  scale_color_manual(name="Type of model", values=c(simple_model="darkblue", CV_model="purple", two_classes_model="blue"))
dev.off()


listofframes[20]
best <- dff_1202_1802_1802_0803_20_5_ML_10_3_20_5_ML_10_3
boxplot(best$ratio_VV ~ best$Main_Damag.x, ylim=c(0.5,2.5))
middle <- dff_1202_1802_1802_0803_10_3_ML_6_2__10_3_ML_6_2_
boxplot(middle$ratio_VV ~ middle$Main_Damag.x, ylim=c(0.5,2.5))
worst <- dff_1202_1802_1802_0803_7_2_ML_10_3__7_2_ML_10_3_

pdf("./plots/plot19.pdf")
ggplot(best, aes(x = Main_Damag.x)) +
  ggtitle("Boxplot of Best Model") +
  xlab("Damage Class") +
  ylab("Coherence Change Ratio (VV)") +
  geom_boxplot(aes(y=ratio_VV))
dev.off()

pdf("./plots/plot20.pdf")
ggplot(middle, aes(x = Main_Damag.x)) +
  ggtitle("Boxplot of Medium Performing Model") +
  xlab("Damage Class") +
  ylab("Coherence Change Ratio (VV)") +
  geom_boxplot(aes(y=ratio_VV))
dev.off()

pdf("./plots/plot21.pdf")
ggplot(worst, aes(x = Main_Damag.x)) +
  ggtitle("Boxplot of Worst Performing Model") +
  xlab("Damage Class") +
  ylab("Coherence Change Ratio (VV)") +
  geom_boxplot(aes(y=ratio_VV))
dev.off()

# export as layer of dataframe
test_shp <- shp
for (k in seq(4,24,2)) {
  frame <- get(listofframes[k])
  name <- listofframes[k]
  test_shp[[name]] <- frame$ratio_VV
}
newnames <- names(test_shp)[1:10]
newnames <- c(newnames, "10x3_4x1", "10x3_6x2", "15x4_10x3", "15x4_4x1", "15x4_6x2", "20x5_10x3", "20x5_4x1", "20x5_6x2", "7x2_10x3", "7x2_4x1", "7x2_6x2")
names(test_shp) <- newnames
sf::write_sf(test_shp, dsn="./R_results/all_models_VV.shp")

# example frame
fr <- dff_1202_1802_1802_0803_15_4_ML_4_1__15_4_ML_4_1_
fr$status <- "incorrect"

up <- 1.9
low <- 1.6
up <- 1.9
low <- 1.8
up <- 1.9
low <- 1.85
for(g in 1:1052) {
  if(fr[g,2] > up) {
    fr[g,6] <- "Major damage"
  }
  if(fr[g,2] > low && fr[g,2] < up) {
    fr[g,6] <- "Minor damage"
  }
  if(fr[g,2] < low) {
    fr[g,6] <- "No visible damage"
  }
}

for (g in 1:1052) {
  if(fr[g,2] > up && fr[g,3] == "Major damage") {
    fr[g,6] <- "Major - correct"
  }
  if(fr[g,2] > up && fr[g,3] == "Minor damage") {
    fr[g,6] <- "overest - damage"
  }
  if(fr[g,2] > up && fr[g,3] == "No visible damage") {
    fr[g,6] <- "overest - no damage"
  }
  if(fr[g,2] > low && fr[g,2] < up && fr[g,3] == "Minor damage") {
    fr[g,6] <- "Minor - correct"
  }
  if(fr[g,2] > low && fr[g,2] < up && fr[g,3] == "Major damage") {
    fr[g,6] <- "underest - damage"
  }
  if(fr[g,2] > low && fr[g,2] < up && fr[g,3] == "No visible damage") {
    fr[g,6] <- "overest - no damage"
  }
  if(fr[g,2] < low && fr[g,3] == "No visible damage") {
    fr[g,6] <- "No damage - correct"
  }
  if(fr[g,2] < low && fr[g,3] == "Major damage") {
    fr[g,6] <- "underest - no damage"
  }
  if(fr[g,2] < low && fr[g,3] == "Minor damage") {
    fr[g,6] <- "underest - no damage"
  }
}

ex_shp <- shp
ex_shp[["status"]] <- fr$status
sf::write_sf(ex_shp, dsn="./R_results/status_shp_1985.shp")

correc <- matrix(c(1,1,2), ncol=3)
correc <- as.data.frame(correc)
correc2 <- matrix(c(1,1,2), ncol=3)
correc2 <- as.data.frame(correc2)

for (top in seq(1,3,0.05)) {
  for (bot in seq(0,2,0.05)) {
    if(top > bot) {
      fr <- dff_1202_1802_1802_0803_15_4_ML_4_1__15_4_ML_4_1_
      
      count = 0
      for (g in 1:1052) {
        if(fr[g,2] > top && fr[g,3] == "Major damage") {
          count <- count + 1
        }
        # if(fr[g,2] > top && fr[g,3] == "Minor damage") {
        #   count <- count + 1
        # }
        if(fr[g,2] > bot && fr[g,2] < top && fr[g,3] == "Minor damage") {
          count <- count + 1
        }
        # if(fr[g,2] > bot && fr[g,2] < top && fr[g,3] == "Major damage") {
        #   count <- count + 1
        # }
        if(fr[g,2] < bot && fr[g,3] == "No visible damage") {
          count <- count + 1
        }
      }
      correc2 <- rbind(correc2, c(top, bot, count))
    }
  }
}

# orig algo
for (g in 1:1052) {
  if(fr[g,2] > top && fr[g,3] == "Major damage") {
    fr[g,6] <- "correct"
  }
  if(fr[g,2] > top && fr[g,3] == "Minor damage") {
    fr[g,6] <- "correct"
  }
  if(fr[g,2] > bot && fr[g,2] < top && fr[g,3] == "Minor damage") {
    fr[g,6] <- "correct"
  }
  if(fr[g,2] > bot && fr[g,2] < top && fr[g,3] == "Major damage") {
    fr[g,6] <- "correct"
  }
  if(fr[g,2] < bot && fr[g,3] == "No visible damage") {
    fr[g,6] <- "correct"
  }
}

## try and make NDVI comparison
# choose best model: 15x4, 4x1
listofframes[10]
frame_NONDVI <- dff_1202_1802_1802_0803_15_4_ML_4_1__15_4_ML_4_1_

# make NDVI frames
filelist
# 5 & 17
path <- "/home/petra/Downloads/damascus_SLC/case_study_one/TIF"
# create raster stacks
SAR_pre <- raster::stack(paste(path,filelist[5], sep="/"))
SAR_post <- raster::stack(paste(path,filelist[17], sep="/"))

ras_020NDVI <- SAR_ratio
ras_035NDVI <- SAR_ratio
# apply ndvi threshold
ndvi <- resample(ndvi, SAR_ratio)
ndvi[ndvi > 0.2] <- NA
ras_020NDVI <- crop(ras_020NDVI, ndvi)
ras_020NDVI <- mask(x=ras_020NDVI, mask=ndvi)
ras_035NDVI <- crop(ras_035NDVI, ndvi)
ras_035NDVI <- mask(x=ras_035NDVI, mask=ndvi)

writeRaster(ras_035NDVI,"./R_results/SAR_ratio_15x4_4x1_035NDVI.grd",overwrite=TRUE)

# create raster df
SAR_ratio <- ras_035NDVI
df <- extract(SAR_ratio, shp, df=TRUE)
shp$PolyID <- 1:nrow(shp)
df <- merge(df,shp,by.x="ID",by.y="PolyID")

# create shp mean dff
dff <- merge(aggregate(ratio_VV ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
dfh <- merge(aggregate(ratio_VH ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
dff <- merge(dff, dfh, by.x = "ID", by.y = "ID")

SAR_ratio <- ras_035NDVI
# model training
predictors <- names(SAR_ratio)
trainDat <- dff[1:4]
model <- train(trainDat[,predictors],
               trainDat$Main_Damag.x,
               method="rf",
               importance=TRUE)
model_35NDVI <- model
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
model_cv_35NDVI <- model_cv
# break down to damage/no damage
trainDat2 <- trainDat
trainDat2[trainDat2$Main_Damag.x == "Major damage",3] <- "Minor damage"
model_twoclasses <- train(trainDat2[,predictors],
                          trainDat2$Main_Damag.x,
                          method="rf",
                          importance=TRUE)
model_twoclasses_35NDVI <- model_twoclasses

NDVI_Kappa_frame <- KappaResults[KappaResults$ID == 5,]
NDVI_Kappa_frame$NDVI <- 0
NDVI_Kappa_frame <- rbind(NDVI_Kappa_frame, c(model_02NDVI$results$Kappa, model_cv_02NDVI$results$Kappa, model_twoclasses_02NDVI$results$Kappa, "no specs", 13, 15, 4, 0, 0.2))
NDVI_Kappa_frame <- rbind(NDVI_Kappa_frame, c(model_35NDVI$results$Kappa, model_cv_35NDVI$results$Kappa, model_twoclasses_35NDVI$results$Kappa, "no specs", 13, 15, 4, 0, 0.35))

NDVI_Kappa_frame[,1] <- as.numeric(paste(NDVI_Kappa_frame[,1]))
NDVI_Kappa_frame[,2] <- as.numeric(paste(NDVI_Kappa_frame[,2]))
NDVI_Kappa_frame[,3] <- as.numeric(paste(NDVI_Kappa_frame[,3]))

NDVI_Kappa_frame$Mean <- rowMeans(NDVI_Kappa_frame[,1:3])

ggplot(NDVI_Kappa_frame, aes(x=Mean, y=NDVI)) +
  geom_point() +
  geom_smooth(method="lm")

coh15x4_ML4x1 <- raster::stack("./R_results/SAR_ratio_15_4_ML_4_1__15_4_ML_4_1_.grd")
# Plot with layer-list passed to `sp.layout`
brks <- seq(0,5,0.5)
spplot(ras_035NDVI$ratio_VV, col.regions = colo, at=brks)
spplot(ras_020NDVI$ratio_VV, col.regions = colo, at=brks)
spplot(coh15x4_ML4x1$ratio_VV, col.regions = colo, at=brks)

ndvi <- raster("NDVI.tif")
ndvi <- crop(ndvi, shp)
ndvi <- mask(x=ndvi, mask=shp)

ndvi <- resample(ndvi, SAR_ratio)
ndvi2 <- ndvi
ndvi35 <- ndvi
ndvi2[ndvi2 > 0.2] <- NA
ndvi35[ndvi35 > 0.35] <- NA
writeRaster(ndvi2,"NDVI_002_threshold.grd",overwrite=TRUE)
writeRaster(ndvi35,"NDVI_035_threshold.grd",overwrite=TRUE)

SAR_ratio02 <- SAR_ratio
SAR_ratio35 <- SAR_ratio

SAR_ratio02 <- crop(SAR_ratio02, ndvi2)
SAR_ratio02 <- mask(x=SAR_ratio02, mask=ndvi2)
SAR_ratio35 <- crop(SAR_ratio35, ndvi35)
SAR_ratio35 <- mask(x=SAR_ratio35, mask=ndvi35)

writeRaster(SAR_ratio02,"RATIO_002_threshold.grd",overwrite=TRUE)
writeRaster(SAR_ratio35,"RATIO_035_threshold.grd",overwrite=TRUE)

nd <- seq(-1, 1, 0.2)
spplot(ndvi, col.regions = colo, at = nd)

pred <- predict(model,dff)
table(pred,dff$Main_Damag.x)[1,1]
v <- confusionMatrix(pred,factor(dff$Main_Damag.x))
v$table[1,1]
