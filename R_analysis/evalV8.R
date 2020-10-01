# ANOVA
# test a 15x4, 4x1 model with anova, and models with all kinds of damage
setwd("/home/petra/Downloads/BA_DATA/sync_level")
library(rstatix)
library(ggplot2)
library(ggpubr)

dff <- dff_1202_1802_1802_0803_15_4_ML_4_1__15_4_ML_4_1_
head(dff)
dff <- dff[,1:4]
data("dff")
# show random row 
dff %>% sample_n_by(Main_Damag.x, size=1)
# order by levels
levels(dff$Main_Damag.x)
dff <- dff %>% reorder_levels(Main_Damag.x, order = c("No visible damage", "Minor damage", "Major damage"))
dff %>% group_by(Main_Damag.x) %>% get_summary_stats(ratio_VV, type="mean_sd")
# no extreme outliers
a <- dff %>% group_by(Main_Damag.x) %>% identify_outliers(ratio_VV)
# check normality
Model <- lm(ratio_VV ~ Main_Damag.x, data=dff)
## qq plot good and shapiro test also passed
ggqqplot(residuals(Model))
shapiro_test(residuals(Model))
# shapiro for each group
dff %>% group_by(Main_Damag.x) %>% shapiro_test(ratio_VV)
ggqqplot(dff, "ratio_VV", facet.by = "Main_Damag.x")
# ANOVA
res.aov <- dff %>% anova_test(ratio_VV ~ Main_Damag.x)
res.aov
pwc <- dff %>% tukey_hsd(ratio_VV ~ Main_Damag.x)
pwc
pwc <- pwc %>% add_xy_position(x = "Main_Damag.x")
ggboxplot(dff, x = "Main_Damag.x", y = "ratio_VV") +
        stat_pvalue_manual(pwc, hide.ns = TRUE) +
        labs(
                subtitle = get_test_label(res.aov, detailed = TRUE),
                caption = get_pwc_label(pwc)
        )

#### 2 way ANOVA
ggboxplot(dff, x = "ratio_VV", y = "ratio_VH")
dff %>% group_by(Main_Damag.x) %>% identify_outliers(ratio_VV)

Model1  <- lm(ratio_VV*ratio_VH ~ Main_Damag.x,
             data = dff)
ggqqplot(residuals(Model1))
ggqqplot(dff, "Main_Damag.x", ggtheme = theme_bw()) +
        facet_grid(ratio_VV ~ ratio_VH)

### boxplot between NDVI and no NDVI threshold
library(raster)

SAR_ratio <- raster::stack("./R_results/SAR_ratio_15_4_ML_4_1__15_4_ML_4_1_.grd")
shp <- sf::read_sf("./UNOSAT_damascus_new_shapes/Rapidly assessed damage occurring between 23 February 2018 and 6 March 2018 in Eastern Ghouta Area, Damascus Governorate/RDA_Damascus_20180306.shp")

SAR_ratio <- crop(SAR_ratio, shp)
SAR_ratio <- mask(x=SAR_ratio, mask=shp)

ndvi <- raster("NDVI.tif")
ndvi <- crop(ndvi, shp)
ndvi <- mask(x=ndvi, mask=shp)

ndvi <- resample(ndvi, SAR_ratio)

ndvi35 <- ndvi
ndvi35[ndvi35 > 0.35] <- NA

SAR_ratio35 <- SAR_ratio
SAR_ratio35 <- crop(SAR_ratio35, ndvi35)
SAR_ratio35 <- mask(x=SAR_ratio35, mask=ndvi35)

# create raster df
df <- extract(SAR_ratio, shp, df=TRUE)
shp$PolyID <- 1:nrow(shp)
df <- merge(df,shp,by.x="ID",by.y="PolyID")

# create shp mean dff
dff <- merge(aggregate(ratio_VV ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
dfh <- merge(aggregate(ratio_VH ~ ID, data=df, mean, na.rm=TRUE), 
             aggregate(Main_Damag ~ ID, data=df, head, 1))
dff <- merge(dff, dfh, by.x = "ID", by.y = "ID")

pdf("./plots/plot50.pdf")
ggplot(dff, aes(x = Main_Damag.x)) +
  ggtitle("Boxplot") +
  xlab("Damage Class") +
  ylab("Coherence Change Ratio (VV)") +
  geom_boxplot(aes(y=ratio_VV))
dev.off()

pdf("./plots/plot51.pdf")
ggplot(dff35, aes(x = Main_Damag.x)) +
  ggtitle("Boxplot") +
  xlab("Damage Class") +
  ylab("Coherence Change Ratio (VV)") +
  geom_boxplot(aes(y=ratio_VV))
dev.off()

