######## Description ########
# detailed ANOVA and MANOVA
#############################
pred <- predict(SAR_ratio, model_cv)
spplot(pred)

p <- extract(pred, shp)
li <- c(1,2)
for (i in 1:1052) {
  v <- mean(p[[i]])
  if(v < 1.51) {
    li[i] <- "Major damage"
  }
  if(v < 2.51) {
    li[i] <- "Minor damage"
  }
  else {
    li[i] <- "No visible damage"
  }
}

base::table(factor(li), factor(trainDat$Main_Damag.x))
# no whole shp cells are classified as major damage, mean and median are always not majdam
# validating against shp with diff res is difficult

pf <- extract(pred, shp, df=TRUE)
pf <- merge(pf,shp,by.x="ID",by.y="PolyID")
pf <- pf[,c(1,2,8)]

# producers acc.
# how often are real features correctly shown
# omission error (left out): col down. how many of tr. data were classified diff.?

# users acc:
# how often is map class actually the ground class
# comission error: rows. from class. point of view. how many class. data are wrongly class?

# -> so: is the class. result in the correct training bin -> prod acc.
# but: high, high error due to all other pixels being there.

########### ANOVA ############
setwd("/home/petra/Downloads/BA_DATA/sync_level")
library(rstatix)
library(ggplot2)
library(ggpubr)
library(car)
library(broom)
library(tidyverse)

dff <- dff_1202_1802_1802_0803_15_4_ML_4_1__15_4_ML_4_1_[,1:4]
names(dff) <- c("ID", "ratio_VV", "Main_Damag", "ratio_VH")
# levels and reorder
dff$Main_Damag <- as.factor(dff$Main_Damag)
levels(dff$Main_Damag)
dff <- dff %>% reorder_levels(Main_Damag, order = c("No visible damage", "Minor damage", "Major damage"))
# info
dff %>% group_by(Main_Damag) %>% get_summary_stats(c(ratio_VH, ratio_VV), type = "mean_sd")

# OUTLIERS
outliers <- dff %>% group_by(Main_Damag) %>% identify_outliers(ratio_VH)
outliers$is.extreme

# NORMALITY
model  <- lm(ratio_VV ~ Main_Damag, data = dff)
ggqqplot(residuals(model)) +
  ggtitle("QQ plot of model residuals")
shapiro_test(residuals(model))
# for each group now
dff %>% group_by(Main_Damag) %>% shapiro_test(ratio_VV)
# qqplots for each group
ggqqplot(dff, "ratio_VH", facet.by = "Main_Damag")

# HOMOGENEITY
plot(model, 1)
dff %>% levene_test(ratio_VV ~ Main_Damag)

# ANOVA
res.aov <- dff %>% anova_test(ratio_VH ~ Main_Damag)
res.aov

# POST_HOC TUKEY
pwc <- dff %>% tukey_hsd(ratio_VH ~ Main_Damag)
pwc

########### MANOVA
# merged poxplot!
ggboxplot(dff, x = "Main_Damag", y = c("ratio_VV", "ratio_VH"), merge = TRUE, palette = "jco")

# sample size given

# univariate outliers
# given for both
dff %>% group_by(Main_Damag) %>% identify_outliers(ratio_VH)

# multivariate outliers
iris2 %>% group_by(Species) %>%
  mahalanobis_distance(-id) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

# NORMALITY
# shapiro needs to be > 0.05
dff %>% group_by(Main_Damag) %>% shapiro_test(ratio_VH, ratio_VV) %>% arrange(variable)
# plots
ggqqplot(dff, "ratio_VH", facet.by = "Main_Damag")
ggqqplot(dff, "ratio_VV", facet.by = "Main_Damag")
# -> not superduper normal distribution but MANOVA is robust to deviation from normality

# multivariate normality
dff %>% select(ratio_VH, ratio_VV) %>% mshapiro_test()
# -> NOT given

# colinearity
dff %>% cor_test(ratio_VV, ratio_VH)
# -> not too low, not too high

# linearity
library(GGally)
res <- dff %>% select(ratio_VH, ratio_VV, Main_Damag) %>% group_by(Main_Damag) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
res$plots[[1]]
res$plots[[2]]
res$plots[[3]]
# -> some loss of power is assumed

# homogeneity of covariances
box_m(dff[, c("ratio_VH", "ratio_VV")], dff$Main_Damag)

# homogeneity of variances
dff %>% gather(key = "variable", value = "value", ratio_VH, ratio_VV) %>%
  group_by(variable) %>%
  levene_test(value ~ Main_Damag)

# binary model
dfb <- dff
dfb$Main_Damag[dfb$Main_Damag == "Major damage"] <- "damage"
dfb$Main_Damag[dfb$Main_Damag == "Minor damage"] <- "damage"
model1 <- lm(cbind(ratio_VV, ratio_VH) ~ Main_Damag, dfb)
Manova(model1, test.statistic = "Pillai")

# CALCULATION
model1 <- lm(cbind(ratio_VV, ratio_VH) ~ Main_Damag, dff)
Manova(model1, test.statistic = "Pillai")
# test checks out

# now single anovas are possible

# Games Howell
pwc <- dff %>%
  gather(key = "variables", value = "value", ratio_VH, ratio_VV) %>%
  group_by(variables) %>%
  games_howell_test(value ~ Main_Damag) %>%
  select(-estimate, -conf.low, -conf.high) # Remove details
pwc
# minor damage & VH seems not significant

# plot
pwc <- pwc %>% add_xy_position(x = "damage")
test.label <- create_test_label(
  description = "MANOVA", statistic.text = quote(italic("F")),
  statistic = 127.68, p= "<0.0001", parameter = "4,2098",
  type = "expression", detailed = TRUE
)
ggboxplot(
  dff, x = "Main_Damag", y = c("ratio_VH", "ratio_VV"), 
  merge = TRUE, palette = "jco"
) + 
  #stat_pvalue_manual(
   # pwc, hide.ns = TRUE, tip.length = 0, 
  #  step.increase = 0.1, step.group.by = "variables",
  #  color = "variables"
  #) +
  labs(
    subtitle = test.label,
    caption = get_pwc_label(pwc, type = "expression")
  )

#### ANOVA for coherence / multi-looking
res.aov <- KappaResults %>% anova_test(coherence_window ~ Mean)
res.aov

ggplot(KappaResults, aes(y=Mean, x=coherence_window)) +
  geom_point(aes(color = coherence_window))

# info
KappaResults %>% group_by(coherence_window) %>% get_summary_stats(Mean, type = "mean_sd")

# OUTLIERS
outliers <- KappaResults %>% group_by(coherence_window) %>% identify_outliers(Mean)
outliers$is.extreme

# NORMALITY
model  <- lm(Mean ~ coherence_window, data = KappaResults)
ggqqplot(residuals(model)) +
  ggtitle("QQ plot of model residuals")
shapiro_test(residuals(model))
# for each group now
KappaResults %>% group_by(coherence_window) %>% shapiro_test(Mean)
# qqplots for each group
ggqqplot(KappaResults, "Mean", facet.by = "coherence_window")

# HOMOGENEITY
plot(model, 1)
KappaResults %>% levene_test(coherence_window ~ Main_Damag)

# ANOVA
res.aov <- KappaResults %>% anova_test(Mean ~ coherence_window)
res.aov

# POST_HOC TUKEY
pwc <- dff %>% tukey_hsd(ratio_VH ~ Main_Damag)
pwc
