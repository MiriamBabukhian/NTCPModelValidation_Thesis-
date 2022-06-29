#######################################################
#####install all the packages and libraries I need#####
#######################################################
library(dplyr)
library(readxl)
# install.packages("data.table")
library("data.table")
library(tidyverse)
library(rms)
library(Hmisc)
library(knitr)
library(broom)
library(pander)
library(ggbeeswarm)
library(gridExtra)
library(grid)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(viridis)

library(mlbench)
# library(MASS)
library(pROC)
# require(MASS)

# load and inspect the data
data_xerostomia_lipp2 <- read_excel("/Volumes/research/Projects/cds/p0389-protrait/Miriam/data_xerostomia_3_lipp2.xlsx") #contains all the NTCP values
data_xerostomia_lipp2
df_x2 <- data_xerostomia_lipp2[,c("eortc_hn35_dry_mouth", "linear_factor_photons", "Dmean parotis contralateraal (Gy)_FOT", "Dmean parotis ipsilateraal (Gy)_FOT", "Dmean submand beiderzijds (Gy)_FOT", "Baseline droge mond")]
df_x2

is.na.data.frame(df_x2)

# compute the 4 models 
m1 <- glm(eortc_hn35_dry_mouth ~ -1 + offset(linear_factor_photons), data = df_x2, family = binomial)
m1

m2 <- glm(eortc_hn35_dry_mouth ~ 1+ offset(linear_factor_photons), data = df_x2, family = binomial)
m2

m3 <- glm(eortc_hn35_dry_mouth ~1 +  linear_factor_photons,  data = df_x2, family = binomial)
m3

formula_4 <- as.formula(
  "eortc_hn35_dry_mouth ~ 1+(sqrt(`Dmean parotis ipsilateraal (Gy)_FOT`) + sqrt(`Dmean parotis contralateraal (Gy)_FOT`)) + `Dmean submand beiderzijds (Gy)_FOT` + `Baseline droge mond` "
)

m4 <- glm(formula_4, data = df_x2, family = binomial)
m4

# library(data.table)

# ROC curve and calibration plot

roc(eortc_hn35_dry_mouth~m1$fitted.values, data = df_x2, plot = TRUE,lwd = 2, legacy.axes = TRUE, print.auc=TRUE, main = "ROC CURVE", col= "blue")
library(ModelGood)
joint$hn_tox_dysph<- ifelse(joint$hn_tox_dysph =="Yes",1,0) 

calPlot2(list(m1,m2,m3,m4),data=data_xerostomia_lipp2,legend=FALSE,xlab = "Predicted probability" ,ylab = "Observed probability")

plotl()
legend(x = 0.8, y = 0.3, # Coordinates          # Position
       legend = c("original", "calibration in the large", "calibration","revision"),  # Legend texts
       lty = c(1),
       bty = "n",
       # inset = c(-0.45, 0),# Line types
       cex = 0.6, # Change legend size
       
       col = c("black","red","green", "lightblue"),           # Line colors
       lwd = 2,xpd = TRUE,horiz = FALSE)


# Model predicted probabilties
df_x2$m1_pred <- predict(m1, type = "response")
df_x2$m2_pred <- predict(m2, type = "response")
df_x2$m3_pred <- predict(m3, type = "response")
df_x2$m4_pred <- predict(m4, type = "response")

# Brier Score

library(DescTools)

BrierScore(m1, scaled = FALSE)
BrierScore(m2, scaled = FALSE)
BrierScore(m3, scaled = FALSE)
BrierScore(m4, scaled = FALSE)


# Metrics  
val_m1 <- val.prob(df_x2$m1_pred, as.numeric(df_x2$eortc_hn35_dry_mouth ) - 1, 
                   pl = FALSE) %>% round(3)
val_m2 <- val.prob(df_x2$m2_pred, as.numeric(df_x2$eortc_hn35_dry_mouth ) - 1, 
                   pl = FALSE) %>% round(3)
val_m3 <- val.prob(df_x2$m3_pred, as.numeric(df_x2$eortc_hn35_dry_mouth ) - 1, 
                   pl = FALSE) %>% round(3)
val_m4 <- val.prob(df_x2$m4_pred, as.numeric(df_x2$eortc_hn35_dry_mouth ) - 1, 
                   pl = FALSE) %>% round(3)

# ended up using the normal Brier Score (between 0 and 1) and not the rescaled one coded here

rescale_brier <- function(x, p, ...){ 
  format(round(1 - (x / (mean(p) * (1 - mean(p)))), digits = 2), nsmall = 2)
}

b1 <- rescale_brier(val_m1["Brier"], 0.50) 
b2 <- rescale_brier(val_m2["Brier"], 0.50)
b3 <- rescale_brier(val_m3["Brier"], 0.50)
b4 <- rescale_brier(val_m4["Brier"], 0.50)
# Note: 0.50 is the marginal probabilty of developing NTC in the entire sample

install.packages("pander")
library(pander)

pander(val_m1) 
pander(val_m2) 
pander(val_m3) 
pander(val_m4) 


# closed tetsing procedure 

library(mdscore)
lr.test(m1,m4)
lr.test(m2,m4)
lr.test(m3,m4)


if(lr.test(m1,m4)$pvalue>0.05){
  print("Adopt the original model")
}else if(lr.test(m2,m4)$pvalue>0.05){
  print("Adopt the model intercept")
}else if (lr.test(m3,m4)$pvalue>0.05){
  print("adopt the recalibrated model")
}else if(lr.test(m3,m4)$pvalue<0.05){
  print("Adopt the revised model")
}
