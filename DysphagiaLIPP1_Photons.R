library(dplyr)

install.packages("readxl")
# install.packages("data.table")
library("data.table")
library(tidyverse)
library(readxl)
install.packages("rms")
library(rms)
install.packages("Hmisc")
library(Hmisc)
library(knitr)
library(broom)
install.packages("pander")
library(pander)
install.packages("ggbeeswarm")
install.packages("ggplot2")
library(ggbeeswarm)
library(gridExtra)
library(grid)
install.packages("sjPlot")
library(sjPlot)
library(sjmisc)
library(sjlabelled)
install.packages("viridisLite")
library(viridis)
install.packages("mlbench")
library(mlbench)
install.packages("MASS")
library(MASS)
library(pROC)
# require(MASS)
library(dplyr)

# load and inspect the data

data_dysphagia_lipp1 <- read_excel("/Volumes/research/Projects/cds/p0389-protrait/Miriam/data_dysphagia_lipp1.xlsx")

data_dysphagia_lipp1

df1 <- data_dysphagia_lipp1[,c("Dmean mondholte (Gy)_FOT", "Dmean PCM superior (Gy)_FOT", "hn_tox_dysph_six_months", "hn_tox_dysph_baseline", "linear_factor_photons")]
df1

is.na.data.frame(df1)

# Compute the 4 models

m1 <- glm(hn_tox_dysph_six_months ~ -1 + offset(linear_factor_photons), data = df1, family = binomial)
m1

m2 <- glm(hn_tox_dysph_six_months ~ 1 + offset(linear_factor_photons), data = df1, family = binomial)
m2

m3 <- glm(hn_tox_dysph_six_months ~ 1 + linear_factor_photons, data = df1, family = binomial)
m3

formula_4 <- as.formula(
  "hn_tox_dysph_six_months ~ 1+`Dmean mondholte (Gy)_FOT`+ `Dmean PCM superior (Gy)_FOT` + `hn_tox_dysph_baseline` "
)

m4 <- glm(formula_4, data = df1, family = binomial)
m4

# ROC and calibration plot

roc_1 <- roc(hn_tox_dysph_six_months~m1$fitted.values, data = df1, plot = TRUE,lwd = 2, legacy.axes = TRUE, print.auc=TRUE, main = "ROC CURVE", col= "blue")
library(ModelGood)

calPlot2(list(m1,m2,m3,m4),data=df1,legend=FALSE,xlab = "Predicted probability" ,ylab = "Observed probability", main="Title")
legend(x = 0.8, y = 0.3,           
       legend = c("original", "calibration in the large", "calibration","revision"),  
       lty = c(1),
       bty = "n",
       # inset = c(-0.45, 0),
       cex = 0.6, 
       
       col = c("black","red","green", "lightblue"),          
       lwd = 2,xpd = TRUE,horiz = FALSE)

# Model predicted probabilties
df1$m1_pred <- predict(m1, type = "response")
df1$m2_pred <- predict(m2, type = "response")
df1$m3_pred <- predict(m3, type = "response")
df1$m4_pred <- predict(m4, type = "response")

# Brier Score

library(DescTools)

BrierScore(m1, scaled = FALSE)
BrierScore(m2, scaled = FALSE)
BrierScore(m3, scaled = FALSE)
BrierScore(m4, scaled = FALSE)


# Metrics  
val_m1 <- val.prob(df1$m1_pred, as.numeric(df1$hn_tox_dysph_six_months ) - 1, 
                   pl = FALSE) %>% round(3)
val_m2 <- val.prob(df1$m2_pred, as.numeric(df1$hn_tox_dysph_six_months ) - 1, 
                   pl = FALSE) %>% round(3)
val_m3 <- val.prob(df1$m3_pred, as.numeric(df1$hn_tox_dysph_six_months ) - 1, 
                   pl = FALSE) %>% round(3)
val_m4 <- val.prob(df1$m4_pred, as.numeric(df1$hn_tox_dysph_six_months ) - 1, 
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

install.packages('Rcpp')
library(Rcpp)

pander(val_m1) 
pander(val_m2) 
pander(val_m3) 
pander(val_m4) 

# closed testing procedure 

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

# since the revised model was selected the AUC for M4 should be calculated 

roc_2 <- roc(hn_tox_dysph_six_months~m4$fitted.values, data = df1, plot = TRUE,lwd = 2, legacy.axes = TRUE, print.auc=TRUE, main = "ROC CURVE", col= "blue")
plot(roc_1, col="blue")
plot(roc_2, add=TRUE, col="red")
legend(x = 0.8, y = 0.3,           
       legend = c("Original model, AUC = 0,716", "Revised model, AUC = 0,759"),  
       lty = c(1),
       bty = "n",
       # inset = c(-0.45, 0),
       cex = 0.6, 
       
       col = c("blue","red"),          
       lwd = 2,xpd = TRUE,horiz = FALSE)
