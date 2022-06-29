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

data_lip2<- read_excel("\\\\smb.isilon01.ad.maastro.nl\\research\\Projects\cds\\p0389-protrait\\Miriam\\data_dysphagia_2_lipp2.xlsx")

data_lip2

df_d2 <- data_lip2[,c("hn_tox_dysph", "linear_factor_photons", "Dmean mondholte (Gy)_FOT", "Dmean PCM superior (Gy)_FOT", "Dmean PCM medius (Gy)_FOT", "Dmean PCM inferior (Gy)_FOT", "Baseline dysfagie", "Tumorlokatie")]
df_d2

is.na.data.frame(df_d2)

# compute the 4 models
m1 <- glm(hn_tox_dysph~ -1 + offset(linear_factor_photons), data = df_d2, family = binomial)

m2 <- glm(hn_tox_dysph ~ 1 +offset(linear_factor_photons), data = df_d2, family = binomial)

m3 <- glm(hn_tox_dysph ~1 +  linear_factor_photons,  data = df_d2, family = binomial)

formula_4 <- as.formula(
  "hn_tox_dysph ~  1+ `Dmean mondholte (Gy)_FOT` + `Dmean PCM superior (Gy)_FOT` + `Dmean PCM medius (Gy)_FOT` + `Dmean PCM inferior (Gy)_FOT` + `Baseline dysfagie` + `Tumorlokatie`"
)

m4 <- glm(formula_4, data = data_lip2, family = binomial)

m1
m2
m3
m4

# ROC curve and calibration plot

roc(hn_tox_dysph~m1$fitted.values, data = df_d2, plot = TRUE,lwd = 2, legacy.axes = TRUE, print.auc=TRUE, main = "ROC CURVE", col= "blue")
# calPlot2(list(m1,m2),data=data_lip2)

library(ModelGood)
df_d2$hn_tox_dysph<- ifelse(df_d2$hn_tox_dysph =="Yes",1,0)
df_d2$hn_tox_dysph

calPlot2(list(m1,m2,m3,m4),data=df_d2,legend=FALSE,xlab = "Predicted probability" ,ylab = "Observed probability"
)


# plotl()
legend(x = 0.8, y = 0.3, # Coordinates          # Position
       legend = c("original", "calibration in the large", "calibration","revision"),  # Legend texts
       lty = c(1),
       bty = "n",
       # inset = c(-0.45, 0),# Line types
       cex = 0.6, # Change legend size
       
       col = c("black","red","green", "lightblue"),           # Line colors
       lwd = 2,xpd = TRUE,horiz = FALSE)

# Model predicted probabilties
df_d2$m1_pred <- predict(m1, type = "response")
df_d2$m2_pred <- predict(m2, type = "response")
df_d2$m3_pred <- predict(m3, type = "response")
df_d2$m4_pred <- predict(m4, type = "response")

# Brier Score
install.packages('DescTools')
library(DescTools)

BrierScore(m1, scaled = FALSE)
BrierScore(m2, scaled = FALSE)
BrierScore(m3, scaled = FALSE)
BrierScore(m4, scaled = FALSE)


# Metrics  
val_m1 <- val.prob(df_d2$m1_pred, as.numeric(df_d2$hn_tox_dysph ) - 1, 
                   pl = FALSE) %>% round(3)
val_m2 <- val.prob(df_d2$m2_pred, as.numeric(df_d2$hn_tox_dysph ) - 1, 
                   pl = FALSE) %>% round(3)
val_m3 <- val.prob(df_d2$m3_pred, as.numeric(df_d2$hn_tox_dysph ) - 1, 
                   pl = FALSE) %>% round(3)
val_m4 <- val.prob(df_d2$m4_pred, as.numeric(df_d2$hn_tox_dysph ) - 1, 
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
install.packages("mdscore")
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


