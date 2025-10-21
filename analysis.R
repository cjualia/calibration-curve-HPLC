# Title: Calibration curve for HPLC data anaylsis
# Author: Cristina Juárez Alía
# Date: 20-10-2025
# Description: 
################################################################################

# 1. Reading data
data_lin <- read.csv2("data/raw/data_linealidad.csv")
data_sens <- read.csv2("data/raw/data_sensibilidad.csv")

# 2. Coarcing variables
data_lin$sample <- as.factor(data_lin$sample)

data_sens$sample <- as.factor(data_sens$sample)

# 3. Saving as.Rda object
save(data_lin, data_sens, file = "data/processed/data")
rm(data_lin, data_sens)

# 4. Loading data
load("data/processed/data")

# 4. Calibration curve

## 4.a. Mean and SD from replicates

### reps_summ returns a dataframe with mean and SD for each sample
reps_summ <- function(df){
  means <- aggregate(area ~ cc, data=df, FUN=mean)
  sds <- aggregate(area ~ cc, data=df, FUN=sd)
  return(data.frame("cc" = means$cc, 
                    "mean.area" = means$area,
                    "sd.area" = sds$area))
}

### Applying reps_summ to both datasets (data_lin and data_sens).
### Results are saved in a list. Each element of the list corresponds to one of
### the datasets
results <- lapply(list("linealidad" = data_lin,
                 "sensibilidad" = data_sens), 
            reps_summ)

### Saving results in /data/processed
save(results, file="data/processed/mean_sd")

## 4.b. Plot 
#install.packages("ggplot2")
library("ggplot2")

p_cc.vs.area <- ggplot(data = results$linealidad, 
                       aes(x= cc, 
                           y = mean.area)) +
                geom_point(size = 3, colour = "blue") +
                theme_light() + 
                labs(title= "Concentración vs Area",
                     x = "Concentración (mg/mL)",
                     y = "UA") +
                geom_errorbar(aes(ymin=mean.area-sd.area, 
                                  ymax=mean.area+sd.area), 
                              width = 0.003,
                              colour = "red")

## 4.c. Linear Regression 
### Fitting a regression line:
mod <- lm(mean.area ~ cc, data = results$linealidad)
### Printing results
summary(mod) #y = 94.58 + 149836.94, R

## 4.d. Residuals
### Plot Fitted vs Residuals
p_resid.vs.fitted <- ggplot(data = NULL, aes(x = mod$fitted.values,
                                y = mod$residuals)) +
                     geom_point(size = 2, colour = "red") +
                     theme_light() +
                     labs (title = "Residuos vs Valores Ajustados",
                           x = "Valores ajustados",
                           y = "Residuos")

# All plots residuals analysis
plot(mod)

## 4.e. Mandel's Test (Linearity test)
### Cuadratic model (non linear model)
mod2 <- lm(mean.area ~ cc + I(cc^2), data = results$linealidad)
summary(mod2)
### ANOVA linear model vs cuadratic model
anova(mod,mod2) # pval> 0.05 so mod2 is not better fit

# 5. Sensivity Analysis
# Linear model
mod_sens <- lm(mean.area ~ cc, data = results$sensibilidad)
summary(mod_sens)

p_sens_cc.vs.area <- ggplot(data = results$sensibilidad, 
                       aes(x= cc, 
                           y = mean.area)) +
  geom_point(size = 3, colour = "blue") +
  theme_light() + 
  labs(title= "Concentración vs Area",
       x = "Concentración (mg/mL)",
       y = "UA") +
  geom_errorbar(aes(ymin=mean.area-sd.area, 
                    ymax=mean.area+sd.area), 
                width = 0.003,
                colour = "red")

# Residual analysis
plot(mod_sens)

# LOD and LOQ estimation
summary(mod_sens)
sigma1 <- sigma(mod_sens)
S1<- coef(mod_sens)[2]

LOD1 <- (3.3 * sigma1) / S1
LOQ1 <- (10 * sigma1) / S1

# Eliminación punto 6 (cc = 0.250 mg/mL)
mod_sens2 <- lm(mean.area ~ cc, data = results$sensibilidad[-6, ])
summary(mod_sens2)
plot(mod_sens2)

# LOD and LOQ estimation (2)
sigma2 <- sigma(mod_sens2)
S2<- coef(mod_sens2)[2]

LOD2 <- (3.3 * sigma2) / S2
LOQ2 <- (10 * sigma2) / S2

# Eliminación puntos 6, 7, 8
mod_sens3 <- lm(mean.area ~ cc, data = results$sensibilidad[-c(6,7,8), ])
summary(mod_sens3)
plot(mod_sens3)

# LOD and LOQ estimation (3)
sigma3 <- sigma(mod_sens3)
S3<- coef(mod_sens3)[2]

LOD3 <- (3.3 * sigma3) / S3
LOQ3 <- (10 * sigma3) / S3
