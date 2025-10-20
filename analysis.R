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
data_lin$sample <- as.factor(data_lin$cc)

data_sens$sample <- as.factor(data_sens$sample)
data_sens$sample <- as.factor(data_sens$cc)

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
                 "sensibililidad" = data_sens), 
            reps_summ)

### Saving results in /data/processed
save(results, file="data/processed/mean_sd")

## 4.b. Plot 
install.packages("ggplot2")
library("ggplot2")

p_cc.vs.area <- ggplot(data = results$linealidad, 
                       aes(x= cc, 
                           y = mean.area)) +
                geom_point(size = 2, colour = "blue") +
                theme_light() + 
                labs(title= "Concentración vs Area",
                     x = "Concentración (mg/mL)",
                     y = "UA")

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

