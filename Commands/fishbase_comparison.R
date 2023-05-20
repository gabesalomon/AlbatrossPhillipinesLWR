# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)

# S. fuscescens Dataset ----
Calculated_variables <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Calculated_variables.xlsx")
View(Calculated_variables)
summary(Calculated_variables)
y <- c(Calculated_variables$Mass_g)
xS <- c(Calculated_variables$SL_cm)
xT <- c(Calculated_variables$TL_cm)

# Fishbase comparison ----
length_weight("Siganus fuscescens")
fb_a <- c(length_weight("Siganus fuscescens"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

ggplot(Siganus_fuscescens, aes(x=SL_cm, y=Mass_g))+