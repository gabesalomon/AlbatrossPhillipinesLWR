# readme ------------------------------------------------------------------

# Created by: Gabriel Salomon
# Last Updated by: John Whalen
# Last Updated: 6/5/24

#### set working directory ------------------------------------------------

# set working directory. working directory is set to the directory that has this file. for John
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries ( Load only libraries if not first time) ----
install.packages("pacman")
install.packages("rlang")
install.packages("ggpubr")
install.packages("rfishbase")
install.packages("readxl")
install.packages("ggplot2")
install.packages("nls2")
install.packages("patchwork")
install.packages("dplyr")
library(pacman)
library(rlang)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)
library(patchwork)
library(dplyr)

# Import S. delicatulus Dataset ----

# for Gabriel
Spratelloides_delicatulus <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/Albatross_LWR_data.xlsx",8)

# for John. Uses the relative path from the Commands/Albatross/ directory where this R file is located. 
Spratelloides_delicatulus <- read_excel("../../Data/Albatross_LWR_data/Albatross_LWR_data.xlsx", 8)

View(Spratelloides_delicatulus)
summary(Spratelloides_delicatulus)
y <- c(Spratelloides_delicatulus$Mass_g)
xS <- c(Spratelloides_delicatulus$SL_cm)
xT <- c(Spratelloides_delicatulus$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=.5, bfit=.5))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2) #error: plot.new has not been called yet
a <- 0.007912
b <- 3.154975
summary(nls1)

# Plot S. delicatulus ----
ggplot(Spratelloides_delicatulus, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.007912*(x^(3.154975))), se = TRUE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus")+
  xlab("SL (cm)")+
  ylab("Mass (g)")

# Fishbase comparison ----
length_weight("Spratelloides delicatulus")
fb_a <- c(length_weight("Spratelloides delicatulus"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

log_a <- log10(fb_a$a)
data.frame(fb_a$b, log_a)
comp2 <- data.frame(fb_a$b, log_a)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a))+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of S. delicatulus")+
  xlab("b")+
  ylab("log10a")
  
# Fishbase Genus comparison ---
log10ab <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/loga_b_genus_comparison.xlsx",1)
log10ab_after <- na.omit(log10ab)
View(log10ab_after)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot(data = log10ab_after, aes(x=log10ab_after$b, y=log10ab_after$log10a, color=Species))+
  geom_point()+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(log10ab_after, method = lm, mapping=aes(x=log10ab_after$b, y=log10ab_after$log10a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_color_discrete(log10ab_after$Species)+
  ggtitle("Length-Weight log10a vs b Comparison of the genus Spratelloides")+
  xlab("b")+
  ylab("log10a")

# Annotated Graph ---- 
ggplot(Spratelloides_delicatulus, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.007912*(x^(3.154975))), se = TRUE)+
  annotate("text" , label="y ~ 0.0079x^(3.15)  R2 ~ 0.989", x=5, y=0.25)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus")+
  xlab("SL (cm)")+
  ylab("Mass (g)")

# Relative condition factor 
exp_weight <- ((a)*((xS)^(b)))
Kn <- (y)/(exp_weight)
rcf <- data.frame(xS, Kn)
avg_Kn <- mean(Kn)
avg_Kn
lmrcf <- lm(formula = Kn ~ xS, data = rcf)
lmrcf
summary(lmrcf)
rKn <- (exp_weight)/((a)*(xS))
cf <- ((100)*((y)/(xS)^(3)))
avg_cf <- mean(cf)
avg_cf
summary(Kn)

ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. delicatulus")+
  xlab("SL (cm)")+
  ylab("Kn")

#Linear regression formula
logw <- log10(y)
logl <- log10(xS)

logxS_y <- data.frame(logl, logw)
logxS_y
lwr_rg <- lm(logw ~ logl, data = logxS_y)
lwr_rg
coefrg <- coef(lwr_rg)
intercept <- coefrg[1]
slope <- coefrg[2]
formula_rg <- paste("y =", round(intercept, 2), "+", round(slope, 2), "* x")
summary(lwr_rg)
formula_rg

ggplot(logxS_y, aes(x=logl, y=logw))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm, se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Linear Regression model of S. delicatulus")+
  xlab("log10 SL (cm)")+
  ylab("log10 Mass (g)")

# Z score (Outliers) ---- 
avgxS <- mean(xS)
sdxS <- sd(xS)
zxS <- ((xS)-(avgxS)/(sdxS))
avgz <- mean(zxS)
avgz
xSscore <- data.frame(xS, zxS)
xSscore[xSscore$zxS <=3, ]
xSscore
xSnooutlier <- xSscore[xSscore$zxS <=3, ]
xSnooutlier

# Fishbase comparison genus----
library(rfishbase)
genus_name <- "Spratelloides"
spratelloides_species <- species(genus = genus_name)
length_weight_data <- spratelloides_species[, c("Species", "LWLength", "LWWeight")]
summary(length_weight_data)

length_weight("Spratelloides")
fb_a <- c(length_weight("Spratelloides"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

log_a <- log10(fb_a$a)
data.frame(fb_a$b, log_a)
comp2 <- data.frame(fb_a$b, log_a)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a))+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of S. delicatulus")+
  xlab("b")+
  ylab("log10a")

# Site comparison ----

print(unique(Spratelloides_delicatulus$Locality))

# Calculate summary statistics for SL_mm & Mass_g by Locality
summary_stats <- Spratelloides_delicatulus %>%
  group_by(Locality) %>%
  summarise(
    mean_SL_mm = mean(SL_mm, na.rm = TRUE),
    se_SL_mm = sd(SL_mm, na.rm = TRUE) / sqrt(n()),
    mean_Mass_g = mean(Mass_g, na.rm = TRUE), 
    se_Mass_g = sd(Mass_g, na.rm = TRUE) / sqrt(n())
  )
print(summary_stats)

# Merge summary statistics with the original data
Spratelloides_delicatulus <- Spratelloides_delicatulus %>%
  left_join(summary_stats, by = "Locality")

# Boxplot for SL_mm with jittered points and standard error bars
ggplot(Spratelloides_delicatulus, aes(x = Locality, y = SL_mm)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_errorbar(
    aes(ymin = mean_SL_mm - se_SL_mm, ymax = mean_SL_mm + se_SL_mm), 
    width = 0.2, 
    color = "blue"
  ) +
  labs(title = "Standard Length by Locality with Jittered Points and SE Bars", 
       x = "Locality", y = "Standard Length (mm)") +
  theme_minimal()

# Boxplot for Mass_g with jittered points and standard error bars

ggplot(Spratelloides_delicatulus, aes(x = Locality, y = Mass_g)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_errorbar(
    aes(ymin = mean_Mass_g - se_Mass_g, ymax = mean_Mass_g + se_Mass_g), 
    width = 0.2, 
    color = "blue"
  ) +
  labs(title = "Mass by Locality with Jittered Points and SE Bars", 
       x = "Locality", y = "Mass (g)") +
  theme_minimal()
