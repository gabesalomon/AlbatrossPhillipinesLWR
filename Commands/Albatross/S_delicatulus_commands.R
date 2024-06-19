#### readme ####

# Created by: Gabriel Salomon
# Last Updated by: John Whalen
# Last Updated: 6/18/24

#### INITIALIZE ####

# for John. set working directory. working directory is set to the directory that has this file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries ( Load only libraries if not first time) 
install.packages("pacman")
install.packages("rlang")
install.packages("ggpubr")
install.packages("rfishbase")
install.packages("readxl")
install.packages("ggplot2")
install.packages("nls2")
install.packages("patchwork")
install.packages("dplyr")
install.packages("car")
install.packages("dunn.test")
install.packages("ggsignif")
install.packages("maps")

library(pacman)
library(rlang)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)
library(patchwork)
library(dplyr)
library(car)
library(dunn.test)
library(ggsignif)
library(maps)

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

# add coordinates to sites in the dataframe Spratelloides_delicatulus ----
# Create a data frame with Locality, Latitude, and Longitude
locality_coords <- data.frame(
  Locality = c("Cagayan_de_Jolo", "Jamelo_Cove_Luzon", "Mansalay_Mindoro", "Sacol_Island_Zamboanga"),
  lat = c(6.96233, 14.18105, 12.5161, 6.94555),
  lon = c(118.47916, 120.60461, 121.44065, 122.2489)
)

# Print the locality_coords data frame
print(locality_coords)

# Merge the locality_coords data frame with Spratelloides_delicatulus data frame
Spratelloides_delicatulus <- merge(Spratelloides_delicatulus, locality_coords, by = "Locality", all.x = TRUE)

# Print the updated Spratelloides_delicatulus data frame
head(Spratelloides_delicatulus)

# Desired eq.: Mass_g = a*SL_cm^b (SL). Calculating constants a and b from sampled data? ----
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

# for John. Uses relative path from setwd which sets my home directory to the Commands/Albatross/ directory. 
log10ab <- read_excel("../../Data/Albatross_LWR_data/loga_b_genus_comparison.xlsx",1)
# for Gabe
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
Kn <- (y)/(exp_weight) # Le Cren's Relative Condition Factor
rcf <- data.frame(xS, Kn)
avg_Kn <- mean(Kn)
avg_Kn
lmrcf <- lm(formula = Kn ~ xS, data = rcf)
lmrcf
summary(lmrcf)
rKn <- (exp_weight)/((a)*(xS)) 
cf <- ((100)*((y)/(xS)^(3))) # Fulton's Condition factor
avg_cf <- mean(cf)
avg_cf
summary(Kn)

# adding relative condition factor data to new columns in the Spratelloides_delicatulus dataframe
# Step 1: Create the 'exp_weight' column. This is based on the length-weight relationship equation: W = aL^b
Spratelloides_delicatulus$exp_weight <- a * (Spratelloides_delicatulus$SL_cm ^ b)

# Step 2: Create the 'Kn' column. Le Cren's Relative Condition Factor equation: Kn = W/aL^b
Spratelloides_delicatulus$Kn <- Spratelloides_delicatulus$Mass_g / Spratelloides_delicatulus$exp_weight

# Step 3: Create the 'rKn' column.Le Cren's Relative Condition Factor, but using the expected instead of the observed weight.
Spratelloides_delicatulus$rKn <- Spratelloides_delicatulus$exp_weight / (a * Spratelloides_delicatulus$SL_cm)

# Step 4: Create the 'cf' column. Fulton's Condition Factor equation: cf = 100(W/SL^3)
Spratelloides_delicatulus$cf <- 100 * (Spratelloides_delicatulus$Mass_g / (Spratelloides_delicatulus$SL_cm ^ 3))

# Relative Condition Factor (Kn) of S. delicatulus. 
ggplot(Spratelloides_delicatulus, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. delicatulus")+
  xlab("SL (cm)")+
  ylab("Kn")

# Relative Condition Factor (Kn) of S. delicatulus with points colored by locality.
ggplot(Spratelloides_delicatulus, aes(x=SL_cm, y=Kn))+
  geom_point(aes(color = Locality))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. delicatulus")+
  xlab("SL (cm)")+
  ylab("Kn")

# Le Cren's Relative Condition Factor (Kn) of S. delicatulus by locality.
ggplot(Spratelloides_delicatulus, aes(x=SL_cm, y=Kn, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Le Cren's Relative Condition Factor (Kn) of S. delicatulus by Locality")+
  xlab("SL (cm)")+
  ylab("Kn")

# Fulton's Condition Factor (cf) of S. delicatulus by locality.
ggplot(Spratelloides_delicatulus, aes(x=SL_cm, y=cf, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Fulton's Condition Factor (cf) of S. delicatulus by Locality")+
  xlab("SL (cm)")+
  ylab("cf")


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

# adding linear regression data to new columns in the Spratelloides_delicatulus dataframe
# Calculate log-transformed values for Mass_g and SL_cm
Spratelloides_delicatulus$log_Mass_g <- log10(Spratelloides_delicatulus$Mass_g)
Spratelloides_delicatulus$log_SL_cm <- log10(Spratelloides_delicatulus$SL_cm)


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

# Calculate summary statistics for SL_cm & Mass_g by Locality
summary_stats <- Spratelloides_delicatulus %>%
  group_by(Locality) %>%
  summarise(
    mean_SL_cm = mean(SL_cm, na.rm = TRUE),
    se_SL_cm = sd(SL_cm, na.rm = TRUE) / sqrt(n()),
    mean_Mass_g = mean(Mass_g, na.rm = TRUE), 
    se_Mass_g = sd(Mass_g, na.rm = TRUE) / sqrt(n())
  )
print(summary_stats)

# Merge summary statistics with the original data
Spratelloides_delicatulus <- Spratelloides_delicatulus %>%
  left_join(summary_stats, by = "Locality")

# Boxplot for SL_cm with jittered points and standard error bars
ggplot(Spratelloides_delicatulus, aes(x = Locality, y = SL_cm)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_errorbar(
    aes(ymin = mean_SL_cm - se_SL_cm, ymax = mean_SL_cm + se_SL_cm), 
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


# Statistical Tests for significant differences of cf by Locality ----

# Boxplot for cf to visualize data
ggplot(Spratelloides_delicatulus, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Spratelloides_delicatulus$cf)
# p-value = 0.01767 (< 0.05), so the data are not normally distributed
# use non-parametric test. use Kruskal-Wallis Test

# Check homogeneity of variances using Levene's Test
leveneTest(cf ~ Locality, data = Spratelloides_delicatulus)
# p-value = 0.6788 (>0.05), so assumption of homogeneity of data is not violated

# Perform the Kruskal-Wallis test for Fulton's Condition Factor (cf)
kruskal_cf <- kruskal.test(cf ~ Locality, data = Spratelloides_delicatulus)
print(kruskal_cf)
# p-value < 2.2e-16. This indicates that there is a significant difference in cf across Localities. This suggests that at least one Locality has a different median cf compared to the others.

# Check if the Kruskal-Wallis test is significant
if (kruskal_cf$p.value < 0.05) {
  # If significant, perform Dunn's test for post-hoc analysis. This will identify which specific pairs of localities differ. 
  dunn_cf <- dunn.test(Spratelloides_delicatulus$cf, Spratelloides_delicatulus$Locality, method="bonferroni")
  print(dunn_cf)
}

# Create the base boxplot
cf_plot_sig <- ggplot(Spratelloides_delicatulus, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(cf_plot_sig)


# Statistical Tests for significant differences of Kn by Locality ----

# Boxplot for Kn to visualize the data
ggplot(Spratelloides_delicatulus, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Spratelloides_delicatulus$Kn)
# p-value = 0.01573 (< 0.05), so the data are not normally distributed
# use non-parametric test. use Kruskal-Wallis Test

# Check homogeneity of variances using Levene's Test
leveneTest(Kn ~ Locality, data = Spratelloides_delicatulus)
# p-value = 0.5911 (>0.05), so assumption of homogeneity of data is not violated

# Perform the Kruskal-Wallis test for Le Cren's Relative Condition Factor (Kn)
kruskal_Kn <- kruskal.test(Kn ~ Locality, data = Spratelloides_delicatulus)
print(kruskal_Kn)
# p-value < 2.2e-16. This indicates that there is a significant difference in Kn across Localities. This suggests that at least one Locality has a different median Kn compared to the others.

# Check if the Kruskal-Wallis test is significant
if (kruskal_Kn$p.value < 0.05) {
  # If significant, perform Dunn's test for post-hoc analysis. This will identify which specific pairs of localities differ. 
  dunn_Kn <- dunn.test(Spratelloides_delicatulus$Kn, Spratelloides_delicatulus$Locality, method="bonferroni")
  print(dunn_Kn)
}

# Create the base boxplot
Kn_plot_sig <- ggplot(Spratelloides_delicatulus, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(Kn_plot_sig)


# create a map of sampling Locations -----

# Get the map data for the Philippines
philippines_map <- map_data("world2", region = "Philippines")

# create the map
ggplot() +
  geom_polygon(data = philippines_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = Spratelloides_delicatulus, aes(x = lon, y = lat, color = Locality), size = 3) +
  scale_color_manual(values = c("Cagayan_de_Jolo" = "#F8766D", "Jamelo_Cove_Luzon" = "#00BA38", "Mansalay_Mindoro" = "#619CFF", "Sacol_Island_Zamboanga" = "#F564E3")) +
  labs(title = "Sampling Locations of Spratelloides delicatulus", x = "Longitude", y = "Latitude", color = "Locality") +
  scale_x_continuous(breaks = seq(116, 128, by = 2)) +
  coord_fixed(ratio = 1.3, xlim = c(116, 128), ylim = c(4, 20)) +
  theme_minimal()




