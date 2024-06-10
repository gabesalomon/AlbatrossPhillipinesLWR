#### readme ------------------------------------------------------------------

# Created by: Gabriel Salomon
# Last Updated by: John Whalen
# Last Updated: 6/10/24

#### INITIALIZE ####

# for John
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### PACKAGES ####
packages_used <- 
  c("pacman",
    "rlang",
    "ggpubr",
    "rfishbase",
    "readxl",
    "ggplot2",
    "nls2",
    "patchwork",
    "dunn.test",
    "ggsignif",
    "maps",
    "dplyr",
    "car")

packages_to_install <- 
  packages_used[!packages_used %in% installed.packages()[,1]]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, 
                   Ncpus = Sys.getenv("NUMBER_OF_PROCESSORS") - 1)
}

lapply(packages_used, 
       require, 
       character.only = TRUE)

# for John. Uses the relative path from the Commands/Albatross/ directory where this R file is located. 
Gerres_oyena <- read_excel("../../Data/Albatross_LWR_data/Albatross_LWR_data.xlsx", 2)

# for Gabe. G. oyena Dataset ----
Gerres_oyena <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/Albatross_LWR_Data.xlsx",2)

View(Gerres_oyena)
summary(Gerres_oyena)
y <- c(Gerres_oyena$Mass_g)
xS <- c(Gerres_oyena$SL_cm)
xT <- c(Gerres_oyena$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

#### add coordinates to sites in the dataframe Gerres oyena ----

#unique Localities
print(unique(Gerres_oyena$Locality))

# Create a data frame with Locality, Latitude, and Longitude
locality_coords <- data.frame(
  Locality = c("Port_San_Vicente_Luzon", "Malcochin_Harbor_Linapacan", "Gulf_of_Davao_Mindinao"),
  lat = c(18.5154, 11.49066, 7.07919),
  lon = c(122.13897, 119.82816, 125.63367)
)

# Locality Coordinates
# Port_San_Vicente_Luzon:18.5154, 122.13897
# Malcochin_Harbor_Linapacan:11.49066, 119.82816
# Gulf_of_Davao_Mindinao: 7.07919, 125.63367

# Print the locality_coords data frame
print(locality_coords)

# Merge the locality_coords data frame with Gerres_oyena data frame
Gerres_oyena <- merge(Gerres_oyena, locality_coords, by = "Locality", all.x = TRUE)

# Print the updated Gerres_oyena data frame
head(Gerres_oyena)


# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.01193
b <- 3.27441
summary(nls1)

# Plot G. oyena ----
ggplot(Gerres_oyena, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.01193*(x^(3.27441))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of G. oyena")+
  xlab("SL (cm)")+
  ylab("Mass (g)")

# Fishbase comparison ----
length_weight("Gerres oyena")
fb_a <- c(length_weight("Gerres oyena"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

log_a <- log10(fb_a$a)
color <- c("black","black","green","green")
data.frame(fb_a$b, log_a, color)
comp2 <- data.frame(fb_a$b, log_a, color)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a), color=color)+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of G. oyena")+
  xlab("b")+
  ylab("log10a")
  
# for Gabe Fishbase Genus comparison ---
log10ab <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/loga_b_genus_comparison.xlsx",2)

# for John
log10ab <- read_excel("../../Data/Albatross_LWR_data/loga_b_genus_comparison.xlsx",2)

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
  ggtitle("Length-Weight log10a vs b Comparison of the genus Gerres")+
  xlab("b")+
  ylab("log10a")

# Annotated Graph ---- 
ggplot(Gerres_oyena, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.01193*(x^(3.27441))), se = TRUE)+
  annotate("text" , label="y ~ 0.01193x^(3.27441)  RSE ~ 1.128", x=12, y=16)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of G. oyena")+
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

#### adding relative condition factor data to new columns in the Gerres_oyena dataframe ####
# Step 1: Create the 'exp_weight' column. This is based on the length-weight relationship equation: W = aL^b
Gerres_oyena$exp_weight <- a * (Gerres_oyena$SL_cm ^ b)

# Step 2: Create the 'Kn' column. Le Cren's Relative Condition Factor equation: Kn = W/aL^b
Gerres_oyena$Kn <- Gerres_oyena$Mass_g / Gerres_oyena$exp_weight

# Step 3: Create the 'rKn' column.Le Cren's Relative Condition Factor, but using the expected instead of the observed weight.
Gerres_oyena$rKn <- Gerres_oyena$exp_weight / (a * Gerres_oyena$SL_cm)

# Step 4: Create the 'cf' column. Fulton's Condition Factor equation: cf = 100(W/SL^3)
Gerres_oyena$cf <- 100 * (Gerres_oyena$Mass_g / (Gerres_oyena$SL_cm ^ 3))

# Relative Condition Factor (Kn) of G. oyena. 
ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of G. oyena")+
  xlab("SL (cm)")+
  ylab("Kn")

# Relative Condition Factor (Kn) of G. oyena with points colored by locality.
ggplot(Gerres_oyena, aes(x=SL_cm, y=Kn))+
  geom_point(aes(color = Locality))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of G. oyena")+
  xlab("SL (cm)")+
  ylab("Kn")

# Le Cren's Relative Condition Factor (Kn) of G. oyena by locality.
ggplot(Gerres_oyena, aes(x=SL_cm, y=Kn, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Le Cren's Relative Condition Factor (Kn) of G. oyena by Locality")+
  xlab("SL (cm)")+
  ylab("Kn")

# Fulton's Condition Factor (cf) of G. oyena by locality.
ggplot(Gerres_oyena, aes(x=SL_cm, y=cf, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Fulton's Condition Factor (cf) of G. oyena by Locality")+
  xlab("SL (cm)")+
  ylab("cf")


ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 1.030282", x=11, y=1.14)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of G. oyena")+
  xlab("SL (cm)")+
  ylab("Kn")

#Linear regression formula
logw <- log10(y)
logl <- log10(xS)

# adding linear regression data to new columns in the Gerres_oyena dataframe
# Calculate log-transformed values for Mass_g and SL_cm
Gerres_oyena$log_Mass_g <- log10(Gerres_oyena$Mass_g)
Gerres_oyena$log_SL_cm <- log10(Gerres_oyena$SL_cm)


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
  ggtitle("Linear Regression model of G. oyena")+
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


#### Site comparison ----

print(unique(Gerres_oyena$Locality))

# Calculate summary statistics for SL_cm & Mass_g by Locality
summary_stats <- Gerres_oyena %>%
  group_by(Locality) %>%
  summarise(
    mean_SL_cm = mean(SL_cm, na.rm = TRUE),
    se_SL_cm = sd(SL_cm, na.rm = TRUE) / sqrt(n()),
    mean_Mass_g = mean(Mass_g, na.rm = TRUE), 
    se_Mass_g = sd(Mass_g, na.rm = TRUE) / sqrt(n())
  )
print(summary_stats)

# Merge summary statistics with the original data
Gerres_oyena <- Gerres_oyena %>%
  left_join(summary_stats, by = "Locality")

# Boxplot for SL_cm with jittered points and standard error bars
ggplot(Gerres_oyena, aes(x = Locality, y = SL_cm)) +
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

ggplot(Gerres_oyena, aes(x = Locality, y = Mass_g)) +
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


#### Statistical Tests for significant differences of cf by Locality ----

# Boxplot for cf to visualize data
ggplot(Gerres_oyena, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Gerres_oyena$cf)
# p-value = 0.7944 (> 0.05), so the data do not violate the assumption of a normal distribution

# Check homogeneity of variances using Levene's Test
leveneTest(cf ~ Locality, data = Gerres_oyena)
# p-value = 0.1.421 (> 0.05), so the data do not violate the assumption of homogeneity
# Use parametric test. Use ANOVA. 

# ANOVA
cf_anova_result <- aov(cf ~ Locality, data = Gerres_oyena)
cf_anova_summary <- summary(cf_anova_result)
print(cf_anova_summary)
#p-value = 0.613 (> 0.05), so there is not a significant difference in cf among localities.

# Create the base boxplot
cf_plot <- ggplot(Gerres_oyena, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(cf_plot)


#### Statistical Tests for significant differences of Kn by Locality ----

# Boxplot for Kn to visualize the data
ggplot(Gerres_oyena, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Gerres_oyena$Kn)
# p-value = 0.274 (> 0.05), so the data do not violate the assumption of a normal distribution

# Check homogeneity of variances using Levene's Test
leveneTest(Kn ~ Locality, data = Gerres_oyena)
# p-value = 0.1327 (>0.05), so assumption of homogeneity of data is not violated

# Use parametric test. Use ANOVA. 

# ANOVA
Kn_anova_result <- aov(Kn ~ Locality, data = Gerres_oyena)
Kn_anova_summary <- summary(Kn_anova_result)
print(Kn_anova_summary)
# p = 0.123. There is not a significant difference in Kn between Localities

# Create the base boxplot
Kn_plot <- ggplot(Gerres_oyena, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(Kn_plot)


#### Map of sampling Locations -----

# Get the map data for the Philippines
philippines_map <- map_data("world2", region = "Philippines")

# create the map
ggplot() +
  geom_polygon(data = philippines_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = Gerres_oyena, aes(x = lon, y = lat, color = Locality), size = 3) +
  scale_color_manual(values = c("Gulf_of_Davao_Mindinao" = "#F8766D", "Malcochin_Harbor_Linapacan" = "#00BA38", "Port_San_Vicente_Luzon" = "#619CFF")) +
  labs(title = "Sampling Locations of Gerres oyena", x = "Longitude", y = "Latitude", color = "Locality") +
  scale_x_continuous(breaks = seq(116, 128, by = 2)) +
  coord_fixed(ratio = 1.3, xlim = c(116, 128), ylim = c(4, 20)) +
  theme_minimal()



