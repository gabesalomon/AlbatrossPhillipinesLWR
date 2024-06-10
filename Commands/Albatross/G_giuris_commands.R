#### readme ------------------------------------------------------------------

# Created by: Gabriel Salomon
# Last Updated by: John Whalen
# Last Updated: 6/8/24

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
Glossogobius_giuris <- read_excel("../../Data/Albatross_LWR_data/Albatross_LWR_data.xlsx", 3)

# for Gabe. G. giuris Dataset ----
Glossogobius_giuris <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/Albatross_LWR_data.xlsx",3)

View(Glossogobius_giuris)
summary(Glossogobius_giuris)
y <- c(Glossogobius_giuris$Mass_g)
xS <- c(Glossogobius_giuris$SL_cm)
xT <- c(Glossogobius_giuris$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# add coordinates to sites in the dataframe Glossogobius_giuris ----
# Create a data frame with Locality, Latitude, and Longitude
locality_coords <- data.frame(
  Locality = c("Naujan_Mindoro", "Port_Caltom_Busuanga", "Nakoda_Bay_Palawan"),
  lat = c(13.335, 12.18582, 9.29111),
  lon = c(121.31333, 120.10223, 117.95805)
)

# Locality Coordinates
# Naujan_Mindoro: 13.335, 121.31333
# Port_Caltom_Busuanga: 12.18582, 120.10223
# Nakoda_Bay_Palawan: 9.29111, 117.95805

# Print the locality_coords data frame
print(locality_coords)

# Merge the locality_coords data frame with Glossogobius_giuris data frame
Glossogobius_giuris <- merge(Glossogobius_giuris, locality_coords, by = "Locality", all.x = TRUE)

# Print the updated Glossogobius_giuris data frame
head(Glossogobius_giuris)



# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.0083756
b <- 3.2282862
summary(nls1)

# Plot G. giuris ----
ggplot(Glossogobius_giuris, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0083756*(x^(3.2282862))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of G. giuris")+
  xlab("SL (cm)")+
  ylab("Mass (g)")

# Fishbase comparison ----
length_weight("Glossogobius giuris")
fb_a <- c(length_weight("Glossogobius giuris"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

log_a <- log10(fb_a$a)
color <- c("black","black","black","green","black","black","green","green","black","black","black","black","black","black","black","black","black","black","black","black")
data.frame(fb_a$b, log_a, color)
comp2 <- data.frame(fb_a$b, log_a, color)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a), color=color)+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of G. giuris")+
  xlab("b")+
  ylab("log10a")
  
  
# Annotated Graph ---- 
ggplot(Glossogobius_giuris, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0083756*(x^(3.2282862))), se = TRUE)+
  annotate("text" , label="y ~ 0.0083756x^(3.2282862)  RSE ~ 0.1289", x=6, y=1)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of G. giuris")+
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

# Relative condition factor (Kn)
ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 1.025615", x=6, y=1.25)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of G. giuris")+
  xlab("SL (cm)")+
  ylab("Kn")

# adding relative condition factor data to new columns in the Glossogobius_giuris dataframe
# Step 1: Create the 'exp_weight' column. This is based on the length-weight relationship equation: W = aL^b
Glossogobius_giuris$exp_weight <- a * (Glossogobius_giuris$SL_cm ^ b)

# Step 2: Create the 'Kn' column. Le Cren's Relative Condition Factor equation: Kn = W/aL^b
Glossogobius_giuris$Kn <- Glossogobius_giuris$Mass_g / Glossogobius_giuris$exp_weight

# Step 3: Create the 'rKn' column.Le Cren's Relative Condition Factor, but using the expected instead of the observed weight.
Glossogobius_giuris$rKn <- Glossogobius_giuris$exp_weight / (a * Glossogobius_giuris$SL_cm)

# Step 4: Create the 'cf' column. Fulton's Condition Factor equation: cf = 100(W/SL^3)
Glossogobius_giuris$cf <- 100 * (Glossogobius_giuris$Mass_g / (Glossogobius_giuris$SL_cm ^ 3))

# Relative Condition Factor (Kn) of G. giuris. 
ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of G. giuris")+
  xlab("SL (cm)")+
  ylab("Kn")

# Relative Condition Factor (Kn) of G. giuris with points colored by locality.
ggplot(Glossogobius_giuris, aes(x=SL_cm, y=Kn))+
  geom_point(aes(color = Locality))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of G. giuris")+
  xlab("SL (cm)")+
  ylab("Kn")

# Le Cren's Relative Condition Factor (Kn) of G. giuris by locality.
ggplot(Glossogobius_giuris, aes(x=SL_cm, y=Kn, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Le Cren's Relative Condition Factor (Kn) of G. giuris by Locality")+
  xlab("SL (cm)")+
  ylab("Kn")

# Fulton's Condition Factor (cf) of G. giuris by locality.
ggplot(Glossogobius_giuris, aes(x=SL_cm, y=cf, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Fulton's Condition Factor (cf) of G. giuris by Locality")+
  xlab("SL (cm)")+
  ylab("cf")


ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9637977", x=6.75, y=.78)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of G. giuris")+
  xlab("SL (cm)")+
  ylab("Kn")


#Linear regression formula
logw <- log10(y)
logl <- log10(xS)

# adding linear regression data to new columns in the Glossogobius_giuris dataframe
# Calculate log-transformed values for Mass_g and SL_cm
Glossogobius_giuris$log_Mass_g <- log10(Glossogobius_giuris$Mass_g)
Glossogobius_giuris$log_SL_cm <- log10(Glossogobius_giuris$SL_cm)


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
  ggtitle("Linear Regression model of G. giuris")+
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


#### Site comparison ----

print(unique(Glossogobius_giuris$Locality))

# Calculate summary statistics for SL_cm & Mass_g by Locality
summary_stats <- Glossogobius_giuris %>%
  group_by(Locality) %>%
  summarise(
    mean_SL_cm = mean(SL_cm, na.rm = TRUE),
    se_SL_cm = sd(SL_cm, na.rm = TRUE) / sqrt(n()),
    mean_Mass_g = mean(Mass_g, na.rm = TRUE), 
    se_Mass_g = sd(Mass_g, na.rm = TRUE) / sqrt(n())
  )
print(summary_stats)

# Merge summary statistics with the original data
Glossogobius_giuris <- Glossogobius_giuris %>%
  left_join(summary_stats, by = "Locality")

# Boxplot for SL_cm with jittered points and standard error bars
ggplot(Glossogobius_giuris, aes(x = Locality, y = SL_cm)) +
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

ggplot(Glossogobius_giuris, aes(x = Locality, y = Mass_g)) +
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
ggplot(Glossogobius_giuris, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Glossogobius_giuris$cf)
# p-value = 0.3138 (> 0.05), so the data do not violate the assumption of a normal distribution

# Check homogeneity of variances using Levene's Test
leveneTest(cf ~ Locality, data = Glossogobius_giuris)
# p-value = 0.1099 (> 0.05), so the data do not violate the assumption of homogeneity
# Use parametric test. Use ANOVA. 

# ANOVA
cf_anova_result <- aov(cf ~ Locality, data = Glossogobius_giuris)
cf_anova_summary <- summary(cf_anova_result)
print(cf_anova_summary)
#p-value = 1.82e-11 (< 0.05), so there is a significant difference in cf among localities. Run Tukey's HSD post-hoc test.


# If significant, perform Tukey's HSD post-hoc test
if (cf_anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  cf_tukey_result <- TukeyHSD(cf_anova_result)
  print(cf_tukey_result)
}
# Tukey HSD Results
# $Locality
# diff         lwr         upr     p adj
# Naujan_Mindoro-Nakoda_Bay_Palawan        0.11938340  0.03968191  0.19908488 0.0016590 (significant)
# Port_Caltom_Busuanga-Nakoda_Bay_Palawan -0.07277948 -0.15746704  0.01190807 0.1067517
# Port_Caltom_Busuanga-Naujan_Mindoro     -0.19216288 -0.24942212 -0.13490364 0.0000000 (significant)


# Create the base boxplot
cf_plot_sig <- ggplot(Glossogobius_giuris, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(cf_plot_sig)


#### Statistical Tests for significant differences of Kn by Locality ----

# Boxplot for Kn to visualize the data
ggplot(Glossogobius_giuris, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Glossogobius_giuris$Kn)
# p-value = 0.002065 (< 0.05), so the data are not normally distributed
# use non-parametric test. use Kruskal-Wallis Test

# Check homogeneity of variances using Levene's Test
leveneTest(Kn ~ Locality, data = Glossogobius_giuris)
# p-value = 0.05302 (>0.05), so assumption of homogeneity of data is not violated

# Perform the Kruskal-Wallis test for Le Cren's Relative Condition Factor (Kn)
kruskal_Kn <- kruskal.test(Kn ~ Locality, data = Glossogobius_giuris)
print(kruskal_Kn)
# p-value = 4.154e-06. This indicates that there is a significant difference in Kn across Localities. This suggests that at least one Locality has a different median Kn compared to the others.

# Check if the Kruskal-Wallis test is significant
if (kruskal_Kn$p.value < 0.05) {
  # If significant, perform Dunn's test for post-hoc analysis. This will identify which specific pairs of localities differ. 
  dunn_Kn <- dunn.test(Glossogobius_giuris$Kn, Glossogobius_giuris$Locality, method="bonferroni")
  print(dunn_Kn)
}

# Kruskal_Wallis, bonferroni results
# Comparison of x by group                            
# (Bonferroni)                                  
# Col Mean-|
#   Row Mean |   Nakoda_B   Naujan_M
# ---------+----------------------
#   Naujan_M |  -3.304785
#            |    0.0014*
#            |
#   Port_Cal |  -0.084059   4.475741
#            |     1.0000    0.0000*


# Create the base boxplot
Kn_plot_sig <- ggplot(Glossogobius_giuris, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(Kn_plot_sig)


#### Map of sampling Locations -----

# Get the map data for the Philippines
philippines_map <- map_data("world2", region = "Philippines")

# create the map
ggplot() +
  geom_polygon(data = philippines_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = Glossogobius_giuris, aes(x = lon, y = lat, color = Locality), size = 3) +
  scale_color_manual(values = c("Nakoda_Bay_Palawan" = "#F8766D", "Naujan_Mindoro" = "#00BA38", "Port_Caltom_Busuanga" = "#619CFF")) +
  labs(title = "Sampling Locations of Glossogobius giuris", x = "Longitude", y = "Latitude", color = "Locality") +
  scale_x_continuous(breaks = seq(116, 128, by = 2)) +
  coord_fixed(ratio = 1.3, xlim = c(116, 128), ylim = c(4, 20)) +
  theme_minimal()
