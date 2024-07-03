#### readme ####

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

#### IMPORT DATA ####

# for John. Uses the relative path from the Commands/Albatross/ directory where this R file is located. 
Siganus_fuscescens <- read_excel("../../Data/Albatross_LWR_data/Albatross_LWR_data.xlsx", 1)

# for Gabe. S. fuscescens Dataset ----
Siganus_fuscescens <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/Albatross_LWR_data.xlsx",1)

view(Siganus_fuscescens)

#### CALCULATIONS ####

summary(Siganus_fuscescens)
y <- c(Siganus_fuscescens$Mass_g)
xS <- c(Siganus_fuscescens$SL_cm)
xT <- c(Siganus_fuscescens$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

#### add coordinates to sites in the dataframe Siganus fuscescens ####

# unique Localities
print(unique(Siganus_fuscescens$Locality))

# Locality Coordinates
# Mindanao_River_Cotabato_Mindanao: 7.27337, 124.1982
# Pandanon_Island_between_Cebu_and_Bohol: 10.17966, 124.09
# Surigao_Mindanao: 9.80115, 125.48876
# Guijulugan_Negros: 10.14337, 123.28466
# Mactan_Island_Cebu: 10.17966, 124.09
# Cebu_Market: 10.17966, 124.09

# Combine sites Cebu Market, Mactan Island Cebu, and Pandanon Island. Use the Pandanon Island Coordinates because that has the majority of the sample size. 
# Modify the Locality column
Siganus_fuscescens <- Siganus_fuscescens %>%
  mutate(Locality = case_when(
    Locality %in% c("Pandanon_Island_between_Cebu_and_Bohol", "Mactan_Island_Cebu", "Cebu_Market") ~ "Cebu_Strait",
    TRUE ~ Locality
  ))

# Print the unique values in the Locality column to verify the changes
print(unique(Siganus_fuscescens$Locality))

# Create a data frame with Locality, Latitude, and Longitude.
locality_coords <- data.frame(
  Locality = c("Mindanao_River_Cotabato_Mindanao", "Cebu_Strait", "Surigao_Mindanao", "Guijulugan_Negros"),
  lat = c(7.27337, 10.17966, 9.80115, 10.14337),
  lon = c(124.1982, 124.09, 125.48876, 123.28466)
)

# Print the locality_coords data frame
print(locality_coords)

# Merge the locality_coords data frame with Siganus_fuscescens data frame
Siganus_fuscescens <- merge(Siganus_fuscescens, locality_coords, by = "Locality", all.x = TRUE)

# Print the updated Siganus_fuscescens data frame
head(Siganus_fuscescens)

#### Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2) #error: plot.new has not been called yet
a <- 0.018735
b <- 3.022117
summary(nls1)

# Plot S. fuscescens ----
ggplot(Siganus_fuscescens, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.018735*(x^(3.022117))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. fuscescens")+
  xlab("SL (cm)")+
  ylab("Mass (g)")

# Fishbase comparison ----
# for Gabe
logab <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/loga_b_fishbase_comparison.xlsx",1) #! this file is empty

# for John
logab <- read_excel("../../Data/Albatross_LWR_data/loga_b_fishbase_comparison.xlsx",1) #! this file is empty

logab_after <- na.omit(logab)
View(logab_after)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot(data = logab_after, aes(x=logab_after$b, y=logab_after$log10a, color=Locality))+
  geom_point()+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(logab_after, method = lm, mapping=aes(x=logab_after$b, y=logab_after$log10a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_color_discrete(logab_after$Locality)+
  guides(colour=guide_legend(title = "Locality"))+
  ggtitle("Length-Weight log10a vs b of Siganus fuscescens")+
  xlab("b")+
  ylab("log10a")

# Original Genus comparison ---
length_weight("Siganus fuscescens")
fb_a <- c(length_weight("Siganus fuscescens"))
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
  ggtitle("Length-Weight log10a vs b of S. fuscescens")+
  xlab("b")+
  ylab("log10a")


# Annotated Graph ---- 
ggplot(Siganus_fuscescens, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.018735*(x^(3.022117))), se = TRUE)+
  annotate("text" , label="y ~ 0.018735x^(3.022117)  RSE ~ 0.212", x=7.5, y=90)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. fuscescens")+
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


#### adding relative condition factor data to new columns in the Siganus_fuscescens dataframe ####
# Step 1: Create the 'exp_weight' column. This is based on the length-weight relationship equation: W = aL^b
Siganus_fuscescens$exp_weight <- a * (Siganus_fuscescens$SL_cm ^ b)

# Step 2: Create the 'Kn' column. Le Cren's Relative Condition Factor equation: Kn = W/aL^b
Siganus_fuscescens$Kn <- Siganus_fuscescens$Mass_g / Siganus_fuscescens$exp_weight

# Step 3: Create the 'rKn' column.Le Cren's Relative Condition Factor, but using the expected instead of the observed weight.
Siganus_fuscescens$rKn <- Siganus_fuscescens$exp_weight / (a * Siganus_fuscescens$SL_cm)

# Step 4: Create the 'cf' column. Fulton's Condition Factor equation: cf = 100(W/SL^3)
Siganus_fuscescens$cf <- 100 * (Siganus_fuscescens$Mass_g / (Siganus_fuscescens$SL_cm ^ 3))

#### Sample size for each Locality ####
locality_counts <- Siganus_fuscescens %>%
  count(Locality, name = "Sample_Size")

# Print the counts
print(locality_counts)

#### Remove Guijulugan_Negros because it only has 3 individuals. ####
# Filter the rows where Locality is not equal to 'Guijulugan_Negros'
Siganus_fuscescens <- Siganus_fuscescens %>%
  filter(Locality != "Guijulugan_Negros")

# Print the first few rows of the filtered dataframe to verify
print(head(Siganus_fuscescens))


#### Remove Outliers. Filter the rows where SL_cm is less than or equal to 10.
Siganus_fuscescens <- Siganus_fuscescens %>%
  filter(SL_cm <= 10)

# Print the first few rows of the filtered dataframe to verify
print(head(Siganus_fuscescens))

#### Sample size for each Locality after filtering ####
locality_counts <- Siganus_fuscescens %>%
  count(Locality, name = "Sample_Size")

# Print the counts
print(locality_counts)


# Relative Condition Factor (Kn) of S. fuscescens. 
ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. fuscescens")+
  xlab("SL (cm)")+
  ylab("Kn")

# Relative Condition Factor (Kn) of S. fuscescens with points colored by locality.
ggplot(Siganus_fuscescens, aes(x=SL_cm, y=Kn))+
  geom_point(aes(color = Locality))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. fuscescens")+
  xlab("SL (cm)")+
  ylab("Kn")

# Le Cren's Relative Condition Factor (Kn) of S. fuscescens by locality.
ggplot(Siganus_fuscescens, aes(x=SL_cm, y=Kn, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Le Cren's Relative Condition Factor (Kn) of S. fuscescens by Locality")+
  xlab("SL (cm)")+
  ylab("Kn")

# Fulton's Condition Factor (cf) of S. fuscescens by locality.
ggplot(Siganus_fuscescens, aes(x=SL_cm, y=cf, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Fulton's Condition Factor (cf) of S. fuscescens by Locality")+
  xlab("SL (cm)")+
  ylab("cf")

#! description needed
ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.8993142", x=12, y=.8)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. fuscescens")+
  xlab("SL (cm)")+
  ylab("Kn")

#Linear regression formula
logw <- log10(y)
logl <- log10(xS)

# adding linear regression data to new columns in the Siganus_fuscescens dataframe
# Calculate log-transformed values for Mass_g and SL_cm
Siganus_fuscescens$log_Mass_g <- log10(Siganus_fuscescens$Mass_g)
Siganus_fuscescens$log_SL_cm <- log10(Siganus_fuscescens$SL_cm)


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
  ggtitle("Linear Regression model of S. fuscescens")+
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

print(unique(Siganus_fuscescens$Locality))

# Calculate summary statistics for SL_cm & Mass_g by Locality
summary_stats <- Siganus_fuscescens %>%
  group_by(Locality) %>%
  summarise(
    mean_SL_cm = mean(SL_cm, na.rm = TRUE),
    se_SL_cm = sd(SL_cm, na.rm = TRUE) / sqrt(n()),
    mean_Mass_g = mean(Mass_g, na.rm = TRUE), 
    se_Mass_g = sd(Mass_g, na.rm = TRUE) / sqrt(n())
  )
print(summary_stats)

# Merge summary statistics with the original data
Siganus_fuscescens <- Siganus_fuscescens %>%
  left_join(summary_stats, by = "Locality")

# Boxplot for SL_cm with jittered points and standard error bars
ggplot(Siganus_fuscescens, aes(x = Locality, y = SL_cm)) +
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

ggplot(Siganus_fuscescens, aes(x = Locality, y = Mass_g)) +
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
ggplot(Siganus_fuscescens, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Siganus_fuscescens$cf)
# p-value = 0.2365 (> 0.05), so the data do not violate the assumption of a normal distribution

# Check homogeneity of variances using Levene's Test
leveneTest(cf ~ Locality, data = Siganus_fuscescens)
# p-value = 0.327 (> 0.05), so the data do not violate the assumption of homogeneity
# Use parametric test. Use ANOVA. 

# ANOVA
cf_anova_result <- aov(cf ~ Locality, data = Siganus_fuscescens)
cf_anova_summary <- summary(cf_anova_result)
print(cf_anova_summary)
#p-value = 4.71e-11 (< 0.05), so there is a significant difference in cf among localities. Run Tukey's HSD post-hoc test.

# If significant, perform Tukey's HSD post-hoc test
if (cf_anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  cf_tukey_result <- TukeyHSD(cf_anova_result)
  print(cf_tukey_result)
}
# Tukey HSD Results
# $Locality   diff          lwr        upr     p adj
# Mindanao_River_Cotabato_Mindanao-Cebu_Strait       0.5136416  0.364885607  0.6623975 0.0000000 (significant)
# Surigao_Mindanao-Cebu_Strait                       0.1498965 -0.002086654  0.3018796 0.0539813
# Surigao_Mindanao-Mindanao_River_Cotabato_Mindanao -0.3637451 -0.503067786 -0.2244224 0.0000002 (significant

# Create the base boxplot
cf_plot <- ggplot(Siganus_fuscescens, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(cf_plot)


#### Statistical Tests for significant differences of Kn by Locality ----

# Boxplot for Kn to visualize the data
ggplot(Siganus_fuscescens, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Siganus_fuscescens$Kn)
# p-value = 0.3406 (> 0.05), so the data do not violate the assumption of a normal distribution

# Check homogeneity of variances using Levene's Test
leveneTest(Kn ~ Locality, data = Siganus_fuscescens)
# p-value = 0.3395 (>0.05), so assumption of homogeneity of data is not violated

# Use parametric test. Use ANOVA. 

# ANOVA
Kn_anova_result <- aov(Kn ~ Locality, data = Siganus_fuscescens)
Kn_anova_summary <- summary(Kn_anova_result)
print(Kn_anova_summary)
# p = 2.17e-10 (<0.05). There is a significant difference in Kn between Localities. Run Tukey's HSD post-hoc test.

# If significant, perform Tukey's HSD post-hoc test
if (Kn_anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  Kn_tukey_result <- TukeyHSD(Kn_anova_result)
  print(Kn_tukey_result)
}
# Tukey HSD Results
# $Locality       diff           lwr        upr     p adj
# Mindanao_River_Cotabato_Mindanao-Cebu_Strait       0.25427496  0.1773565344  0.3311934 0.0000000 (significant)
# Surigao_Mindanao-Cebu_Strait                       0.07879766  0.0002105228  0.1573848 0.0492535 (significant)
# Surigao_Mindanao-Mindanao_River_Cotabato_Mindanao -0.17547730 -0.2475180145 -0.1034366 0.0000009 (significant)

# Create the base boxplot
Kn_plot <- ggplot(Siganus_fuscescens, aes(x = Locality, y = Kn, fill = Locality)) +
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
  geom_point(data = Siganus_fuscescens, aes(x = lon, y = lat, color = Locality), size = 3) +
  scale_color_manual(values = c("Cebu_Strait" = "#F8766D", "Mindanao_River_Cotabato_Mindanao" = "#00BA38", "Surigao_Mindanao" = "#619CFF")) +
  labs(title = "Sampling Locations of Siganus fuscescens", x = "Longitude", y = "Latitude", color = "Locality") +
  scale_x_continuous(breaks = seq(116, 128, by = 2)) +
  coord_fixed(ratio = 1.3, xlim = c(116, 128), ylim = c(4, 20)) +
  theme_minimal()
