#### readme ####

# Created by: Gabriel Salomon
# Last Updated by: John Whalen
# Last Updated: 6/18/24

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

# for Gabe. S. gracilis Dataset ----
Spratelloides_gracilis <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/Albatross_LWR_data.xlsx", 9)

# for John. Uses the relative path from the Commands/Albatross/ directory where this R file is located. 
Spratelloides_gracilis <- read_excel("../../Data/Albatross_LWR_data/Albatross_LWR_data.xlsx", 9)

View(Spratelloides_gracilis)
summary(Spratelloides_gracilis)
y <- c(Spratelloides_gracilis$Mass_g)
xS <- c(Spratelloides_gracilis$SL_cm)
xT <- c(Spratelloides_gracilis$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# add coordinates to sites in the dataframe Spratelloides_gracilis ----
# print unique localities
print(unique(Spratelloides_gracilis$Locality))

# Create a data frame with Locality, Latitude, and Longitude
locality_coords <- data.frame(
  Locality = c("Luoc_Lubong_Island", "Grande_Island_Reef"),
  lat = c(13.72644, 14.73765),
  lon = c(120.28504, 120.27013)
)

# Print the locality_coords data frame
print(locality_coords)

# Merge the locality_coords data frame with Spratelloides_gracilis data frame
Spratelloides_gracilis <- merge(Spratelloides_gracilis, locality_coords, by = "Locality", all.x = TRUE)

# Print the updated Spratelloides_gracilis data frame
head(Spratelloides_gracilis)

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start=list(afit=.5, bfit=.5))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.0059858
b <- 3.1470502
summary(nls1)

# Plot S. gracilis ----
ggplot(Spratelloides_gracilis, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0059858*(x^(3.1470502))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. gracilis")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----

# for John
logab <- read_excel("../../Data/Albatross_LWR_data/loga_b_fishbase_comparison.xlsx", 7) #! only 1 sheet, not 7

# for Gabe
logab <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/loga_b_fishbase_comparison.xlsx",7) #! only 1 sheet, not 7

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
  ggtitle("Length-Weight log10a vs b of Spratelloides gracilis")+
  xlab("b")+
  ylab("log10a")

# Original Fishbase comparison ---
length_weight("Spratelloides gracilis")
fb_a <- c(length_weight("Spratelloides gracilis"))
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
  ggtitle("Length-Weight log10a vs b of S. gracilis")+
  xlab("b")+
  ylab("log10a")
  

# Annotated Graph ---- 
ggplot(Spratelloides_gracilis, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0059858*(x^(3.1470502))), se = TRUE)+
  annotate("text" , label="y ~ 0.0059858x^(3.1470502)  RSE ~ 0.06427", x=3.5, y=1.5)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. gracilis")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Relative condition factor 
exp_weight <- ((a)*((xS)^(b)))
Kn <- (y)/(exp_weight)
rcf <- data.frame(xS, Kn)
avg_Kn <- mean(Kn)
avg_Kn
rKn <- (exp_weight)/((a)*(xS))
cf <- ((100)*((y)/(xS)^(3)))
avg_cf <- mean(cf)
avg_cf

# adding relative condition factor data to new columns in the Spratelloides_gracilis dataframe
# Step 1: Create the 'exp_weight' column. This is based on the length-weight relationship equation: W = aL^b
Spratelloides_gracilis$exp_weight <- a * (Spratelloides_gracilis$SL_cm ^ b)

# Step 2: Create the 'Kn' column. Le Cren's Relative Condition Factor equation: Kn = W/aL^b
Spratelloides_gracilis$Kn <- Spratelloides_gracilis$Mass_g / Spratelloides_gracilis$exp_weight

# Step 3: Create the 'rKn' column.Le Cren's Relative Condition Factor, but using the expected instead of the observed weight.
Spratelloides_gracilis$rKn <- Spratelloides_gracilis$exp_weight / (a * Spratelloides_gracilis$SL_cm)

# Step 4: Create the 'cf' column. Fulton's Condition Factor equation: cf = 100(W/SL^3)
Spratelloides_gracilis$cf <- 100 * (Spratelloides_gracilis$Mass_g / (Spratelloides_gracilis$SL_cm ^ 3))

# Relative Condition Factor (Kn) of S. gracilis. 
ggplot(Spratelloides_gracilis, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. gracilis")+
  xlab("SL (cm)")+
  ylab("Kn")

# Relative Condition Factor (Kn) of S. gracilis with points colored by locality.
ggplot(Spratelloides_gracilis, aes(x=SL_cm, y=Kn))+
  geom_point(aes(color = Locality))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. gracilis")+
  xlab("SL (cm)")+
  ylab("Kn")

# Le Cren's Relative Condition Factor (Kn) of S. gracilis by locality.
ggplot(Spratelloides_gracilis, aes(x=SL_cm, y=Kn, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Le Cren's Relative Condition Factor (Kn) of S. gracilis by Locality")+
  xlab("SL (cm)")+
  ylab("Kn")

# Fulton's Condition Factor (cf) of S. gracilis by locality.
ggplot(Spratelloides_gracilis, aes(x=SL_cm, y=cf, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Fulton's Condition Factor (cf) of S. gracilis by Locality")+
  xlab("SL (cm)")+
  ylab("cf")


ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 1.003948", x=3, y=1.25)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. gracilis")+
  xlab("SL_cm")+
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
  ggtitle("Linear Regression model of S. gracilis")+
  xlab("log10_SL_cm")+
  ylab("log10_Mass_g")


# adding linear regression data to new columns in the Spratelloides_gracilis dataframe
# Calculate log-transformed values for Mass_g and SL_cm
Spratelloides_gracilis$log_Mass_g <- log10(Spratelloides_gracilis$Mass_g)
Spratelloides_gracilis$log_SL_cm <- log10(Spratelloides_gracilis$SL_cm)


#### Site comparison ####

print(unique(Spratelloides_gracilis$Locality))

# Calculate summary statistics for SL_cm & Mass_g by Locality
summary_stats <- Spratelloides_gracilis %>%
  group_by(Locality) %>%
  summarise(
    mean_SL_cm = mean(SL_cm, na.rm = TRUE),
    se_SL_cm = sd(SL_cm, na.rm = TRUE) / sqrt(n()),
    mean_Mass_g = mean(Mass_g, na.rm = TRUE), 
    se_Mass_g = sd(Mass_g, na.rm = TRUE) / sqrt(n())
  )
print(summary_stats)

# Merge summary statistics with the original data
Spratelloides_gracilis <- Spratelloides_gracilis %>%
  left_join(summary_stats, by = "Locality")

# Boxplot for SL_cm with jittered points and standard error bars
ggplot(Spratelloides_gracilis, aes(x = Locality, y = SL_cm)) +
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

ggplot(Spratelloides_gracilis, aes(x = Locality, y = Mass_g)) +
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
ggplot(Spratelloides_gracilis, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Spratelloides_gracilis$cf)
# p-value = 0.3014 (> 0.05), so the data are normally distributed

# Check homogeneity of variances using Levene's Test
leveneTest(cf ~ Locality, data = Spratelloides_gracilis)
# p-value = 0.004428 (< 0.05), so assumption of homogeneity of data is violated
# use parametric test. Use Kruskal-Wallis Test

# Perform the Kruskal-Wallis test for Fulton's Condition Factor (cf)
kruskal_cf <- kruskal.test(cf ~ Locality, data = Spratelloides_gracilis)
print(kruskal_cf)
# p-value = 0.04899. This indicates that there is a significant difference in cf across Localities. This suggests that at least one Locality has a different median cf compared to the others.

# Check if the Kruskal-Wallis test is significant
if (kruskal_cf$p.value < 0.05) {
  # If significant, perform Dunn's test for post-hoc analysis. This will identify which specific pairs of localities differ. 
  dunn_cf <- dunn.test(Spratelloides_gracilis$cf, Spratelloides_gracilis$Locality, method="bonferroni")
  print(dunn_cf)
}

# Create the base boxplot
cf_plot_sig <- ggplot(Spratelloides_gracilis, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(cf_plot_sig)


# Statistical Tests for significant differences of Kn by Locality ----

# Boxplot for Kn to visualize the data
ggplot(Spratelloides_gracilis, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Spratelloides_gracilis$Kn)
# p-value = 0.04967 (< 0.05), so the data are not normally distributed
# use non-parametric test. use Kruskal-Wallis Test

# Check homogeneity of variances using Levene's Test
leveneTest(Kn ~ Locality, data = Spratelloides_gracilis)
# p-value = 0.001447 (< 0.05), so assumption of homogeneity of data is violated
# use non-parametric test. use Kruskal-Wallis Test

# Perform the Kruskal-Wallis test for Le Cren's Relative Condition Factor (Kn)
kruskal_Kn <- kruskal.test(Kn ~ Locality, data = Spratelloides_gracilis)
print(kruskal_Kn)
# p-value = 0.3978. This indicates that there is not a significant difference in Kn across Localities. 

# Check if the Kruskal-Wallis test is significant
if (kruskal_Kn$p.value < 0.05) {
  # If significant, perform Dunn's test for post-hoc analysis. This will identify which specific pairs of localities differ. 
  dunn_Kn <- dunn.test(Spratelloides_gracilis$Kn, Spratelloides_gracilis$Locality, method="bonferroni")
  print(dunn_Kn)
}

# Create the base boxplot
Kn_plot_sig <- ggplot(Spratelloides_gracilis, aes(x = Locality, y = Kn, fill = Locality)) +
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
  geom_point(data = Spratelloides_gracilis, aes(x = lon, y = lat, color = Locality), size = 3) +
  scale_color_manual(values = c("Grande_Island_Reef" = "#F8766D", "Luoc_Lubong_Island" = "#619CFF")) +
  labs(title = "Sampling Locations of Spratelloides gracilis", x = "Longitude", y = "Latitude", color = "Locality") +
  scale_x_continuous(breaks = seq(116, 128, by = 2)) +
  coord_fixed(ratio = 1.3, xlim = c(116, 128), ylim = c(4, 20)) +
  theme_minimal()
