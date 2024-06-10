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

# for Gabe. A. endrachtensis Dataset ----
Atherinomorus_endrachtensis <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/Albatross_LWR_data.xlsx",13)

# for John. Uses the relative path from the Commands/Albatross/ directory where this R file is located. 
Atherinomorus_endrachtensis <- read_excel("../../Data/Albatross_LWR_data/Albatross_LWR_data.xlsx", 13)

View(Atherinomorus_endrachtensis)
summary(Atherinomorus_endrachtensis)
y <- c(Atherinomorus_endrachtensis$Mass_g)
xS <- c(Atherinomorus_endrachtensis$SL_cm)
xT <- c(Atherinomorus_endrachtensis$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

#### add coordinates to sites in the dataframe Atherinomorus_endrachtensis ----
# Create a data frame with Locality, Latitude, and Longitude
locality_coords <- data.frame(
  Locality = c("Luzon_Port_San_Vicente", "Busin_Harbor_Burias_Island", "Malcochin_Harbor_Linapacan"),
  lat = c(18.5154, 13.12921, 11.49066),
  lon = c(122.13897, 122.97338, 119.82816)
)

# Print the locality_coords data frame
print(locality_coords)

# Merge the locality_coords data frame with Atherinomorus_endrachtensis data frame
Atherinomorus_endrachtensis <- merge(Atherinomorus_endrachtensis, locality_coords, by = "Locality", all.x = TRUE)

# Print the updated Atherinomorus_endrachtensis data frame
head(Atherinomorus_endrachtensis)

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=.5, bfit=.5))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2) # error: plot.new has not been called yet
a <- 0.0059859
b <- 3.5772218
summary(nls1)

# Plot A. endrachtensis ----
ggplot(Atherinomorus_endrachtensis, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0059859*(x^(3.5772218))), se = TRUE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of A. endrachtensis")+
  xlab("SL (cm)")+
  ylab("Mass (g)")

# Fishbase comparison ---- None for this specie
length_weight("Atherinomorus endrachtensis")
fb_a <- c(length_weight("Atherinomorus endrachtensis"))
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
  ggtitle("Length-Weight log10a vs b of A. endrachtensis")+
  xlab("b")+
  ylab("log10a")

# Annotated Graph ---- 
ggplot(Atherinomorus_endrachtensis, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0059859*(x^(3.5772218))), se = TRUE)+
  annotate("text" , label="y ~ 0.0059859x^(3.5772218)  RSE ~ 0.2423", x=6.75, y=1.75)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of A. endrachtensis")+
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


# adding relative condition factor data to new columns in the Atherinomorus_endrachtensis dataframe
# Step 1: Create the 'exp_weight' column. This is based on the length-weight relationship equation: W = aL^b
Atherinomorus_endrachtensis$exp_weight <- a * (Atherinomorus_endrachtensis$SL_cm ^ b)

# Step 2: Create the 'Kn' column. Le Cren's Relative Condition Factor equation: Kn = W/aL^b
Atherinomorus_endrachtensis$Kn <- Atherinomorus_endrachtensis$Mass_g / Atherinomorus_endrachtensis$exp_weight

# Step 3: Create the 'rKn' column.Le Cren's Relative Condition Factor, but using the expected instead of the observed weight.
Atherinomorus_endrachtensis$rKn <- Atherinomorus_endrachtensis$exp_weight / (a * Atherinomorus_endrachtensis$SL_cm)

# Step 4: Create the 'cf' column. Fulton's Condition Factor equation: cf = 100(W/SL^3)
Atherinomorus_endrachtensis$cf <- 100 * (Atherinomorus_endrachtensis$Mass_g / (Atherinomorus_endrachtensis$SL_cm ^ 3))

# Relative Condition Factor (Kn) of A. endrachtensis. 
ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of A. endrachtensis")+
  xlab("SL (cm)")+
  ylab("Kn")

# Relative Condition Factor (Kn) of A. endrachtensis with points colored by locality.
ggplot(Atherinomorus_endrachtensis, aes(x=SL_cm, y=Kn))+
  geom_point(aes(color = Locality))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of A. endrachtensis")+
  xlab("SL (cm)")+
  ylab("Kn")


# Le Cren's Relative Condition Factor (Kn) of A. endrachtensis by locality.
ggplot(Atherinomorus_endrachtensis, aes(x=SL_cm, y=Kn, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Le Cren's Relative Condition Factor (Kn) of A. endrachtensis by Locality")+
  xlab("SL (cm)")+
  ylab("Kn")

# Fulton's Condition Factor (cf) of A. endrachtensis by locality.
ggplot(Atherinomorus_endrachtensis, aes(x=SL_cm, y=cf, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Fulton's Condition Factor (cf) of A. endrachtensis by Locality")+
  xlab("SL (cm)")+
  ylab("cf")


ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9637977", x=6.75, y=.78)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of A. endrachtensis")+
  xlab("SL (cm)")+
  ylab("Kn")

#Linear regression formula
logw <- log10(y)
logl <- log10(xS)

# adding linear regression data to new columns in the Atherinomorus_endrachtensis dataframe
# Calculate log-transformed values for Mass_g and SL_cm
Atherinomorus_endrachtensis$log_Mass_g <- log10(Atherinomorus_endrachtensis$Mass_g)
Atherinomorus_endrachtensis$log_SL_cm <- log10(Atherinomorus_endrachtensis$SL_cm)


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
  ggtitle("Linear Regression model of A. endrachtensis")+
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

print(unique(Atherinomorus_endrachtensis$Locality))

# Calculate summary statistics for SL_cm & Mass_g by Locality
summary_stats <- Atherinomorus_endrachtensis %>%
  group_by(Locality) %>%
  summarise(
    mean_SL_cm = mean(SL_cm, na.rm = TRUE),
    se_SL_cm = sd(SL_cm, na.rm = TRUE) / sqrt(n()),
    mean_Mass_g = mean(Mass_g, na.rm = TRUE), 
    se_Mass_g = sd(Mass_g, na.rm = TRUE) / sqrt(n())
  )
print(summary_stats)

# Merge summary statistics with the original data
Atherinomorus_endrachtensis <- Atherinomorus_endrachtensis %>%
  left_join(summary_stats, by = "Locality")

# Boxplot for SL_cm with jittered points and standard error bars
ggplot(Atherinomorus_endrachtensis, aes(x = Locality, y = SL_cm)) +
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

ggplot(Atherinomorus_endrachtensis, aes(x = Locality, y = Mass_g)) +
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
ggplot(Atherinomorus_endrachtensis, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Atherinomorus_endrachtensis$cf)
# p-value = 5.432e-06 (< 0.05), so the data are not normally distributed
# use non-parametric test. use Kruskal-Wallis Test

# Check homogeneity of variances using Levene's Test
leveneTest(cf ~ Locality, data = Atherinomorus_endrachtensis)
# p-value = 1.274e-08 (< 0.05), so the data violate the assumption of homogeneity

# Perform the Kruskal-Wallis test for Fulton's Condition Factor (cf)
kruskal_cf <- kruskal.test(cf ~ Locality, data = Atherinomorus_endrachtensis)
print(kruskal_cf)
# p-value = 1.309e-09. This indicates that there is a significant difference in cf across Localities. This suggests that at least one Locality has a different median cf compared to the others.

# Check if the Kruskal-Wallis test is significant
if (kruskal_cf$p.value < 0.05) {
  # If significant, perform Dunn's test for post-hoc analysis. This will identify which specific pairs of localities differ. 
  dunn_cf <- dunn.test(Atherinomorus_endrachtensis$cf, Atherinomorus_endrachtensis$Locality, method="bonferroni")
  print(dunn_cf)
}

# Create the base boxplot
cf_plot_sig <- ggplot(Atherinomorus_endrachtensis, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(cf_plot_sig)


#### Statistical Tests for significant differences of Kn by Locality ----

# Boxplot for Kn to visualize the data
ggplot(Atherinomorus_endrachtensis, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Atherinomorus_endrachtensis$Kn)
# p-value = 2.794e-09 (< 0.05), so the data are not normally distributed
# use non-parametric test. use Kruskal-Wallis Test

# Check homogeneity of variances using Levene's Test
leveneTest(Kn ~ Locality, data = Atherinomorus_endrachtensis)
# p-value = 2.063e-6 (<0.05), so assumption of homogeneity of data is violated

# Perform the Kruskal-Wallis test for Le Cren's Relative Condition Factor (Kn)
kruskal_Kn <- kruskal.test(Kn ~ Locality, data = Atherinomorus_endrachtensis)
print(kruskal_Kn)
# p-value = 1.113e-05. This indicates that there is a significant difference in Kn across Localities. This suggests that at least one Locality has a different median Kn compared to the others.

# Check if the Kruskal-Wallis test is significant
if (kruskal_Kn$p.value < 0.05) {
  # If significant, perform Dunn's test for post-hoc analysis. This will identify which specific pairs of localities differ. 
  dunn_Kn <- dunn.test(Atherinomorus_endrachtensis$Kn, Atherinomorus_endrachtensis$Locality, method="bonferroni")
  print(dunn_Kn)
}

# Create the base boxplot
Kn_plot_sig <- ggplot(Atherinomorus_endrachtensis, aes(x = Locality, y = Kn, fill = Locality)) +
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
  geom_point(data = Atherinomorus_endrachtensis, aes(x = lon, y = lat, color = Locality), size = 3) +
  scale_color_manual(values = c("Busin_Harbor_Burias_Island" = "#F8766D", "Luzon_Port_San_Vicente" = "#00BA38", "Malcochin_Harbor_Linapacan" = "#619CFF")) +
  labs(title = "Sampling Locations of Spratelloides delicatulus", x = "Longitude", y = "Latitude", color = "Locality") +
  scale_x_continuous(breaks = seq(116, 128, by = 2)) +
  coord_fixed(ratio = 1.3, xlim = c(116, 128), ylim = c(4, 20)) +
  theme_minimal()



