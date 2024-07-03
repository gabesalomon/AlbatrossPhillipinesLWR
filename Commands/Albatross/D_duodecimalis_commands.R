#### readme ####

# Created by: Gabriel Salomon
# Last Updated by: John Whalen
# Last Updated: 6/20/24

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
Doboatherina_duodecimalis <- read_excel("../../Data/Albatross_LWR_data/Albatross_LWR_data.xlsx", 12)

# for Gabe. D. duodecimalis Dataset
Doboatherina_duodecimalis <- read_excel("AlbatrossPhillipinesLWR/Data/Albatross_LWR_data/Albatross_LWR_data.xlsx",12)

view(Doboatherina_duodecimalis)

#### add coordinates to sites in the dataframe Doboatherina_duodecimalis ####

# unique Localities
print(unique(Doboatherina_duodecimalis$Locality))

# Locality Coordinates
# Port_Busin_Burias_Island: 13.12921, 122.97338
# Atulayan_Bay_Luzon: 13.55397, 123.55014
# Pandanon_Island: 10.17966, 124.09
# Bolinao_Bay_Luzon: 16.38973, 119.90839
# Parang_Mindanao: 7.3713, 124.25457

# Create a data frame with Locality, Latitude, and Longitude.
locality_coords <- data.frame(
  Locality = c("Port_Busin_Burias_Island", "Atulayan_Bay_Luzon", "Pandanon_Island", "Bolinao_Bay_Luzon", "Parang_Mindanao"),
  lat = c(13.12921, 13.55397, 10.17966, 16.38973, 7.3713),
  lon = c(122.97338, 123.55014, 124.09, 119.90839, 124.25457)
)

# Print the locality_coords data frame
print(locality_coords)

# Merge the locality_coords data frame with Doboatherina_duodecimalis data frame
Doboatherina_duodecimalis <- merge(Doboatherina_duodecimalis, locality_coords, by = "Locality", all.x = TRUE)

# Print the updated Doboatherina_duodecimalis data frame
head(Doboatherina_duodecimalis)


#### CALCULATIONS ####

summary(Doboatherina_duodecimalis)
y <- c(Doboatherina_duodecimalis$Mass_g)
xS <- c(Doboatherina_duodecimalis$SL_cm)
xT <- c(Doboatherina_duodecimalis$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.0097448
b <- 3.1910590
summary(nls1)

# Plot D. duodecimalis ----
ggplot(Doboatherina_duodecimalis, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0097448*(x^(3.1910590))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of D. duodecimalis")+
  xlab("SL (cm)")+
  ylab("Mass (g)")

# Fishbase comparison ----
length_weight("Doboatherina duodecimalis")
fb_a <- c(length_weight("Doboatherina duodecimalis"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

log_a <- log10(fb_a$a)
color <- c("black","black")
data.frame(fb_a$b, log_a, color)
comp2 <- data.frame(fb_a$b, log_a, color)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a), color=color)+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of D. duodecimalis")+
  xlab("b")+
  ylab("log10a")

# Annotated Graph ---- 
ggplot(Doboatherina_duodecimalis, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0097448*(x^(3.1910590))), se = TRUE)+
  annotate("text" , label="y ~ 0.0097448x^(3.1910590)  RSE ~ 0.1456", x=6, y=.8)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of D. duodecimalis")+
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

#### adding relative condition factor data to new columns in the Doboatherina_duodecimalis dataframe ####
# Step 1: Create the 'exp_weight' column. This is based on the length-weight relationship equation: W = aL^b
Doboatherina_duodecimalis$exp_weight <- a * (Doboatherina_duodecimalis$SL_cm ^ b)

# Step 2: Create the 'Kn' column. Le Cren's Relative Condition Factor equation: Kn = W/aL^b
Doboatherina_duodecimalis$Kn <- Doboatherina_duodecimalis$Mass_g / Doboatherina_duodecimalis$exp_weight

# Step 3: Create the 'rKn' column.Le Cren's Relative Condition Factor, but using the expected instead of the observed weight.
Doboatherina_duodecimalis$rKn <- Doboatherina_duodecimalis$exp_weight / (a * Doboatherina_duodecimalis$SL_cm)

# Step 4: Create the 'cf' column. Fulton's Condition Factor equation: cf = 100(W/SL^3)
Doboatherina_duodecimalis$cf <- 100 * (Doboatherina_duodecimalis$Mass_g / (Doboatherina_duodecimalis$SL_cm ^ 3))

#### Sample size for each Locality ####
locality_counts <- Doboatherina_duodecimalis %>%
  count(Locality, name = "Sample_Size")

# Print the counts
print(locality_counts)


# Relative Condition Factor (Kn) of D. duodecimalis. 
ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of D. duodecimalis")+
  xlab("SL (cm)")+
  ylab("Kn")

# Relative Condition Factor (Kn) of D. duodecimalis with points colored by locality.
ggplot(Doboatherina_duodecimalis, aes(x=SL_cm, y=Kn))+
  geom_point(aes(color = Locality))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9734, R2 = 0.1443", x=2.75, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of D. duodecimalis")+
  xlab("SL (cm)")+
  ylab("Kn")

# Le Cren's Relative Condition Factor (Kn) of D. duodecimalis by locality.
ggplot(Doboatherina_duodecimalis, aes(x=SL_cm, y=Kn, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Le Cren's Relative Condition Factor (Kn) of D. duodecimalis by Locality")+
  xlab("SL (cm)")+
  ylab("Kn")

# Fulton's Condition Factor (cf) of D. duodecimalis by locality.
ggplot(Doboatherina_duodecimalis, aes(x=SL_cm, y=cf, color = Locality))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Fulton's Condition Factor (cf) of D. duodecimalis by Locality")+
  xlab("SL (cm)")+
  ylab("cf")

#Linear regression formula
logw <- log10(y)
logl <- log10(xS)

# adding linear regression data to new columns in the Doboatherina_duodecimalis dataframe
# Calculate log-transformed values for Mass_g and SL_cm
Doboatherina_duodecimalis$log_Mass_g <- log10(Doboatherina_duodecimalis$Mass_g)
Doboatherina_duodecimalis$log_SL_cm <- log10(Doboatherina_duodecimalis$SL_cm)

ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9784393", x=6, y=.75)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of D. duodecimalis")+
  xlab("SL (cm)")+
  ylab("Kn")

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
  ggtitle("Linear Regression model of D. duodecimalis")+
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

print(unique(Doboatherina_duodecimalis$Locality))

# Calculate summary statistics for SL_cm & Mass_g by Locality
summary_stats <- Doboatherina_duodecimalis %>%
  group_by(Locality) %>%
  summarise(
    mean_SL_cm = mean(SL_cm, na.rm = TRUE),
    se_SL_cm = sd(SL_cm, na.rm = TRUE) / sqrt(n()),
    mean_Mass_g = mean(Mass_g, na.rm = TRUE), 
    se_Mass_g = sd(Mass_g, na.rm = TRUE) / sqrt(n())
  )
print(summary_stats)

# Merge summary statistics with the original data
Doboatherina_duodecimalis <- Doboatherina_duodecimalis %>%
  left_join(summary_stats, by = "Locality")

# Boxplot for SL_cm with jittered points and standard error bars
ggplot(Doboatherina_duodecimalis, aes(x = Locality, y = SL_cm)) +
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

ggplot(Doboatherina_duodecimalis, aes(x = Locality, y = Mass_g)) +
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
ggplot(Doboatherina_duodecimalis, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Doboatherina_duodecimalis$cf)
# p-value = 1.289e-06 (< 0.05), so the data violate the assumption of a normal distribution

# Check homogeneity of variances using Levene's Test
leveneTest(cf ~ Locality, data = Doboatherina_duodecimalis)
# p-value = 2.358e-07 (< 0.05), so the data violate the assumption of homogeneity
# Use nonparametric test. Use Kruskal-Wallis. 

# Perform the Kruskal-Wallis test for Fulton's Condition Factor (cf)
kruskal_cf <- kruskal.test(cf ~ Locality, data = Doboatherina_duodecimalis)
print(kruskal_cf)
# p-value = 4.923e-08. This indicates that there is a significant difference in cf across Localities. This suggests that at least one Locality has a different median cf compared to the others.

# Check if the Kruskal-Wallis test is significant
if (kruskal_cf$p.value < 0.05) {
  # If significant, perform Dunn's test for post-hoc analysis. This will identify which specific pairs of localities differ. 
  dunn_cf <- dunn.test(Doboatherina_duodecimalis$cf, Doboatherina_duodecimalis$Locality, method="bonferroni")
  print(dunn_cf)
}

# Kruskal-Wallis rank sum test
# 
# data: x and group
# Kruskal-Wallis chi-squared = 39.7297, df = 4, p-value = 0
# 
# 
# Comparison of x by group                            
# (Bonferroni)                                  
# Col Mean-|
#   Row Mean |   Atulayan   Bolinao_   Pandanon   Parang_M
# ---------+--------------------------------------------
#   Bolinao_ |  -2.932728
#            |    0.0168*
#            |
#   Pandanon |   0.238753   3.756674
#            |     1.0000    0.0009*
#            |
#   Parang_M |  -1.304333   3.487831  -1.933655
#            |     0.9606    0.0024*     0.2658
#            |
#   Port_Bus |   0.161936   5.853206  -0.156512   3.639781
#            |     1.0000    0.0000*     1.0000    0.0014*


# Create the base boxplot
cf_plot_sig <- ggplot(Doboatherina_duodecimalis, aes(x = Locality, y = cf, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Fulton's Condition Factor (cf) by Locality", x = "Locality", y = "cf") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(cf_plot_sig)


#### Statistical Tests for significant differences of Kn by Locality ----

# Boxplot for Kn to visualize the data
ggplot(Doboatherina_duodecimalis, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(Doboatherina_duodecimalis$Kn)
# p-value = 0.01041 (< 0.05), so the data violate the assumption of a normal distribution

# Check homogeneity of variances using Levene's Test
leveneTest(Kn ~ Locality, data = Doboatherina_duodecimalis)
# p-value = 3.541e-07 (<0.05), so the data violate the assumption of homogeneity
# Use non parametric test. Use Kruskal-Wallis. 


# Perform the Kruskal-Wallis test for Le Cren's Relative Condition Factor (Kn)
kruskal_Kn <- kruskal.test(Kn ~ Locality, data = Doboatherina_duodecimalis)
print(kruskal_Kn)
# p-value = 9.137e-08. This indicates that there is a significant difference in Kn across Localities. This suggests that at least one Locality has a different median Kn compared to the others.

# Check if the Kruskal-Wallis test is significant
if (kruskal_Kn$p.value < 0.05) {
  # If significant, perform Dunn's test for post-hoc analysis. This will identify which specific pairs of localities differ. 
  dunn_Kn <- dunn.test(Doboatherina_duodecimalis$Kn, Doboatherina_duodecimalis$Locality, method="bonferroni")
  print(dunn_Kn)
}


# Kruskal-Wallis rank sum test
# 
# data: x and group
# Kruskal-Wallis chi-squared = 38.4296, df = 4, p-value = 0
# 
# 
# Comparison of x by group                            
# (Bonferroni)                                  
# Col Mean-|
#   Row Mean |   Atulayan   Bolinao_   Pandanon   Parang_M
# ---------+--------------------------------------------
#   Bolinao_ |  -3.452803
#            |    0.0028*
#            |
#   Pandanon |  -0.723387   3.057426
#            |     1.0000    0.0112*
#            |
#   Parang_M |  -0.940496   5.226360  -0.043185
#            |     1.0000    0.0000*     1.0000
#            |
#   Port_Bus |  -0.336983   5.943368   0.657974   1.478367
#            |     1.0000    0.0000*     1.0000     0.6965


# Create the base boxplot
Kn_plot_sig <- ggplot(Doboatherina_duodecimalis, aes(x = Locality, y = Kn, fill = Locality)) +
  geom_boxplot() +
  labs(title = "Le Cren's Relative Condition Factor (Kn) by Locality", x = "Locality", y = "Kn") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(Kn_plot_sig)


#### Map of sampling Locations ####

# Get the map data for the Philippines
philippines_map <- map_data("world2", region = "Philippines")

# create the map
ggplot() +
  geom_polygon(data = philippines_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = Doboatherina_duodecimalis, aes(x = lon, y = lat, color = Locality), size = 3) +
  scale_color_manual(values = c("Atulayan_Bay_Luzon" = "#F8766D", "Bolinao_Bay_Luzon" = "#C5B358", "Pandanon_Island" = "#00BA38", "Parang_Mindanao" = "#619CFF", "Port_Busin_Burias_Island" = "#F564E3")) +
  labs(title = "Sampling Locations of Doboatherina duodecimalis", x = "Longitude", y = "Latitude", color = "Locality") +
  scale_x_continuous(breaks = seq(116, 128, by = 2)) +
  coord_fixed(ratio = 1.3, xlim = c(116, 128), ylim = c(4, 20)) +
  theme_minimal()


