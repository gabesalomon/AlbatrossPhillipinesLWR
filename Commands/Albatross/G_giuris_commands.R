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

# add coordinates to sites in the dataframe Atherinomorus_endrachtensis ----
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

ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 1.025615", x=6, y=1.25)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of G. giuris")+
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
xSnooutlier