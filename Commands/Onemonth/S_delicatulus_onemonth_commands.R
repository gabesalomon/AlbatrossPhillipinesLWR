#### README ------------------------------------------------------------------
  
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

#### Read in data

# for John. Uses the relative path from the Commands/Albatross/ directory where this R file is located. 
S_delicatulus_onemonth <- read_excel("../../Data/One_month_LWR_data/One_month_LWR_data.xlsx",3)

# for Gabe. S. delicatulus Dataset ----
S_delicatulus_onemonth <- read_excel("AlbatrossPhillipinesLWR/Data/One_month_LWR_data/One_month_LWR_data.xlsx",3)


S_delicatulus_after <- na.omit(S_delicatulus_onemonth) #! S_delicatulus_after is a blank dataframe that has omitted everything from the dataframe S_delicatulus_onemonth

View(S_delicatulus_after)
summary(S_delicatulus_after)
y <- c(S_delicatulus_after$Mass_g)
xS <- c(S_delicatulus_after$SL_cm)
xT <- c(S_delicatulus_after$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=.5, bfit=.5))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.014510
b <- 2.771572
summary(nls1)

# Plot S. delicatulus ----
ggplot(S_delicatulus_after, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.014510*(x^(2.771572))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus after 1 month in EtOH")+
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
  ggtitle("Length-Weight log10a vs b of S. delicatulus after 1 month in EtOH")+
  xlab("b")+
  ylab("log10a")

# Annotated Graph ---- 
ggplot(S_delicatulus_after, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.014510*(x^(2.771572))), se = TRUE)+
  annotate("text" , label="y ~ 0.014510^(2.771572)  RSE ~ 0.1844", x=5.3, y=.6)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus after 1 month in EtOH")+
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
  annotate("text" , label="Average Kn = 0.999", x=3.6, y=1.25)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. delicatulus after 1 month in EtOH")+
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
  ggtitle("Linear Regression model of S. delicatulus after 1 month in EtOH")+
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
