# Libraries ( Load only libraries if not first time) ----
install.packages("pacman")
install.packages("rlang")
install.packages("ggpubr")
install.packages("rfishbase")
install.packages("readxl")
install.packages("ggplot2")
install.packages("nls2")
install.packages("patchwork")
library(pacman)
library(rlang)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)
library(patchwork)

# S. delicatulus Dataset ----
Spratelloides_delicatulus_restricted <- read_excel("AlbatrossPhillipinesLWR/Data/Matching_LWR_data/Matching_LWR_data.xlsx",3)
View(Spratelloides_delicatulus_restricted)
summary(Spratelloides_delicatulus_restricted)
y <- c(Spratelloides_delicatulus_restricted$Mass_g)
xS <- c(Spratelloides_delicatulus_restricted$SL_cm)
xT <- c(Spratelloides_delicatulus_restricted$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=.5, bfit=.5))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.008220
b <- 3.130741
summary(nls1)

# Plot S. delicatulus ----
ggplot(Spratelloides_delicatulus_restricted, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.008220*(x^(3.130741))), se = TRUE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus")+
  xlab("SL (cm)")+
  ylab("Mass (g)")

# Fishbase comparison ---
log10ab <- read_excel("AlbatrossPhillipinesLWR/Data/Matching_LWR_data/loga_b_comparison.xlsx",3)
log10ab_after <- na.omit(log10ab)
View(log10ab_after)

ggplot(data = log10ab_after, aes(x=b, y=log10a, color=Source))+
  geom_point()+
  geom_smooth(log10ab_after, method = lm, mapping=aes(x=b, y=log10a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_color_discrete(log10ab_after$Source)+
  guides(colour=guide_legend(title = "Source"))+
  ggtitle("Length-Weight log10a vs b Comparison of S. delicatulus")+
  xlab("b")+
  ylab("log10a")

# Annotated Graph ---- 
ggplot(Spratelloides_delicatulus_restricted, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.008220*(x^(3.130741))), se = TRUE)+
  annotate("text" , label="y ~ 0.008220^(3.130741)  RSE ~ 0.1146", x=5, y=0.5)+
  theme(axis.text.x = element_text(hjust = 0.5), )+
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
  annotate("text" , label="Average Kn = 0.9991468", x=5.2, y=0.85)+  
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

#Approximating shrinkage in ethanol ----
S_delicatulus_after <- read_excel("AlbatrossPhillipinesLWR/Data/One_month_LWR_data/One_month_LWR_data.xlsx",3)
View(S_delicatulus_after)
summary(S_delicatulus_after)

Spratelloides_delicatulus_fresh <- read_excel("AlbatrossPhillipinesLWR/Data/Fresh_LWR_data/Fresh_LWR_data.xlsx",3)
View(Spratelloides_delicatulus_fresh)
summary(Spratelloides_delicatulus_fresh)

xS_before <- c(Spratelloides_delicatulus_fresh$SL_cm)
wt_before <- c(Spratelloides_delicatulus_fresh$Mass_g)
xS_after <- c(S_delicatulus_after$Standard_length_mm)
wt_after <- c(S_delicatulus_after$wt_g_after)
wt_before_comp <- data.frame(xS_before, wt_before)
wt_after_comp <- data.frame(xS_after, wt_after)

nls_before <- nls(wt_before ~ afit*xS_before^bfit, data.frame(xS_before, wt_before), start=list(afit=.05, bfit=.05), control = nls.control(maxiter = 1000))
print(nls_before)
nls_after <- nls(wt_after ~ afit*xS_after^bfit, data.frame(xS_after, wt_after), start=list(afit=.05, bfit=.05), control = nls.control(maxiter = 1000))
print(nls_after)
summary(nls_before)
summary(nls_after)

xScomb <- c(S_delicatulus_after$SL_mm_comb)
wtcomb <- c(S_delicatulus_after$wt_g_comb)
comb <- data.frame(xScomb, wtcomb)

ggplot(comb, aes(x=xScomb, y=wtcomb))+
  geom_point()+
  geom_smooth(method = glm, formula = y ~ I(0.03206*(x^(2.38442))), se = FALSE, color ="green")+
  geom_smooth(method = glm, formula = y ~ I(0.013246*(x^(2.829))), se = FALSE, color = "red")+
  geom_smooth(method = glm, formula = y ~ I(0.008220*(x^(3.130741))), se = FALSE, color = "blue")+
  theme(axis.text.x = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0))+
  ggtitle("Approximate shrinkage of S. delicatulus")+
  xlab("SL_cm")+
  ylab("Mass_g")+
  labs(caption = "Fresh = y ~ 0.03206x^(2.38442) (Green), 1 Month in EtOH = y ~ 0.013246x^(2.829) (Red) , Matching Albatross = y ~ 0.008220x^(3.130741) (Blue)")

#Incomplete ---- 
y <- c(Spratelloides_gracilis_restricted$Mass_g)
xS <- c(Spratelloides_gracilis_restricted$SL_cm)
S_gracilis_eth <- data.frame(xS, y) 
print(nls1)
summary(nls1)
beforeandafter <- data.frame(xS_before, wt_before, xS_after, wt_after)


