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

# S. fuscescens Dataset ----
Siganus_fuscescens_restricted <- read_excel("AlbatrossPhillipinesLWR/Data/Matching_LWR_data/Matching_LWR_data.xlsx",1)
View(Siganus_fuscescens_restricted)
summary(Siganus_fuscescens_restricted)
y <- c(Siganus_fuscescens_restricted$Mass_g)
xS <- c(Siganus_fuscescens_restricted$SL_cm)
xT <- c(Siganus_fuscescens_restricted$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.05021
b <- 2.52718
summary(nls1)

# Plot S. fuscescens ----
ggplot(Siganus_fuscescens_restricted, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.05021*(x^(2.52718))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. fuscescens")+
  xlab("SL (cm)")+
  ylab("Mass (g)")

# Fishbase comparison ---
log10ab <- read_excel("AlbatrossPhillipinesLWR/Data/Matching_LWR_data/loga_b_comparison.xlsx",1)
log10ab_after <- na.omit(log10ab)
View(log10ab_after)

ggplot(data = log10ab_after, aes(x=b, y=log10a, color=Source))+
  geom_point()+
  geom_smooth(log10ab_after, method = lm, mapping=aes(x=b, y=log10a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_color_discrete(log10ab_after$Species)+
  guides(colour=guide_legend(title = "Source"))+
  ggtitle("Length-Weight log10a vs b Comparison of S. fuscescens")+
  xlab("b")+
  ylab("log10a")

# Annotated Graph ---- 
ggplot(Siganus_fuscescens_restricted, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.05021*(x^(2.52718))), se = TRUE)+
  annotate("text" , label="y ~ 0.05021x^(2.52718)  RSE ~ 0.6569", x=7.5, y=5)+
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

ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 1.0019", x=7.25, y=1.2)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. fuscescens")+
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

#Approximating shrinkage in ethanol ----
S_fuscescens_after <- read_excel("AlbatrossPhillipinesLWR/Data/One_month_LWR_data/One_month_LWR_data.xlsx",1)
View(S_fuscescens_after)
summary(S_fuscescens_after)

Siganus_fuscescens_fresh <- read_excel("AlbatrossPhillipinesLWR/Data/Fresh_LWR_data/Fresh_LWR_data.xlsx",1)
View(Siganus_fuscescens_fresh)
summary(Siganus_fuscescens_fresh)

xS_before <- c(Siganus_fuscescens_fresh$SL_cm)
wt_before <- c(Siganus_fuscescens_fresh$Mass_g)
xS_after <- c(S_fuscescens_after$Standard_length_mm)
wt_after <- c(S_fuscescens_after$wt_g_after)
wt_before_comp <- data.frame(xS_before, wt_before)
wt_after_comp <- data.frame(xS_after, wt_after)

nls_before <- nls(wt_before ~ afit*xS_before^bfit, data.frame(xS_before, wt_before), start=list(afit=.05, bfit=.05), control = nls.control(maxiter = 1000))
print(nls_before)
nls_after <- nls(wt_after ~ afit*xS_after^bfit, data.frame(xS_after, wt_after), start=list(afit=.05, bfit=.05), control = nls.control(maxiter = 1000))
print(nls_after)
summary(nls_before)
summary(nls_after)

xScomb <- c(S_fuscescens_after$SL_mm_comb)
wtcomb <- c(S_fuscescens_after$wt_g_comb)
comb <- data.frame(xScomb, wtcomb)

ggplot(comb, aes(x=xScomb, y=wtcomb))+
  geom_point()+
  geom_smooth(method = glm, formula = y ~ I(0.006169*(x^(2.760337))), se = FALSE, color ="green")+
  geom_smooth(method = glm, formula = y ~ I(0.013246*(x^(2.829))), se = FALSE, color = "red")+
  geom_smooth(method = glm, formula = y ~ I(0.05021*(x^(2.52718))), se = FALSE, color = "blue")+
  theme(axis.text.x = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0))+
  ggtitle("Approximate shrinkage of S. fuscescens")+
  xlab("SL_cm")+
  ylab("Mass_g")+
  labs(caption = "Fresh = y ~ 0.006169x^(2.760337) (Green), 1 Month in EtOH = y ~ 0.013246x^(2.829) (Red) , Matching Albatross = y ~ 0.05021x^(2.52718) (Blue)")

#Incomplete ---- 
y <- c(Spratelloides_gracilis_restricted$Mass_g)
xS <- c(Spratelloides_gracilis_restricted$SL_cm)
S_gracilis_eth <- data.frame(xS, y) 
print(nls1)
summary(nls1)
beforeandafter <- data.frame(xS_before, wt_before, xS_after, wt_after)

