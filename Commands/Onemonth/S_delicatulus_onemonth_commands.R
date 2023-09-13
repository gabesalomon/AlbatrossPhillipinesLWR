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
S_delicatulus_onemonth <- read_excel("AlbatrossPhillipinesLWR/Data/One_month_LWR_data/S_delicatulus_after.xlsx")
S_delicatulus_after <- na.omit(S_delicatulus_onemonth)
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
a <- 0.013248
b <- 2.828562
summary(nls1)

# Plot S. delicatulus ----
ggplot(S_delicatulus_after, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.013248*(x^(2.828562))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus after 1 month in EtOH")+
  xlab("SL_cm")+
  ylab("Mass_g")

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
  geom_smooth(method = glm, formula = y ~ I(0.013248*(x^(2.828562))), se = TRUE)+
  geom_segment(aes(x = 6.4, xend = 1.6, y = 2, yend = 2), color = "red")+
  annotate("text" , label="y ~ 0.013248^(2.828562)  RSE ~ 0.1854", x=3.5, y=1.5)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus after 1 month in EtOH")+
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

ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.988", x=3.6, y=1.25)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of S. delicatulus after 1 month in EtOH")+
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
  ggtitle("Linear Regression model of S. delicatulus after 1 month in EtOH")+
  xlab("log10_SL_cm")+
  ylab("log10_Mass_g")

