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

# G. oyena Dataset ----
Gerres_oyena_restricted <- read_excel("AlbatrossPhillipinesLWR/Data/Matching_LWR_data/Gerres_oyena_restricted.xlsx")
View(Gerres_oyena_restricted)
summary(Gerres_oyena_restricted)
y <- c(Gerres_oyena_restricted$Mass_g)
xS <- c(Gerres_oyena_restricted$SL_cm)
xT <- c(Gerres_oyena_restricted$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.013367
b <- 3.231499
summary(nls1)

# Plot G. oyena ----
ggplot(Gerres_oyena_restricted, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.013367*(x^(3.231499))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of G. oyena")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Gerres oyena")
fb_a <- c(length_weight("Gerres oyena"))
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
  ggtitle("Length-Weight log10a vs b of G. oyena")+
  xlab("b")+
  ylab("log10a")


# Annotated Graph ---- 
ggplot(Gerres_oyena_restricted, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.013367*(x^(3.231499))), se = TRUE)+
  geom_segment(aes(x = 19, xend = 2.8, y = 2, yend = 2), color = "red")+
  annotate("text" , label="y ~ 0.013367x^(3.231499)  RSE ~ 0.5396", x=8, y=30)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of G. oyena")+
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
  annotate("text" , label="Average Kn = 1.002363", x=7, y=1.1)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of G. oyena")+
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
  ggtitle("Linear Regression model of G. oyena")+
  xlab("log10_SL_cm")+
  ylab("log10_Mass_g")

#Approximating shrinkage in ethanol ----
G_oyena_after <- read_excel("AlbatrossPhillipinesLWR/Data/One_month_LWR_data/G_oyena_after.xlsx")
View(G_oyena_after)
summary(G_oyena_after)

Gerres_oyena_fresh <- read_excel("AlbatrossPhillipinesLWR/Data/Fresh_LWR_data/Gerres_oyena_fresh.xlsx")
View(Gerres_oyena_fresh)
summary(Gerres_oyena_fresh)

xS_before <- c(Gerres_oyena_fresh$SL_cm)
wt_before <- c(Gerres_oyena_fresh$Mass_g)
xS_after <- c(G_oyena_after$Standard_length_mm)
wt_after <- c(G_oyena_after$wt_g_after)
wt_before_comp <- data.frame(xS_before, wt_before)
wt_after_comp <- data.frame(xS_after, wt_after)

nls_before <- nls(wt_before ~ afit*xS_before^bfit, data.frame(xS_before, wt_before), start=list(afit=.05, bfit=.05), control = nls.control(maxiter = 1000))
print(nls_before)
nls_after <- nls(wt_after ~ afit*xS_after^bfit, data.frame(xS_after, wt_after), start=list(afit=.05, bfit=.05), control = nls.control(maxiter = 1000))
print(nls_after)
summary(nls_before)
summary(nls_after)

xScomb <- c(G_oyena_after$SL_mm_comb)
wtcomb <- c(G_oyena_after$wt_g_comb)
comb <- data.frame(xScomb, wtcomb)

ggplot(comb, aes(x=xScomb, y=wtcomb))+
  geom_point()+
  geom_smooth(method = glm, formula = y ~ I(0.006169*(x^(2.760337))), se = FALSE, color ="green")+
  geom_smooth(method = glm, formula = y ~ I(0.010633*(x^(2.923))), se = FALSE, color = "red")+
  geom_smooth(method = glm, formula = y ~ I(0.013367*(x^(3.231499))), se = FALSE, color = "blue")+
  theme(axis.text.x = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0))+
  ggtitle("Approximate shrinkage of G. oyena")+
  xlab("SL_cm")+
  ylab("Mass_g")+
  labs(caption = "Fresh = y ~ 0.006169x^(2.760337) (Green), 1 Month in EtOH = y ~ 0.010633x^(2.923) (Red) , Matching Albatross = y ~ 0.013367x^(3.231499) (Blue)")

#Incomplete ---- 
y <- c(Spratelloides_gracilis_restricted$Mass_g)
xS <- c(Spratelloides_gracilis_restricted$SL_cm)
S_gracilis_eth <- data.frame(xS, y) 
print(nls1)
summary(nls1)
beforeandafter <- data.frame(xS_before, wt_before, xS_after, wt_after)

