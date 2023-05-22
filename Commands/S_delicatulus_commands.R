# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(nls2)
library(tidyverse)
library(patchwork)

# S. delicatulus Dataset ----
Spratelloides_delicatulus <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Spratelloides_delicatulus.xlsx")
View(Spratelloides_delicatulus)
summary(Spratelloides_delicatulus)
y <- c(Spratelloides_delicatulus$Mass_g)
xS <- c(Spratelloides_delicatulus$SL_cm)
xT <- c(Spratelloides_delicatulus$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=.5, bfit=.5))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.007912
b <- 3.154975
summary(nls1)
summary(nls1)$coeffdetermination

# Plot S. delicatulus ----
ggplot(Spratelloides_delicatulus, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.007912*(x^(3.154975))), se = TRUE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus")+
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
  ggtitle("Length-Weight log10a vs b of S. delicatulus")+
  xlab("b")+
  ylab("log10a")
  
  # Annotated Graph ---- 
ggplot(Spratelloides_delicatulus, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.007912*(x^(3.154975))), se = TRUE)+
  geom_segment(aes(x = 6.4, xend = 1.6, y = 2, yend = 2), color = "red")+
  annotate("text" , label="y ~ 0.007912x^(3.154975)  RSE ~ 0.1009", x=3.5, y=1.5)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus")+
  xlab("SL_cm")+
  ylab("Mass_g")

 # Incomplete R2
compdata <- data.frame(xS, y)
rsq = c(log(y) ~ deriv(0.007912) + (3.154975(log(xS))), data = compdata)
summary(rsq)

ggplot(rsq, aes(x=data.xS, y=data.y))+
  geom_point(aes(fill=))+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus")+
  xlab("SL_cm")+
  ylab("Mass_g")
