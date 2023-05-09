# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)

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
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.031066
b <- 2.922080
summary(nls1)
summary(nls1)$coeffdetermination

# Plot S. delicatulus ----
ggplot(Spratelloides_delicatulus, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.031066*(x^(2.922080))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. delicatulus")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Spratelloides delicatulus")
fb_a <- c(length_weight("Spratelloides delicatulus"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

ggplot(Spratelloides_delicatulus, aes(x=SL_cm, y=Mass_g))+
  
  
  # INCOMPLETE ---- 