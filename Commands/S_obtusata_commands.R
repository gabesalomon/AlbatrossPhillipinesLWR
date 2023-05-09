# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)

# S. obtusata Dataset ----
Sphyraena_obtusata <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Sphyraena_obtusata.xlsx")
View(Sphyraena_obtusata)
summary(Sphyraena_obtusata)
y <- c(Sphyraena_obtusata$Mass_g)
xS <- c(Sphyraena_obtusata$SL_cm)
xT <- c(Sphyraena_obtusata$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.0024577
b <- 3.3447781
summary(nls1)
summary(nls1)$coeffdetermination

# Plot S. obtusata ----
ggplot(Sphyraena_obtusata, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0024577*(x^(3.3447781))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. obtusata")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Sphyraena obtusata")
fb_a <- c(length_weight("Sphyraena obtusata"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

ggplot(Sphyraena_obtusata, aes(x=SL_cm, y=Mass_g))+
  
  
  # INCOMPLETE ---- 