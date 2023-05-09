# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)

# S. gracilis Dataset ----
Spratelloides_gracilis <- read_excel("C:/Users/gabes/Desktop/Albatross_Raw_Data/Spratelloides_gracilis.xlsx")
View(Spratelloides_gracilis)
summary(Spratelloides_gracilis)
y <- c(Spratelloides_gracilis$Mass_g)
xS <- c(Spratelloides_gracilis$SL_cm)
xT <- c(Spratelloides_gracilis$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.005308
b <- 3.216338
summary(nls1)
summary(nls1)$coeffdetermination

# Plot S. gracilis ----
ggplot(Spratelloides_gracilis, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.005308*(x^(3.216338))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. gracilis")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Spratelloides gracilis")
fb_a <- c(length_weight("Spratelloides gracilis"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

ggplot(Spratelloides_gracilis, aes(x=SL_cm, y=Mass_g))+
  
  
  # INCOMPLETE ---- 
