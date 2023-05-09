# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)

# K. marginata Dataset ----
Kuhlia_marginata <- read_excel("C:/Users/gabes/Desktop/Albatross_Raw_Data/Kuhlia_marginata.xlsx")
View(Kuhlia_marginata)
summary(Kuhlia_marginata)
y <- c(Kuhlia_marginata$Mass_g)
xS <- c(Kuhlia_marginata$SL_cm)
xT <- c(Kuhlia_marginata$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.012192
b <- 3.212346
summary(nls1)
summary(nls1)$coeffdetermination

# Plot K. marginata ----
ggplot(Kuhlia_marginata, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.012192*(x^(3.212346))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of K. marginata")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Kuhlia marginata")
fb_a <- c(length_weight("Kuhlia marginata"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

ggplot(Kuhlia_marginata, aes(x=SL_cm, y=Mass_g))+
  
  
  # INCOMPLETE ---- 
