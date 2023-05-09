# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)

# A. interruptus Dataset ----
Ambassis_interruptus <- read_excel("C:/Users/gabes/Desktop/Albatross_Raw_Data/Ambassis_interruptus.xlsx")
View(Ambassis_interruptus)
summary(Ambassis_interruptus)
y <- c(Ambassis_interruptus$Mass_g)
xS <- c(Ambassis_interruptus$SL_cm)
xT <- c(Ambassis_interruptus$TL_cm)

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
b <- 2.92208
summary(nls1)
summary(nls1)$coeffdetermination

# Plot A. interruptus ----
ggplot(Ambassis_interruptus, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.031066*(x^(2.92208))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of A. interruptus")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Ambassis interruptus")
fb_a <- c(length_weight("Ambassis interruptus"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

ggplot(Ambassis_interruptus, aes(x=SL_cm, y=Mass_g))+
  
  
  # INCOMPLETE ---- 
