# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)

# G. giuris Dataset ----
Glossogobius_giuris <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Glossogobius_giuris.xlsx")
View(Glossogobius_giuris)
summary(Glossogobius_giuris)
y <- c(Glossogobius_giuris$Mass_g)
xS <- c(Glossogobius_giuris$SL_cm)
xT <- c(Glossogobius_giuris$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.008736
b <- 3.192777
summary(nls1)
summary(nls1)$coeffdetermination

# Plot G. giuris ----
ggplot(Glossogobius_giuris, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.008736*(x^(3.192777))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of G. giuris")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Glossogobius giuris")
fb_a <- c(length_weight("Glossogobius giuris"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

ggplot(Siganus_fuscescens, aes(x=SL_cm, y=Mass_g))+
  
  
# INCOMPLETE ---- 