# Libraries ( Load only libraries if not first time) ----
install.packages("pacman")
library(pacman)
install.packages("rlang")
library(rlang)
install.packages("ggpubr")
library(ggpubr)
install.packages("rfishbase")
library(rfishbase)
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("nls2")
library(nls2)
install.packages("patchwork")
library(patchwork)

# P. bindus Dataset ----
Photopectoralis_bindus <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Photopectoralis_bindus.xlsx")
View(Photopectoralis_bindus)
summary(Photopectoralis_bindus)
y <- c(Photopectoralis_bindus$Mass_g)
xS <- c(Photopectoralis_bindus$SL_cm)
xT <- c(Photopectoralis_bindus$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.023412
b <- 2.980925
summary(nls1)
summary(nls1)$coeffdetermination

# Plot P. bindus ----
ggplot(Photopectoralis_bindus, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.023412*(x^(2.980925))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of P. bindus")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Photopectoralis bindus")
fb_a <- c(length_weight("Photopectoralis bindus"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

log_a <- log10(fb_a$a)
color <- c("black","black","green","black","black","black","black","black","black","black","black","black","black","black","black","black")
data.frame(fb_a$b, log_a, color)
comp2 <- data.frame(fb_a$b, log_a, color)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a), color=color)+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of P. bindus")+
  xlab("b")+
  ylab("log10a")
  
  
  # Annotated Graph ---- 
ggplot(Photopectoralis_bindus, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.023412*(x^(3.27441))), se = TRUE)+
  geom_segment(aes(x = 9.4, xend = 2.4, y = 8, yend = 8), color = "red")+
  annotate("text" , label="y ~ 0.023412x^(3.27441)  RSE ~ 0.1675", x=8, y=4)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of P. bindus")+
  xlab("SL_cm")+
  ylab("Mass_g")
