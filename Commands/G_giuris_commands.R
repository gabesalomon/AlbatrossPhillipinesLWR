# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)
library(patchwork)

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

log_a <- log10(fb_a$a)
color <- c("black","black","black","green","black","black","green","green","black","black","black","black","black","black","black","black","black","black","black","black")
data.frame(fb_a$b, log_a, color)
comp2 <- data.frame(fb_a$b, log_a, color)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a), color=color)+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of G. giuris")+
  xlab("b")+
  ylab("log10a")
  
  
# Annotated Graph ---- 
ggplot(Glossogobius_giuris, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.008736*(x^(3.192777))), se = TRUE)+
  geom_segment(aes(x = 17, xend = 2.8, y = 11, yend = 11), color = "red")+
  annotate("text" , label="y ~ 0.008736x^(3.192777)  R2 ~ ", x=5, y=10)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of G. giuris")+
  xlab("SL_cm")+
  ylab("Mass_g")
