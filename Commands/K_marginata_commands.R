# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)
library(patchwork)

# K. marginata Dataset ----
Kuhlia_marginata <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Kuhlia_marginata.xlsx")
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

log_a <- log10(fb_a$a)
color <- c("black","black")
data.frame(fb_a$b, log_a, color)
comp2 <- data.frame(fb_a$b, log_a, color)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a), color=color)+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of K. marginata")+
  xlab("b")+
  ylab("log10a")
  
  
# Annotated Graph ---- 
ggplot(Kuhlia_marginata, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.012192*(x^(3.212346))), se = TRUE)+
  geom_segment(aes(x = 15, xend = 4, y = 17, yend = 17), color = "red")+
  annotate("text" , label="y ~ 0.012192x^(3.212346)  RSE ~ 0.4723", x=6, y=11)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of K. marginata")+
  xlab("SL_cm")+
  ylab("Mass_g")
