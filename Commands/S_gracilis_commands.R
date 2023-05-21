# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)
library(patchwork)

# S. gracilis Dataset ----
Spratelloides_gracilis <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Spratelloides_gracilis.xlsx")
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
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start=list(afit=.5, bfit=.5))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.0059858
b <- 3.1470502
summary(nls1)
summary(nls1)$coeffdetermination

# Plot S. gracilis ----
ggplot(Spratelloides_gracilis, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0059858*(x^(3.1470502))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. gracilis")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Spratelloides gracilis")
fb_a <- c(length_weight("Spratelloides gracilis"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

log_a <- log10(fb_a$a)
data.frame(fb_a$b, log_a,)
comp2 <- data.frame(fb_a$b, log_a,)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a))+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of S. gracilis")+
  xlab("b")+
  ylab("log10a")
  
  
  # Annotated Graph ---- 
ggplot(Spratelloides_gracilis, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0059858*(x^(3.1470502))), se = TRUE)+
  geom_segment(aes(x = 10, xend = 3, y = 2, yend = 2), color = "red")+
  annotate("text" , label="y ~ 0.0059858x^(3.1470502)  R2 ~ ", x=3.5, y=1.5)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. gracilis")+
  xlab("SL_cm")+
  ylab("Mass_g")
