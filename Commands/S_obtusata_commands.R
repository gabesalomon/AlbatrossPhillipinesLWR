# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)
library(patchwork)
library(dplyr)

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

log_a <- log10(fb_a$a)
color <- c("black","green","black","black","black","black","black","black","black","green")
data.frame(fb_a$b, log_a, color)
comp2 <- data.frame(fb_a$b, log_a, color)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a), color=color)+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of S. obtusata")+
  xlab("b")+
  ylab("log10a")

fb_s_obtusata <- length_weight("Sphyraena obtusata") 
filtered_data <- fb_s_obtusata %>% filter(fb_s_obtusata$EsQ != "yes") 
  
# Annotated Graph ---- 
ggplot(Sphyraena_obtusata, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.0024577*(x^(3.3447781))), se = TRUE)+
  geom_segment(aes(x = 26, xend = 13, y = 8, yend = 8), color = "red")+
  annotate("text" , label="y ~ 0.0024577x^(3.3447781)  R2 ~ ", x=10, y=5)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. obtusata")+
  xlab("SL_cm")+
  ylab("Mass_g")

