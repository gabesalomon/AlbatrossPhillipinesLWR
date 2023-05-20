# Libraries ----
library(pacman)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)
library(patchwork)

# A. interrupta Dataset ----
Ambassis_interrupta <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Ambassis_interrupta.xlsx")
View(Ambassis_interrupta)
summary(Ambassis_interrupta)
y <- c(Ambassis_interrupta$Mass_g)
xS <- c(Ambassis_interrupta$SL_cm)
xT <- c(Ambassis_interrupta$TL_cm)

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

# Plot A. interrupta ----
ggplot(Ambassis_interrupta, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.031066*(x^(2.92208))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of A. interrupta")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Ambassis interrupta")
fb_a <- c(length_weight("Ambassis interrupta"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

log_a <- log10(fb_a$a)
data.frame(fb_a$b, log_a)
comp2 <- data.frame(fb_a$b, log_a)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a))+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(method = lm, se = TRUE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of A. interrupta")+
  xlab("b")+
  ylab("log10a")


# Annotated Graph ---- 
ggplot(Ambassis_interrupta, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.031066*(x^(2.92208))), se = TRUE)+
  geom_segment(aes(x = 12, xend = 5, y = 10, yend = 10), color = "red")+
  annotate("text" , label="y ~ 0.031066x^(2.92208)  R2 ~ ", x=4, y=8)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of A. interrupta")+
  xlab("SL_cm")+
  ylab("Mass_g")
