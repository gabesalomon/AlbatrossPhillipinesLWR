# Libraries ( Load only libraries if not first time) ----
install.packages("pacman")
install.packages("rlang")
install.packages("ggpubr")
install.packages("rfishbase")
install.packages("readxl")
install.packages("ggplot2")
install.packages("nls2")
install.packages("patchwork")
library(pacman)
library(rlang)
library(ggpubr)
library(rfishbase)
library(readxl)
library(ggplot2)
library(nls2)
library(patchwork)

# G. oyena Dataset ----
Gerres_oyena_fresh <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Fresh_LWR_Data/Gerres_oyena_fresh.xlsx")
View(Gerres_oyena_fresh)
summary(Gerres_oyena_fresh)
y <- c(Gerres_oyena_fresh$Mass_g)
xS <- c(Gerres_oyena_fresh$SL_cm)
xT <- c(Gerres_oyena_fresh$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.032333
b <- 2.885629
summary(nls1)
r2 <- summary(nls1)$coeffdetermination

# Plot G. oyena ----
ggplot(Gerres_oyena_fresh, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.032333*(x^(2.885629))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of Fresh G. oyena")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Gerres oyena")
fb_a <- c(length_weight("Gerres oyena"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

log_a <- log10(fb_a$a)
color <- c("black","black","green","green")
data.frame(fb_a$b, log_a, color)
comp2 <- data.frame(fb_a$b, log_a, color)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a), color=color)+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of Fresh G. oyena")+
  xlab("b")+
  ylab("log10a")


# Annotated Graph ---- 
ggplot(Gerres_oyena_fresh, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.032333*(x^(2.885629))), se = TRUE)+
  geom_segment(aes(x = 19, xend = 4, y = 2, yend = 2), color = "red")+
  annotate("text" , label="y ~ 0.032333x^(2.885629)  RSE ~ 0.9767", x=8, y=30)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of Fresh G. oyena")+
  xlab("SL_cm")+
  ylab("Mass_g")
