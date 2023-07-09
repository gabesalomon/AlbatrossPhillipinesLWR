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

# S. fuscescens Dataset ----
Siganus_fuscescens <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Siganus_fuscescens.xlsx")
View(Siganus_fuscescens)
summary(Siganus_fuscescens)
y <- c(Siganus_fuscescens$Mass_g)
xS <- c(Siganus_fuscescens$SL_cm)
xT <- c(Siganus_fuscescens$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.018735
b <- 3.022117
summary(nls1)
summary(nls1)$coeffdetermination

# Plot S. fuscescens ----
ggplot(Siganus_fuscescens, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.018735*(x^(3.022117))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. fuscescens")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Siganus fuscescens")
fb_a <- c(length_weight("Siganus fuscescens"))
compar1 <- data.frame(var0 = c(a,b), var1 = c(fb_a$a,fb_a$b))

log_a <- log10(fb_a$a)
color <- c("black","black","orange","orange")
data.frame(fb_a$b, log_a, color)
comp2 <- data.frame(fb_a$b, log_a, color)

coll_log_a <- log10(a)
collected1 <- data.frame(b, coll_log_a)

ggplot()+
  geom_point(comp2, mapping=aes(x=fb_a.b, y=log_a), color=color)+
  geom_point(collected1, mapping=aes(x=b, y=coll_log_a), color="red")+
  geom_smooth(comp2, method = lm, mapping=aes(x=fb_a.b, y=log_a), se = FALSE, color="blue")+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Length-Weight log10a vs b of S. fuscescens")+
  xlab("b")+
  ylab("log10a")


  # Annotated Graph ---- 
ggplot(Siganus_fuscescens, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.018735*(x^(3.022117))), se = TRUE)+
  geom_segment(aes(x = 20, xend = 3, y = 10, yend = 10), color = "red")+
  annotate("text" , label="y ~ 0.018735x^(3.022117)  RSE ~ 0.212", x=10, y=4)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of S. fuscescens")+
  xlab("SL_cm")+
  ylab("Mass_g")
  
