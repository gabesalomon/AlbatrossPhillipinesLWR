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

# H. temminckii Dataset ----
Hypoatherina_temminckii <- read_excel("AlbatrossPhillipinesLWR/Albatross_Raw_Data/Hypoatherina_temminckii.xlsx")
View(Hypoatherina_temminckii)
summary(Hypoatherina_temminckii)
y <- c(Hypoatherina_temminckii$Mass_g)
xS <- c(Hypoatherina_temminckii$SL_cm)
xT <- c(Hypoatherina_temminckii$TL_cm)

#Standard Error ----
sqrt(sum((y-mean(y))^2/(length(y)-1)))/sqrt(length(y))
sqrt(sum((xS-mean(xS))^2/(length(xS)-1)))/sqrt(length(xS))
sqrt(sum((xT-mean(xT))^2/(length(xT)-1)))/sqrt(length(xT))

# Desired eq.: Mass_g = a*SL_cm^b (SL) ----
nls1 <- nls(y ~ afit*xS^bfit, data.frame(xS , y), start = list(afit=1, bfit=1))
print(nls1)
yfit <- coef(nls1)[1]*xS^coef(nls1)[2]
lines(xS, yfit, col=2)
a <- 0.021097
b <- 2.648878
summary(nls1)

# Plot H. temminckii ----
ggplot(Hypoatherina_temminckii, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.021097*(x^(2.648878))), se = FALSE)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of H. temminckii")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Fishbase comparison ----
length_weight("Hypoatherina temminckii")
fb_a <- c(length_weight("Hypoatherina temminckii"))
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
  ggtitle("Length-Weight log10a vs b of H. temminckii")+
  xlab("b")+
  ylab("log10a")


# Annotated Graph ---- 
ggplot(Hypoatherina_temminckii, aes(x=SL_cm, y=Mass_g))+
  geom_point(aes(fill=))+
  geom_smooth(method = glm, formula = y ~ I(0.021097*(x^(2.648878))), se = FALSE)+
  annotate("text" , label="y ~ 0.021097x^(2.648878)  RSE ~ 0.5987", x=5, y=5)+
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("LWR of H. temminckii")+
  xlab("SL_cm")+
  ylab("Mass_g")

# Relative condition factor 
exp_weight <- ((a)*((xS)^(b)))
Kn <- (y)/(exp_weight)
rcf <- data.frame(xS, Kn)
avg_Kn <- mean(Kn)
avg_Kn
rKn <- (exp_weight)/((a)*(xS))
cf <- ((100)*((y)/(xS)^(3)))
avg_cf <- mean(cf)
avg_cf

ggplot(rcf, aes(x=xS, y=Kn))+
  geom_point(aes(fill=))+
  geom_smooth(method = lm)+
  annotate("text" , label="Average Kn = 0.9682088", x=5, y=1.5)+  
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Relative Condition Factor (Kn) of H. temminckii")+
  xlab("SL_cm")+
  ylab("Kn")
