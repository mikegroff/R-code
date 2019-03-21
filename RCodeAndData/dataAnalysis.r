D <- read.csv("wg.csv",header=TRUE)

# Summary Statistics
mean(D$wg)
mean(D$wg, na.rm=TRUE)
median(D$wg,na.rm=TRUE)
quantile(D$wg,c(.25,.5,.75),na.rm=TRUE)
summary(D$wg)
range(D$wg,na.rm=TRUE)
IQR(D$wg,na.rm=TRUE)
sd(D$wg,na.rm=TRUE)

# Testing
# One Sample Test
t.test(D$wg, alternative="greater", mu=10, conf.level=0.95)

# Two Sample Testsetwd
wg.M <- D[D$Gender =="M",]
wg.F <- D[D$Gender =="F",]
t.test(wg.M$wg, wg.F$wg, alternative="two.sided", mu=0, conf.level = 0.95)

# Paired T Test
Pt <- read.csv("Pairedt.csv",header=TRUE)
t.test(Pt$A,Pt$B, alternative="two.sided", mu=0, paired = TRUE, conf.level = 0.95)

# Simple Linear Regression
cherry <- read.csv("cherry.csv",header=TRUE)
plot(cherry$Height,cherry$Volume)


lm(Volume ~ Height, data = cherry)

cherry.lm <- lm(Volume ~ Height,data = cherry)
summary(cherry.lm)

par(mfrow=c(2,2))
plot(cherry.lm)

names(cherry.lm)
names(summary(cherry.lm))

par(mfrow=c(1,1))
plot(cherry$Height,cherry$Volume)
abline(cherry.lm)

predict.lm(cherry.lm, interval="confidence")

# Multiple Linear Regression
pairs(cherry)
cherry.lm <- lm(Volume ~ Height + Diam, data=cherry)

summary(cherry.lm)
anova(cherry.lm)
cherry.lm <- lm(Volume ~ Diam + Height, data=cherry)
anova(cherry.lm)
extractAIC(cherry.lm)
step(cherry.lm)
confint(cherry.lm, level = 0.97)

# ANOVA and Kruskal Wallis
do2 <- read.csv("do2.csv",header=TRUE)
do2.aov <- aov(DO2 ~ Stream, data=do2)
summary(do2.aov)

do2.lm <- lm(DO2 ~ Stream, data=do2)
anova(do2.lm)

TukeyHSD(do2.aov)
do2.HSD <- TukeyHSD(do2.aov)
plot(do2.HSD)

do2.HSD <- TukeyHSD(do2.aov, conf.level = 0.97)
plot(do2.HSD)

kruskal.test(DO2 ~ Stream, data=do2)

cherry.lo <- loess(Volume ~ Diam, data=cherry)
cherry.lo$fitted

cherry.plo <- predict(cherry.lo, cherry$Diam)
plot(cherry$Diam,cherry$Volume)
lines(cherry$Diam,cherry.plo)
