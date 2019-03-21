D <- read.csv("wg.csv",header=TRUE)

hist(D$wg)
hist(D$wg , main='Weight Gain')
hist(D$wg , main='Weight Gain',xlab='Weight Gain', ylab ='Frequency')
hist(D$wg, main='Weight Gain',xlab='Weight Gain', ylab ='Frequency', col='blue')
par(mfrow=c(1,1))
hist(D$wg, main='Red',xlab='Weight Gain', ylab ='Frequency', col='red')
hist(D$wg, main='Grey',xlab='Weight Gain', ylab ='Frequency', col='grey')
hist(D$wg, main='Green',xlab='Weight Gain', ylab ='Frequency', col='green')
hist(D$wg, main='Heat',xlab='Weight Gain', ylab ='Frequency', col=heat.colors(14))
boxplot(D$wg)
boxplot(D$wg,main='Weight Gain',ylab='Weight Gain (lbs)')

wg.m <- D[D$Gender=="M",]
wg.f <- D[D$Gender=="F",]
boxplot(wg.m$wg,wg.f$wg)

boxplot(wg.m$wg, wg.f$wg, main='Weight Gain', ylab='Weight Gain (lbs)', 
names = c('Male','Female'))

levels(D$Shift)

wg.7a <- D[D$Shift=="7am",]
wg.8a <- D[D$Shift=="8am",]
wg.9a <- D[D$Shift=="9am",]
wg.10a <- D[D$Shift=="10am",]
wg.11a <- D[D$Shift=="11am",]
wg.12p <- D[D$Shift=="12pm",]

boxplot(wg.7a$wg, wg.8a$wg, wg.9a$wg, wg.10a$wg, wg.11a$wg, wg.12p$wg,  main='Weight Gain', ylab='Weight Gain (lbs)',
xlab='Shift', names = c('7am','8am','9am','10am','11am','12pm'))

plot(D$metmin,D$wg)
plot(D$metmin,D$wg,main='Met Minutes vs. Weight Gain', xlab='Mets (min)',ylab='Weight Gain (lbs)')
plot(D$metmin,D$wg,main='Met Minutes vs. Weight Gain', xlab='Mets (min)',ylab='Weight Gain (lbs)',pch=2)


D2 <- read.csv("Dell.csv",header=TRUE)
t1 <- 1:nrow(D2)
plot(t1,D2$DELL)
plot(t1,D2$DELL,type="l")
plot(t1,D2$DELL,type="l",main='Dell Closing Stock Price',xlab='Time',ylab='Price $')
lines(t1,D2$Intel,lty=2)

plot(t1,D2$Intel,type="l",main='Closing Stock Prices',xlab='Time',ylab='Price $')
lines(t1,D2$DELL,lty=2)

par(mfrow=c(1,1))
plot(t1,D2$Intel,type="l",main='Closing Stock Prices',xlab='Time',ylab='Price $')
lines(t1,D2$DELL,lty=2)
legend(60,45,c('Intel','Dell'),lty=c(1,2))

par(mfrow=c(2,2))
hist(D$wg, main='Histogram',xlab='Weight Gain', ylab ='Frequency', col=heat.colors(14))
boxplot(wg.7a$wg, wg.8a$wg, wg.9a$wg, wg.10a$wg, wg.11a$wg, wg.12p$wg,  main='Weight Gain', ylab='Weight Gain (lbs)',
xlab='Shift', names = c('7am','8am','9am','10am','11am','12pm'))
plot(D$metmin,D$wg,main='Met Minutes vs. Weight Gain', xlab='Mets (min)',ylab='Weight Gain (lbs)',pch=2)
plot(t1,D2$Intel,type="l",main='Closing Stock Prices',xlab='Time',ylab='Price $')
lines(t1,D2$DELL,lty=2)
