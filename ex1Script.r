library(stats)
library(dplyr)
library(magrittr)

#1,2
df = read.csv('gia_nha.csv');
new_DF = df %>% select('price', 'sqft_living15', 'floors','condition','sqft_above','sqft_living');
new_DF = new_DF %>% filter(!(is.na(price) | is.na(sqft_living15) | is.na(sqft_living) | is.na(floors) | is.na(condition) | is.na(sqft_above)));
new_DF = new_DF %>% rename('log(price)'=price, 'log(sqft_living15)'=sqft_living15, 'log(sqft_living)'=sqft_living, 'log(sqft_above)'=sqft_above);

#3
#a)
Log = function(x) {log(x)};
new_DF = data.frame(new_DF %>% select('floors','condition'), apply(new_DF %>% select('log(price)', 'log(sqft_living15)','log(sqft_living)','log(sqft_above)'),2,Log));
print(new_DF);
#b)colMeans(new_DF[,c(3:6)]);
table = data.frame(Name = c('mean','median','sd','min','max'),
        log.price.= c(mean(new_DF[,'log.price.']), median(new_DF[,'log.price.']),sd(new_DF[,'log.price.']),min(new_DF[,'log.price.']),max(new_DF[,'log.price.'])),
        log.sqft_living15.= c(mean(new_DF[,'log.sqft_living15.']), median(new_DF[,'log.sqft_living15.']),sd(new_DF[,'log.sqft_living15.']),min(new_DF[,'log.sqft_living15.']),max(new_DF[,'log.sqft_living15.'])),
        log.sqft_living.= c(mean(new_DF[,'log.sqft_living.']), median(new_DF[,'log.sqft_living.']),sd(new_DF[,'log.sqft_living.']),min(new_DF[,'log.sqft_living.']),max(new_DF[,'log.sqft_living.'])),
        log.sqft_above.= c(mean(new_DF[,'log.sqft_above.']), median(new_DF[,'log.sqft_above.']),sd(new_DF[,'log.sqft_above.']),min(new_DF[,'log.sqft_above.']),max(new_DF[,'log.sqft_above.']))
)
#c)
ftable(new_DF[,'floors'])
ftable(new_DF[,'condition'])
#d)
#hist(new_DF[,'log.price.'],freq=FALSE, probability = TRUE)
P = ecdf(new_DF[,'log.price.']);
plot(P)

#e) Not sure 
boxplot(new_DF$log.price. ~ new_DF$floors, ylab = "Prices", xlab = "Prices by floors number")
boxplot(new_DF$log.price. ~ new_DF$condition, ylab = "Prices", xlab = " Prices by conditions")
#pairs(new_DF[,c('floors','log.price.')])
#pairs(new_DF[,c('condition','log.price.')])

#f)
pairs(new_DF[,c('log.sqft_living15.','log.price.')])
pairs(new_DF[,c('log.sqft_living.','log.price.')])
pairs(new_DF[,c('log.sqft_above.','log.price.')])

#4
#a)
lmPrice = lm(log.price.~log.sqft_living15.+log.sqft_living.+log.sqft_above.+floors+condition,new_DF)
summary(lmPrice)
#plot(new_DF)
#abline(lmPrice)

#b)
#Eliminate floors condition sign 0.1 1 (>0.05)

#lmPricy = lm(log.price.~condition,new_DF[,c('log.price.','condition')])
#summary(lmPricy)
#plot(new_DF[,c('condition','log.price.')])
#abline(lmPricy)

#c)
lmPriceC = lm(log.price.~log.sqft_living15.+log.sqft_living.+log.sqft_above.+floors,new_DF)
summary(lmPriceC)

#d)
lmPricy = lm(log.price.~condition,new_DF[,c('log.price.','condition')])
#summary(lmPricy)
anovaD = anova(lmPricy)
anovaD

lmPrici = lm(log.price.~log.sqft_living15.,new_DF[,c('log.price.','log.sqft_living15.')])
#summary(lmPrici)
anovai = anova(lmPrici)
anovai
#Assume price not linearly dependent with condition M2 better

#e)
resi = lmPriceC$residuals
fitted = lmPriceC$fitted.values
plot(fitted, resi)

#5)
#meanX = c(mean(new_DF[,'log.sqft_living15.']), mean(new_DF[,'log.sqft_living.']),mean(new_DF[,'log.sqft_above.']), new_DF$floors=2, new_DF$condition=3)
#maxX = c(max(new_DF[,'log.sqft_living15.']), max(new_DF[,'log.sqft_living.']),max(new_DF[,'log.sqft_above.']))

x1 = data.frame(log.sqft_living15. = mean(new_DF[,'log.sqft_living15.']), log.sqft_living. = mean(new_DF[,'log.sqft_living.']), log.sqft_above. = mean(new_DF[,'log.sqft_above.']), floors = 2, condition = 3)
x2 = data.frame(log.sqft_living15. = max(new_DF[,'log.sqft_living15.']), log.sqft_living. = max(new_DF[,'log.sqft_living.']), log.sqft_above. = max(new_DF[,'log.sqft_above.']), floors = 2, condition = 3)

predict(lmPrice, x1, interval = "confidence")
predict(lmPrice, x2, interval = "confidence")
