library(stats)
library(dplyr)
library(magrittr)

#1,2
df = read.csv('gia_nha.csv')
new_DF = df %>% select('price', 'sqft_living15', 'floors','condition','sqft_above','sqft_living');
new_DF = new_DF %>% filter(!(is.na(price) | is.na(sqft_living15) | is.na(sqft_living) | is.na(floors) | is.na(condition) | is.na(sqft_above)));
new_DF = new_DF %>% rename('log(price)'=price, 'log(sqft_living15)'=sqft_living15, 'log(sqft_living)'=sqft_living, 'log(sqft_above)'=sqft_above);
#3
#a)
Log = function(x) {log(x)}
data.frame(new_DF %>% select('floors','condition'), apply(new_DF %>% select('log(price)', 'log(sqft_living15)','log(sqft_living)','log(sqft_above)'),2,Log))
print(new_DF);