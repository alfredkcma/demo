library(tidyverse)
library(lubridate)
library(lmtest)
library(sandwich)


dsp <- read_csv('../dat/sp500.csv', skip=3, col_select=1:5)
dasx <- read_csv('../dat/ASX.csv', skip=3, col_select=1:5)

dsp$Date <- as.Date(dsp$Date, "%d-%b-%Y")
dasx$Date <- as.Date(dasx$Date, "%d-%b-%Y")

dsp <- dsp %>% mutate(SPRet=log(Close/Open)) 
dasx <- dasx %>% mutate(ASXRet=log(Close/Open)) 

d <- inner_join(dsp %>% select(Date, SPRet), dasx %>% select(Date, ASXRet)) 

# some plotting to get the ideas
d %>% ggplot(aes(x=ASXRet, y=SPRet)) + geom_point() + geom_smooth(method=lm, formula = y ~ x, se=FALSE)

# how about adding month effect (why not day effect?)

d_month <- d %>% mutate(Month=month(Date)) 
res <- lm(SPRet ~ ASXRet + factor(Month), d_month)
print(summary(res))

# using robust standard error
res1 <- coeftest(res, vcov = vcovHC(res, type = 'HC0'))
print(res1)

# nonlinear test
res2 <- lm(SPRet ~ ASXRet + I(ASXRet^2) + I(ASXRet^3), d)
print(summary(res2))
d %>% ggplot(aes(x=ASXRet, y=SPRet)) + geom_point() + geom_smooth(method=lm, formula = y ~ x + I(x^2) + I(x^3), se=FALSE)

# let's assume we want to predict upward market in SP500
dsp <- dsp %>% mutate(SPUP=as.numeric(Close > Open))
d_binary <- inner_join(dsp %>% select(Date, SPUP), dasx %>% select(Date, ASXRet)) 

res3 <- lm(SPUP ~ ASXRet, d_binary)
summary(res3)
d_binary %>% ggplot(aes(x=ASXRet, y=SPUP)) + geom_point() + geom_smooth(method=lm)

# logistic regression
res4 <- glm(SPUP ~ ASXRet, data=d_binary, family=binomial(link='logit'))
summary(res4)

# probit 
res5 <- glm(SPUP ~ ASXRet, data=d_binary, family=binomial(link='probit'))
summary(res5)

