library(tidyverse)
library(lubridate)


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
summary(res)
