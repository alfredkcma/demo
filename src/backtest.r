library(tidyverse)
library(slider)

d <- read_csv('../dat/topix.csv', skip=3, col_select=1:5)

#change the date format
d$Date <- as.Date(d$Date, "%d-%b-%Y")

d <- d %>% arrange(Date)

days <- 50
beta <- 0.01

d <- d %>% mutate(MaxRolling=slide_max(Close, before=days, after=-1, complete=TRUE)) %>% 
    mutate(MinRolling=slide_min(Close, before=days, after=-1, complete=TRUE)) %>%
    mutate(Signal=(Close > (1+beta) * MaxRolling) - (Close < (1+beta) * MinRolling)) %>%
    mutate(Ret=log(lead(Close)/Close))


