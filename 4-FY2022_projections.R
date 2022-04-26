library(tidyverse)
library(forecast)

# ====================
#read in quarterly data
dat <- read.csv("data/quarterly_natz.csv")
dat$t <- 1:nrow(dat)
mod <- lm(Applications.Approved ~ t + as.factor(Quarter), dat)

# predict onto 2022 (quarters 2,3,4)
to_predict<- data.frame(Year = c(2022, 2022, 2022), Quarter = c(2,3,4), t = c(26,27,28))
pred_y <- predict(mod, newdata = to_predict)

# create new dataframe with existing and predicted naturalizations from 2017 to 2022
to_predict$Applications.Approved <- pred_y
newly_natz <- rbind(dat, to_predict)

# plot trend
plot(ts(newly_natz$Applications.Approved, start = 2017, frequency=4))

# number of newly naturalized citizens from 2016
sum(newly_natz$Applications.Approved)

# number of newly naturalized citizens in FY 2022
sum(newly_natz %>% subset(Year == 2022) %>% select(Applications.Approved))

# =======================
#read in quarterly data
dat <- read.csv("data/quarterly_natz_ohio.csv")
dat$t <- 1:nrow(dat)
mod <- lm(Applications.Approved ~ t + as.factor(Quarter), dat)

# predict onto 2022 (quarters 2,3,4)
to_predict<- data.frame(Year = c(2022, 2022, 2022), Quarter = c(2,3,4), t = c(26,27,28))
pred_y <- predict(mod, newdata = to_predict)

# create new dataframe with existing and predicted naturalizations from 2017 to 2022
to_predict$Applications.Approved <- pred_y
newly_natz <- rbind(dat, to_predict)

# plot trend
plot(ts(newly_natz$Applications.Approved, start = 2017, frequency=4))

# number of newly naturalized citizens from 2016
sum(newly_natz$Applications.Approved, na.rm = T)

# number of newly naturalized citizens in FY 2022
sum(newly_natz %>% subset(Year == 2022) %>% select(Applications.Approved))
