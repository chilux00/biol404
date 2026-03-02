# packages
library(tidyverse)

# read
forest <- read_csv("data_raw/transectmockup.csv")
forest$method <- as.factor(forest$method)
forest$group_id <- as.factor(forest$group_id)
forest$x <- as.numeric(forest$x)
forest$z <- as.numeric(forest$z)

# create separate mini dtf
tsq <- filter(forest, method=="tsquare")
ord <- filter(forest, method=="ordered_distance")
vari <- filter(forest, method=="variable_area")

# tsqaure
tsq.magic <- tsq %>%
  mutate(areaz = (pi*z*z),
         areax = (pi*x*x),
         areaxz = (areaz+areax)/1.5)

# intermediates
tsq.magic <- tsq %>%
  mutate(areaz = (pi*z*z),
         areax = (pi*x*x),
         areaxz = (areaz+areax)/1.5) %>%
  group_by(group_id) %>%
  summarise(method = first(method),
            meanx=mean(x, na.rm=TRUE),
            sumx=sum(x, na.rm=TRUE),
            varx=var(x, na.rm=TRUE),
            time=first(time),
            number =sum(x>=0, na.rm=TRUE),
            meanareaxz = mean(areaxz, na.rm=TRUE),
            sdareaxz = sd(areaxz, na.rm=TRUE),
            sumz=sum(z, na.rm=TRUE),
            se = sd(areaxz)/sqrt(number),
            est = number*number/(2*sumx*sqrt(2)*sumz),
            lower = 1/(0.5*(meanareaxz+1.96*se)),
            upper = 1/(0.5*(meanareaxz-1.96*se)),
            )

# summary stats
ord.magic <- ord %>%
  group_by(group_id) %>%
  summarise(meanx=mean(x, na.rm=TRUE),
            sumsqx=sum(x^2, na.rm=TRUE),
            sumx=sum(x, na.rm=TRUE),
            varx=var(x, na.rm=TRUE),
            method=first(method),
            time=first(time),
            number=sum(x>=0, na.rm=TRUE))

# density estimator
ord.magic$est <- (3*ord.magic$number-1)/(pi*ord.magic$sumsqx)
ord.magic$se <- sqrt((ord.magic$est^2)/(3*6-2)) 
ord.magic$lower <- (((12*6-1)^0.5-1.96)/(4*pi*ord.magic$sumsqx)^0.5)^2
ord.magic$upper <- (((12*6-1)^0.5+1.96)/(4*pi*ord.magic$sumsqx)^0.5)^2

# variable area intermediates
vari.magic <- vari %>%
  group_by(group_id) %>%
  summarise(meanx=mean(x, na.rm=TRUE),
            sumsqx=sum(x^2, na.rm=TRUE),
            sumx=sum(x, na.rm=TRUE),
            varx=var(x, na.rm=TRUE),
            method=first(method),
            time=first(time),
            number=sum(x>=0, na.rm=TRUE))

vari.magic$est <- (3*vari.magic$number-1)/(6*vari.magic$sumx)
vari.magic$se <- sqrt((vari.magic$est^2)/(3*6-2))

C1<-qchisq(p=1-0.975,df=6*6)
C2<-qchisq(p=1-0.025,df=6*6)
vari.magic$lower<-C1/(2*6*vari.magic$sumx)
vari.magic$upper<-C2/(2*6*vari.magic$sumx)

# join dataframes
sumdata<-tsq.magic %>%
  full_join(ord.magic) %>%
  full_join(vari.magic) %>%
  mutate(interval=upper-lower)
