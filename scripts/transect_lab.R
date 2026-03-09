# packages
library(tidyverse)
library(lmerTest)

# read
forest <- read_csv("data_raw/transectdata.csv")
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

# randomized block anova
m1<-lmer(est~method+(1|group_id), data=sumdata)
anova(m1)
m2<-lmer(time~method+(1|group_id), data=sumdata)
anova(m2)
sumdata$ci_width <- sumdata$upper - sumdata$lower # calculate precision
m3<-lmer(ci_width~method+(1|group_id), data=sumdata)
anova(m3)

# check residuals
par(mfrow=c(1,1))
qqnorm(residuals(m1))
qqline(residuals(m1))
qqnorm(residuals(m2))
qqline(residuals(m2))
qqnorm(residuals(m3))
qqline(residuals(m3))

# plot
sumdata_plot <- sumdata %>%
  mutate(
    group_id = factor(group_id),
    method = factor(
      method,
      levels = c("tsquare", "ordered_distance", "variable_area"),
      labels = c("T-square", "Ordered distance", "Variable area")
    ),
    interval = upper - lower
  ) %>%
  filter(is.finite(est), is.finite(time), is.finite(interval),
         est > 0, time > 0, interval > 0)

# long format
sumdata_long <- sumdata_plot %>%
  select(group_id, method, est, time, interval) %>%
  pivot_longer(cols = c(est, time, interval),
               names_to = "metric", values_to = "value") %>%
  mutate(
    metric = case_when(
      metric == "est"      ~ "'Estimated density ('*trees~m^{-2}*')'",
      metric == "interval" ~ "'95% CI width ('*trees~m^{-2}*')'",
      metric == "time"     ~ "'Time (min)'"
    ),
    metric = factor(metric, levels = c(
      "'95% CI width ('*trees~m^{-2}*')'",
      "'Estimated density ('*trees~m^{-2}*')'",
      "'Time (min)'"
    ))
  )

# LiDAR  
lidar_lines <- tibble(
  metric = "'Estimated density ('*trees~m^{-2}*')'",
  y = c(0.0055, 0.0061),
  ref = "LiDAR estimate")

# plot
p_final <- ggplot(sumdata_long, 
                  aes(x = method, 
                      y = value)) +
  geom_point(aes(colour = group_id), size = 2) +
  geom_hline(
    data = lidar_lines,
    aes(yintercept = y, 
        linetype = ref),
    linewidth = 0.6,
    colour = "black",
    inherit.aes = FALSE
  ) +
  scale_linetype_manual(values = c("LiDAR estimate" = "dashed")) +
  facet_wrap(~ metric, 
             scales = "free_y", 
             nrow = 1, 
             labeller = label_parsed) +
  theme_bw() +
  theme(legend.position = "right") +
  guides(
    colour = guide_legend(order = 1),
    linetype = guide_legend(order = 2)) +
  labs(
    x = "Method (sampling design)",
    y = "Response value",
    colour = "Group",
    linetype = "Reference"
  ) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1))

p_final

