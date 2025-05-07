#  OFFICE TENURE IN MEDIEVAL HUNGARY ------------------
#    R script by Davor Salihovic, University of Antwerp                                                 
#                      davor.salihovic@uantwerpen.be 

rm(list = ls())

library(dplyr); library(tidyr); library(lubridate); library(data.table)
library(survival); library(eha); library(survminer); library(bshazard)
library(ggplot2); library(brms); library(rstanarm); library(posterior); library(performance); 
library(pastecs); library(car)

load("data/data.RData")

# 1. ANALYSIS ----

## 1.1 Plots of survival and hazard functions ----

### 1.1.1 Pre- and post-1382 hazards ----

# Tenure metric
m82 <- stan_surv(Surv(tenure, status) ~ 1, 
                 basehaz = "ms",
                 cores = 8, chains = 4,
                 prior = normal(0, 2),
                 prior_intercept = normal(0, 2),
                 iter = 4000, warmup = 1000,
                 data = d1[d1$angevine_regime == 1,],
                 seed = 123)
m83 <- stan_surv(Surv(tenure, status) ~ 1, 
                 basehaz = "ms",
                 cores = 8, chains = 4,
                 prior = normal(0, 2),
                 prior_intercept = normal(0, 2),
                 iter = 4000, warmup = 1000,
                 data = d1[d1$angevine_regime == 0,],
                 seed = 123)
pm82 <- plot(m82)$data
pm83 <- plot(m83)$data

# Monarchs' reign metric
km82 <- stan_surv(Surv(time0, time1, status) ~ 1, 
                  basehaz = "ms",
                  cores = 8, chains = 4,
                  prior = normal(0, 2),
                  prior_intercept = normal(0, 2),
                  iter = 4000, warmup = 1000,
                  data = d1[d1$angevine_regime == 1,],
                  seed = 123)
km83 <- stan_surv(Surv(time0, time1, status) ~ 1, 
                  basehaz = "ms",
                  cores = 8, chains = 4,
                  prior = normal(0, 2),
                  prior_intercept = normal(0, 2),
                  iter = 4000, warmup = 1000,
                  data = d1[d1$angevine_regime == 0,],
                  seed = 123)
pkm82 <- plot(km82)$data
pkm83 <- plot(km83)$data

hzp8 <- rbind(
  data.frame(time = pm82$times, haz = pm82$med, l = pm82$lb, u = pm82$ub, years = pm82$times / 365.25, period = "Old"),
  data.frame(time = pm83$times, haz = pm83$med, l = pm83$lb, u = pm83$ub, years = pm83$times / 365.25, period = "New")
)

hzk8 <- rbind(
  data.frame(time = pkm82$times, haz = pkm82$med, l = pkm82$lb, u = pkm82$ub, years = pkm82$times / 365.25, period = "Old"),
  data.frame(time = pkm83$times, haz = pkm83$med, l = pkm83$lb, u = pkm83$ub, years = pkm83$times / 365.25, period = "New")
)

### 1.1.2 Pre- and post-1382 survival ----
mdl <- Surv(tenure, status) ~ 1
mdlk <- Surv(time0, time1, status) ~ 1

# Tenure metric
m82p <- survfit(mdl, d1[d1$angevine_regime == 1,])
m83p <- survfit(mdl, d1[d1$angevine_regime == 0,])

kmp8 <- rbind(
  data.frame(time = m82p$time, surv = m82p$surv, l = m82p$lower, u = m82p$upper, years = m82p$time / 365.25, period = "Old"),
  data.frame(time = m83p$time, surv = m83p$surv, l = m83p$lower, u = m83p$upper, years = m83p$time / 365.25, period = "New")
)

# Monarchs' reign metric
m82pk <- survfit(mdlk, d1[d1$angevine_regime == 1,])
m83pk <- survfit(mdlk, d1[d1$angevine_regime == 0,])

kmp8k <- rbind(
  data.frame(time = m82pk$time, surv = m82pk$surv, l = m82pk$lower, u = m82pk$upper, years = m82pk$time / 365.25, period = "Old"),
  data.frame(time = m83pk$time, surv = m83pk$surv, l = m83pk$lower, u = m83pk$upper, years = m83pk$time / 365.25, period = "New")
)

### 1.1.3 Pre- and post-1439 hazards ----

# Tenure metric
m39 <- stan_surv(Surv(tenure, status) ~ 1, 
                 basehaz = "ms",
                 cores = 8, chains = 4,
                 prior = normal(0, 2),
                 prior_intercept = normal(0, 2),
                 iter = 4000, warmup = 1000,
                 data = d1[d1$old_regime == 1,],
                 seed = 123)
m40 <- stan_surv(Surv(tenure, status) ~ 1, 
                 basehaz = "ms",
                 cores = 8, chains = 4,
                 prior = normal(0, 2),
                 prior_intercept = normal(0, 2),
                 iter = 4000, warmup = 1000,
                 data = d1[d1$old_regime == 0,],
                 seed = 123)
pm39 <- plot(m39)$data
pm40 <- plot(m40)$data

# Monarchs' reign metric
km39 <- stan_surv(Surv(time0, time1, status) ~ 1, 
                  basehaz = "ms",
                  cores = 8, chains = 4,
                  prior = normal(0, 2),
                  prior_intercept = normal(0, 2),
                  iter = 4000, warmup = 1000,
                  data = d1[d1$old_regime == 1,],
                  seed = 123)
km40 <- stan_surv(Surv(time0, time1, status) ~ 1, 
                  basehaz = "ms",
                  cores = 8, chains = 4,
                  prior = normal(0, 2),
                  prior_intercept = normal(0, 2),
                  iter = 4000, warmup = 1000,
                  data = d1[d1$old_regime == 0,],
                  seed = 123)
pkm39 <- plot(km39)$data
pkm40 <- plot(km40)$data

hzp3 <- rbind(
  data.frame(time = pm39$times, haz = pm39$med, l = pm39$lb, u = pm39$ub, years = pm39$times / 365.25, period = "Old"),
  data.frame(time = pm40$times, haz = pm40$med, l = pm40$lb, u = pm40$ub, years = pm40$times / 365.25, period = "New")
)

hzk3 <- rbind(
  data.frame(time = pkm39$times, haz = pkm39$med, l = pkm39$lb, u = pkm39$ub, years = pkm39$times / 365.25, period = "Old"),
  data.frame(time = pkm40$times, haz = pkm40$med, l = pkm40$lb, u = pkm40$ub, years = pkm40$times / 365.25, period = "New")
)

### 1.1.4 Pre- and post-1439 survival ----

# Tenure metric
m39p <- survfit(mdl, d1[d1$old_regime == 1,])
m40p <- survfit(mdl, d1[d1$old_regime == 0,])

kmp3 <- rbind(
  data.frame(time = m39p$time, surv = m39p$surv, l = m39p$lower, u = m39p$upper, years = m39p$time / 365.25, period = "Old"),
  data.frame(time = m40p$time, surv = m40p$surv, l = m40p$lower, u = m40p$upper, years = m40p$time / 365.25, period = "New")
)

# Monarchs' reign metric
m39pk <- survfit(mdlk, d1[d1$old_regime == 1,])
m40pk <- survfit(mdlk, d1[d1$old_regime == 0,])

kmp3k <- rbind(
  data.frame(time = m39pk$time, surv = m39pk$surv, l = m39pk$lower, u = m39pk$upper, years = m39pk$time / 365.25, period = "Old"),
  data.frame(time = m40pk$time, surv = m40pk$surv, l = m40pk$lower, u = m40pk$upper, years = m40pk$time / 365.25, period = "New")
)


### 1.1.5 Figures ----
p1 <- ggplot(kmp3k, aes(x = years, y = surv, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Old" = "grey40", "New" = "grey7"), 
                     labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  scale_fill_manual(values = c("Old" = "black", "New" = "black"), 
                    labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  scale_linetype_manual(values = c("Old" = 1, "New" = 2), 
                        labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  theme_classic() +
  ggtitle("King tenure metric") +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 14, hjust = .5)) +
  xlab("") +
  ylab("In office") +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_y_continuous(labels = label_scientific())

p2 <- ggplot(kmp3, aes(x = years, y = surv, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Old" = "grey40", "New" = "grey7"), 
                     labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  scale_fill_manual(values = c("Old" = "black", "New" = "black"), 
                    labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  scale_linetype_manual(values = c("Old" = 1, "New" = 2), 
                        labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  theme_classic() +
  ggtitle("Personal tenure metric") +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 14, hjust = .5)) +
  xlab("") +
  ylab("") +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_y_continuous(labels = label_scientific())

p3 <- ggplot(hzk3, aes(x = years, y = haz, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Old" = "grey40", "New" = "grey7"), 
                     labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  scale_fill_manual(values = c("Old" = "black", "New" = "black"), 
                    labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  scale_linetype_manual(values = c("Old" = 1, "New" = 2), 
                        labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  theme_classic() +
  ggtitle("") +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 14, hjust = .5)) +
  xlab("Year") +
  ylab("") +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_y_continuous(labels = label_scientific())

p4 <- ggplot(hzp3, aes(x = years, y = haz, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Old" = "grey40", "New" = "grey7"), 
                     labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  scale_fill_manual(values = c("Old" = "black", "New" = "black"), 
                    labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  scale_linetype_manual(values = c("Old" = 1, "New" = 2), 
                        labels = c("Old" = "Pre-1439 regime", "New" = "Post-1439 regime")) +
  theme_classic() +
  ggtitle("") +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 14, hjust = .5)) +
  xlab("Year") +
  ylab("Hazard") +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_y_continuous(labels = label_scientific())

fig1 <- ggarrange(p1, p2, p3, p4, labels = LETTERS[1:4], common.legend = T, legend = "bottom")

p5 <- ggplot(kmp8k, aes(x = years, y = surv, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Old" = "grey40", "New" = "grey7"), 
                     labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  scale_fill_manual(values = c("Old" = "black", "New" = "black"), 
                    labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  scale_linetype_manual(values = c("Old" = 1, "New" = 2), 
                        labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  theme_classic() +
  ggtitle("King tenure metric") +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 14, hjust = .5)) +
  xlab("") +
  ylab("In office") +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_y_continuous(labels = label_scientific())

p6 <- ggplot(kmp8, aes(x = years, y = surv, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Old" = "grey40", "New" = "grey7"), 
                     labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  scale_fill_manual(values = c("Old" = "black", "New" = "black"), 
                    labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  scale_linetype_manual(values = c("Old" = 1, "New" = 2), 
                        labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  theme_classic() +
  ggtitle("Personal tenure metric") +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 14, hjust = .5)) +
  xlab("") +
  ylab("") +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_y_continuous(labels = label_scientific())

p7 <- ggplot(hzk8, aes(x = years, y = haz, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Old" = "grey40", "New" = "grey7"), 
                     labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  scale_fill_manual(values = c("Old" = "black", "New" = "black"), 
                    labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  scale_linetype_manual(values = c("Old" = 1, "New" = 2), 
                        labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  theme_classic() +
  ggtitle("") +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 14, hjust = .5)) +
  xlab("Year") +
  ylab("") +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_y_continuous(labels = label_scientific())

p8 <- ggplot(hzp8, aes(x = years, y = haz, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Old" = "grey40", "New" = "grey7"), 
                     labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  scale_fill_manual(values = c("Old" = "black", "New" = "black"), 
                    labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  scale_linetype_manual(values = c("Old" = 1, "New" = 2), 
                        labels = c("Old" = "Pre-1382 regime", "New" = "Post-1382 regime")) +
  theme_classic() +
  ggtitle("") +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 14, hjust = .5)) +
  xlab("Year") +
  ylab("Hazard") +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_y_continuous(labels = label_scientific())

fig2 <- ggarrange(p5, p6, p7, p8, labels = c(LETTERS[1:4]), common.legend = T, legend = "bottom")

tiff("Figure1.tiff", w = 8.3, h = 8.3, units = "in", res = 600)
fig1
dev.off()

tiff("Figure2.tiff", w = 8.3, h = 8.3, units = "in", res = 600)
fig2
dev.off()


## 1.2 Models ----

### 1.2.1 Log-rank tests ----
survdiff(Surv(tenure, status) ~ old_regime, data = d1)
survdiff(Surv(tenure, status) ~ angevine_regime, data = d1)

### 1.2.2 Models 1-5 ----
m1 <- coxph(Surv(tenure, status) ~ old_regime, data = d1, ties = "efron")
m2 <- coxme(Surv(tenure, status) ~ old_regime + (1 | office), data = d1, ties = "efron")
m3 <- coxme(Surv(tenure, status) ~ old_regime + (1 | office) + (1 | p_id), data = d1, ties = "efron")
m4 <- coxme(Surv(tenure, status) ~ angevine_regime + (1 | office) + (1 | p_id), data = d1, ties = "efron")
m5 <- coxme(Surv(tenure, status) ~ regime + (1 | office) + (1 | p_id), data = d1, ties = "efron")

AIC(m1, m2, m3, m4, m5)
anova(m1, m2, m3)
anova(m1, m3)

# Check proportionality
models <- list(m3, m4, m5)
transform <- c("identity", "log", "km", "rank")
for (i in transform) {
  for (j in models) {
    print(cox.zph(j, transform = i))
  }
}

### 1.2.3 Models 6-9 ----
m6 <- coxph(Surv(time0, time1, status) ~ castles*change, data = d2, ties = "efron")
m7 <- coxme(Surv(time0, time1, status) ~ castles*change + (1 | office), data = d2, ties = "efron")
m8 <- coxme(Surv(time0, time1, status) ~ castles*change + (1 | office) + (1 | p_id), data = d2, ties = "efron")
m9 <- coxme(Surv(time0, time1, status) ~ (1 | office) + (1 | p_id), data = d2, ties = "efron")

AIC(m6, m7, m8, m9)
anova(m6, m7, m8, m9)
anova(m6, m8)
anova(m6, m9)

cox.zph(m8)

### 1.2.4 Models 10-14 ----
m10 <- coxph(Surv(time0, time1, status) ~ castles, data = d3, ties = "efron")
m11 <- coxph(Surv(time0, time1, status) ~ castles + change, data = d3, ties = "efron")
m12 <- coxme(Surv(time0, time1, status) ~ castles + change + (1 | p_id), data = d3, ties = "efron")
m13 <- coxme(Surv(time0, time1, status) ~ castles + change + aristocrat + (1 | p_id), data = d3, ties = "efron")
m14 <- coxph(Surv(time0, time1, status) ~ aristocrat, data = d3, ties = "efron")

AIC(m10, m11, m12, m13, m14)
models <- list(m11, m12, m13, m14)
lapply(models, function(mod) {
  anova(m10, mod)
})

cox.zph(m13)
cox.zph(m14)
plot(cox.zph(m13)[1])


### 1.2.5 Sequential proportional hazards models ----
years <- 1350:1500
s <- data.frame(years = years, l = NA, u = NA)

for (i in seq_along(s$years)) {
  year <- s$years[i]
  x <- d1
  x$regime <- ifelse(x$year < year, 1, 0)
  
  m <- coxph(Surv(tenure, status) ~ regime, data = x, ties = "efron")
  s$l[i] <- exp(confint(m)[1])
  s$u[i] <- exp(confint(m)[2])
  s$n[i] <- sum(x$regime)
  s$n1[i] <- 689 - sum(x$regime)
}

meanten <- d1 %>%
  group_by(year) %>%
  summarise(avg_t = mean(tenure)) %>%
  mutate(avg_ten = avg_t/max(avg_t))
meanten$year <- as.numeric(meanten$year)
s$years <- as.numeric(s$years)

fig4 <- ggplot(data = s, aes(x = years, ymin = l, ymax = u)) +
  geom_ribbon(fill = "grey", alpha = .6, col = "black", lty = 2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 1382, linetype = "dotted") +
  geom_vline(xintercept = 1439, linetype = "dotted") +
  geom_line(data = meanten, aes(x = year, y = avg_ten), inherit.aes = F) +
  labs(x = "Cut-off year",
       y = "Hazard ratio (CI)") +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 14, hjust = .5))

tiff("Figure4.tiff", w = 10.3, h = 8.3, units = "in", res = 600)
fig4
dev.off()


## 1.3 Figure 3, comparing hazard functions during Charles' and Louis' reigns ----
dlou <- d1[d1$king == "louis",]
dmch <- d1[d1$king == "charles",]

mlou <- stan_surv(Surv(time0, time1, status) ~ 1, 
                  basehaz = "ms",
                  cores = 8, chains = 4,
                  prior = normal(0, 2),
                  prior_intercept = normal(0, 2),
                  iter = 4000, warmup = 1000,
                  data = dlou)
mch <- stan_surv(Surv(time0, time1, status) ~ 1, 
                 basehaz = "ms",
                 cores = 8, chains = 4,
                 prior = normal(0, 2),
                 prior_intercept = normal(0, 2),
                 iter = 4000, warmup = 1000,
                 data = dmch)
dplou <- plot(mlou)$data
dpmch <- plot(mch)$data

kmlou <- survfit(Surv(time0, time1, status) ~ 1, data = dlou)
kmch <- survfit(Surv(time0, time1, status) ~ 1, data = dmch)

hzcl <- rbind(
  data.frame(time = dplou$times, haz = dplou$med, l = dplou$lb, u = dplou$ub, years = dplou$times / 365.25, period = "New"),
  data.frame(time = dpmch$times, haz = dpmch$med, l = dpmch$lb, u = dpmch$ub, years = dpmch$times / 365.25, period = "Old")
)

kmcl <- rbind(
  data.frame(time = kmlou$time, surv = kmlou$surv, l = kmlou$lower, u = kmlou$upper, years = kmlou$time / 365.25, period = "New"),
  data.frame(time = kmch$time, surv = kmch$surv, l = kmch$lower, u = kmch$upper, years = kmch$time / 365.25, period = "Old")
)

p9 <- ggplot(kmcl, aes(x = years, y = surv, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Old" = "grey40", "New" = "grey7"), 
                     labels = c("Old" = "Charles", "New" = "Louis")) +
  scale_fill_manual(values = c("Old" = "black", "New" = "black"), 
                    labels = c("Old" = "Charles", "New" = "Louis")) +
  scale_linetype_manual(values = c("Old" = 1, "New" = 2), 
                        labels = c("Old" = "Charles", "New" = "Louis")) +
  theme_classic() +
  ggtitle("King tenure metric") +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 14, hjust = .5)) +
  xlab("") +
  ylab("") +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_y_continuous(labels = label_scientific())

p10 <- ggplot(hzcl, aes(x = years, y = haz, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Old" = "grey40", "New" = "grey7"), 
                     labels = c("Old" = "Charles (r. 1308-1342)", "New" = "Louis (r. 1342-1382)")) +
  scale_fill_manual(values = c("Old" = "black", "New" = "black"), 
                    labels = c("Old" = "Charles (r. 1308-1342)", "New" = "Louis (r. 1342-1382)")) +
  scale_linetype_manual(values = c("Old" = 1, "New" = 2), 
                        labels = c("Old" = "Charles (r. 1308-1342)", "New" = "Louis (r. 1342-1382)")) +
  theme_classic() +
  ggtitle("") +
  theme(text = element_text(family = "serif", size = 18),
        legend.title = element_blank(), 
        legend.position = "bottom",
        plot.title = element_text(size = 14, hjust = .5)) +
  xlab("Year") +
  ylab("Hazard") +
  coord_cartesian(xlim = c(0, 20)) + 
  scale_y_continuous(labels = label_scientific())

fig3 <- p10
tiff("Figure3.tiff", w = 8, h = 8, units = "in", res = 600)
fig3
dev.off()
