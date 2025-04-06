#  OFFICE TENURE IN MEDIEVAL HUNGARY ------------------
#    R script by Davor Salihovic, University of Antwerp                                                 
#                      davor.salihovic@uantwerpen.be 

rm(list = ls())

library(dplyr); library(tidyr); library(lubridate); library(data.table)
library(survival); library(eha); library(survminer); library(bshazard)
library(ggplot2); library(brms); library(rstanarm); library(posterior); library(performance); 
library(pastecs); library(car)

load("data/data.RData")

d$cens <- 1 - d$status
d <- d[, c("time_in_office", "start_since_king", "stop_since_king", "p_id", "cens", "status", "old_regime", "office", "king_dismissed", "royal_estates", "royal_factor")]
d <- d[complete.cases(d), ]
d$office[d$office == "ban of szorenyi"] <- "ban of szoreny"

# 1. ANALYSIS ----

## 1.1 Tenure in Office ----
p0 <- ggplot(data = d, aes(x = old_regime, y = time_in_office)) +
  geom_violin() +
  scale_x_discrete(limits = c("0", "1")) + 
  geom_boxplot(width = .1) +
  theme_classic() +
  labs(x = "Old Regime", y = "Tenure (days)")
p0

  # Descriptive statistics and assumptions
by(d$time_in_office, d$royal_factor, stat.desc, basic = T)
leveneTest(d$time_in_office, d$royal_factor)

  # Difference in mean term in office between the old and new regimes?
t.test(d$time_in_office ~ d$old_regime, na.rm = T)

  # Planned-contrast analysis of variance
contrast1 <- c(-1, 0, 1)
contrast2 <- c(0, -1, 1)

contrasts(d$royal_factor) <- cbind(contrast1, contrast2)

avt <- aov(time_in_office ~ royal_factor, d)
summary(avt)
summary.lm(avt)

contrast1 <- c(-2, 1, 1)
contrast2 <- c(0, -1, 1)

contrasts(d$royal_factor) <- cbind(contrast1, contrast2)

avt <- aov(time_in_office ~ royal_factor, d)
summary(avt)
summary.lm(avt)
# Some evidence of significant difference between the earliest and latest eras.
# The importance of the latest era is indicated by no change in the model
  # with the inclusion of a different reference contrast.


## 1.2 Survival in Office Across Periods ----
  
### 1.2.1 King-day metrics ----
mdl <- Surv(start_since_king, stop_since_king, status) ~ 1

# Overall KM estimate
km <- survfit(mdl, d)

# Hazard rate
hr <- bshazard(mdl, data = d)

# Weibull curve
m0 <- aftreg(mdl, dist = "weibull", data = d)

p1 <- ggplot(data = data.frame(time = km$time, 
                                  surv = km$surv,
                                  l = km$lower,
                                  u = km$upper,
                                  weib = 1 - pweibull(km$time, exp(m0$coefficients[2]), exp(m0$coefficients[1])),
                                  years = km$time / 365.25), aes(x = years, y = surv)) +
  geom_line(color = "red4", lty = 2, lwd = .7) + 
  geom_line(aes(x = years, y = weib), color = "blue4", lwd = .7) + # Weibull estimate
  geom_ribbon(aes(ymin = l, ymax = u), stat = "identity", fill = "red3", alpha = .1) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16)) +
  xlab("Years") +
  ylab("Survival") +
  coord_cartesian(xlim = c(0, 20))

#### 1.2.1.1 Times of crisis/times of stability ----
stable <- c("charles", "louis", "sigismund", "matthias")

d.crisis <- d[!(d$king_dismissed %in% stable), ]
d.stable <- d[d$king_dismissed %in% stable, ]

km.crisis <- survfit(mdl, data = d.crisis)
km.stable <- survfit(mdl, data = d.stable)

haz.crisis <- bshazard(mdl, data = d.crisis, nk = 100)
haz.stable <- bshazard(mdl, data = d.stable, nk = 100)

km.combined <- rbind(
  data.frame(time = km.stable$time, surv = km.stable$surv, l = km.stable$lower, u = km.stable$upper, years = km.stable$time / 365.25, period = "Stable"),
  data.frame(time = km.crisis$time, surv = km.crisis$surv, l = km.crisis$lower, u = km.crisis$upper, years = km.crisis$time / 365.25, period = "Crisis")
)

haz.combined <- rbind(
  data.frame(time = haz.stable$time, haz = haz.stable$hazard, l = haz.stable$lower.ci, u = haz.stable$upper.ci, years = haz.stable$time / 365.25, period = "Stable"),
  data.frame(time = haz.crisis$time, haz = haz.crisis$hazard, l = haz.crisis$lower.ci, u = haz.crisis$upper.ci, years = haz.crisis$time / 365.25, period = "Crisis")
)

p2 <- ggplot(km.combined, aes(x = years, y = surv, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Crisis" = "blue", "Stable" = "red4"), 
                     labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  scale_fill_manual(values = c("Crisis" = "blue4", "Stable" = "red3"), 
                    labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  scale_linetype_manual(values = c("Crisis" = 1, "Stable" = 2), 
                        labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank()) +
  xlab("Years") +
  ylab("Survival") +
  coord_cartesian(xlim = c(0, 20))

p3 <- ggplot(data = haz.combined, aes(x = years, y = haz, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Crisis" = "blue", "Stable" = "red4"), 
                     labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  scale_fill_manual(values = c("Crisis" = "blue4", "Stable" = "red3"), 
                    labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  scale_linetype_manual(values = c("Crisis" = 1, "Stable" = 2), 
                        labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank()) +
  xlab("Years") +
  ylab("Hazard") +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 0.002))


#### 1.2.1.2 Pre- and post-Sigismund Period ----
km.pre <- survfit(mdl, data = d[d$old_regime == 1,])
km.post <- survfit(mdl, data = d[d$old_regime == 0,])

haz.pre <- bshazard(mdl, data = d[d$old_regime == 1,], nk = 100)
haz.post <- bshazard(mdl, data = d[d$old_regime == 0,], nk = 100)

km.prepost <- rbind(
  data.frame(time = km.pre$time, surv = km.pre$surv, l = km.pre$lower, u = km.pre$upper, years = km.pre$time / 365.25, period = "Pre"),
  data.frame(time = km.post$time, surv = km.post$surv, l = km.post$lower, u = km.post$upper, years = km.post$time / 365.25, period = "Post")
)

haz.prepost <- rbind(
  data.frame(time = haz.pre$time, haz = haz.pre$hazard, l = haz.pre$lower.ci, u = haz.pre$upper.ci, years = haz.pre$time / 365.25, period = "Pre"),
  data.frame(time = haz.post$time, haz = haz.post$hazard, l = haz.post$lower.ci, u = haz.post$upper.ci, years = haz.post$time / 365.25, period = "Post")
)

p4 <- ggplot(km.prepost, aes(x = years, y = surv, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Pre" = "blue", "Post" = "red4"), 
                     labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  scale_fill_manual(values = c("Pre" = "blue", "Post" = "red4"), 
                    labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  scale_linetype_manual(values = c("Pre" = 1, "Post" = 2), 
                        labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank(),
        legend.position = "none") +
  xlab("Years") +
  ylab("Survival") +
  coord_cartesian(xlim = c(0, 20))

p5 <- ggplot(data = haz.prepost, aes(x = years, y = haz, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Pre" = "blue", "Post" = "red4"), 
                     labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  scale_fill_manual(values = c("Pre" = "blue", "Post" = "red4"), 
                    labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  scale_linetype_manual(values = c("Pre" = 1, "Post" = 2), 
                        labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank(),
        legend.position = "bottom") +
  xlab("Years") +
  ylab("Hazard") +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 0.002))


### 1.2.2 Personal tenure metrics ----
mdlp <- Surv(time_in_office, status) ~ 1

# Overall KM estimate
kmp <- survfit(mdlp, d)

# Hazard rate
hrp <- bshazard(mdlp, data = d)

# Weibull curve
m0p <- aftreg(mdlp, dist = "weibull", data = d)

p6 <- ggplot(data = data.frame(time = kmp$time, 
                               surv = kmp$surv,
                               l = kmp$lower,
                               u = kmp$upper,
                               weib = 1 - pweibull(kmp$time, exp(m0p$coefficients[2]), exp(m0p$coefficients[1])),
                               years = kmp$time / 365.25), aes(x = years, y = surv)) +
  geom_line(color = "red4", lty = 2, lwd = .7) + 
  geom_line(aes(x = years, y = weib), color = "blue4", lwd = .7) + # Weibull estimate
  geom_ribbon(aes(ymin = l, ymax = u), stat = "identity", fill = "red3", alpha = .1) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16)) +
  xlab("Years") +
  ylab("Survival") +
  coord_cartesian(xlim = c(0, 20))


#### 1.2.2.1 Times of crisis/times of stability ----
kmp.crisis <- survfit(mdlp, data = d.crisis)
kmp.stable <- survfit(mdlp, data = d.stable)

hazp.crisis <- bshazard(mdlp, data = d.crisis, nk = 100)
hazp.stable <- bshazard(mdlp, data = d.stable, nk = 100)

kmp.combined <- rbind(
  data.frame(time = kmp.stable$time, surv = kmp.stable$surv, l = kmp.stable$lower, u = kmp.stable$upper, years = kmp.stable$time / 365.25, period = "Stable"),
  data.frame(time = kmp.crisis$time, surv = kmp.crisis$surv, l = kmp.crisis$lower, u = kmp.crisis$upper, years = kmp.crisis$time / 365.25, period = "Crisis")
)

hazp.combined <- rbind(
  data.frame(time = hazp.stable$time, haz = hazp.stable$hazard, l = hazp.stable$lower.ci, u = hazp.stable$upper.ci, years = hazp.stable$time / 365.25, period = "Stable"),
  data.frame(time = hazp.crisis$time, haz = hazp.crisis$hazard, l = hazp.crisis$lower.ci, u = hazp.crisis$upper.ci, years = hazp.crisis$time / 365.25, period = "Crisis")
)

p7 <- ggplot(kmp.combined, aes(x = years, y = surv, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Crisis" = "blue", "Stable" = "red4"), 
                     labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  scale_fill_manual(values = c("Crisis" = "blue4", "Stable" = "red3"), 
                    labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  scale_linetype_manual(values = c("Crisis" = 1, "Stable" = 2), 
                        labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank()) +
  xlab("Years") +
  ylab("Survival") +
  coord_cartesian(xlim = c(0, 17))

p8 <- ggplot(data = hazp.combined, aes(x = years, y = haz, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Crisis" = "blue", "Stable" = "red4"), 
                     labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  scale_fill_manual(values = c("Crisis" = "blue4", "Stable" = "red3"), 
                    labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  scale_linetype_manual(values = c("Crisis" = 1, "Stable" = 2), 
                        labels = c("Crisis" = "Times of crisis", "Stable" = "Times of stability")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank()) +
  xlab("Years") +
  ylab("Hazard") +
  coord_cartesian(xlim = c(0, 17), ylim = c(0, 0.002))


#### 1.2.1.2 Pre- and post-Sigismund Period ----
kmp.pre <- survfit(mdlp, data = d[d$old_regime == 1,])
kmp.post <- survfit(mdlp, data = d[d$old_regime == 0,])

hazp.pre <- bshazard(mdlp, data = d[d$old_regime == 1,], nk = 100)
hazp.post <- bshazard(mdlp, data = d[d$old_regime == 0,], nk = 100)

kmp.prepost <- rbind(
  data.frame(time = kmp.pre$time, surv = kmp.pre$surv, l = kmp.pre$lower, u = kmp.pre$upper, years = kmp.pre$time / 365.25, period = "Pre"),
  data.frame(time = kmp.post$time, surv = kmp.post$surv, l = kmp.post$lower, u = kmp.post$upper, years = kmp.post$time / 365.25, period = "Post")
)

hazp.prepost <- rbind(
  data.frame(time = hazp.pre$time, haz = hazp.pre$hazard, l = hazp.pre$lower.ci, u = hazp.pre$upper.ci, years = hazp.pre$time / 365.25, period = "Pre"),
  data.frame(time = hazp.post$time, haz = hazp.post$hazard, l = hazp.post$lower.ci, u = hazp.post$upper.ci, years = hazp.post$time / 365.25, period = "Post")
)

p9 <- ggplot(kmp.prepost, aes(x = years, y = surv, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Pre" = "blue", "Post" = "red4"), 
                     labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  scale_fill_manual(values = c("Pre" = "blue", "Post" = "red4"), 
                    labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  scale_linetype_manual(values = c("Pre" = 1, "Post" = 2), 
                        labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank()) +
  xlab("Years") +
  ylab("Survival") +
  coord_cartesian(xlim = c(0, 17))

p10 <- ggplot(data = hazp.prepost, aes(x = years, y = haz, color = period, linetype = period)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = l, ymax = u, fill = period), alpha = .1) +
  scale_color_manual(values = c("Pre" = "blue", "Post" = "red4"), 
                     labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  scale_fill_manual(values = c("Pre" = "blue", "Post" = "red4"), 
                    labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  scale_linetype_manual(values = c("Pre" = 1, "Post" = 2), 
                        labels = c("Pre" = "Period before 1439", "Post" = "Period after 1439")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank()) +
  xlab("Years") +
  ylab("Hazard") +
  coord_cartesian(xlim = c(0, 17), ylim = c(0, 0.002))


### 1.2.3 Models and Inference ----
survdiff(Surv(time_in_office, status) ~ old_regime, data = d)
survdiff(Surv(time_in_office, status) ~ old_regime, data = d, rho = 1)

mCk <- coxph(Surv(start_since_king, stop_since_king, status) ~ old_regime, data = d, method = "efron")
mCp <- coxph(Surv(time_in_office, status) ~ old_regime, data = d, method = "efron")

cox.zph(mCk)
cox.zph(mCp)

(residualsK <- data.frame(deviance = residuals(mCk, type = "deviance"),
                         martingale = residuals(mCk, tyoe = "martingale"),
                         dfbeta = residuals(mCk, type = "dfbetas")))

(residualsP <- data.frame(deviance = residuals(mCp, type = "deviance"),
                         martingale = residuals(mCp, tyoe = "martingale"),
                         dfbeta = residuals(mCp, type = "dfbetas")))

mCk
mCp


#### 1.2.3.1 Bayesian models for tenure. Right censoring, default flat priors throughout, Cox PH models and Weibull likelihood ----

# COX
mCP0 <- brm(time_in_office | cens(cens) ~ 1 + old_regime,
            family = brmsfamily("cox"),
            cores = 4, chains = 4, 
            iter = 4000, warmup = 1000,
            data = d)

mCP1 <- brm(time_in_office | cens(cens) ~ 1 + old_regime + (1 | office),
             family = brmsfamily("cox"),
             cores = 4, chains = 4, 
             iter = 4000, warmup = 1000,
             data = d)

mCP2 <- brm(time_in_office | cens(cens) ~ 1 + old_regime + (1 | office) + (1 | p_id),
             family = brmsfamily("cox"),
             cores = 4, chains = 4, 
             iter = 4000, warmup = 1000,
             control = list(adapt_delta = .85),
             data = d)

mCP0 <- add_criterion(mCP0, "waic")
mCP1 <- add_criterion(mCP1, "waic")
mCP2 <- add_criterion(mCP2, "waic")

loo_compare(mCP0, mCP1, mCP2, criterion = "waic")
model_weights(mCP0, mCP1, mCP2, weights = "waic") %>%
  round(digits = 3)

post <- as.array(mCP2)
np <- nuts_params(mCP2)
mcmc_trace(post, pars = "b_old_regime1", np = np)

plot(density(post[, , 2]))

hypothesis(mCP2, "old_regime1 < 0", alpha = .05)

resultsC <- as.data.frame(summarise_draws(mCP2, mean, ~quantile(.x, probs = c(.025, .975)))) %>%
  filter(grepl("r_office", variable))
cbind(resultsC$variable, exp(resultsC[, 2:4]))

# WEIBULL
mCW0 <- brm(time_in_office | cens(cens) ~ 1 + old_regime,
            family = weibull(),
            cores = 4, chains = 4, 
            iter = 4000, warmup = 1000,
            data = d)

mCW1 <- brm(time_in_office | cens(cens) ~ 1 + old_regime + (1 | office),
            family = weibull(),
            cores = 4, chains = 4, 
            iter = 4000, warmup = 1000,
            data = d)

mCW2 <- brm(time_in_office | cens(cens) ~ 1 + old_regime + (1 | office) + (1 | p_id),
            family = weibull(),
            cores = 4, chains = 4, 
            iter = 4000, warmup = 1000,
            control = list(adapt_delta = .85),
            data = d)

mCW0 <- add_criterion(mCW0, "waic")
mCW1 <- add_criterion(mCW1, "waic")
mCW2 <- add_criterion(mCW2, "waic")

loo_compare(mCW0, mCW1, mCW2, criterion = "waic")
model_weights(mCW0, mCW1, mCW2, weights = "waic") %>%
  round(digits = 3)

pp_check(mCW2)

postW <- as.array(mCW2)
npW <- nuts_params(mCW2)
mcmc_trace(postW, pars = "b_old_regime1", np = npW)

plot(density(postW[, , 2]))

hypothesis(mCW2, "old_regime1 > 0", alpha = .05)

resultsW <- as.data.frame(summarise_draws(mCW2, mean, ~quantile(.x, probs = c(.025, .975)))) %>%
  filter(grepl("r_office", variable))
cbind(resultsW$variable, exp(resultsW[, 2:4]))

saveRDS(mCP2, "cox_model.rds")
saveRDS(mCW2, "weibull_model.rds")

#### 1.2.3.2 Hazard models ----
  # Personal tenure
mHP0 <- brm(
  status ~  1 + old_regime + poly(time_in_office, 2),
  family = bernoulli,
  cores = 4, chains = 4, 
  iter = 4000, warmup = 1000,
  data = d
)

mHP1 <- brm(
  status ~ 1 + old_regime + poly(time_in_office, 2) + (1 | office),
  family = bernoulli,
  cores = 4, chains = 4, 
  iter = 4000, warmup = 1000,
  data = d
)

mHP2 <- brm(
  status ~ 1 + old_regime + poly(time_in_office, 2) + (1 | office) + (1 | p_id),
  family = bernoulli,
  cores = 4, chains = 4, 
  iter = 4000, warmup = 1000,
  data = d
)

mHP0 <- add_criterion(mHP0, "waic")
mHP1 <- add_criterion(mHP1, "waic")
mHP2 <- add_criterion(mHP2, "waic")

loo_compare(mHP0, mHP1, mHP2, criterion = "waic")
model_weights(mHP0, mHP1, mHP2, weights = "waic") %>%
  round(digits = 3)

postHP <- as.array(mHP2)
npHP <- nuts_params(mHP2)
mcmc_trace(postHP, pars = "b_old_regime1", np = npHP)
mcmc_trace(postHP, pars = "b_polytime_in_office21", np = npHP)

hypothesis(mHP2, "old_regime1 > 0", alpha = .05)

resultsHP <- as.data.frame(summarise_draws(mHP2, mean, ~quantile(.x, probs = c(.025, .975)))) %>%
  filter(grepl("r_office", variable))
cbind(resultsHP$variable, exp(resultsHP[, 2:4]))

saveRDS(mHP2, "hazard_model.rds")

  # King-day metrics
mHK0 <- brm(
  status ~  1 + old_regime + poly(stop_since_king, 2),
  family = bernoulli,
  cores = 4, chains = 4, 
  iter = 4000, warmup = 1000,
  data = d
)

mHK1 <- brm(
  status ~ 1 + old_regime + poly(stop_since_king, 2) + (1 | office),
  family = bernoulli,
  cores = 4, chains = 4, 
  iter = 4000, warmup = 1000,
  data = d
)

mHK2 <- brm(
  status ~ 1 + old_regime + poly(stop_since_king, 2) + (1 | office) + (1 | p_id),
  family = bernoulli,
  cores = 4, chains = 4, 
  iter = 4000, warmup = 1000,
  data = d
)

mHK0 <- add_criterion(mHK0, "waic")
mHK1 <- add_criterion(mHK1, "waic")
mHK2 <- add_criterion(mHK2, "waic")

loo_compare(mHK0, mHK1, mHK2, criterion = "waic")
model_weights(mHK0, mHK1, mHK2, weights = "waic") %>%
  round(digits = 3)

postHK <- as.array(mHK2)
npHK <- nuts_params(mHK2)
mcmc_trace(postHK, pars = "b_old_regime1", np = npHK)
mcmc_trace(postHK, pars = "b_polystop_since_king21", np = npHK)

hypothesis(mHK2, "old_regime1 > 0", alpha = .05)

resultsHK <- as.data.frame(summarise_draws(mHK2, mean, ~quantile(.x, probs = c(.025, .975)))) %>%
  filter(grepl("r_office", variable))
cbind(resultsHK$variable, exp(resultsHK[, 2:4]))

#### 1.2.3.3 Survival models with M-spline baseline hazard ----
mSP <- stan_surv(Surv(time_in_office, status) ~ old_regime + (1 | office) + (1 | p_id),
                 basehaz = "ms",
                 cores = 4, chains = 4, 
                 iter = 4000, warmup = 1000,
                 data = d) 

mSK <- stan_surv(Surv(start_since_king, stop_since_king, status) ~ old_regime + (1 | office) + (1 | p_id),
                 basehaz = "ms",
                 cores = 4, chains = 4, 
                 iter = 4000, warmup = 1000,
                 data = d)

postmSP <- as.array(mSP)
npSP <- nuts_params(mSP)
mcmc_trace(postmSP, pars = "old_regime1", np = npSP)

postmSK <- as.array(mSK)
npSK <- nuts_params(mSK)
mcmc_trace(postmSK, pars = "old_regime1", np = npSK)

resultsSP <- as.data.frame(summarise_draws(mSP, mean, ~quantile(.x, probs = c(.025, .975)))) %>%
  filter(grepl("office", variable))
cbind(resultsSP$variable, exp(resultsSP[, 2:4]))

resultsSK <- as.data.frame(summarise_draws(mSK, mean, ~quantile(.x, probs = c(.025, .975)))) %>%
  filter(grepl("office", variable))
cbind(resultsSK$variable, exp(resultsSK[, 2:4]))

saveRDS(mSP, "m_spline_model_person.rds")
saveRDS(mSK, "m_spline_model_king.rds")

## 1.3 Plots ----
  # Hazard person tenure
pred.haz0 <- data.frame(time_in_office = seq(100, 10000), 
                        old_regime = 0, 
                        office = "palatine", 
                        p_id = "P0003") 

pred.haz1 <- data.frame(time_in_office = seq(100, 10000), 
                        old_regime = 1, 
                        office = "palatine", 
                        p_id = "P0003") 


haz.plot.data0 <- as.data.frame(fitted(mHP2, newdata = pred.haz0))
haz.plot.data1 <- as.data.frame(fitted(mHP2, newdata = pred.haz1))

haz.plot.data0 <- haz.plot.data0 %>% 
  mutate(
    time = pred.haz0$time_in_office / 365.25,
    old_regime = "No"
  )

haz.plot.data1 <- haz.plot.data1 %>% 
  mutate(
    time = pred.haz1$time_in_office / 365.25,
    old_regime = "Yes"
  )

haz.plot.data <- rbind(haz.plot.data0, haz.plot.data1)


p11 <- ggplot(data = haz.plot.data, aes(x = time, y = Estimate, 
                                         color = old_regime, linetype = old_regime)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = old_regime), alpha = .1) +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red4"), 
                     labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red4"), 
                    labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  scale_linetype_manual(values = c("Yes" = 1, "No" = 2), 
                        labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank()) +
  xlab("Years") +
  ylab("Probability") +
  coord_cartesian(xlim = c(0, 15))
p11
             
# Hazard king-day
pred.hazK0 <- data.frame(stop_since_king = seq(100, 10000), 
                        old_regime = 0, 
                        office = "palatine", 
                        p_id = "P0003") 

pred.hazK1 <- data.frame(stop_since_king = seq(100, 10000), 
                        old_regime = 1, 
                        office = "palatine", 
                        p_id = "P0003") 


haz.plot.dataK0 <- as.data.frame(fitted(mHK2, newdata = pred.hazK0))
haz.plot.dataK1 <- as.data.frame(fitted(mHK2, newdata = pred.hazK1))

haz.plot.dataK0 <- haz.plot.dataK0 %>% 
  mutate(
    time = pred.hazK0$stop_since_king / 365.25,
    old_regime = "No"
  )

haz.plot.dataK1 <- haz.plot.dataK1 %>% 
  mutate(
    time = pred.hazK1$stop_since_king / 365.25,
    old_regime = "Yes"
  )

haz.plot.dataK <- rbind(haz.plot.dataK0, haz.plot.dataK1)


p12 <- ggplot(data = haz.plot.dataK, aes(x = time, y = Estimate, 
                                        color = old_regime, linetype = old_regime)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = old_regime), alpha = .1) +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red4"), 
                     labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red4"), 
                    labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  scale_linetype_manual(values = c("Yes" = 1, "No" = 2), 
                        labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank()) +
  xlab("Years") +
  ylab("Probability") +
  coord_cartesian(xlim = c(0, 15))

# Hazards from M-spline models
  # Person
postSP0 <- posterior_survfit(mSP, newdata = 
                               data.frame(
                                 old_regime = 0, 
                                 office = "palatine", 
                                 p_id = "P0003"), type = "haz")
                                 
postSP1 <- posterior_survfit(mSP, newdata = 
                               data.frame(
                                 old_regime = 1, 
                                 office = "palatine", 
                                 p_id = "P0003"), type = "haz")

postSP0 <- as.data.frame(postSP0) %>% mutate(year = time / 365.25, old_regime = "No")
postSP1 <- as.data.frame(postSP1) %>% mutate(year = time / 365.25, old_regime = "Yes")
postSP <- rbind(postSP0, postSP1)

p13 <- ggplot(data = postSP, aes(x = year, y = median, 
                                         color = old_regime, linetype = old_regime)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub, fill = old_regime), alpha = .1) +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red4"), 
                     labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red4"), 
                    labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  scale_linetype_manual(values = c("Yes" = 1, "No" = 2), 
                        labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank()) +
  xlab("Years") +
  ylab("Hazard") +
  coord_cartesian(xlim = c(0, 20))

 # King
postSK0 <- posterior_survfit(mSK, newdata = 
                               data.frame(
                                 old_regime = 0, 
                                 office = "palatine", 
                                 p_id = "P0003"), type = "haz")

postSK1 <- posterior_survfit(mSK, newdata = 
                               data.frame(
                                 old_regime = 1, 
                                 office = "palatine", 
                                 p_id = "P0003"), type = "haz")

postSK0 <- as.data.frame(postSK0) %>% mutate(year = time / 365.25, old_regime = "No")
postSK1 <- as.data.frame(postSK1) %>% mutate(year = time / 365.25, old_regime = "Yes")
postSK <- rbind(postSK0, postSK1)

p14 <- ggplot(data = postSK, aes(x = year, y = median, 
                                 color = old_regime, linetype = old_regime)) +
  geom_line(lwd = .7) +
  geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub, fill = old_regime), alpha = .1) +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red4"), 
                     labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red4"), 
                    labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  scale_linetype_manual(values = c("Yes" = 1, "No" = 2), 
                        labels = c("Yes" = "Period before 1439", "No" = "Period after 1439")) +
  theme_classic() +
  theme(text = element_text(family = "serif", size = 16),
        legend.title = element_blank()) +
  xlab("Years") +
  ylab("Hazard") +
  coord_cartesian(xlim = c(0, 20))
