library(lme4)
library(jtools)
# install.packages("robustlmm")
library(robustlmm)
library(tidyverse)
# install.packages("reghelper")
library(reghelper)
library(lmerTest)
library(effects)
library(emmeans)
library(car)
library(MASS)
require(lattice)
#install.packages("blme")
library(blme)

#data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Cleaned_Data/Final.Data.csv_2019-09-06_12-18-56.csv")


### use ALL trials, no wm rejects ###
data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Cleaned_Data/Final.Data.csv_2020-02-05_10-02-59.csv")

data <- data[, -1]
long <- gather(data, key = "Condition", value = "PerNeg",
               lo.emo.sur_rate, hi.emo.sur_rate,
               lo.neu.sur_rate, hi.neu.sur_rate)

#### RATE ####
long <- gather(data, key = "Condition", value = "PerNeg",
               lo.emo.sur_rate, hi.emo.sur_rate,
               lo.neu.sur_rate, hi.neu.sur_rate)

level1 <- long[, c("subjID", "Condition", "PerNeg")]
level1$Dom <- ifelse(level1$Condition == "lo.emo.sur_rate", 1,
                      ifelse(level1$Condition == "hi.emo.sur_rate", 1, 0))
level1$Load <- ifelse(level1$Condition == "lo.emo.sur_rate", 0,
                      ifelse(level1$Condition == "lo.neu.sur_rate", 0, 1))

#write.csv(level1, "mlm_l1.csv", row.names = F)

### try alternative distribution ###
level1$PerNeg.t <- level1$PerNeg * 100
# level1$PerNeg.t <- level1$PerNeg.t + .1

### normal ###
qqp(level1$PerNeg.t, "norm")

### log normal ###
qqp(level1$PerNeg.t, "lnorm")

### Binomial ###
nbinom <- fitdistr(level1$PerNeg.t, "Negative Binomial")
qqp(level1$PerNeg.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

### Poisson ###
poisson <- fitdistr(level1$PerNeg.t, "Poisson")
qqp(level1$PerNeg.t, "pois", poisson$estimate)

### gamma ###
gamma <- fitdistr(level1$PerNeg.t, "gamma")
qqp(level1$PerNeg.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

### make factors ###
level1$Dom <- as.factor(level1$Dom)
level1$Load <- as.factor(level1$Load)
level1$subjID <- as.factor(level1$subjID)
### model building ###
intercept.only <- lm(PerNeg.t ~ subjID, data = level1)
summary(intercept.only)

### random intercept model ###
rand.int.only <- (lmer(PerNeg.t ~ (1 | subjID), data = level1))
summ(rand.int.only)

### test for random int significance ###
lmerTest::rand(rand.int.only)

### add domain ###
m1 <- (lmer(PerNeg.t ~ Dom + (1 | subjID), data = level1))
summary(m1)

### compare w/ int-only ###
anova(rand.int.only, m1)

### random effect? ###
m1.r <- (lmer(PerNeg.t ~ Dom + (Dom | subjID), data = level1))
summary(m1.r)

### compare w/ fixed model ###
anova(m1.r, m1)

### add load ###
m2 <- (lmer(PerNeg.t ~ Dom + Load + (1 | subjID), data = level1))
summary(m2)

### compare w/ previous ###
anova(m1, m2)

### random load? ###
m2.r <- (lmer(PerNeg.t ~ Dom + Load + (Load | subjID), data = level1))
summary(m2.r)

### compare w/ previous ###
anova(m2.r, m2)

### add interaction ###
m3 <- (lmer(PerNeg.t ~ Dom * Load + (1 | subjID), data = level1))
summary(m3)

### compare w/ previous ###
anova(m2, m3)

level1$Condition <- dplyr::recode(level1$Condition,
                           "hi.emo.sur_rate" = "High Emotional",
                           "lo.emo.sur_rate" = "Low Emotional",
                           "hi.neu.sur_rate" = "High Neutral",
                           "lo.neu.sur_rate" = "Low Neutral")
level1$Condition <- factor(level1$Condition, levels = c("Low Neutral",
                                                         "High Neutral",
                                                         "Low Emotional",
                                                         "High Emotional"))

### plot ratings ###
ggplot(data = level1, mapping = aes(x = Condition, y = PerNeg.t, fill = Condition)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_fill_grey() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1) +
  ylim(c(0, 100)) +
  ggtitle("Subjective Interpretations of Surprise Faces") +
  ylab("Percent Negative Ratings") +
  theme_apa() +
  theme(axis.text.x=element_blank()) 




plot(full.mod)
Linearity<-plot(resid(full.mod),long$PerNeg) 
qqmath(full.mod, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)
?qqmath


hist(residuals(full.mod))
shapiro.test(residuals(full.mod))

full.mod <- (lmer(PerNeg.t ~  as.factor(Dom) + as.factor(Load) + 
                     as.factor(Dom):as.factor(Load) + (1 | subjID), data = level1, REML = F))

emo <- subset(level1, Dom == 1)
neu <- subset(level1, Dom == 0)
cor.test(emo$PerNeg, neu$PerNeg)

rand.int.mod <- lmer(PerNeg ~ (1 | subjID), data = level1, REML = F)
summary(full.mod.g)

full.mod.g <- glmer(PerNeg.t ~  as.factor(Dom) + as.factor(Load) + 
                    as.factor(Dom):as.factor(Load) + (1 | subjID), data = level1, nAGQ=0,
                 family = lognormal)

level1$PerNeg.l <- log(1 - level1$PerNeg.t)



summary(full.mod.g)

emmip(full.mod.g, ~ Dom)

mean(level1$PerNeg.t, na.rm = T)

anova(full.mod, full.mod.g)

summary(full.mod)
shapiro.test(residuals(full.mod))

qqnorm(full.mod, ~ranef(., level=1))

getME(rlmer(PerNeg ~ Dom + (1 | subjID), data = level1), devcomp)

Anova(full.mod)


sigma(rand.int.mod)

library(nlme)
glm.model <- glmmPQL(PerNeg ~ Dom * Load, ~1 | subID, family = gaussian(link = "log"),
               data = recog, verbose = FALSE)

methods("predict")
level1$predlm = predict(full.mod)


ggplot(level1, aes(x = as.factor(Dom), y = PerNeg, color = as.factor(subjID)), group = 1) +
  geom_point(stat = "summary", fun.y = sum) +
  #geom_smooth(method = "lm", se = F)
  stat_summary(fun.y = sum, geom = "line")







#### MAD #####
long.md <- gather(data, key = "Condition", value = "MD",
                  lo.emo.sur_p_MAD, lo.emo.sur_n_MAD,
                  hi.emo.sur_p_MAD, hi.emo.sur_n_MAD,
                  lo.neu.sur_p_MAD, lo.neu.sur_n_MAD, 
                  hi.neu.sur_p_MAD, hi.neu.sur_n_MAD)

# long.md <- gather(data, key = "Condition", value = "MD",
#                   lo.emo.sur_MAD,
#                   hi.emo.sur_MAD,
#                   lo.neu.sur_MAD,
#                   hi.neu.sur_MAD,)
level1 <- long.md[, c("subjID", "Condition", "MD")]

level1$Dom <- ifelse(level1$Condition %in% c("lo.emo.sur_p_MAD",
                                             "hi.emo.sur_p_MAD",
                                             "lo.emo.sur_n_MAD",
                                             "hi.emo.sur_n_MAD"), 1, 0)

level1$Load <- ifelse(level1$Condition %in% c("lo.neu.sur_p_MAD",
                                              "lo.neu.sur_n_MAD",
                                              "lo.emo.sur_n_MAD",
                                              "lo.emo.sur_p_MAD"), 0, 1)

level1$Rate <- ifelse(level1$Condition %in% c("lo.emo.sur_p_MAD",
                                              "hi.emo.sur_p_MAD",
                                              "lo.neu.sur_p_MAD",
                                              "hi.neu.sur_p_MAD"), 0, 1)

### recode NA as . for spss ###
# level1[is.na(level1)] <- "."
# write.csv(level1, "mlm_l1_mad.csv", row.names = F)

### look at each effect ###
summary(rand.int.mod <- lmer(MD ~ (1 | subjID), data = level1, REML = F))
rand(rand.int.mod)
summary(dom.mod <- lmer(MD ~ Dom + (1 | subjID), data = level1, REML = F))

summary(load.mod <- rlmer(MD ~ Load + (1 | subjID), data = level1, REML = F))

summary(rate.mod <- lmer(MD ~ Rate + (1 | subjID), data = level1, REML = F))




level1$Rate <- as.factor(level1$Rate)
level1$Load <- as.factor(level1$Load)
level1$Dom <- as.factor(level1$Dom)
level1$subjID <- as.factor(level1$subjID)
### full model ###
summary(full.mod <- lmer(MD ~  Rate * Load * Dom + (1 | subjID), data = level1,
                         REML = F))

summ(full.mod)

summary(blmer(MD ~  Rate * Load * Dom + (1 | subjID), data = level1,
     REML = F))


summ(full.mod <- rlmer(MD ~ Load * Rate * Dom + (1 | subjID), data = level1,
                         REML = F))

# simple_slopes(full.mod)
# graph_model(full.mod, y=MD, x=Rate, lines=momed.c, bargraph = TRUE)



### Probe Rate x Load ###
rateload <- as.data.frame(effect("Rate:Load", full.mod))
rateload$Rate <- dplyr::recode(rateload$Rate,
                        "0" = "Positive",
                        "1" = "Negative")
rateload$Load <- dplyr::recode(rateload$Load,
                        "0" = "Low Load",
                        "1" = "High Load")
ggplot(rateload, aes(Rate, fit, color = Load, group = Load)) +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ylim(0, .75) +
  ggtitle("Interaction of Rating and Load") +
  ylab("Maximum Deviation") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load") +
  theme_apa() +
  scale_colour_grey(start = .4, end = .6)

two.emms <- emmeans(full.mod, ~ Rate * (Load))
pairs(two.emms, adjust = "none")

### Probe Rate x Load x Dom ###
rateloaddom <- as.data.frame(effect("Rate:Load:Dom", full.mod))
rateloaddom$Rate <- dplyr::recode(rateloaddom$Rate,
                        "0" = "Positive",
                        "1" = "Negative")
rateloaddom$Load <- dplyr::recode(rateloaddom$Load,
                        "0" = "Low Load",
                        "1" = "High Load")
rateloaddom$Dom <- dplyr::recode(rateloaddom$Dom,
                          "0" = "Neutral",
                          "1" = "Emotional")
ggplot(rateloaddom, aes(Rate, fit, color = Load, group = Load)) +
  facet_grid(~Dom) +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ylim(0, .75) +
  ylab("Maximum Deviation") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load") +
  theme_apa() +
  scale_colour_grey(start = .4, end = .6)
 
### barplot ###
ggplot(rateloaddom, aes(Dom, fit, fill = Rate)) +
  facet_grid(~Load) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
                position = position_dodge(.9)) +
  ylab("Maximum Deviation") +
  xlab("Content Type") +
  ylim(c(0, .8)) +
  theme_apa() +
  scale_fill_grey(start = .4, end = .6) +
  ggtitle("Interaction of Rating, Load, and Content Type")

# ggplot(rateloaddom, aes(Rate, fit, fill = Load)) +
#   facet_grid(~Dom,labeller="label_both") +
#   geom_bar(position="dodge", stat="identity") +
#   geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
#                 position = position_dodge(.9)) +
#   ylab("Maximum Deviation") +
#   xlab("Rating")
# 
# ggplot(rateloaddom, aes(Dom, fit, fill = Load)) +
#   facet_grid(~Rate,labeller="label_both") +
#   geom_bar(position="dodge", stat="identity") +
#   geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
#                 position = position_dodge(.9)) +
#   ylab("Maximum Deviation") +
#   xlab("Rating")

three.emms <- emmeans(full.mod, ~ Rate * (Load * Dom))
pairs(three.emms, adjust = "none")

pwpp(three.emms)
install.packages("lsmeans")
library(lsmeans)
class(full.mod)

lsmeans(full.mod, pairwise ~ Rate:Load:Dom)
#### Reaction Time Analysis ####
#### RT #####
long.RT <- gather(data, key = "Condition", value = "RT",
                  lo.emo.sur_p_RT, lo.emo.sur_n_RT,
                  hi.emo.sur_p_RT, hi.emo.sur_n_RT,
                  lo.neu.sur_p_RT, lo.neu.sur_n_RT, 
                  hi.neu.sur_p_RT, hi.neu.sur_n_RT)

# long.md <- gather(data, key = "Condition", value = "MD",
#                   lo.emo.sur_MAD,
#                   hi.emo.sur_MAD,
#                   lo.neu.sur_MAD,
#                   hi.neu.sur_MAD,)
level1 <- long.RT[, c("subjID", "Condition", "RT")]

level1$Dom <- ifelse(level1$Condition %in% c("lo.emo.sur_p_RT",
                                             "hi.emo.sur_p_RT",
                                             "lo.emo.sur_n_RT",
                                             "hi.emo.sur_n_RT"), 1, 0)

level1$Load <- ifelse(level1$Condition %in% c("lo.neu.sur_p_RT",
                                              "lo.neu.sur_n_RT",
                                              "lo.emo.sur_n_RT",
                                              "lo.emo.sur_p_RT"), 0, 1)

level1$Rate <- ifelse(level1$Condition %in% c("lo.emo.sur_p_RT",
                                              "hi.emo.sur_p_RT",
                                              "lo.neu.sur_p_RT",
                                              "hi.neu.sur_p_RT"), 0, 1)
# write.csv(level1, "mlm_l1_mad.csv", row.names = F)

### look at each effect ###
summary(rand.int.mod <- lmer(RT ~ (1 | subjID), data = level1, REML = F))

summary(dom.mod <- lmer(RT ~ Dom + (1 | subjID), data = level1, REML = F))

summary(load.mod <- lmer(RT ~ Load + (1 | subjID), data = level1, REML = F))

summary(rate.mod <- lmer(RT ~ Rate + (1 | subjID), data = level1, REML = F))

### full model ###
level1$Rate <- as.factor(level1$Rate)
level1$Load <- as.factor(level1$Load)
level1$Dom <- as.factor(level1$Dom)
level1$subjID <- as.factor(level1$subjID)
### full model ###
summary(full.mod <- lmer(RT ~ Rate * Load * Dom + (1 | subjID), data = level1,
                         REML = F))
Anova(full.mod)
### Probe Rate x Load ###
rateload <- as.data.frame(effect("Rate:Load", full.mod))
rateload$Rate <- dplyr::recode(rateload$Rate,
                        "0" = "Positive",
                        "1" = "Negative")
rateload$Load <- dplyr::recode(rateload$Load,
                        "0" = "Low",
                        "1" = "High")
ggplot(rateload, aes(Rate, fit, color = Load, group = Load)) +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ggtitle("Rate x Load") +
  ylab("Reaction Time") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

two.emms <- emmeans(full.mod, ~ Rate * (Load))
pairs(two.emms, adj = "none")

### Probe Rate x Dom ###
ratedom <- as.data.frame(effect("Rate:Dom", full.mod))
ratedom$Rate <- recode(ratedom$Rate,
                        "0" = "Positive",
                        "1" = "Negative")
ratedom$Dom <- recode(ratedom$Dom,
                        "0" = "Neu",
                        "1" = "Emo")
ggplot(ratedom, aes(Rate, fit, color = Dom, group = Dom)) +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ggtitle("Rate x Dom") +
  ylab("Reaction Time") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Dom")

two.emms <- emmeans(full.mod, ~ Rate * (Dom))
pairs(two.emms)

### Probe Load x Dom ###
loaddom <- as.data.frame(effect("Load:Dom", full.mod))
loaddom$Load <- recode(loaddom$Load,
                       "0" = "Low",
                       "1" = "High")
loaddom$Dom <- recode(loaddom$Dom,
                      "0" = "Neu",
                      "1" = "Emo")
ggplot(loaddom, aes(Load, fit, color = Dom, group = Dom)) +
  geom_point() +
  geom_line(mapping = aes(Load, fit)) +
  ggtitle("Load x Dom") +
  ylab("Reaction Time") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Dom")

two.emms <- emmeans(full.mod, ~ Load * (Dom))
pairs(two.emms)

### Probe Rate x Load x Dom ###
rateloaddom <- as.data.frame(effect("Rate:Load:Dom", full.mod))
rateloaddom$Rate <- dplyr::recode(rateloaddom$Rate,
                           "0" = "Positive",
                           "1" = "Negative")
rateloaddom$Load <- dplyr::recode(rateloaddom$Load,
                           "0" = "Low",
                           "1" = "High")
rateloaddom$Dom <- dplyr::recode(rateloaddom$Dom,
                          "0" = "Neutral",
                          "1" = "Emotional")
ggplot(rateloaddom, aes(Rate, fit, color = Load, group = Load)) +
  facet_grid(~Dom,labeller="label_both") +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ylab("Reaction Time") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

### barplot ###
ggplot(rateloaddom, aes(Dom, fit, fill = Load)) +
  facet_grid(~Rate,labeller="label_both") +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
                position = position_dodge(.9)) +
  ylab("Reaction Time") +
  xlab("Rating")


three.emms <- emmeans(full.mod, ~ Rate * (Load * Dom))
pairs(three.emms)
