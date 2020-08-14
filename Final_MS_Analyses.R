#install.packages("TOSTER")
{
library(ggsignif)
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
}
### use ALL trials, no wm rejects ###
#data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Cleaned_Data/Final.Data.csv_2020-02-05_10-02-59.csv")
### set working directory to MouseTrap Demo folder ###

### No RT / No WM exclusion ###
#data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Cleaned_Data/Final_Data_NoExclusions.csv.csv")
data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Cleaned_Data/Final_Data_NoExclusions.csv")
### 2 SD RT exclusion ###
#data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Cleaned_Data/Final_Data_2SDRTexclusion.csv")

### 3 SD RT exclusion ###
#data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Cleaned_Data/Final_Data_2SDRTexclusion.csv")

data <- data[, -1]
data <- MT.data.rating.table2
### RATE ###
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
# level1$Dom <- as.character(level1$Dom)
level1$Dom <- dplyr::recode(level1$Dom,
                            "0" = "Non-emotional",
                            "1" = "Emotional")

level1$Load <- dplyr::recode(level1$Load,
                             "0" = "Low",
                             "1" = "High")
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
summ(m1)
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

m3 <- (lmer(PerNeg.t ~ Dom * Load + (1 | subjID) + (1 | subjID:Dom) + (1 | subjID:Load), data = level1,
            REML = F))
m4 <- (lmer(PerNeg.t ~ Dom * Load + (1 | subjID), data = level1,
            REML = F))
summary(m3)
summary(m4)

anova(m3, m4)
### compare w/ previous ###
anova(m2, m3)
Anova(m3, type = "III")
anova(m3, type = "III")
confint(emmeans(m3, pairwise ~ Dom, adjust = "none"))
emmeans(m3.temp, pairwise ~ Dom, adjust = "none")
confint(m3)
m3
emm_options(opt.digits = TRUE) 
confint.merMod(m3)
### check at different reference levels ###
level1$Dom <- relevel(as.factor(level1$Dom), ref = "Emotional")
level1$Load <- relevel(as.factor(level1$Load), ref = "High")
summ(m3)

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
# ggplot(data = level1, mapping = aes(x = Condition, y = PerNeg.t, fill = Condition)) +
#   geom_bar(stat = "summary", fun.y = "mean") +
#   geom_signif(comparisons = list(c("Low Neutral", "Low Emotional")),
#               annotation = c(""), textsize=6, y_position = 85) +
#   geom_signif(comparisons = list(c("Low Neutral", "High Emotional")),
#               annotation = c(""), textsize=6, y_position = 98) +
#   geom_signif(comparisons = list(c("High Neutral", "Low Emotional")),
#               annotation = c(""), textsize=6, y_position = 80) +
#   geom_signif(comparisons = list(c("High Neutral", "High Emotional")),
#               annotation = c(""), textsize=6, y_position = 93) +
#   scale_fill_grey() +
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = .1) +
#   coord_cartesian(ylim = c(0, 100)) +
#   ggtitle("Subjective Interpretations of Surprise Faces") +
#   ylab("Percent Negative Ratings") +
#   theme_apa() +
#   theme(axis.text.x=element_blank(),
#         legend.title = element_text(color = "black", size = 20),
#         legend.text = element_text(color = "black", size = 15),
#         axis.text=element_text(size=20),
#         axis.title=element_text(size=20,face="bold")) 

level1$Load <- as.factor(level1$Load)
level1$Dom <- as.factor(level1$Dom)

level1$Load <- dplyr::recode(level1$Load,
                             "0" = "Low",
                             "1" = "High")

level1$Dom <- dplyr::recode(level1$Dom,
                             "0" = "Neutral",
                             "1" = "Emotional")

## plot ratings ###
ggplot(data = level1, mapping = aes(x = Load, y = PerNeg.t, fill = Dom)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge", width = .5) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = .5), width = 0.1) +
  geom_signif(y_position = c(90, 90), xmin = c(.875, 1.875), xmax = c(1.125, 2.125),
              annotation = c(""), size = 1.25) +
  geom_signif(annotation = "both p's ≤ .001",
              comparisons = list(c("Low", "High")),
              y_position = 93, size = 1.25,
              textsize = 7.5) +
  #geom_signif(comparisons = list(c("High Neutral", "Low Emotional")),
  #             map_signif_level = F, textsize=6, y_position = 90) +
  #geom_signif(comparisons = list(c("High Neutral", "High Emotional")),
  #             map_signif_level = function(p)sprintf("p = %.2g", p), textsize=6, y_position = 80) +
  scale_fill_grey(start = .4, end = .6) +
  coord_cartesian(ylim = c(0, 100)) +
  ylab("Percent Negative Ratings") +
  theme_apa(x.font.size = 30,
            y.font.size = 30,
            legend.use.title = F,
            legend.font.size = 30) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(face = "bold"),
        text = element_text(size = 30),
        plot.title = element_text(size=30))

level1$Dom <- relevel(level1$Dom, ref = "Non-emotional")
level1$Load <- relevel(level1$Load, ref = "Low")
ggplot(data = level1, mapping = aes(x = Dom, y = PerNeg.t, fill = Load)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge", width = .5) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = .5), width = 0.1) +
  geom_signif(y_position = c(85), xmin = c(1), xmax = c(2),
              annotation = c("**"), size = 1.25,
              textsize = 8) +
  #geom_signif(comparisons = list(c("High Neutral", "Low Emotional")),
  #             map_signif_level = F, textsize=6, y_position = 90) +
  #geom_signif(comparisons = list(c("High Neutral", "High Emotional")),
  #             map_signif_level = function(p)sprintf("p = %.2g", p), textsize=6, y_position = 80) +
  scale_fill_grey(start = .4, end = .6) +
  coord_cartesian(ylim = c(0, 100)) +
  ylab("Percent Negative Ratings") +
  xlab("Domain") +
  theme_apa(x.font.size = 30,
            y.font.size = 30,
            legend.use.title = F,
            legend.font.size = 30) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(face = "bold"),
        text = element_text(size = 30),
        plot.title = element_text(size=30))


  # #geom_path(x=c(2,2,3,3),y=c(37,38,38,37))+
  # #geom_path(x=c(3,3,4,4),y=c(49,50,50,49))+
  # annotate("text",x=1.5,y=27,label="p=0.012")
  # #annotate("text",x=2.5,y=39,label="p<0.0001")+
  # #annotate("text",x=3.5,y=51,label="p<0.0001")

emmeans(m3, pairwise ~ Dom, adjust = "none")
?emmeans

### equivalence testing ###
library(TOSTER)
# install.packages("ANOVAreplication")
library(ANOVAreplication)
#install.packages("effsize")
library(effsize)
# Set smallest effect size of interest and alpha
sesoi <- 5.9 # mean diff req'd for .22 es
alpha <- 0.05
data <- level1[, c("PerNeg.t", "Load")]
pooled.sd(data)
cohen.d(data$PerNeg.t, data$Load)


### calculate effect size for main effect of load ###
emmeans(m3, pairwise ~ Load, adjust = "none")
meandif <- 70.2-68.9
es <- meandif/(pooled.sd(data))

low.mean <- mean(subset(data, data$Load == "Low")$PerNeg.t, na.rm = T)
high.mean <- mean(subset(data, data$Load == "High")$PerNeg.t, na.rm = T)
low.sd <- sd(subset(data, data$Load == "Low")$PerNeg.t, na.rm = T)
high.sd <- sd(subset(data, data$Load == "High")$PerNeg.t, na.rm = T)
low.n <- as.numeric(count((subset(data, data$Load == "Low"))))
high.n <- as.numeric(count((subset(data, data$Load == "High"))))

#  Performing the equivalence test
res <- TOSTER::TOSTtwo.raw(m1  = low.mean,
                            m2  = high.mean,
                            sd1 = low.sd,
                            sd2 = high.sd,
                            n1  = low.n,
                            n2  = high.n,
                            low_eqbound  = -sesoi,
                            high_eqbound =  sesoi,
                            alpha = .05)



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
level1$Rate <- as.factor(level1$Rate)
level1$Load <- as.factor(level1$Load)
level1$Dom <- as.factor(level1$Dom)
level1$subjID <- as.factor(level1$subjID)


### rand int model ###
summary(rand.int.md <- lmer(MD ~ (1 | subjID), data = level1,
                    REML = F))
summ(rand.int.md)
lmerTest::rand(rand.int.md)

### full model ###
anova((full.mod <- lmer(MD ~  Rate * Load * Dom + (1 | subjID) + (1 | subjID:Rate) +
                          (1 | subjID:Load) + (1 | subjID:Dom), data = level1,
                         REML = F)), type = "III")

summ(lmer(MD~(1 | subjID), level1, REML = F))
summ(full.mod)

summary(blmer(MD ~  Rate * Load * Dom + (1 | subjID), data = level1,
              REML = F))

### Probe Rate x Load ###
level1$Rate <- dplyr::recode(level1$Rate,
                      '0' = "Positive",
                      '1' = "Negative")
level1$Load <- dplyr::recode(level1$Load,
                             '0' = "Low",
                             '1' = "High")
level1$Dom <- dplyr::recode(level1$Dom,
                             '0' = "NonEmo",
                             '1' = "Emo")

level1$Int.Cond <- paste(level1$Load, level1$Rate, sep = "_")

### Hartigan Dip Statistics ###
diptest::dip.test()
?dip.test
library(diptest)
dip.test(subset(level1, level1$Int.Cond == "Low_Negative")$MD)

ggplot(level1, aes(x = Load, y = MD, fill = Rate)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge", width = .5) +
  coord_cartesian(ylim = c(0, .75)) +
  geom_signif(annotation = "**",
              y_position = c(.63), xmin = c(.875), xmax = c(1.125),
              size = 1.25, textsize = 8, tip_length = .01) +
  geom_signif(annotation = "*",
              y_position = c(.55), xmin = c(1.125), xmax = c(2.125),
              size = 1.25, textsize = 8, tip_length = .01) +
  geom_signif(annotation = "+",
              y_position = c(.70), xmin = c(.875), xmax = c(1.825),
              size = 1.25, textsize = 8, tip_length = .01) +
  ylab("Maximum Deviation") +
  xlab("Load") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = .5), width = .1) +
  labs(color = "Load") +
  theme_apa(x.font.size = 30,
            y.font.size = 30,
            legend.use.title = F,
            legend.font.size = 30) +
  scale_fill_grey(start = .4, end = .6) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(face = "bold"),
        text = element_text(size = 30),
        plot.title = element_text(size=30))

emmeans(full.mod, pairwise ~ Rate * Load, adjust = "none")
confint(full.mod)
two.emms <- emmeans(full.mod, ~ Rate * (Load))
pairs(two.emms, adjust = "none")
confint(emmeans(full.mod, pairwise ~ Rate * Load, adjust = "none"))


contrasts <- emmeans(full.mod, pairwise ~ Rate * Load * Dom, adjust = "none")
contrasts <- as.data.frame(contrasts$contrasts)
### three way
ggplot(level1, aes(x = Load, y = MD, color = Rate, group = Dom)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge", width = .5) +
  # coord_cartesian(ylim = c(0, .75)) +
  # geom_signif(annotation = "**",
  #             y_position = c(.63), xmin = c(.875), xmax = c(1.125),
  #             size = 1.25, textsize = 8, tip_length = .01) +
  # geom_signif(annotation = "*",
  #             y_position = c(.55), xmin = c(1.125), xmax = c(2.125),
  #             size = 1.25, textsize = 8, tip_length = .01) +
  # geom_signif(annotation = "+",
  #             y_position = c(.70), xmin = c(.875), xmax = c(1.825),
  #             size = 1.25, textsize = 8, tip_length = .01) +
  ylab("Maximum Deviation") +
  xlab("Load") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = .5), width = .1) +
  labs(color = "Load") +
  theme_apa(x.font.size = 30,
            y.font.size = 30,
            legend.use.title = F,
            legend.font.size = 30) +
  scale_fill_grey(start = .4, end = .6) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(face = "bold"),
        text = element_text(size = 30),
        plot.title = element_text(size=30))


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
  ylab("MD") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

### barplot ###
rateloaddom$Load <- relevel(rateloaddom$Load, ref = "Low")
ggplot(rateloaddom, aes(Rate, fit, fill = Load)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
                position = position_dodge(.9)) +
  coord_cartesian(ylim = c(0, .75)) +
  ylab("MD") +
  xlab("Rating") +
  # geom_signif(annotation = "p = .003",
  #             y_position = c(.63), xmin = c(.75), xmax = c(1.75),
  #             size = 1.25, textsize = 3, tip_length = .01) +
  facet_grid(~Dom,labeller="label_both")
  # geom_signif(annotation = "*",
  #             y_position = c(.55), xmin = c(1.125), xmax = c(2.125),
  #             size = 1.25, textsize = 8, tip_length = .01) +
  # geom_signif(annotation = "+",
  #             y_position = c(.70), xmin = c(.875), xmax = c(1.825),
  #             size = 1.25, textsize = 8, tip_length = .01)

### REACTION TIME ###
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

### full model ###
level1$Rate <- as.factor(level1$Rate)
level1$Load <- as.factor(level1$Load)
level1$Dom <- as.factor(level1$Dom)
level1$subjID <- as.factor(level1$subjID)
### full model ###
summary(full.mod <- lmer(RT ~ Rate * Load * Dom + (1 | subjID), data = level1,
                         REML = F))
summ(full.mod)
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
  ylab("MD") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

### barplot ###
ggplot(rateloaddom, aes(Rate, fit, fill = Load)) +
  facet_grid(~Dom,labeller="label_both") +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
                position = position_dodge(.9)) +
  ylab("MD") +
  xlab("Rating")


three.emms <- emmeans(full.mod, ~ Rate * (Load * Dom))
pairs(three.emms, adjust = "none")




### probe accuracy ###

### mean correct ###

probe.df <- gather(data, key = "condition", value = "correct",
                   lo.emo.mem, hi.emo.mem, lo.neu.mem, hi.neu.mem)

probe.df$dom <- ifelse(probe.df$condition == "lo.emo.mem", "emo",
                       ifelse(probe.df$condition == "hi.emo.mem", "emo",
                              "neu"))
probe.df$load <- ifelse(probe.df$condition == "lo.emo.mem", "low",
                        ifelse(probe.df$condition == "lo.neu.mem", "low",
                               "high"))
### normal ###
qqp(probe.df$correct, "norm")

### log normal ###
qqp(probe.df$correct, "lnorm")
probe.df$cor_log = -log(probe.df$correct)
plotNormalHistogram(probe.df$cor_log)
library(rcompanion)

probe.df$load <- as.factor(probe.df$load)
probe.df$subjID <- as.factor(probe.df$subjID)
probe.df$dom <- as.factor(probe.df$dom)

### mixed effects model is not a good idea ###
rand.int <- lmer(correct ~ (1 | subjID), probe.df,
                 REML = F)
summ(rand.int)
rand(rand.int)

full.mod <- lmer(correct ~ dom * load + (1 | subjID), probe.df,
                 REML = F)
summ(full.mod)
rand(full.mod)
Effect('dom:load', full.mod)


### rep meas anova ###
### ez anova matches jasp output ###
#install.packages("ez")
library(ez)
options(contrasts=c("contr.sum","contr.poly"))
rt_anova = ezANOVA(
  data = probe.df
  , dv = .(correct)
  , wid = .(subjID)
  , within = .(dom,load),
  type =3,
  return_aov = T
)

emmeans(rt_anova$aov, pairwise ~ dom:load, adjust = "none")
emmeans(rt_anova$aov, ~ dom:load)
emmeans(rt_anova$aov, ~ load)
emmeans(rt_anova$aov, ~ dom)

summary(rt_anova$aov)
### reg bc that's what the rest of the paper uses.. ###
summ(pro.mod <- lm(correct ~ dom * load, probe.df))

pairs(emmeans(pro.mod, ~ dom * load, adjust = "none"), adjust = "none")

domload <- as.data.frame(effect("dom:load", pro.mod))

ggplot(domload, aes(x = dom, y = fit, fill = load)) +
  geom_bar(position="dodge", stat="identity") +
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               position = "dodge", width = .1)


ggplot(probe.df, aes(x = condition, y = correct, group = condition)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               position = "dodge", width = .1)

ggplot(probe.df, aes(x = condition, y = correct)) +
  geom_point() +
  geom_jitter()

mean(data$hi.emo.mem)
sd(data$hi.emo.mem)
probe.df$load <- ifelse()
write.csv(probe.df, "~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Cleaned_Data/probe.df.csv")


### now i want to test if pos vs. neg probe on inaccurate probe responses...
### run data_cleaning_MERP.R lines 1:313
temp <- MT.data

temp$data <- subset(temp$data, temp$data$trialtype == "memory" &
                      temp$data$mem.cor == 0)
temp <- temp$data

### recode stim to get valence for each probe image... ###
temp$stim.recode <- str_sub(temp$stim, 22, 25)

### now want to get pos vs. neg probe image conditions... ###
iaps.norm <- read_excel("~/Documents/Nick-Grad/Neta_Lab/Stimuli/IAPS_ratings.xls")

### find the valence cutoffs for pos and neg in the 144 stim... 
min(stim.data$Val_Mn[which(stim.data$Condition == "POS")])
max(stim.data$Val_Mn[which(stim.data$Condition == "POS")])
min(stim.data$Val_Mn[which(stim.data$Condition == "NEG")])
max(stim.data$Val_Mn[which(stim.data$Condition == "NEG")])

### code probe conditions ###
iaps.norm$CONDITION <- ifelse(iaps.norm$valmn <= 4.32, "NEG",
                              ifelse(iaps.norm$valmn >= 5.07, "POS",
                                     "NEU"))
### rename for merging ###
names(iaps.norm)[2] <- "stim.recode"

test <- merge(temp, iaps.norm)

count(test$CONDITION)
