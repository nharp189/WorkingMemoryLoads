# install.packages("gee")
library(gee)
library(geepack)
library(lme4)
library(lmerTest)
library(effects)
library(emmeans)
### run data_cleaning_MERP until line 287 ###

# Reshape time-normalized trajectories data into wide format
# only keeping xpos and ypos
# and adding Condition variable
trajectories_wide <- mt_reshape(MT.data,
                                use="tn_trajectories", use_variables = c("xpos"),
                                use2_variables = "condition.rating",
                                trajectories_long = FALSE
)

### merge with .data ###
full <- cbind(MT.data$data, trajectories_wide)

### check RT min / max for outliers ###
meanrt <- mean(full$RT[which(full$trialtype == "face")])
sdrt <- sd(full$RT[which(full$trialtype == "face")])
outlier <- meanrt + (3*sdrt)
#print(outlier)

### remove trials > outlierRT ###
full <-full[!(full$RT >= outlier),]

## flag trials before incorrect memory probes ###
for(i in 2:nrow(full)) {
  full$flag[i-1] <- ifelse(full$mem.cor[i] == 0, 0, 1)
}

### remove flagged trials ###
full <- subset(full, full$flag == 1)

#temp <- (ddply(full, "subjID", summarise,
 #              xpos1_sur_n = mean(xpos_1[which(condition.rating == "Sur.Neg")]),
  #             xpos2_sur_n = mean(xpos_2[which(condition.rating == "Sur.Neg")])))

#for(i in full[, c(41:142)]) {
#  
#}

varz <- 1:101
varz <- paste("xpos_", varz, sep = "")

### split to average x coordinates by rating ###
sur.neg.low.neu <- full %>% subset(condition.rating == "Sur.Neg" & 
                                     load == "LOW" & type == "NEU")
sur.neg.low.neu <- sur.neg.low.neu %>% group_by(subjID) %>% 
  summarise_at(varz, list(mean = mean))

sur.pos.low.neu <- full %>% subset(condition.rating == "Sur.Pos" & 
                                     load == "LOW" & type == "NEU")
sur.pos.low.neu <- sur.pos.low.neu %>% group_by(subjID) %>% 
  summarise_at(varz, list(mean = mean))

sur.neg.hi.neu <- full %>% subset(condition.rating == "Sur.Neg" & 
                                     load == "HIGH" & type == "NEU")
sur.neg.hi.neu <- sur.neg.hi.neu %>% group_by(subjID) %>% 
  summarise_at(varz, list(mean = mean))

sur.pos.hi.neu <- full %>% subset(condition.rating == "Sur.Pos" & 
                                     load == "HIGH" & type == "NEU")
sur.pos.hi.neu <- sur.pos.hi.neu %>% group_by(subjID) %>% 
  summarise_at(varz, list(mean = mean))

sur.neg.low.emo <- full %>% subset(condition.rating == "Sur.Neg" & 
                                     load == "LOW" & type == "EMO")
sur.neg.low.emo <- sur.neg.low.emo %>% group_by(subjID) %>% 
  summarise_at(varz, list(mean = mean))

sur.pos.low.emo <- full %>% subset(condition.rating == "Sur.Pos" & 
                                     load == "LOW" & type == "EMO")
sur.pos.low.emo <- sur.pos.low.emo %>% group_by(subjID) %>% 
  summarise_at(varz, list(mean = mean))

sur.neg.hi.emo <- full %>% subset(condition.rating == "Sur.Neg" & 
                                    load == "HIGH" & type == "EMO")
sur.neg.hi.emo <- sur.neg.hi.emo %>% group_by(subjID) %>% 
  summarise_at(varz, list(mean = mean))

sur.pos.hi.emo <- full %>% subset(condition.rating == "Sur.Pos" & 
                                    load == "HIGH" & type == "EMO")
sur.pos.hi.emo <- sur.pos.hi.emo %>% group_by(subjID) %>% 
  summarise_at(varz, list(mean = mean))

### add in rows for missing subjects ###
setdiff( sur.neg.hi.emo$subjID, sur.neg.hi.neu$subjID)
sur.neg.hi.neu[50, 1] <- "71020"

setdiff( sur.neg.hi.emo$subjID, sur.neg.low.neu$subjID)
sur.neg.low.neu[49, 1] <- "71020"
sur.neg.low.neu[50, 1] <- "71030"

setdiff( sur.neg.hi.emo$subjID, sur.pos.hi.emo$subjID)
sur.pos.hi.emo[33, 1] <- "71001"
sur.pos.hi.emo[34, 1] <- "71002"
sur.pos.hi.emo[35, 1] <- "71005"
sur.pos.hi.emo[36, 1] <- "71011"
sur.pos.hi.emo[37, 1] <- "71014"
sur.pos.hi.emo[38, 1] <- "71015"
sur.pos.hi.emo[39, 1] <- "71019"
sur.pos.hi.emo[40, 1] <- "71021"
sur.pos.hi.emo[41, 1] <- "71023"
sur.pos.hi.emo[42, 1] <- "71024"
sur.pos.hi.emo[43, 1] <- "71025"
sur.pos.hi.emo[44, 1] <- "71029"
sur.pos.hi.emo[45, 1] <- "71033"
sur.pos.hi.emo[46, 1] <- "71035"
sur.pos.hi.emo[47, 1] <- "71049"
sur.pos.hi.emo[48, 1] <- "71052"
sur.pos.hi.emo[49, 1] <- "71055"
sur.pos.hi.emo[50, 1] <- "71059"

setdiff( sur.neg.hi.emo$subjID, sur.pos.hi.neu$subjID)
sur.pos.hi.neu[46, 1] <- "71010"
sur.pos.hi.neu[47, 1] <- "71019"
sur.pos.hi.neu[48, 1] <- "71033"
sur.pos.hi.neu[49, 1] <- "71035"
sur.pos.hi.neu[50, 1] <- "71049"

setdiff( sur.neg.hi.emo$subjID, sur.pos.low.emo$subjID)
sur.pos.low.emo[38, 1] <- "71014"
sur.pos.low.emo[39, 1] <- "71015"
sur.pos.low.emo[40, 1] <- "71019"
sur.pos.low.emo[41, 1] <- "71029"
sur.pos.low.emo[42, 1] <- "71033"
sur.pos.low.emo[43, 1] <- "71035"
sur.pos.low.emo[44, 1] <- "71043"
sur.pos.low.emo[45, 1] <- "71047"
sur.pos.low.emo[46, 1] <- "71049"
sur.pos.low.emo[47, 1] <- "71052"
sur.pos.low.emo[48, 1] <- "71053"
sur.pos.low.emo[49, 1] <- "71055"
sur.pos.low.emo[50, 1] <- "71059"

setdiff( sur.neg.hi.emo$subjID, sur.pos.low.neu$subjID)
sur.pos.low.neu[48, 1] <- "71009"
sur.pos.low.neu[49, 1] <- "71049"
sur.pos.low.neu[50, 1] <- "71060"

### add factor labels ###
sur.neg.hi.emo$Rate <- 1
sur.pos.hi.emo$Rate <- 0
sur.neg.hi.neu$Rate <- 1
sur.pos.hi.neu$Rate <- 0
sur.neg.low.emo$Rate <- 1
sur.pos.low.emo$Rate <- 0
sur.neg.low.neu$Rate <- 1
sur.pos.low.neu$Rate <- 0

sur.neg.hi.emo$Load <- 1
sur.pos.hi.emo$Load <- 1
sur.neg.hi.neu$Load <- 1
sur.pos.hi.neu$Load <- 1
sur.neg.low.emo$Load <- 0
sur.pos.low.emo$Load <- 0
sur.neg.low.neu$Load <- 0
sur.pos.low.neu$Load <- 0

sur.neg.hi.emo$Dom <- 1
sur.pos.hi.emo$Dom <- 1
sur.neg.hi.neu$Dom <- 0
sur.pos.hi.neu$Dom <- 0
sur.neg.low.emo$Dom <- 1
sur.pos.low.emo$Dom <- 1
sur.neg.low.neu$Dom <- 0
sur.pos.low.neu$Dom <- 0

### combine again ###
comb <- rbind(sur.neg.hi.emo, sur.neg.hi.neu,
              sur.neg.low.emo, sur.neg.low.neu,
              sur.pos.hi.emo, sur.pos.hi.neu,
              sur.pos.low.emo, sur.pos.low.neu)

long <- gather(comb, key = "timestep", value = "xpos",
       xpos_1_mean, xpos_2_mean, xpos_3_mean, xpos_4_mean, xpos_5_mean,
       xpos_6_mean, xpos_7_mean, xpos_8_mean, xpos_9_mean, xpos_10_mean,
       xpos_11_mean, xpos_12_mean, xpos_13_mean, xpos_14_mean, xpos_15_mean,
       xpos_16_mean, xpos_17_mean, xpos_18_mean, xpos_19_mean, xpos_20_mean,
       xpos_21_mean, xpos_22_mean, xpos_23_mean, xpos_24_mean, xpos_25_mean,
       xpos_26_mean, xpos_27_mean, xpos_28_mean, xpos_29_mean, xpos_30_mean,
       xpos_31_mean, xpos_32_mean, xpos_33_mean, xpos_34_mean, xpos_35_mean,
       xpos_36_mean, xpos_37_mean, xpos_38_mean, xpos_39_mean, xpos_40_mean,
       xpos_41_mean, xpos_42_mean, xpos_43_mean, xpos_44_mean, xpos_45_mean,
       xpos_46_mean, xpos_47_mean, xpos_48_mean, xpos_49_mean, xpos_50_mean,
       xpos_51_mean, xpos_52_mean, xpos_53_mean, xpos_54_mean, xpos_55_mean,
       xpos_56_mean, xpos_57_mean, xpos_58_mean, xpos_59_mean, xpos_60_mean,
       xpos_61_mean, xpos_62_mean, xpos_63_mean, xpos_64_mean, xpos_65_mean,
       xpos_66_mean, xpos_67_mean, xpos_68_mean, xpos_69_mean, xpos_70_mean,
       xpos_71_mean, xpos_72_mean, xpos_73_mean, xpos_74_mean, xpos_75_mean,
       xpos_76_mean, xpos_77_mean, xpos_78_mean, xpos_79_mean, xpos_80_mean,
       xpos_81_mean, xpos_82_mean, xpos_83_mean, xpos_84_mean, xpos_85_mean,
       xpos_86_mean, xpos_87_mean, xpos_88_mean, xpos_89_mean, xpos_90_mean,
       xpos_91_mean, xpos_92_mean, xpos_93_mean, xpos_94_mean, xpos_95_mean,
       xpos_96_mean, xpos_97_mean, xpos_98_mean, xpos_99_mean, xpos_100_mean,
       xpos_101_mean)


### make timebins ###
varz <- 1:101
varz <- paste("xpos_", varz, sep = "")
varz <- paste(varz, "_mean", sep = "")

long$Bin <- ifelse(long$timestep %in% varz[1:20],"0", 
                   ifelse(long$timestep %in%  varz[81:101], "0",
                          ifelse(long$timestep %in% varz[21:40], "1",
                                 ifelse(long$timestep %in% varz[61:80], "1",
                                        ifelse(long$timestep %in% varz[41:60], "2", "")))))

# ### dummy code bin ###
# long$bin_dum0 <- recode(long$Bin,
#                         "0" = "1",
#                         "1" = "0",
#                         "2" = "0")
# 
# long$bin_dum1 <- recode(long$Bin,
#                         "0" = "0",
#                         "1" = "1",
#                         "2" = "0")

long$bin_dum2 <- recode(long$Bin,
                        "0" = "1",
                        "1" = "0",
                        "2" = "0")

long$bin_dum3 <- recode(long$Bin,
                        "0" = "0",
                        "1" = "0",
                        "2" = "1")
### make factors ###
long$Rate <- as.factor(long$Rate)
long$Load <- as.factor(long$Load)
long$Dom <- as.factor(long$Dom)
long$bin_dum0 <- as.factor(long$bin_dum0)
long$bin_dum1 <- as.factor(long$bin_dum1)

lmer(xpos ~ (1 | subjID), data = long, REML = F)

### run as lmer / GEE options below ###
summary(full.mod <- lmer(xpos ~ Rate * Dom * Load * (bin_dum2 + bin_dum3) + (1 | subjID),
     data = long, REML = F))

# summary(gee(xpos ~ Rate * Dom * Load, id = subjID, data = long))


#geeInd <- geeglm(xpos ~ Rate * Dom * Load, id = subjID, data = long, family=gaussian, corstr="ind")
#summary(geeInd)

### probe interactions ###
### Probe Rate x Load ###
rateload <- as.data.frame(effect("Rate:Load", full.mod))
rateload$Rate <- recode(rateload$Rate,
                        "0" = "Positive",
                        "1" = "Negative")
rateload$Load <- recode(rateload$Load,
                        "0" = "Low",
                        "1" = "High")
ggplot(rateload, aes(Rate, fit, color = Load, group = Load)) +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ggtitle("Rate x Load") +
  ylab("X-Coordinates") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

two.emms <- emmeans(full.mod, ~ Rate * (Load),
                    emm_options(pbkrtest.limit = 36158))
pairs(two.emms)

### Probe Rate x bin_dum0 ###
ratebin <- as.data.frame(effect("Rate:bin_dum0", full.mod))
ratebin$Rate <- recode(ratebin$Rate,
                        "0" = "Positive",
                        "1" = "Negative")
ratebin$bin_dum0 <- recode(ratebin$bin_dum0,
                        "0" = "Middle 40-60%",
                        "1" = "Start/end 0-20 and 80-100%")
ggplot(ratebin, aes(Rate, fit, color = bin_dum0, group = bin_dum0)) +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ggtitle("Rate x Bin") +
  ylab("X-Coordinates") +
  xlab("Rating") +
  ylim(c(0, -1)) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Bin")

two.emms <- emmeans(full.mod, ~ Rate * (bin_dum0),
                    emm_options(pbkrtest.limit = 36158))
pairs(two.emms)

### Probe Dom x bin_dum0 ###
dombin <- as.data.frame(effect("Dom:bin_dum0", full.mod))
dombin$Dom <- recode(dombin$Dom,
                       "0" = "Neutral",
                       "1" = "Emotional")
dombin$bin_dum0 <- recode(dombin$bin_dum0,
                           "0" = "Middle 40-60%",
                           "1" = "Start/end 0-20 and 80-100%")
ggplot(dombin, aes(Dom, fit, color = bin_dum0, group = bin_dum0)) +
  geom_point() +
  geom_line(mapping = aes(Dom, fit)) +
  ggtitle("Dom x Bin") +
  ylab("X-Coordinates") +
  xlab("Domain") +
  ylim(c(0, -1)) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Bin")

two.emms <- emmeans(full.mod, ~ Dom * (bin_dum0),
                    emm_options(pbkrtest.limit = 36158))
pairs(two.emms)

### Probe Load x bin_dum0 ###
loadbin <- as.data.frame(effect("Load:bin_dum0", full.mod))
loadbin$Load <- recode(loadbin$Load,
                     "0" = "Low",
                     "1" = "High")
loadbin$bin_dum0 <- recode(loadbin$bin_dum0,
                          "0" = "Middle 40-60%",
                          "1" = "Start/end 0-20 and 80-100%")
ggplot(loadbin, aes(Load, fit, color = bin_dum0, group = bin_dum0)) +
  geom_point() +
  geom_line(mapping = aes(Load, fit)) +
  ggtitle("Load x Bin") +
  ylab("X-Coordinates") +
  xlab("Load") +
  ylim(c(0, -1)) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Bin")

two.emms <- emmeans(full.mod, ~ Load * (bin_dum0),
                    emm_options(pbkrtest.limit = 36158))
pairs(two.emms)

### Probe Rate x Load x bin_dum0 ###
rateloadbin <- as.data.frame(effect("Rate:Load:bin_dum0", full.mod))
rateloadbin$Rate <- recode(rateloadbin$Rate,
                           "0" = "Positive",
                           "1" = "Negative")
rateloadbin$Load <- recode(rateloadbin$Load,
                           "0" = "Low",
                           "1" = "High")
rateloadbin$bin_dum0 <- recode(rateloadbin$bin_dum0,
                          "0" = "Middle 40-60%",
                          "1" = "Start/end 0-20 and 80-100%")
ggplot(rateloadbin, aes(Rate, fit, color = Load, group = Load)) +
  facet_grid(~bin_dum0,labeller="label_both") +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ylim(0, -1) +
  ylab("X Coordinates") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

### barplot ###
ggplot(rateloadbin, aes(Rate, fit, fill = Load)) +
  facet_grid(~bin_dum0,labeller="label_both") +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
                position = position_dodge(.9)) +
  ylim(c(0, -1)) +
  ylab("X Coordinates") +
  xlab("Rating")


three.emms <- emmeans(full.mod, ~ Rate * (Load * bin_dum0),
                      emm_options(pbkrtest.limit = 36158))
pairs(three.emms)

### Probe Dom x Load x bin_dum0 ###
domloadbin <- as.data.frame(effect("Dom:Load:bin_dum1", full.mod))
domloadbin$Dom <- recode(domloadbin$Dom,
                           "0" = "Neutral",
                           "1" = "Emotional")
domloadbin$Load <- recode(domloadbin$Load,
                           "0" = "Low",
                           "1" = "High")
domloadbin$bin_dum1 <- recode(domloadbin$bin_dum1,
                               "0" = "Middle 40-60%",
                               "1" = "Intermediary 20-40 and 60-80%")
ggplot(domloadbin, aes(Dom, fit, color = Load, group = Load)) +
  facet_grid(~bin_dum1,labeller="label_both") +
  geom_point() +
  geom_line(mapping = aes(Dom, fit)) +
  ylim(0, -1) +
  ylab("X Coordinates") +
  xlab("Domain") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

### barplot ###
ggplot(domloadbin, aes(Dom, fit, fill = Load)) +
  facet_grid(~bin_dum1,labeller="label_both") +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
                position = position_dodge(.9)) +
  ylim(c(0, -1)) +
  ylab("X Coordinates") +
  xlab("Domain")


three.emms <- emmeans(full.mod, ~ Dom * (Load * bin_dum1),
                      emm_options(pbkrtest.limit = 36158))
pairs(three.emms)


#### Reference group = timebin 2 ####
### Probe Rate x Load ###
rateload <- as.data.frame(effect("Rate:Load", full.mod))
rateload$Rate <- recode(rateload$Rate,
                        "0" = "Positive",
                        "1" = "Negative")
rateload$Load <- recode(rateload$Load,
                        "0" = "Low",
                        "1" = "High")
ggplot(rateload, aes(Rate, fit, color = Load, group = Load)) +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ggtitle("Rate x Load") +
  ylab("X-Coordinates") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

two.emms <- emmeans(full.mod, ~ Rate * (Load),
                    emm_options(pbkrtest.limit = 36158))
pairs(two.emms)

### Probe Dom x Load ###
domload <- as.data.frame(effect("Dom:Load", full.mod))
domload$Dom <- recode(domload$Dom,
                        "0" = "Neutral",
                        "1" = "Emotional")
domload$Load <- recode(domload$Load,
                        "0" = "Low",
                        "1" = "High")
ggplot(domload, aes(Dom, fit, color = Load, group = Load)) +
  geom_point() +
  geom_line(mapping = aes(Dom, fit)) +
  ggtitle("Domain x Load") +
  ylab("X-Coordinates") +
  xlab("Domain") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

two.emms <- emmeans(full.mod, ~ Dom * (Load),
                    emm_options(pbkrtest.limit = 36158))
pairs(two.emms)

### Probe Rate x bin_dum2 ###
ratebin <- as.data.frame(effect("Rate:bin_dum2", full.mod))
ratebin$Rate <- recode(ratebin$Rate,
                       "0" = "Positive",
                       "1" = "Negative")
ratebin$bin_dum2 <- recode(ratebin$bin_dum2,
                           "0" = "Intermediary 20-40 and 60-80%",
                           "1" = "Start/end 0-20 and 80-100%")
ggplot(ratebin, aes(Rate, fit, color = bin_dum2, group = bin_dum2)) +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ggtitle("Rate x Bin") +
  ylab("X-Coordinates") +
  xlab("Rating") +
  ylim(c(0, -1)) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Bin")

two.emms <- emmeans(full.mod, ~ Rate * (bin_dum0),
                    emm_options(pbkrtest.limit = 36158))
pairs(two.emms)

### Probe Dom x bin_dum2 ###
dombin <- as.data.frame(effect("Dom:bin_dum2", full.mod))
dombin$Dom <- recode(dombin$Dom,
                     "0" = "Neutral",
                     "1" = "Emotional")
dombin$bin_dum2 <- recode(dombin$bin_dum2,
                           "0" = "Intermediary 20-40 and 60-80%",
                           "1" = "Start/end 0-20 and 80-100%")
ggplot(dombin, aes(Dom, fit, color = bin_dum2, group = bin_dum2)) +
  geom_point() +
  geom_line(mapping = aes(Dom, fit)) +
  ggtitle("Dom x Bin") +
  ylab("X-Coordinates") +
  xlab("Domain") +
  ylim(c(0, -1)) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Bin")

two.emms <- emmeans(full.mod, ~ Dom * (bin_dum2),
                    emm_options(pbkrtest.limit = 36158))
pairs(two.emms)

### Probe Load x bin_dum2 ###
loadbin <- as.data.frame(effect("Load:bin_dum2", full.mod))
loadbin$Load <- recode(loadbin$Load,
                     "0" = "Neutral",
                     "1" = "Emotional")
loadbin$bin_dum2 <- recode(loadbin$bin_dum2,
                          "0" = "Intermediary 20-40 and 60-80%",
                          "1" = "Start/end 0-20 and 80-100%")
ggplot(loadbin, aes(Load, fit, color = bin_dum2, group = bin_dum2)) +
  geom_point() +
  geom_line(mapping = aes(Load, fit)) +
  ggtitle("Load x Bin") +
  ylab("X-Coordinates") +
  xlab("Domain") +
  ylim(c(0, -1)) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Bin")

two.emms <- emmeans(full.mod, ~ Load * (bin_dum2),
                    emm_options(pbkrtest.limit = 36158))
pairs(two.emms)

### Probe Rate x Dom x Load ###
ratedomload <- as.data.frame(effect("Rate:Dom:Load", full.mod))
ratedomload$Dom <- recode(ratedomload$Dom,
                          "0" = "Neutral",
                          "1" = "Emotional")
ratedomload$Load <- recode(ratedomload$Load,
                           "0" = "Low",
                           "1" = "High")
ratedomload$Rate <- recode(ratedomload$Rate,
                           "0" = "Positive",
                           "1" = "Negative")
ggplot(ratedomload, aes(Dom, fit, color = Load, group = Load)) +
  facet_grid(~Rate,labeller="label_both") +
  geom_point() +
  geom_line(mapping = aes(Dom, fit)) +
  ylim(0, -1) +
  ylab("X Coordinates") +
  xlab("Domain") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

### barplot ###
ggplot(ratedomload, aes(Rate, fit, fill = Load)) +
  facet_grid(~Dom,labeller="label_both") +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
                position = position_dodge(.9)) +
  ylim(c(0, -1)) +
  ylab("X Coordinates") +
  xlab("Rate")

three.emms <- emmeans(full.mod, ~ Rate * (Load * Dom),
                      emm_options(pbkrtest.limit = 36158))
pairs(three.emms)


### Probe Rate x Load x bin_dum2 ###
rateloadbin <- as.data.frame(effect("Rate:Load:bin_dum2", full.mod))
rateloadbin$Rate <- recode(rateloadbin$Rate,
                           "0" = "Positive",
                           "1" = "Negative")
rateloadbin$Load <- recode(rateloadbin$Load,
                           "0" = "Low",
                           "1" = "High")
rateloadbin$bin_dum2 <- recode(rateloadbin$bin_dum2,
                           "0" = "Intermediary 20-40 and 60-80%",
                           "1" = "Start/end 0-20 and 80-100%")
ggplot(rateloadbin, aes(Rate, fit, color = Load, group = Load)) +
  facet_grid(~bin_dum2,labeller="label_both") +
  geom_point() +
  geom_line(mapping = aes(Rate, fit)) +
  ylim(0, -1) +
  ylab("X Coordinates") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

### barplot ###
ggplot(rateloadbin, aes(Rate, fit, fill = Load)) +
  facet_grid(~bin_dum2,labeller="label_both") +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
                position = position_dodge(.9)) +
  ylim(c(0, -1)) +
  ylab("X Coordinates") +
  xlab("Rating")


three.emms <- emmeans(full.mod, ~ Rate * (Load * bin_dum2),
                      emm_options(pbkrtest.limit = 36158))
pairs(three.emms)

### Probe Domain x Load x bin_dum2 ###
domloadbin <- as.data.frame(effect("Dom:Load:bin_dum2", full.mod))
domloadbin$Dom <- recode(domloadbin$Dom,
                           "0" = "Neutral",
                           "1" = "Emotional")
domloadbin$Load <- recode(domloadbin$Load,
                           "0" = "Low",
                           "1" = "High")
domloadbin$bin_dum2 <- recode(domloadbin$bin_dum2,
                               "0" = "Intermediary 20-40 and 60-80%",
                               "1" = "Start/end 0-20 and 80-100%")
ggplot(domloadbin, aes(Dom, fit, color = Load, group = Load)) +
  facet_grid(~bin_dum2,labeller="label_both") +
  geom_point() +
  geom_line(mapping = aes(Dom, fit)) +
  ylim(0, -1) +
  ylab("X Coordinates") +
  xlab("Rating") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1) +
  labs(color = "Load")

### barplot ###
ggplot(domloadbin, aes(Dom, fit, fill = Load)) +
  facet_grid(~bin_dum2,labeller="label_both") +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.1,
                position = position_dodge(.9)) +
  ylim(c(0, -1)) +
  ylab("X Coordinates") +
  xlab("Rating")


three.emms <- emmeans(full.mod, ~ Dom * (Load * bin_dum2),
                      emm_options(pbkrtest.limit = 36158))
pairs(three.emms)
## scratch space below ###


### one try at avg x coords ###
mt_example <- mt_average(MT.data$tn_trajectories, save_as="av_trajectories",
                         interval_size=100)
View(MT.data$tn_trajectories)

### second try at avg x coords ###
varz <- 1:101
varz <- paste(varz, ".xpos", sep = "")

average_measures <- mt_aggregate_per_subject(
  MT.data,
  use="tn_trajectories",
  use_variables=c("1.xpos"),
  use2_variables="Condition",
  subject_id="subject_nr"
)
print(varz )
View(MT.data$measures)
