setwd("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses")
#install.packages("BANOVA")
library(BANOVA)
library(tidyr)
library(dplyr)
library(yarrr)
library(emmeans)
library(BayesFactor)
library(readr)

bound.data <- read_csv("Final_Data_20190324.csv")
bound.data <- bound.data[,-1]
bound.data$subjID <- factor(bound.data$subjID)
### remove bad subject ###
t.test(MT.data.rating.table$emo.mem, MT.data.rating.table$neu.mem, paired = TRUE)
mean(MT.data.rating.table$neu.mem)
library(ggpubr)
shapiro.test(MT.data.rating.table$lo.neu.sur_rate)

rate.data_long <- gather(MT.data.rating.table, key = condition, value = "Percent Negative",  
                         lo.emo.ang_rate,hi.emo.ang_rate,lo.neu.ang_rate,hi.neu.ang_rate,
                         lo.emo.hap_rate,hi.emo.hap_rate,lo.neu.hap_rate,hi.neu.hap_rate,
                         lo.emo.sur_rate,hi.emo.sur_rate,lo.neu.sur_rate,hi.neu.sur_rate)
mem.data_long <- gather(MT.data.rating.table, key = Memory, value = "Accuracy",  
                         lo.mem, hi.mem)
### effect of load (Low = 2images, High = 6images ###
rate.data_long$load <- ifelse(rate.data_long$condition %in% 
                                c("lo.emo.ang_rate","lo.neu.ang_rate",
                                  "lo.emo.hap_rate","lo.neu.hap_rate",
                                  "lo.emo.sur_rate","lo.neu.sur_rate"), "Low",
                              ifelse(rate.data_long$condition %in%
                                       c("hi.emo.ang_rate","hi.neu.ang_rate",
                                         "hi.emo.hap_rate","hi.neu.hap_rate",
                                         "hi.emo.sur_rate","hi.neu.sur_rate"), "High",""))

### effect of valence ###
rate.data_long$type <- ifelse(rate.data_long$condition %in% 
                               c("lo.emo.ang_rate","hi.emo.ang_rate", 
                                 "lo.emo.hap_rate", "hi.emo.hap_rate",
                                 "lo.emo.sur_rate", "hi.emo.sur_rate"), "Emo",
                             ifelse(rate.data_long$condition %in% 
                                      c("lo.neu.ang_rate", "hi.neu.ang_rate",
                                        "lo.neu.hap_rate", "hi.neu.hap_rate",
                                        "lo.neu.sur_rate", "hi.neu.sur_rate"), "Non", ""))
                                    
### effect of valence ###
rate.data_long$val <- ifelse(rate.data_long$condition %in% 
                               c("lo.emo.ang_rate","hi.emo.ang_rate",
                                 "lo.neu.ang_rate","hi.neu.ang_rate"), "Angry",
                             ifelse(rate.data_long$condition %in% 
                                      c("lo.emo.hap_rate","hi.emo.hap_rate",
                                        "lo.neu.hap_rate","hi.neu.hap_rate"), "Happy",
                                    ifelse(rate.data_long$condition %in% 
                                             c("lo.emo.sur_rate","hi.emo.sur_rate",
                                               "lo.neu.sur_rate","hi.neu.sur_rate"), "Surprise", "")))


rate.data_long$val <- as.factor(rate.data_long$val)
rate.data_long$load <- as.factor(rate.data_long$load)
rate.data_long$type <- as.factor(rate.data_long$type)


### Memory Accuracy Graph ###
mem.data_long$Memorylab <- ifelse(mem.data_long$Memory == "hi.mem", 
                                  "High Load", "Low Load")
mem.data_long$Accuracy <- mem.data_long$Accuracy * 100
MAD.surp.sum3 <- mem.data_long %>%
  group_by(Memorylab) %>%
  summarise(`Percent Correct` = mean(Accuracy, na.rm = TRUE),
            sd_Acc = sd(Accuracy, na.rm  = TRUE),
            n_Acc = n(),
            SE_Acc = sd(Accuracy, na.rm = TRUE)/sqrt(n()))

p3 <- (ggplot(MAD.surp.sum3, aes(x = Memorylab, y = `Percent Correct`, fill = Memorylab)) + 
        geom_bar(stat = "Summary", fun.y = "mean") +
        scale_fill_manual(values = c("#84BDA1","#27BD4F")) +
        geom_errorbar(aes(ymin = `Percent Correct` - SE_Acc, ymax = `Percent Correct` + SE_Acc), width=.2) +
        theme_bw())  
p3 <-  p3 + labs(y= "Percent Correct", x = "Load") + theme_classic()

ggplotly(p3)
mem.data_long$Memory <- factor(mem.data_long$Memory, levels=c("lo.mem","hi.mem"))

pirateplot(Accuracy ~ Memory, mem.data_long,
           inf.method = "se",
           bar.f.col = c("#ACACAC", "#4B4042"),
           bar.f.o = .8, 
           bean.f.col = "White",
           ylim = c(0, 100))
mem.data_long$Accuracy <- mem.data_long$Accuracy*100

rate.data_long$`Percent Negative` <- rate.data_long$`Percent Negative` * 100
MAD.surp.sum4 <- rate.data_long %>%
  group_by(condition) %>%
  summarise(`Percent Negative` = mean(`Percent Negative`, na.rm = TRUE),
            sd_PN = sd(`Percent Negative`, na.rm  = TRUE),
            n_PN = n(),
            SE_PN = sd(`Percent Negative`, na.rm = TRUE)/sqrt(n()))

p4 <- (ggplot(MAD.surp.sum4, aes(x = condition, y = `Percent Negative`, fill = condition)) + 
         geom_bar(stat = "Summary", fun.y = "mean") +
         scale_fill_manual(values = c("#84BDA1","#27BD4F", "#84BDA1","#27BD4F")) +
         geom_errorbar(aes(ymin = `Percent Negative` - SE_PN, ymax = `Percent Negative` + SE_PN), width=.2) +
         theme_bw())  
p4 <-  p4 + labs(y= "Percent Negative", x = "Condition") + theme_classic()

ggplotly(p4)


rate.data_long$condition <- factor(rate.data_long$condition, levels=c("lo.neu.sur_rate",
                                                                      "hi.neu.sur_rate",
                                                                      "lo.emo.sur_rate",
                                                                      "hi.emo.sur_rate"))
pirateplot(`Percent Negative` ~ condition, data = rate.data_long,
           inf.method = "se",
           bar.f.o = .8,
           bean.f.col = "white",
           bar.b.col = "black",
           bar.f.col = c("#ACACAC", "#4B4042", "#517BAC", "#A67BC0"))


MT.data.rating.table$val_bias <- rowMeans(MT.data.rating.table[c('lo.emo.sur_rate', 'hi.emo.sur_rate',
                                                     'lo.neu.sur_rate', 'hi.neu.sur_rate')])


plot(MT.data.rating.table$val_bias, MT.data.rating.table$lo.emo.sur_n_RT)
cor.test(MT.data.rating.table$val_bias, MT.data.rating.table$lo.emo.sur_n_RT,
         use = "pairwise.complete.obs", method = "spearman")
cor.test(MT.data.rating.table$hi.emo.sur_rate, MT.data.rating.table$hi.emo.sur_n_RT,
         use = "pairwise.complete.obs", method = "spearman")
cor.test(MT.data.rating.table$lo.neu.sur_rate, MT.data.rating.table$lo.neu.sur_n_RT,
         use = "pairwise.complete.obs", method = "spearman")
cor.test(MT.data.rating.table$hi.neu.sur_rate, MT.data.rating.table$hi.neu.sur_n_RT,
         use = "pairwise.complete.obs", method = "spearman")
cor.test(MT.data.rating.table$lo.emo.sur_rate, MT.data.rating.table$lo.emo.sur_p_RT,
         use = "pairwise.complete.obs", method = "spearman")
cor.test(MT.data.rating.table$hi.emo.sur_rate, MT.data.rating.table$hi.emo.sur_p_RT,
         use = "pairwise.complete.obs", method = "spearman")
cor.test(MT.data.rating.table$lo.neu.sur_rate, MT.data.rating.table$lo.neu.sur_p_RT,
         use = "pairwise.complete.obs", method = "spearman")
cor.test(MT.data.rating.table$hi.neu.sur_rate, MT.data.rating.table$hi.neu.sur_p_RT,
         use = "pairwise.complete.obs", method = "spearman")






bayes.model <- anovaBF(`Percent Negative` ~ val * load * type, data = rate.data_long)
summary(bayes.model)
aov.model <- aov(`Percent Negative` ~ type * val, data = rate.data_long)
summary(aov.model)
rate.data_long$`Percent Negative` <- rate.data_long$`Percent Negative`*100
pirateplot(`Percent Negative`~ load * val, data = rate.data_long,
           pal = c("Red","Red", "Green","Green", "Blue", "Blue"),
           inf.method = "se",
           bar.f.o = .9,
           bar.b.o = 1,
           inf.f.o = .8)
pirateplot(`Percent Negative`~  val, data = rate.data_long,
           pal = c("Red", "Green", "Blue"), inf.method = "ci", 
           bar.f.o = 1,
           inf.f.o = .5, 
           bean.b.o = 1,
           point.col = "Black",
           avg.line.col = c("Red", "Green", "Blue"),
           ylab = "Percent Negative")

pirateplot(measurement ~ load + type + type:val, data = rate.data_long, inf.method = "ci")
pirateplot(`Percent Negative` ~ type, data = rate.data_long, inf.method = "ci")
View(rate.data_long)
bound.data2 <- bound.data
?anovaBF
piratepal(palette = "all")

#BF10 (type * val interaction)#
3.575921e+256/(1.55547e+255+.1313153)

#BF10 (load * val interaction)#
7.196383e+252/(1.55547e+255+0.09509338)
s1.data_long <- gather(session1.ratings.table, key = valence, value = "Percent Negative",  
                         ang_rate, hap_rate, sur_rate)
pirateplot(`Percent Negative` ~ condition, data = s1.data_long)

t.test(MT.data.rating.table$hi.mem, MT.data.rating.table$lo.mem, paired = TRUE)

### calculate change score ###


mem.data_long$Accuracy <- mem.data_long$Accuracy*100
mean(mem.data_long$Accuracy)
ggplot(data = mem.data_long, aes(x = Memory, y = Accuracy)) +
  geom_bar(stat = "Identity") +
  stat = "summary" +
  fun.y = "mean"
mem.data_long <- 

  