library(dplyr)
library(ggpubr)

### checking normality ###

### surprise ###
shapiro.test(MT.data.rating.table$lo.emo.sur_rate)
ggdensity(MT.data.rating.table$lo.emo.sur_rate)
ggqqplot(MT.data.rating.table$lo.emo.sur_rate)

shapiro.test(MT.data.rating.table$hi.emo.sur_rate)
ggdensity(MT.data.rating.table$hi.emo.sur_rate)
ggqqplot(MT.data.rating.table$hi.emo.sur_rate)

shapiro.test(MT.data.rating.table$lo.neu.sur_rate)
ggdensity(MT.data.rating.table$lo.neu.sur_rate)
ggqqplot(MT.data.rating.table$lo.neu.sur_rate)

shapiro.test(MT.data.rating.table$hi.neu.sur_rate)
ggdensity(MT.data.rating.table$hi.neu.sur_rate)
ggqqplot(MT.data.rating.table$hi.neu.sur_rate)

### happy ###
shapiro.test(MT.data.rating.table$lo.emo.hap_rate)
ggdensity(MT.data.rating.table$lo.emo.hap_rate)
ggqqplot(MT.data.rating.table$lo.emo.hap_rate)

shapiro.test(MT.data.rating.table$hi.emo.hap_rate)
ggdensity(MT.data.rating.table$hi.emo.hap_rate)
ggqqplot(MT.data.rating.table$hi.emo.hap_rate)

shapiro.test(MT.data.rating.table$lo.neu.hap_rate)
ggdensity(MT.data.rating.table$lo.neu.hap_rate)
ggqqplot(MT.data.rating.table$lo.neu.hap_rate)

shapiro.test(MT.data.rating.table$hi.neu.hap_rate)
ggdensity(MT.data.rating.table$hi.neu.hap_rate)
ggqqplot(MT.data.rating.table$hi.neu.hap_rate)

### angry ###
shapiro.test(MT.data.rating.table$lo.emo.ang_rate)
ggdensity(MT.data.rating.table$lo.emo.ang_rate)
ggqqplot(MT.data.rating.table$lo.emo.ang_rate)

shapiro.test(MT.data.rating.table$hi.emo.ang_rate)
ggdensity(MT.data.rating.table$hi.emo.ang_rate)
ggqqplot(MT.data.rating.table$hi.emo.ang_rate)

shapiro.test(MT.data.rating.table$lo.neu.ang_rate)
ggdensity(MT.data.rating.table$lo.neu.ang_rate)
ggqqplot(MT.data.rating.table$lo.neu.ang_rate)

shapiro.test(MT.data.rating.table$hi.neu.ang_rate)
ggdensity(MT.data.rating.table$hi.neu.ang_rate)
ggqqplot(MT.data.rating.table$hi.neu.ang_rate)


### RT ###
### surprise-negative ###
shapiro.test(MT.data.rating.table$lo.emo.sur_n_RT)
ggdensity(MT.data.rating.table$lo.emo.sur_n_RT)
ggqqplot(MT.data.rating.table$lo.emo.sur_n_RT)

shapiro.test(MT.data.rating.table$hi.emo.sur_n_RT)
ggdensity(MT.data.rating.table$hi.emo.sur_n_RT)
ggqqplot(MT.data.rating.table$hi.emo.sur_n_RT)

shapiro.test(MT.data.rating.table$lo.neu.sur_n_RT)
ggdensity(MT.data.rating.table$lo.neu.sur_n_RT)
ggqqplot(MT.data.rating.table$lo.neu.sur_n_RT)

shapiro.test(MT.data.rating.table$hi.neu.sur_n_RT)
ggdensity(MT.data.rating.table$hi.neu.sur_n_RT)
ggqqplot(MT.data.rating.table$hi.neu.sur_n_RT)

### surprise positive ###
shapiro.test(MT.data.rating.table$lo.emo.sur_p_RT)
ggdensity(MT.data.rating.table$lo.emo.sur_p_RT)
ggqqplot(MT.data.rating.table$lo.emo.sur_p_RT)

shapiro.test(MT.data.rating.table$hi.emo.sur_p_RT)
ggdensity(MT.data.rating.table$hi.emo.sur_p_RT)
ggqqplot(MT.data.rating.table$hi.emo.sur_p_RT)

shapiro.test(MT.data.rating.table$lo.neu.sur_p_RT)
ggdensity(MT.data.rating.table$lo.neu.sur_p_RT)
ggqqplot(MT.data.rating.table$lo.neu.sur_p_RT)

shapiro.test(MT.data.rating.table$hi.neu.sur_p_RT)
ggdensity(MT.data.rating.table$hi.neu.sur_p_RT)
ggqqplot(MT.data.rating.table$hi.neu.sur_p_RT)

### happy ###
shapiro.test(MT.data.rating.table$lo.emo.hap_RT)
ggdensity(MT.data.rating.table$lo.emo.hap_RT)
ggqqplot(MT.data.rating.table$lo.emo.hap_RT)

shapiro.test(MT.data.rating.table$hi.emo.hap_RT)
ggdensity(MT.data.rating.table$hi.emo.hap_RT)
ggqqplot(MT.data.rating.table$hi.emo.hap_RT)

shapiro.test(MT.data.rating.table$lo.neu.hap_RT)
ggdensity(MT.data.rating.table$lo.neu.hap_RT)
ggqqplot(MT.data.rating.table$lo.neu.hap_RT)

shapiro.test(MT.data.rating.table$hi.neu.hap_RT)
ggdensity(MT.data.rating.table$hi.neu.hap_RT)
ggqqplot(MT.data.rating.table$hi.neu.hap_RT)

### angry ###
shapiro.test(MT.data.rating.table$lo.emo.ang_RT)
ggdensity(MT.data.rating.table$lo.emo.ang_RT)
ggqqplot(MT.data.rating.table$lo.emo.ang_RT)

shapiro.test(MT.data.rating.table$hi.emo.ang_RT)
ggdensity(MT.data.rating.table$hi.emo.ang_RT)
ggqqplot(MT.data.rating.table$hi.emo.ang_RT)

shapiro.test(MT.data.rating.table$lo.neu.ang_RT)
ggdensity(MT.data.rating.table$lo.neu.ang_RT)
ggqqplot(MT.data.rating.table$lo.neu.ang_RT)

shapiro.test(MT.data.rating.table$hi.neu.ang_RT)
ggdensity(MT.data.rating.table$hi.neu.ang_RT)
ggqqplot(MT.data.rating.table$hi.neu.ang_RT)


### MAD ####
### surprise-negative ###
shapiro.test(by.surp.wideMAD$lo.em.sur.MAD.n)
ggdensity(by.surp.wideMAD$lo.em.sur.MAD.n)
ggqqplot(by.surp.wideMAD$lo.em.sur.MAD.n)

shapiro.test(by.surp.wideMAD$hi.em.sur.MAD.n)
ggdensity(by.surp.wideMAD$hi.em.sur.MAD.n)
ggqqplot(by.surp.wideMAD$hi.em.sur.MAD.n)

shapiro.test(by.surp.wideMAD$hi.neu.sur.MAD.n)
ggdensity(by.surp.wideMAD$hi.neu.sur.MAD.n)
ggqqplot(by.surp.wideMAD$hi.neu.sur.MAD.n)

shapiro.test(by.surp.wideMAD$lo.neu.sur.MAD.n)
ggdensity(by.surp.wideMAD$lo.neu.sur.MAD.n)
ggqqplot(by.surp.wideMAD$lo.neu.sur.MAD.n)

### surprise-positive ###
shapiro.test(by.surp.wideMAD$lo.em.sur.MAD.p)
ggdensity(by.surp.wideMAD$lo.em.sur.MAD.p)
ggqqplot(by.surp.wideMAD$lo.em.sur.MAD.p)

shapiro.test(by.surp.wideMAD$hi.em.sur.MAD.p)
ggdensity(by.surp.wideMAD$hi.em.sur.MAD.p)
ggqqplot(by.surp.wideMAD$hi.em.sur.MAD.p)

shapiro.test(by.surp.wideMAD$hi.neu.sur.MAD.p)
ggdensity(by.surp.wideMAD$hi.neu.sur.MAD.p)
ggqqplot(by.surp.wideMAD$hi.neu.sur.MAD.p)

shapiro.test(by.surp.wideMAD$lo.neu.sur.MAD.p)
ggdensity(by.surp.wideMAD$lo.neu.sur.MAD.p)
ggqqplot(by.surp.wideMAD$lo.neu.sur.MAD.p)
