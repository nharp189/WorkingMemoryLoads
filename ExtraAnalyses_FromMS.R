## Reaction times
```{r analyze RTs, include = FALSE}
## assess normality ###
shapiro.test(data$lo.neu.sur_RT)
shapiro.test(data$hi.neu.sur_RT)
shapiro.test(data$lo.emo.sur_RT)
shapiro.test(data$hi.emo.sur_RT)

### make long ###
data.long <- gather(data, key = "Condition", value = "ReactionTime",
                    lo.neu.sur_RT, hi.neu.sur_RT,
                    lo.emo.sur_RT, hi.emo.sur_RT)

### make factors ###
data.long$Load <- ifelse(data.long$Condition == "lo.neu.sur_RT", "Low",
                         ifelse(data.long$Condition == "lo.emo.sur_RT", "Low",
                                ifelse(data.long$Condition == "hi.neu.sur_RT", "High",
                                       ifelse(data.long$Condition == "hi.emo.sur_RT", "High", ""))))

data.long$Type <- ifelse(data.long$Condition == "lo.neu.sur_RT", "Neutral",
                         ifelse(data.long$Condition == "lo.emo.sur_RT", "Emotional",
                                ifelse(data.long$Condition == "hi.neu.sur_RT", "Neutral",
                                       ifelse(data.long$Condition == "hi.emo.sur_RT", "Emotional", ""))))

### make w/in subjs factors ###
data.long$subjID <- as.factor(data.long$subjID)

### traditional ANOVA ###
RTaov <- with(data.long,
              aov(ReactionTime ~ (Load * Type) +
                    Error(subjID / (Load * Type))), contrasts = contr.sum())
summary(RTaov)
RTaov <- tidy(RTaov)
```

To assses for normality, the data were first tested with Shapiro-Wilk's test. All conditions appeared to be sampled from normal distributions (p's > .300). 
A Load (Low, High) X Type (Neutral, Emotional) repeated measures ANOVA was used to assess differences in the RTs. There was a trend towards a main effect of
domain, F(`r (RTaov$df[which(RTaov$term == "Type")])`, `r (RTaov$df[which(RTaov$stratum == "subjID")])`) = `r printnum(RTaov$statistic[which(RTaov$term == "Type")])`,
p = `r printnum(RTaov$p.value[which(RTaov$term == "Type")])`. There was no effect of load, F(`r (RTaov$df[which(RTaov$term == "Load")])`, `r (RTaov$df[which(RTaov$stratum
== "subjID")])`) = `r printnum(RTaov$statistic[which(RTaov$term == "Load")])`, p = `r printnum(RTaov$p.value[which(RTaov$term == "Load")])`, nor any interaction between 
load and domain, F(`r (RTaov$df[which(RTaov$term == "Load:Type")])`, `r (RTaov$df[which(RTaov$stratum == "subjID")])`) = `r printnum(RTaov$statistic[which(RTaov$term == 
"Load:Type")])`, p = `r printnum(RTaov$p.value[which(RTaov$term == "Load:Type")])`. 

```{r plot RT data, include = TRUE}
ggplot(data.long, aes(Condition, `ReactionTime`, fill = as.factor(Condition))) +
  geom_bar(stat = "summary", fun.y = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1)
```

Next, we tested for reaction time differences between positive and negative interpretations of surprised facial expressions within each working memory load condition. A large proportion of the conditions were not normally distributed, thus we used paired-sample Wilcoxon signed rank tests for assessing differences between positive and negative ratings of surprise RTs. This difference was significant *only* for the low emotional load trials, with surprise rated as positive taking signficantly longer than surprise rated as negative (`r printp(lo.neu.RT.pvn$p.value)`). All other comparisons were not significant (p's > .200). Further, this effect survived Bonferroni correction (p = `r printp(.05 / 4)`). 

