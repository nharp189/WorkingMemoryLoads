### remove bad sub ###
bound.data <- bound.data[-38,]
bound.data$subjID <- as.factor(bound.data$subjID)
long.data <- gather(bound.data, Condition, Correct,
                    mem.cor.yes, mem.cor.no, emo.mem, neu.mem,
                    lo.mem, hi.mem)
pirateplot(Correct ~ Condition, data = long.data)

t.test(bound.data$mem.cor.yes, bound.data$mem.cor.no, paired = TRUE)

t.test(bound.data$emo.mem, bound.data$neu.mem, paired = TRUE)

t.test(bound.data$hi.mem, bound.data$lo.mem, paired = TRUE)
