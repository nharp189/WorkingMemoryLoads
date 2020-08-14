MT.data$data$stim.recode2 <- str_sub(MT.data$data$stim, 22, 25)

iaps.norm <- read_excel("~/Documents/Nick-Grad/Neta_Lab/Stimuli/IAPS_ratings.xls")

### find the valence cutoffs for pos and neg in the 144 stim... 
min(stim.data$Val_Mn[which(stim.data$Condition == "POS")])
max(stim.data$Val_Mn[which(stim.data$Condition == "POS")])
min(stim.data$Val_Mn[which(stim.data$Condition == "NEG")])
max(stim.data$Val_Mn[which(stim.data$Condition == "NEG")])
stim.data <- read.xlsx("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/IAPS_Stim_List.xlsx")
### ok, now label the "no" images based off the valence cutoffs above... 
iaps.norm$CONDITION <- ifelse(iaps.norm$valmn >= 5.07, "POS",
                              ifelse(iaps.norm$valmn <= 4.32, "NEG",
                                     "NEU"))


MT.data$data$probeVal <- ifelse(MT.data$data$stim.recode2 %in% iaps.norm$IAPS,
                                iaps.norm$CONDITION, "")
names(MT.data$data)[names(MT.data$data)=="stim.recode2"] <- "IAPS"
MT.data$data <- merge(MT.data$data, iaps.norm[, c("IAPS", "CONDITION")], by = "IAPS")
View(MT.data$data)
MT.data$data$probeVal <- MT.data$data$CONDITION

MT.data$data <- subset(MT.data$data, MT.data$data$trialtype == "memory")
### fix 9635.2 discrepancy 
MT.data$data$probeVal <- ifelse(MT.data$data$IAPS == 9635, "NEG",
                                MT.data$data$probeVal)
MT.data$data$mem.cor <- as.numeric(MT.data$data$mem.cor)
MT.data.rating.table2 <- (ddply(MT.data$data, "subjID", summarise, 
                                emo.acc.Pos = mean(mem.cor[which(type == "EMO" & CONDITION == "POS")], na.rm = T),
                                emo.acc.Neg = mean(mem.cor[which(type == "EMO" & CONDITION == "NEG")], na.rm = T)))

t.test(MT.data.rating.table2$emo.acc.Pos,
       MT.data.rating.table2$emo.acc.Neg, paired = T)
View(MT.data$data)
mean(MT.data.rating.table2$emo.acc.Pos)
