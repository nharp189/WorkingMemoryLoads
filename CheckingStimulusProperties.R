### Run the data cleaning MERP script to get the MT.data object... ###

### count new IAPS images in the memory probe condition ###
## flag trials after face trials ###
for(i in 1:(nrow(MT.data$data)-1)) {
  MT.data$data$flag[i+1] <- ifelse(MT.data$data$cond.correct[i] %in% c("Surprise",
                                                                       "Angry", "Happy"),
                                   1, 0)
}

### remove flagged trials ###
MT.data$data <- subset(MT.data$data, MT.data$data$flag == 1)
View(MT.data$data)
no <- subset(MT.data$data, MT.data$data$condition == "No" & MT.data$data$flag == 1)
yes <- subset(MT.data$data, MT.data$data$condition == "Yes" & MT.data$data$flag == 1)
count(yes$subjID)

unique(no$stim) # 63 unique

### read in the 144 images in the matrices to compare which were repeated in the probe ###
library(openxlsx)
stim.data <- read.xlsx("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/IAPS_Stim_List.xlsx")
MT.data$data$stim.recode <- str_sub(MT.data$data$stim, 22, 25)
MT.data$data$stim.recode
MT.data$data$stimflag <- ifelse(c(MT.data$data$stim) %in% c(MT.data$data$stim.recode),
                                1, 0)
temp <- mt_subset(MT.data, MT.data$data$stimflag == 1) # empty dataframe, none of the "no" images were in the 144 IAPS. 

### now want to equate pos vs. neg probe images... ###
iaps.norm <- read_excel("~/Documents/Nick-Grad/Neta_Lab/Stimuli/IAPS_ratings.xls")

### subset to only include "no" images from above ###
no$stim.recode <- str_sub(no$stim, 22, 25)
iaps.norm <- subset(iaps.norm, iaps.norm$IAPS %in% no$stim.recode)

### find the valence cutoffs for pos and neg in the 144 stim... 
min(stim.data$Val_Mn[which(stim.data$Condition == "POS")])
max(stim.data$Val_Mn[which(stim.data$Condition == "POS")])
min(stim.data$Val_Mn[which(stim.data$Condition == "NEG")])
max(stim.data$Val_Mn[which(stim.data$Condition == "NEG")])

### ok, now label the "no" images based off the valence cutoffs above... 
iaps.norm$CONDITION <- ifelse(iaps.norm$valmn >= 5.07, "POS",
                              ifelse(iaps.norm$valmn <= 4.32, "NEG",
                                     "NEU"))

source("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/wilcox_test.R")

shapiro.test(iaps.norm$aromn[which(iaps.norm$CONDITION == "POS")])
shapiro.test(iaps.norm$aromn[which(iaps.norm$CONDITION == "NEG")])

res <- wilcox_test(iaps.norm$aromn[which(iaps.norm$CONDITION == "POS")],
            iaps.norm$aromn[which(iaps.norm$CONDITION == "NEG")])
res$z_val
res$p.value
