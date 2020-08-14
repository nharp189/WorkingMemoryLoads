### set working directory to MouseTrap Demo folder ###
setwd("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads")

qualtrics <- read.csv("Data/qualtrics_2019.11.12.csv")

### read data file ###
data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Cleaned_Data/Final.Data.csv_2019-09-06_12-18-56.csv")

qualtrics <- qualtrics[-c(1:2), ]
rownames(qualtrics) <- NULL

### drop no shows ###
qualtrics <- subset(qualtrics, !Q2 %in% c("71004", "71022", "71057"))

qualtrics %>% count(Q4, sort = T, "sex")

class(qualtrics$Q3)
qualtrics$Q3 <- as.character(qualtrics$Q3)
qualtrics$Q3 <- as.integer(qualtrics$Q3)
mean(qualtrics$Q3)
sd(qualtrics$Q3)
