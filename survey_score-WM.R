### set wd ###
### make this your working directory (file path) ###
setwd("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/")

### load packages ###
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(psych)){
  install.packages("psych")
  library(psych)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
### import data ###
data <- read.csv("data/qualtrics_data_20190607.csv")

### preview data (optional) ###
# View(data)

### remove junk rows ###
### typically row 1 and 2 in qualtrics csv files ###
data <- data[-(c(1,2)),]

### reset row names ###
rownames(data) <- NULL

### create column values ###
firstcol <- "erq_1"
lastcol <- "erq_10"
ERQ.data <- select(data, firstcol:lastcol)


### convert choice text to numeric ###
for(i in 1:ncol(ERQ.data)) {
  ERQ.data[,i] <- recode(ERQ.data[,i], 
                         "1-Strongly disagree" = 1,
                         "2" = 2, "3" = 3, "4-Neutral" = 4,
                         "5" = 5, "6" = 6, "7-Strongly agree" = 7)
}

### add items for participant scores and create column in survey.data ###
ERQ.data$ERQ_CR <-  (rowSums(ERQ.data[,c(1,3,5,7,8,10)], 
                             na.rm = FALSE)  / 6 )

ERQ.data$ERQ_ES <- ( rowSums(ERQ.data[,c(2,4,6,9)], 
                             na.rm = FALSE) / 4 )


### create NEOFFI data frame ###
### create column values ###
firstcol <- "NEO_only_1"
lastcol <- "NEO_only_60"
NEO.data <- select(data, firstcol:lastcol)


### convert choice text to numeric ###
for(i in 1:ncol(NEO.data)) {
  NEO.data[,i] <- recode(NEO.data[,i], 
                         "Strongly disagree" = 0, "Disagree" = 1, "Neither agree nor disagree" = 2, 
                         "Agree" = 3, "Strongly agree" = 4)
}

key.NEO <- c(-1,1,1,1,1,1,1,1,-1,1,1,-1,1,-1,-1,-1,1,-1,-1,1,1,1,-1,-1,1,1,-1,-1,1,-1,-1,1,-1,1,1,1,1,1,-1,1,1,-1,1,-1,-1,-1,1,-1,1,1,1,1,1,-1,-1,1,-1,1,-1,1)
NEO.data <- reverse.code(key.NEO, NEO.data, mini=0, maxi=4)

NEO.data <- as.data.frame(NEO.data)

NEO.data$NEO.N <- rowSums(NEO.data[,c(1,6,11,16,21,26,31,36,41,46,51,56)],na.rm = FALSE)
NEO.data$NEO.E <- rowSums(NEO.data[,c(2,7,12,17,22,27,32,37,42,47,52,57)],na.rm = FALSE)
NEO.data$NEO.O <- rowSums(NEO.data[,c(3,8,13,18,23,28,33,38,43,48,53,58)],na.rm = FALSE)
NEO.data$NEO.A <- rowSums(NEO.data[,c(4,9,14,19,24,29,34,39,44,49,54,59)],na.rm = FALSE)
NEO.data$NEO.C <- rowSums(NEO.data[,c(5,10,15,20,25,30,35,40,45,50,55,60)],na.rm = FALSE)
# NEO.data$mean.NEO.N <- rowMeans(NEO.data[,c(1,6,11,16,21,26,31,36,41,46,51,56)],na.rm = FALSE)
# NEO.data$mean.NEO.E <- rowMeans(NEO.data[,c(2,7,12,17,22,27,32,37,42,47,52,57)],na.rm = FALSE)
# NEO.data$mean.NEO.O <- rowMeans(NEO.data[,c(3,8,13,18,23,28,33,38,43,48,53,58)],na.rm = FALSE)
# NEO.data$mean.NEO.A <- rowMeans(NEO.data[,c(4,9,14,19,24,29,34,39,44,49,54,59)],na.rm = FALSE)
# NEO.data$mean.NEO.C <- rowMeans(NEO.data[,c(5,10,15,20,25,30,35,40,45,50,55,60)],na.rm = FALSE)

### Five Facet Mindfulness Questionnaire ###
firstcol <- "ffmq_1"
lastcol <- "ffmq_39"
FFMQ.data <- select(data, firstcol:lastcol)

## note the \n for page break... Nick, don't put this crap in qualtrics
## variable names  in the future!!! ###
for(i in 1:ncol(FFMQ.data)) {
  FFMQ.data[,i] <- recode(FFMQ.data[,i],
                          "1" = 1,
                          "2" = 2,
                          "3" = 3,
                          "4" = 4,
                          "5" = 5)
}

key.FFMQ <- c(1,1,-1,1,-1,1,1,-1,1,-1,1,-1,-1,-1,1,-1,-1,-1,1,1,1,-1,-1,1,-1,1,1,-1,1,-1,1,1,1,-1,-1,1,1,-1,-1)
FFMQ.data <- reverse.code(key.FFMQ, FFMQ.data, mini = 1, maxi = 5)
FFMQ.data <- as.data.frame(FFMQ.data)
FFMQ.data$FFMQ.Ob <- rowMeans(FFMQ.data[,c(1,6,11,15,20,26,31,36)])
FFMQ.data$FFMQ.Ds <- rowMeans(FFMQ.data[,c(2,7,12,16,22,27,32,37)])
FFMQ.data$FFMQ.Aa <- rowMeans(FFMQ.data[,c(5,8,13,18,23,28,34,38)])
FFMQ.data$FFMQ.Nj <- rowMeans(FFMQ.data[,c(3,10,14,17,25,30,35,39)])
FFMQ.data$FFMQ.Nr <- rowMeans(FFMQ.data[,c(4,9,19,21,24,29,33)])

### create column values ###
firstcol <- "stai.s_1"
lastcol <- "stai.t_20"
STAI.data <- select(data, firstcol:lastcol)

### create subset data frames for ID, State, and Trait ###
firstcol_state <- "stai.s_1"
lastcol_state <- "stai.s_20"
firstcol_trait <- "stai.t_1"
lastcol_trait <-"stai.t_20"
df_state <- select(STAI.data,firstcol_state:lastcol_state)
df_trait <- select(STAI.data,firstcol_trait:lastcol_trait)

### convert choice text to numeric ###
for(i in 1:20) {
  df_state[,i] <- recode(df_state[,i], 
                         "Not at all" = 1, "Somewhat" = 2, 
                         "Moderately so" = 3, "Very much so" = 4)
}
for(i in 1:20) {
  df_trait[,i] <- recode(df_trait[,i], 
                         "Almost never" = 1, "Sometimes" = 2, 
                         "Often" = 3, "Almost always" = 4)
}

### reverse scoring STAI-S ###
key.STAIS <- c(-1,-1,1,1,-1,1,1,-1,1,-1,-1,1,1,1,-1,-1,1,1,-1,-1)
key.STAIT <- c(-1,1,-1,1,1,-1,-1,1,1,-1,1,1,-1,-1,1,-1,1,1,-1,1)
df_state <- reverse.code(key.STAIS,df_state,mini=1,maxi=4)
df_trait <- reverse.code(key.STAIT,df_trait,mini=1,maxi=4)

### recombine split datasets and clean up workspace ###
STAI.data <-cbind(df_state,df_trait)
rm(firstcol,lastcol,firstcol_state,firstcol_trait,lastcol_state,lastcol_trait,
   df_state,df_trait, key.STAIS, key.STAIT, i)

### add scores ###
STAI.data <- as.data.frame(STAI.data)
STAI.data$STAI.S <- (rowSums(STAI.data[,c(1:20)], na.rm = FALSE))
STAI.data$STAI.T <- (rowSums(STAI.data[,c(21:40)], na.rm = FALSE))

## EQ ###
EQ.data <- data[,c(155:182)]

### create column values ###
firstcol <- "eq_1"
lastcol <- "eq_28"
EQ.data <- select(data, firstcol:lastcol)

for(i in 1:ncol(EQ.data)) {
  EQ.data[,i] <- recode(EQ.data[,i], 
                        "Strongly agree" = 1, "Slightly agree" = 2, 
                        "Slightly disagree" = 3, "Strongly disagree" = 4)
}

key.EQ <- c(1,-1,1,-1,-1,-1,1,-1,1,1,1,-1,-1,-1,1,1,1,1,1,1,-1,-1,1,1,1,1,1,1)
EQ.data <- reverse.code(key.EQ, EQ.data, mini=1, maxi=4)
### code to 0, 1, 2, 2 ###
for(i in 1:28) {
  EQ.data[,i] <- recode(EQ.data[,i], `1`=2,`2`=1,`3`=0,`4`=0)
}
EQ.data <- as.data.frame(EQ.data)

EQ.data$EQ.full.sum <- rowSums(EQ.data, na.rm = FALSE)
EQ.data$EQ.CE <- rowMeans(EQ.data[,c(1,7,10,11,16,17,20,23,24,25,27)],na.rm = FALSE)
EQ.data$EQ.ER <- rowMeans(EQ.data[,c(3,8,9,12,13,14,18,19,21,22,28)],na.rm = FALSE)
EQ.data$EQ.SS <- rowMeans(EQ.data[,c(2,4,5,6,15,26)],na.rm = FALSE)

scored <- EQ.data[,c(29:32)]
scored <- cbind(scored, FFMQ.data[,c(39:44)])
scored <- cbind(scored, ERQ.data[,c(11:12)])
scored <- cbind(scored, NEO.data[,c(61:65)])
scored <- cbind(scored, data$Q2)
scored <- cbind(scored, data$Q3)
scored <- cbind(scored, data$Q4)


inperson.data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_data/Qualtrics/ExMBSRS1.csv")
inperson.data <- inperson.data[-c(1:2),]
{
  ### reset row names ###
  rownames(inperson.data) <- NULL
  
  ### create column values ###
  firstcol <- "erq_1"
  lastcol <- "erq_10"
  ERQ.inperson.data <- select(inperson.data, firstcol:lastcol)
  
  
  ### convert choice text to numeric ###
  for(i in 1:ncol(ERQ.inperson.data)) {
    ERQ.inperson.data[,i] <- recode(ERQ.inperson.data[,i], 
                           "1-Strongly disagree" = 1,
                           "2" = 2, "3" = 3, "4-Neutral" = 4,
                           "5" = 5, "6" = 6, "7-Strongly agree" = 7)
  }
  
  ### add items for participant scores and create column in survey.inperson.data ###
  ERQ.inperson.data$ERQ_CR <-  (rowSums(ERQ.inperson.data[,c(1,3,5,7,8,10)], 
                               na.rm = FALSE)  / 6 )
  
  ERQ.inperson.data$ERQ_ES <- ( rowSums(ERQ.inperson.data[,c(2,4,6,9)], 
                               na.rm = FALSE) / 4 )
  
  
  ### create NEOFFI inperson.data frame ###
  ### create column values ###
  firstcol <- "NEO_only_1"
  lastcol <- "NEO_only_60"
  NEO.inperson.data <- select(inperson.data, firstcol:lastcol)
  
  
  ### convert choice text to numeric ###
  for(i in 1:ncol(NEO.inperson.data)) {
    NEO.inperson.data[,i] <- recode(NEO.inperson.data[,i], 
                           "Strongly disagree" = 0, "Disagree" = 1, "Neither agree nor disagree" = 2, 
                           "Agree" = 3, "Strongly agree" = 4)
  }
  
  key.NEO <- c(-1,1,1,1,1,1,1,1,-1,1,1,-1,1,-1,-1,-1,1,-1,-1,1,1,1,-1,-1,1,1,-1,-1,1,-1,-1,1,-1,1,1,1,1,1,-1,1,1,-1,1,-1,-1,-1,1,-1,1,1,1,1,1,-1,-1,1,-1,1,-1,1)
  NEO.inperson.data <- reverse.code(key.NEO, NEO.inperson.data, mini=0, maxi=4)
  
  NEO.inperson.data <- as.inperson.data.frame(NEO.inperson.data)
  
  NEO.inperson.data$NEO.N <- rowSums(NEO.inperson.data[,c(1,6,11,16,21,26,31,36,41,46,51,56)],na.rm = FALSE)
  NEO.inperson.data$NEO.E <- rowSums(NEO.inperson.data[,c(2,7,12,17,22,27,32,37,42,47,52,57)],na.rm = FALSE)
  NEO.inperson.data$NEO.O <- rowSums(NEO.inperson.data[,c(3,8,13,18,23,28,33,38,43,48,53,58)],na.rm = FALSE)
  NEO.inperson.data$NEO.A <- rowSums(NEO.inperson.data[,c(4,9,14,19,24,29,34,39,44,49,54,59)],na.rm = FALSE)
  NEO.inperson.data$NEO.C <- rowSums(NEO.inperson.data[,c(5,10,15,20,25,30,35,40,45,50,55,60)],na.rm = FALSE)
  # NEO.inperson.data$mean.NEO.N <- rowMeans(NEO.inperson.data[,c(1,6,11,16,21,26,31,36,41,46,51,56)],na.rm = FALSE)
  # NEO.inperson.data$mean.NEO.E <- rowMeans(NEO.inperson.data[,c(2,7,12,17,22,27,32,37,42,47,52,57)],na.rm = FALSE)
  # NEO.inperson.data$mean.NEO.O <- rowMeans(NEO.inperson.data[,c(3,8,13,18,23,28,33,38,43,48,53,58)],na.rm = FALSE)
  # NEO.inperson.data$mean.NEO.A <- rowMeans(NEO.inperson.data[,c(4,9,14,19,24,29,34,39,44,49,54,59)],na.rm = FALSE)
  # NEO.inperson.data$mean.NEO.C <- rowMeans(NEO.inperson.data[,c(5,10,15,20,25,30,35,40,45,50,55,60)],na.rm = FALSE)
  
  ### Five Facet Mindfulness Questionnaire ###
  firstcol <- "ffmq_1"
  lastcol <- "ffmq_39"
  FFMQ.inperson.data <- select(inperson.data, firstcol:lastcol)
  
  ## note the \n for page break... Nick, don't put this crap in qualtrics
  ## variable names  in the future!!! ###
  for(i in 1:ncol(FFMQ.inperson.data)) {
    FFMQ.inperson.data[,i] <- recode(FFMQ.inperson.data[,i],
                            "1" = 1,
                            "2" = 2,
                            "3" = 3,
                            "4" = 4,
                            "5" = 5)
  }
  
  key.FFMQ <- c(1,1,-1,1,-1,1,1,-1,1,-1,1,-1,-1,-1,1,-1,-1,-1,1,1,1,-1,-1,1,-1,1,1,-1,1,-1,1,1,1,-1,-1,1,1,-1,-1)
  FFMQ.inperson.data <- reverse.code(key.FFMQ, FFMQ.inperson.data, mini = 1, maxi = 5)
  FFMQ.inperson.data <- as.inperson.data.frame(FFMQ.inperson.data)
  FFMQ.inperson.data$FFMQ.Ob <- rowMeans(FFMQ.inperson.data[,c(1,6,11,15,20,26,31,36)])
  FFMQ.inperson.data$FFMQ.Ds <- rowMeans(FFMQ.inperson.data[,c(2,7,12,16,22,27,32,37)])
  FFMQ.inperson.data$FFMQ.Aa <- rowMeans(FFMQ.inperson.data[,c(5,8,13,18,23,28,34,38)])
  FFMQ.inperson.data$FFMQ.Nj <- rowMeans(FFMQ.inperson.data[,c(3,10,14,17,25,30,35,39)])
  FFMQ.inperson.data$FFMQ.Nr <- rowMeans(FFMQ.inperson.data[,c(4,9,19,21,24,29,33)])
  
  ### create column values ###
  firstcol <- "stai.s_1"
  lastcol <- "stai.t_20"
  STAI.inperson.data <- select(inperson.data, firstcol:lastcol)
  
  ### create subset inperson.data frames for ID, State, and Trait ###
  firstcol_state <- "stai.s_1"
  lastcol_state <- "stai.s_20"
  firstcol_trait <- "stai.t_1"
  lastcol_trait <-"stai.t_20"
  df_state <- select(STAI.inperson.data,firstcol_state:lastcol_state)
  df_trait <- select(STAI.inperson.data,firstcol_trait:lastcol_trait)
  
  ### convert choice text to numeric ###
  for(i in 1:20) {
    df_state[,i] <- recode(df_state[,i], 
                           "Not at all" = 1, "Somewhat" = 2, 
                           "Moderately so" = 3, "Very much so" = 4)
  }
  for(i in 1:20) {
    df_trait[,i] <- recode(df_trait[,i], 
                           "Almost never" = 1, "Sometimes" = 2, 
                           "Often" = 3, "Almost always" = 4)
  }
  
  ### reverse scoring STAI-S ###
  key.STAIS <- c(-1,-1,1,1,-1,1,1,-1,1,-1,-1,1,1,1,-1,-1,1,1,-1,-1)
  key.STAIT <- c(-1,1,-1,1,1,-1,-1,1,1,-1,1,1,-1,-1,1,-1,1,1,-1,1)
  df_state <- reverse.code(key.STAIS,df_state,mini=1,maxi=4)
  df_trait <- reverse.code(key.STAIT,df_trait,mini=1,maxi=4)
  
  ### recombine split inperson.datasets and clean up workspace ###
  STAI.inperson.data <-cbind(df_state,df_trait)
  rm(firstcol,lastcol,firstcol_state,firstcol_trait,lastcol_state,lastcol_trait,
     df_state,df_trait, key.STAIS, key.STAIT, i)
  
  ### add scores ###
  STAI.inperson.data <- as.inperson.data.frame(STAI.inperson.data)
  STAI.inperson.data$STAI.S <- (rowSums(STAI.inperson.data[,c(1:20)], na.rm = FALSE))
  STAI.inperson.data$STAI.T <- (rowSums(STAI.inperson.data[,c(21:40)], na.rm = FALSE))
  
  ## EQ ###
  EQ.inperson.data <- inperson.data[,c(155:182)]
  
  ### create column values ###
  firstcol <- "eq_1"
  lastcol <- "eq_28"
  EQ.inperson.data <- select(inperson.data, firstcol:lastcol)
  
  for(i in 1:ncol(EQ.inperson.data)) {
    EQ.inperson.data[,i] <- recode(EQ.inperson.data[,i], 
                          "Strongly agree" = 1, "Slightly agree" = 2, 
                          "Slightly disagree" = 3, "Strongly disagree" = 4)
  }
  
  key.EQ <- c(1,-1,1,-1,-1,-1,1,-1,1,1,1,-1,-1,-1,1,1,1,1,1,1,-1,-1,1,1,1,1,1,1)
  EQ.inperson.data <- reverse.code(key.EQ, EQ.inperson.data, mini=1, maxi=4)
  ### code to 0, 1, 2, 2 ###
  for(i in 1:28) {
    EQ.inperson.data[,i] <- recode(EQ.inperson.data[,i], `1`=2,`2`=1,`3`=0,`4`=0)
  }
  EQ.inperson.data <- as.inperson.data.frame(EQ.inperson.data)
  
  EQ.inperson.data$EQ.full.sum <- rowSums(EQ.inperson.data, na.rm = FALSE)
  EQ.inperson.data$EQ.CE <- rowMeans(EQ.inperson.data[,c(1,7,10,11,16,17,20,23,24,25,27)],na.rm = FALSE)
  EQ.inperson.data$EQ.ER <- rowMeans(EQ.inperson.data[,c(3,8,9,12,13,14,18,19,21,22,28)],na.rm = FALSE)
  EQ.inperson.data$EQ.SS <- rowMeans(EQ.inperson.data[,c(2,4,5,6,15,26)],na.rm = FALSE)
  
}


s3.surveys <- read.csv("MBSR_Online_S3_20190715.csv")

{
  ### preview s3.surveys (optional) ###
  # View(s3.surveys)
  
  ### remove junk rows ###
  ### typically row 1 and 2 in qualtrics csv files ###
  s3.surveys <- s3.surveys[-(c(1,2)),]
  
  ### reset row names ###
  rownames(s3.surveys) <- NULL
  
  ### create column values ###
  firstcol <- "erq_1"
  lastcol <- "erq_10"
  ERQ.s3.surveys <- select(s3.surveys, firstcol:lastcol)
  
  
  ### convert choice text to numeric ###
  for(i in 1:ncol(ERQ.s3.surveys)) {
    ERQ.s3.surveys[,i] <- recode(ERQ.s3.surveys[,i], 
                           "1-Strongly disagree" = 1,
                           "2" = 2, "3" = 3, "4-Neutral" = 4,
                           "5" = 5, "6" = 6, "7-Strongly agree" = 7)
  }
  
  ### add items for participant scores and create column in survey.s3.surveys ###
  ERQ.s3.surveys$ERQ_CR <-  (rowSums(ERQ.s3.surveys[,c(1,3,5,7,8,10)], 
                               na.rm = FALSE)  / 6 )
  
  ERQ.s3.surveys$ERQ_ES <- ( rowSums(ERQ.s3.surveys[,c(2,4,6,9)], 
                               na.rm = FALSE) / 4 )
  
  ### Five Facet Mindfulness Questionnaire ###
  firstcol <- "ffmq_1"
  lastcol <- "ffmq_39"
  FFMQ.s3.surveys <- select(s3.surveys, firstcol:lastcol)
  
  ## note the \n for page break... Nick, don't put this crap in qualtrics
  ## variable names  in the future!!! ###
  for(i in 1:ncol(FFMQ.s3.surveys)) {
    FFMQ.s3.surveys[,i] <- recode(FFMQ.s3.surveys[,i],
                            "1" = 1,
                            "2" = 2,
                            "3" = 3,
                            "4" = 4,
                            "5" = 5)
  }
  
  key.FFMQ <- c(1,1,-1,1,-1,1,1,-1,1,-1,1,-1,-1,-1,1,-1,-1,-1,1,1,1,-1,-1,1,-1,1,1,-1,1,-1,1,1,1,-1,-1,1,1,-1,-1)
  FFMQ.s3.surveys <- reverse.code(key.FFMQ, FFMQ.s3.surveys, mini = 1, maxi = 5)
  FFMQ.s3.surveys <- as.data.frame(FFMQ.s3.surveys)
  FFMQ.s3.surveys$FFMQ.Ob <- rowMeans(FFMQ.s3.surveys[,c(1,6,11,15,20,26,31,36)])
  FFMQ.s3.surveys$FFMQ.Ds <- rowMeans(FFMQ.s3.surveys[,c(2,7,12,16,22,27,32,37)])
  FFMQ.s3.surveys$FFMQ.Aa <- rowMeans(FFMQ.s3.surveys[,c(5,8,13,18,23,28,34,38)])
  FFMQ.s3.surveys$FFMQ.Nj <- rowMeans(FFMQ.s3.surveys[,c(3,10,14,17,25,30,35,39)])
  FFMQ.s3.surveys$FFMQ.Nr <- rowMeans(FFMQ.s3.surveys[,c(4,9,19,21,24,29,33)])
  
  ### create column values ###
  firstcol <- "stai.s_1"
  lastcol <- "stai.t_20"
  STAI.s3.surveys <- select(s3.surveys, firstcol:lastcol)
  
  ### create subset s3.surveys frames for ID, State, and Trait ###
  firstcol_state <- "stai.s_1"
  lastcol_state <- "stai.s_20"
  firstcol_trait <- "stai.t_1"
  lastcol_trait <-"stai.t_20"
  df_state <- select(STAI.s3.surveys,firstcol_state:lastcol_state)
  df_trait <- select(STAI.s3.surveys,firstcol_trait:lastcol_trait)
  
  ### convert choice text to numeric ###
  for(i in 1:20) {
    df_state[,i] <- recode(df_state[,i], 
                           "Not at all" = 1, "Somewhat" = 2, 
                           "Moderately so" = 3, "Very much so" = 4)
  }
  for(i in 1:20) {
    df_trait[,i] <- recode(df_trait[,i], 
                           "Almost never" = 1, "Sometimes" = 2, 
                           "Often" = 3, "Almost always" = 4)
  }
  
  ### reverse scoring STAI-S ###
  key.STAIS <- c(-1,-1,1,1,-1,1,1,-1,1,-1,-1,1,1,1,-1,-1,1,1,-1,-1)
  key.STAIT <- c(-1,1,-1,1,1,-1,-1,1,1,-1,1,1,-1,-1,1,-1,1,1,-1,1)
  df_state <- reverse.code(key.STAIS,df_state,mini=1,maxi=4)
  df_trait <- reverse.code(key.STAIT,df_trait,mini=1,maxi=4)
  
  ### recombine split s3.surveyssets and clean up workspace ###
  STAI.s3.surveys <-cbind(df_state,df_trait)
  rm(firstcol,lastcol,firstcol_state,firstcol_trait,lastcol_state,lastcol_trait,
     df_state,df_trait, key.STAIS, key.STAIT, i)
  
  ### add scores ###
  STAI.s3.surveys <- as.data.frame(STAI.s3.surveys)
  STAI.s3.surveys$STAI.S <- (rowSums(STAI.s3.surveys[,c(1:20)], na.rm = FALSE))
  STAI.s3.surveys$STAI.T <- (rowSums(STAI.s3.surveys[,c(21:40)], na.rm = FALSE))
  
  ## EQ ###
  EQ.s3.surveys <- s3.surveys[,c(155:182)]
  
  ### create column values ###
  firstcol <- "eq_1"
  lastcol <- "eq_28"
  EQ.s3.surveys <- select(s3.surveys, firstcol:lastcol)
  
  for(i in 1:ncol(EQ.s3.surveys)) {
    EQ.s3.surveys[,i] <- recode(EQ.s3.surveys[,i], 
                          "Strongly agree" = 1, "Slightly agree" = 2, 
                          "Slightly disagree" = 3, "Strongly disagree" = 4)
  }
  
  key.EQ <- c(1,-1,1,-1,-1,-1,1,-1,1,1,1,-1,-1,-1,1,1,1,1,1,1,-1,-1,1,1,1,1,1,1)
  EQ.s3.surveys <- reverse.code(key.EQ, EQ.s3.surveys, mini=1, maxi=4)
  ### code to 0, 1, 2, 2 ###
  for(i in 1:28) {
    EQ.s3.surveys[,i] <- recode(EQ.s3.surveys[,i], `1`=2,`2`=1,`3`=0,`4`=0)
  }
  EQ.s3.surveys <- as.data.frame(EQ.s3.surveys)
  
  EQ.s3.surveys$EQ.full.sum <- rowSums(EQ.s3.surveys, na.rm = FALSE)
  EQ.s3.surveys$EQ.CE <- rowMeans(EQ.s3.surveys[,c(1,7,10,11,16,17,20,23,24,25,27)],na.rm = FALSE)
  EQ.s3.surveys$EQ.ER <- rowMeans(EQ.s3.surveys[,c(3,8,9,12,13,14,18,19,21,22,28)],na.rm = FALSE)
  EQ.s3.surveys$EQ.SS <- rowMeans(EQ.s3.surveys[,c(2,4,5,6,15,26)],na.rm = FALSE)
  
  
}

scored2 <- EQ.s3.surveys[,c(29:32)]
scored2 <- cbind(scored2, FFMQ.s3.surveys[,c(40:44)])
scored2 <- cbind(scored2, ERQ.s3.surveys[,c(11:12)])
scored2 <- cbind(scored2, s3.surveys$Q2)

names(data) <- c("EQ.full.sum2", "EQ.CE.2", "EQ.ER.2", "EQ.SS.2",
                 "FFMQ.Ob.2", "FFMQ.Ds.2", "FFMQ.AA.2", "FFMQ.Nj.2",
                 "FFMQ.Nr.2", "ERQ_CR.2", "ERQ_ES.2", "subjID")

colnames(scored2)[colnames(scored2)=="s3.surveys$Q2"] <- "subjID"
all.survey <- merge(scored, scored2)




### separate IRQ data ###
IRQ.data <- data[,c(193:208)]

### create column values ###
firstcol <- "Q16.IRQ_1"
lastcol <- "Q16.IRQ_16"
ERQ.data <- select(data, firstcol:lastcol)

for(i in 1:ncol(IRQ.data)) {
  IRQ.data[,i] <- recode(IRQ.data[,i], 
                         "Strongly disagree" = 1,
                         "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                         "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7)
}

IRQ.data$NT <- rowSums(IRQ.data[,c(1:4)])
IRQ.data$NE <- rowSums(IRQ.data[,c(5:8)])
IRQ.data$PT <- rowSums(IRQ.data[,c(9:12)])
IRQ.data$PE <- rowSums(IRQ.data[,c(13:16)])
IRQ.data$IRQ.total <- rowSums(IRQ.data[,c(17:20)])


### Rosenberg Self-Esteem Scale (RSES) ###
### create column values ###
firstcol <- "Q17.RSES_1"
lastcol <- "Q17.RSES_10"
RSES.data <- select(data, firstcol:lastcol)

for(i in 1:ncol(RSES.data)) {
  RSES.data[,i] <- recode(RSES.data[,i], 
                          "Strongly Disagree" = 1,
                          "Disagree" = 2, "Agree" = 3, "Strongly Agree" = 4)
}

key.RSES <- c(1,-1,1,1,-1,-1,1,-1,-1,1)
RSES.data <- reverse.code(key.RSES, RSES.data, mini=1, maxi=4)
RSES.data <- as.data.frame(RSES.data)
RSES.data$RSES <- rowSums(RSES.data, na.rm = FALSE)


### Intolerance of Uncertainty (IUS) ###
### note: R changes the "-" to "." in these col names ###
firstcol <- "Q18.IUS_1"
lastcol <- "Q18.IUS_27"
IUS.data <- select(data, firstcol:lastcol)
for(i in 1:ncol(IUS.data)) {
  IUS.data[,i] <- recode(IUS.data[,i], 
                         "1 - Not at all" = 1,
                         "2" = 2, "3 - Somewhat" = 3, "4" = 4,
                         "5 - Entirely" = 5)
}

IUS.data$IUSF1 <- rowSums(IUS.data[,c(1:3, 9, 12:17, 20, 22:25)], na.rm = FALSE)
IUS.data$IUSF2 <- rowSums(IUS.data[,c(4:8, 10, 11, 18, 19, 21, 26, 27)], na.rm = FALSE)
IUS.data$IUSTotal <- rowSums(IUS.data[,c(1:27)], na.rm = FALSE)

### do we use sums or means? xlsx scorer has both ###

### Difficulty with Emotion Regulation Scale (DERS) ###
firstcol <- "Q19.DERS_1"
lastcol <- "Q19.DERS_36"
DERS.data <- select(data, firstcol:lastcol)

for(i in 1:ncol(DERS.data)) {
  DERS.data[,i] <- ifelse((grepl("Always",DERS.data[,i])),5,
                          ifelse((grepl("Most",DERS.data[,i])),4,
                                 ifelse((grepl("half",DERS.data[,i])),3,
                                        ifelse((grepl("Sometimes",DERS.data[,i])),2,
                                               ifelse((grepl("Never",DERS.data[,i])),1,"")))))
  DERS.data[,i] <- as.numeric(DERS.data[,i])
}

key.DERS <- c(-1,-1,1,1,1,-1,-1,-1,1,-1,1,1,1,1,1,1,-1,1,1,-1,1,-1,1,-1,1,1,1,1,1,1,1,1,1,-1,1,1)
DERS.data <- reverse.code(key.DERS, DERS.data, mini = 1, maxi = 5)
DERS.data <- as.data.frame(DERS.data)

DERS.data$DERS.NA <- rowSums(DERS.data[,c(11:12, 21, 23, 25, 29)], na.rm = FALSE)
DERS.data$DERS.G <- rowSums(DERS.data[,c(13, 18, 20, 26, 33)], na.rm = FALSE)
DERS.data$DERS.I <- rowSums(DERS.data[,c(3, 14, 19, 24, 27, 32)], na.rm = FALSE)
DERS.data$DERS.A <- rowSums(DERS.data[,c(2, 6, 8, 10,17, 34)], na.rm = FALSE)
DERS.data$DERS.S <- rowSums(DERS.data[,c(15:16, 22, 28, 30:31, 35:36)], na.rm = FALSE)
DERS.data$DERS.C <- rowSums(DERS.data[,c(1, 4:5, 7, 9)], na.rm = FALSE)



### bring it all back together ###
scored.data <- cbind(idcol, STAI.data, NEO.data, EQ.data,
                     ERQ.data, IRQ.data, RSES.data, IUS.data,
                     DERS.data, FFMQ.data)


### write out the cleaned and scored data file ###
write.csv(scored.data, "~/Your/path/scored.data.csv")

