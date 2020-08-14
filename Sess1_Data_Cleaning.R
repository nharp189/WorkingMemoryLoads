### set working directory to MouseTrap Demo folder ###
setwd("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads")

if (!require("processx")) install.packages("processx")

### load the packages... This needs to be done every time. ###
{suppressPackageStartupMessages(library(mousetrap)) 
  suppressPackageStartupMessages(library(foreach))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(readbulk))
  suppressPackageStartupMessages(library(plyr))
  suppressPackageStartupMessages(library(utils))
  suppressPackageStartupMessages(library(stats))
  suppressPackageStartupMessages(library(pracma))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(magrittr))
  suppressPackageStartupMessages(library(graphics))
  suppressPackageStartupMessages(library(grDevices))
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(scales))
  suppressPackageStartupMessages(library(psych))
  suppressPackageStartupMessages(library(Rcpp))
  suppressPackageStartupMessages(library(diptest))
  suppressPackageStartupMessages(library(RColorBrewer))
  suppressPackageStartupMessages(library(cstab))
  suppressPackageStartupMessages(library(fastcluster))
  suppressPackageStartupMessages(library(parallel))
  suppressPackageStartupMessages(library(fields))
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(yarrr))
}

  session1files <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Session1", 
                              pattern = "*.mt", full.names = TRUE, recursive = FALSE)
  session1.data <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/Session1/", fun = read_mt, 
                             extension = ".mt")
  session1.data <- mt_import_wide(session1.data)
  foreach(file = session1files) %do% {
    ### create "rate" variable (0 = Positive, 1 = Negative) ###
    session1.data$data$rate <- ifelse(session1.data$data$response == "POSITIVE", 0, 1)
    
    ### create "correct response" variable for clearly valenced faces (0 = Incorrect, 1 = Correct) ###
    session1.data$data$correct <- ifelse(session1.data$data$condition == "Angry",  
                                         ifelse(session1.data$data$rate == 1, 1, 0), 
                                         ifelse(session1.data$data$condition == "Happy", 
                                                ifelse(session1.data$data$rate == 0, 1, 0), 
                                                ifelse(session1.data$data$condition == "NEG", 
                                                       ifelse(session1.data$data$rate == 1, 1, 0),
                                                       ifelse(session1.data$data$condition == "POS", 
                                                              ifelse(session1.data$data$rate == 0, 1, 0), NA))))
    
    ### Create column to average for each face ###
    session1.data$data$ang_rate <- ifelse(session1.data$data$condition == "Angry", session1.data$data$rate, NA)
    session1.data$data$hap_rate <- ifelse(session1.data$data$condition == "Happy", session1.data$data$rate, NA)
    session1.data$data$sur_rate <- ifelse(session1.data$data$condition == "Surprise", session1.data$data$rate, NA)
    session1.data$data$neg_rate <- ifelse(session1.data$data$condition == "NEG", session1.data$data$rate, NA)
    session1.data$data$pos_rate <- ifelse(session1.data$data$condition == "POS", session1.data$data$rate, NA)
    session1.data$data$amb_rate <- ifelse(session1.data$data$condition == "AMBIG", session1.data$data$rate, NA)
    
    ### Create column to average for each face in Block 1 ###
    session1.data$data$ang_rate_b1 <- ifelse(session1.data$data$order < 27,
                                             ifelse(session1.data$data$condition == "Angry", session1.data$data$rate, NA),NA)
    session1.data$data$hap_rate_b1 <- ifelse(session1.data$data$order < 27,
                                             ifelse(session1.data$data$condition == "Happy", session1.data$data$rate, NA),NA)
    session1.data$data$sur_rate_b1 <- ifelse(session1.data$data$order < 27,
                                             ifelse(session1.data$data$condition == "Surprise", session1.data$data$rate, NA),NA)
    session1.data$data$neg_rate_b1 <- ifelse(session1.data$data$order < 27,
                                             ifelse(session1.data$data$condition == "NEG", session1.data$data$rate, NA),NA)
    session1.data$data$pos_rate_b1 <- ifelse(session1.data$data$order < 27,
                                             ifelse(session1.data$data$condition == "POS", session1.data$data$rate, NA),NA)
    session1.data$data$amb_rate_b1 <- ifelse(session1.data$data$order < 27,
                                             ifelse(session1.data$data$condition == "AMBIG", session1.data$data$rate, NA),NA)
    
    ### Create column to average for each face in Block 2 ###
    session1.data$data$ang_rate_b2 <- ifelse(session1.data$data$order > 26 & session1.data$data$order < 51,
                                             ifelse(session1.data$data$condition == "Angry", session1.data$data$rate, NA),NA)
    session1.data$data$hap_rate_b2 <- ifelse(session1.data$data$order > 26 & session1.data$data$order < 51,
                                             ifelse(session1.data$data$condition == "Happy", session1.data$data$rate, NA),NA)
    session1.data$data$sur_rate_b2 <- ifelse(session1.data$data$order > 26 & session1.data$data$order < 51,
                                             ifelse(session1.data$data$condition == "Surprise", session1.data$data$rate, NA),NA)
    session1.data$data$neg_rate_b2 <- ifelse(session1.data$data$order > 26 & session1.data$data$order < 51,
                                             ifelse(session1.data$data$condition == "NEG", session1.data$data$rate, NA),NA)
    session1.data$data$pos_rate_b2 <- ifelse(session1.data$data$order > 26 & session1.data$data$order < 51,
                                             ifelse(session1.data$data$condition == "POS", session1.data$data$rate, NA),NA)
    session1.data$data$amb_rate_b2 <- ifelse(session1.data$data$order > 26 & session1.data$data$order < 51,
                                             ifelse(session1.data$data$condition == "AMBIG", session1.data$data$rate, NA),NA)
    
    ### Create column to average for each face in Block 3 ###
    session1.data$data$ang_rate_b3 <- ifelse(session1.data$data$order > 50 & session1.data$data$order < 75,
                                             ifelse(session1.data$data$condition == "Angry", session1.data$data$rate, NA),NA)
    session1.data$data$hap_rate_b3 <- ifelse(session1.data$data$order > 50 & session1.data$data$order < 75,
                                             ifelse(session1.data$data$condition == "Happy", session1.data$data$rate, NA),NA)
    session1.data$data$sur_rate_b3 <- ifelse(session1.data$data$order > 50 & session1.data$data$order < 75,
                                             ifelse(session1.data$data$condition == "Surprise", session1.data$data$rate, NA),NA)
    session1.data$data$neg_rate_b3 <- ifelse(session1.data$data$order > 50 & session1.data$data$order < 75,
                                             ifelse(session1.data$data$condition == "NEG", session1.data$data$rate, NA),NA)
    session1.data$data$pos_rate_b3 <- ifelse(session1.data$data$order > 50 & session1.data$data$order < 75,
                                             ifelse(session1.data$data$condition == "POS", session1.data$data$rate, NA),NA)
    session1.data$data$amb_rate_b3 <- ifelse(session1.data$data$order > 50 & session1.data$data$order < 75,
                                             ifelse(session1.data$data$condition == "AMBIG", session1.data$data$rate, NA),NA)
    
    ### Create column to average for each face in Block 4 ###
    session1.data$data$ang_rate_b4 <- ifelse(session1.data$data$order > 74,
                                             ifelse(session1.data$data$condition == "Angry", session1.data$data$rate, NA),NA)
    session1.data$data$hap_rate_b4 <- ifelse(session1.data$data$order > 74,
                                             ifelse(session1.data$data$condition == "Happy", session1.data$data$rate, NA),NA)
    session1.data$data$sur_rate_b4 <- ifelse(session1.data$data$order > 74,
                                             ifelse(session1.data$data$condition == "Surprise", session1.data$data$rate, NA),NA)
    session1.data$data$neg_rate_b4 <- ifelse(session1.data$data$order > 74,
                                             ifelse(session1.data$data$condition == "NEG", session1.data$data$rate, NA),NA)
    session1.data$data$pos_rate_b4 <- ifelse(session1.data$data$order > 74,
                                             ifelse(session1.data$data$condition == "POS", session1.data$data$rate, NA),NA)
    session1.data$data$amb_rate_b4 <- ifelse(session1.data$data$order > 74,
                                             ifelse(session1.data$data$condition == "AMBIG", session1.data$data$rate, NA),NA)
    
    ### Reaction times ###
    session1.data$data$ang_RT <- ifelse(session1.data$data$condition == "Angry", 
                                        ifelse(session1.data$data$correct == 1, session1.data$data$RT, NA), NA)
    session1.data$data$hap_RT <- ifelse(session1.data$data$condition == "Happy", 
                                        ifelse(session1.data$data$correct == 1, session1.data$data$RT, NA), NA)
    session1.data$data$sur_RT <- ifelse(session1.data$data$condition == "Surprise", 
                                          session1.data$data$RT, NA)
    session1.data$data$sur_p_RT <- ifelse(session1.data$data$condition == "Surprise", 
                                          ifelse(session1.data$data$rate == 0, session1.data$data$RT, NA), NA)
    session1.data$data$sur_n_RT <- ifelse(session1.data$data$condition == "Surprise", 
                                          ifelse(session1.data$data$rate == 1, session1.data$data$RT, NA), NA)
    session1.data$data$neg_RT <- ifelse(session1.data$data$condition == "NEG", 
                                        ifelse(session1.data$data$correct == 1, session1.data$data$RT, NA), NA)
    session1.data$data$pos_RT <- ifelse(session1.data$data$condition == "POS", 
                                        ifelse(session1.data$data$correct == 1, session1.data$data$RT, NA), NA)
    session1.data$data$amb_RT <- ifelse(session1.data$data$condition == "AMBIG",
                                        session1.data$data$RT, NA)
    session1.data$data$amb_p_RT <- ifelse(session1.data$data$condition == "AMBIG", 
                                          ifelse(session1.data$data$rate == 0, session1.data$data$RT, NA), NA)
    session1.data$data$amb_n_RT <- ifelse(session1.data$data$condition == "AMBIG", 
                                          ifelse(session1.data$data$rate == 1, session1.data$data$RT, NA), NA)
    
    ### Create informative condition variables (i.e., angry, happy, sur-neg, sur-pos, etc.) ###
    session1.data$data$condition.rating <- ifelse(session1.data$data$condition == "Angry",  
                                                  ifelse(session1.data$data$rate == 1, "Angry", NA), 
                                                  
                                                  ifelse(session1.data$data$condition == "Happy", 
                                                         ifelse(session1.data$data$rate == 0, "Happy", NA), 
                                                         
                                                         ifelse(session1.data$data$condition == "Surprise",  
                                                                ifelse(session1.data$data$rate == 1, "Sur.Neg", "Sur.Pos"), 
                                                                
                                                                ifelse(session1.data$data$condition == "NEG", 
                                                                       ifelse(session1.data$data$rate == 1, "Negative", NA),
                                                                       
                                                                       ifelse(session1.data$data$condition == "POS", 
                                                                              ifelse(session1.data$data$rate == 0, "Positive", NA),
                                                                              
                                                                              ifelse(session1.data$data$condition == "AMBIG",
                                                                                     ifelse(session1.data$data$rate == 1, "Amb-Neg", "Amb-Pos"), NA))))))
    
  }
  
  session1.ratings.table <- (ddply(session1.data$data, "subjID", summarise, 
                                   ang_rate = mean(ang_rate, na.rm = TRUE),
                                   hap_rate = mean(hap_rate, na.rm = TRUE),
                                   sur_rate = mean(sur_rate, na.rm = TRUE),
                                   neg_rate = mean(neg_rate, na.rm = TRUE),
                                   pos_rate = mean(pos_rate, na.rm = TRUE),
                                   amb_rate = mean(amb_rate, na.rm = TRUE),
                                   ang_rate_b1 = mean(ang_rate_b1, na.rm = TRUE),
                                   hap_rate_b1 = mean(hap_rate_b1, na.rm = TRUE),
                                   sur_rate_b1 = mean(sur_rate_b1, na.rm = TRUE),
                                   neg_rate_b1 = mean(neg_rate_b1, na.rm = TRUE),
                                   pos_rate_b1 = mean(pos_rate_b1, na.rm = TRUE),
                                   amb_rate_b1 = mean(amb_rate_b1, na.rm = TRUE),
                                   ang_rate_b2 = mean(ang_rate_b2, na.rm = TRUE),
                                   hap_rate_b2 = mean(hap_rate_b2, na.rm = TRUE),
                                   sur_rate_b2 = mean(sur_rate_b2, na.rm = TRUE),
                                   neg_rate_b2 = mean(neg_rate_b2, na.rm = TRUE),
                                   pos_rate_b2 = mean(pos_rate_b2, na.rm = TRUE),
                                   amb_rate_b2 = mean(amb_rate_b2, na.rm = TRUE),
                                   ang_rate_b3 = mean(ang_rate_b3, na.rm = TRUE),
                                   hap_rate_b3 = mean(hap_rate_b3, na.rm = TRUE),
                                   sur_rate_b3 = mean(sur_rate_b3, na.rm = TRUE),
                                   neg_rate_b3 = mean(neg_rate_b3, na.rm = TRUE),
                                   pos_rate_b3 = mean(pos_rate_b3, na.rm = TRUE),
                                   amb_rate_b3 = mean(amb_rate_b3, na.rm = TRUE),
                                   ang_rate_b4 = mean(ang_rate_b4, na.rm = TRUE),
                                   hap_rate_b4 = mean(hap_rate_b4, na.rm = TRUE),
                                   sur_rate_b4 = mean(sur_rate_b4, na.rm = TRUE),
                                   neg_rate_b4 = mean(neg_rate_b4, na.rm = TRUE),
                                   pos_rate_b4 = mean(pos_rate_b4, na.rm = TRUE),
                                   amb_rate_b4 = mean(amb_rate_b4, na.rm = TRUE),
                                   ang_RT = mean(ang_RT, na.rm = TRUE),
                                   hap_RT = mean(hap_RT, na.rm = TRUE),
                                   sur_RT = mean(sur_RT, na.rm = TRUE),
                                   sur_p_RT = mean(sur_p_RT, na.rm = TRUE),
                                   sur_n_RT = mean(sur_n_RT, na.rm = TRUE),
                                   neg_RT = mean(neg_RT, na.rm = TRUE),
                                   pos_RT = mean(pos_RT, na.rm = TRUE),
                                   amb_RT = mean(amb_RT, na.rm = TRUE),
                                   amb_p_RT = mean(amb_p_RT, na.rm = TRUE),
                                   amb_n_RT = mean(amb_n_RT, na.rm = TRUE)))

survey.data <- read.csv("Data/Qualtrics_20191218.csv")  

### make ID numeric ###
survey.data$Q2 <- as.numeric(as.character(survey.data$Q2))

### fix errors ###
survey.data[8, "Q2"] <- 71006
survey.data[27, "Q2"] <- 71025
survey.data[40, "Q2"] <- 71039

### ERQ ###
data.ERQ <- select(survey.data, Q2, erq_1:erq_10)

### drop first two junk rows ###
data.ERQ <- data.ERQ[-c(1:2), ]
### make factors into underlying character ###
for(i in 2:ncol(data.ERQ)) {
  data.ERQ[, i] <- as.character(data.ERQ[, i])
}

### convert choice text to numeric ###
for(i in 2:ncol(data.ERQ)) {
  data.ERQ[,i] <- recode(data.ERQ[,i], 
                      "1-Strongly disagree" = 1,
                      "2" = 2, "3" = 3, "4-Neutral" = 4,
                      "5" = 5, "6" = 6, "7-Strongly agree" = 7,
                      "1" = 1, "4" = 4, "7" = 7,
                      "1- Strongly disgree" = 1, "4-Neutral" = 4,
                      "7- Strongly agree" = 7, "1- Strongly disagree" = 1)
}

### make factors into underlying character ###
for(i in 2:ncol(data.ERQ)) {
  data.ERQ[, i] <- as.numeric(data.ERQ[, i])
}

### mean imputation for any missing responses ###
for(i in c(1:nrow(data.ERQ))) {
  data.ERQ[i, c("erq_1","erq_3","erq_5",
                "erq_7","erq_8","erq_10")] <- ifelse(is.na(data.ERQ[i, c("erq_1","erq_3","erq_5",
                                                                         "erq_7","erq_8","erq_10")]),
                                                  rowMeans(data.ERQ[i, c("erq_1","erq_3","erq_5",
                                                                         "erq_7","erq_8","erq_10")], na.rm = TRUE), 
                                                  data.ERQ[i, c("erq_1","erq_3","erq_5",
                                                                "erq_7","erq_8","erq_10")])
  
  data.ERQ[i, c("erq_2","erq_4","erq_6","erq_9")] <- ifelse(is.na(data.ERQ[i, c("erq_2","erq_4","erq_6","erq_9")]),
                                                        rowMeans(data.ERQ[i, c("erq_2","erq_4","erq_6","erq_9")], na.rm = TRUE), 
                                                        data.ERQ[i, c("erq_2","erq_4","erq_6","erq_9")])
}

### add items for participant scores and create column in survey.data ###
data.ERQ$ERQCR <-  (rowSums(data.ERQ[,c("erq_1","erq_3","erq_5",
                                        "erq_7","erq_8","erq_10")], 
                            na.rm = FALSE)  / 6 )

data.ERQ$ERQES <- ( rowSums(data.ERQ[,c("erq_2","erq_4","erq_6","erq_9")], 
                            na.rm = FALSE) / 4 )

colnames(data.ERQ)[1] <- "subjID"

##########################################################

data.BDI <- select(survey.data, Q2, bdi.1:bdi.21)


### drop first two junk rows ###
data.BDI <- data.BDI[-c(1:2), ]

for(i in 1:ncol(data.BDI)) {do
  data.BDI[,i] <- as.character(data.BDI[,i])
}

### convert choice text to numeric ###
for(i in 2:ncol(data.BDI)) {
  data.BDI[,i] <- ifelse(grepl("3.", data.BDI[,i]), 3, 
                         ifelse(grepl("2.", data.BDI[,i]), 2, 
                                ifelse(grepl("1.", data.BDI[,i]), 1, 
                                       ifelse(grepl("0.", data.BDI[,i]), 0, NA))))
}

### get rid of any numbers greater than feasible response ###
for(i in 2:ncol(data.BDI)) {
  data.BDI[, i] <- ifelse(data.BDI[, i] >= 3.1, NA, data.BDI[, i])
}


for(i in 1:ncol(data.BDI)) {do
  data.BDI[,i] <- as.numeric(data.BDI[,i])
}

# ### make NaNs NAs ###
# is.nan.data.frame <- function(x)
#   do.call(cbind, lapply(x, is.nan))

### now fill in omitted responses... ###
for(i in c(1:nrow(data.BDI))) {
  data.BDI[i,c(2:22)] <- ifelse(is.na(data.BDI[i,c(2:22)]),
                                rowMeans(data.BDI[i,c(2:22)], na.rm = TRUE), 
                                data.BDI[i,c(2:22)])
  
}

### add scores ###
data.BDI <- as.data.frame(data.BDI)
data.BDI$BDI <- (rowSums(data.BDI[,c(2:22)], na.rm = FALSE))

colnames(data.BDI)[1] <- "subjID"

############################################################

data.NEO <- select(survey.data, Q2, NE_only_1:NE_only_60)

### remove top junk rows and merge ###
data.NEO <- data.NEO[-c(1:2), ]

### as character ###
for(i in c(2:ncol(data.NEO))) {
  data.NEO[, i] <- as.character(data.NEO[, i])
}

### convert choice text to numeric ###
for(i in 2:ncol(data.NEO)) {
  data.NEO[, i] <- recode(data.NEO[, i], 
                          "Strongly disagree" = 0, "Disagree" = 1, "Neither agree nor disagree" = 2, 
                          "Agree" = 3, "Strongly agree" = 4, "Strongly Disagree" = 0, "Neutral" = 2,
                          "Strongly Agree" = 4, "E-PrimeErr" = NA_real_, "didnotcollect" = NA_real_, 
                          "EPrime err" = NA_real_, "NoSess2" = NA_real_, "1" = 0, "2" = 1,
                          "3" = 2, "4" = 3, "5" = 4, " 1" = 0, " 2" = 1,
                          " 3" = 2, " 4" = 3, " 5" = 4)
}

### get rid of any numbers greater than feasible response ###
for(i in 2:ncol(data.NEO)) {
  data.NEO[, i] <- ifelse(data.NEO[, i] >= 4.1, NA, data.NEO[, i])
}

### back to numeric ####
### make from factors into numeric ###
for(i in c(2:ncol(data.NEO))) {
  data.NEO[, i] <- as.numeric(as.character(data.NEO[, i]))
}

### this order is NEO1:NEO60 ###
key.NEO <- c(-1, 1, 1, 1, 1,
             1, 1, 1, -1, 1,
             1, -1, 1, -1, -1,
             -1, 1, -1, -1, 1,
             1, 1, -1, -1, 1,
             1, -1, -1, 1, -1, 
             -1, 1, -1, 1, 1, 
             1, 1, 1, -1, 1,
             1, -1, 1, -1, -1,
             -1, 1, -1, 1, 1,
             1, 1, 1, -1, -1,
             1, -1, 1, -1, 1)

### strip IDs for recoding ###
data.NEO.id <- data.NEO[, c("Q2")]
data.NEO <- data.NEO[, c(2:61)]

data.NEO <- reverse.code(key.NEO, data.NEO, mini=0, maxi=4)

### bind IDs back... ###
data.NEO <- cbind(data.NEO.id, data.NEO)

### back to dataframe ###
data.NEO <- as.data.frame(data.NEO)

### now fill in omitted responses... ###
for(i in c(1:nrow(data.NEO))) {
  data.NEO[i, c("NE_only_1-","NE_only_6","NE_only_11","NE_only_16-","NE_only_21","NE_only_26",
                "NE_only_31-","NE_only_36","NE_only_41","NE_only_46-","NE_only_51","NE_only_56")] <- ifelse(is.na(data.NEO[i, c("NE_only_1-","NE_only_6","NE_only_11","NE_only_16-","NE_only_21","NE_only_26",
                                                                                                  "NE_only_31-","NE_only_36","NE_only_41","NE_only_46-","NE_only_51","NE_only_56")]),
                                                                              rowMeans(data.NEO[i, c("NE_only_1-","NE_only_6","NE_only_11","NE_only_16-","NE_only_21","NE_only_26",
                                                                                                     "NE_only_31-","NE_only_36","NE_only_41","NE_only_46-","NE_only_51","NE_only_56")], na.rm = TRUE), 
                                                                              data.NEO[i, c("NE_only_1-","NE_only_6","NE_only_11","NE_only_16-","NE_only_21","NE_only_26",
                                                                                            "NE_only_31-","NE_only_36","NE_only_41","NE_only_46-","NE_only_51","NE_only_56")])
  data.NEO[i, c("NE_only_2","NE_only_7","NE_only_12-","NE_only_17","NE_only_22","NE_only_27-","NE_only_32",
                "NE_only_37","NE_only_42-","NE_only_47","NE_only_52","NE_only_57-")] <- ifelse(is.na(data.NEO[i, c("NE_only_2","NE_only_7","NE_only_12-","NE_only_17","NE_only_22","NE_only_27-","NE_only_32",
                                                                                          "NE_only_37","NE_only_42-","NE_only_47","NE_only_52","NE_only_57-")]),
                                                                      rowMeans(data.NEO[i, c("NE_only_2","NE_only_7","NE_only_12-","NE_only_17","NE_only_22","NE_only_27-","NE_only_32",
                                                                                             "NE_only_37","NE_only_42-","NE_only_47","NE_only_52","NE_only_57-")], na.rm = TRUE), 
                                                                      data.NEO[i, c("NE_only_2","NE_only_7","NE_only_12-","NE_only_17","NE_only_22","NE_only_27-","NE_only_32",
                                                                                    "NE_only_37","NE_only_42-","NE_only_47","NE_only_52","NE_only_57-")])
  
  data.NEO[i, c("NE_only_3","NE_only_8","NE_only_13",
                "NE_only_18-","NE_only_23-","NE_only_28-",
                "NE_only_33-","NE_only_38","NE_only_43",
                "NE_only_48-","NE_only_53","NE_only_58")] <- ifelse(is.na(data.NEO[i, c("NE_only_3","NE_only_8","NE_only_13",
                                                                         "NE_only_18-","NE_only_23-","NE_only_28-",
                                                                         "NE_only_33-","NE_only_38","NE_only_43",
                                                                         "NE_only_48-","NE_only_53","NE_only_58")]),
                                                     rowMeans(data.NEO[i, c("NE_only_3","NE_only_8","NE_only_13",
                                                                            "NE_only_18-","NE_only_23-","NE_only_28-",
                                                                            "NE_only_33-","NE_only_38","NE_only_43",
                                                                            "NE_only_48-","NE_only_53","NE_only_58")], na.rm = TRUE),
                                                     data.NEO[i, c("NE_only_3","NE_only_8","NE_only_13",
                                                                   "NE_only_18-","NE_only_23-","NE_only_28-",
                                                                   "NE_only_33-","NE_only_38","NE_only_43",
                                                                   "NE_only_48-","NE_only_53","NE_only_58")])
  data.NEO[i, c("NE_only_4","NE_only_9-","NE_only_14-",
                "NE_only_19-","NE_only_24-","NE_only_29",
                "NE_only_34","NE_only_39-","NE_only_44-",
                "NE_only_49","NE_only_54-","NE_only_59-")] <- ifelse(is.na(data.NEO[i, c("NE_only_4","NE_only_9-","NE_only_14-",
                                                                          "NE_only_19-","NE_only_24-","NE_only_29",
                                                                          "NE_only_34","NE_only_39-","NE_only_44-",
                                                                          "NE_only_49","NE_only_54-","NE_only_59-")]),
                                                      rowMeans(data.NEO[i, c("NE_only_4","NE_only_9-","NE_only_14-",
                                                                             "NE_only_19-","NE_only_24-","NE_only_29",
                                                                             "NE_only_34","NE_only_39-","NE_only_44-",
                                                                             "NE_only_49","NE_only_54-","NE_only_59-")], na.rm = TRUE),
                                                      data.NEO[i, c("NE_only_4","NE_only_9-","NE_only_14-",
                                                                    "NE_only_19-","NE_only_24-","NE_only_29",
                                                                    "NE_only_34","NE_only_39-","NE_only_44-",
                                                                    "NE_only_49","NE_only_54-","NE_only_59-")])
  data.NEO[i, c("NE_only_5","NE_only_10","NE_only_15-",
                "NE_only_20","NE_only_25","NE_only_30-",
                "NE_only_35","NE_only_40","NE_only_45-",
                "NE_only_50","NE_only_55-","NE_only_60")] <- ifelse(is.na(data.NEO[i, c("NE_only_5","NE_only_10","NE_only_15-",
                                                                         "NE_only_20","NE_only_25","NE_only_30-",
                                                                         "NE_only_35","NE_only_40","NE_only_45-",
                                                                         "NE_only_50","NE_only_55-","NE_only_60")]),
                                                     rowMeans(data.NEO[i, c("NE_only_5","NE_only_10","NE_only_15-",
                                                                            "NE_only_20","NE_only_25","NE_only_30-",
                                                                            "NE_only_35","NE_only_40","NE_only_45-",
                                                                            "NE_only_50","NE_only_55-","NE_only_60")], na.rm = TRUE), 
                                                     data.NEO[i, c("NE_only_5","NE_only_10","NE_only_15-",
                                                                   "NE_only_20","NE_only_25","NE_only_30-",
                                                                   "NE_only_35","NE_only_40","NE_only_45-",
                                                                   "NE_only_50","NE_only_55-","NE_only_60")])
  
}

### need to identify Q#'s bc columns are out of order ###
data.NEO$NEO.N <- rowSums(data.NEO[,c("NE_only_1-","NE_only_6","NE_only_11","NE_only_16-","NE_only_21","NE_only_26",
                                      "NE_only_31-","NE_only_36","NE_only_41","NE_only_46-","NE_only_51","NE_only_56")],na.rm = FALSE)
data.NEO$NEO.E <- rowSums(data.NEO[,c("NE_only_2","NE_only_7","NE_only_12-","NE_only_17","NE_only_22","NE_only_27-","NE_only_32",
                                      "NE_only_37","NE_only_42-","NE_only_47","NE_only_52","NE_only_57-")],na.rm = FALSE)
data.NEO$NEO.O <- rowSums(data.NEO[,c("NE_only_3","NE_only_8","NE_only_13", "NE_only_18-","NE_only_23-","NE_only_28-",
                                      "NE_only_33-","NE_only_38","NE_only_43", "NE_only_48-","NE_only_53","NE_only_58")],na.rm = FALSE)
data.NEO$NEO.A <- rowSums(data.NEO[,c("NE_only_4","NE_only_9-","NE_only_14-", "NE_only_19-","NE_only_24-","NE_only_29",
                                      "NE_only_34","NE_only_39-","NE_only_44-", "NE_only_49","NE_only_54-","NE_only_59-")],na.rm = FALSE)
data.NEO$NEO.C <- rowSums(data.NEO[,c("NE_only_5","NE_only_10","NE_only_15-", "NE_only_20","NE_only_25","NE_only_30-",
                                      "NE_only_35","NE_only_40","NE_only_45-", "NE_only_50","NE_only_55-","NE_only_60")],na.rm = FALSE)

### final NEO data ###
colnames(data.NEO)[colnames(data.NEO)=="data.NEO.id"] <- "subjID"
# retain <- ifelse(is.na(data.NEO$NEO.N), "", data.NEO$UID)
# data.NEO <- subset(data.NEO, UID %in% retain)

### clean workspace ###
rm(data.NEO.id, key.NEO)

##########################################################

data.STAIS <- select(survey.data, Q2, stai.s_1:stai.s_20)

### trash junk rows ###
data.STAIS <- data.STAIS[-c(1:2), ]

### convert choice text to numeric ###
for(i in 2:ncol(data.STAIS)) {
  data.STAIS[,i] <- recode(data.STAIS[,i], 
                           "Not at all" = 1, "Somewhat" = 2, 
                           "Moderately so" = 3, "Very much so" = 4,
                           "1" = 1, "2" = 2, "3" = 3, "4" = 4,
                           " 1" = 1, " 2" = 2, " 3" = 3, " 4" = 4)
}

### get rid of any numbers greater than feasible response ###
for(i in 2:ncol(data.STAIS)) {
  data.STAIS[, i] <- ifelse(data.STAIS[, i] >= 4.1, NA, data.STAIS[, i])
}

# ### make from factors into numeric ###
# for(i in c(2:ncol(data.STAIS))) {
#   data.STAIS[, i] <- as.numeric(data.STAIS[, i])
# }

### strip IDs for recoding ###
data.STAIS.id <- data.STAIS[, c("Q2")]
data.STAIS <- data.STAIS[, c(2:21)]

### reverse scoring STAI-S ###
key.STAIS <- c(-1,-1,1,1,-1,1,1,-1,1,-1,-1,1,1,1,-1,-1,1,1,-1,-1)
data.STAIS <- reverse.code(key.STAIS,data.STAIS,mini=1,maxi=4)

### bind IDs back... ###
data.STAIS <- cbind(data.STAIS.id, data.STAIS)

data.STAIS <- as.data.frame(data.STAIS)

### now fill in omitted responses... ###
for(i in c(1:nrow(data.STAIS))) {
  data.STAIS[i, c("stai.s_1-","stai.s_2-","stai.s_3","stai.s_4","stai.s_5-",
                  "stai.s_6", "stai.s_7","stai.s_8-","stai.s_9","stai.s_10-",
                  "stai.s_11-","stai.s_12", "stai.s_13", "stai.s_14", "stai.s_15-",
                  "stai.s_16-", "stai.s_17", "stai.s_18", "stai.s_19-", "stai.s_20-")] <- ifelse(is.na(data.STAIS[i, c("stai.s_1-","stai.s_2-","stai.s_3","stai.s_4","stai.s_5-",
                                                                                                                       "stai.s_6", "stai.s_7","stai.s_8-","stai.s_9","stai.s_10-",
                                                                                                                       "stai.s_11-","stai.s_12", "stai.s_13", "stai.s_14", "stai.s_15-",
                                                                                                                       "stai.s_16-", "stai.s_17", "stai.s_18", "stai.s_19-", "stai.s_20-")]),
                                                                                                      rowMeans(data.STAIS[i, c("stai.s_1-","stai.s_2-","stai.s_3","stai.s_4","stai.s_5-",
                                                                                                                               "stai.s_6", "stai.s_7","stai.s_8-","stai.s_9","stai.s_10-",
                                                                                                                               "stai.s_11-","stai.s_12", "stai.s_13", "stai.s_14", "stai.s_15-",
                                                                                                                               "stai.s_16-", "stai.s_17", "stai.s_18", "stai.s_19-", "stai.s_20-")], na.rm = TRUE), 
                                                                                                      data.STAIS[i, c("stai.s_1-","stai.s_2-","stai.s_3","stai.s_4","stai.s_5-",
                                                                                                                      "stai.s_6", "stai.s_7","stai.s_8-","stai.s_9","stai.s_10-",
                                                                                                                      "stai.s_11-","stai.s_12", "stai.s_13", "stai.s_14", "stai.s_15-",
                                                                                                                      "stai.s_16-", "stai.s_17", "stai.s_18", "stai.s_19-", "stai.s_20-")])
  
}

### add scores ###
data.STAIS <- as.data.frame(data.STAIS)
data.STAIS$STAI.S <- (rowSums(data.STAIS[,c(2:21)], na.rm = FALSE))

colnames(data.STAIS)[colnames(data.STAIS)=="data.STAIS.id"] <- "subjID"
# retain <- ifelse(is.na(data.STAIS$STAI.S), "", data.STAIS$UID)
# data.STAIS <- subset(data.STAIS, UID %in% retain)

### clean up workspace ###
rm(data.STAIS.id, key.STAIS)

#####################################################################
data.STAIT <- select(survey.data, Q2, stai.t_1:stai.t_20)


### trash junk rows ###
data.STAIT <- data.STAIT[-c(1:2), ]

### convert choice text to numeric ###
for(i in 2:ncol(data.STAIT)) {
  data.STAIT[,i] <- recode(data.STAIT[,i], 
                           "Not at all" = 1, "Somewhat" = 2, 
                           "Moderately so" = 3, "Very much so" = 4,
                           "Almost never"= 1, "sometimes" = 2, "Sometimes" = 2,
                           "often" = 3, "Often" = 3, "almost always" = 4, 
                           "Almost always" = 4,
                           "1" = 1, "2" = 2, "3" = 3, "4" = 4,
                           " 1" = 1, " 2" = 2, " 3" = 3, " 4" = 4)
}

### get rid of any numbers greater than feasible response ###
for(i in 2:ncol(data.STAIT)) {
  data.STAIT[, i] <- ifelse(data.STAIT[, i] >= 4.1, NA, data.STAIT[, i])
}

# ### make from factors into numeric ###
# for(i in c(2:ncol(data.STAIT))) {
#   data.STAIT[, i] <- as.numeric(data.STAIT[, i])
# }

### strip IDs for recoding ###
data.STAIT.id <- data.STAIT[, c("Q2")]
data.STAIT <- data.STAIT[, c(2:21)]

### reverse scoring STAI-S ###
key.STAIT <- c(-1,1,-1,1,1,-1,-1,1,1,-1,1,1,-1,-1,1,-1,1,1,-1,1)
data.STAIT <- reverse.code(key.STAIT,data.STAIT,mini=1,maxi=4)
data.STAIT <- as.data.frame(data.STAIT)

### now fill in omitted responses... ###
for(i in c(1:nrow(data.STAIT))) {
  data.STAIT[i, c(1:20)] <- ifelse(is.na(data.STAIT[i, c(1:20)]),
                                   rowMeans(data.STAIT[i, c(1:20)], na.rm = TRUE), 
                                   data.STAIT[i, c(1:20)])
  
}

### bind IDs back... ###
data.STAIT <- cbind(data.STAIT.id, data.STAIT)

data.STAIT <- as.data.frame(data.STAIT)

### add scores ###
data.STAIT <- as.data.frame(data.STAIT)
data.STAIT$STAI.T <- (rowSums(data.STAIT[,c(2:21)], na.rm = FALSE))

colnames(data.STAIT)[colnames(data.STAIT)=="data.STAIT.id"] <- "subjID"
# retain <- ifelse(is.na(data.STAIS$STAI.S), "", data.STAIS$UID)
# data.STAIS <- subset(data.STAIS, UID %in% retain)

### clean up workspace ###
rm(data.STAIT.id, key.STAIT)

#################################################################

data.dem <- select(survey.data, Q2, Q3, Q4, Q5, Q6)
colnames(data.dem)[colnames(data.dem)=="Q2"] <- "subjID"
colnames(data.dem)[colnames(data.dem)=="Q3"] <- "age"
colnames(data.dem)[colnames(data.dem)=="Q4"] <- "sex"
colnames(data.dem)[colnames(data.dem)=="Q5"] <- "race"
colnames(data.dem)[colnames(data.dem)=="Q6"] <- "ethn"

#################################################################
data <- merge(session1.ratings.table, data.ERQ)
data <- merge(data, data.STAIS)
data <- merge(data, data.STAIT)
data <- merge(data, data.NEO)
data <- merge(data, data.BDI)
write.csv(data, "WM_Study_S1_itemData.csv")
data <- data[, c(1:7, 33:41, 52:53,
                 74, 95, 156:160, 182)]

data <- merge(data, data.dem)

write.csv(data, "WM_Study_S1_ERQ.csv")
