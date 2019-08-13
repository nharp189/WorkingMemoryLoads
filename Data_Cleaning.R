### set working directory to MouseTrap Demo folder ###
setwd("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads")

if (!require("processx")) install.packages("processx")

{### install MouseTrap... This only needs to be done once. ###
### ... and readbulk for applying to a set of files ###
#install.packages("mousetrap")
#install.packages("readbulk")
#install.packages("here")
#install.packages("foreach")
#install.packages("tidyverse")
#install.packages("plyr")
#install.packages("utils")
#install.packages("stats")
#install.packages("pracma")
#install.packages("tidyr")
#install.packages("magrittr")
#install.packages("graphics")
#install.packages("grDevices")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("psych")
#install.packages("Rcpp")
#install.packages("diptest")
#install.packages("RColorBrewer")
#install.packages("cstab")
#install.packages("fastcluster")
#install.packages("parallel")
#install.packages("fields")
#install.packages("orca")
#install.packages("plotly")
#install.packages("weights")
}
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

{
session1files <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/Data/Session1", 
                            pattern = "*.mt", full.names = TRUE, recursive = FALSE)
session1.data <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/Data/Session1/", fun = read_mt, 
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
  session1.data$data$sur_p_RT <- ifelse(session1.data$data$condition == "Surprise", 
                                        ifelse(session1.data$data$rate == 0, session1.data$data$RT, NA), NA)
  session1.data$data$sur_n_RT <- ifelse(session1.data$data$condition == "Surprise", 
                                        ifelse(session1.data$data$rate == 1, session1.data$data$RT, NA), NA)
  session1.data$data$neg_RT <- ifelse(session1.data$data$condition == "NEG", 
                                      ifelse(session1.data$data$correct == 1, session1.data$data$RT, NA), NA)
  session1.data$data$pos_RT <- ifelse(session1.data$data$condition == "POS", 
                                      ifelse(session1.data$data$correct == 1, session1.data$data$RT, NA), NA)
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
                                 sur_p_RT = mean(sur_p_RT, na.rm = TRUE),
                                 sur_n_RT = mean(sur_n_RT, na.rm = TRUE),
                                 neg_RT = mean(neg_RT, na.rm = TRUE),
                                 pos_RT = mean(pos_RT, na.rm = TRUE),
                                 amb_p_RT = mean(amb_p_RT, na.rm = TRUE),
                                 amb_n_RT = mean(amb_n_RT, na.rm = TRUE)))

### Session1.data MT Measures ###
{### get derivatives ###
  session1.data <- mt_derivatives(session1.data)
  
  ### Flip traSurp.Negectories ###
  session1.data <- mt_remap_symmetric(session1.data) 
  
  ### adSurp.Negust to identical start/end traSurp.Negectories ###
  session1.data <- mt_align_start_end(session1.data) 
  
  ### normalize, there's some issues w/ trials w/ 0 variability: hence this weird bit ###
  session1.data$data$pos_var <- apply(session1.data$trajectories[,,"xpos"],1,var,na.rm=TRUE) + apply(session1.data$trajectories[,,"ypos"],1,var,na.rm=TRUE)
  table(session1.data$data$pos_var==0)
  session1.data <- mt_subset(session1.data, pos_var>0)
  session1.data <- mt_time_normalize(session1.data) 
  
  ### add measures ###
  session1.data <- mt_measures(session1.data)}
### get your data collapsed per sub, merge this with the final data! ###
session1.data.persub <- mt_aggregate_per_subject(session1.data, use = "measures", subject_id = "subjID" )
### merge MT measures w/ rating data ###
session1.ratings.table <- merge(session1.data.persub, session1.ratings.table, by = "subjID")
### write output for session1 ###
write.csv(session1.ratings.table, "WM.Session1.csv")



}
### sanity check ###
# mt_plot_aggregate(session1.data, use = "tn_trajectories", color = "condition.rating")

# Merge measures with trial data
MT.results1 <- dplyr::inner_join(
  session1.data$data, session1.data$measures,
  by="mt_id")


### can run some analyses based off S1 MAD if desired ###
{### make MAD wide ###
wide.MAD <- MT.results1 %>% spread(condition.rating, MAD)
MAD.plot <- (ddply(wide.MAD, "subjID", summarise, 
            sur_rate = mean(sur_rate, na.rm = TRUE),
            amb_rate = mean(amb_rate, na.rm = TRUE),
            amb.neg.MAD = mean(`Amb-Neg`, na.rm = TRUE),
            amb.pos.MAD = mean(`Amb-Pos`, na.rm = TRUE),
            ang.MAD = mean(Angry, na.rm = TRUE),
            hap.MAD = mean(Happy, na.rm = TRUE),
            neg.MAD = mean(Negative, na.rm = TRUE),
            pos.MAD = mean(Positive, na.rm = TRUE),
            sur.neg.MAD = mean(`Sur.Neg`, na.rm = TRUE),
            sur.pos.MAD = mean(`Sur.Pos`, na.rm = TRUE)))

### plot interpretion MD x val_bias (faces only) ###
plot(MAD.plot$sur_rate,MAD.plot$sur.pos.MAD, pch = 16, 
     cex = 1.3, xlab = "Valence Bias", ylab = "Surprise as Positive MD")

### add regression line ###
abline(lm(MAD.plot$sur.pos.MAD ~ MAD.plot$sur_rate))

cor.test(MAD.plot$sur.pos.MAD, MAD.plot$sur_rate)

plot(MAD.plot$sur_rate,MAD.plot$sur.neg.MAD, pch = 16, 
     cex = 1.3, xlab = "Valence Bias", ylab = "Surprise as Negative MD")

### add regression line ###
abline(lm(MAD.plot$sur.neg.MAD ~ MAD.plot$sur_rate))


cor.test(MAD.plot$sur.neg.MAD, MAD.plot$sur_rate)

### now include IAPS and faces... ###
MAD.plot$val_bias <- ((MAD.plot$sur_rate + MAD.plot$amb_rate)/2)
### need to average MADs ###
MAD.plot$pos.res <- ((MAD.plot$amb.pos.MAD + MAD.plot$sur.pos.MAD)/2)
MAD.plot$neg.res <- ((MAD.plot$amb.neg.MAD + MAD.plot$sur.neg.MAD)/2)

plot(MAD.plot$val_bias,MAD.plot$pos.res, pch = 16, 
     cex = 1.3, xlab = "Valence Bias", ylab = "Positive Response (Amb Face / IAPS) MD",
     main = "Correlation of Valence Bias and Ambiguous Images Interpreted Positively")
lm(MAD.plot$pos.res ~ MAD.plot$val_bias)
abline(.2334, .1734)
cor.test(MAD.plot$val_bias,MAD.plot$pos.res)

plot(MAD.plot$val_bias,MAD.plot$neg.res, pch = 16, 
     cex = 1.3, xlab = "Valence Bias", ylab = "Negative Response (Amb Face / IAPS) MD",
     main = "Correlation of Valence Bias and Ambiguous Images Interpreted Negatively")
lm(MAD.plot$neg.res ~ MAD.plot$val_bias)
abline(.7397, -.6821)
cor.test(MAD.plot$val_bias,MAD.plot$neg.res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### import session two files ###
{### set1va ###
set1.va.files.list <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set1_task_vA", pattern = "*.mt", full.names = TRUE,
                                 recursive = FALSE)
set1.va.files <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set1_task_vA/", fun = read_mt, 
                           extension = ".mt")
### set1.va has incorrectly labelled trials... we'll need to import the correct trial order from another file ###
set1.va.trial.order <- read_excel("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/trial_order_corrections.xlsx", sheet = 1)

### correct conditions ###
set1.va.files <- merge(set1.va.files, set1.va.trial.order, by = "stim")

set1.vc.files.list <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set1_task_vC", pattern = "*.mt", full.names = TRUE,
                                 recursive = FALSE)
set1.vc.files <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set1_task_vC/", fun = read_mt, 
                           extension = ".mt")
### set1.vc has incorrectly labelled trials... we'll need to import the correct trial order from another file ###
set1.vc.trial.order <- read_excel("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/trial_order_corrections.xlsx", sheet = 2)

### correct conditions ###
set1.vc.files <- merge(set1.vc.files, set1.vc.trial.order, by = "stim")

set2.va.files.list <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set2_task_vA", pattern = "*.mt", full.names = TRUE,
                                 recursive = FALSE)
set2.va.files <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set2_task_vA/", fun = read_mt, 
                           extension = ".mt")
### set2.va has incorrectly labelled trials... we'll need to import the correct trial order from another file ###
set2.va.trial.order <- read_excel("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/trial_order_corrections.xlsx", sheet = 4)

### correct conditions ###
set2.va.files <- merge(set2.va.files, set2.va.trial.order, by = "stim")

set2.vc.files.list <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set2_task_vC", pattern = "*.mt", full.names = TRUE,
                                 recursive = FALSE)
set2.vc.files <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set2_task_vC/", fun = read_mt, 
                           extension = ".mt")

### set2.vc has incorrectly labelled trials... we'll need to import the correct trial order from another file ###
set2.vc.trial.order <- read_excel("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/trial_order_corrections.xlsx", sheet = 5)

### correct conditions ###
set2.vc.files <- merge(set2.vc.files, set2.vc.trial.order, by = "stim")

set3.vd.files.list <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set3_task_vD", pattern = "*.mt", full.names = TRUE,
                            recursive = FALSE)
set3.vd.files <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set3_task_vD/", fun = read_mt, 
                           extension = ".mt")

### set3.vd has incorrectly labelled trials... we'll need to import the correct trial order from another file ###
set3.vd.trial.order <- read_excel("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/trial_order_corrections.xlsx", sheet = 6)

### correct conditions ###
set3.vd.files <- merge(set3.vd.files, set3.vd.trial.order, by = "stim")

set4.vd.files.list <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set4_task_vD", pattern = "*.mt", full.names = TRUE,
                            recursive = FALSE)
set4.vd.files <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/Data/set4_task_vD/", fun = read_mt, 
                           extension = ".mt")
### set4.vd has incorrectly labelled trials... we'll need to import the correct trial order from another file ###
set4.vd.trial.order <- read_excel("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study2/Analyses/WorkingMemoryLoads/trial_order_corrections.xlsx", sheet = 7)

### correct conditions ###
set4.vd.files <- merge(set4.vd.files, set4.vd.trial.order, by = "stim")  

### combine into one mousetrap object ###
MT.data <- rbind.fill(set1.va.files, set1.vc.files)
MT.data <- rbind.fill(MT.data, set2.va.files)
MT.data <- rbind.fill(MT.data, set2.vc.files)
MT.data <- rbind.fill(MT.data, set3.vd.files)
MT.data <- rbind.fill(MT.data, set4.vd.files)
MT.data <- arrange(MT.data, subjID, order)
file.list <- list(c(set1.va.files.list, set1.vc.files.list, set2.va.files.list,
                    set2.vc.files.list, set3.vd.files.list, set4.vd.files.list))}

### import MT data ###
MT.data <- mt_import_wide(MT.data)

### process data ###
{foreach(file = file.list) %do% {

  ### create "rate" variable (0 = Positive, 1 = Negative) ###
  MT.data$data$rate <- ifelse(MT.data$data$response == "POSITIVE", 0, 
                                    ifelse(MT.data$data$response == "YES", 0, 1))

  ### coerce to character ###
  MT.data$data$cond.correct <- as.character(MT.data$data$cond.correct)

  ### create "correct response" variable for clearly valenced faces (0 = Incorrect, 1 = Correct) ###
  MT.data$data$correct <- ifelse(MT.data$data$cond.correct == "Angry",  
                                       ifelse(MT.data$data$rate == 1, 1, 0), 
                                       ifelse(MT.data$data$cond.correct == "Happy", 
                                              ifelse(MT.data$data$rate == 0, 1, 0), ""))
  ### label face vs. memory probe trials ###
  MT.data$data$trialtype <- ifelse(MT.data$data$condition %in% c("Happy", "Angry","Surprise"),"face",
                                   ifelse(MT.data$data$condition %in% c("Yes","No"),"memory", "instruct"))
  
  
  ### label conditions (emo, neu, hi, lo) ###
  MT.data$data$type <- ifelse(MT.data$data$cond.load %in% c("LoEmoSurpriseYes","HiEmoSurpriseYes",
                                                                 "LoEmoAngryYes", "HiEmoAngryYes",
                                                                 "LoEmoHappyYes", "HiEmoHappyYes",
                                                                 "LoEmoSurpriseNo","HiEmoSurpriseNo",
                                                                 "LoEmoAngryNo", "HiEmoAngryNo",
                                                                 "LoEmoHappyNo", "HiEmoHappyNo",
                                                                 "LoEmoSurprise","HiEmoSurprise",
                                                                 "LoEmoAngry", "HiEmoAngry",
                                                                 "LoEmoHappy", "HiEmoHappy",
                                                                 "LoEmoSurprise","HiEmoSurprise",
                                                                 "LoEmoAngry", "HiEmoAngry",
                                                                 "LoEmoHappy", "HiEmoHappy"),
                                   "EMO", 
                                   ifelse(MT.data$data$cond.load %in% c("LoNeuSurpriseYes","HiNeuSurpriseYes",
                                                                        "LoNeuAngryYes", "HiNeuAngryYes",
                                                                        "LoNeuHappyYes", "HiNeuHappyYes",
                                                                        "LoNeuSurpriseNo","HiNeuSurpriseNo",
                                                                        "LoNeuAngryNo", "HiNeuAngryNo",
                                                                        "LoNeuHappyNo", "HiNeuHappyNo",
                                                                        "LoNeuSurprise","HiNeuSurprise",
                                                                        "LoNeuAngry", "HiNeuAngry",
                                                                        "LoNeuHappy", "HiNeuHappy",
                                                                        "LoNeuSurprise","HiNeuSurprise",
                                                                        "LoNeuAngry", "HiNeuAngry",
                                                                        "LoNeuHappy", "HiNeuHappy"),
                                          "NEU", NA))
  
  MT.data$data$load <- ifelse(MT.data$data$cond.load %in% c("LoNeuSurpriseYes", "LoNeuAngryYes",
                                                                 "LoNeuHappyYes", "LoNeuSurpriseNo", 
                                                                 "LoNeuAngryNo", "LoNeuHappyNo",
                                                                 "LoEmoSurpriseYes", "LoEmoAngryYes", 
                                                                 "LoEmoHappyYes", "LoEmoSurpriseNo",
                                                                 "LoEmoAngryNo", "LoEmoHappyNo",
                                                            "LoNeuSurprise", "LoNeuAngry",
                                                            "LoNeuHappy", "LoNeuSurprise", 
                                                            "LoNeuAngry", "LoNeuHappy",
                                                            "LoEmoSurprise", "LoEmoAngry", 
                                                            "LoEmoHappy", "LoEmoSurprise",
                                                            "LoEmoAngry", "LoEmoHappy"),
                                   "LOW",
                                   ifelse(MT.data$data$cond.load %in% c("HiNeuSurpriseYes", "HiNeuAngryYes",
                                                                        "HiNeuHappyYes", "HiNeuSurpriseNo", 
                                                                        "HiNeuAngryNo", "HiNeuHappyNo",
                                                                        "HiEmoSurpriseYes", "HiEmoAngryYes", 
                                                                        "HiEmoHappyYes", "HiEmoSurpriseNo",
                                                                        "HiEmoAngryNo", "HiEmoHappyNo",
                                                                        "HiNeuSurprise", "HiNeuAngry",
                                                                        "HiNeuHappy", "HiNeuSurprise", 
                                                                        "HiNeuAngry", "HiNeuHappy",
                                                                        "HiEmoSurprise", "HiEmoAngry", 
                                                                        "HiEmoHappy", "HiEmoSurprise",
                                                                        "HiEmoAngry", "HiEmoHappy"),
                                          "HIGH", NA))
  
  ### create correct response column for memory probes ###
  MT.data$data$mem.cor <- ifelse(MT.data$data$cond.correct == "Yes",
                                 ifelse(MT.data$data$rate == 0, 1, 0),
                                 ifelse(MT.data$data$cond.correct == "No",
                                        ifelse(MT.data$data$rate == 1, 1, 0), ""))
  
  ### create correct response for EMO mem probes ###
  MT.data$data$emo.mem <- as.numeric(ifelse(MT.data$data$cond.load %in% c("LoEmoSurpriseYes","HiEmoSurpriseYes",
                                                                          "LoEmoAngryYes", "HiEmoAngryYes",
                                                                          "LoEmoHappyYes", "HiEmoHappyYes",
                                                                          "LoEmoSurpriseNo","HiEmoSurpriseNo",
                                                                          "LoEmoAngryNo", "HiEmoAngryNo",
                                                                          "LoEmoHappyNo", "HiEmoHappyNo"),
                                            MT.data$data$mem.cor, ""))
  
  ### create correct response for EMO mem probes ###
  MT.data$data$neu.mem <- as.numeric(ifelse(MT.data$data$cond.load %in% c("LoNeuSurpriseYes","HiNeuSurpriseYes",
                                                                          "LoNeuAngryYes", "HiNeuAngryYes",
                                                                          "LoNeuHappyYes", "HiNeuHappyYes",
                                                                          "LoNeuSurpriseNo","HiNeuSurpriseNo",
                                                                          "LoNeuAngryNo", "HiNeuAngryNo",
                                                                          "LoNeuHappyNo", "HiNeuHappyNo"),
                                            MT.data$data$mem.cor, ""))
  
  ### create correct response for LOW mem probes ###
  MT.data$data$lo.mem <- as.numeric(ifelse(MT.data$data$cond.load %in% c("LoNeuSurpriseYes","LoEmoSurpriseYes",
                                                                         "LoNeuAngryYes", "LoEmoAngryYes",
                                                                         "LoNeuHappyYes", "LoEmoHappyYes",
                                                                         "LoNeuSurpriseNo","LoEmoSurpriseNo",
                                                                         "LoNeuAngryNo", "LoEmoAngryNo",
                                                                         "LoNeuHappyNo", "LoEmoHappyNo"),
                                           MT.data$data$mem.cor, ""))
  
  ### create correct response for HIGH mem probes ###
  MT.data$data$hi.mem <- as.numeric(ifelse(MT.data$data$cond.load %in% c("HiNeuSurpriseYes","HiEmoSurpriseYes",
                                                                         "HiNeuAngryYes", "HiEmoAngryYes",
                                                                         "HiNeuHappyYes", "HiEmoHappyYes",
                                                                         "HiNeuSurpriseNo","HiEmoSurpriseNo",
                                                                         "HiNeuAngryNo", "HiEmoAngryNo",
                                                                         "HiNeuHappyNo", "HiEmoHappyNo"),
                                           MT.data$data$mem.cor, ""))
  ### Create column to average for each face ###
  MT.data$data$mem.cor.yes <- ifelse(MT.data$data$cond.correct == "Yes", MT.data$data$mem.cor, NA)
  MT.data$data$mem.cor.no <- ifelse(MT.data$data$cond.correct == "No", MT.data$data$mem.cor, NA)
  MT.data$data$mem.cor.yes <- as.numeric(MT.data$data$mem.cor.yes)
  MT.data$data$mem.cor.no <- as.numeric(MT.data$data$mem.cor.no)
  
  ### Create informative condition variables (i.e., angry, happy, sur-neg, sur-pos, etc.) ###
  MT.data$data$condition.rating <- ifelse(MT.data$data$cond.correct == "Angry",  
                                          ifelse(MT.data$data$rate == 1, "Angry", NA),
                                          ifelse(MT.data$data$cond.correct == "Happy", 
                                                 ifelse(MT.data$data$rate == 0, "Happy", NA), 
                                                 ifelse(MT.data$data$cond.correct == "Surprise",  
                                                        ifelse(MT.data$data$rate == 1, "Sur.Neg", "Sur.Pos"), NA)))
                                        
  ### label load, face, and ratings together ###
  MT.data$data$cond.full <- paste(MT.data$data$cond.load, MT.data$data$condition.rating)
  
  ## there are no factors b/w memory trials.. while I did not randomize correctly,
  ## let's look to see if there's some interesting relationships. Starting by
  ## categorizing memory trials... 
  MT.data$data$memtype <- paste(MT.data$data$load,MT.data$data$type)
  MT.data$data$memtype <- ifelse(MT.data$data$trialtype == "memory",
                                 MT.data$data$memtype, "")
  
  ### make subj factor ###
  MT.data$data$subjID <- factor(MT.data$data$subjID)
  
  ### add RT z scores ###
  MT.data$data$RTz <- ave(MT.data$data$RT, MT.data$data$subjID, FUN=scale)
  
  ### factor ###
  MT.data$data$factor <- paste(MT.data$data$type, MT.data$data$load)
  
}

###  MT Measures ###
{### get derivatives ###
  MT.data <- mt_derivatives(MT.data)
  
  ### Flip traSurp.Negectories ###
  MT.data <- mt_remap_symmetric(MT.data) 
  
  ### adSurp.Negust to identical start/end traSurp.Negectories ###
  MT.data <- mt_align_start_end(MT.data) 
  
  ### normalize, there's some issues w/ trials w/ 0 variability: hence this weird bit ###
  #MT.data$data$pos_var <- apply(MT.data$trajectories[,,"xpos"],1,var,na.rm=TRUE) + apply(MT.data$trajectories[,,"ypos"],1,var,na.rm=TRUE)
  #table(MT.data$data$pos_var==0)
  #MT.data <- mt_subset(MT.data, pos_var>0)
  MT.data <- mt_time_normalize(MT.data) 
  
  ### add measures ###
  MT.data <- mt_measures(MT.data)
}}

### check RT min / max for outliers ###
meanrt <- mean(MT.data$data$RT)
sdrt <- sd(MT.data$data$RT)
outlier <- meanrt + (3*sdrt)
#print(outlier)

### remove trials > outlierRT ###
MT.data$data <- subset(MT.data$data, RT <= outlier)

### flag trials before incorrect memory probes ###
for(i in 2:nrow(MT.data$data)) {
  MT.data$data$flag[i-1] <- ifelse(MT.data$data$mem.cor[i] == 0, 0, 1)
}

### remove flagged trials ###
MT.data$data <- subset(MT.data$data, MT.data$data$flag == 1)

# Merge measures with trial data
MT.results2 <- dplyr::inner_join(
  MT.data$data, MT.data$measures,
  by="mt_id")

### is surprise ratings different from low vs high trials? ###
MT.data.rating.table2 <- (ddply(MT.results2, "subjID", summarise, 
                               lo.emo.ang_rate = mean(rate[which(cond.correct == "Angry" & type == "EMO" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.emo.ang_rate = mean(rate[which(cond.correct == "Angry" & type == "EMO" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.neu.ang_rate = mean(rate[which(cond.correct == "Angry" & type == "NEU" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.neu.ang_rate = mean(rate[which(cond.correct == "Angry" & type == "NEU" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.emo.hap_rate = mean(rate[which(cond.correct == "Happy" & type == "EMO" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.emo.hap_rate = mean(rate[which(cond.correct == "Happy" & type == "EMO" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.neu.hap_rate = mean(rate[which(cond.correct == "Happy" & type == "NEU" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.neu.hap_rate = mean(rate[which(cond.correct == "Happy" & type == "NEU" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.emo.sur_rate = mean(rate[which(cond.correct == "Surprise" & type == "EMO" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.emo.sur_rate = mean(rate[which(cond.correct == "Surprise" & type == "EMO" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.neu.sur_rate = mean(rate[which(cond.correct == "Surprise" & type == "NEU" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.neu.sur_rate = mean(rate[which(cond.correct == "Surprise" & type == "NEU" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.emo.ang_RT = mean(RT.x[which(cond.correct == "Angry" & type == "EMO" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.emo.ang_RT = mean(RT.x[which(cond.correct == "Angry" & type == "EMO" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.neu.ang_RT = mean(RT.x[which(cond.correct == "Angry" & type == "NEU" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.neu.ang_RT = mean(RT.x[which(cond.correct == "Angry" & type == "NEU" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.emo.hap_RT = mean(RT.x[which(cond.correct == "Happy" & type == "EMO" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.emo.hap_RT = mean(RT.x[which(cond.correct == "Happy" & type == "EMO" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.neu.hap_RT = mean(RT.x[which(cond.correct == "Happy" & type == "NEU" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.neu.hap_RT = mean(RT.x[which(cond.correct == "Happy" & type == "NEU" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.emo.sur_n_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "EMO" & load == "LOW" & trialtype == "face" & rate == 1)], na.rm = TRUE),
                               hi.emo.sur_n_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "EMO" & load == "HIGH" & trialtype == "face" & rate == 1)], na.rm = TRUE),
                               lo.neu.sur_n_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "NEU" & load == "LOW" & trialtype == "face" & rate == 1)], na.rm = TRUE),
                               hi.neu.sur_n_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "NEU" & load == "HIGH" & trialtype == "face" & rate == 1)], na.rm = TRUE),
                               lo.emo.sur_p_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "EMO" & load == "LOW" & trialtype == "face" & rate == 0)], na.rm = TRUE),
                               hi.emo.sur_p_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "EMO" & load == "HIGH" & trialtype == "face" & rate == 0)], na.rm = TRUE),
                               lo.neu.sur_p_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "NEU" & load == "LOW" & trialtype == "face" & rate == 0)], na.rm = TRUE),
                               hi.neu.sur_p_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "NEU" & load == "HIGH" & trialtype == "face" & rate == 0)], na.rm = TRUE),
                               lo.neu.sur_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "NEU" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.neu.sur_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "NEU" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.emo.sur_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "EMO" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.emo.sur_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "EMO" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.sur_RT = mean(RT.x[which(cond.correct == "Surprise" & load == "LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.sur_RT = mean(RT.x[which(cond.correct == "Surprise" & load == "HIGH" & trialtype == "face")], na.rm = TRUE),
                               neu.sur_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "NEU" & trialtype == "face")], na.rm = TRUE),
                               emo.sur_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "EMO" & trialtype == "face")], na.rm = TRUE),
                               lo.sur_n_RT = mean(RT.x[which(cond.correct == "Surprise" & load == "LOW" & trialtype == "face" & rate == 1)], na.rm = TRUE),
                               hi.sur_n_RT = mean(RT.x[which(cond.correct == "Surprise" & load == "HIGH" & trialtype == "face" & rate == 1)], na.rm = TRUE),
                               neu.sur_n_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "NEU" & trialtype == "face" & rate == 1)], na.rm = TRUE),
                               emo.sur_n_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "EMO" & trialtype == "face" & rate ==1)], na.rm = TRUE),
                               lo.sur_p_RT = mean(RT.x[which(cond.correct == "Surprise" & load == "LOW" & trialtype == "face" & rate == 0)], na.rm = TRUE),
                               hi.sur_p_RT = mean(RT.x[which(cond.correct == "Surprise" & load == "HIGH" & trialtype == "face" & rate == 0)], na.rm = TRUE),
                               neu.sur_p_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "NEU" & trialtype == "face" & rate == 0)], na.rm = TRUE),
                               emo.sur_p_RT = mean(RT.x[which(cond.correct == "Surprise" & type == "EMO" & trialtype == "face" & rate ==0)], na.rm = TRUE),
                               mem.cor.yes = mean(mem.cor.yes, na.rm = TRUE),
                               mem.cor.no = mean(mem.cor.no, na.rm = TRUE),
                               emo.mem = mean(emo.mem, na.rm = TRUE),
                               neu.mem = mean(neu.mem, na.rm = TRUE),
                               lo.mem = mean(lo.mem, na.rm = TRUE),
                               hi.mem = mean(hi.mem, na.rm = TRUE),
                               lo.emo.sur_MAD = mean(MAD[which(cond.load == "LoEmoSurprise")], na.rm = TRUE),
                               hi.emo.sur_MAD = mean(MAD[which(cond.load == "HiEmoSurprise")], na.rm = TRUE),
                               lo.neu.sur_MAD = mean(MAD[which(cond.load == "LoNeuSurprise")], na.rm = TRUE),
                               hi.neu.sur_MAD = mean(MAD[which(cond.load == "HiNeuSurprise")], na.rm = TRUE),
                               lo.emo.sur_n_MAD = mean(MAD[which(cond.full == "LoEmoSurprise Sur.Neg")], na.rm = TRUE),
                               lo.emo.sur_p_MAD = mean(MAD[which(cond.full == "LoEmoSurprise Sur.Pos")], na.rm = TRUE),
                               hi.emo.sur_n_MAD = mean(MAD[which(cond.full == "HiEmoSurprise Sur.Neg")], na.rm = TRUE),
                               hi.emo.sur_p_MAD = mean(MAD[which(cond.full == "HiEmoSurprise Sur.Pos")], na.rm = TRUE),
                               lo.neu.sur_n_MAD = mean(MAD[which(cond.full == "LoNeuSurprise Sur.Neg")], na.rm = TRUE),
                               lo.neu.sur_p_MAD = mean(MAD[which(cond.full == "LoNeuSurprise Sur.Pos")], na.rm = TRUE),
                               hi.neu.sur_n_MAD = mean(MAD[which(cond.full == "HiNeuSurprise Sur.Neg")], na.rm = TRUE),
                               hi.neu.sur_p_MAD = mean(MAD[which(cond.full == "HiNeuSurprise Sur.Pos")], na.rm = TRUE),
                               lo.emo.ang_RTz = mean(RTz[which(cond.load == "LoEmoAngry")], na.rm = TRUE),
                               hi.emo.ang_RTz = mean(RTz[which(cond.load == "HiEmoAngry")], na.rm = TRUE),
                               lo.neu.ang_RTz = mean(RTz[which(cond.load == "LoNeuAngry")], na.rm = TRUE),
                               hi.neu.ang_RTz = mean(RTz[which(cond.load == "HiNeuAngry")], na.rm = TRUE),
                               lo.emo.hap_RTz = mean(RTz[which(cond.load == "LoEmoHappy")], na.rm = TRUE),
                               hi.emo.hap_RTz = mean(RTz[which(cond.load == "HiEmoHappy")], na.rm = TRUE),
                               lo.neu.hap_RTz = mean(RTz[which(cond.load == "LoNeuHappy")], na.rm = TRUE),
                               hi.neu.hap_RTz = mean(RTz[which(cond.load == "HiNeuHappy")], na.rm = TRUE),
                               lo.emo.sur_p_RTz = mean(RTz[which(cond.full == "LoEmoSurprise Sur.Pos")], na.rm = TRUE),
                               hi.emo.sur_p_RTz = mean(RTz[which(cond.full == "HiEmoSurprise Sur.Pos")], na.rm = TRUE),
                               lo.neu.sur_p_RTz = mean(RTz[which(cond.full == "LoNeuSurprise Sur.Pos")], na.rm = TRUE),
                               hi.neu.sur_p_RTz = mean(RTz[which(cond.full == "HiNeuSurprise Sur.Pos")], na.rm = TRUE),
                               lo.emo.sur_n_RTz = mean(RTz[which(cond.full == "LoEmoSurprise Sur.Neg")], na.rm = TRUE),
                               hi.emo.sur_n_RTz = mean(RTz[which(cond.full == "HiEmoSurprise Sur.Neg")], na.rm = TRUE),
                               lo.neu.sur_n_RTz = mean(RTz[which(cond.full == "LoNeuSurprise Sur.Neg")], na.rm = TRUE),
                               hi.neu.sur_n_RTz = mean(RTz[which(cond.full == "HiNeuSurprise Sur.Neg")], na.rm = TRUE),
                               lo.emo.sur_RTz = mean(RTz[which(cond.load == "LoEmoSurprise")], na.rm = TRUE),
                               hi.emo.sur_RTz = mean(RTz[which(cond.load == "HiEmoSurprise")], na.rm = TRUE),
                               lo.neu.sur_RTz = mean(RTz[which(cond.load == "LoNeuSurprise")], na.rm = TRUE),
                               hi.neu.sur_RTz = mean(RTz[which(cond.load == "HiNeuSurprise")], na.rm = TRUE),
                               lo.neu.RTz = mean(RTz[which(factor == "NEU LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.neu.RTz = mean(RTz[which(factor == "NEU HIGH" & trialtype == "face")], na.rm = TRUE),
                               lo.emo.RTz = mean(RTz[which(factor == "EMO LOW" & trialtype == "face")], na.rm = TRUE),
                               hi.emo.RTz = mean(RTz[which(factor == "EMO HIGH" & trialtype == "face")], na.rm = TRUE)))

write.csv(MT.data.rating.table2, paste("Data/Cleaned_Data/Final.Data.csv",format(Sys.time(),'_%Y-%m-%d_%H-%M-%S'),
                                      '.csv',sep = ''))


### MD analysis, just like RT by response ###
MT.results.small <- subset(MT.data.rating.table2[,c(1,56:63)])

shapiro.test(MT.results.small$lo.emo.sur_n_MAD)
shapiro.test(MT.results.small$lo.emo.sur_p_MAD)
wilcox.test(MT.results.small$lo.emo.sur_n_MAD, MT.results.small$lo.emo.sur_p_MAD, paired = TRUE)
MAD.long.resp <- gather(MT.results.small, key = "Condition", value = "MAD",
                        lo.emo.sur_n_MAD, lo.emo.sur_p_MAD)
ggplot(data = MAD.long.resp, aes(x = Condition, y = MAD)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .25, color = c("Black"))


shapiro.test(MT.results.small$hi.emo.sur_n_MAD)
shapiro.test(MT.results.small$hi.emo.sur_p_MAD)
wilcox.test(MT.results.small$hi.emo.sur_n_MAD, MT.results.small$hi.emo.sur_p_MAD, paired = TRUE)
MAD.long.resp <- gather(MT.results.small, key = "Condition", value = "MAD",
                        hi.emo.sur_n_MAD, hi.emo.sur_p_MAD)
ggplot(data = MAD.long.resp, aes(x = Condition, y = MAD)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .25, color = c("Black"))

shapiro.test(MT.results.small$lo.neu.sur_n_MAD)
shapiro.test(MT.results.small$lo.neu.sur_p_MAD)
wilcox.test(MT.results.small$lo.neu.sur_n_MAD, MT.results.small$lo.neu.sur_p_MAD, paired = TRUE)
MAD.long.resp <- gather(MT.results.small, key = "Condition", value = "MAD",
                        lo.neu.sur_n_MAD, lo.neu.sur_p_MAD)
ggplot(data = MAD.long.resp, aes(x = Condition, y = MAD)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .25, color = c("Black"))


shapiro.test(MT.results.small$hi.neu.sur_n_MAD)
shapiro.test(MT.results.small$hi.neu.sur_p_MAD)
wilcox.test(MT.results.small$hi.neu.sur_n_MAD, MT.results.small$hi.neu.sur_p_MAD, paired = TRUE)
MAD.long.resp <- gather(MT.results.small, key = "Condition", value = "MAD",
                        hi.neu.sur_n_MAD, hi.neu.sur_p_MAD)
ggplot(data = MAD.long.resp, aes(x = Condition, y = MAD)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .25, color = c("Black"))












shapiro.test(MT.data.rating.table2$lo.neu.sur_RTz)
RTz.long <- gather(MT.data.rating.table2, key = Condition, value = RTz,
               lo.neu.sur_RTz, hi.neu.sur_RTz, lo.emo.sur_RTz, hi.emo.sur_RTz)

### make factors ###
RTz.long$Load <- ifelse(RTz.long$Condition == "lo.neu.sur_RTz", "Low",
                        ifelse(RTz.long$Condition == "lo.emo.sur_RTz", "Low",
                               ifelse(RTz.long$Condition == "hi.neu.sur_RTz", "High",
                                      ifelse(RTz.long$Condition == "hi.emo.sur_RTz", "High", ""))))

RTz.long$Type <- ifelse(RTz.long$Condition == "lo.neu.sur_RTz", "Neutral",
                        ifelse(RTz.long$Condition == "lo.emo.sur_RTz", "Emotional",
                               ifelse(RTz.long$Condition == "hi.neu.sur_RTz", "Neutral",
                                      ifelse(RTz.long$Condition == "hi.emo.sur_RTz", "Emotional", ""))))

### make w/in subjs factors ###
RTz.long$subjID <- as.factor(RTz.long$subjID)

### traditional ANOVA ###
RTaov <- with(RTz.long,
              aov(RTz ~ (Load * Type) +
                    Error(subjID / (Load * Type))), contrasts = contr.sum())
summary(RTaov)

ggplot(RTz.long, aes(Condition, RTz, fill = as.factor(Condition))) +
  geom_bar(stat = "summary", fun.y = "mean", na.rm = TRUE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .25, color = c("Black"))



### analyzing MAD by resp ###
MAD.long.resp <- gather(MT.results.small, key = "Condition", value = "MAD",
                        lo.emo.sur_n_MAD, lo.emo.sur_p_MAD, hi.emo.sur_n_MAD, hi.emo.sur_p_MAD,
                        lo.neu.sur_n_MAD, lo.neu.sur_p_MAD, hi.neu.sur_n_MAD, hi.neu.sur_p_MAD)

MAD.long.resp$load <- ifelse(MAD.long.resp$Condition == "lo.emo.sur_n_MAD", "Low",
                            ifelse(MAD.long.resp$Condition == "lo.emo.sur_p_MAD", "Low",
                                   ifelse(MAD.long.resp$Condition == "lo.neu.sur_n_MAD", "Low",
                                          ifelse(MAD.long.resp$Condition == "lo.neu.sur_p_MAD", "Low", "High"))))
                            
MAD.long.resp$type <- ifelse(MAD.long.resp$Condition == "lo.emo.sur_n_MAD", "Emo",
                             ifelse(MAD.long.resp$Condition == "lo.emo.sur_p_MAD", "Emo",
                                    ifelse(MAD.long.resp$Condition == "hi.emo.sur_n_MAD", "Emo",
                                           ifelse(MAD.long.resp$Condition == "hi.emo.sur_p_MAD", "Emo", "Neu"))))

MAD.long.resp$resp <- ifelse(MAD.long.resp$Condition == "lo.emo.sur_n_MAD", "Neg",
                             ifelse(MAD.long.resp$Condition == "hi.emo.sur_n_MAD", "Neg",
                                    ifelse(MAD.long.resp$Condition == "hi.neu.sur_n_MAD", "Neg",
                                           ifelse(MAD.long.resp$Condition == "lo.neu.sur_n_MAD", "Neg", "Pos"))))


MAD.anova.resp <- aov(MAD ~ load * type * resp, data = MAD.long.resp)
summary.aov(MAD.anova.resp)

MAD.res.long.sum <- summarySE(MAD.long.resp, measurevar="MAD", groupvars = "Condition")
ggplot(MAD.res.long.sum, aes(Condition, MAD, fill = as.factor(Condition))) +
  geom_bar(stat = "summary", fun.y = "mean") +
  geom_bar(fun.data = mean_se, geom = "errorbar", width = .1)





MT.data.rating.table$sur_emo_avg <- rowMeans(MT.data.rating.table[,c("lo.emo.sur_rate", "hi.emo.sur_rate")]) 
MT.data.rating.table$sur_neu_avg <- rowMeans(MT.data.rating.table[,c("lo.neu.sur_rate", "hi.neu.sur_rate")]) 
MT.data.rating.table$emo.neu.chg <- MT.data.rating.table$lo.emo.sur_rate - MT.data.rating.table$lo.neu.sur_rate
MT.data.rating.table$sur_emoRT_avg <- rowMeans(MT.data.rating.table[,c("lo.emo.sur_RT", "hi.emo.sur_RT")]) 
MT.data.rating.table$sur_neuRT_avg <- rowMeans(MT.data.rating.table[,c("lo.neu.sur_RT", "hi.neu.sur_RT")]) 
MT.data.rating.table$emo.neu.RT.chg <- MT.data.rating.table$lo.emo.sur_RT - MT.data.rating.table$lo.neu.sur_RT
plot(MT.data.rating.table$emo.neu.chg, MT.data.rating.table$emo.neu.RT.chg)
cor.test(MT.data.rating.table$emo.neu.chg, MT.data.rating.table$emo.neu.RT.chg, method = "spearman")
t.test(MT.data.rating.table$lo.sur_n_RT, MT.data.rating.table$lo.neu.sur_p_RT, paired = TRUE)
mean(MT.data.rating.table$lo.neu.sur_n_RT, na.rm = TRUE)

mean(MT.data.rating.table$mem.cor.yes)
shapiro.test(MT.data.rating.table$mem.cor.yes)
### make data long ###
memory.long.data <- gather(MT.data.rating.table, key = "Condition", value = "Memory Accuracy",
                        mem.cor.yes, mem.cor.no)
friedman.test(`Memory Accuracy` ~ Condition|subjID, data = memory.long.data)
mean(MT.data.rating.table$mem.cor.no)
mean(MT.data.rating.table$mem.cor.yes)



### filter out incorrect face ratings ###
#MT.results2 <- subset(MT.results2, correct != 0)

### check for RT outliers ###
subRTs <- ddply(MT.data$data,"subjID", summarise, 
                RTs = mean(RT, na.rm = TRUE),
                RTsd = sd(RT, na.rm = TRUE))

#write.csv(subRTs, "~/Desktop/subrts.csv")
### group by sur-pos vs sur-neg ###
sur.MTresults2 <- subset(MT.results2, cond.correct == "Surprise")
sur.pos.MTresults2 <- subset(sur.MTresults2, rate == 0)
sur.neg.MTresults2 <- subset(sur.MTresults2, rate == 1)


library(yarrr)
### plot MDs by surprise response type ###
pirateplot(MAD ~ condition.rating +  type, data = sur.MTresults2,
           bean.f.col = c("Red", "Blue"),
           main = "MDs - Surprise by Response Type and Emo/Neu Load",
           inf.method = "se")
pirateplot(MAD ~ type + load, data = sur.pos.MTresults2,
           inf.method = "se",
           main = "Surprise as Positive - MDs")
pirateplot(MAD ~ type + load, data = sur.neg.MTresults2,
           inf.method = "se",
           main = "Surprise as Negative - MDs")

pirateplot(MD ~ condition, data = by.surp.wideMAD.spread,
           inf.method = "se",
           bean.f.col = c("Red", "Blue", "Red", "Blue"))

### make MAD wide ###
sur.pos.wideMAD <- sur.pos.MTresults2 %>% spread(cond.load, MAD)
sur.pos.wideMAD <- (ddply(sur.pos.wideMAD, "subjID", summarise,
                   hi.em.sur.MAD.p = mean(HiEmoSurprise, na.rm = TRUE),
                   lo.em.sur.MAD.p = mean(LoEmoSurprise, na.rm = TRUE),
                   hi.neu.sur.MAD.p = mean(HiNeuSurprise, na.rm = TRUE),
                   lo.neu.sur.MAD.p = mean(LoNeuSurprise, na.rm = TRUE),
                   LOW.surp.MAD.p = mean(c(LoNeuSurprise,LoEmoSurprise), na.rm = TRUE),
                   HI.surp.MAD.p = mean(c(HiNeuSurprise,HiEmoSurprise), na.rm = TRUE),
                   NEU.surp.MAD.p = mean(c(LoNeuSurprise,HiNeuSurprise), na.rm = TRUE),
                   EMO.surp.MAD.p = mean(c(LoEmoSurprise,HiEmoSurprise), na.rm = TRUE),
                   lo.emo.sur_p_RT = mean(lo.emo.sur_p_RT, na.rm = TRUE),
                   hi.emo.sur_p_RT = mean(hi.emo.sur_p_RT, na.rm = TRUE),
                   lo.neu.sur_p_RT = mean(lo.neu.sur_p_RT, na.rm = TRUE),
                   hi.neu.sur_p_RT = mean(hi.neu.sur_p_RT, na.rm = TRUE)))
                   
sur.neg.wideMAD <- sur.neg.MTresults2 %>% spread(cond.load, MAD)
sur.neg.wideMAD <- (ddply(sur.neg.wideMAD, "subjID", summarise,
                          hi.em.sur.MAD.n = mean(HiEmoSurprise, na.rm = TRUE),
                          lo.em.sur.MAD.n = mean(LoEmoSurprise, na.rm = TRUE),
                          hi.neu.sur.MAD.n = mean(HiNeuSurprise, na.rm = TRUE),
                          lo.neu.sur.MAD.n = mean(LoNeuSurprise, na.rm = TRUE),
                          LOW.surp.MAD.n = mean(c(LoNeuSurprise,LoEmoSurprise), na.rm = TRUE),
                          HI.surp.MAD.n = mean(c(HiNeuSurprise,HiEmoSurprise), na.rm = TRUE),
                          NEU.surp.MAD.n = mean(c(LoNeuSurprise,HiNeuSurprise), na.rm = TRUE),
                          EMO.surp.MAD.n = mean(c(LoEmoSurprise,HiEmoSurprise), na.rm = TRUE),
                          hi.neu.sur_n_RT = mean(hi.neu.sur_n_RT, na.rm = TRUE),
                          hi.emo.sur_n_RT = mean(hi.emo.sur_n_RT, na.rm = TRUE),
                          lo.emo.sur_n_RT = mean(lo.emo.sur_n_RT, na.rm = TRUE), 
                          lo.neu.sur_n_RT = mean(lo.neu.sur_n_RT, na.rm = TRUE)))
                          
                          


### merge ###
by.surp.wideMAD <- merge(sur.neg.wideMAD, sur.pos.wideMAD, by = "subjID")
#by.surp.wideMAD <- na.omit(by.surp.wideMAD)


#by.surp.wideMAD <- gather(by.surp.wideMAD, key = condition, value = MD,
                         # LowNeg, LowPos, HighNeg, HighPos)
#write.csv(by.surp.wideMAD, "~/Desktop/by.surp.MAD.csv")
### make MAD wide both ratings ###
wideMAD2 <- sur.MTresults2 %>% spread(cond.load, MAD)
wideMAD2 <- (ddply(wideMAD2, "subjID", summarise, 
                          lo.emo.sur_rate = mean(lo.emo.sur_rate, na.rm = TRUE),
                          hi.emo.sur_rate = mean(hi.emo.sur_rate, na.rm = TRUE),
                          lo.neu.sur_rate = mean(lo.neu.sur_rate, na.rm = TRUE),
                          hi.neu.sur_rate = mean(hi.neu.sur_rate, na.rm = TRUE),
                          hi.em.sur.MAD = mean(HiEmoSurprise, na.rm = TRUE),
                          lo.em.sur.MAD = mean(LoEmoSurprise, na.rm = TRUE),
                          hi.neu.sur.MAD = mean(HiNeuSurprise, na.rm = TRUE),
                          lo.neu.sur.MAD = mean(LoNeuSurprise, na.rm = TRUE),
                          lo.emo.sur_RT = mean(lo.emo.sur_RT, na.rm = TRUE),
                          hi.emo.sur_RT = mean(hi.emo.sur_RT, na.rm = TRUE),
                          lo.neu.sur_RT = mean(lo.neu.sur_RT, na.rm = TRUE),
                          hi.neu.sur_RT = mean(hi.neu.sur_RT, na.rm = TRUE),
                          lo.neu.mem = mean(ifelse((paste(type,load) == "NEULOW")),
                                            mem.cor, NA)))

### merge w/ by surp ratings ###
MAD.surp <- merge(by.surp.wideMAD, wideMAD2, by = "subjID")
MAD.surp$val_bias <- rowMeans(MAD.surp[c('lo.emo.sur_rate', 'hi.emo.sur_rate',
                                                 'lo.neu.sur_rate', 'hi.neu.sur_rate')],
                              na.rm = TRUE)

### filter out any subs with na's ###
MAD.surp <- MAD.surp[complete.cases(MAD.surp),]
shapiro.test(MAD.surp$val_bias)
View(MAD.surp)
## think about more positive (better emo reg) being able
## to better recall less negative pics?? 




# ### create LOW/HIGH main effect MD ###
# MAD.surp$LOW.surp.MAD.n <- ((MAD.surp$lo.neu.sur.MAD.n + MAD.surp$lo.em.sur.MAD.n)/2)
# MAD.surp$LOW.surp.MAD.p <- ((MAD.surp$lo.neu.sur.MAD.p + MAD.surp$lo.em.sur.MAD.p)/2)
# MAD.surp$HI.surp.MAD.n <- ((MAD.surp$hi.neu.sur.MAD.n + MAD.surp$hi.em.sur.MAD.n)/2)
# MAD.surp$HI.surp.MAD.p <- ((MAD.surp$hi.neu.sur.MAD.p + MAD.surp$hi.em.sur.MAD.p)/2)
# 
# ### create EMO/NEU main effect MD ###
# MAD.surp$EMO.surp.MAD.n <- ((MAD.surp$lo.em.sur.MAD.n + MAD.surp$hi.em.sur.MAD.n)/2)
# MAD.surp$EMO.surp.MAD.p <- ((MAD.surp$hi.em.sur.MAD.p + MAD.surp$lo.em.sur.MAD.p)/2)
# MAD.surp$NEU.surp.MAD.n <- ((MAD.surp$lo.neu.sur.MAD.n + MAD.surp$hi.neu.sur.MAD.n)/2)
# MAD.surp$NEU.surp.MAD.p <- ((MAD.surp$hi.neu.sur.MAD.p + MAD.surp$lo.neu.sur.MAD.p)/2)
### RTs Repeated Measures ANOVA (Load x Type x Response)
RT.surp.long <- gather(MAD.surp, key = RTcond, value = RT,
                        lo.neu.sur_n_RT, lo.neu.sur_p_RT,
                        hi.neu.sur_n_RT, hi.neu.sur_p_RT,
                        lo.emo.sur_n_RT, lo.emo.sur_p_RT,
                        hi.emo.sur_n_RT, hi.emo.sur_p_RT)
MAD.surp.long <- gather(MAD.surp, key = MADcond, value = MAD,
                         NEU.surp.MAD.n, NEU.surp.MAD.p,
                         EMO.surp.MAD.n, EMO.surp.MAD.p)
# LOW.surp.MAD.n, LOW.surp.MAD.p,
# HI.surp.MAD.n, HI.surp.MAD.p)
RT.surp.long$type <- ifelse(RT.surp.long$RTcond %in%
                               c("lo.neu.sur_n_RT", "lo.neu.sur_p_RT",
                                 "hi.neu.sur_n_RT", "hi.neu.sur_p_RT"),
                                 "Neutral", "Emotional")
MAD.surp.long$type <- ifelse(MAD.surp.long$MADcond %in%
                               c("NEU.surp.MAD.n", "NEU.surp.MAD.p"),
                             "Neutral", 
                             ifelse(MAD.surp.long$MADcond %in%
                                      c("EMO.surp.MAD.n", "EMO.surp.MAD.p"),
                              "Emotional",
                              ifelse(MAD.surp.long$MADcond %in%
                                      c("LOW.surp.MAD.n", "LOW.surp.MAD.p",
                                        "Low", "High"))))
RT.surp.long$load <- ifelse(RT.surp.long$RTcond %in%
                               c("lo.neu.sur_n_RT", "lo.neu.sur_p_RT",
                                 "lo.emo.sur_n_RT", "lo.emo.sur_p_RT"),
                             "Low", "High")

RT.surp.long$rate <- ifelse(RT.surp.long$RTcond %in%
                               c("lo.neu.sur_n_RT", "hi.neu.sur_n_RT",
                                 "lo.emo.sur_n_RT", "hi.emo.sur_n_RT"),
                             "Negative", "Positive")


MAD.surp.long$MADcond <- factor(MAD.surp.long$MADcond, levels = c("EMO.surp.MAD.n", "EMO.surp.MAD.p","NEU.surp.MAD.n","NEU.surp.MAD.p"))
pirateplot(MAD ~ MADcond, data = MAD.surp.long, inf.method = "se", avg.line.fun = mean,
           bean.f.col = c("#FB575B","#6C9549","#12C9FF", "#AB77C3"))
### colors purple lowpos AB77C3 ###
### colors green high pos 6C9549 ###
### colors red high neg FB575B ###
### colors blue low neg 12C9FF ###


MAD.surp.long.cleaned <- subset(MAD.surp.long, RT <= 3234.433)
MAD.surp.long.cleaned$typeload <- paste(MAD.surp.long.cleaned$type, MAD.surp.long.cleaned$load)
MAD.surp.RTcleanwide <- spread(MAD.surp.long.cleaned, RTcond, RT)




shapiro.test(MAD.surp$HI.surp.MAD.p)

write.csv(MAD.surp, "~/Desktop/RT423.csv")
### plotted in NSM poster ###
MAD.surp.long.cleaned$type <- factor(MAD.surp.long.cleaned$type, levels=c("Neutral","Emotional"))
MAD.surp.long.cleaned$load <- factor(MAD.surp.long.cleaned$load, levels=c("Low","High"))
RT.surp.long$typeload <- paste(RT.surp.long$type, RT.surp.long$load)
RT.surp.long$typeload <- factor(RT.surp.long$typeload, levels = c("Neutral Low", "Neutral High", "Emotional Low", "Emotional High"))
pirateplot(RT ~ typeload, RT.surp.long,
           inf.method = "se",
           bean.f.col = "white",
           bean.f.o = 0,
           bar.f.col = c("#ACACAC", "#4B4042", "#517BAC", "#A67BC0"),
           bar.f.o = .8)

MAD.surp.sum <- MAD.surp.long %>%
  group_by(type) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE),
            sd_RT = sd(RT, na.rm  = TRUE),
            n_RT = n(),
            SE_RT = sd(RT, na.rm = TRUE)/sqrt(n()))

p <- (ggplot(MAD.surp.sum, aes(x = type, y = mean_RT, fill = type)) + 
  geom_bar(stat = "Summary", fun.y = "mean") +
  scale_fill_manual(values = c("#A432C6","#3C3A3D")) +
  geom_errorbar(aes(ymin = mean_RT - SE_RT, ymax = mean_RT + SE_RT), width=.2) +
  theme_bw())  
p <-  p + labs(y="Mean RT +/- SE", x = "Type") + theme_classic()

ggplotly(p)

MAD.surp.sum1 <- MAD.surp.long %>%
  group_by(RTcond) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE),
            sd_RT = sd(RT, na.rm  = TRUE),
            n_RT = n(),
            SE_RT = sd(RT, na.rm = TRUE)/sqrt(n()))
p1 <- (ggplot(MAD.surp.sum1, aes(x = RTcond, y = mean_RT, fill = RTcond)) +
         geom_bar(stat = "Summary", fun.y = "mean"))
ggplotly(p1)


### check plot trajectories ###
MT.data.sur <- mt_subset(MT.data, cond.correct == "Surprise")
#MT.data.sur <- subset(MT.results2, cond.correct == "Surprise")
MT.data.sur$data$byresp <- paste(MT.data.sur$data$load, MT.data.sur$data$condition.rating,
                            sep = "")

#MT.data.sur$data$byresp <- paste(MT.data.sur$load, MT.data.sur$condition.rating,
                                 #sep = "")
# MT.data.sur <- ddply(MT.data.sur, "byresp", summarise, 
#                      MADm = mean(MAD),
#                      se = (sd(MAD/(sqrt(nrow(MT.data.sur))))))
mt_plot_aggregate(MT.data.sur, use = "tn_trajectories", color = "byresp")
View(MT.data.sur$data)
pirateplot(MAD ~ byresp, data = MT.data.sur, inf.method = "se")


### LOW LOAD MD X VALBIAS ###
plot(MAD.surp$val_bias, MAD.surp$LOW.surp.MAD.n,
     ylab = "MD - LOW Surp as Neg",
     xlab = "Valence Bias",
     xlim = c(0, 1),
     main = "MD X ValBias - Low Load Surprise Ratings",
     pch = "-",
     cex = 3,
     abline(lm(LOW.surp.MAD.n ~ val_bias, MAD.surp)))
cor.test(MAD.surp$val_bias, MAD.surp$LOW.surp.MAD.n, use = "pairwise.complete.obs",
         method = "spearman")
abline(lm(LOW.surp.MAD.n ~ val_bias, MAD.surp))

MAD.surp.long$plot <- ifelse(MAD.surp.long$MADcond == "HI.surp.MAD.n",
                             "High Load | Surprise Rated Negative",
                             ifelse(MAD.surp.long$MADcond == "HI.surp.MAD.p",
                                    "High Load | Surprise Rated Positive",
                                    ifelse(MAD.surp.long$MADcond == "LOW.surp.MAD.n",
                                           "Low Load | Surprise Rated Negative",
                                           ifelse(MAD.surp.long$MADcond == "LOW.surp.MAD.p",
                                                  "Low Load | Surprise Rated Positive", ""))))
MAD.surp.long$plot <- factor(MAD.surp.long$plot, levels = c("Low Load | Surprise Rated Negative",
                                                               "Low Load | Surprise Rated Positive",
                                                               "High Load | Surprise Rated Negative",
                                                               "High Load | Surprise Rated Positive"))
MAD.surp.long$`Valence Bias | Percent Negative Ratings` <- MAD.surp.long$val_bias
plot4 <- ggplot(data=MAD.surp.long,aes(x=`Valence Bias | Percent Negative Ratings`),y=MAD)+
  geom_point(size = 1) +
  stat_smooth(method = "lm") +
  theme_bw() +
  scale_y_continuous(limits = c(-.25, 1.25))
plot4 + facet_wrap( ~ plot)  

plot(MAD.surp$val_bias, MAD.surp$LOW.surp.MAD.p,
     ylab = "MD - LOW Surp as Pos",
     xlab = "Valence Bias",
     xlim = c(0, 1),
     main = "MD X ValBias - Low Load Surprise Ratings",
     pch = 3,
     cex = 3,
     abline(lm(LOW.surp.MAD.p ~ val_bias, MAD.surp)))
cor.test(MAD.surp$val_bias, MAD.surp$LOW.surp.MAD.p, 
         method = "spearman", use = "pairwise.complete.obs")

ggplot(data=MAD.surp,aes(x=val_bias,y=LOW.surp.MAD.p))+
  geom_point(shape = 3, size = 3)+
  stat_smooth(method = "lm") +
  theme_bw() + 
  scale_y_continuous(limits = c(-.25, 1.25))

points(neg, pos, col = "red")

### HIGH LOAD MD X VALBIAS ###
plot(MAD.surp$val_bias, MAD.surp$HI.surp.MAD.n,
     ylab = "MD - HIGH Surp as Neg",
     xlab = "Valence Bias",
     main = "MD X ValBias - High Load Surprise Ratings",
     xlim = c(0, 1),
     cex = 2)
cor.test(MAD.surp$val_bias, MAD.surp$HI.surp.MAD.n, 
         method = "spearman",use = "complete.pairwise.obs")
abline(lm(HI.surp.MAD.n ~ val_bias, MAD.surp))
ggplot(data=MAD.surp,aes(x=val_bias,y=HI.surp.MAD.n))+
  geom_point(shape = 45, size = 10) +
  stat_smooth(method = "lm") +
  theme_bw() + 
  scale_y_continuous(limits = c(-.25, 1.25))




plot(MAD.surp$val_bias, MAD.surp$HI.surp.MAD.p,
     ylab = "MD - HIGH Surp as Pos",
     xlab = "Valence Bias",
     main = "MD X ValBias - High Load Surprise Ratings",
     xlim = c(0, 1),
     pch = 3,
     cex = 2)
cor.test(MAD.surp$val_bias, MAD.surp$HI.surp.MAD.p, 
         method = "spearman",use = "complete.pairwise.obs")
abline(lm(HI.surp.MAD.p ~ val_bias, MAD.surp))
ggplot(data=MAD.surp,aes(x=val_bias,y=HI.surp.MAD.p))+
  geom_point(shape = 3, size = 3)+
  stat_smooth(method = "lm") +
  theme_bw()+ 
  scale_y_continuous(limits = c(-.25, 1.25))


shapiro.test(MAD.surp$lo.neu.sur_rate)

### EMO LOAD X VALBIAS ###
plot(MAD.surp$val_bias, MAD.surp$EMO.surp.MAD.n,
     ylab = "MD - EMO Surp as Neg",
     xlab = "Valence Bias",
     main = "MD X ValBias - Emo Load Surprise Ratings",
     xlim = c(0, 1),
     cex = 2)
cor.test(MAD.surp$val_bias, MAD.surp$EMO.surp.MAD.n, 
         method = "spearman",use = "complete.pairwise.obs")
abline(lm(EMO.surp.MAD.n ~ val_bias, MAD.surp))
ggplot(data=MAD.surp,aes(x=val_bias,y=EMO.surp.MAD.n))+
  geom_point(shape = 45, size = 10) +
  stat_smooth(method = "lm") +
  theme_bw() + 
  scale_y_continuous(limits = c(-.25, 1.25))

plot(MAD.surp$val_bias, MAD.surp$EMO.surp.MAD.p,
     ylab = "MD - EMO Surp as Pos",
     xlab = "Valence Bias",
     main = "MD X ValBias - Emo Load Surprise Ratings",
     xlim = c(0, 1),
     cex = 2,
     pch = 3)
cor.test(MAD.surp$val_bias, MAD.surp$EMO.surp.MAD.p,
         method = "spearman",use = "complete.pairwise.obs")
abline(lm(EMO.surp.MAD.p ~ val_bias, MAD.surp))
ggplot(data=MAD.surp,aes(x=val_bias,y=EMO.surp.MAD.p))+
  geom_point(shape = 3, size = 3)+
  stat_smooth(method = "lm") +
  theme_bw()

### NEU LOAD X VALBIAS ###
plot(MAD.surp$val_bias, MAD.surp$NEU.surp.MAD.n,
     ylab = "MD - NEU Surp as Neg",
     xlab = "Valence Bias",
     main = "MD X ValBias - Neu Load Surprise Ratings",
     xlim = c(0, 1),
     cex = 2)
cor.test(MAD.surp$val_bias, MAD.surp$NEU.surp.MAD.n, use = "complete.pairwise.obs")
abline(lm(NEU.surp.MAD.n ~ val_bias, MAD.surp))
ggplot(data=MAD.surp,aes(x=val_bias,y=NEU.surp.MAD.n))+
  geom_point(shape = 45, size = 10) +
  stat_smooth(method = "lm") +
  theme_bw() 

plot(MAD.surp$val_bias, MAD.surp$NEU.surp.MAD.p,
     ylab = "MD - NEU Surp as Pos",
     xlab = "Valence Bias",
     main = "MD X ValBias - Neu Load Surprise Ratings",
     xlim = c(0, 1),
     cex = 2,
     pch = 3)
cor.test(MAD.surp$val_bias, MAD.surp$NEU.surp.MAD.p)
abline(lm(NEU.surp.MAD.p ~ val_bias, MAD.surp))
ggplot(data=MAD.surp,aes(x=val_bias,y=NEU.surp.MAD.p))+
  geom_point(shape = 3, size = 3)+
  stat_smooth(method = "lm") +
  theme_bw()


#####
#####
#####
#####

### check RT normality ###
{### hi.neu.sur_p_RT = normal ###
### hi.emo.sur_n_RT = normal ###
### all others non-normal ###
shapiro.test(MAD.surp$lo.neu.sur_n_RT)
shapiro.test(MAD.surp$lo.neu.sur_p_RT)
shapiro.test(MAD.surp$lo.emo.sur_n_RT)
shapiro.test(MAD.surp$lo.emo.sur_p_RT)
shapiro.test(MAD.surp$hi.neu.sur_n_RT)
shapiro.test(MAD.surp$hi.neu.sur_p_RT)
shapiro.test(MAD.surp$hi.emo.sur_n_RT)
shapiro.test(MAD.surp$hi.emo.sur_p_RT)
}


### LOW EMO MD X VALBIAS ###
plot(MAD.surp$val_bias, MAD.surp$lo.em.sur.MAD.n,
     ylab = "MD - LOW EMO Surp as Neg",
     xlab = "Valence Bias",
     xlim = c(0, 1),
     main = "Low Emo MD X ValBias",
     cex = 2)
cor.test(MAD.surp$val_bias, MAD.surp$lo.em.sur.MAD.n)
abline(lm(lo.em.sur.MAD.n ~ val_bias, MAD.surp))

plot(MAD.surp$val_bias, MAD.surp$lo.em.sur.MAD.p,
     ylab = "MD - LOW EMO Surp as Pos",
     xlab = "Valence Bias",
     main = "Low Emo MD X ValBias",
     cex = 2,
     pch = 3)
cor.test(MAD.surp$val_bias, MAD.surp$lo.em.sur.MAD.p)
abline(lm(lo.em.sur.MAD.p ~ val_bias, MAD.surp))

### LOW NEU MD X VALBIAS ###
plot(MAD.surp$val_bias, MAD.surp$lo.neu.sur.MAD.n,
     ylab = "MD - LOW NEU Surp as Neg",
     xlab = "Valence Bias",
     main = "Low Neu MD X ValBias",
     cex = 2)
cor.test(MAD.surp$val_bias, MAD.surp$lo.neu.sur.MAD.n)
abline(lm(lo.neu.sur.MAD.n ~ val_bias, MAD.surp))

plot(MAD.surp$val_bias, MAD.surp$lo.neu.sur.MAD.p,
     ylab = "MD - LOW NEU Surp as Pos",
     xlab = "Valence Bias",
     main = "Low Neu MD X ValBias",
     cex = 2,
     pch = 3)
cor.test(MAD.surp$val_bias, MAD.surp$lo.neu.sur.MAD.p)
abline(lm(lo.neu.sur.MAD.p ~ val_bias, MAD.surp))

### HIGH EMO MD X VALBIAS ###
plot(MAD.surp$val_bias, MAD.surp$hi.em.sur.MAD.n,
     ylab = "MD - HIGH EMO Surp as Neg",
     xlab = "Valence Bias",
     main = "High Emo MD X ValBias",
     cex = 2)
cor.test(MAD.surp$val_bias, MAD.surp$hi.em.sur.MAD.n)
abline(lm(hi.em.sur.MAD.n ~ val_bias, MAD.surp))

plot(MAD.surp$val_bias, MAD.surp$hi.em.sur.MAD.p,
     ylab = "MD - HIGH EMO Surp as Pos",
     xlab = "Valence Bias",
     main = "High Emo MD X ValBias",
     cex = 2,
     pch = 3)
cor.test(MAD.surp$val_bias, MAD.surp$hi.em.sur.MAD.p)
abline(lm(hi.em.sur.MAD.p ~ val_bias, MAD.surp))

### HIGH NEU MD X VALBIAS ###
plot(MAD.surp$val_bias, MAD.surp$hi.neu.sur.MAD.n,
     ylab = "MD - HIGH NEU Surp as Neg",
     xlab = "Valence Bias",
     main = "High Neu MD X ValBias",
     cex = 2)
cor.test(MAD.surp$val_bias, MAD.surp$hi.neu.sur.MAD.n)
abline(lm(hi.neu.sur.MAD.n ~ val_bias, MAD.surp))

plot(MAD.surp$val_bias, MAD.surp$hi.neu.sur.MAD.p,
     ylab = "MD - HIGH NEU Surp as Pos",
     xlab = "Valence Bias",
     main = "High Neu MD X ValBias",
     cex = 2,
     pch = 3)
cor.test(MAD.surp$val_bias, MAD.surp$hi.neu.sur.MAD.p)
abline(lm(hi.neu.sur.MAD.p ~ val_bias, MAD.surp))


### need to add pos vs. neg response into the MADs above... ###

plot(wideMAD2$lo.emo.sur_rate, wideMAD2$lo.em.sur.MAD)
lm(lo.em.sur.MAD ~ lo.emo.sur_rate, wideMAD2)
abline(.6634, -.4547)
cor.test(wideMAD2$lo.emo.sur_rate, wideMAD2$lo.em.sur.MAD)

plot(wideMAD2$hi.emo.sur_rate, wideMAD2$hi.em.sur.MAD)
lm(hi.em.sur.MAD ~ hi.emo.sur_rate, wideMAD2)
abline(.6367, -.3)
cor.test(wideMAD2$hi.emo.sur_rate, wideMAD2$hi.em.sur.MAD)

plot(wideMAD2$lo.neu.sur_rate, wideMAD2$lo.neu.sur.MAD)
lm(lo.neu.sur.MAD ~ lo.neu.sur_rate, wideMAD2)
abline(.4803, -.236)
cor.test(wideMAD2$lo.neu.sur_rate, wideMAD2$lo.neu.sur.MAD)

plot(wideMAD2$hi.neu.sur_rate, wideMAD2$hi.neu.sur.MAD)
lm(hi.neu.sur.MAD ~ hi.neu.sur_rate, wideMAD2)
abline(.4621, -.1165)
cor.test(wideMAD2$hi.neu.sur_rate, wideMAD2$hi.neu.sur.MAD)







plot(MAD.plot2$lo.emo.sur_rate, MAD.plot2$lo.em.sur.MAD)
cor.test(MAD.plot2$lo.emo.sur_rate, MAD.plot2$lo.em.sur.MAD)



















### create low vs. high for ttest ###
MT.data.rating.table$LOWsur <- ((MT.data.rating.table$lo.emo.sur_rate + MT.data.rating.table$lo.neu.sur_rate)/2)
MT.data.rating.table$HIGHsur <- ((MT.data.rating.table$hi.emo.sur_rate + MT.data.rating.table$hi.neu.sur_rate)/2)
t.test(MT.data.rating.table$LOWsur, MT.data.rating.table$HIGHsur, paired = TRUE)
### no diff b/w high vs low (p =.486) ###





### aggregate MT measures across conditions ###
MT.agg <- mt_aggregate(MT.data, use = "measures", use2_variables = "cond.load")


####
###
###
###
###
#       NEED TO REMOVE INCORRECT TRIALS!!!! #####

### plot face MDs in bean plot ###
face.MT.results <- subset(MT.results, cond.correct %in% c("Happy", "Angry", "Surprise"))
pirateplot(MAD ~ load + type + cond.correct  , face.MT.results, inf.method = "ci")


hap.MD <- subset(MT.results, cond.correct %in% c("Happy"))
pirateplot(MAD ~ load + type + cond.correct  , hap.MD, inf.method = "ci",
           bean.f.col = c("Dark Blue", "Light Blue"))

ang.MD <- subset(MT.results, cond.correct %in% c("Angry"))
pirateplot(MAD ~ load + type + cond.correct  , ang.MD, inf.method = "ci",
           bean.f.col = c("Dark Red", "Red"))

sur.MD <- subset(MT.results, cond.correct %in% c("Surprise"))
pirateplot(MAD ~ load + type + cond.correct  , sur.MD, inf.method = "ci",
           bean.f.col = c("Dark Green", "Light Green"))


### plot trajectories of face ratings ###

### surp trajectories ###
faces.MT.data <- mt_subset(MT.data, trialtype == "face")
surp.MT.data <- mt_subset(faces.MT.data, cond.correct == "Surprise")
mt_plot_aggregate(surp.MT.data, use = "tn_trajectories", color = "cond.load")

### ang trajectories ###
ang.MT.data <- mt_subset(faces.MT.data, cond.correct == "Angry")
mt_plot_aggregate(ang.MT.data, use = "tn_trajectories", color = "cond.load")

### hap trajectories ###
hap.MT.data <- mt_subset(faces.MT.data, cond.correct == "Happy")
mt_plot_aggregate(hap.MT.data, use = "tn_trajectories", color = "cond.load")














### subset faces ###

faces.MT <- dplyr::inner_join(
  faces.MT.data$data, faces.MT.data$measures,
  by="mt_id")

mt_plot_aggregate(faces.MT.data, use = "tn_trajectories", color = "cond.load")

### write final csv ###
write.csv(MT.data.rating.table, "Final.Data.20190417.csv")
rm(bound.data)










### get your data collapsed per sub, merge this with the final data! ###
set1.vc.files.persub <- mt_aggregate_per_subject(set1.vc.files, use = "measures", subject_id = "subjID" )
### merge MT measures w/ rating data ###
session1.ratings.table <- merge(session1.data.persub, session1.ratings.table, by = "subjID")
### write output for session1 ###


### read in all the MT files for session two ###
session2files <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/depletion_study/study1/data_ratings/Raw_data/R_Analysis/Session2", 
                            pattern = "*.mt", full.names = TRUE, recursive = FALSE)






















### Begin the data merging ###
###
###
###
###
##
##
##
##
#

### combine into one table ###
study1.all.sessions.ratings <- merge(session1.ratings.table, session2.ratings.table, by = "subjID")
study2.all.sessions.ratings <- merge(study2.session1.ratings.table, study2.session2.ratings.table, by = "subjID")

### combine with conditions csv's (one for each study) or metadata.xlsx file created by hand ###
### NOTE: Conditions ( 1 = non; 2 = emo; 3 = con; 4 = unknown) ###
study1.conditions <- read_csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study1/data_ratings/Raw_data/R_Analysis/conditions.study1.csv")
study2.conditions <- read_csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study1/data_ratings/Raw_data/R_Analysis/conditions.study2.csv")
all.study1.sessions.ratings <- merge(conditions, all.sessions.ratings, by = "subjID")

### add ID-recode and conditions ###
study1.all.sessions.ratings <- merge(study1.all.sessions.ratings, study1.conditions, by = "subjID")
study2.all.sessions.ratings <- merge(study2.all.sessions.ratings, study2.conditions, by = "subjID")

### combine these bad boys ###
final.ratings <- rbind(study1.all.sessions.ratings,study2.all.sessions.ratings)

### YAYYYYY MAKE CSV FILE!!! ####
write.csv(final.ratings, file = "final_ratings.csv")

### what if we want these dataframes as mousetrap objects?? do this! ###
bound.data <- rbind(study1.session1.data, study1.session2.data)
bound.data <- rbind(bound.data, study2.session1.data)
bound.data <- rbind(bound.data, study2.session2.data)
mousetrap.data <- mt_import_wide(bound.data)


### MANUALLY ADDED ID-RECODE; SHOULD CODE THIS TO BE PULLED FROM METADATA FILE ###

# <- read_csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study1/data_ratings/Raw_data/R_Analysis/final_ratings.csv")

### Import survey data from session one ###
surveys.s1 <- read_csv("~/Documents/Nick-Grad/Neta_Lab/depletion_study/study1/data_ratings/Raw_data/R_Analysis/surveys_s1.csv")

### merge ratings with session one surveys ###
full.data <- merge(final.ratings, surveys.s1, by = "ID-Recode")

### compute STAI score ###
### make STAI dataframe ###
STAI <- full.data[,c(1, 81:120)]

### score STAI state ###
full.data$STAI.S <- (rowSums(STAI[, c(2:21)]))

### score STAI trait ###
full.data$STAI.T <- (rowSums(STAI[, c(22:41)]))

### compute ERQ score ###
### create ERQ dataframe ###
ERQ <- full.data[,c(1, 121:130)]


### add items for participant scores and create column in survey.data ###
full.data$ERQ_CR <-  (rowSums(ERQ[,c("ERQ.1", "ERQ.3", "ERQ.5", 
                               "ERQ.7", "ERQ.8", "ERQ.10")], 
                        na.rm = FALSE)  / 6)

full.data$ERQ_ES <- ( rowSums(ERQ[,c("ERQ.2", "ERQ.4", "ERQ.6", 
                               "ERQ.9")], 
                        na.rm = FALSE) / 4 )

### compute PANAS.s1.t1 score ###
### create PANAS dataframe ###
PANAS.s1.t1 <- full.data[,c(1, 131:150)]

### add items for positive affect ###
full.data$PANAS.s1.t1.PA <-  (rowSums(PANAS.s1.t1[,c(2, 4, 6, 10, 11, 13, 15, 17, 18, 20)], 
                              na.rm = FALSE)) 

### add items for negative affect ###
full.data$PANAS.s1.t1.NA <-  (rowSums(PANAS.s1.t1[,c(3, 5, 7, 8, 9, 12, 14, 16, 19, 21)], 
                                      na.rm = FALSE)) 

write.csv(full.data, file = "full.data.csv")


  



ggplot(MAD.surp, aes_(MAD.surp$val_bias, MAD.surp$lo.em.sur.MAD.n) +
         geom_point(color = "black"))

scatter_by(MAD.surp, val_bias, lo.em.sur.MAD.n)




#### TRY RUNNING CORRELATIONS W/ S1 VAL BIAS ###
master.mad <- merge(wide.MAD, MAD.surp, by="subjID")
### LOW LOAD MD X VALBIAS ###
plot(master.mad$sur_rate, master.mad$LOW.surp.MAD.n,
     ylab = "MD - LOW Surp as Neg",
     xlab = "Valence Bias",
     xlim = c(0, 1),
     main = "MD X ValBias - Low Load Surprise Ratings",
     pch = 1,
     cex = 2)
cor.test(master.mad$sur_rate, master.mad$LOW.surp.MAD.n)
abline(lm(LOW.surp.MAD.n ~ sur_rate, master.mad))

plot(master.mad$sur_rate, master.mad$LOW.surp.MAD.p,
     ylab = "MD - LOW Surp as Pos",
     xlab = "Valence Bias",
     xlim = c(0, 1),
     main = "MD X ValBias - Low Load Surprise Ratings",
     pch = 3,
     cex = 2)
cor.test(master.mad$sur_rate, master.mad$LOW.surp.MAD.p)
abline(lm(LOW.surp.MAD.p ~ sur_rate, master.mad))

### HIGH LOAD MD X VALBIAS ###
plot(master.mad$sur_rate, master.mad$HI.surp.MAD.n,
     ylab = "MD - HIGH Surp as Neg",
     xlab = "Valence Bias",
     main = "MD X ValBias - High Load Surprise Ratings",
     xlim = c(0, 1),
     cex = 2)
cor.test(master.mad$sur_rate, master.mad$HI.surp.MAD.n)
abline(lm(HI.surp.MAD.n ~ sur_rate, master.mad))

plot(master.mad$sur_rate, master.mad$HI.surp.MAD.p,
     ylab = "MD - HIGH Surp as Pos",
     xlab = "Valence Bias",
     main = "MD X ValBias - High Load Surprise Ratings",
     xlim = c(0, 1),
     pch = 3,
     cex = 2)
cor.test(master.mad$sur_rate, master.mad$HI.surp.MAD.p)
abline(lm(HI.surp.MAD.p ~ sur_rate, master.mad))

### EMO LOAD X VALBIAS ###
plot(master.mad$sur_rate, master.mad$EMO.surp.MAD.n,
     ylab = "MD - EMO Surp as Neg",
     xlab = "Valence Bias",
     xlim = c(0, 1),
     cex = 2)
cor.test(master.mad$sur_rate, master.mad$EMO.surp.MAD.n)
abline(lm(EMO.surp.MAD.n ~ sur_rate, master.mad))

plot(master.mad$sur_rate, master.mad$EMO.surp.MAD.p,
     ylab = "MD - EMO Surp as Pos",
     xlab = "Valence Bias",
     main = "MD X ValBias - EMO Surp as Pos",
     xlim = c(0, 1),
     cex = 2,
     pch = 3)
cor.test(master.mad$sur_rate, master.mad$EMO.surp.MAD.p)
abline(lm(EMO.surp.MAD.p ~ sur_rate, master.mad))

### NEU LOAD X VALBIAS ###
plot(master.mad$sur_rate, master.mad$NEU.surp.MAD.n,
     ylab = "MD - NEU Surp as Neg",
     xlab = "Valence Bias",
     xlim = c(0, 1))
cor.test(master.mad$sur_rate, master.mad$NEU.surp.MAD.n)
abline(lm(NEU.surp.MAD.n ~ sur_rate, master.mad))

plot(master.mad$sur_rate, master.mad$NEU.surp.MAD.p,
     ylab = "MD - NEU Surp as Pos",
     xlab = "Valence Bias",
     xlim = c(0, 1))
cor.test(master.mad$sur_rate, master.mad$NEU.surp.MAD.p)
abline(lm(NEU.surp.MAD.p ~ sur_rate, master.mad))



#####
#####
#####
#####

### LOW EMO MD X VALBIAS ###
plot(master.mad$sur_rate, master.mad$lo.em.sur.MAD.n,
     ylab = "MD - LOW EMO Surp as Neg",
     xlab = "Valence Bias",
     xlim = c(0, 1),
     main = "Low Emo MD X ValBias",
     cex = 2)
cor.test(master.mad$sur_rate, master.mad$lo.em.sur.MAD.n)
abline(lm(lo.em.sur.MAD.n ~ sur_rate, master.mad))

plot(master.mad$sur_rate, master.mad$lo.em.sur.MAD.p,
     ylab = "MD - LOW EMO Surp as Pos",
     xlab = "Valence Bias",
     main = "Low Emo MD X ValBias",
     cex = 2,
     pch = 3)
cor.test(master.mad$sur_rate, master.mad$lo.em.sur.MAD.p)
abline(lm(lo.em.sur.MAD.p ~ sur_rate, master.mad))

### LOW NEU MD X VALBIAS ###
plot(master.mad$sur_rate, master.mad$lo.neu.sur.MAD.n,
     ylab = "MD - LOW NEU Surp as Neg",
     xlab = "Valence Bias",
     main = "Low Neu MD X ValBias",
     cex = 2)
cor.test(master.mad$sur_rate, master.mad$lo.neu.sur.MAD.n)
abline(lm(lo.neu.sur.MAD.n ~ sur_rate, master.mad))

plot(master.mad$sur_rate, master.mad$lo.neu.sur.MAD.p,
     ylab = "MD - LOW NEU Surp as Pos",
     xlab = "Valence Bias",
     main = "Low Neu MD X ValBias",
     cex = 2,
     pch = 3)
cor.test(master.mad$sur_rate, master.mad$lo.neu.sur.MAD.p)
abline(lm(lo.neu.sur.MAD.p ~ sur_rate, master.mad))

### HIGH EMO MD X VALBIAS ###
plot(master.mad$sur_rate, master.mad$hi.em.sur.MAD.n,
     ylab = "MD - HIGH EMO Surp as Neg",
     xlab = "Valence Bias",
     main = "High Emo MD X ValBias",
     cex = 2)
cor.test(master.mad$sur_rate, master.mad$hi.em.sur.MAD.n)
abline(lm(hi.em.sur.MAD.n ~ sur_rate, master.mad))

plot(master.mad$sur_rate, master.mad$hi.em.sur.MAD.p,
     ylab = "MD - HIGH EMO Surp as Pos",
     xlab = "Valence Bias",
     main = "High Emo MD X ValBias",
     cex = 2,
     pch = 3)
cor.test(master.mad$sur_rate, master.mad$hi.em.sur.MAD.p)
abline(lm(hi.em.sur.MAD.p ~ sur_rate, master.mad))

### HIGH NEU MD X VALBIAS ###
plot(master.mad$sur_rate, master.mad$hi.neu.sur.MAD.n,
     ylab = "MD - HIGH NEU Surp as Neg",
     xlab = "Valence Bias",
     main = "High Neu MD X ValBias",
     cex = 2)
cor.test(master.mad$sur_rate, master.mad$hi.neu.sur.MAD.n)
abline(lm(hi.neu.sur.MAD.n ~ sur_rate, master.mad))

plot(master.mad$sur_rate, master.mad$hi.neu.sur.MAD.p,
     ylab = "MD - HIGH NEU Surp as Pos",
     xlab = "Valence Bias",
     main = "High Neu MD X ValBias",
     cex = 2,
     pch = 3)
cor.test(master.mad$sur_rate, master.mad$hi.neu.sur.MAD.p)
abline(lm(hi.neu.sur.MAD.p ~ sur_rate, master.mad))







## LOOK AT THE FIRST HALF OF THE TRAILS (MAYBE EVEN FIRST THIRD) ###
### MOVING TRIALS / JACKKNIFING? ESTIMATE OF HOW IT CHANGES FROM TRIAL TO TRIAL ###
### FIRST 15 OR SO FOR EACH CONDITION ###

### ACTUALLY JUST LOOK AT FIRST BLOCK ###

MT.results2.bl1 <- subset(MT.results2, order <= 54)




### calculate ch

MT.data.sur <- mt_subset(MT.data, cond.correct == "Surprise")
MT.data.sur$data$finalfact <- paste(MT.data.sur$data$type, MT.data.sur$data$response)
mt_plot_aggregate(data = MT.data.sur, use = "tn_trajectories", color = "finalfact",
                  only_ggplot = TRUE) + geom_path(size = 2) + theme_bw()
colors <- c("#FF2330","#48F456","#FF9190","#C1F48E")
### colors purple lowpos AB77C3 ###
### colors green high pos 6C9549 ###
### colors red high neg FB575B ###
### colors blue low neg 12C9FF ###

# + scale_fill_manual(values = 
#                                                             c("#A432C6","#3C3A3D",
#                                                               "84BDA1", "84BDA1")) + geom_path(alpha = 1) + 
#                                                             theme_bw() + 
#   geom_line(size = 1) +
#   theme(axis.text = element_text(size = 18))


?mt_plot_aggregate()
