### set wd ###
nhpath <- "~/Documents/Nick-Grad/Neta_Lab/covid19/Git/data/"
# cbpath <- "~/Documents/Github/COVID/data/"
setwd(nhpath)

#install.packages("anytime")
library(anytime)
library(lubridate)

### READ IN CONSENT 
cbs_soc_v4_consent <- read.csv("data_exp_17280-v4_questionnaire-4qon.csv")
cbs_soc_v5_consent <- read.csv("data_exp_17280-v5_questionnaire-4qon.csv")
cbs_soc_v6_consent <- read.csv("data_exp_17280-v6_questionnaire-4qon.csv")

### pull final questionnaire node to get duration ###
cbs_soc_end_v4 <- read.csv("data_exp_17280-v4_questionnaire-1r6c.csv")
cbs_soc_end_v5 <- read.csv("data_exp_17280-v5_questionnaire-1r6c.csv")
cbs_soc_end_v6 <- read.csv("data_exp_17280-v6_questionnaire-1r6c.csv")

cbs_soc_end_v4 <- cbs_soc_end_v4[!grepl("END OF FILE", cbs_soc_end_v4$Event.Index), ]
cbs_soc_end_v5 <- cbs_soc_end_v5[!grepl("END OF FILE", cbs_soc_end_v5$Event.Index), ]
cbs_soc_end_v6 <- cbs_soc_end_v6[!grepl("END OF FILE", cbs_soc_end_v6$Event.Index), ]

### read in the final node for "end time" 
cbs_times <- merge(cbs_soc_v4_consent, cbs_soc_end_v4, by = "Participant.Private.ID")

cbs_times2 <- merge(cbs_soc_v5_consent, cbs_soc_end_v5, by = "Participant.Private.ID")

cbs_times3 <- merge(cbs_soc_v6_consent, cbs_soc_end_v6, by = "Participant.Private.ID")

cbs_times <- cbs_times[, c("UTC.Date.x", "UTC.Date.y", "Local.Timezone.x", "Local.Timezone.y",
                           "Local.Date.x", "Local.Date.y", "Participant.Private.ID",
                           "Participant.Public.ID.x", "Participant.Completion.Code.x")]
cbs_times2 <- cbs_times2[, c("UTC.Date.x", "UTC.Date.y", "Local.Timezone.x", "Local.Timezone.y",
                             "Local.Date.x", "Local.Date.y", "Participant.Private.ID",
                             "Participant.Public.ID.x", "Participant.Completion.Code.x")]
cbs_times3 <- cbs_times3[, c("UTC.Date.x", "UTC.Date.y", "Local.Timezone.x", "Local.Timezone.y",
                             "Local.Date.x", "Local.Date.y", "Participant.Private.ID",
                             "Participant.Public.ID.x", "Participant.Completion.Code.x")]

cbs_times <- rbind(cbs_times, cbs_times2, cbs_times3)


cbs_times$Local.Date.x <- anytime(cbs_times$Local.Date.x)
cbs_times$Local.Date.y <- anytime(cbs_times$Local.Date.y)

cbs_times$duration <- interval(cbs_times$Local.Date.x, cbs_times$Local.Date.y) %/% seconds(1)
write.csv(cbs_times, "social_media_durations.csv")

# cbs_times$Duration <- as.POSIXct.Date(cbs_times$Local.Date.x) - as.POSIXct.Date(cbs_times$Local.Date.y)
