
library("foreign")
library("plyr")
library("dplyr")
library("car")
library("MASS")
library("sfsmisc")
library("QuantPsyc")
library("forcats")
library("reshape2")


#### Data Handling ####
# 2017
eu2017 <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/06. Concurrent and convergent validity of a single, brief question for physical activity assessment_PUBLICADO/MMR/Modified eu 2017.sav", to.data.frame = T, use.missings = T)
eu2017 <- eu2017[c(6, 9, 43:44, 45:46, 47, 48, 50:51, 53:54, 56, 57:58, 223, 224,222, 277, 242,291, 236, 237:240, 270, 11, 420)]
names(eu2017)[2] <- "ID"
names(eu2017)[3:15] <- c("Sport_Freq", "Sport_Freq_rec", "PA_Freq", "PA_Freq_rec", "Sport_PA_freq", "Vig_Days", "Vig_Time", "Mod_Days", "Mod_Time", "Walk_Days", "Walk_Time", "Sit", "Sit_rec") 
names(eu2017)[16:28] <- c("Gender", "Age","Marital status", "Social class subjective", "Type of community", "Sizeofcommunity" ,"Education", "Ocupation", "Ocupation_rec1", "Ocupation_rec2", "Ocupation_last_job", "Bills", "Country")
names(eu2017)[29] <- "w23"
eu2017["survey"] <- "2017"
eu2017 <- eu2017[which(eu2017$Age >= 18), ]

eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "Never" & eu2017$Sport_Freq == "DK")), ] # REMOVE NOT VALID CASES
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "Never")), ]
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "DK")), ]
eu2017$Sport_PA_freq <- fct_recode(eu2017$Sport_PA_freq, Never = "Never/DK")



#### IPAQ, VPA, MPA, MVPA calculation in METs (and Time per Week) ####

eu2017$Vig_Days <- as.numeric(eu2017$Vig_Days) # TO CONTINIOUS
eu2017$Mod_Days <- as.numeric(eu2017$Mod_Days)
eu2017$Walk_Days <- as.numeric(eu2017$Walk_Days)

eu2017$Vig_Days[which(is.na(eu2017$Vig_Days))] <- 0 # RECODING AND  MISSING VALUES RECOVERY
eu2017$Vig_Days[eu2017$Vig_Days == 8] <- 0
eu2017$Vig_Days[eu2017$Vig_Days == 9] <- NA
eu2017$Vig_Time[which(is.na(eu2017$Vig_Time))] <- "Never do any vigorous physical activity "
eu2017$Vig_Time[eu2017$Vig_Time == "DK"] <- NA

eu2017$Mod_Days[which(is.na(eu2017$Mod_Days))] <- 0
eu2017$Mod_Days[eu2017$Mod_Days == 8] <- 0
eu2017$Mod_Days[eu2017$Mod_Days == 9] <- NA
eu2017$Mod_Time[which(is.na(eu2017$Mod_Time))] <- "Never do any moderate physical activity "
eu2017$Mod_Time[eu2017$Mod_Time == "DK"] <- NA

eu2017$Walk_Days[eu2017$Walk_Days == 8] <- 0
eu2017$Walk_Days[eu2017$Walk_Days == 9] <- NA
eu2017$Walk_Time[eu2017$Walk_Time == "DK"] <- NA

# INTERVAL MEDIAN VALUES BY PA TIME      
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "Never do any vigorous physical activity " | eu2017$Vig_Time  == "Never do vigorous physical activities"] <- 0 
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "30 minutes or less" ] <- 15   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "31 to 60 minutes" ] <- 45   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "61 to 90 minutes" ] <- 75   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "91 to 120 minutes" ] <- 105   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "More than 120 minutes" ] <- 135   

eu2017$Mod_Time_med [ eu2017$Mod_Time  == "Never do any moderate physical activity " | eu2017$Mod_Time  == "Never do moderate physical activities"] <- 0 
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "30 minutes or less" ] <- 15   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "31 to 60 minutes" ] <- 45   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "61 to 90 minutes" ] <- 75   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "91 to 120 minutes" ] <- 105   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "More than 120 minutes" ] <- 135  

eu2017$Walk_Time_med [eu2017$Walk_Time == "Never walk for 10 minutes at a time"] <- 0
eu2017$Walk_Time_med [eu2017$Walk_Time == "30 minutes or less"] <- 15
eu2017$Walk_Time_med [eu2017$Walk_Time == "31 to 60 minutes"] <- 45
eu2017$Walk_Time_med [eu2017$Walk_Time == "61 to 90 minutes"] <- 75
eu2017$Walk_Time_med [eu2017$Walk_Time == "91 to 120 minutes"] <- 105
eu2017$Walk_Time_med [eu2017$Walk_Time == "More than 120 minutes"] <- 135


# TIME PER WEEK
eu2017$VPA_tot_time <- eu2017$Vig_Days * eu2017$Vig_Time_med
eu2017$MPA_tot_time <- eu2017$Mod_Days * eu2017$Mod_Time_med
eu2017$Walk_tot_time <- eu2017$Walk_Days * eu2017$Walk_Time_med

eu2017$Mod_plus_walk_tottime <- eu2017$MPA_tot_time + eu2017$Walk_tot_time # MOD + WALK WEEKLY TIME TO WHO COMPLIANCE 

eu2017$MVPA_tot_time_outwalk  <- eu2017$VPA_tot_time + eu2017$MPA_tot_time
eu2017$MVPA_tot_time_pluswalk <- eu2017$VPA_tot_time + eu2017$MPA_tot_time + eu2017$Walk_tot_time

# METS PER WEEK
eu2017$VPA_met <- eu2017$VPA_tot_time * 8
eu2017$MPA_met <- eu2017$MPA_tot_time * 4
eu2017$Walk_met <- eu2017$Walk_tot_time * 3.3 

# COMPUTE MVPA METs 

eu2017$MVPA_met <- eu2017$VPA_met + eu2017$MPA_met + eu2017$Walk_met
eu2017$MVPA_met_outwalk <- eu2017$VPA_met + eu2017$MPA_met

# WHO PREVALENCE - 150' MPA, 75' VPA or a equivalent combination (VPA = 2* MPA) and quartiles

eu2017$WHO_prev [eu2017$VPA_tot_time >= 75 | eu2017$Mod_plus_walk_tottime >= 150 | (eu2017$Mod_plus_walk_tottime + (2 * eu2017$VPA_tot_time) >= 150)] <- "Active"
eu2017$WHO_prev [which(is.na(eu2017$WHO_prev))] <-"Inactive"
eu2017$WHO_prev [which(is.na(eu2017$Mod_plus_walk_tottime) & (is.na(eu2017$VPA_tot_time)))] <- NA


#### Covariates Reordering, Assigning missing values and dropping useless levels ####
eu2017$Bills[eu2017$Bills == "Refusal (SPONT.)"] <- NA; eu2017$Bills<- fct_drop(eu2017$Bills, only = "Refusal (SPONT.)")

eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Towns and suburbs/ small urban area"] <- "Urban"; eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Cities/ large urban area"] <- "Urban"; 
eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Rural area"] <- "Rural"

eu2017$Age_3clusters [ eu2017$Age >= 18 & eu2017$Age < 45 ] <- "18-44"  
eu2017$Age_3clusters [ eu2017$Age >= 45 & eu2017$Age < 70 ] <- "45-69"  
eu2017$Age_3clusters [ eu2017$Age >= 70] <- "70+"  

eu2017$Age_3clusters2 [ eu2017$Age >= 18 & eu2017$Age < 35 ] <- "18-34"  
eu2017$Age_3clusters2 [ eu2017$Age >= 35 & eu2017$Age < 65 ] <- "35-64"  
eu2017$Age_3clusters2 [ eu2017$Age >= 65] <- "65+"  

eu2017$Age_3clusters3 [ eu2017$Age < 35 ] <- "15-34"  
eu2017$Age_3clusters3 [ eu2017$Age >= 35 & eu2017$Age < 65 ] <- "35-64"  
eu2017$Age_3clusters3 [ eu2017$Age >= 65] <- "65+"

eu2017$Age_6clusters [ eu2017$Age >= 15 & eu2017$Age < 25 ] <- "15-24"  
eu2017$Age_6clusters [ eu2017$Age >= 25 & eu2017$Age < 35 ] <- "25-34"  
eu2017$Age_6clusters [ eu2017$Age >= 35 & eu2017$Age < 45 ] <- "35-44"  
eu2017$Age_6clusters [ eu2017$Age >= 45 & eu2017$Age < 55 ] <- "45-54"  
eu2017$Age_6clusters [ eu2017$Age >= 55 & eu2017$Age < 65 ] <- "55-64"  
eu2017$Age_6clusters [ eu2017$Age >= 65 ] <- "65+"  

eu2017$Country_rec <- eu2017$Country; eu2017$Country_rec <- fct_expand(eu2017$Country_rec, c("DE Germany", "UK United Kingdom")) 
eu2017$Country_rec[eu2017$Country_rec == "DE-W - Germany - West" | eu2017$Country_rec =="DE-E Germany East" ] <- "DE Germany"
eu2017$Country_rec[eu2017$Country_rec ==  "GB-NIR Northern Ireland" | eu2017$Country_rec =="GB-GBN - Great Britain"] <- "UK United Kingdom"
eu2017$Country_rec<- fct_drop(eu2017$Country_rec, only = c("DE-W - Germany - West", "DE-E Germany East", "GB-NIR Northern Ireland", "GB-GBN - Great Britain", 
       "LI - Liechtenstein (NOT INCLUDED)", "IS - Iceland (NOT INCLUDED)", "CH - Switzerland (NOT INCLUDED)", "NO - Norway (NOT INCLUDED)",
       "RS - Serbia (NOT INCLUDED)", "ME - Montenegro (NOT INCLUDED)", "MK - Makedonia/FYROM (NOT INCLUDED)", "CY-TCC - Cyprus TCC (NOT INCLUDED)",
       "TR - Turkey (NOT INCLUDED)", "-"))



eu2017$`Marital status`[eu2017$`Marital status` == "Other (SPONT.)"] <- NA
eu2017$`Marital status`[eu2017$`Marital status` == "Refusal (SPONT.)"] <- NA
eu2017$`Marital status`<- fct_drop(eu2017$`Marital status`, only = c("Refusal (SPONT.)", "Other (SPONT.)"))


eu2017$Education[eu2017$Education == "DK"] <- NA
eu2017$Education[eu2017$Education == "Refusal_duplicated_7"] <- NA
eu2017$Education[eu2017$Education == "Refusal"] <- NA
eu2017$Education[eu2017$Education == "No full-time education"] <- NA
eu2017$Education <- fct_drop(eu2017$Education, only = c("DK", "Refusal_duplicated_7", "Refusal", "No full-time education"))

eu2017$Education_3cat[eu2017$Education == "Still Studying" & eu2017$Age <= 15] <- "Up to 15 years"
eu2017$Education_3cat[eu2017$Education == "Still Studying" & eu2017$Age > 15 & eu2017$Age < 20] <- "16-19"
eu2017$Education_3cat[eu2017$Education == "Still Studying" & eu2017$Age >= 20] <- "20 years and older"
eu2017$Education_3cat[eu2017$Education == "Up to 15 years"] <- "Up to 15 years"
eu2017$Education_3cat[eu2017$Education == "16-19"] <- "16-19"
eu2017$Education_3cat[eu2017$Education == "20 years and older"] <- "20 years and older"

eu2017$Education_3cat_recod[eu2017$Education_3cat == "Up to 15 years"] <- "Primary"
eu2017$Education_3cat_recod[eu2017$Education_3cat == "16-19"] <- "Secondary"
eu2017$Education_3cat_recod[eu2017$Education_3cat == "20 years and older"] <- "University"


eu2017$Ocupation_3cat[eu2017$Ocupation == "Supervisor" | eu2017$Ocupation == "Skilled manual worker" | eu2017$Ocupation == "Unskilled manual worker, etc." | eu2017$Ocupation == "Employed position, service job"] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Farmer" | eu2017$Ocupation == "Fisherman" |  eu2017$Ocupation == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation == "Business proprietors, etc." | eu2017$Ocupation == "Employed position, at desk" | eu2017$Ocupation == "Employed position, travelling" | eu2017$Ocupation == "Professional (lawyer, etc.)"] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Employed professional (employed doctor, etc.)"  |  eu2017$Ocupation == "General management, etc."  | eu2017$Ocupation == "Middle management, etc." ] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" | eu2017$Ocupation == "Responsible for ordinary shopping, etc."] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work"] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student"] <- NA

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA


#### Filters ####
# REMOVE ILLOGICAL VALUES AND RECOVERING SOME NAs LOGICAL VALUES

eu2017 <- eu2017[-c(which(eu2017$Vig_Days == 0 & eu2017$Vig_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Mod_Days == 0 & eu2017$Mod_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Walk_Days == 0 & eu2017$Walk_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Vig_Days > 0 & eu2017$Vig_Time_med == 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Mod_Days > 0 & eu2017$Mod_Time_med == 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Walk_Days > 0 & eu2017$Walk_Time_med == 0)), ] 


# REMOVING NA METS AND SIT TIME VALUES TO GET FINAL DATABASE SAMPLE SIZE
eu2017 <- eu2017[which(eu2017$MVPA_met >= 0), ]




#### Descriptive ####
library(survey)
desc.w <- svydesign(ids = ~1, data = eu2017, weights = eu2017$w23)

svytable(~Gender, design = desc.w);prop.table(svytable(~Gender, design = desc.w))
svymean(~Age, design = desc.w);sqrt(svyvar(~Age, design = desc.w))
svytable(~Age_3clusters3, design = desc.w);prop.table(svytable(~Age_3clusters3, design = desc.w))
svytable(~`Marital status`, design = desc.w);prop.table(svytable(~`Marital status`, design = desc.w))
svytable(~Sizeofcommunity, design = desc.w);prop.table(svytable(~Sizeofcommunity, design = desc.w))

svytable(~Education_3cat_recod, design = desc.w);prop.table(svytable(~Education_3cat_recod, design = desc.w))
svytable(~Ocupation_3cat, design = desc.w);prop.table(svytable(~Ocupation_3cat, design = desc.w))
svytable(~Bills, design = desc.w);prop.table(svytable(~Bills, design = desc.w))

svytable(~WHO_prev, design = desc.w);prop.table(svytable(~WHO_prev, design = desc.w))



## Physical activity descrip and sit across gender with covariates
library(descr)
library(Hmisc)

eu2017 %>%  group_by(Education_3cat_recod) %>% 
    summarise(n = n(),
              mean_age = wtd.mean(Age, weights = w23, na.rm = T),
              sd_age = sqrt(wtd.var(Age, weights = w23, na.rm = T)))
summary(aov(Age ~ Education_3cat_recod, data = eu2017))

crosstab(eu2017$Education_3cat_recod, eu2017$Gender,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Education_3cat_recod, eu2017$Age_3clusters3,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Education_3cat_recod, eu2017$`Marital status`,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Education_3cat_recod, eu2017$Sizeofcommunity,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Education_3cat_recod, eu2017$Ocupation_3cat,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Education_3cat_recod, eu2017$Bills,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Education_3cat_recod, eu2017$WHO_prev,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)


eu2017 %>%  group_by(Ocupation_3cat) %>% 
    summarise(n = n(),
              mean_age = wtd.mean(Age, weights = w23, na.rm = T),
              sd_age = sqrt(wtd.var(Age, weights = w23, na.rm = T)))
summary(aov(Age ~ Ocupation_3cat, data = eu2017))

crosstab(eu2017$Ocupation_3cat, eu2017$Gender,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Ocupation_3cat, eu2017$Age_3clusters3,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Ocupation_3cat, eu2017$`Marital status`,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Ocupation_3cat, eu2017$Sizeofcommunity,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Ocupation_3cat, eu2017$Education_3cat_recod,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Ocupation_3cat, eu2017$Bills,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Ocupation_3cat, eu2017$WHO_prev,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)


eu2017 %>%  group_by(Bills) %>% 
    summarise(n = n(),
              mean_age = wtd.mean(Age, weights = w23, na.rm = T),
              sd_age = sqrt(wtd.var(Age, weights = w23, na.rm = T)))
summary(aov(Age ~ Bills, data = eu2017))

crosstab(eu2017$Bills, eu2017$Gender,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Bills, eu2017$Age_3clusters3,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Bills, eu2017$`Marital status`,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Bills, eu2017$Sizeofcommunity,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Bills, eu2017$Education_3cat_recod,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Bills, eu2017$Ocupation_3cat,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$Bills, eu2017$WHO_prev,weight = eu2017$w23, prop.c = T,digits = 2, chisq = T, plot = F)




#### Binomial logistic regressions ####
library("moments")
library("sjPlot")
library("lme4")
set.seed(123456)

eu2017$Gender <- relevel(eu2017$Gender, ref = "Woman")
eu2017$Sizeofcommunity <- relevel(eu2017$Sizeofcommunity, ref = "Rural area")
eu2017$`Marital status` <- relevel(eu2017$`Marital status`, ref = "Single hh without children (9,11,13 in d7)")
eu2017$Education_3cat_recod <- relevel(eu2017$Education_3cat_recod, ref = "Primary")
eu2017$Ocupation_3cat <- relevel(eu2017$Ocupation_3cat, ref = "V-VII")
eu2017$Bills <- relevel(eu2017$Bills, ref = "Most of the time")
eu2017$WHO_prev <- relevel(eu2017$WHO_prev, ref = "Active")

eu2017$Age_centred <- scale(eu2017$Age, center = T, scale = F)

# Covariates crude models
model_age_unadj <- glmer(WHO_prev ~ Age_centred + (1 | Country_rec), data = eu2017, family = binomial, 
                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_gender_unadj <- glmer(WHO_prev ~ Gender + (1 | Country_rec), data = eu2017, family = binomial, 
                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_place_unadj <- glmer(WHO_prev ~ Sizeofcommunity + (1 | Country_rec), data = eu2017, family = binomial, 
                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_marital_unadj <- glmer(WHO_prev ~ `Marital status` + (1 | Country_rec), data = eu2017, family = binomial, 
                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_age_unadj, model_gender_unadj, model_place_unadj, model_marital_unadj, digits.re = 3)


# Education - model 1
model_educ_unadj <- glmer(WHO_prev ~ Education_3cat_recod + (1 | Country_rec), data = eu2017, family = binomial, 
      control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_adj <- glmer(WHO_prev ~ Education_3cat_recod + Age_centred + Gender + Sizeofcommunity + `Marital status` + (1 | Country_rec), 
      data = eu2017, family = binomial, 
      control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_educ_unadj, model_educ_adj, digits.re = 3)

model_educ_age_adj <- glmer(WHO_prev ~ Education_3cat_recod + Age_centred + (1 | Country_rec), data = eu2017, family = binomial, 
                         control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_gender_adj <- glmer(WHO_prev ~ Education_3cat_recod +  Gender + (1 | Country_rec), data = eu2017, family = binomial, 
                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_place_adj <- glmer(WHO_prev ~ Education_3cat_recod + Sizeofcommunity + (1 | Country_rec), data = eu2017, family = binomial, 
                           control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_marital_adj <- glmer(WHO_prev ~ Education_3cat_recod + `Marital status` + (1 | Country_rec), data = eu2017, family = binomial, 
                             control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_educ_age_adj, model_educ_gender_adj, model_educ_place_adj, model_educ_marital_adj, digits.re = 3)


# Occupation - model 1
model_ocup_unadj <- glmer(WHO_prev ~ Ocupation_3cat + (1 | Country_rec), data = eu2017, family = binomial, 
      control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_adj <- glmer(WHO_prev ~ Ocupation_3cat + Age_centred + Gender + Sizeofcommunity + `Marital status` + (1 | Country_rec), 
      data = eu2017, family = binomial, 
      control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_ocup_unadj, model_ocup_adj, digits.re = 3)


model_ocup_age_adj <- glmer(WHO_prev ~ Ocupation_3cat + Age_centred + (1 | Country_rec), data = eu2017, family = binomial, 
                              control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_gender_adj <- glmer(WHO_prev ~ Ocupation_3cat +  Gender + (1 | Country_rec), data = eu2017, family = binomial, 
                                 control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_place_adj <- glmer(WHO_prev ~ Ocupation_3cat + Sizeofcommunity + (1 | Country_rec), data = eu2017, family = binomial, 
                                control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_marital_adj <- glmer(WHO_prev ~ Ocupation_3cat + `Marital status` + (1 | Country_rec), data = eu2017, family = binomial, 
                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_ocup_age_adj, model_ocup_gender_adj, model_ocup_place_adj, model_ocup_marital_adj, digits.re = 3)


# Economic issues - model 1
model_bills_unadj <- glmer(WHO_prev ~ Bills + (1 | Country_rec), data = eu2017, family = binomial, 
      control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_adj <- glmer(WHO_prev ~ Bills + Age_centred + Gender + Sizeofcommunity + `Marital status` + (1 | Country_rec), 
      data = eu2017, family = binomial, 
      control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_bills_unadj, model_bills_adj, digits.re = 3)

model_bills_age_adj <- glmer(WHO_prev ~ Bills + Age_centred + (1 | Country_rec), data = eu2017, family = binomial, 
                              control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_gender_adj <- glmer(WHO_prev ~ Bills +  Gender + (1 | Country_rec), data = eu2017, family = binomial, 
                                 control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_place_adj <- glmer(WHO_prev ~ Bills + Sizeofcommunity + (1 | Country_rec), data = eu2017, family = binomial, 
                                control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_marital_adj <- glmer(WHO_prev ~ Bills + `Marital status` + (1 | Country_rec), data = eu2017, family = binomial, 
                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_bills_age_adj, model_bills_gender_adj, model_bills_place_adj, model_bills_marital_adj, digits.re = 3)



eu2017$Education_3cat_recod_2 <- eu2017$Education_3cat_recod
eu2017$Ocupation_3cat_2 <- eu2017$Ocupation_3cat
eu2017$Bills_2 <- eu2017$Bills

eu2017$Education_3cat_recod_2 <- relevel(eu2017$Education_3cat_recod_2, ref = "Secondary")
eu2017$Ocupation_3cat_2 <- relevel(eu2017$Ocupation_3cat_2, ref = "III-IV")
eu2017$Bills_2 <- relevel(eu2017$Bills_2, ref = "From time to time")


# Education - model 2
model_educ_unadj_2 <- glmer(WHO_prev ~ Education_3cat_recod_2 + (1 | Country_rec), 
                          data = eu2017, family = binomial, 
                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_adj_2 <- glmer(WHO_prev ~ Education_3cat_recod_2 + Age_centred + Gender + Sizeofcommunity + `Marital status` + (1 | Country_rec), 
      data = eu2017, family = binomial, 
      control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)

model_educ_age_adj_2 <- glmer(WHO_prev ~ Education_3cat_recod_2 + Age_centred + (1 | Country_rec), data = eu2017, family = binomial, 
                              control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_gender_adj_2 <- glmer(WHO_prev ~ Education_3cat_recod_2 +  Gender + (1 | Country_rec), data = eu2017, family = binomial, 
                                 control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_place_adj_2 <- glmer(WHO_prev ~ Education_3cat_recod_2 + Sizeofcommunity + (1 | Country_rec), data = eu2017, family = binomial, 
                                control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_marital_adj_2 <- glmer(WHO_prev ~ Education_3cat_recod_2 + `Marital status` + (1 | Country_rec), data = eu2017, family = binomial, 
                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_educ_age_adj_2, model_educ_gender_adj_2, model_educ_place_adj_2, model_educ_marital_adj_2, digits.re = 3)


# Occupation - model 2
model_ocup_unadj_2 <- glmer(WHO_prev ~ Ocupation_3cat_2  + (1 | Country_rec), 
                          data = eu2017, family = binomial, 
                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_adj_2 <- glmer(WHO_prev ~ Ocupation_3cat_2 + Age_centred + Gender + Sizeofcommunity + `Marital status` + (1 | Country_rec), 
      data = eu2017, family = binomial, 
      control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)

model_ocup_age_adj_2 <- glmer(WHO_prev ~ Ocupation_3cat_2 + Age_centred + (1 | Country_rec), data = eu2017, family = binomial, 
                                control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_gender_adj_2 <- glmer(WHO_prev ~ Ocupation_3cat_2 +  Gender + (1 | Country_rec), data = eu2017, family = binomial, 
                                   control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_place_adj_2 <- glmer(WHO_prev ~ Ocupation_3cat_2 + Sizeofcommunity + (1 | Country_rec), data = eu2017, family = binomial, 
                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_marital_adj_2 <- glmer(WHO_prev ~ Ocupation_3cat_2 + `Marital status` + (1 | Country_rec), data = eu2017, family = binomial, 
                                    control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_ocup_age_adj_2, model_ocup_gender_adj_2, model_ocup_place_adj_2, model_ocup_marital_adj_2, digits.re = 3)


# Economic issues - model 2
model_bills_unadj_2 <- glmer(WHO_prev ~ Bills_2 + (1 | Country_rec), 
                           data = eu2017, family = binomial, 
                           control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_adj_2 <- glmer(WHO_prev ~ Bills_2 + Age_centred + Gender + Sizeofcommunity + `Marital status` + (1 | Country_rec), 
      data = eu2017, family = binomial, 
      control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)

model_bills_age_adj_2 <- glmer(WHO_prev ~ Bills_2 + Age_centred + (1 | Country_rec), data = eu2017, family = binomial, 
                                control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_gender_adj_2 <- glmer(WHO_prev ~ Bills_2 +  Gender + (1 | Country_rec), data = eu2017, family = binomial, 
                                   control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_place_adj_2 <- glmer(WHO_prev ~ Bills_2 + Sizeofcommunity + (1 | Country_rec), data = eu2017, family = binomial, 
                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_marital_adj_2 <- glmer(WHO_prev ~ Bills_2 + `Marital status` + (1 | Country_rec), data = eu2017, family = binomial, 
                                    control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_bills_age_adj_2, model_bills_gender_adj_2, model_bills_place_adj_2, model_bills_marital_adj_2, digits.re = 3)

tab_model(model_educ_unadj_2, model_educ_adj_2, 
          model_ocup_unadj_2, model_ocup_adj_2, 
          model_bills_unadj_2, model_bills_adj_2, digits.re = 3)



## Supplementary regression models - interaction terms and segmented analysis ##
# Education - model 1
model_educ_adj_int <- glmer(WHO_prev ~ Education_3cat_recod + Age_centred + Gender + Sizeofcommunity + `Marital status` + 
                          Education_3cat_recod*Gender + Education_3cat_recod*Sizeofcommunity + Education_3cat_recod*`Marital status` +
                          (1 | Country_rec), 
                        data = eu2017, family = binomial, 
                        control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_educ_adj_int, digits.re = 3)

model_educ_adj_int_rural <- glmer(WHO_prev ~ Education_3cat_recod + Age_centred + Gender + `Marital status` + 
                              Education_3cat_recod*Gender + Education_3cat_recod*`Marital status` +
                              (1 | Country_rec), 
                            data = subset(eu2017, Sizeofcommunity =="Rural area"), family = binomial, 
                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_adj_int_suburban <- glmer(WHO_prev ~ Education_3cat_recod + Age_centred + Gender + `Marital status` + 
                              Education_3cat_recod*Gender + Education_3cat_recod*`Marital status` +
                              (1 | Country_rec), 
                            data = subset(eu2017, Sizeofcommunity =="Towns and suburbs/ small urban area"), family = binomial, 
                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_adj_int_city <- glmer(WHO_prev ~ Education_3cat_recod + Age_centred + Gender + `Marital status` + 
                                  Education_3cat_recod*Gender + Education_3cat_recod*`Marital status` +
                                  (1 | Country_rec), 
                                data = subset(eu2017, Sizeofcommunity =="Cities/ large urban area"), family = binomial, 
                                control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_educ_adj_int_rural, model_educ_adj_int_suburban, model_educ_adj_int_city, digits.re = 3)

model_educ_adj_int_single_no_children <- glmer(WHO_prev ~ Education_3cat_recod + Age_centred + Gender + Sizeofcommunity + 
                                    Education_3cat_recod*Gender + Education_3cat_recod*Sizeofcommunity +
                                    (1 | Country_rec), 
                                  data = subset(eu2017, `Marital status` =="Single hh without children (9,11,13 in d7)"), family = binomial, 
                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_adj_int_multiple_no_children <- glmer(WHO_prev ~ Education_3cat_recod + Age_centred + Gender + Sizeofcommunity + 
                                       Education_3cat_recod*Gender + Education_3cat_recod*Sizeofcommunity +
                                       (1 | Country_rec), 
                                     data = subset(eu2017, `Marital status` =="Multiple hh without children (1, 5 in d7)"), family = binomial, 
                                     control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_adj_int_multiple_children <- glmer(WHO_prev ~ Education_3cat_recod + Age_centred + Gender + Sizeofcommunity + 
                                   Education_3cat_recod*Gender + Education_3cat_recod*Sizeofcommunity +
                                   (1 | Country_rec), 
                                 data = subset(eu2017, `Marital status` =="Multiple hh with children (2-4, 6-8 in d7)"), family = binomial, 
                                 control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_educ_adj_int_single_no_children, model_educ_adj_int_multiple_no_children, model_educ_adj_int_multiple_children, digits.re = 3)


# Occupation - model 1
model_ocup_adj_int <- glmer(WHO_prev ~ Ocupation_3cat + Age_centred + Gender + Sizeofcommunity + `Marital status` + 
                          Ocupation_3cat*Gender + Ocupation_3cat*Sizeofcommunity + Ocupation_3cat*`Marital status` +
                          (1 | Country_rec),
                        data = eu2017, family = binomial, 
                        control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_ocup_adj_int, digits.re = 3)

model_ocup_adj_int_rural <- glmer(WHO_prev ~ Ocupation_3cat + Age_centred + Gender + `Marital status` + 
                                    Ocupation_3cat*Gender + Ocupation_3cat*`Marital status` +
                                    (1 | Country_rec), 
                                  data = subset(eu2017, Sizeofcommunity =="Rural area"), family = binomial, 
                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_adj_int_suburban <- glmer(WHO_prev ~ Ocupation_3cat + Age_centred + Gender + `Marital status` + 
                                       Ocupation_3cat*Gender + Ocupation_3cat*`Marital status` +
                                       (1 | Country_rec), 
                                     data = subset(eu2017, Sizeofcommunity =="Towns and suburbs/ small urban area"), family = binomial, 
                                     control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_adj_int_city <- glmer(WHO_prev ~ Ocupation_3cat + Age_centred + Gender + `Marital status` + 
                                   Ocupation_3cat*Gender + Ocupation_3cat*`Marital status` +
                                   (1 | Country_rec), 
                                 data = subset(eu2017, Sizeofcommunity =="Cities/ large urban area"), family = binomial, 
                                 control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_ocup_adj_int_rural, model_ocup_adj_int_suburban, model_ocup_adj_int_city, digits.re = 3)


# Economic issues - model 1
model_bills_adj_int <- glmer(WHO_prev ~ Bills + Age_centred + Sizeofcommunity + `Marital status` + 
                           Bills*Gender + Bills*Sizeofcommunity + Bills*`Marital status` +
                           (1 | Country_rec),
                         data = eu2017, family = binomial, 
                         control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_bills_adj_int, digits.re = 3)

model_bills_adj_int_single_no_children <- glmer(WHO_prev ~ Bills + Age_centred + Gender + Sizeofcommunity + 
                                                 Bills*Gender + Bills*Sizeofcommunity +
                                                 (1 | Country_rec), 
                                               data = subset(eu2017, `Marital status` =="Single hh without children (9,11,13 in d7)"), family = binomial, 
                                               control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_adj_int_multiple_no_children <- glmer(WHO_prev ~ Bills + Age_centred + Gender + Sizeofcommunity + 
                                                   Bills*Gender + Bills*Sizeofcommunity +
                                                   (1 | Country_rec), 
                                                 data = subset(eu2017, `Marital status` =="Multiple hh without children (1, 5 in d7)"), family = binomial, 
                                                 control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_adj_int_multiple_children <- glmer(WHO_prev ~ Bills + Age_centred + Gender + Sizeofcommunity + 
                                                Bills*Gender + Bills*Sizeofcommunity +
                                                (1 | Country_rec), 
                                              data = subset(eu2017, `Marital status` =="Multiple hh with children (2-4, 6-8 in d7)"), family = binomial, 
                                              control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_bills_adj_int_single_no_children, model_bills_adj_int_multiple_no_children, model_bills_adj_int_multiple_children, digits.re = 3)


# Education - model 2
model_educ_adj_2_int <- glmer(WHO_prev ~ Education_3cat_recod_2 + Age_centred + Gender + Sizeofcommunity + `Marital status` + 
                            Education_3cat_recod_2*Gender + Education_3cat_recod_2*Sizeofcommunity + Education_3cat_recod_2*`Marital status` +
                            (1 | Country_rec),
                          data = eu2017, family = binomial, 
                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_educ_adj_2_int, digits.re = 3)

model_educ_adj_2_int_single_no_children <- glmer(WHO_prev ~ Education_3cat_recod_2 + Age_centred + Gender + Sizeofcommunity + 
                                                  Education_3cat_recod_2*Gender + Education_3cat_recod_2*Sizeofcommunity +
                                                  (1 | Country_rec), 
                                                data = subset(eu2017, `Marital status` =="Single hh without children (9,11,13 in d7)"), family = binomial, 
                                                control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_adj_2_int_multiple_no_children <- glmer(WHO_prev ~ Education_3cat_recod_2 + Age_centred + Gender + Sizeofcommunity + 
                                                    Education_3cat_recod_2*Gender + Education_3cat_recod_2*Sizeofcommunity +
                                                    (1 | Country_rec), 
                                                  data = subset(eu2017, `Marital status` =="Multiple hh without children (1, 5 in d7)"), family = binomial, 
                                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_adj_2_int_multiple_children <- glmer(WHO_prev ~ Education_3cat_recod_2 + Age_centred + Gender + Sizeofcommunity + 
                                                 Education_3cat_recod_2*Gender + Education_3cat_recod_2*Sizeofcommunity +
                                                 (1 | Country_rec), 
                                               data = subset(eu2017, `Marital status` =="Multiple hh with children (2-4, 6-8 in d7)"), family = binomial, 
                                               control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_educ_adj_2_int_single_no_children, model_educ_adj_2_int_multiple_no_children, model_educ_adj_2_int_multiple_children, digits.re = 3)

model_educ_adj_2_int_rural <- glmer(WHO_prev ~ Education_3cat_recod_2 + Age_centred + Gender + `Marital status` + 
                                    Education_3cat_recod_2*Gender + Education_3cat_recod_2*`Marital status` +
                                    (1 | Country_rec), 
                                  data = subset(eu2017, Sizeofcommunity =="Rural area"), family = binomial, 
                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_educ_adj_2_int_suburban <- glmer(WHO_prev ~ Education_3cat_recod_2 + Age_centred + Gender + `Marital status` + 
                                       Education_3cat_recod_2*Gender + Education_3cat_recod_2*`Marital status` +
                                       (1 | Country_rec), 
                                     data = subset(eu2017, Sizeofcommunity =="Towns and suburbs/ small urban area"), family = binomial, 
                                     control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_educ_adj_2_int_rural, model_educ_adj_2_int_suburban, digits.re = 3)


# Occupation - model 2
model_ocup_adj_2_int <- glmer(WHO_prev ~ Ocupation_3cat_2 + Age_centred + Gender + Sizeofcommunity + `Marital status` + 
                            Ocupation_3cat_2*Gender + Ocupation_3cat_2*Sizeofcommunity + Ocupation_3cat_2*`Marital status` +
                            (1 | Country_rec), 
                          data = eu2017, family = binomial, 
                          control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_ocup_adj_2_int, digits.re = 3)

model_ocup_adj_2_int_rural <- glmer(WHO_prev ~ Ocupation_3cat_2 + Age_centred + Gender + `Marital status` + 
                                    Ocupation_3cat_2*Gender + Ocupation_3cat_2*`Marital status` +
                                    (1 | Country_rec), 
                                  data = subset(eu2017, Sizeofcommunity =="Rural area"), family = binomial, 
                                  control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_adj_2_int_suburban <- glmer(WHO_prev ~ Ocupation_3cat_2 + Age_centred + Gender + `Marital status` + 
                                       Ocupation_3cat_2*Gender + Ocupation_3cat_2*`Marital status` +
                                       (1 | Country_rec), 
                                     data = subset(eu2017, Sizeofcommunity =="Towns and suburbs/ small urban area"), family = binomial, 
                                     control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_ocup_adj_2_int_city <- glmer(WHO_prev ~ Ocupation_3cat_2 + Age_centred + Gender + `Marital status` + 
                                   Ocupation_3cat_2*Gender + Ocupation_3cat_2*`Marital status` +
                                   (1 | Country_rec), 
                                 data = subset(eu2017, Sizeofcommunity =="Cities/ large urban area"), family = binomial, 
                                 control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_ocup_adj_2_int_rural, model_ocup_adj_2_int_suburban, model_ocup_adj_2_int_city, digits.re = 3)


# Economic issues - model 2
model_bills_adj_2_int <- glmer(WHO_prev ~ Bills_2 + Age_centred + Gender + Sizeofcommunity + `Marital status` + 
                             Bills_2*Gender + Bills_2*Sizeofcommunity + Bills_2*`Marital status` +
                             (1 | Country_rec),
                           data = eu2017, family = binomial, 
                           control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_bills_adj_2_int, digits.re = 3)

model_bills_adj_2_int_single_no_children <- glmer(WHO_prev ~ Bills_2 + Age_centred + Gender + Sizeofcommunity + 
                                                   Bills_2*Gender + Bills_2*Sizeofcommunity +
                                                   (1 | Country_rec), 
                                                 data = subset(eu2017, `Marital status` =="Single hh without children (9,11,13 in d7)"), family = binomial, 
                                                 control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_adj_2_int_multiple_no_children <- glmer(WHO_prev ~ Bills_2 + Age_centred + Gender + Sizeofcommunity + 
                                                     Bills_2*Gender + Bills_2*Sizeofcommunity +
                                                     (1 | Country_rec), 
                                                   data = subset(eu2017, `Marital status` =="Multiple hh without children (1, 5 in d7)"), family = binomial, 
                                                   control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
model_bills_adj_2_int_multiple_children <- glmer(WHO_prev ~ Bills_2 + Age_centred + Gender + Sizeofcommunity + 
                                                  Bills_2*Gender + Bills_2*Sizeofcommunity +
                                                  (1 | Country_rec), 
                                                data = subset(eu2017, `Marital status` =="Multiple hh with children (2-4, 6-8 in d7)"), family = binomial, 
                                                control = glmerControl(optimizer = "bobyqa"), nAGQ = 10, na.action = na.exclude, weights = w23)
tab_model(model_bills_adj_2_int_single_no_children, model_bills_adj_2_int_multiple_no_children, model_bills_adj_2_int_multiple_children, digits.re = 3)

tab_model(model_educ_adj_2_int, model_ocup_adj_2_int, model_bills_adj_2_int, digits.re = 3)



# Hypothetical reduction of inequality by Odds Ratios in a 10%, 20% and 50%
OR_SES_data <- read.csv2("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/13. Physical activity increase attruible to better living conditions in EU 2017/MMR/OR_results.csv", dec = ",", header = T, sep = ";")

OR_SES_data$OR_10_perc_adjusted <- ((OR_SES_data$OR_adjusted - 1) * 0.9) + 1
OR_SES_data$OR_20_perc_adjusted <- ((OR_SES_data$OR_adjusted - 1) * 0.8) + 1
OR_SES_data$OR_50_perc_adjusted <- ((OR_SES_data$OR_adjusted - 1) * 0.5) + 1


#### Computing PFP ####
## Indirect estimation of high social class among inactive people
library("plotrix")


function_educ <- function(data, i, OR_mid_low, OR_high_low, OR_high_mid) {
    data[i,] %>% group_by(Country_rec) %>% summarise(Primary = sum(Education_3cat_recod == "Primary", na.rm = T),
                                                   Secondary = sum(Education_3cat_recod == "Secondary", na.rm = T),
                                                   Universitary = sum(Education_3cat_recod == "University", na.rm = T)) -> data_educ
    data_educ$n <- data_educ$Primary + data_educ$Secondary + data_educ$Universitary
    data_educ$Primary_perc <- (data_educ$Primary * 100)/ data_educ$n
    data_educ$Secondary_perc <- (data_educ$Secondary * 100)/ data_educ$n
    data_educ$Universitary_perc <- (data_educ$Universitary * 100)/ data_educ$n
    
    data_educ$High_mid_inequality <- data_educ$Universitary_perc - data_educ$Secondary_perc
    data_educ$High_low_inequality <- data_educ$Universitary_perc - data_educ$Primary_perc
    data_educ$Mid_low_inequality <- data_educ$Secondary_perc - data_educ$Primary_perc
    
    data_educ$Prev_high_mid_educ_in_inactive_people <- (data_educ$Universitary_perc * OR_high_mid)/((data_educ$Universitary_perc * OR_high_mid)+(1 - data_educ$Universitary_perc))
    data_educ$Prev_high_low_educ_in_inactive_people <- (data_educ$Universitary_perc * OR_high_low)/((data_educ$Universitary_perc * OR_high_low)+(1 - data_educ$Universitary_perc))
    data_educ$Prev_mid_educ_in_inactive_people <- (data_educ$Secondary_perc * OR_mid_low)/((data_educ$Secondary_perc * OR_mid_low)+(1 - data_educ$Secondary_perc))
    
    data_educ$PFP_high_mid_educ <- (data_educ$Prev_high_mid_educ_in_inactive_people * (1 - OR_high_mid))/(1 - (1 - OR_high_mid)*(1 - data_educ$Prev_high_mid_educ_in_inactive_people))
    data_educ$PFP_high_low_educ <- (data_educ$Prev_high_low_educ_in_inactive_people * (1 - OR_high_low))/(1 - (1 - OR_high_low)*(1 - data_educ$Prev_high_low_educ_in_inactive_people))
    data_educ$PFP_mid_educ <- (data_educ$Prev_mid_educ_in_inactive_people * (1 - OR_mid_low))/(1 - (1 - OR_mid_low)*(1 - data_educ$Prev_mid_educ_in_inactive_people))
    
    return(c(data_educ$Primary_perc, data_educ$Secondary_perc, data_educ$Universitary_perc, data_educ$Prev_high_mid_educ_in_inactive_people, 
                      data_educ$Prev_high_low_educ_in_inactive_people, data_educ$Prev_mid_educ_in_inactive_people, data_educ$PFP_high_mid_educ, 
                      data_educ$PFP_high_low_educ, data_educ$PFP_mid_educ, data_educ$High_mid_inequality, data_educ$High_low_inequality, 
             data_educ$Mid_low_inequality))
}


Boots_educ_adjusted <- boot(data = eu2017, statistic = function_educ, R = 1000, 
          OR_mid_low = OR_SES_data[1,6], OR_high_low = OR_SES_data[2,6], OR_high_mid = OR_SES_data[3,6])


getCI <- function(x,w) {
    b1 <- boot.ci(x,index=w,type = "perc")
    ## extract info for all CI types
    tab <- t(sapply(b1[-(1:3)],function(x) tail(c(x),2)))
    ## combine with metadata: CI method, index
    tab <- cbind(w,rownames(tab),as.data.frame(tab))
    colnames(tab) <- c("index","method","lwr","upr")
    tab
}


Boots_educ_adjusted$t0 -> Boots_educ_adjusted_estimates
Boots_educ_adjusted$t -> Boots_educ_adjusted_error1
std.error(Boots_educ_adjusted_error1) -> Boots_educ_adjusted_error2
Boots_educ_adjusted2 <- data.frame(Boots_educ_adjusted_estimates,Boots_educ_adjusted_error2)
Boots_educ_adjusted2$Boots_educ_adjusted_error2 <- Boots_educ_adjusted2$Boots_educ_adjusted_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_educ_adjusted)) -> Boots_educ_adjusted_CI
Boots_educ_adjusted2$Boots_educ_adjusted_low <- Boots_educ_adjusted_CI$lwr
Boots_educ_adjusted2$Boots_educ_adjusted_up <- Boots_educ_adjusted_CI$upr

Boots_educ_adjusted2$ID <- seq(from=1,to=336)
Boots_educ_adjusted2$ID2 <- seq(from=1,to=28)

Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 0 & Boots_educ_adjusted2$ID  <= 28] <- "Primary_perc" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 28 & Boots_educ_adjusted2$ID  <= 56] <- "Secondary_perc" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 56 & Boots_educ_adjusted2$ID  <= 84] <- "Universitary_perc" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 84 & Boots_educ_adjusted2$ID  <= 112] <- "Prev_high_mid_educ_in_inactive_people" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 112 & Boots_educ_adjusted2$ID  <= 140] <- "Prev_high_low_educ_in_inactive_people" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 140 & Boots_educ_adjusted2$ID  <= 168] <- "Prev_mid_educ_in_inactive_people" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 168 & Boots_educ_adjusted2$ID  <= 196] <- "PFP_high_mid_educ" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 196 & Boots_educ_adjusted2$ID  <= 224] <- "PFP_high_low_educ" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 224 & Boots_educ_adjusted2$ID  <= 252] <- "PFP_mid_educ" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 252 & Boots_educ_adjusted2$ID  <= 280] <- "High_mid_inequality" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 280 & Boots_educ_adjusted2$ID  <= 308] <- "High_low_inequality" 
Boots_educ_adjusted2$variable [ Boots_educ_adjusted2$ID  > 308] <- "Mid_low_inequality" 
Boots_educ_adjusted2$variable <- as.factor(Boots_educ_adjusted2$variable)

Boots_educ_adjusted2_Primary_perc <- Boots_educ_adjusted2[Boots_educ_adjusted2$ID  > 0 & Boots_educ_adjusted2$ID  <= 28,]
names(Boots_educ_adjusted2_Primary_perc)[1] <- "Primary_perc_educ_boot"
names(Boots_educ_adjusted2_Primary_perc)[2] <- "Primary_perc_educ_boot_se"
names(Boots_educ_adjusted2_Primary_perc)[3] <- "Primary_perc_educ_boot_low"
names(Boots_educ_adjusted2_Primary_perc)[4] <- "Primary_perc_educ_boot_up"

Boots_educ_adjusted2_Secondary_perc <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 28 & Boots_educ_adjusted2$ID  <= 56,] 
names(Boots_educ_adjusted2_Secondary_perc)[1] <- "Secondary_perc_educ_boot"
names(Boots_educ_adjusted2_Secondary_perc)[2] <- "Secondary_perc_educ_boot_se"
names(Boots_educ_adjusted2_Secondary_perc)[3] <- "Secondary_perc_educ_boot_low"
names(Boots_educ_adjusted2_Secondary_perc)[4] <- "Secondary_perc_educ_boot_up"

Boots_educ_adjusted2_Universitary_perc <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 56 & Boots_educ_adjusted2$ID  <= 84,] 
names(Boots_educ_adjusted2_Universitary_perc)[1] <- "Universitary_perc_educ_boot"
names(Boots_educ_adjusted2_Universitary_perc)[2] <- "Universitary_perc_educ_boot_se"
names(Boots_educ_adjusted2_Universitary_perc)[3] <- "Universitary_perc_educ_boot_low"
names(Boots_educ_adjusted2_Universitary_perc)[4] <- "Universitary_perc_educ_boot_up"

Boots_educ_adjusted2_Prev_high_mid_educ_in_inactive_people <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 84 & Boots_educ_adjusted2$ID  <= 112,] 
names(Boots_educ_adjusted2_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_educ_in_inactive_people_educ_boot"
names(Boots_educ_adjusted2_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_se"
names(Boots_educ_adjusted2_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_low"
names(Boots_educ_adjusted2_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_up"

Boots_educ_adjusted2_Prev_high_low_educ_in_inactive_people <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 112 & Boots_educ_adjusted2$ID  <= 140,]
names(Boots_educ_adjusted2_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_educ_in_inactive_people_educ_boot"
names(Boots_educ_adjusted2_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_educ_in_inactive_people_educ_boot_se"
names(Boots_educ_adjusted2_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_educ_in_inactive_people_educ_boot_low"
names(Boots_educ_adjusted2_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_educ_in_inactive_people_educ_boot_up"

Boots_educ_adjusted2_Prev_mid_educ_in_inactive_people <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 140 & Boots_educ_adjusted2$ID  <= 168,]  
names(Boots_educ_adjusted2_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_educ_in_inactive_people_educ_boot"
names(Boots_educ_adjusted2_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_educ_in_inactive_people_educ_boot_se"
names(Boots_educ_adjusted2_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_educ_in_inactive_people_educ_boot_low"
names(Boots_educ_adjusted2_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_educ_in_inactive_people_educ_boot_up"

Boots_educ_adjusted2_PFP_high_mid_educ <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 168 & Boots_educ_adjusted2$ID  <= 196,]
names(Boots_educ_adjusted2_PFP_high_mid_educ)[1] <- "PFP_high_mid_educ_educ_boot"
names(Boots_educ_adjusted2_PFP_high_mid_educ)[2] <- "PFP_high_mid_educ_educ_boot_se"
names(Boots_educ_adjusted2_PFP_high_mid_educ)[3] <- "PFP_high_mid_educ_educ_boot_low"
names(Boots_educ_adjusted2_PFP_high_mid_educ)[4] <- "PFP_high_mid_educ_educ_boot_up"

Boots_educ_adjusted2_PFP_high_low_educ <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 196 & Boots_educ_adjusted2$ID  <= 224,]  
names(Boots_educ_adjusted2_PFP_high_low_educ)[1] <- "PFP_high_low_educ_educ_boot"
names(Boots_educ_adjusted2_PFP_high_low_educ)[2] <- "PFP_high_low_educ_educ_boot_se"
names(Boots_educ_adjusted2_PFP_high_low_educ)[3] <- "PFP_high_low_educ_educ_boot_low"
names(Boots_educ_adjusted2_PFP_high_low_educ)[4] <- "PFP_high_low_educ_educ_boot_up"

Boots_educ_adjusted2_PFP_mid_educ <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 224 & Boots_educ_adjusted2$ID  <= 252,] 
names(Boots_educ_adjusted2_PFP_mid_educ)[1] <- "PFP_mid_educ_educ_boot"
names(Boots_educ_adjusted2_PFP_mid_educ)[2] <- "PFP_mid_educ_educ_boot_se"
names(Boots_educ_adjusted2_PFP_mid_educ)[3] <- "PFP_mid_educ_educ_boot_low"
names(Boots_educ_adjusted2_PFP_mid_educ)[4] <- "PFP_mid_educ_educ_boot_up"

Boots_educ_adjusted2_High_mid_inequality <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 252 & Boots_educ_adjusted2$ID  <= 280,] 
names(Boots_educ_adjusted2_High_mid_inequality)[1] <- "High_mid_inequality_educ_boot"
names(Boots_educ_adjusted2_High_mid_inequality)[2] <- "High_mid_inequality_educ_boot_se"
names(Boots_educ_adjusted2_High_mid_inequality)[3] <- "High_mid_inequality_educ_boot_low"
names(Boots_educ_adjusted2_High_mid_inequality)[4] <- "High_mid_inequality_educ_boot_up"

Boots_educ_adjusted2_High_low_inequality <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 280 & Boots_educ_adjusted2$ID  <= 308,] 
names(Boots_educ_adjusted2_High_low_inequality)[1] <- "High_low_inequality_educ_boot"
names(Boots_educ_adjusted2_High_low_inequality)[2] <- "High_low_inequality_educ_boot_se"
names(Boots_educ_adjusted2_High_low_inequality)[3] <- "High_low_inequality_educ_boot_low"
names(Boots_educ_adjusted2_High_low_inequality)[4] <- "High_low_inequality_educ_boot_up"

Boots_educ_adjusted2_Mid_low_inequality <- Boots_educ_adjusted2 [ Boots_educ_adjusted2$ID  > 308,] 
names(Boots_educ_adjusted2_Mid_low_inequality)[1] <- "Mid_low_inequality_educ_boot"
names(Boots_educ_adjusted2_Mid_low_inequality)[2] <- "Mid_low_inequality_educ_boot_se"
names(Boots_educ_adjusted2_Mid_low_inequality)[3] <- "Mid_low_inequality_educ_boot_low"
names(Boots_educ_adjusted2_Mid_low_inequality)[4] <- "Mid_low_inequality_educ_boot_up"

Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_Primary_perc, Boots_educ_adjusted2_Secondary_perc, by  = "ID2")
Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_final, Boots_educ_adjusted2_Universitary_perc, by  = "ID2")
Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_final, Boots_educ_adjusted2_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_final, Boots_educ_adjusted2_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_final, Boots_educ_adjusted2_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_final, Boots_educ_adjusted2_PFP_high_mid_educ, by  = "ID2")
Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_final, Boots_educ_adjusted2_PFP_high_low_educ, by  = "ID2")
Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_final, Boots_educ_adjusted2_PFP_mid_educ, by  = "ID2")
Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_final, Boots_educ_adjusted2_High_mid_inequality, by  = "ID2")
Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_final, Boots_educ_adjusted2_High_low_inequality, by  = "ID2")
Boots_educ_adjusted2_final <- merge(Boots_educ_adjusted2_final, Boots_educ_adjusted2_Mid_low_inequality, by  = "ID2")
Boots_educ_adjusted2_final <- Boots_educ_adjusted2_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]



Boots_educ_unadjusted <- boot(data = eu2017, statistic = function_educ, R = 1000, 
                            OR_mid_low = OR_SES_data[1,3], OR_high_low = OR_SES_data[2,3], OR_high_mid = OR_SES_data[3,3])

Boots_educ_unadjusted$t0 -> Boots_educ_unadjusted_estimates
Boots_educ_unadjusted$t -> Boots_educ_unadjusted_error1
std.error(Boots_educ_unadjusted_error1) -> Boots_educ_unadjusted_error2
Boots_educ_unadjusted2 <- data.frame(Boots_educ_unadjusted_estimates,Boots_educ_unadjusted_error2)
Boots_educ_unadjusted2$Boots_educ_unadjusted_error2 <- Boots_educ_unadjusted2$Boots_educ_unadjusted_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_educ_unadjusted)) -> Boots_educ_unadjusted_CI
Boots_educ_unadjusted2$Boots_educ_unadjusted_low <- Boots_educ_unadjusted_CI$lwr
Boots_educ_unadjusted2$Boots_educ_unadjusted_up <- Boots_educ_unadjusted_CI$upr

Boots_educ_unadjusted2$ID <- seq(from=1,to=336)
Boots_educ_unadjusted2$ID2 <- seq(from=1,to=28)

Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 0 & Boots_educ_unadjusted2$ID  <= 28] <- "Primary_perc" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 28 & Boots_educ_unadjusted2$ID  <= 56] <- "Secondary_perc" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 56 & Boots_educ_unadjusted2$ID  <= 84] <- "Universitary_perc" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 84 & Boots_educ_unadjusted2$ID  <= 112] <- "Prev_high_mid_educ_in_inactive_people" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 112 & Boots_educ_unadjusted2$ID  <= 140] <- "Prev_high_low_educ_in_inactive_people" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 140 & Boots_educ_unadjusted2$ID  <= 168] <- "Prev_mid_educ_in_inactive_people" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 168 & Boots_educ_unadjusted2$ID  <= 196] <- "PFP_high_mid_educ" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 196 & Boots_educ_unadjusted2$ID  <= 224] <- "PFP_high_low_educ" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 224 & Boots_educ_unadjusted2$ID  <= 252] <- "PFP_mid_educ" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 252 & Boots_educ_unadjusted2$ID  <= 280] <- "High_mid_inequality" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 280 & Boots_educ_unadjusted2$ID  <= 308] <- "High_low_inequality" 
Boots_educ_unadjusted2$variable [ Boots_educ_unadjusted2$ID  > 308] <- "Mid_low_inequality" 
Boots_educ_unadjusted2$variable <- as.factor(Boots_educ_unadjusted2$variable)

Boots_educ_unadjusted2_Primary_perc <- Boots_educ_unadjusted2[Boots_educ_unadjusted2$ID  > 0 & Boots_educ_unadjusted2$ID  <= 28,]
names(Boots_educ_unadjusted2_Primary_perc)[1] <- "Primary_perc_educ_boot_unadj"
names(Boots_educ_unadjusted2_Primary_perc)[2] <- "Primary_perc_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_Primary_perc)[3] <- "Primary_perc_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_Primary_perc)[4] <- "Primary_perc_educ_boot_unadj_up"

Boots_educ_unadjusted2_Secondary_perc <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 28 & Boots_educ_unadjusted2$ID  <= 56,] 
names(Boots_educ_unadjusted2_Secondary_perc)[1] <- "Secondary_perc_educ_boot_unadj"
names(Boots_educ_unadjusted2_Secondary_perc)[2] <- "Secondary_perc_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_Secondary_perc)[3] <- "Secondary_perc_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_Secondary_perc)[4] <- "Secondary_perc_educ_boot_unadj_up"

Boots_educ_unadjusted2_Universitary_perc <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 56 & Boots_educ_unadjusted2$ID  <= 84,] 
names(Boots_educ_unadjusted2_Universitary_perc)[1] <- "Universitary_perc_educ_boot_unadj"
names(Boots_educ_unadjusted2_Universitary_perc)[2] <- "Universitary_perc_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_Universitary_perc)[3] <- "Universitary_perc_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_Universitary_perc)[4] <- "Universitary_perc_educ_boot_unadj_up"

Boots_educ_unadjusted2_Prev_high_mid_educ_in_inactive_people <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 84 & Boots_educ_unadjusted2$ID  <= 112,] 
names(Boots_educ_unadjusted2_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_unadj"
names(Boots_educ_unadjusted2_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_unadj_up"

Boots_educ_unadjusted2_Prev_high_low_educ_in_inactive_people <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 112 & Boots_educ_unadjusted2$ID  <= 140,]
names(Boots_educ_unadjusted2_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_educ_in_inactive_people_educ_boot_unadj"
names(Boots_educ_unadjusted2_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_educ_in_inactive_people_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_educ_in_inactive_people_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_educ_in_inactive_people_educ_boot_unadj_up"

Boots_educ_unadjusted2_Prev_mid_educ_in_inactive_people <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 140 & Boots_educ_unadjusted2$ID  <= 168,]  
names(Boots_educ_unadjusted2_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_educ_in_inactive_people_educ_boot_unadj"
names(Boots_educ_unadjusted2_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_educ_in_inactive_people_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_educ_in_inactive_people_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_educ_in_inactive_people_educ_boot_unadj_up"

Boots_educ_unadjusted2_PFP_high_mid_educ <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 168 & Boots_educ_unadjusted2$ID  <= 196,]
names(Boots_educ_unadjusted2_PFP_high_mid_educ)[1] <- "PFP_high_mid_educ_educ_boot_unadj"
names(Boots_educ_unadjusted2_PFP_high_mid_educ)[2] <- "PFP_high_mid_educ_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_PFP_high_mid_educ)[3] <- "PFP_high_mid_educ_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_PFP_high_mid_educ)[4] <- "PFP_high_mid_educ_educ_boot_unadj_up"

Boots_educ_unadjusted2_PFP_high_low_educ <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 196 & Boots_educ_unadjusted2$ID  <= 224,]  
names(Boots_educ_unadjusted2_PFP_high_low_educ)[1] <- "PFP_high_low_educ_educ_boot_unadj"
names(Boots_educ_unadjusted2_PFP_high_low_educ)[2] <- "PFP_high_low_educ_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_PFP_high_low_educ)[3] <- "PFP_high_low_educ_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_PFP_high_low_educ)[4] <- "PFP_high_low_educ_educ_boot_unadj_up"

Boots_educ_unadjusted2_PFP_mid_educ <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 224 & Boots_educ_unadjusted2$ID  <= 252,] 
names(Boots_educ_unadjusted2_PFP_mid_educ)[1] <- "PFP_mid_educ_educ_boot_unadj"
names(Boots_educ_unadjusted2_PFP_mid_educ)[2] <- "PFP_mid_educ_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_PFP_mid_educ)[3] <- "PFP_mid_educ_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_PFP_mid_educ)[4] <- "PFP_mid_educ_educ_boot_unadj_up"

Boots_educ_unadjusted2_High_mid_inequality <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 252 & Boots_educ_unadjusted2$ID  <= 280,] 
names(Boots_educ_unadjusted2_High_mid_inequality)[1] <- "High_mid_inequality_educ_boot_unadj"
names(Boots_educ_unadjusted2_High_mid_inequality)[2] <- "High_mid_inequality_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_High_mid_inequality)[3] <- "High_mid_inequality_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_High_mid_inequality)[4] <- "High_mid_inequality_educ_boot_unadj_up"

Boots_educ_unadjusted2_High_low_inequality <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 280 & Boots_educ_unadjusted2$ID  <= 308,] 
names(Boots_educ_unadjusted2_High_low_inequality)[1] <- "High_low_inequality_educ_boot_unadj"
names(Boots_educ_unadjusted2_High_low_inequality)[2] <- "High_low_inequality_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_High_low_inequality)[3] <- "High_low_inequality_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_High_low_inequality)[4] <- "High_low_inequality_educ_boot_unadj_up"

Boots_educ_unadjusted2_Mid_low_inequality <- Boots_educ_unadjusted2 [ Boots_educ_unadjusted2$ID  > 308,] 
names(Boots_educ_unadjusted2_Mid_low_inequality)[1] <- "Mid_low_inequality_educ_boot_unadj"
names(Boots_educ_unadjusted2_Mid_low_inequality)[2] <- "Mid_low_inequality_educ_boot_se_unadj"
names(Boots_educ_unadjusted2_Mid_low_inequality)[3] <- "Mid_low_inequality_educ_boot_unadj_low"
names(Boots_educ_unadjusted2_Mid_low_inequality)[4] <- "Mid_low_inequality_educ_boot_se_unadj_up"

Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_Primary_perc, Boots_educ_unadjusted2_Secondary_perc, by  = "ID2")
Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_final, Boots_educ_unadjusted2_Universitary_perc, by  = "ID2")
Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_final, Boots_educ_unadjusted2_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_final, Boots_educ_unadjusted2_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_final, Boots_educ_unadjusted2_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_final, Boots_educ_unadjusted2_PFP_high_mid_educ, by  = "ID2")
Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_final, Boots_educ_unadjusted2_PFP_high_low_educ, by  = "ID2")
Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_final, Boots_educ_unadjusted2_PFP_mid_educ, by  = "ID2")
Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_final, Boots_educ_unadjusted2_High_mid_inequality, by  = "ID2")
Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_final, Boots_educ_unadjusted2_High_low_inequality, by  = "ID2")
Boots_educ_unadjusted2_final <- merge(Boots_educ_unadjusted2_final, Boots_educ_unadjusted2_Mid_low_inequality, by  = "ID2")
Boots_educ_unadjusted2_final <- Boots_educ_unadjusted2_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]



Boots_educ_adjusted_10 <- boot(data = eu2017, statistic = function_educ, R = 1000, 
                            OR_mid_low = OR_SES_data[1,9], OR_high_low = OR_SES_data[2,9], OR_high_mid = OR_SES_data[3,9])

Boots_educ_adjusted_10$t0 -> Boots_educ_adjusted_10_estimates
Boots_educ_adjusted_10$t -> Boots_educ_adjusted_10_error1
std.error(Boots_educ_adjusted_10_error1) -> Boots_educ_adjusted_10_error2
Boots_educ_adjusted_102 <- data.frame(Boots_educ_adjusted_10_estimates,Boots_educ_adjusted_10_error2)
Boots_educ_adjusted_102$Boots_educ_adjusted_10_error2 <- Boots_educ_adjusted_102$Boots_educ_adjusted_10_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_educ_adjusted_10)) -> Boots_educ_adjusted_10_CI
Boots_educ_adjusted_102$Boots_educ_adjusted_10_low <- Boots_educ_adjusted_10_CI$lwr
Boots_educ_adjusted_102$Boots_educ_adjusted_10_up <- Boots_educ_adjusted_10_CI$upr

Boots_educ_adjusted_102$ID <- seq(from=1,to=336)
Boots_educ_adjusted_102$ID2 <- seq(from=1,to=28)

Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 0 & Boots_educ_adjusted_102$ID  <= 28] <- "Primary_perc" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 28 & Boots_educ_adjusted_102$ID  <= 56] <- "Secondary_perc" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 56 & Boots_educ_adjusted_102$ID  <= 84] <- "Universitary_perc" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 84 & Boots_educ_adjusted_102$ID  <= 112] <- "Prev_high_mid_educ_in_inactive_people" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 112 & Boots_educ_adjusted_102$ID  <= 140] <- "Prev_high_low_educ_in_inactive_people" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 140 & Boots_educ_adjusted_102$ID  <= 168] <- "Prev_mid_educ_in_inactive_people" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 168 & Boots_educ_adjusted_102$ID  <= 196] <- "PFP_high_mid_educ" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 196 & Boots_educ_adjusted_102$ID  <= 224] <- "PFP_high_low_educ" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 224 & Boots_educ_adjusted_102$ID  <= 252] <- "PFP_mid_educ" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 252 & Boots_educ_adjusted_102$ID  <= 280] <- "High_mid_inequality" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 280 & Boots_educ_adjusted_102$ID  <= 308] <- "High_low_inequality" 
Boots_educ_adjusted_102$variable [ Boots_educ_adjusted_102$ID  > 308] <- "Mid_low_inequality" 
Boots_educ_adjusted_102$variable <- as.factor(Boots_educ_adjusted_102$variable)

Boots_educ_adjusted_102_Primary_perc <- Boots_educ_adjusted_102[Boots_educ_adjusted_102$ID  > 0 & Boots_educ_adjusted_102$ID  <= 28,]
names(Boots_educ_adjusted_102_Primary_perc)[1] <- "Primary_perc_educ_boot_10"
names(Boots_educ_adjusted_102_Primary_perc)[2] <- "Primary_perc_educ_boot_se_10"
names(Boots_educ_adjusted_102_Primary_perc)[3] <- "Primary_perc_educ_boot_10_low"
names(Boots_educ_adjusted_102_Primary_perc)[4] <- "Primary_perc_educ_boot_10_up"

Boots_educ_adjusted_102_Secondary_perc <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 28 & Boots_educ_adjusted_102$ID  <= 56,] 
names(Boots_educ_adjusted_102_Secondary_perc)[1] <- "Secondary_perc_educ_boot_10"
names(Boots_educ_adjusted_102_Secondary_perc)[2] <- "Secondary_perc_educ_boot_se_10"
names(Boots_educ_adjusted_102_Secondary_perc)[3] <- "Secondary_perc_educ_boot_10_low"
names(Boots_educ_adjusted_102_Secondary_perc)[4] <- "Secondary_perc_educ_boot_10_up"

Boots_educ_adjusted_102_Universitary_perc <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 56 & Boots_educ_adjusted_102$ID  <= 84,] 
names(Boots_educ_adjusted_102_Universitary_perc)[1] <- "Universitary_perc_educ_boot_10"
names(Boots_educ_adjusted_102_Universitary_perc)[2] <- "Universitary_perc_educ_boot_se_10"
names(Boots_educ_adjusted_102_Universitary_perc)[3] <- "Universitary_perc_educ_boot_10_low"
names(Boots_educ_adjusted_102_Universitary_perc)[4] <- "Universitary_perc_educ_boot_10_up"

Boots_educ_adjusted_102_Prev_high_mid_educ_in_inactive_people <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 84 & Boots_educ_adjusted_102$ID  <= 112,] 
names(Boots_educ_adjusted_102_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_10"
names(Boots_educ_adjusted_102_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_se_10"
names(Boots_educ_adjusted_102_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_10_low"
names(Boots_educ_adjusted_102_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_10_up"

Boots_educ_adjusted_102_Prev_high_low_educ_in_inactive_people <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 112 & Boots_educ_adjusted_102$ID  <= 140,]
names(Boots_educ_adjusted_102_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_educ_in_inactive_people_educ_boot_10"
names(Boots_educ_adjusted_102_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_educ_in_inactive_people_educ_boot_se_10"
names(Boots_educ_adjusted_102_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_educ_in_inactive_people_educ_boot_10_low"
names(Boots_educ_adjusted_102_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_educ_in_inactive_people_educ_boot_10_up"

Boots_educ_adjusted_102_Prev_mid_educ_in_inactive_people <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 140 & Boots_educ_adjusted_102$ID  <= 168,]  
names(Boots_educ_adjusted_102_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_educ_in_inactive_people_educ_boot_10"
names(Boots_educ_adjusted_102_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_educ_in_inactive_people_educ_boot_se_10"
names(Boots_educ_adjusted_102_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_educ_in_inactive_people_educ_boot_10_low"
names(Boots_educ_adjusted_102_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_educ_in_inactive_people_educ_boot_10_up"

Boots_educ_adjusted_102_PFP_high_mid_educ <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 168 & Boots_educ_adjusted_102$ID  <= 196,]
names(Boots_educ_adjusted_102_PFP_high_mid_educ)[1] <- "PFP_high_mid_educ_educ_boot_10"
names(Boots_educ_adjusted_102_PFP_high_mid_educ)[2] <- "PFP_high_mid_educ_educ_boot_se_10"
names(Boots_educ_adjusted_102_PFP_high_mid_educ)[3] <- "PFP_high_mid_educ_educ_boot_10_low"
names(Boots_educ_adjusted_102_PFP_high_mid_educ)[4] <- "PFP_high_mid_educ_educ_boot_10_up"

Boots_educ_adjusted_102_PFP_high_low_educ <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 196 & Boots_educ_adjusted_102$ID  <= 224,]  
names(Boots_educ_adjusted_102_PFP_high_low_educ)[1] <- "PFP_high_low_educ_educ_boot_10"
names(Boots_educ_adjusted_102_PFP_high_low_educ)[2] <- "PFP_high_low_educ_educ_boot_se_10"
names(Boots_educ_adjusted_102_PFP_high_low_educ)[3] <- "PFP_high_low_educ_educ_boot_10_low"
names(Boots_educ_adjusted_102_PFP_high_low_educ)[4] <- "PFP_high_low_educ_educ_boot_10_up"

Boots_educ_adjusted_102_PFP_mid_educ <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 224 & Boots_educ_adjusted_102$ID  <= 252,] 
names(Boots_educ_adjusted_102_PFP_mid_educ)[1] <- "PFP_mid_educ_educ_boot_10"
names(Boots_educ_adjusted_102_PFP_mid_educ)[2] <- "PFP_mid_educ_educ_boot_se_10"
names(Boots_educ_adjusted_102_PFP_mid_educ)[3] <- "PFP_mid_educ_educ_boot_10_low"
names(Boots_educ_adjusted_102_PFP_mid_educ)[4] <- "PFP_mid_educ_educ_boot_10_up"

Boots_educ_adjusted_102_High_mid_inequality <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 252 & Boots_educ_adjusted_102$ID  <= 280,] 
names(Boots_educ_adjusted_102_High_mid_inequality)[1] <- "High_mid_inequality_educ_boot_10"
names(Boots_educ_adjusted_102_High_mid_inequality)[2] <- "High_mid_inequality_educ_boot_se_10"
names(Boots_educ_adjusted_102_High_mid_inequality)[3] <- "High_mid_inequality_educ_boot_10_low"
names(Boots_educ_adjusted_102_High_mid_inequality)[4] <- "High_mid_inequality_educ_boot_10_up"

Boots_educ_adjusted_102_High_low_inequality <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 280 & Boots_educ_adjusted_102$ID  <= 308,] 
names(Boots_educ_adjusted_102_High_low_inequality)[1] <- "High_low_inequality_educ_boot_10"
names(Boots_educ_adjusted_102_High_low_inequality)[2] <- "High_low_inequality_educ_boot_se_10"
names(Boots_educ_adjusted_102_High_low_inequality)[3] <- "High_low_inequality_educ_boot_10_low"
names(Boots_educ_adjusted_102_High_low_inequality)[4] <- "High_low_inequality_educ_boot_10_up"

Boots_educ_adjusted_102_Mid_low_inequality <- Boots_educ_adjusted_102 [ Boots_educ_adjusted_102$ID  > 308,] 
names(Boots_educ_adjusted_102_Mid_low_inequality)[1] <- "Mid_low_inequality_educ_boot_10"
names(Boots_educ_adjusted_102_Mid_low_inequality)[2] <- "Mid_low_inequality_educ_boot_se_10"
names(Boots_educ_adjusted_102_Mid_low_inequality)[3] <- "Mid_low_inequality_educ_boot_10_low"
names(Boots_educ_adjusted_102_Mid_low_inequality)[4] <- "Mid_low_inequality_educ_boot_10_up"

Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_Primary_perc, Boots_educ_adjusted_102_Secondary_perc, by  = "ID2")
Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_final, Boots_educ_adjusted_102_Universitary_perc, by  = "ID2")
Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_final, Boots_educ_adjusted_102_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_final, Boots_educ_adjusted_102_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_final, Boots_educ_adjusted_102_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_final, Boots_educ_adjusted_102_PFP_high_mid_educ, by  = "ID2")
Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_final, Boots_educ_adjusted_102_PFP_high_low_educ, by  = "ID2")
Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_final, Boots_educ_adjusted_102_PFP_mid_educ, by  = "ID2")
Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_final, Boots_educ_adjusted_102_High_mid_inequality, by  = "ID2")
Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_final, Boots_educ_adjusted_102_High_low_inequality, by  = "ID2")
Boots_educ_adjusted_102_final <- merge(Boots_educ_adjusted_102_final, Boots_educ_adjusted_102_Mid_low_inequality, by  = "ID2")
Boots_educ_adjusted_102_final <- Boots_educ_adjusted_102_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]






Boots_educ_adjusted_20 <- boot(data = eu2017, statistic = function_educ, R = 1000, 
                               OR_mid_low = OR_SES_data[1,10], OR_high_low = OR_SES_data[2,10], OR_high_mid = OR_SES_data[3,10])


Boots_educ_adjusted_20$t0 -> Boots_educ_adjusted_20_estimates
Boots_educ_adjusted_20$t -> Boots_educ_adjusted_20_error1
std.error(Boots_educ_adjusted_20_error1) -> Boots_educ_adjusted_20_error2
Boots_educ_adjusted_202 <- data.frame(Boots_educ_adjusted_20_estimates,Boots_educ_adjusted_20_error2)
Boots_educ_adjusted_202$Boots_educ_adjusted_20_error2 <- Boots_educ_adjusted_202$Boots_educ_adjusted_20_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_educ_adjusted_20)) -> Boots_educ_adjusted_20_CI
Boots_educ_adjusted_202$Boots_educ_adjusted_20_low <- Boots_educ_adjusted_20_CI$lwr
Boots_educ_adjusted_202$Boots_educ_adjusted_20_up <- Boots_educ_adjusted_20_CI$upr

Boots_educ_adjusted_202$ID <- seq(from=1,to=336)
Boots_educ_adjusted_202$ID2 <- seq(from=1,to=28)

Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 0 & Boots_educ_adjusted_202$ID  <= 28] <- "Primary_perc" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 28 & Boots_educ_adjusted_202$ID  <= 56] <- "Secondary_perc" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 56 & Boots_educ_adjusted_202$ID  <= 84] <- "Universitary_perc" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 84 & Boots_educ_adjusted_202$ID  <= 112] <- "Prev_high_mid_educ_in_inactive_people" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 112 & Boots_educ_adjusted_202$ID  <= 140] <- "Prev_high_low_educ_in_inactive_people" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 140 & Boots_educ_adjusted_202$ID  <= 168] <- "Prev_mid_educ_in_inactive_people" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 168 & Boots_educ_adjusted_202$ID  <= 196] <- "PFP_high_mid_educ" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 196 & Boots_educ_adjusted_202$ID  <= 224] <- "PFP_high_low_educ" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 224 & Boots_educ_adjusted_202$ID  <= 252] <- "PFP_mid_educ" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 252 & Boots_educ_adjusted_202$ID  <= 280] <- "High_mid_inequality" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 280 & Boots_educ_adjusted_202$ID  <= 308] <- "High_low_inequality" 
Boots_educ_adjusted_202$variable [ Boots_educ_adjusted_202$ID  > 308] <- "Mid_low_inequality" 
Boots_educ_adjusted_202$variable <- as.factor(Boots_educ_adjusted_202$variable)

Boots_educ_adjusted_202_Primary_perc <- Boots_educ_adjusted_202[Boots_educ_adjusted_202$ID  > 0 & Boots_educ_adjusted_202$ID  <= 28,]
names(Boots_educ_adjusted_202_Primary_perc)[1] <- "Primary_perc_educ_boot_20"
names(Boots_educ_adjusted_202_Primary_perc)[2] <- "Primary_perc_educ_boot_se_20"
names(Boots_educ_adjusted_202_Primary_perc)[3] <- "Primary_perc_educ_boot_20_low"
names(Boots_educ_adjusted_202_Primary_perc)[4] <- "Primary_perc_educ_boot_20_up"

Boots_educ_adjusted_202_Secondary_perc <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 28 & Boots_educ_adjusted_202$ID  <= 56,] 
names(Boots_educ_adjusted_202_Secondary_perc)[1] <- "Secondary_perc_educ_boot_20"
names(Boots_educ_adjusted_202_Secondary_perc)[2] <- "Secondary_perc_educ_boot_se_20"
names(Boots_educ_adjusted_202_Secondary_perc)[3] <- "Secondary_perc_educ_boot_20_low"
names(Boots_educ_adjusted_202_Secondary_perc)[4] <- "Secondary_perc_educ_boot_20_up"

Boots_educ_adjusted_202_Universitary_perc <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 56 & Boots_educ_adjusted_202$ID  <= 84,] 
names(Boots_educ_adjusted_202_Universitary_perc)[1] <- "Universitary_perc_educ_boot_20"
names(Boots_educ_adjusted_202_Universitary_perc)[2] <- "Universitary_perc_educ_boot_se_20"
names(Boots_educ_adjusted_202_Universitary_perc)[3] <- "Universitary_perc_educ_boot_20_low"
names(Boots_educ_adjusted_202_Universitary_perc)[4] <- "Universitary_perc_educ_boot_20_up"

Boots_educ_adjusted_202_Prev_high_mid_educ_in_inactive_people <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 84 & Boots_educ_adjusted_202$ID  <= 112,] 
names(Boots_educ_adjusted_202_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_20"
names(Boots_educ_adjusted_202_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_se_20"
names(Boots_educ_adjusted_202_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_20_low"
names(Boots_educ_adjusted_202_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_20_up"

Boots_educ_adjusted_202_Prev_high_low_educ_in_inactive_people <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 112 & Boots_educ_adjusted_202$ID  <= 140,]
names(Boots_educ_adjusted_202_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_educ_in_inactive_people_educ_boot_20"
names(Boots_educ_adjusted_202_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_educ_in_inactive_people_educ_boot_se_20"
names(Boots_educ_adjusted_202_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_educ_in_inactive_people_educ_boot_20_low"
names(Boots_educ_adjusted_202_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_educ_in_inactive_people_educ_boot_20_up"

Boots_educ_adjusted_202_Prev_mid_educ_in_inactive_people <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 140 & Boots_educ_adjusted_202$ID  <= 168,]  
names(Boots_educ_adjusted_202_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_educ_in_inactive_people_educ_boot_20"
names(Boots_educ_adjusted_202_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_educ_in_inactive_people_educ_boot_se_20"
names(Boots_educ_adjusted_202_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_educ_in_inactive_people_educ_boot_20_low"
names(Boots_educ_adjusted_202_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_educ_in_inactive_people_educ_boot_20_up"

Boots_educ_adjusted_202_PFP_high_mid_educ <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 168 & Boots_educ_adjusted_202$ID  <= 196,]
names(Boots_educ_adjusted_202_PFP_high_mid_educ)[1] <- "PFP_high_mid_educ_educ_boot_20"
names(Boots_educ_adjusted_202_PFP_high_mid_educ)[2] <- "PFP_high_mid_educ_educ_boot_se_20"
names(Boots_educ_adjusted_202_PFP_high_mid_educ)[3] <- "PFP_high_mid_educ_educ_boot_20_low"
names(Boots_educ_adjusted_202_PFP_high_mid_educ)[4] <- "PFP_high_mid_educ_educ_boot_20_up"

Boots_educ_adjusted_202_PFP_high_low_educ <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 196 & Boots_educ_adjusted_202$ID  <= 224,]  
names(Boots_educ_adjusted_202_PFP_high_low_educ)[1] <- "PFP_high_low_educ_educ_boot_20"
names(Boots_educ_adjusted_202_PFP_high_low_educ)[2] <- "PFP_high_low_educ_educ_boot_se_20"
names(Boots_educ_adjusted_202_PFP_high_low_educ)[3] <- "PFP_high_low_educ_educ_boot_20_low"
names(Boots_educ_adjusted_202_PFP_high_low_educ)[4] <- "PFP_high_low_educ_educ_boot_20_up"

Boots_educ_adjusted_202_PFP_mid_educ <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 224 & Boots_educ_adjusted_202$ID  <= 252,] 
names(Boots_educ_adjusted_202_PFP_mid_educ)[1] <- "PFP_mid_educ_educ_boot_20"
names(Boots_educ_adjusted_202_PFP_mid_educ)[2] <- "PFP_mid_educ_educ_boot_se_20"
names(Boots_educ_adjusted_202_PFP_mid_educ)[3] <- "PFP_mid_educ_educ_boot_20_low"
names(Boots_educ_adjusted_202_PFP_mid_educ)[4] <- "PFP_mid_educ_educ_boot_20_up"

Boots_educ_adjusted_202_High_mid_inequality <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 252 & Boots_educ_adjusted_202$ID  <= 280,] 
names(Boots_educ_adjusted_202_High_mid_inequality)[1] <- "High_mid_inequality_educ_boot_20"
names(Boots_educ_adjusted_202_High_mid_inequality)[2] <- "High_mid_inequality_educ_boot_se_20"
names(Boots_educ_adjusted_202_High_mid_inequality)[3] <- "High_mid_inequality_educ_boot_20_low"
names(Boots_educ_adjusted_202_High_mid_inequality)[4] <- "High_mid_inequality_educ_boot_20_up"

Boots_educ_adjusted_202_High_low_inequality <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 280 & Boots_educ_adjusted_202$ID  <= 308,] 
names(Boots_educ_adjusted_202_High_low_inequality)[1] <- "High_low_inequality_educ_boot_20"
names(Boots_educ_adjusted_202_High_low_inequality)[2] <- "High_low_inequality_educ_boot_se_20"
names(Boots_educ_adjusted_202_High_low_inequality)[3] <- "High_low_inequality_educ_boot_20_low"
names(Boots_educ_adjusted_202_High_low_inequality)[4] <- "High_low_inequality_educ_boot_20_up"

Boots_educ_adjusted_202_Mid_low_inequality <- Boots_educ_adjusted_202 [ Boots_educ_adjusted_202$ID  > 308,] 
names(Boots_educ_adjusted_202_Mid_low_inequality)[1] <- "Mid_low_inequality_educ_boot_20"
names(Boots_educ_adjusted_202_Mid_low_inequality)[2] <- "Mid_low_inequality_educ_boot_se_20"
names(Boots_educ_adjusted_202_Mid_low_inequality)[3] <- "Mid_low_inequality_educ_boot_20_low"
names(Boots_educ_adjusted_202_Mid_low_inequality)[4] <- "Mid_low_inequality_educ_boot_20_up"

Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_Primary_perc, Boots_educ_adjusted_202_Secondary_perc, by  = "ID2")
Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_final, Boots_educ_adjusted_202_Universitary_perc, by  = "ID2")
Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_final, Boots_educ_adjusted_202_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_final, Boots_educ_adjusted_202_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_final, Boots_educ_adjusted_202_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_final, Boots_educ_adjusted_202_PFP_high_mid_educ, by  = "ID2")
Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_final, Boots_educ_adjusted_202_PFP_high_low_educ, by  = "ID2")
Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_final, Boots_educ_adjusted_202_PFP_mid_educ, by  = "ID2")
Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_final, Boots_educ_adjusted_202_High_mid_inequality, by  = "ID2")
Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_final, Boots_educ_adjusted_202_High_low_inequality, by  = "ID2")
Boots_educ_adjusted_202_final <- merge(Boots_educ_adjusted_202_final, Boots_educ_adjusted_202_Mid_low_inequality, by  = "ID2")
Boots_educ_adjusted_202_final <- Boots_educ_adjusted_202_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]






Boots_educ_adjusted_50 <- boot(data = eu2017, statistic = function_educ, R = 1000, 
                               OR_mid_low = OR_SES_data[1,11], OR_high_low = OR_SES_data[2,11], OR_high_mid = OR_SES_data[3,11])


Boots_educ_adjusted_50$t0 -> Boots_educ_adjusted_50_estimates
Boots_educ_adjusted_50$t -> Boots_educ_adjusted_50_error1
std.error(Boots_educ_adjusted_50_error1) -> Boots_educ_adjusted_50_error2
Boots_educ_adjusted_502 <- data.frame(Boots_educ_adjusted_50_estimates,Boots_educ_adjusted_50_error2)
Boots_educ_adjusted_502$Boots_educ_adjusted_50_error2 <- Boots_educ_adjusted_502$Boots_educ_adjusted_50_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_educ_adjusted_50)) -> Boots_educ_adjusted_50_CI
Boots_educ_adjusted_502$Boots_educ_adjusted_50_low <- Boots_educ_adjusted_50_CI$lwr
Boots_educ_adjusted_502$Boots_educ_adjusted_50_up <- Boots_educ_adjusted_50_CI$upr

Boots_educ_adjusted_502$ID <- seq(from=1,to=336)
Boots_educ_adjusted_502$ID2 <- seq(from=1,to=28)

Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 0 & Boots_educ_adjusted_502$ID  <= 28] <- "Primary_perc" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 28 & Boots_educ_adjusted_502$ID  <= 56] <- "Secondary_perc" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 56 & Boots_educ_adjusted_502$ID  <= 84] <- "Universitary_perc" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 84 & Boots_educ_adjusted_502$ID  <= 112] <- "Prev_high_mid_educ_in_inactive_people" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 112 & Boots_educ_adjusted_502$ID  <= 140] <- "Prev_high_low_educ_in_inactive_people" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 140 & Boots_educ_adjusted_502$ID  <= 168] <- "Prev_mid_educ_in_inactive_people" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 168 & Boots_educ_adjusted_502$ID  <= 196] <- "PFP_high_mid_educ" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 196 & Boots_educ_adjusted_502$ID  <= 224] <- "PFP_high_low_educ" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 224 & Boots_educ_adjusted_502$ID  <= 252] <- "PFP_mid_educ" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 252 & Boots_educ_adjusted_502$ID  <= 280] <- "High_mid_inequality" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 280 & Boots_educ_adjusted_502$ID  <= 308] <- "High_low_inequality" 
Boots_educ_adjusted_502$variable [ Boots_educ_adjusted_502$ID  > 308] <- "Mid_low_inequality" 
Boots_educ_adjusted_502$variable <- as.factor(Boots_educ_adjusted_502$variable)

Boots_educ_adjusted_502_Primary_perc <- Boots_educ_adjusted_502[Boots_educ_adjusted_502$ID  > 0 & Boots_educ_adjusted_502$ID  <= 28,]
names(Boots_educ_adjusted_502_Primary_perc)[1] <- "Primary_perc_educ_boot_50"
names(Boots_educ_adjusted_502_Primary_perc)[2] <- "Primary_perc_educ_boot_se_50"
names(Boots_educ_adjusted_502_Primary_perc)[3] <- "Primary_perc_educ_boot_50_low"
names(Boots_educ_adjusted_502_Primary_perc)[4] <- "Primary_perc_educ_boot_50_up"

Boots_educ_adjusted_502_Secondary_perc <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 28 & Boots_educ_adjusted_502$ID  <= 56,] 
names(Boots_educ_adjusted_502_Secondary_perc)[1] <- "Secondary_perc_educ_boot_50"
names(Boots_educ_adjusted_502_Secondary_perc)[2] <- "Secondary_perc_educ_boot_se_50"
names(Boots_educ_adjusted_502_Secondary_perc)[3] <- "Secondary_perc_educ_boot_50_low"
names(Boots_educ_adjusted_502_Secondary_perc)[4] <- "Secondary_perc_educ_boot_50_up"

Boots_educ_adjusted_502_Universitary_perc <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 56 & Boots_educ_adjusted_502$ID  <= 84,] 
names(Boots_educ_adjusted_502_Universitary_perc)[1] <- "Universitary_perc_educ_boot_50"
names(Boots_educ_adjusted_502_Universitary_perc)[2] <- "Universitary_perc_educ_boot_se_50"
names(Boots_educ_adjusted_502_Universitary_perc)[3] <- "Universitary_perc_educ_boot_50_low"
names(Boots_educ_adjusted_502_Universitary_perc)[4] <- "Universitary_perc_educ_boot_50_up"

Boots_educ_adjusted_502_Prev_high_mid_educ_in_inactive_people <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 84 & Boots_educ_adjusted_502$ID  <= 112,] 
names(Boots_educ_adjusted_502_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_50"
names(Boots_educ_adjusted_502_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_se_50"
names(Boots_educ_adjusted_502_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_50_low"
names(Boots_educ_adjusted_502_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_educ_in_inactive_people_educ_boot_50_up"

Boots_educ_adjusted_502_Prev_high_low_educ_in_inactive_people <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 112 & Boots_educ_adjusted_502$ID  <= 140,]
names(Boots_educ_adjusted_502_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_educ_in_inactive_people_educ_boot_50"
names(Boots_educ_adjusted_502_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_educ_in_inactive_people_educ_boot_se_50"
names(Boots_educ_adjusted_502_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_educ_in_inactive_people_educ_boot_50_low"
names(Boots_educ_adjusted_502_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_educ_in_inactive_people_educ_boot_50_up"

Boots_educ_adjusted_502_Prev_mid_educ_in_inactive_people <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 140 & Boots_educ_adjusted_502$ID  <= 168,]  
names(Boots_educ_adjusted_502_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_educ_in_inactive_people_educ_boot_50"
names(Boots_educ_adjusted_502_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_educ_in_inactive_people_educ_boot_se_50"
names(Boots_educ_adjusted_502_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_educ_in_inactive_people_educ_boot_50_low"
names(Boots_educ_adjusted_502_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_educ_in_inactive_people_educ_boot_50_up"

Boots_educ_adjusted_502_PFP_high_mid_educ <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 168 & Boots_educ_adjusted_502$ID  <= 196,]
names(Boots_educ_adjusted_502_PFP_high_mid_educ)[1] <- "PFP_high_mid_educ_educ_boot_50"
names(Boots_educ_adjusted_502_PFP_high_mid_educ)[2] <- "PFP_high_mid_educ_educ_boot_se_50"
names(Boots_educ_adjusted_502_PFP_high_mid_educ)[3] <- "PFP_high_mid_educ_educ_boot_50_low"
names(Boots_educ_adjusted_502_PFP_high_mid_educ)[4] <- "PFP_high_mid_educ_educ_boot_50_up"

Boots_educ_adjusted_502_PFP_high_low_educ <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 196 & Boots_educ_adjusted_502$ID  <= 224,]  
names(Boots_educ_adjusted_502_PFP_high_low_educ)[1] <- "PFP_high_low_educ_educ_boot_50"
names(Boots_educ_adjusted_502_PFP_high_low_educ)[2] <- "PFP_high_low_educ_educ_boot_se_50"
names(Boots_educ_adjusted_502_PFP_high_low_educ)[3] <- "PFP_high_low_educ_educ_boot_50_low"
names(Boots_educ_adjusted_502_PFP_high_low_educ)[4] <- "PFP_high_low_educ_educ_boot_50_up"

Boots_educ_adjusted_502_PFP_mid_educ <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 224 & Boots_educ_adjusted_502$ID  <= 252,] 
names(Boots_educ_adjusted_502_PFP_mid_educ)[1] <- "PFP_mid_educ_educ_boot_50"
names(Boots_educ_adjusted_502_PFP_mid_educ)[2] <- "PFP_mid_educ_educ_boot_se_50"
names(Boots_educ_adjusted_502_PFP_mid_educ)[3] <- "PFP_mid_educ_educ_boot_50_low"
names(Boots_educ_adjusted_502_PFP_mid_educ)[4] <- "PFP_mid_educ_educ_boot_50_up"

Boots_educ_adjusted_502_High_mid_inequality <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 252 & Boots_educ_adjusted_502$ID  <= 280,] 
names(Boots_educ_adjusted_502_High_mid_inequality)[1] <- "High_mid_inequality_educ_boot_50"
names(Boots_educ_adjusted_502_High_mid_inequality)[2] <- "High_mid_inequality_educ_boot_se_50"
names(Boots_educ_adjusted_502_High_mid_inequality)[3] <- "High_mid_inequality_educ_boot_50_low"
names(Boots_educ_adjusted_502_High_mid_inequality)[4] <- "High_mid_inequality_educ_boot_50_up"

Boots_educ_adjusted_502_High_low_inequality <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 280 & Boots_educ_adjusted_502$ID  <= 308,] 
names(Boots_educ_adjusted_502_High_low_inequality)[1] <- "High_low_inequality_educ_boot_50"
names(Boots_educ_adjusted_502_High_low_inequality)[2] <- "High_low_inequality_educ_boot_se_50"
names(Boots_educ_adjusted_502_High_low_inequality)[3] <- "High_low_inequality_educ_boot_50_low"
names(Boots_educ_adjusted_502_High_low_inequality)[4] <- "High_low_inequality_educ_boot_50_up"

Boots_educ_adjusted_502_Mid_low_inequality <- Boots_educ_adjusted_502 [ Boots_educ_adjusted_502$ID  > 308,] 
names(Boots_educ_adjusted_502_Mid_low_inequality)[1] <- "Mid_low_inequality_educ_boot_50"
names(Boots_educ_adjusted_502_Mid_low_inequality)[2] <- "Mid_low_inequality_educ_boot_se_50"
names(Boots_educ_adjusted_502_Mid_low_inequality)[3] <- "Mid_low_inequality_educ_boot_50_low"
names(Boots_educ_adjusted_502_Mid_low_inequality)[4] <- "Mid_low_inequality_educ_boot_50_up"

Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_Primary_perc, Boots_educ_adjusted_502_Secondary_perc, by  = "ID2")
Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_final, Boots_educ_adjusted_502_Universitary_perc, by  = "ID2")
Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_final, Boots_educ_adjusted_502_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_final, Boots_educ_adjusted_502_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_final, Boots_educ_adjusted_502_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_final, Boots_educ_adjusted_502_PFP_high_mid_educ, by  = "ID2")
Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_final, Boots_educ_adjusted_502_PFP_high_low_educ, by  = "ID2")
Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_final, Boots_educ_adjusted_502_PFP_mid_educ, by  = "ID2")
Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_final, Boots_educ_adjusted_502_High_mid_inequality, by  = "ID2")
Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_final, Boots_educ_adjusted_502_High_low_inequality, by  = "ID2")
Boots_educ_adjusted_502_final <- merge(Boots_educ_adjusted_502_final, Boots_educ_adjusted_502_Mid_low_inequality, by  = "ID2")
Boots_educ_adjusted_502_final <- Boots_educ_adjusted_502_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]







function_ocup <- function(data, i, OR_mid_low, OR_high_low, OR_high_mid) {
    data[i,] %>% group_by(Country_rec) %>% summarise(Low_sc = sum(Ocupation_3cat == "V-VII", na.rm = T),
                                                   Mid_sc = sum(Ocupation_3cat == "III-IV", na.rm = T),
                                                   High_sc = sum(Ocupation_3cat == "I-II", na.rm = T)) -> data_sc
    data_sc$n <- data_sc$Low_sc + data_sc$Mid_sc + data_sc$High_sc
    data_sc$Low_sc_perc <- (data_sc$Low_sc * 100)/ data_sc$n
    data_sc$Mid_sc_perc <- (data_sc$Mid_sc * 100)/ data_sc$n
    data_sc$High_sc_perc <- (data_sc$High_sc * 100)/ data_sc$n
    
    data_sc$High_mid_inequality <- data_sc$High_sc_perc - data_sc$Mid_sc_perc
    data_sc$High_low_inequality <- data_sc$High_sc_perc - data_sc$Low_sc_perc
    data_sc$Mid_low_inequality <- data_sc$Mid_sc_perc - data_sc$Low_sc_perc
    
    data_sc$Prev_high_mid_sc_in_inactive_people <- (data_sc$High_sc_perc * OR_high_mid)/((data_sc$High_sc_perc * OR_high_mid)+(1 - data_sc$High_sc_perc))
    data_sc$Prev_high_low_sc_in_inactive_people <- (data_sc$High_sc_perc * OR_high_low)/((data_sc$High_sc_perc * OR_high_low)+(1 - data_sc$High_sc_perc))
    data_sc$Prev_mid_sc_in_inactive_people <- (data_sc$Mid_sc_perc * OR_mid_low)/((data_sc$Mid_sc_perc * OR_mid_low)+(1 - data_sc$Mid_sc_perc))
    
    data_sc$PFP_high_mid_sc <- (data_sc$Prev_high_mid_sc_in_inactive_people * (1 - OR_high_mid))/(1 - (1 - OR_high_mid)*(1 - data_sc$Prev_high_mid_sc_in_inactive_people))
    data_sc$PFP_high_low_sc <- (data_sc$Prev_high_low_sc_in_inactive_people * (1 - OR_high_low))/(1 - (1 - OR_high_low)*(1 - data_sc$Prev_high_low_sc_in_inactive_people))
    data_sc$PFP_mid_sc <- (data_sc$Prev_mid_sc_in_inactive_people * (1 - OR_mid_low))/(1 - (1 - OR_mid_low)*(1 - data_sc$Prev_mid_sc_in_inactive_people))
    
    return(c(data_sc$Low_sc_perc, data_sc$Mid_sc_perc, data_sc$High_sc_perc, data_sc$Prev_high_mid_sc_in_inactive_people, 
                      data_sc$Prev_high_low_sc_in_inactive_people, data_sc$Prev_mid_sc_in_inactive_people, data_sc$PFP_high_mid_sc, 
                      data_sc$PFP_high_low_sc, data_sc$PFP_mid_sc, data_sc$High_mid_inequality, data_sc$High_low_inequality, 
                      data_sc$Mid_low_inequality))
}


Boots_sc_adjusted <- boot(data = eu2017, statistic = function_ocup, R = 1000, 
                            OR_mid_low = OR_SES_data[5,6], OR_high_low = OR_SES_data[4,6], OR_high_mid = OR_SES_data[6,6])

Boots_sc_adjusted$t0 -> Boots_sc_adjusted_estimates
Boots_sc_adjusted$t -> Boots_sc_adjusted_error1
std.error(Boots_sc_adjusted_error1) -> Boots_sc_adjusted_error2
Boots_sc_adjusted2 <- data.frame(Boots_sc_adjusted_estimates,Boots_sc_adjusted_error2)
Boots_sc_adjusted2$Boots_sc_adjusted_error2 <- Boots_sc_adjusted2$Boots_sc_adjusted_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_sc_adjusted)) -> Boots_sc_adjusted_CI
Boots_sc_adjusted2$Boots_sc_adjusted_low <- Boots_sc_adjusted_CI$lwr
Boots_sc_adjusted2$Boots_sc_adjusted_up <- Boots_sc_adjusted_CI$upr

Boots_sc_adjusted2$ID <- seq(from=1,to=336)
Boots_sc_adjusted2$ID2 <- seq(from=1,to=28)

Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 0 & Boots_sc_adjusted2$ID  <= 28] <- "Low_sc_perc" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 28 & Boots_sc_adjusted2$ID  <= 56] <- "Mid_sc_perc" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 56 & Boots_sc_adjusted2$ID  <= 84] <- "High_sc_perc" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 84 & Boots_sc_adjusted2$ID  <= 112] <- "Prev_high_mid_sc_in_inactive_people" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 112 & Boots_sc_adjusted2$ID  <= 140] <- "Prev_high_low_sc_in_inactive_people" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 140 & Boots_sc_adjusted2$ID  <= 168] <- "Prev_mid_sc_in_inactive_people" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 168 & Boots_sc_adjusted2$ID  <= 196] <- "PFP_high_mid_sc" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 196 & Boots_sc_adjusted2$ID  <= 224] <- "PFP_high_low_sc" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 224 & Boots_sc_adjusted2$ID  <= 252] <- "PFP_mid_sc" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 252 & Boots_sc_adjusted2$ID  <= 280] <- "High_mid_inequality" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 280 & Boots_sc_adjusted2$ID  <= 308] <- "High_low_inequality" 
Boots_sc_adjusted2$variable [ Boots_sc_adjusted2$ID  > 308] <- "Mid_low_inequality" 
Boots_sc_adjusted2$variable <- as.factor(Boots_sc_adjusted2$variable)

Boots_sc_adjusted2_Primary_perc <- Boots_sc_adjusted2[Boots_sc_adjusted2$ID  > 0 & Boots_sc_adjusted2$ID  <= 28,]
names(Boots_sc_adjusted2_Primary_perc)[1] <- "Low_sc_perc_boot"
names(Boots_sc_adjusted2_Primary_perc)[2] <- "Low_sc_perc_boot_se"
names(Boots_sc_adjusted2_Primary_perc)[3] <- "Low_sc_perc_boot_low"
names(Boots_sc_adjusted2_Primary_perc)[4] <- "Low_sc_perc_boot_up"

Boots_sc_adjusted2_Secondary_perc <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 28 & Boots_sc_adjusted2$ID  <= 56,] 
names(Boots_sc_adjusted2_Secondary_perc)[1] <- "Mid_sc_perc_boot"
names(Boots_sc_adjusted2_Secondary_perc)[2] <- "Mid_sc_perc_boot_se"
names(Boots_sc_adjusted2_Secondary_perc)[3] <- "Mid_sc_perc_boot_low"
names(Boots_sc_adjusted2_Secondary_perc)[4] <- "Mid_sc_perc_boot_up"

Boots_sc_adjusted2_Universitary_perc <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 56 & Boots_sc_adjusted2$ID  <= 84,] 
names(Boots_sc_adjusted2_Universitary_perc)[1] <- "High_sc_perc_boot"
names(Boots_sc_adjusted2_Universitary_perc)[2] <- "High_sc_perc_boot_se"
names(Boots_sc_adjusted2_Universitary_perc)[3] <- "High_sc_perc_boot_low"
names(Boots_sc_adjusted2_Universitary_perc)[4] <- "High_sc_perc_boot_up"

Boots_sc_adjusted2_Prev_high_mid_educ_in_inactive_people <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 84 & Boots_sc_adjusted2$ID  <= 112,] 
names(Boots_sc_adjusted2_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_sc_in_inactive_people_boot"
names(Boots_sc_adjusted2_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_sc_in_inactive_people_boot_se"
names(Boots_sc_adjusted2_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_sc_in_inactive_people_boot_low"
names(Boots_sc_adjusted2_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_sc_in_inactive_people_boot_up"

Boots_sc_adjusted2_Prev_high_low_educ_in_inactive_people <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 112 & Boots_sc_adjusted2$ID  <= 140,]
names(Boots_sc_adjusted2_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_sc_in_inactive_people_boot"
names(Boots_sc_adjusted2_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_sc_in_inactive_people_boot_se"
names(Boots_sc_adjusted2_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_sc_in_inactive_people_boot_low"
names(Boots_sc_adjusted2_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_sc_in_inactive_people_boot_up"

Boots_sc_adjusted2_Prev_mid_educ_in_inactive_people <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 140 & Boots_sc_adjusted2$ID  <= 168,]  
names(Boots_sc_adjusted2_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_sc_in_inactive_people_boot"
names(Boots_sc_adjusted2_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_sc_in_inactive_people_boot_se"
names(Boots_sc_adjusted2_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_sc_in_inactive_people_boot_low"
names(Boots_sc_adjusted2_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_sc_in_inactive_people_boot_up"

Boots_sc_adjusted2_PFP_high_mid_educ <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 168 & Boots_sc_adjusted2$ID  <= 196,]
names(Boots_sc_adjusted2_PFP_high_mid_educ)[1] <- "PFP_high_mid_sc_boot"
names(Boots_sc_adjusted2_PFP_high_mid_educ)[2] <- "PFP_high_mid_sc_boot_se"
names(Boots_sc_adjusted2_PFP_high_mid_educ)[3] <- "PFP_high_mid_sc_boot_low"
names(Boots_sc_adjusted2_PFP_high_mid_educ)[4] <- "PFP_high_mid_sc_boot_up"

Boots_sc_adjusted2_PFP_high_low_educ <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 196 & Boots_sc_adjusted2$ID  <= 224,]  
names(Boots_sc_adjusted2_PFP_high_low_educ)[1] <- "PFP_high_low_sc_boot"
names(Boots_sc_adjusted2_PFP_high_low_educ)[2] <- "PFP_high_low_sc_boot_se"
names(Boots_sc_adjusted2_PFP_high_low_educ)[3] <- "PFP_high_low_sc_boot_low"
names(Boots_sc_adjusted2_PFP_high_low_educ)[4] <- "PFP_high_low_sc_boot_up"

Boots_sc_adjusted2_PFP_mid_educ <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 224 & Boots_sc_adjusted2$ID  <= 252,] 
names(Boots_sc_adjusted2_PFP_mid_educ)[1] <- "PFP_mid_sc_boot"
names(Boots_sc_adjusted2_PFP_mid_educ)[2] <- "PFP_mid_sc_boot_se"
names(Boots_sc_adjusted2_PFP_mid_educ)[3] <- "PFP_mid_sc_boot_low"
names(Boots_sc_adjusted2_PFP_mid_educ)[4] <- "PFP_mid_sc_boot_up"

Boots_sc_adjusted2_High_mid_inequality <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 252 & Boots_sc_adjusted2$ID  <= 280,] 
names(Boots_sc_adjusted2_High_mid_inequality)[1] <- "High_mid_inequality_sc_boot"
names(Boots_sc_adjusted2_High_mid_inequality)[2] <- "High_mid_inequality_sc_boot_se"
names(Boots_sc_adjusted2_High_mid_inequality)[3] <- "High_mid_inequality_sc_boot_low"
names(Boots_sc_adjusted2_High_mid_inequality)[4] <- "High_mid_inequality_sc_boot_up"

Boots_sc_adjusted2_High_low_inequality <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 280 & Boots_sc_adjusted2$ID  <= 308,] 
names(Boots_sc_adjusted2_High_low_inequality)[1] <- "High_low_inequality_sc_boot"
names(Boots_sc_adjusted2_High_low_inequality)[2] <- "High_low_inequality_sc_boot_se"
names(Boots_sc_adjusted2_High_low_inequality)[3] <- "High_low_inequality_sc_boot_low"
names(Boots_sc_adjusted2_High_low_inequality)[4] <- "High_low_inequality_sc_boot_up"

Boots_sc_adjusted2_Mid_low_inequality <- Boots_sc_adjusted2 [ Boots_sc_adjusted2$ID  > 308,] 
names(Boots_sc_adjusted2_Mid_low_inequality)[1] <- "Mid_low_inequality_sc_boot"
names(Boots_sc_adjusted2_Mid_low_inequality)[2] <- "Mid_low_inequality_sc_boot_se"
names(Boots_sc_adjusted2_Mid_low_inequality)[3] <- "Mid_low_inequality_sc_boot_low"
names(Boots_sc_adjusted2_Mid_low_inequality)[4] <- "Mid_low_inequality_sc_boot_up"

Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_Primary_perc, Boots_sc_adjusted2_Secondary_perc, by  = "ID2")
Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_final, Boots_sc_adjusted2_Universitary_perc, by  = "ID2")
Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_final, Boots_sc_adjusted2_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_final, Boots_sc_adjusted2_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_final, Boots_sc_adjusted2_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_final, Boots_sc_adjusted2_PFP_high_mid_educ, by  = "ID2")
Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_final, Boots_sc_adjusted2_PFP_high_low_educ, by  = "ID2")
Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_final, Boots_sc_adjusted2_PFP_mid_educ, by  = "ID2")
Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_final, Boots_sc_adjusted2_High_mid_inequality, by  = "ID2")
Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_final, Boots_sc_adjusted2_High_low_inequality, by  = "ID2")
Boots_sc_adjusted2_final <- merge(Boots_sc_adjusted2_final, Boots_sc_adjusted2_Mid_low_inequality, by  = "ID2")
Boots_sc_adjusted2_final <- Boots_sc_adjusted2_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]





Boots_sc_unadjusted <- boot(data = eu2017, statistic = function_ocup, R = 1000, 
                              OR_mid_low = OR_SES_data[5,3], OR_high_low = OR_SES_data[4,3], OR_high_mid = OR_SES_data[6,3])


Boots_sc_unadjusted$t0 -> Boots_sc_unadjusted_estimates
Boots_sc_unadjusted$t -> Boots_sc_unadjusted_error1
std.error(Boots_sc_unadjusted_error1) -> Boots_sc_unadjusted_error2
Boots_sc_unadjusted2 <- data.frame(Boots_sc_unadjusted_estimates,Boots_sc_unadjusted_error2)
Boots_sc_unadjusted2$Boots_sc_unadjusted_error2 <- Boots_sc_unadjusted2$Boots_sc_unadjusted_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_sc_unadjusted)) -> Boots_sc_unadjusted_CI
Boots_sc_unadjusted2$Boots_sc_unadjusted_low <- Boots_sc_unadjusted_CI$lwr
Boots_sc_unadjusted2$Boots_sc_unadjusted_up <- Boots_sc_unadjusted_CI$upr

Boots_sc_unadjusted2$ID <- seq(from=1,to=336)
Boots_sc_unadjusted2$ID2 <- seq(from=1,to=28)

Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 0 & Boots_sc_unadjusted2$ID  <= 28] <- "Low_sc_perc" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 28 & Boots_sc_unadjusted2$ID  <= 56] <- "Mid_sc_perc" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 56 & Boots_sc_unadjusted2$ID  <= 84] <- "High_sc_perc" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 84 & Boots_sc_unadjusted2$ID  <= 112] <- "Prev_high_mid_sc_in_inactive_people" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 112 & Boots_sc_unadjusted2$ID  <= 140] <- "Prev_high_low_sc_in_inactive_people" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 140 & Boots_sc_unadjusted2$ID  <= 168] <- "Prev_mid_sc_in_inactive_people" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 168 & Boots_sc_unadjusted2$ID  <= 196] <- "PFP_high_mid_sc" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 196 & Boots_sc_unadjusted2$ID  <= 224] <- "PFP_high_low_sc" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 224 & Boots_sc_unadjusted2$ID  <= 252] <- "PFP_mid_sc" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 252 & Boots_sc_unadjusted2$ID  <= 280] <- "High_mid_inequality" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 280 & Boots_sc_unadjusted2$ID  <= 308] <- "High_low_inequality" 
Boots_sc_unadjusted2$variable [ Boots_sc_unadjusted2$ID  > 308] <- "Mid_low_inequality" 
Boots_sc_unadjusted2$variable <- as.factor(Boots_sc_unadjusted2$variable)

Boots_sc_unadjusted2_Primary_perc <- Boots_sc_unadjusted2[Boots_sc_unadjusted2$ID  > 0 & Boots_sc_unadjusted2$ID  <= 28,]
names(Boots_sc_unadjusted2_Primary_perc)[1] <- "Low_sc_perc_boot_unadj"
names(Boots_sc_unadjusted2_Primary_perc)[2] <- "Low_sc_perc_boot_se_unadj"
names(Boots_sc_unadjusted2_Primary_perc)[3] <- "Low_sc_perc_boot_unadj_low"
names(Boots_sc_unadjusted2_Primary_perc)[4] <- "Low_sc_perc_boot_unadj_up"

Boots_sc_unadjusted2_Secondary_perc <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 28 & Boots_sc_unadjusted2$ID  <= 56,] 
names(Boots_sc_unadjusted2_Secondary_perc)[1] <- "Mid_sc_perc_boot_unadj"
names(Boots_sc_unadjusted2_Secondary_perc)[2] <- "Mid_sc_perc_boot_se_unadj"
names(Boots_sc_unadjusted2_Secondary_perc)[3] <- "Mid_sc_perc_boot_unadj_low"
names(Boots_sc_unadjusted2_Secondary_perc)[4] <- "Mid_sc_perc_boot_unadj_up"

Boots_sc_unadjusted2_Universitary_perc <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 56 & Boots_sc_unadjusted2$ID  <= 84,] 
names(Boots_sc_unadjusted2_Universitary_perc)[1] <- "High_sc_perc_boot_unadj"
names(Boots_sc_unadjusted2_Universitary_perc)[2] <- "High_sc_perc_boot_se_unadj"
names(Boots_sc_unadjusted2_Universitary_perc)[3] <- "High_sc_perc_boot_unadj_low"
names(Boots_sc_unadjusted2_Universitary_perc)[4] <- "High_sc_perc_boot_unadj_up"

Boots_sc_unadjusted2_Prev_high_mid_educ_in_inactive_people <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 84 & Boots_sc_unadjusted2$ID  <= 112,] 
names(Boots_sc_unadjusted2_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_sc_in_inactive_people_boot_unadj"
names(Boots_sc_unadjusted2_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_sc_in_inactive_people_boot_se_unadj"
names(Boots_sc_unadjusted2_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_sc_in_inactive_people_boot_unadj_low"
names(Boots_sc_unadjusted2_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_sc_in_inactive_people_boot_unadj_up"

Boots_sc_unadjusted2_Prev_high_low_educ_in_inactive_people <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 112 & Boots_sc_unadjusted2$ID  <= 140,]
names(Boots_sc_unadjusted2_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_sc_in_inactive_people_boot_unadj"
names(Boots_sc_unadjusted2_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_sc_in_inactive_people_boot_se_unadj"
names(Boots_sc_unadjusted2_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_sc_in_inactive_people_boot_unadj_low"
names(Boots_sc_unadjusted2_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_sc_in_inactive_people_boot_unadj_up"

Boots_sc_unadjusted2_Prev_mid_educ_in_inactive_people <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 140 & Boots_sc_unadjusted2$ID  <= 168,]  
names(Boots_sc_unadjusted2_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_sc_in_inactive_people_boot_unadj"
names(Boots_sc_unadjusted2_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_sc_in_inactive_people_boot_se_unadj"
names(Boots_sc_unadjusted2_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_sc_in_inactive_people_boot_unadj_low"
names(Boots_sc_unadjusted2_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_sc_in_inactive_people_boot_unadj_up"

Boots_sc_unadjusted2_PFP_high_mid_educ <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 168 & Boots_sc_unadjusted2$ID  <= 196,]
names(Boots_sc_unadjusted2_PFP_high_mid_educ)[1] <- "PFP_high_mid_sc_boot_unadj"
names(Boots_sc_unadjusted2_PFP_high_mid_educ)[2] <- "PFP_high_mid_sc_boot_se_unadj"
names(Boots_sc_unadjusted2_PFP_high_mid_educ)[3] <- "PFP_high_mid_sc_boot_unadj_low"
names(Boots_sc_unadjusted2_PFP_high_mid_educ)[4] <- "PFP_high_mid_sc_boot_unadj_up"

Boots_sc_unadjusted2_PFP_high_low_educ <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 196 & Boots_sc_unadjusted2$ID  <= 224,]  
names(Boots_sc_unadjusted2_PFP_high_low_educ)[1] <- "PFP_high_low_sc_boot_unadj"
names(Boots_sc_unadjusted2_PFP_high_low_educ)[2] <- "PFP_high_low_sc_boot_se_unadj"
names(Boots_sc_unadjusted2_PFP_high_low_educ)[3] <- "PFP_high_low_sc_boot_unadj_low"
names(Boots_sc_unadjusted2_PFP_high_low_educ)[4] <- "PFP_high_low_sc_boot_unadj_up"

Boots_sc_unadjusted2_PFP_mid_educ <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 224 & Boots_sc_unadjusted2$ID  <= 252,] 
names(Boots_sc_unadjusted2_PFP_mid_educ)[1] <- "PFP_mid_sc_boot_unadj"
names(Boots_sc_unadjusted2_PFP_mid_educ)[2] <- "PFP_mid_sc_boot_se_unadj"
names(Boots_sc_unadjusted2_PFP_mid_educ)[3] <- "PFP_mid_sc_boot_unadj_low"
names(Boots_sc_unadjusted2_PFP_mid_educ)[4] <- "PFP_mid_sc_boot_unadj_up"

Boots_sc_unadjusted2_High_mid_inequality <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 252 & Boots_sc_unadjusted2$ID  <= 280,] 
names(Boots_sc_unadjusted2_High_mid_inequality)[1] <- "High_mid_inequality_sc_boot_unadj"
names(Boots_sc_unadjusted2_High_mid_inequality)[2] <- "High_mid_inequality_sc_boot_se_unadj"
names(Boots_sc_unadjusted2_High_mid_inequality)[3] <- "High_mid_inequality_sc_boot_unadj_low"
names(Boots_sc_unadjusted2_High_mid_inequality)[4] <- "High_mid_inequality_sc_boot_unadj_up"

Boots_sc_unadjusted2_High_low_inequality <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 280 & Boots_sc_unadjusted2$ID  <= 308,] 
names(Boots_sc_unadjusted2_High_low_inequality)[1] <- "High_low_inequality_sc_boot_unadj"
names(Boots_sc_unadjusted2_High_low_inequality)[2] <- "High_low_inequality_sc_boot_se_unadj"
names(Boots_sc_unadjusted2_High_low_inequality)[3] <- "High_low_inequality_sc_boot_unadj_low"
names(Boots_sc_unadjusted2_High_low_inequality)[4] <- "High_low_inequality_sc_boot_unadj_up"

Boots_sc_unadjusted2_Mid_low_inequality <- Boots_sc_unadjusted2 [ Boots_sc_unadjusted2$ID  > 308,] 
names(Boots_sc_unadjusted2_Mid_low_inequality)[1] <- "Mid_low_inequality_sc_boot_unadj"
names(Boots_sc_unadjusted2_Mid_low_inequality)[2] <- "Mid_low_inequality_sc_boot_se_unadj"
names(Boots_sc_unadjusted2_Mid_low_inequality)[3] <- "Mid_low_inequality_sc_boot_unadj_low"
names(Boots_sc_unadjusted2_Mid_low_inequality)[4] <- "Mid_low_inequality_sc_boot_unadj_up"

Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_Primary_perc, Boots_sc_unadjusted2_Secondary_perc, by  = "ID2")
Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_final, Boots_sc_unadjusted2_Universitary_perc, by  = "ID2")
Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_final, Boots_sc_unadjusted2_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_final, Boots_sc_unadjusted2_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_final, Boots_sc_unadjusted2_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_final, Boots_sc_unadjusted2_PFP_high_mid_educ, by  = "ID2")
Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_final, Boots_sc_unadjusted2_PFP_high_low_educ, by  = "ID2")
Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_final, Boots_sc_unadjusted2_PFP_mid_educ, by  = "ID2")
Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_final, Boots_sc_unadjusted2_High_mid_inequality, by  = "ID2")
Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_final, Boots_sc_unadjusted2_High_low_inequality, by  = "ID2")
Boots_sc_unadjusted2_final <- merge(Boots_sc_unadjusted2_final, Boots_sc_unadjusted2_Mid_low_inequality, by  = "ID2")
Boots_sc_unadjusted2_final <- Boots_sc_unadjusted2_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]





Boots_sc_adjusted_10 <- boot(data = eu2017, statistic = function_ocup, R = 1000, 
                               OR_mid_low = OR_SES_data[5,9], OR_high_low = OR_SES_data[4,9], OR_high_mid = OR_SES_data[6,9])


Boots_sc_adjusted_10$t0 -> Boots_sc_adjusted_10_estimates
Boots_sc_adjusted_10$t -> Boots_sc_adjusted_10_error1
std.error(Boots_sc_adjusted_10_error1) -> Boots_sc_adjusted_10_error2
Boots_sc_adjusted_102 <- data.frame(Boots_sc_adjusted_10_estimates,Boots_sc_adjusted_10_error2)
Boots_sc_adjusted_102$Boots_sc_adjusted_10_error2 <- Boots_sc_adjusted_102$Boots_sc_adjusted_10_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_sc_adjusted_10)) -> Boots_sc_adjusted_10_CI
Boots_sc_adjusted_102$Boots_sc_adjusted_10_low <- Boots_sc_adjusted_10_CI$lwr
Boots_sc_adjusted_102$Boots_sc_adjusted_10_up <- Boots_sc_adjusted_10_CI$upr

Boots_sc_adjusted_102$ID <- seq(from=1,to=336)
Boots_sc_adjusted_102$ID2 <- seq(from=1,to=28)

Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 0 & Boots_sc_adjusted_102$ID  <= 28] <- "Low_sc_perc" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 28 & Boots_sc_adjusted_102$ID  <= 56] <- "Mid_sc_perc" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 56 & Boots_sc_adjusted_102$ID  <= 84] <- "High_sc_perc" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 84 & Boots_sc_adjusted_102$ID  <= 112] <- "Prev_high_mid_sc_in_inactive_people" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 112 & Boots_sc_adjusted_102$ID  <= 140] <- "Prev_high_low_sc_in_inactive_people" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 140 & Boots_sc_adjusted_102$ID  <= 168] <- "Prev_mid_sc_in_inactive_people" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 168 & Boots_sc_adjusted_102$ID  <= 196] <- "PFP_high_mid_sc" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 196 & Boots_sc_adjusted_102$ID  <= 224] <- "PFP_high_low_sc" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 224 & Boots_sc_adjusted_102$ID  <= 252] <- "PFP_mid_sc" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 252 & Boots_sc_adjusted_102$ID  <= 280] <- "High_mid_inequality" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 280 & Boots_sc_adjusted_102$ID  <= 308] <- "High_low_inequality" 
Boots_sc_adjusted_102$variable [ Boots_sc_adjusted_102$ID  > 308] <- "Mid_low_inequality" 
Boots_sc_adjusted_102$variable <- as.factor(Boots_sc_adjusted_102$variable)

Boots_sc_adjusted_102_Primary_perc <- Boots_sc_adjusted_102[Boots_sc_adjusted_102$ID  > 0 & Boots_sc_adjusted_102$ID  <= 28,]
names(Boots_sc_adjusted_102_Primary_perc)[1] <- "Low_sc_perc_boot_10"
names(Boots_sc_adjusted_102_Primary_perc)[2] <- "Low_sc_perc_boot_se_10"
names(Boots_sc_adjusted_102_Primary_perc)[3] <- "Low_sc_perc_boot_10_low"
names(Boots_sc_adjusted_102_Primary_perc)[4] <- "Low_sc_perc_boot_10_up"

Boots_sc_adjusted_102_Secondary_perc <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 28 & Boots_sc_adjusted_102$ID  <= 56,] 
names(Boots_sc_adjusted_102_Secondary_perc)[1] <- "Mid_sc_perc_boot_10"
names(Boots_sc_adjusted_102_Secondary_perc)[2] <- "Mid_sc_perc_boot_se_10"
names(Boots_sc_adjusted_102_Secondary_perc)[3] <- "Mid_sc_perc_boot_10_low"
names(Boots_sc_adjusted_102_Secondary_perc)[4] <- "Mid_sc_perc_boot_10_up"

Boots_sc_adjusted_102_Universitary_perc <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 56 & Boots_sc_adjusted_102$ID  <= 84,] 
names(Boots_sc_adjusted_102_Universitary_perc)[1] <- "High_sc_perc_boot_10"
names(Boots_sc_adjusted_102_Universitary_perc)[2] <- "High_sc_perc_boot_se_10"
names(Boots_sc_adjusted_102_Universitary_perc)[3] <- "High_sc_perc_boot_10_low"
names(Boots_sc_adjusted_102_Universitary_perc)[4] <- "High_sc_perc_boot_10_up"

Boots_sc_adjusted_102_Prev_high_mid_educ_in_inactive_people <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 84 & Boots_sc_adjusted_102$ID  <= 112,] 
names(Boots_sc_adjusted_102_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_sc_in_inactive_people_boot_10"
names(Boots_sc_adjusted_102_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_sc_in_inactive_people_boot_se_10"
names(Boots_sc_adjusted_102_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_sc_in_inactive_people_boot_10_low"
names(Boots_sc_adjusted_102_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_sc_in_inactive_people_boot_10_up"

Boots_sc_adjusted_102_Prev_high_low_educ_in_inactive_people <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 112 & Boots_sc_adjusted_102$ID  <= 140,]
names(Boots_sc_adjusted_102_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_sc_in_inactive_people_boot_10"
names(Boots_sc_adjusted_102_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_sc_in_inactive_people_boot_se_10"
names(Boots_sc_adjusted_102_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_sc_in_inactive_people_boot_10_low"
names(Boots_sc_adjusted_102_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_sc_in_inactive_people_boot_10_up"

Boots_sc_adjusted_102_Prev_mid_educ_in_inactive_people <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 140 & Boots_sc_adjusted_102$ID  <= 168,]  
names(Boots_sc_adjusted_102_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_sc_in_inactive_people_boot_10"
names(Boots_sc_adjusted_102_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_sc_in_inactive_people_boot_se_10"
names(Boots_sc_adjusted_102_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_sc_in_inactive_people_boot_10_low"
names(Boots_sc_adjusted_102_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_sc_in_inactive_people_boot_10_up"

Boots_sc_adjusted_102_PFP_high_mid_educ <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 168 & Boots_sc_adjusted_102$ID  <= 196,]
names(Boots_sc_adjusted_102_PFP_high_mid_educ)[1] <- "PFP_high_mid_sc_boot_10"
names(Boots_sc_adjusted_102_PFP_high_mid_educ)[2] <- "PFP_high_mid_sc_boot_se_10"
names(Boots_sc_adjusted_102_PFP_high_mid_educ)[3] <- "PFP_high_mid_sc_boot_10_low"
names(Boots_sc_adjusted_102_PFP_high_mid_educ)[4] <- "PFP_high_mid_sc_boot_10_up"

Boots_sc_adjusted_102_PFP_high_low_educ <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 196 & Boots_sc_adjusted_102$ID  <= 224,]  
names(Boots_sc_adjusted_102_PFP_high_low_educ)[1] <- "PFP_high_low_sc_boot_10"
names(Boots_sc_adjusted_102_PFP_high_low_educ)[2] <- "PFP_high_low_sc_boot_se_10"
names(Boots_sc_adjusted_102_PFP_high_low_educ)[3] <- "PFP_high_low_sc_boot_10_low"
names(Boots_sc_adjusted_102_PFP_high_low_educ)[4] <- "PFP_high_low_sc_boot_10_up"

Boots_sc_adjusted_102_PFP_mid_educ <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 224 & Boots_sc_adjusted_102$ID  <= 252,] 
names(Boots_sc_adjusted_102_PFP_mid_educ)[1] <- "PFP_mid_sc_boot_10"
names(Boots_sc_adjusted_102_PFP_mid_educ)[2] <- "PFP_mid_sc_boot_se_10"
names(Boots_sc_adjusted_102_PFP_mid_educ)[3] <- "PFP_mid_sc_boot_10_low"
names(Boots_sc_adjusted_102_PFP_mid_educ)[4] <- "PFP_mid_sc_boot_10_up"

Boots_sc_adjusted_102_High_mid_inequality <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 252 & Boots_sc_adjusted_102$ID  <= 280,] 
names(Boots_sc_adjusted_102_High_mid_inequality)[1] <- "High_mid_inequality_sc_boot_10"
names(Boots_sc_adjusted_102_High_mid_inequality)[2] <- "High_mid_inequality_sc_boot_se_10"
names(Boots_sc_adjusted_102_High_mid_inequality)[3] <- "High_mid_inequality_sc_boot_10_low"
names(Boots_sc_adjusted_102_High_mid_inequality)[4] <- "High_mid_inequality_sc_boot_10_up"

Boots_sc_adjusted_102_High_low_inequality <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 280 & Boots_sc_adjusted_102$ID  <= 308,] 
names(Boots_sc_adjusted_102_High_low_inequality)[1] <- "High_low_inequality_sc_boot_10"
names(Boots_sc_adjusted_102_High_low_inequality)[2] <- "High_low_inequality_sc_boot_se_10"
names(Boots_sc_adjusted_102_High_low_inequality)[3] <- "High_low_inequality_sc_boot_10_low"
names(Boots_sc_adjusted_102_High_low_inequality)[4] <- "High_low_inequality_sc_boot_10_up"

Boots_sc_adjusted_102_Mid_low_inequality <- Boots_sc_adjusted_102 [ Boots_sc_adjusted_102$ID  > 308,] 
names(Boots_sc_adjusted_102_Mid_low_inequality)[1] <- "Mid_low_inequality_sc_boot_10"
names(Boots_sc_adjusted_102_Mid_low_inequality)[2] <- "Mid_low_inequality_sc_boot_se_10"
names(Boots_sc_adjusted_102_Mid_low_inequality)[3] <- "Mid_low_inequality_sc_boot_10_low"
names(Boots_sc_adjusted_102_Mid_low_inequality)[4] <- "Mid_low_inequality_sc_boot_10_up"

Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_Primary_perc, Boots_sc_adjusted_102_Secondary_perc, by  = "ID2")
Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_final, Boots_sc_adjusted_102_Universitary_perc, by  = "ID2")
Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_final, Boots_sc_adjusted_102_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_final, Boots_sc_adjusted_102_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_final, Boots_sc_adjusted_102_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_final, Boots_sc_adjusted_102_PFP_high_mid_educ, by  = "ID2")
Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_final, Boots_sc_adjusted_102_PFP_high_low_educ, by  = "ID2")
Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_final, Boots_sc_adjusted_102_PFP_mid_educ, by  = "ID2")
Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_final, Boots_sc_adjusted_102_High_mid_inequality, by  = "ID2")
Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_final, Boots_sc_adjusted_102_High_low_inequality, by  = "ID2")
Boots_sc_adjusted_102_final <- merge(Boots_sc_adjusted_102_final, Boots_sc_adjusted_102_Mid_low_inequality, by  = "ID2")
Boots_sc_adjusted_102_final <- Boots_sc_adjusted_102_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]




Boots_sc_adjusted_20 <- boot(data = eu2017, statistic = function_ocup, R = 1000, 
                               OR_mid_low = OR_SES_data[5,10], OR_high_low = OR_SES_data[4,10], OR_high_mid = OR_SES_data[6,10])



Boots_sc_adjusted_20$t0 -> Boots_sc_adjusted_20_estimates
Boots_sc_adjusted_20$t -> Boots_sc_adjusted_20_error1
std.error(Boots_sc_adjusted_20_error1) -> Boots_sc_adjusted_20_error2
Boots_sc_adjusted_202 <- data.frame(Boots_sc_adjusted_20_estimates,Boots_sc_adjusted_20_error2)
Boots_sc_adjusted_202$Boots_sc_adjusted_20_error2 <- Boots_sc_adjusted_202$Boots_sc_adjusted_20_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_sc_adjusted_20)) -> Boots_sc_adjusted_20_CI
Boots_sc_adjusted_202$Boots_sc_adjusted_20_low <- Boots_sc_adjusted_20_CI$lwr
Boots_sc_adjusted_202$Boots_sc_adjusted_20_up <- Boots_sc_adjusted_20_CI$upr

Boots_sc_adjusted_202$ID <- seq(from=1,to=336)
Boots_sc_adjusted_202$ID2 <- seq(from=1,to=28)

Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 0 & Boots_sc_adjusted_202$ID  <= 28] <- "Low_sc_perc" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 28 & Boots_sc_adjusted_202$ID  <= 56] <- "Mid_sc_perc" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 56 & Boots_sc_adjusted_202$ID  <= 84] <- "High_sc_perc" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 84 & Boots_sc_adjusted_202$ID  <= 112] <- "Prev_high_mid_sc_in_inactive_people" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 112 & Boots_sc_adjusted_202$ID  <= 140] <- "Prev_high_low_sc_in_inactive_people" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 140 & Boots_sc_adjusted_202$ID  <= 168] <- "Prev_mid_sc_in_inactive_people" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 168 & Boots_sc_adjusted_202$ID  <= 196] <- "PFP_high_mid_sc" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 196 & Boots_sc_adjusted_202$ID  <= 224] <- "PFP_high_low_sc" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 224 & Boots_sc_adjusted_202$ID  <= 252] <- "PFP_mid_sc" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 252 & Boots_sc_adjusted_202$ID  <= 280] <- "High_mid_inequality" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 280 & Boots_sc_adjusted_202$ID  <= 308] <- "High_low_inequality" 
Boots_sc_adjusted_202$variable [ Boots_sc_adjusted_202$ID  > 308] <- "Mid_low_inequality" 
Boots_sc_adjusted_202$variable <- as.factor(Boots_sc_adjusted_202$variable)

Boots_sc_adjusted_202_Primary_perc <- Boots_sc_adjusted_202[Boots_sc_adjusted_202$ID  > 0 & Boots_sc_adjusted_202$ID  <= 28,]
names(Boots_sc_adjusted_202_Primary_perc)[1] <- "Low_sc_perc_boot_20"
names(Boots_sc_adjusted_202_Primary_perc)[2] <- "Low_sc_perc_boot_se_20"
names(Boots_sc_adjusted_202_Primary_perc)[3] <- "Low_sc_perc_boot_20_low"
names(Boots_sc_adjusted_202_Primary_perc)[4] <- "Low_sc_perc_boot_20_up"

Boots_sc_adjusted_202_Secondary_perc <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 28 & Boots_sc_adjusted_202$ID  <= 56,] 
names(Boots_sc_adjusted_202_Secondary_perc)[1] <- "Mid_sc_perc_boot_20"
names(Boots_sc_adjusted_202_Secondary_perc)[2] <- "Mid_sc_perc_boot_se_20"
names(Boots_sc_adjusted_202_Secondary_perc)[3] <- "Mid_sc_perc_boot_20_low"
names(Boots_sc_adjusted_202_Secondary_perc)[4] <- "Mid_sc_perc_boot_20_up"

Boots_sc_adjusted_202_Universitary_perc <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 56 & Boots_sc_adjusted_202$ID  <= 84,] 
names(Boots_sc_adjusted_202_Universitary_perc)[1] <- "High_sc_perc_boot_20"
names(Boots_sc_adjusted_202_Universitary_perc)[2] <- "High_sc_perc_boot_se_20"
names(Boots_sc_adjusted_202_Universitary_perc)[3] <- "High_sc_perc_boot_20_low"
names(Boots_sc_adjusted_202_Universitary_perc)[4] <- "High_sc_perc_boot_20_up"

Boots_sc_adjusted_202_Prev_high_mid_educ_in_inactive_people <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 84 & Boots_sc_adjusted_202$ID  <= 112,] 
names(Boots_sc_adjusted_202_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_sc_in_inactive_people_boot_20"
names(Boots_sc_adjusted_202_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_sc_in_inactive_people_boot_se_20"
names(Boots_sc_adjusted_202_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_sc_in_inactive_people_boot_20_low"
names(Boots_sc_adjusted_202_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_sc_in_inactive_people_boot_20_up"

Boots_sc_adjusted_202_Prev_high_low_educ_in_inactive_people <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 112 & Boots_sc_adjusted_202$ID  <= 140,]
names(Boots_sc_adjusted_202_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_sc_in_inactive_people_boot_20"
names(Boots_sc_adjusted_202_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_sc_in_inactive_people_boot_se_20"
names(Boots_sc_adjusted_202_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_sc_in_inactive_people_boot_20_low"
names(Boots_sc_adjusted_202_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_sc_in_inactive_people_boot_20_up"

Boots_sc_adjusted_202_Prev_mid_educ_in_inactive_people <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 140 & Boots_sc_adjusted_202$ID  <= 168,]  
names(Boots_sc_adjusted_202_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_sc_in_inactive_people_boot_20"
names(Boots_sc_adjusted_202_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_sc_in_inactive_people_boot_se_20"
names(Boots_sc_adjusted_202_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_sc_in_inactive_people_boot_20_low"
names(Boots_sc_adjusted_202_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_sc_in_inactive_people_boot_20_up"

Boots_sc_adjusted_202_PFP_high_mid_educ <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 168 & Boots_sc_adjusted_202$ID  <= 196,]
names(Boots_sc_adjusted_202_PFP_high_mid_educ)[1] <- "PFP_high_mid_sc_boot_20"
names(Boots_sc_adjusted_202_PFP_high_mid_educ)[2] <- "PFP_high_mid_sc_boot_se_20"
names(Boots_sc_adjusted_202_PFP_high_mid_educ)[3] <- "PFP_high_mid_sc_boot_20_low"
names(Boots_sc_adjusted_202_PFP_high_mid_educ)[4] <- "PFP_high_mid_sc_boot_20_up"

Boots_sc_adjusted_202_PFP_high_low_educ <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 196 & Boots_sc_adjusted_202$ID  <= 224,]  
names(Boots_sc_adjusted_202_PFP_high_low_educ)[1] <- "PFP_high_low_sc_boot_20"
names(Boots_sc_adjusted_202_PFP_high_low_educ)[2] <- "PFP_high_low_sc_boot_se_20"
names(Boots_sc_adjusted_202_PFP_high_low_educ)[3] <- "PFP_high_low_sc_boot_20_low"
names(Boots_sc_adjusted_202_PFP_high_low_educ)[4] <- "PFP_high_low_sc_boot_20_up"

Boots_sc_adjusted_202_PFP_mid_educ <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 224 & Boots_sc_adjusted_202$ID  <= 252,] 
names(Boots_sc_adjusted_202_PFP_mid_educ)[1] <- "PFP_mid_sc_boot_20"
names(Boots_sc_adjusted_202_PFP_mid_educ)[2] <- "PFP_mid_sc_boot_se_20"
names(Boots_sc_adjusted_202_PFP_mid_educ)[3] <- "PFP_mid_sc_boot_20_low"
names(Boots_sc_adjusted_202_PFP_mid_educ)[4] <- "PFP_mid_sc_boot_20_up"

Boots_sc_adjusted_202_High_mid_inequality <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 252 & Boots_sc_adjusted_202$ID  <= 280,] 
names(Boots_sc_adjusted_202_High_mid_inequality)[1] <- "High_mid_inequality_sc_boot_20"
names(Boots_sc_adjusted_202_High_mid_inequality)[2] <- "High_mid_inequality_sc_boot_se_20"
names(Boots_sc_adjusted_202_High_mid_inequality)[3] <- "High_mid_inequality_sc_boot_20_low"
names(Boots_sc_adjusted_202_High_mid_inequality)[4] <- "High_mid_inequality_sc_boot_20_up"

Boots_sc_adjusted_202_High_low_inequality <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 280 & Boots_sc_adjusted_202$ID  <= 308,] 
names(Boots_sc_adjusted_202_High_low_inequality)[1] <- "High_low_inequality_sc_boot_20"
names(Boots_sc_adjusted_202_High_low_inequality)[2] <- "High_low_inequality_sc_boot_se_20"
names(Boots_sc_adjusted_202_High_low_inequality)[3] <- "High_low_inequality_sc_boot_20_low"
names(Boots_sc_adjusted_202_High_low_inequality)[4] <- "High_low_inequality_sc_boot_20_up"

Boots_sc_adjusted_202_Mid_low_inequality <- Boots_sc_adjusted_202 [ Boots_sc_adjusted_202$ID  > 308,] 
names(Boots_sc_adjusted_202_Mid_low_inequality)[1] <- "Mid_low_inequality_sc_boot_20"
names(Boots_sc_adjusted_202_Mid_low_inequality)[2] <- "Mid_low_inequality_sc_boot_se_20"
names(Boots_sc_adjusted_202_Mid_low_inequality)[3] <- "Mid_low_inequality_sc_boot_20_low"
names(Boots_sc_adjusted_202_Mid_low_inequality)[4] <- "Mid_low_inequality_sc_boot_20_up"

Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_Primary_perc, Boots_sc_adjusted_202_Secondary_perc, by  = "ID2")
Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_final, Boots_sc_adjusted_202_Universitary_perc, by  = "ID2")
Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_final, Boots_sc_adjusted_202_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_final, Boots_sc_adjusted_202_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_final, Boots_sc_adjusted_202_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_final, Boots_sc_adjusted_202_PFP_high_mid_educ, by  = "ID2")
Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_final, Boots_sc_adjusted_202_PFP_high_low_educ, by  = "ID2")
Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_final, Boots_sc_adjusted_202_PFP_mid_educ, by  = "ID2")
Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_final, Boots_sc_adjusted_202_High_mid_inequality, by  = "ID2")
Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_final, Boots_sc_adjusted_202_High_low_inequality, by  = "ID2")
Boots_sc_adjusted_202_final <- merge(Boots_sc_adjusted_202_final, Boots_sc_adjusted_202_Mid_low_inequality, by  = "ID2")
Boots_sc_adjusted_202_final <- Boots_sc_adjusted_202_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]



Boots_sc_adjusted_50 <- boot(data = eu2017, statistic = function_ocup, R = 1000, 
                               OR_mid_low = OR_SES_data[5,11], OR_high_low = OR_SES_data[4,11], OR_high_mid = OR_SES_data[6,11])



Boots_sc_adjusted_50$t0 -> Boots_sc_adjusted_50_estimates
Boots_sc_adjusted_50$t -> Boots_sc_adjusted_50_error1
std.error(Boots_sc_adjusted_50_error1) -> Boots_sc_adjusted_50_error2
Boots_sc_adjusted_502 <- data.frame(Boots_sc_adjusted_50_estimates,Boots_sc_adjusted_50_error2)
Boots_sc_adjusted_502$Boots_sc_adjusted_50_error2 <- Boots_sc_adjusted_502$Boots_sc_adjusted_50_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_sc_adjusted_50)) -> Boots_sc_adjusted_50_CI
Boots_sc_adjusted_502$Boots_sc_adjusted_50_low <- Boots_sc_adjusted_50_CI$lwr
Boots_sc_adjusted_502$Boots_sc_adjusted_50_up <- Boots_sc_adjusted_50_CI$upr

Boots_sc_adjusted_502$ID <- seq(from=1,to=336)
Boots_sc_adjusted_502$ID2 <- seq(from=1,to=28)

Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 0 & Boots_sc_adjusted_502$ID  <= 28] <- "Low_sc_perc" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 28 & Boots_sc_adjusted_502$ID  <= 56] <- "Mid_sc_perc" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 56 & Boots_sc_adjusted_502$ID  <= 84] <- "High_sc_perc" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 84 & Boots_sc_adjusted_502$ID  <= 112] <- "Prev_high_mid_sc_in_inactive_people" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 112 & Boots_sc_adjusted_502$ID  <= 140] <- "Prev_high_low_sc_in_inactive_people" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 140 & Boots_sc_adjusted_502$ID  <= 168] <- "Prev_mid_sc_in_inactive_people" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 168 & Boots_sc_adjusted_502$ID  <= 196] <- "PFP_high_mid_sc" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 196 & Boots_sc_adjusted_502$ID  <= 224] <- "PFP_high_low_sc" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 224 & Boots_sc_adjusted_502$ID  <= 252] <- "PFP_mid_sc" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 252 & Boots_sc_adjusted_502$ID  <= 280] <- "High_mid_inequality" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 280 & Boots_sc_adjusted_502$ID  <= 308] <- "High_low_inequality" 
Boots_sc_adjusted_502$variable [ Boots_sc_adjusted_502$ID  > 308] <- "Mid_low_inequality" 
Boots_sc_adjusted_502$variable <- as.factor(Boots_sc_adjusted_502$variable)

Boots_sc_adjusted_502_Primary_perc <- Boots_sc_adjusted_502[Boots_sc_adjusted_502$ID  > 0 & Boots_sc_adjusted_502$ID  <= 28,]
names(Boots_sc_adjusted_502_Primary_perc)[1] <- "Low_sc_perc_boot_50"
names(Boots_sc_adjusted_502_Primary_perc)[2] <- "Low_sc_perc_boot_se_50"
names(Boots_sc_adjusted_502_Primary_perc)[3] <- "Low_sc_perc_boot_50_low"
names(Boots_sc_adjusted_502_Primary_perc)[4] <- "Low_sc_perc_boot_50_up"

Boots_sc_adjusted_502_Secondary_perc <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 28 & Boots_sc_adjusted_502$ID  <= 56,] 
names(Boots_sc_adjusted_502_Secondary_perc)[1] <- "Mid_sc_perc_boot_50"
names(Boots_sc_adjusted_502_Secondary_perc)[2] <- "Mid_sc_perc_boot_se_50"
names(Boots_sc_adjusted_502_Secondary_perc)[3] <- "Mid_sc_perc_boot_50_low"
names(Boots_sc_adjusted_502_Secondary_perc)[4] <- "Mid_sc_perc_boot_50_up"

Boots_sc_adjusted_502_Universitary_perc <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 56 & Boots_sc_adjusted_502$ID  <= 84,] 
names(Boots_sc_adjusted_502_Universitary_perc)[1] <- "High_sc_perc_boot_50"
names(Boots_sc_adjusted_502_Universitary_perc)[2] <- "High_sc_perc_boot_se_50"
names(Boots_sc_adjusted_502_Universitary_perc)[3] <- "High_sc_perc_boot_50_low"
names(Boots_sc_adjusted_502_Universitary_perc)[4] <- "High_sc_perc_boot_50_up"

Boots_sc_adjusted_502_Prev_high_mid_educ_in_inactive_people <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 84 & Boots_sc_adjusted_502$ID  <= 112,] 
names(Boots_sc_adjusted_502_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_sc_in_inactive_people_boot_50"
names(Boots_sc_adjusted_502_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_sc_in_inactive_people_boot_se_50"
names(Boots_sc_adjusted_502_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_sc_in_inactive_people_boot_50_low"
names(Boots_sc_adjusted_502_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_sc_in_inactive_people_boot_50_up"

Boots_sc_adjusted_502_Prev_high_low_educ_in_inactive_people <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 112 & Boots_sc_adjusted_502$ID  <= 140,]
names(Boots_sc_adjusted_502_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_sc_in_inactive_people_boot_50"
names(Boots_sc_adjusted_502_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_sc_in_inactive_people_boot_se_50"
names(Boots_sc_adjusted_502_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_sc_in_inactive_people_boot_50_low"
names(Boots_sc_adjusted_502_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_sc_in_inactive_people_boot_50_up"

Boots_sc_adjusted_502_Prev_mid_educ_in_inactive_people <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 140 & Boots_sc_adjusted_502$ID  <= 168,]  
names(Boots_sc_adjusted_502_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_sc_in_inactive_people_boot_50"
names(Boots_sc_adjusted_502_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_sc_in_inactive_people_boot_se_50"
names(Boots_sc_adjusted_502_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_sc_in_inactive_people_boot_50_low"
names(Boots_sc_adjusted_502_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_sc_in_inactive_people_boot_50_up"

Boots_sc_adjusted_502_PFP_high_mid_educ <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 168 & Boots_sc_adjusted_502$ID  <= 196,]
names(Boots_sc_adjusted_502_PFP_high_mid_educ)[1] <- "PFP_high_mid_sc_boot_50"
names(Boots_sc_adjusted_502_PFP_high_mid_educ)[2] <- "PFP_high_mid_sc_boot_se_50"
names(Boots_sc_adjusted_502_PFP_high_mid_educ)[3] <- "PFP_high_mid_sc_boot_50_low"
names(Boots_sc_adjusted_502_PFP_high_mid_educ)[4] <- "PFP_high_mid_sc_boot_50_up"

Boots_sc_adjusted_502_PFP_high_low_educ <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 196 & Boots_sc_adjusted_502$ID  <= 224,]  
names(Boots_sc_adjusted_502_PFP_high_low_educ)[1] <- "PFP_high_low_sc_boot_50"
names(Boots_sc_adjusted_502_PFP_high_low_educ)[2] <- "PFP_high_low_sc_boot_se_50"
names(Boots_sc_adjusted_502_PFP_high_low_educ)[3] <- "PFP_high_low_sc_boot_50_low"
names(Boots_sc_adjusted_502_PFP_high_low_educ)[4] <- "PFP_high_low_sc_boot_50_up"

Boots_sc_adjusted_502_PFP_mid_educ <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 224 & Boots_sc_adjusted_502$ID  <= 252,] 
names(Boots_sc_adjusted_502_PFP_mid_educ)[1] <- "PFP_mid_sc_boot_50"
names(Boots_sc_adjusted_502_PFP_mid_educ)[2] <- "PFP_mid_sc_boot_se_50"
names(Boots_sc_adjusted_502_PFP_mid_educ)[3] <- "PFP_mid_sc_boot_50_low"
names(Boots_sc_adjusted_502_PFP_mid_educ)[4] <- "PFP_mid_sc_boot_50_up"

Boots_sc_adjusted_502_High_mid_inequality <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 252 & Boots_sc_adjusted_502$ID  <= 280,] 
names(Boots_sc_adjusted_502_High_mid_inequality)[1] <- "High_mid_inequality_sc_boot_50"
names(Boots_sc_adjusted_502_High_mid_inequality)[2] <- "High_mid_inequality_sc_boot_se_50"
names(Boots_sc_adjusted_502_High_mid_inequality)[3] <- "High_mid_inequality_sc_boot_50_low"
names(Boots_sc_adjusted_502_High_mid_inequality)[4] <- "High_mid_inequality_sc_boot_50_up"

Boots_sc_adjusted_502_High_low_inequality <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 280 & Boots_sc_adjusted_502$ID  <= 308,] 
names(Boots_sc_adjusted_502_High_low_inequality)[1] <- "High_low_inequality_sc_boot_50"
names(Boots_sc_adjusted_502_High_low_inequality)[2] <- "High_low_inequality_sc_boot_se_50"
names(Boots_sc_adjusted_502_High_low_inequality)[3] <- "High_low_inequality_sc_boot_50_low"
names(Boots_sc_adjusted_502_High_low_inequality)[4] <- "High_low_inequality_sc_boot_50_up"

Boots_sc_adjusted_502_Mid_low_inequality <- Boots_sc_adjusted_502 [ Boots_sc_adjusted_502$ID  > 308,] 
names(Boots_sc_adjusted_502_Mid_low_inequality)[1] <- "Mid_low_inequality_sc_boot_50"
names(Boots_sc_adjusted_502_Mid_low_inequality)[2] <- "Mid_low_inequality_sc_boot_se_50"
names(Boots_sc_adjusted_502_Mid_low_inequality)[3] <- "Mid_low_inequality_sc_boot_50_low"
names(Boots_sc_adjusted_502_Mid_low_inequality)[4] <- "Mid_low_inequality_sc_boot_50_up"

Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_Primary_perc, Boots_sc_adjusted_502_Secondary_perc, by  = "ID2")
Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_final, Boots_sc_adjusted_502_Universitary_perc, by  = "ID2")
Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_final, Boots_sc_adjusted_502_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_final, Boots_sc_adjusted_502_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_final, Boots_sc_adjusted_502_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_final, Boots_sc_adjusted_502_PFP_high_mid_educ, by  = "ID2")
Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_final, Boots_sc_adjusted_502_PFP_high_low_educ, by  = "ID2")
Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_final, Boots_sc_adjusted_502_PFP_mid_educ, by  = "ID2")
Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_final, Boots_sc_adjusted_502_High_mid_inequality, by  = "ID2")
Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_final, Boots_sc_adjusted_502_High_low_inequality, by  = "ID2")
Boots_sc_adjusted_502_final <- merge(Boots_sc_adjusted_502_final, Boots_sc_adjusted_502_Mid_low_inequality, by  = "ID2")
Boots_sc_adjusted_502_final <- Boots_sc_adjusted_502_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]





function_bills <- function(data, i, OR_mid_low, OR_high_low, OR_high_mid) {
    data[i,] %>% group_by(Country_rec) %>% summarise(Low_income = sum(Bills == "Most of the time", na.rm = T),
                                                   Mid_income = sum(Bills == "From time to time", na.rm = T),
                                                   High_income = sum(Bills == "Almost never/never", na.rm = T)) -> data_income
    data_income$n <- data_income$Low_income + data_income$Mid_income + data_income$High_income
    data_income$Low_income_perc <- (data_income$Low_income * 100)/ data_income$n
    data_income$Mid_income_perc <- (data_income$Mid_income * 100)/ data_income$n
    data_income$High_income_perc <- (data_income$High_income * 100)/ data_income$n
    
    data_income$High_mid_inequality <- data_income$High_income_perc - data_income$Mid_income_perc
    data_income$High_low_inequality <- data_income$High_income_perc - data_income$Low_income_perc
    data_income$Mid_low_inequality <- data_income$Mid_income_perc - data_income$Low_income_perc
    
    data_income$Prev_high_mid_income_in_inactive_people <- (data_income$High_income_perc * OR_high_mid)/((data_income$High_income_perc * OR_high_mid)+(1 - data_income$High_income_perc))
    data_income$Prev_high_low_income_in_inactive_people <- (data_income$High_income_perc * OR_high_low)/((data_income$High_income_perc * OR_high_low)+(1 - data_income$High_income_perc))
    data_income$Prev_mid_income_in_inactive_people <- (data_income$Mid_income_perc * OR_mid_low)/((data_income$Mid_income_perc * OR_mid_low)+(1 - data_income$Mid_income_perc))
    
    data_income$PFP_high_mid_income <- (data_income$Prev_high_mid_income_in_inactive_people * (1 - OR_high_mid))/(1 - (1 - OR_high_mid)*(1 - data_income$Prev_high_mid_income_in_inactive_people))
    data_income$PFP_high_low_income <- (data_income$Prev_high_low_income_in_inactive_people * (1 - OR_high_low))/(1 - (1 - OR_high_low)*(1 - data_income$Prev_high_low_income_in_inactive_people))
    data_income$PFP_mid_income <- (data_income$Prev_mid_income_in_inactive_people * (1 - OR_mid_low))/(1 - (1 - OR_mid_low)*(1 - data_income$Prev_mid_income_in_inactive_people))
    
    return(c(data_income$Low_income_perc, data_income$Mid_income_perc, data_income$High_income_perc, data_income$Prev_high_mid_income_in_inactive_people,
                      data_income$Prev_high_low_income_in_inactive_people, data_income$Prev_mid_income_in_inactive_people, data_income$PFP_high_mid_income, 
                      data_income$PFP_high_low_income, data_income$PFP_mid_income, data_income$High_mid_inequality, data_income$High_low_inequality, 
                      data_income$Mid_low_inequality))
}


Boots_bills_adjusted <- boot(data = eu2017, statistic = function_bills, R = 1000, 
                          OR_mid_low = OR_SES_data[7,6], OR_high_low = OR_SES_data[8,6], OR_high_mid = OR_SES_data[9,6])

Boots_bills_adjusted$t0 -> Boots_bills_adjusted_estimates
Boots_bills_adjusted$t -> Boots_bills_adjusted_error1
std.error(Boots_bills_adjusted_error1) -> Boots_bills_adjusted_error2
Boots_bills_adjusted2 <- data.frame(Boots_bills_adjusted_estimates,Boots_bills_adjusted_error2)
Boots_bills_adjusted2$Boots_bills_adjusted_error2 <- Boots_bills_adjusted2$Boots_bills_adjusted_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_bills_adjusted)) -> Boots_bills_adjusted_CI
Boots_bills_adjusted2$Boots_bills_adjusted_low <- Boots_bills_adjusted_CI$lwr
Boots_bills_adjusted2$Boots_bills_adjusted_up <- Boots_bills_adjusted_CI$upr

Boots_bills_adjusted2$ID <- seq(from=1,to=336)
Boots_bills_adjusted2$ID2 <- seq(from=1,to=28)

Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 0 & Boots_bills_adjusted2$ID  <= 28] <- "Low_income_perc" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 28 & Boots_bills_adjusted2$ID  <= 56] <- "Mid_income_perc" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 56 & Boots_bills_adjusted2$ID  <= 84] <- "High_income_perc" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 84 & Boots_bills_adjusted2$ID  <= 112] <- "Prev_high_mid_income_in_inactive_people" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 112 & Boots_bills_adjusted2$ID  <= 140] <- "Prev_high_low_income_in_inactive_people" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 140 & Boots_bills_adjusted2$ID  <= 168] <- "Prev_mid_income_in_inactive_people" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 168 & Boots_bills_adjusted2$ID  <= 196] <- "PFP_high_mid_income" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 196 & Boots_bills_adjusted2$ID  <= 224] <- "PFP_high_low_income" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 224 & Boots_bills_adjusted2$ID  <= 252] <- "PFP_mid_income" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 252 & Boots_bills_adjusted2$ID  <= 280] <- "High_mid_inequality" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 280 & Boots_bills_adjusted2$ID  <= 308] <- "High_low_inequality" 
Boots_bills_adjusted2$variable [ Boots_bills_adjusted2$ID  > 308] <- "Mid_low_inequality" 
Boots_bills_adjusted2$variable <- as.factor(Boots_bills_adjusted2$variable)

Boots_bills_adjusted2_Primary_perc <- Boots_bills_adjusted2[Boots_bills_adjusted2$ID  > 0 & Boots_bills_adjusted2$ID  <= 28,]
names(Boots_bills_adjusted2_Primary_perc)[1] <- "Low_income_perc_boot"
names(Boots_bills_adjusted2_Primary_perc)[2] <- "Low_income_perc_boot_se"
names(Boots_bills_adjusted2_Primary_perc)[3] <- "Low_income_perc_boot_low"
names(Boots_bills_adjusted2_Primary_perc)[4] <- "Low_income_perc_boot_up"

Boots_bills_adjusted2_Secondary_perc <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 28 & Boots_bills_adjusted2$ID  <= 56,] 
names(Boots_bills_adjusted2_Secondary_perc)[1] <- "Mid_income_perc_boot"
names(Boots_bills_adjusted2_Secondary_perc)[2] <- "Mid_income_perc_boot_se"
names(Boots_bills_adjusted2_Secondary_perc)[3] <- "Mid_income_perc_boot_low"
names(Boots_bills_adjusted2_Secondary_perc)[4] <- "Mid_income_perc_boot_up"

Boots_bills_adjusted2_Universitary_perc <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 56 & Boots_bills_adjusted2$ID  <= 84,] 
names(Boots_bills_adjusted2_Universitary_perc)[1] <- "High_income_perc_boot"
names(Boots_bills_adjusted2_Universitary_perc)[2] <- "High_income_perc_boot_se"
names(Boots_bills_adjusted2_Universitary_perc)[3] <- "High_income_perc_boot_low"
names(Boots_bills_adjusted2_Universitary_perc)[4] <- "High_income_perc_boot_up"

Boots_bills_adjusted2_Prev_high_mid_educ_in_inactive_people <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 84 & Boots_bills_adjusted2$ID  <= 112,] 
names(Boots_bills_adjusted2_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_income_in_inactive_people_boot"
names(Boots_bills_adjusted2_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_income_in_inactive_people_boot_se"
names(Boots_bills_adjusted2_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_income_in_inactive_people_boot_low"
names(Boots_bills_adjusted2_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_income_in_inactive_people_boot_up"

Boots_bills_adjusted2_Prev_high_low_educ_in_inactive_people <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 112 & Boots_bills_adjusted2$ID  <= 140,]
names(Boots_bills_adjusted2_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_income_in_inactive_people_boot"
names(Boots_bills_adjusted2_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_income_in_inactive_people_boot_se"
names(Boots_bills_adjusted2_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_income_in_inactive_people_boot_low"
names(Boots_bills_adjusted2_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_income_in_inactive_people_boot_up"

Boots_bills_adjusted2_Prev_mid_educ_in_inactive_people <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 140 & Boots_bills_adjusted2$ID  <= 168,]  
names(Boots_bills_adjusted2_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_income_in_inactive_people_boot"
names(Boots_bills_adjusted2_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_income_in_inactive_people_boot_se"
names(Boots_bills_adjusted2_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_income_in_inactive_people_boot_low"
names(Boots_bills_adjusted2_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_income_in_inactive_people_boot_up"

Boots_bills_adjusted2_PFP_high_mid_educ <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 168 & Boots_bills_adjusted2$ID  <= 196,]
names(Boots_bills_adjusted2_PFP_high_mid_educ)[1] <- "PFP_high_mid_income_boot"
names(Boots_bills_adjusted2_PFP_high_mid_educ)[2] <- "PFP_high_mid_income_boot_se"
names(Boots_bills_adjusted2_PFP_high_mid_educ)[3] <- "PFP_high_mid_income_boot_low"
names(Boots_bills_adjusted2_PFP_high_mid_educ)[4] <- "PFP_high_mid_income_boot_up"

Boots_bills_adjusted2_PFP_high_low_educ <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 196 & Boots_bills_adjusted2$ID  <= 224,]  
names(Boots_bills_adjusted2_PFP_high_low_educ)[1] <- "PFP_high_low_income_boot"
names(Boots_bills_adjusted2_PFP_high_low_educ)[2] <- "PFP_high_low_income_boot_se"
names(Boots_bills_adjusted2_PFP_high_low_educ)[3] <- "PFP_high_low_income_boot_low"
names(Boots_bills_adjusted2_PFP_high_low_educ)[4] <- "PFP_high_low_income_boot_up"

Boots_bills_adjusted2_PFP_mid_educ <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 224 & Boots_bills_adjusted2$ID  <= 252,] 
names(Boots_bills_adjusted2_PFP_mid_educ)[1] <- "PFP_mid_income_boot"
names(Boots_bills_adjusted2_PFP_mid_educ)[2] <- "PFP_mid_income_boot_se"
names(Boots_bills_adjusted2_PFP_mid_educ)[3] <- "PFP_mid_income_boot_low"
names(Boots_bills_adjusted2_PFP_mid_educ)[4] <- "PFP_mid_income_boot_up"

Boots_bills_adjusted2_High_mid_inequality <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 252 & Boots_bills_adjusted2$ID  <= 280,] 
names(Boots_bills_adjusted2_High_mid_inequality)[1] <- "High_mid_inequality_income_boot"
names(Boots_bills_adjusted2_High_mid_inequality)[2] <- "High_mid_inequality_income_boot_se"
names(Boots_bills_adjusted2_High_mid_inequality)[3] <- "High_mid_inequality_income_boot_low"
names(Boots_bills_adjusted2_High_mid_inequality)[4] <- "High_mid_inequality_income_boot_up"

Boots_bills_adjusted2_High_low_inequality <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 280 & Boots_bills_adjusted2$ID  <= 308,] 
names(Boots_bills_adjusted2_High_low_inequality)[1] <- "High_low_inequality_income_boot"
names(Boots_bills_adjusted2_High_low_inequality)[2] <- "High_low_inequality_income_boot_se"
names(Boots_bills_adjusted2_High_low_inequality)[3] <- "High_low_inequality_income_boot_low"
names(Boots_bills_adjusted2_High_low_inequality)[4] <- "High_low_inequality_income_boot_up"

Boots_bills_adjusted2_Mid_low_inequality <- Boots_bills_adjusted2 [ Boots_bills_adjusted2$ID  > 308,] 
names(Boots_bills_adjusted2_Mid_low_inequality)[1] <- "Mid_low_inequality_income_boot"
names(Boots_bills_adjusted2_Mid_low_inequality)[2] <- "Mid_low_inequality_income_boot_se"
names(Boots_bills_adjusted2_Mid_low_inequality)[3] <- "Mid_low_inequality_income_boot_low"
names(Boots_bills_adjusted2_Mid_low_inequality)[4] <- "Mid_low_inequality_income_boot_up"

Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_Primary_perc, Boots_bills_adjusted2_Secondary_perc, by  = "ID2")
Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_final, Boots_bills_adjusted2_Universitary_perc, by  = "ID2")
Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_final, Boots_bills_adjusted2_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_final, Boots_bills_adjusted2_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_final, Boots_bills_adjusted2_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_final, Boots_bills_adjusted2_PFP_high_mid_educ, by  = "ID2")
Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_final, Boots_bills_adjusted2_PFP_high_low_educ, by  = "ID2")
Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_final, Boots_bills_adjusted2_PFP_mid_educ, by  = "ID2")
Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_final, Boots_bills_adjusted2_High_mid_inequality, by  = "ID2")
Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_final, Boots_bills_adjusted2_High_low_inequality, by  = "ID2")
Boots_bills_adjusted2_final <- merge(Boots_bills_adjusted2_final, Boots_bills_adjusted2_Mid_low_inequality, by  = "ID2")
Boots_bills_adjusted2_final <- Boots_bills_adjusted2_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]





Boots_bills_unadjusted <- boot(data = eu2017, statistic = function_bills, R = 1000, 
                            OR_mid_low = OR_SES_data[7,3], OR_high_low = OR_SES_data[8,3], OR_high_mid = OR_SES_data[9,3])

Boots_bills_unadjusted$t0 -> Boots_bills_unadjusted_estimates
Boots_bills_unadjusted$t -> Boots_bills_unadjusted_error1
std.error(Boots_bills_unadjusted_error1) -> Boots_bills_unadjusted_error2
Boots_bills_unadjusted2 <- data.frame(Boots_bills_unadjusted_estimates,Boots_bills_unadjusted_error2)
Boots_bills_unadjusted2$Boots_bills_unadjusted_error2 <- Boots_bills_unadjusted2$Boots_bills_unadjusted_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_bills_unadjusted)) -> Boots_bills_unadjusted_CI
Boots_bills_unadjusted2$Boots_bills_unadjusted_low <- Boots_bills_unadjusted_CI$lwr
Boots_bills_unadjusted2$Boots_bills_unadjusted_up <- Boots_bills_unadjusted_CI$upr

Boots_bills_unadjusted2$ID <- seq(from=1,to=336)
Boots_bills_unadjusted2$ID2 <- seq(from=1,to=28)

Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 0 & Boots_bills_unadjusted2$ID  <= 28] <- "Low_income_perc" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 28 & Boots_bills_unadjusted2$ID  <= 56] <- "Mid_income_perc" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 56 & Boots_bills_unadjusted2$ID  <= 84] <- "High_income_perc" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 84 & Boots_bills_unadjusted2$ID  <= 112] <- "Prev_high_mid_income_in_inactive_people" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 112 & Boots_bills_unadjusted2$ID  <= 140] <- "Prev_high_low_income_in_inactive_people" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 140 & Boots_bills_unadjusted2$ID  <= 168] <- "Prev_mid_income_in_inactive_people" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 168 & Boots_bills_unadjusted2$ID  <= 196] <- "PFP_high_mid_income" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 196 & Boots_bills_unadjusted2$ID  <= 224] <- "PFP_high_low_income" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 224 & Boots_bills_unadjusted2$ID  <= 252] <- "PFP_mid_income" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 252 & Boots_bills_unadjusted2$ID  <= 280] <- "High_mid_inequality" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 280 & Boots_bills_unadjusted2$ID  <= 308] <- "High_low_inequality" 
Boots_bills_unadjusted2$variable [ Boots_bills_unadjusted2$ID  > 308] <- "Mid_low_inequality" 
Boots_bills_unadjusted2$variable <- as.factor(Boots_bills_unadjusted2$variable)

Boots_bills_unadjusted2_Primary_perc <- Boots_bills_unadjusted2[Boots_bills_unadjusted2$ID  > 0 & Boots_bills_unadjusted2$ID  <= 28,]
names(Boots_bills_unadjusted2_Primary_perc)[1] <- "Low_income_perc_boot_unadj"
names(Boots_bills_unadjusted2_Primary_perc)[2] <- "Low_income_perc_boot_se_unadj"
names(Boots_bills_unadjusted2_Primary_perc)[3] <- "Low_income_perc_boot_unadj_low"
names(Boots_bills_unadjusted2_Primary_perc)[4] <- "Low_income_perc_boot_unadj_up"

Boots_bills_unadjusted2_Secondary_perc <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 28 & Boots_bills_unadjusted2$ID  <= 56,] 
names(Boots_bills_unadjusted2_Secondary_perc)[1] <- "Mid_income_perc_boot_unadj"
names(Boots_bills_unadjusted2_Secondary_perc)[2] <- "Mid_income_perc_boot_se_unadj"
names(Boots_bills_unadjusted2_Secondary_perc)[3] <- "Mid_income_perc_boot_unadj_low"
names(Boots_bills_unadjusted2_Secondary_perc)[4] <- "Mid_income_perc_boot_unadj_up"

Boots_bills_unadjusted2_Universitary_perc <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 56 & Boots_bills_unadjusted2$ID  <= 84,] 
names(Boots_bills_unadjusted2_Universitary_perc)[1] <- "High_income_perc_boot_unadj"
names(Boots_bills_unadjusted2_Universitary_perc)[2] <- "High_income_perc_boot_se_unadj"
names(Boots_bills_unadjusted2_Universitary_perc)[3] <- "High_income_perc_boot_unadj_low"
names(Boots_bills_unadjusted2_Universitary_perc)[4] <- "High_income_perc_boot_unadj_up"

Boots_bills_unadjusted2_Prev_high_mid_educ_in_inactive_people <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 84 & Boots_bills_unadjusted2$ID  <= 112,] 
names(Boots_bills_unadjusted2_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_income_in_inactive_people_boot_unadj"
names(Boots_bills_unadjusted2_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_income_in_inactive_people_boot_se_unadj"
names(Boots_bills_unadjusted2_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_income_in_inactive_people_boot_unadj_low"
names(Boots_bills_unadjusted2_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_income_in_inactive_people_boot_unadj_up"

Boots_bills_unadjusted2_Prev_high_low_educ_in_inactive_people <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 112 & Boots_bills_unadjusted2$ID  <= 140,]
names(Boots_bills_unadjusted2_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_income_in_inactive_people_boot_unadj"
names(Boots_bills_unadjusted2_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_income_in_inactive_people_boot_se_unadj"
names(Boots_bills_unadjusted2_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_income_in_inactive_people_boot_unadj_low"
names(Boots_bills_unadjusted2_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_income_in_inactive_people_boot_unadj_up"

Boots_bills_unadjusted2_Prev_mid_educ_in_inactive_people <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 140 & Boots_bills_unadjusted2$ID  <= 168,]  
names(Boots_bills_unadjusted2_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_income_in_inactive_people_boot_unadj"
names(Boots_bills_unadjusted2_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_income_in_inactive_people_boot_se_unadj"
names(Boots_bills_unadjusted2_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_income_in_inactive_people_boot_unadj_low"
names(Boots_bills_unadjusted2_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_income_in_inactive_people_boot_unadj_up"

Boots_bills_unadjusted2_PFP_high_mid_educ <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 168 & Boots_bills_unadjusted2$ID  <= 196,]
names(Boots_bills_unadjusted2_PFP_high_mid_educ)[1] <- "PFP_high_mid_income_boot_unadj"
names(Boots_bills_unadjusted2_PFP_high_mid_educ)[2] <- "PFP_high_mid_income_boot_se_unadj"
names(Boots_bills_unadjusted2_PFP_high_mid_educ)[3] <- "PFP_high_mid_income_boot_unadj_low"
names(Boots_bills_unadjusted2_PFP_high_mid_educ)[4] <- "PFP_high_mid_income_boot_unadj_up"

Boots_bills_unadjusted2_PFP_high_low_educ <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 196 & Boots_bills_unadjusted2$ID  <= 224,]  
names(Boots_bills_unadjusted2_PFP_high_low_educ)[1] <- "PFP_high_low_income_boot_unadj"
names(Boots_bills_unadjusted2_PFP_high_low_educ)[2] <- "PFP_high_low_income_boot_se_unadj"
names(Boots_bills_unadjusted2_PFP_high_low_educ)[3] <- "PFP_high_low_income_boot_unadj_low"
names(Boots_bills_unadjusted2_PFP_high_low_educ)[4] <- "PFP_high_low_income_boot_unadj_up"

Boots_bills_unadjusted2_PFP_mid_educ <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 224 & Boots_bills_unadjusted2$ID  <= 252,] 
names(Boots_bills_unadjusted2_PFP_mid_educ)[1] <- "PFP_mid_income_boot_unadj"
names(Boots_bills_unadjusted2_PFP_mid_educ)[2] <- "PFP_mid_income_boot_se_unadj"
names(Boots_bills_unadjusted2_PFP_mid_educ)[3] <- "PFP_mid_income_boot_unadj_low"
names(Boots_bills_unadjusted2_PFP_mid_educ)[4] <- "PFP_mid_income_boot_unadj_up"

Boots_bills_unadjusted2_High_mid_inequality <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 252 & Boots_bills_unadjusted2$ID  <= 280,] 
names(Boots_bills_unadjusted2_High_mid_inequality)[1] <- "High_mid_inequality_income_boot_unadj"
names(Boots_bills_unadjusted2_High_mid_inequality)[2] <- "High_mid_inequality_income_boot_se_unadj"
names(Boots_bills_unadjusted2_High_mid_inequality)[3] <- "High_mid_inequality_income_boot_unadj_low"
names(Boots_bills_unadjusted2_High_mid_inequality)[4] <- "High_mid_inequality_income_boot_unadj_up"

Boots_bills_unadjusted2_High_low_inequality <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 280 & Boots_bills_unadjusted2$ID  <= 308,] 
names(Boots_bills_unadjusted2_High_low_inequality)[1] <- "High_low_inequality_income_boot_unadj"
names(Boots_bills_unadjusted2_High_low_inequality)[2] <- "High_low_inequality_income_boot_se_unadj"
names(Boots_bills_unadjusted2_High_low_inequality)[3] <- "High_low_inequality_income_boot_unadj_low"
names(Boots_bills_unadjusted2_High_low_inequality)[4] <- "High_low_inequality_income_boot_unadj_up"

Boots_bills_unadjusted2_Mid_low_inequality <- Boots_bills_unadjusted2 [ Boots_bills_unadjusted2$ID  > 308,] 
names(Boots_bills_unadjusted2_Mid_low_inequality)[1] <- "Mid_low_inequality_income_boot_unadj"
names(Boots_bills_unadjusted2_Mid_low_inequality)[2] <- "Mid_low_inequality_income_boot_se_unadj"
names(Boots_bills_unadjusted2_Mid_low_inequality)[3] <- "Mid_low_inequality_income_boot_unadj_low"
names(Boots_bills_unadjusted2_Mid_low_inequality)[4] <- "Mid_low_inequality_income_boot_unadj_up"

Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_Primary_perc, Boots_bills_unadjusted2_Secondary_perc, by  = "ID2")
Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_final, Boots_bills_unadjusted2_Universitary_perc, by  = "ID2")
Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_final, Boots_bills_unadjusted2_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_final, Boots_bills_unadjusted2_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_final, Boots_bills_unadjusted2_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_final, Boots_bills_unadjusted2_PFP_high_mid_educ, by  = "ID2")
Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_final, Boots_bills_unadjusted2_PFP_high_low_educ, by  = "ID2")
Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_final, Boots_bills_unadjusted2_PFP_mid_educ, by  = "ID2")
Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_final, Boots_bills_unadjusted2_High_mid_inequality, by  = "ID2")
Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_final, Boots_bills_unadjusted2_High_low_inequality, by  = "ID2")
Boots_bills_unadjusted2_final <- merge(Boots_bills_unadjusted2_final, Boots_bills_unadjusted2_Mid_low_inequality, by  = "ID2")
Boots_bills_unadjusted2_final <- Boots_bills_unadjusted2_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]



Boots_bills_adjusted_10 <- boot(data = eu2017, statistic = function_bills, R = 1000, 
                             OR_mid_low = OR_SES_data[7,9], OR_high_low = OR_SES_data[8,9], OR_high_mid = OR_SES_data[9,9])

Boots_bills_adjusted_10$t0 -> Boots_bills_adjusted_10_estimates
Boots_bills_adjusted_10$t -> Boots_bills_adjusted_10_error1
std.error(Boots_bills_adjusted_10_error1) -> Boots_bills_adjusted_10_error2
Boots_bills_adjusted_102 <- data.frame(Boots_bills_adjusted_10_estimates,Boots_bills_adjusted_10_error2)
Boots_bills_adjusted_102$Boots_bills_adjusted_10_error2 <- Boots_bills_adjusted_102$Boots_bills_adjusted_10_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_bills_adjusted_10)) -> Boots_bills_adjusted_10_CI
Boots_bills_adjusted_102$Boots_bills_adjusted_10_low <- Boots_bills_adjusted_10_CI$lwr
Boots_bills_adjusted_102$Boots_bills_adjusted_10_up <- Boots_bills_adjusted_10_CI$upr

Boots_bills_adjusted_102$ID <- seq(from=1,to=336)
Boots_bills_adjusted_102$ID2 <- seq(from=1,to=28)

Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 0 & Boots_bills_adjusted_102$ID  <= 28] <- "Low_income_perc" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 28 & Boots_bills_adjusted_102$ID  <= 56] <- "Mid_income_perc" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 56 & Boots_bills_adjusted_102$ID  <= 84] <- "High_income_perc" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 84 & Boots_bills_adjusted_102$ID  <= 112] <- "Prev_high_mid_income_in_inactive_people" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 112 & Boots_bills_adjusted_102$ID  <= 140] <- "Prev_high_low_income_in_inactive_people" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 140 & Boots_bills_adjusted_102$ID  <= 168] <- "Prev_mid_income_in_inactive_people" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 168 & Boots_bills_adjusted_102$ID  <= 196] <- "PFP_high_mid_income" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 196 & Boots_bills_adjusted_102$ID  <= 224] <- "PFP_high_low_income" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 224 & Boots_bills_adjusted_102$ID  <= 252] <- "PFP_mid_income" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 252 & Boots_bills_adjusted_102$ID  <= 280] <- "High_mid_inequality" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 280 & Boots_bills_adjusted_102$ID  <= 308] <- "High_low_inequality" 
Boots_bills_adjusted_102$variable [ Boots_bills_adjusted_102$ID  > 308] <- "Mid_low_inequality" 
Boots_bills_adjusted_102$variable <- as.factor(Boots_bills_adjusted_102$variable)

Boots_bills_adjusted_102_Primary_perc <- Boots_bills_adjusted_102[Boots_bills_adjusted_102$ID  > 0 & Boots_bills_adjusted_102$ID  <= 28,]
names(Boots_bills_adjusted_102_Primary_perc)[1] <- "Low_income_perc_boot_10"
names(Boots_bills_adjusted_102_Primary_perc)[2] <- "Low_income_perc_boot_se_10"
names(Boots_bills_adjusted_102_Primary_perc)[3] <- "Low_income_perc_boot_10_low"
names(Boots_bills_adjusted_102_Primary_perc)[4] <- "Low_income_perc_boot_10_up"

Boots_bills_adjusted_102_Secondary_perc <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 28 & Boots_bills_adjusted_102$ID  <= 56,] 
names(Boots_bills_adjusted_102_Secondary_perc)[1] <- "Mid_income_perc_boot_10"
names(Boots_bills_adjusted_102_Secondary_perc)[2] <- "Mid_income_perc_boot_se_10"
names(Boots_bills_adjusted_102_Secondary_perc)[3] <- "Mid_income_perc_boot_10_low"
names(Boots_bills_adjusted_102_Secondary_perc)[4] <- "Mid_income_perc_boot_10_up"

Boots_bills_adjusted_102_Universitary_perc <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 56 & Boots_bills_adjusted_102$ID  <= 84,] 
names(Boots_bills_adjusted_102_Universitary_perc)[1] <- "High_income_perc_boot_10"
names(Boots_bills_adjusted_102_Universitary_perc)[2] <- "High_income_perc_boot_se_10"
names(Boots_bills_adjusted_102_Universitary_perc)[3] <- "High_income_perc_boot_10_low"
names(Boots_bills_adjusted_102_Universitary_perc)[4] <- "High_income_perc_boot_10_up"

Boots_bills_adjusted_102_Prev_high_mid_educ_in_inactive_people <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 84 & Boots_bills_adjusted_102$ID  <= 112,] 
names(Boots_bills_adjusted_102_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_income_in_inactive_people_boot_10"
names(Boots_bills_adjusted_102_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_income_in_inactive_people_boot_se_10"
names(Boots_bills_adjusted_102_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_income_in_inactive_people_boot_10_low"
names(Boots_bills_adjusted_102_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_income_in_inactive_people_boot_10_up"

Boots_bills_adjusted_102_Prev_high_low_educ_in_inactive_people <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 112 & Boots_bills_adjusted_102$ID  <= 140,]
names(Boots_bills_adjusted_102_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_income_in_inactive_people_boot_10"
names(Boots_bills_adjusted_102_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_income_in_inactive_people_boot_se_10"
names(Boots_bills_adjusted_102_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_income_in_inactive_people_boot_10_low"
names(Boots_bills_adjusted_102_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_income_in_inactive_people_boot_10_up"

Boots_bills_adjusted_102_Prev_mid_educ_in_inactive_people <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 140 & Boots_bills_adjusted_102$ID  <= 168,]  
names(Boots_bills_adjusted_102_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_income_in_inactive_people_boot_10"
names(Boots_bills_adjusted_102_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_income_in_inactive_people_boot_se_10"
names(Boots_bills_adjusted_102_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_income_in_inactive_people_boot_10_low"
names(Boots_bills_adjusted_102_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_income_in_inactive_people_boot_10_up"

Boots_bills_adjusted_102_PFP_high_mid_educ <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 168 & Boots_bills_adjusted_102$ID  <= 196,]
names(Boots_bills_adjusted_102_PFP_high_mid_educ)[1] <- "PFP_high_mid_income_boot_10"
names(Boots_bills_adjusted_102_PFP_high_mid_educ)[2] <- "PFP_high_mid_income_boot_se_10"
names(Boots_bills_adjusted_102_PFP_high_mid_educ)[3] <- "PFP_high_mid_income_boot_10_low"
names(Boots_bills_adjusted_102_PFP_high_mid_educ)[4] <- "PFP_high_mid_income_boot_10_up"

Boots_bills_adjusted_102_PFP_high_low_educ <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 196 & Boots_bills_adjusted_102$ID  <= 224,]  
names(Boots_bills_adjusted_102_PFP_high_low_educ)[1] <- "PFP_high_low_income_boot_10"
names(Boots_bills_adjusted_102_PFP_high_low_educ)[2] <- "PFP_high_low_income_boot_se_10"
names(Boots_bills_adjusted_102_PFP_high_low_educ)[3] <- "PFP_high_low_income_boot_10_low"
names(Boots_bills_adjusted_102_PFP_high_low_educ)[4] <- "PFP_high_low_income_boot_10_up"

Boots_bills_adjusted_102_PFP_mid_educ <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 224 & Boots_bills_adjusted_102$ID  <= 252,] 
names(Boots_bills_adjusted_102_PFP_mid_educ)[1] <- "PFP_mid_income_boot_10"
names(Boots_bills_adjusted_102_PFP_mid_educ)[2] <- "PFP_mid_income_boot_se_10"
names(Boots_bills_adjusted_102_PFP_mid_educ)[3] <- "PFP_mid_income_boot_10_low"
names(Boots_bills_adjusted_102_PFP_mid_educ)[4] <- "PFP_mid_income_boot_10_up"

Boots_bills_adjusted_102_High_mid_inequality <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 252 & Boots_bills_adjusted_102$ID  <= 280,] 
names(Boots_bills_adjusted_102_High_mid_inequality)[1] <- "High_mid_inequality_income_boot_10"
names(Boots_bills_adjusted_102_High_mid_inequality)[2] <- "High_mid_inequality_income_boot_se_10"
names(Boots_bills_adjusted_102_High_mid_inequality)[3] <- "High_mid_inequality_income_boot_10_low"
names(Boots_bills_adjusted_102_High_mid_inequality)[4] <- "High_mid_inequality_income_boot_10_up"

Boots_bills_adjusted_102_High_low_inequality <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 280 & Boots_bills_adjusted_102$ID  <= 308,] 
names(Boots_bills_adjusted_102_High_low_inequality)[1] <- "High_low_inequality_income_boot_10"
names(Boots_bills_adjusted_102_High_low_inequality)[2] <- "High_low_inequality_income_boot_se_10"
names(Boots_bills_adjusted_102_High_low_inequality)[3] <- "High_low_inequality_income_boot_10_low"
names(Boots_bills_adjusted_102_High_low_inequality)[4] <- "High_low_inequality_income_boot_10_up"

Boots_bills_adjusted_102_Mid_low_inequality <- Boots_bills_adjusted_102 [ Boots_bills_adjusted_102$ID  > 308,] 
names(Boots_bills_adjusted_102_Mid_low_inequality)[1] <- "Mid_low_inequality_income_boot_10"
names(Boots_bills_adjusted_102_Mid_low_inequality)[2] <- "Mid_low_inequality_income_boot_se_10"
names(Boots_bills_adjusted_102_Mid_low_inequality)[3] <- "Mid_low_inequality_income_boot_10_low"
names(Boots_bills_adjusted_102_Mid_low_inequality)[4] <- "Mid_low_inequality_income_boot_10_up"

Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_Primary_perc, Boots_bills_adjusted_102_Secondary_perc, by  = "ID2")
Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_final, Boots_bills_adjusted_102_Universitary_perc, by  = "ID2")
Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_final, Boots_bills_adjusted_102_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_final, Boots_bills_adjusted_102_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_final, Boots_bills_adjusted_102_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_final, Boots_bills_adjusted_102_PFP_high_mid_educ, by  = "ID2")
Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_final, Boots_bills_adjusted_102_PFP_high_low_educ, by  = "ID2")
Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_final, Boots_bills_adjusted_102_PFP_mid_educ, by  = "ID2")
Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_final, Boots_bills_adjusted_102_High_mid_inequality, by  = "ID2")
Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_final, Boots_bills_adjusted_102_High_low_inequality, by  = "ID2")
Boots_bills_adjusted_102_final <- merge(Boots_bills_adjusted_102_final, Boots_bills_adjusted_102_Mid_low_inequality, by  = "ID2")
Boots_bills_adjusted_102_final <- Boots_bills_adjusted_102_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]




Boots_bills_adjusted_20 <- boot(data = eu2017, statistic = function_bills, R = 1000, 
                             OR_mid_low = OR_SES_data[7,10], OR_high_low = OR_SES_data[8,10], OR_high_mid = OR_SES_data[9,10])

Boots_bills_adjusted_20$t0 -> Boots_bills_adjusted_20_estimates
Boots_bills_adjusted_20$t -> Boots_bills_adjusted_20_error1
std.error(Boots_bills_adjusted_20_error1) -> Boots_bills_adjusted_20_error2
Boots_bills_adjusted_202 <- data.frame(Boots_bills_adjusted_20_estimates,Boots_bills_adjusted_20_error2)
Boots_bills_adjusted_202$Boots_bills_adjusted_20_error2 <- Boots_bills_adjusted_202$Boots_bills_adjusted_20_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_bills_adjusted_20)) -> Boots_bills_adjusted_20_CI
Boots_bills_adjusted_202$Boots_bills_adjusted_20_low <- Boots_bills_adjusted_20_CI$lwr
Boots_bills_adjusted_202$Boots_bills_adjusted_20_up <- Boots_bills_adjusted_20_CI$upr

Boots_bills_adjusted_202$ID <- seq(from=1,to=336)
Boots_bills_adjusted_202$ID2 <- seq(from=1,to=28)

Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 0 & Boots_bills_adjusted_202$ID  <= 28] <- "Low_income_perc" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 28 & Boots_bills_adjusted_202$ID  <= 56] <- "Mid_income_perc" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 56 & Boots_bills_adjusted_202$ID  <= 84] <- "High_income_perc" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 84 & Boots_bills_adjusted_202$ID  <= 112] <- "Prev_high_mid_income_in_inactive_people" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 112 & Boots_bills_adjusted_202$ID  <= 140] <- "Prev_high_low_income_in_inactive_people" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 140 & Boots_bills_adjusted_202$ID  <= 168] <- "Prev_mid_income_in_inactive_people" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 168 & Boots_bills_adjusted_202$ID  <= 196] <- "PFP_high_mid_income" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 196 & Boots_bills_adjusted_202$ID  <= 224] <- "PFP_high_low_income" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 224 & Boots_bills_adjusted_202$ID  <= 252] <- "PFP_mid_income" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 252 & Boots_bills_adjusted_202$ID  <= 280] <- "High_mid_inequality" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 280 & Boots_bills_adjusted_202$ID  <= 308] <- "High_low_inequality" 
Boots_bills_adjusted_202$variable [ Boots_bills_adjusted_202$ID  > 308] <- "Mid_low_inequality" 
Boots_bills_adjusted_202$variable <- as.factor(Boots_bills_adjusted_202$variable)

Boots_bills_adjusted_202_Primary_perc <- Boots_bills_adjusted_202[Boots_bills_adjusted_202$ID  > 0 & Boots_bills_adjusted_202$ID  <= 28,]
names(Boots_bills_adjusted_202_Primary_perc)[1] <- "Low_income_perc_boot_20"
names(Boots_bills_adjusted_202_Primary_perc)[2] <- "Low_income_perc_boot_se_20"
names(Boots_bills_adjusted_202_Primary_perc)[3] <- "Low_income_perc_boot_20_low"
names(Boots_bills_adjusted_202_Primary_perc)[4] <- "Low_income_perc_boot_20_up"

Boots_bills_adjusted_202_Secondary_perc <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 28 & Boots_bills_adjusted_202$ID  <= 56,] 
names(Boots_bills_adjusted_202_Secondary_perc)[1] <- "Mid_income_perc_boot_20"
names(Boots_bills_adjusted_202_Secondary_perc)[2] <- "Mid_income_perc_boot_se_20"
names(Boots_bills_adjusted_202_Secondary_perc)[3] <- "Mid_income_perc_boot_20_low"
names(Boots_bills_adjusted_202_Secondary_perc)[4] <- "Mid_income_perc_boot_20_up"

Boots_bills_adjusted_202_Universitary_perc <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 56 & Boots_bills_adjusted_202$ID  <= 84,] 
names(Boots_bills_adjusted_202_Universitary_perc)[1] <- "High_income_perc_boot_20"
names(Boots_bills_adjusted_202_Universitary_perc)[2] <- "High_income_perc_boot_se_20"
names(Boots_bills_adjusted_202_Universitary_perc)[3] <- "High_income_perc_boot_20_low"
names(Boots_bills_adjusted_202_Universitary_perc)[4] <- "High_income_perc_boot_20_up"

Boots_bills_adjusted_202_Prev_high_mid_educ_in_inactive_people <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 84 & Boots_bills_adjusted_202$ID  <= 112,] 
names(Boots_bills_adjusted_202_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_income_in_inactive_people_boot_20"
names(Boots_bills_adjusted_202_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_income_in_inactive_people_boot_se_20"
names(Boots_bills_adjusted_202_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_income_in_inactive_people_boot_20_low"
names(Boots_bills_adjusted_202_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_income_in_inactive_people_boot_20_up"

Boots_bills_adjusted_202_Prev_high_low_educ_in_inactive_people <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 112 & Boots_bills_adjusted_202$ID  <= 140,]
names(Boots_bills_adjusted_202_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_income_in_inactive_people_boot_20"
names(Boots_bills_adjusted_202_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_income_in_inactive_people_boot_se_20"
names(Boots_bills_adjusted_202_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_income_in_inactive_people_boot_20_low"
names(Boots_bills_adjusted_202_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_income_in_inactive_people_boot_20_up"

Boots_bills_adjusted_202_Prev_mid_educ_in_inactive_people <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 140 & Boots_bills_adjusted_202$ID  <= 168,]  
names(Boots_bills_adjusted_202_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_income_in_inactive_people_boot_20"
names(Boots_bills_adjusted_202_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_income_in_inactive_people_boot_se_20"
names(Boots_bills_adjusted_202_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_income_in_inactive_people_boot_20_low"
names(Boots_bills_adjusted_202_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_income_in_inactive_people_boot_20_up"

Boots_bills_adjusted_202_PFP_high_mid_educ <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 168 & Boots_bills_adjusted_202$ID  <= 196,]
names(Boots_bills_adjusted_202_PFP_high_mid_educ)[1] <- "PFP_high_mid_income_boot_20"
names(Boots_bills_adjusted_202_PFP_high_mid_educ)[2] <- "PFP_high_mid_income_boot_se_20"
names(Boots_bills_adjusted_202_PFP_high_mid_educ)[3] <- "PFP_high_mid_income_boot_20_low"
names(Boots_bills_adjusted_202_PFP_high_mid_educ)[4] <- "PFP_high_mid_income_boot_20_up"

Boots_bills_adjusted_202_PFP_high_low_educ <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 196 & Boots_bills_adjusted_202$ID  <= 224,]  
names(Boots_bills_adjusted_202_PFP_high_low_educ)[1] <- "PFP_high_low_income_boot_20"
names(Boots_bills_adjusted_202_PFP_high_low_educ)[2] <- "PFP_high_low_income_boot_se_20"
names(Boots_bills_adjusted_202_PFP_high_low_educ)[3] <- "PFP_high_low_income_boot_20_low"
names(Boots_bills_adjusted_202_PFP_high_low_educ)[4] <- "PFP_high_low_income_boot_20_up"

Boots_bills_adjusted_202_PFP_mid_educ <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 224 & Boots_bills_adjusted_202$ID  <= 252,] 
names(Boots_bills_adjusted_202_PFP_mid_educ)[1] <- "PFP_mid_income_boot_20"
names(Boots_bills_adjusted_202_PFP_mid_educ)[2] <- "PFP_mid_income_boot_se_20"
names(Boots_bills_adjusted_202_PFP_mid_educ)[3] <- "PFP_mid_income_boot_20_low"
names(Boots_bills_adjusted_202_PFP_mid_educ)[4] <- "PFP_mid_income_boot_20_up"

Boots_bills_adjusted_202_High_mid_inequality <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 252 & Boots_bills_adjusted_202$ID  <= 280,] 
names(Boots_bills_adjusted_202_High_mid_inequality)[1] <- "High_mid_inequality_income_boot_20"
names(Boots_bills_adjusted_202_High_mid_inequality)[2] <- "High_mid_inequality_income_boot_se_20"
names(Boots_bills_adjusted_202_High_mid_inequality)[3] <- "High_mid_inequality_income_boot_20_low"
names(Boots_bills_adjusted_202_High_mid_inequality)[4] <- "High_mid_inequality_income_boot_20_up"

Boots_bills_adjusted_202_High_low_inequality <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 280 & Boots_bills_adjusted_202$ID  <= 308,] 
names(Boots_bills_adjusted_202_High_low_inequality)[1] <- "High_low_inequality_income_boot_20"
names(Boots_bills_adjusted_202_High_low_inequality)[2] <- "High_low_inequality_income_boot_se_20"
names(Boots_bills_adjusted_202_High_low_inequality)[3] <- "High_low_inequality_income_boot_20_low"
names(Boots_bills_adjusted_202_High_low_inequality)[4] <- "High_low_inequality_income_boot_20_up"

Boots_bills_adjusted_202_Mid_low_inequality <- Boots_bills_adjusted_202 [ Boots_bills_adjusted_202$ID  > 308,] 
names(Boots_bills_adjusted_202_Mid_low_inequality)[1] <- "Mid_low_inequality_income_boot_20"
names(Boots_bills_adjusted_202_Mid_low_inequality)[2] <- "Mid_low_inequality_income_boot_se_20"
names(Boots_bills_adjusted_202_Mid_low_inequality)[3] <- "Mid_low_inequality_income_boot_20_low"
names(Boots_bills_adjusted_202_Mid_low_inequality)[4] <- "Mid_low_inequality_income_boot_20_up"

Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_Primary_perc, Boots_bills_adjusted_202_Secondary_perc, by  = "ID2")
Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_final, Boots_bills_adjusted_202_Universitary_perc, by  = "ID2")
Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_final, Boots_bills_adjusted_202_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_final, Boots_bills_adjusted_202_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_final, Boots_bills_adjusted_202_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_final, Boots_bills_adjusted_202_PFP_high_mid_educ, by  = "ID2")
Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_final, Boots_bills_adjusted_202_PFP_high_low_educ, by  = "ID2")
Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_final, Boots_bills_adjusted_202_PFP_mid_educ, by  = "ID2")
Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_final, Boots_bills_adjusted_202_High_mid_inequality, by  = "ID2")
Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_final, Boots_bills_adjusted_202_High_low_inequality, by  = "ID2")
Boots_bills_adjusted_202_final <- merge(Boots_bills_adjusted_202_final, Boots_bills_adjusted_202_Mid_low_inequality, by  = "ID2")
Boots_bills_adjusted_202_final <- Boots_bills_adjusted_202_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]



Boots_bills_adjusted_50 <- boot(data = eu2017, statistic = function_bills, R = 1000, 
                             OR_mid_low = OR_SES_data[7,11], OR_high_low = OR_SES_data[8,11], OR_high_mid = OR_SES_data[9,11])

Boots_bills_adjusted_50$t0 -> Boots_bills_adjusted_50_estimates
Boots_bills_adjusted_50$t -> Boots_bills_adjusted_50_error1
std.error(Boots_bills_adjusted_50_error1) -> Boots_bills_adjusted_50_error2
Boots_bills_adjusted_502 <- data.frame(Boots_bills_adjusted_50_estimates,Boots_bills_adjusted_50_error2)
Boots_bills_adjusted_502$Boots_bills_adjusted_50_error2 <- Boots_bills_adjusted_502$Boots_bills_adjusted_50_error2 * 10

do.call(rbind,lapply(1:336,getCI,x=Boots_bills_adjusted_50)) -> Boots_bills_adjusted_50_CI
Boots_bills_adjusted_502$Boots_bills_adjusted_50_low <- Boots_bills_adjusted_50_CI$lwr
Boots_bills_adjusted_502$Boots_bills_adjusted_50_up <- Boots_bills_adjusted_50_CI$upr

Boots_bills_adjusted_502$ID <- seq(from=1,to=336)
Boots_bills_adjusted_502$ID2 <- seq(from=1,to=28)

Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 0 & Boots_bills_adjusted_502$ID  <= 28] <- "Low_income_perc" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 28 & Boots_bills_adjusted_502$ID  <= 56] <- "Mid_income_perc" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 56 & Boots_bills_adjusted_502$ID  <= 84] <- "High_income_perc" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 84 & Boots_bills_adjusted_502$ID  <= 112] <- "Prev_high_mid_income_in_inactive_people" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 112 & Boots_bills_adjusted_502$ID  <= 140] <- "Prev_high_low_income_in_inactive_people" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 140 & Boots_bills_adjusted_502$ID  <= 168] <- "Prev_mid_income_in_inactive_people" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 168 & Boots_bills_adjusted_502$ID  <= 196] <- "PFP_high_mid_income" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 196 & Boots_bills_adjusted_502$ID  <= 224] <- "PFP_high_low_income" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 224 & Boots_bills_adjusted_502$ID  <= 252] <- "PFP_mid_income" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 252 & Boots_bills_adjusted_502$ID  <= 280] <- "High_mid_inequality" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 280 & Boots_bills_adjusted_502$ID  <= 308] <- "High_low_inequality" 
Boots_bills_adjusted_502$variable [ Boots_bills_adjusted_502$ID  > 308] <- "Mid_low_inequality" 
Boots_bills_adjusted_502$variable <- as.factor(Boots_bills_adjusted_502$variable)

Boots_bills_adjusted_502_Primary_perc <- Boots_bills_adjusted_502[Boots_bills_adjusted_502$ID  > 0 & Boots_bills_adjusted_502$ID  <= 28,]
names(Boots_bills_adjusted_502_Primary_perc)[1] <- "Low_income_perc_boot_50"
names(Boots_bills_adjusted_502_Primary_perc)[2] <- "Low_income_perc_boot_se_50"
names(Boots_bills_adjusted_502_Primary_perc)[3] <- "Low_income_perc_boot_50_low"
names(Boots_bills_adjusted_502_Primary_perc)[4] <- "Low_income_perc_boot_50_up"

Boots_bills_adjusted_502_Secondary_perc <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 28 & Boots_bills_adjusted_502$ID  <= 56,] 
names(Boots_bills_adjusted_502_Secondary_perc)[1] <- "Mid_income_perc_boot_50"
names(Boots_bills_adjusted_502_Secondary_perc)[2] <- "Mid_income_perc_boot_se_50"
names(Boots_bills_adjusted_502_Secondary_perc)[3] <- "Mid_income_perc_boot_50_low"
names(Boots_bills_adjusted_502_Secondary_perc)[4] <- "Mid_income_perc_boot_50_up"

Boots_bills_adjusted_502_Universitary_perc <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 56 & Boots_bills_adjusted_502$ID  <= 84,] 
names(Boots_bills_adjusted_502_Universitary_perc)[1] <- "High_income_perc_boot_50"
names(Boots_bills_adjusted_502_Universitary_perc)[2] <- "High_income_perc_boot_se_50"
names(Boots_bills_adjusted_502_Universitary_perc)[3] <- "High_income_perc_boot_50_low"
names(Boots_bills_adjusted_502_Universitary_perc)[4] <- "High_income_perc_boot_50_up"

Boots_bills_adjusted_502_Prev_high_mid_educ_in_inactive_people <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 84 & Boots_bills_adjusted_502$ID  <= 112,] 
names(Boots_bills_adjusted_502_Prev_high_mid_educ_in_inactive_people)[1] <- "Prev_high_mid_income_in_inactive_people_boot_50"
names(Boots_bills_adjusted_502_Prev_high_mid_educ_in_inactive_people)[2] <- "Prev_high_mid_income_in_inactive_people_boot_se_50"
names(Boots_bills_adjusted_502_Prev_high_mid_educ_in_inactive_people)[3] <- "Prev_high_mid_income_in_inactive_people_boot_50_low"
names(Boots_bills_adjusted_502_Prev_high_mid_educ_in_inactive_people)[4] <- "Prev_high_mid_income_in_inactive_people_boot_50_up"

Boots_bills_adjusted_502_Prev_high_low_educ_in_inactive_people <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 112 & Boots_bills_adjusted_502$ID  <= 140,]
names(Boots_bills_adjusted_502_Prev_high_low_educ_in_inactive_people)[1] <- "Prev_high_low_income_in_inactive_people_boot_50"
names(Boots_bills_adjusted_502_Prev_high_low_educ_in_inactive_people)[2] <- "Prev_high_low_income_in_inactive_people_boot_se_50"
names(Boots_bills_adjusted_502_Prev_high_low_educ_in_inactive_people)[3] <- "Prev_high_low_income_in_inactive_people_boot_50_low"
names(Boots_bills_adjusted_502_Prev_high_low_educ_in_inactive_people)[4] <- "Prev_high_low_income_in_inactive_people_boot_50_up"

Boots_bills_adjusted_502_Prev_mid_educ_in_inactive_people <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 140 & Boots_bills_adjusted_502$ID  <= 168,]  
names(Boots_bills_adjusted_502_Prev_mid_educ_in_inactive_people)[1] <- "Prev_mid_income_in_inactive_people_boot_50"
names(Boots_bills_adjusted_502_Prev_mid_educ_in_inactive_people)[2] <- "Prev_mid_income_in_inactive_people_boot_se_50"
names(Boots_bills_adjusted_502_Prev_mid_educ_in_inactive_people)[3] <- "Prev_mid_income_in_inactive_people_boot_50_low"
names(Boots_bills_adjusted_502_Prev_mid_educ_in_inactive_people)[4] <- "Prev_mid_income_in_inactive_people_boot_50_up"

Boots_bills_adjusted_502_PFP_high_mid_educ <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 168 & Boots_bills_adjusted_502$ID  <= 196,]
names(Boots_bills_adjusted_502_PFP_high_mid_educ)[1] <- "PFP_high_mid_income_boot_50"
names(Boots_bills_adjusted_502_PFP_high_mid_educ)[2] <- "PFP_high_mid_income_boot_se_50"
names(Boots_bills_adjusted_502_PFP_high_mid_educ)[3] <- "PFP_high_mid_income_boot_50_low"
names(Boots_bills_adjusted_502_PFP_high_mid_educ)[4] <- "PFP_high_mid_income_boot_50_up"

Boots_bills_adjusted_502_PFP_high_low_educ <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 196 & Boots_bills_adjusted_502$ID  <= 224,]  
names(Boots_bills_adjusted_502_PFP_high_low_educ)[1] <- "PFP_high_low_income_boot_50"
names(Boots_bills_adjusted_502_PFP_high_low_educ)[2] <- "PFP_high_low_income_boot_se_50"
names(Boots_bills_adjusted_502_PFP_high_low_educ)[3] <- "PFP_high_low_income_boot_50_low"
names(Boots_bills_adjusted_502_PFP_high_low_educ)[4] <- "PFP_high_low_income_boot_50_up"

Boots_bills_adjusted_502_PFP_mid_educ <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 224 & Boots_bills_adjusted_502$ID  <= 252,] 
names(Boots_bills_adjusted_502_PFP_mid_educ)[1] <- "PFP_mid_income_boot_50"
names(Boots_bills_adjusted_502_PFP_mid_educ)[2] <- "PFP_mid_income_boot_se_50"
names(Boots_bills_adjusted_502_PFP_mid_educ)[3] <- "PFP_mid_income_boot_50_low"
names(Boots_bills_adjusted_502_PFP_mid_educ)[4] <- "PFP_mid_income_boot_50_up"

Boots_bills_adjusted_502_High_mid_inequality <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 252 & Boots_bills_adjusted_502$ID  <= 280,] 
names(Boots_bills_adjusted_502_High_mid_inequality)[1] <- "High_mid_inequality_income_boot_50"
names(Boots_bills_adjusted_502_High_mid_inequality)[2] <- "High_mid_inequality_income_boot_se_50"
names(Boots_bills_adjusted_502_High_mid_inequality)[3] <- "High_mid_inequality_income_boot_50_low"
names(Boots_bills_adjusted_502_High_mid_inequality)[4] <- "High_mid_inequality_income_boot_50_up"

Boots_bills_adjusted_502_High_low_inequality <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 280 & Boots_bills_adjusted_502$ID  <= 308,] 
names(Boots_bills_adjusted_502_High_low_inequality)[1] <- "High_low_inequality_income_boot_50"
names(Boots_bills_adjusted_502_High_low_inequality)[2] <- "High_low_inequality_income_boot_se_50"
names(Boots_bills_adjusted_502_High_low_inequality)[3] <- "High_low_inequality_income_boot_50_low"
names(Boots_bills_adjusted_502_High_low_inequality)[4] <- "High_low_inequality_income_boot_50_up"

Boots_bills_adjusted_502_Mid_low_inequality <- Boots_bills_adjusted_502 [ Boots_bills_adjusted_502$ID  > 308,] 
names(Boots_bills_adjusted_502_Mid_low_inequality)[1] <- "Mid_low_inequality_income_boot_50"
names(Boots_bills_adjusted_502_Mid_low_inequality)[2] <- "Mid_low_inequality_income_boot_se_50"
names(Boots_bills_adjusted_502_Mid_low_inequality)[3] <- "Mid_low_inequality_income_boot_50_low"
names(Boots_bills_adjusted_502_Mid_low_inequality)[4] <- "Mid_low_inequality_income_boot_50_up"

Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_Primary_perc, Boots_bills_adjusted_502_Secondary_perc, by  = "ID2")
Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_final, Boots_bills_adjusted_502_Universitary_perc, by  = "ID2")
Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_final, Boots_bills_adjusted_502_Prev_high_mid_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_final, Boots_bills_adjusted_502_Prev_high_low_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_final, Boots_bills_adjusted_502_Prev_mid_educ_in_inactive_people, by  = "ID2")
Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_final, Boots_bills_adjusted_502_PFP_high_mid_educ, by  = "ID2")
Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_final, Boots_bills_adjusted_502_PFP_high_low_educ, by  = "ID2")
Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_final, Boots_bills_adjusted_502_PFP_mid_educ, by  = "ID2")
Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_final, Boots_bills_adjusted_502_High_mid_inequality, by  = "ID2")
Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_final, Boots_bills_adjusted_502_High_low_inequality, by  = "ID2")
Boots_bills_adjusted_502_final <- merge(Boots_bills_adjusted_502_final, Boots_bills_adjusted_502_Mid_low_inequality, by  = "ID2")
Boots_bills_adjusted_502_final <- Boots_bills_adjusted_502_final[-c(6,7,12,13,18,19,24,25,30,31,36,37,42,43,48,49,54,55,60,61,66,67,72,73)]




#### Computing total number of people of physical inactivity averted ####
eu2017 %>% group_by(Country_rec, Age_3clusters3) %>%
    summarise(Median_age = median(Age), n = n(), Inactive = sum(WHO_prev == "Inactive"), 
              Active = sum(WHO_prev == "Active")) -> country_inactivity

country_inactivity$Inactive_perc <- (country_inactivity$Inactive * 100)/ country_inactivity$n
country_inactivity$Active_perc <- (country_inactivity$Active * 100)/ country_inactivity$n


## AGE STANDARIZATION BY EUROSTAT STANDARD 
Total_weight <- 5500+6000+6000+6500+7000+7000+7000+7000+6500+6000+5500+5000+4000+2500+1500+1000 
Weight_15_34 <- (5500+6000+6000+6500) / Total_weight
Weight_35_64 <- (7000+7000+7000+7000+6500+6000) / Total_weight
Weight_65 <- (5500+5000+4000+2500+1500+1000)/ Total_weight

country_inactivity$Age_weights[country_inactivity$Age_3clusters3 == "15-34"] <- Weight_15_34
country_inactivity$Age_weights[country_inactivity$Age_3clusters3 == "35-64"] <- Weight_35_64
country_inactivity$Age_weights[country_inactivity$Age_3clusters3 == "65+"] <- Weight_65

country_inactivity %>% group_by(Country_rec) %>%
    summarise(Inactive_perc = weighted.mean(Inactive_perc, Age_weights)) -> country_inactivity


## Percentage and number of people aged 15 and over by country
Country_data <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/13. Physical activity increase attruible to better living conditions in EU 2017/MMR/Country_level_data.sav", to.data.frame = T, use.missings = F)
Country_data$Country <- fct_expand(Country_data$Country, c("AT - Austria", 
                                                                           "BE - Belgium", "BG - Bulgaria", "CY - Cyprus (Republic)",
                                                                           "CZ - Czech Republic", "DE Germany", "DK - Denmark", "EE - Estonia",
                                                                           "ES -Spain", "FI - Finland", "FR - France", "GR - Greece",
                                                                           "HR - Croatia", "HU - Hungary", "IE - Ireland", "IT - Italy",
                                                                           "LT - Lithuania", "LU - Luxembourg", "LV - Latvia", "MT - Malta",
                                                                           "NL - The Netherlands", "PL - Poland", "PT - Portugal", "RO - Romania",
                                                                           "SE - Sweden", "SI - Slovenia", "SK - Slovakia", "UK United Kingdom")) 

Country_data$Country[Country_data$Country == "AT - Austria          "] <- "AT - Austria"
Country_data$Country[Country_data$Country == "BE - Belgium          "] <- "BE - Belgium"
Country_data$Country[Country_data$Country == "BG - Bulgaria         "] <-"BG - Bulgaria"
Country_data$Country[Country_data$Country == "CY - Cyprus (Republic)"] <- "CY - Cyprus (Republic)"
Country_data$Country[Country_data$Country == "CZ - Czech Republic   "] <- "CZ - Czech Republic"
Country_data$Country[Country_data$Country == "DK - Denmark          "] <- "DK - Denmark"
Country_data$Country[Country_data$Country == "EE - Estonia          "] <- "EE - Estonia"
Country_data$Country[Country_data$Country == "FI - Finland          "] <- "FI - Finland"
Country_data$Country[Country_data$Country == "FR - France           "] <- "FR - France" 
Country_data$Country[Country_data$Country == "DE Germany            " ] <- "DE Germany"
Country_data$Country[Country_data$Country == "GR - Greece           "] <- "GR - Greece"
Country_data$Country[Country_data$Country == "HR - Croatia          "] <- "HR - Croatia"
Country_data$Country[Country_data$Country == "HU - Hungary          " ] <- "HU - Hungary"
Country_data$Country[Country_data$Country == "IE - Ireland          "] <- "IE - Ireland"
Country_data$Country[Country_data$Country == "IT - Italy            " ] <- "IT - Italy"
Country_data$Country[Country_data$Country == "LV - Latvia           "] <- "LV - Latvia" 
Country_data$Country[Country_data$Country == "LT - Lithuania        "] <- "LT - Lithuania"
Country_data$Country[Country_data$Country == "LU - Luxembourg       "] <- "LU - Luxembourg"
Country_data$Country[Country_data$Country == "MT - Malta            " ] <- "MT - Malta"
Country_data$Country[Country_data$Country == "NL - The Netherlands  "] <- "NL - The Netherlands"
Country_data$Country[Country_data$Country == "PL - Poland           " ] <- "PL - Poland"
Country_data$Country[Country_data$Country == "PT - Portugal         "] <- "PT - Portugal"
Country_data$Country[Country_data$Country == "RO - Romania          " ] <- "RO - Romania"
Country_data$Country[Country_data$Country == "SE - Sweden           " ] <- "SE - Sweden"
Country_data$Country[Country_data$Country == "SI - Slovenia         "] <-  "SI - Slovenia"
Country_data$Country[Country_data$Country == "SK - Slovakia         "] <- "SK - Slovakia" 
Country_data$Country[Country_data$Country == "ES -Spain             "] <- "ES -Spain"
Country_data$Country[Country_data$Country == "UK United Kingdom     "] <- "UK United Kingdom"
Country_data$Country<- fct_drop(Country_data$Country, only = c("AT - Austria          ", 
                                                                               "BE - Belgium          ", "BG - Bulgaria         ", "CY - Cyprus (Republic)",
                                                                               "CZ - Czech Republic   ", "DE Germany            ", "DK - Denmark          ", "EE - Estonia          ",
                                                                               "ES -Spain             ", "FI - Finland          ", "FR - France           ", "GR - Greece           ",
                                                                               "HR - Croatia          ", "HU - Hungary          ", "IE - Ireland          ", "IT - Italy            ",
                                                                               "LT - Lithuania        ", "LU - Luxembourg       ", "LV - Latvia           ", "MT - Malta            ",
                                                                               "NL - The Netherlands  ", "PL - Poland           ", "PT - Portugal         ", "RO - Romania          ",
                                                                               "SE - Sweden           ", "SI - Slovenia         ", "SK - Slovakia         ", "UK United Kingdom     "))

Country_data$Pop_15_over_percent_2017 <- 100 - Country_data$Pop_0_14_percent_2017
Country_data$Pop_15_over_number_2017 <- (Country_data$Pop_15_over_percent_2017 * Country_data$Pob_2017)/100
Country_data$ID2 <- seq(from=1,to=28)

names(country_inactivity)[1] <- "Country"
Country_data_final <- merge(country_inactivity, Country_data, by = "Country")

Country_data_final$observed_number_inactivity_people <- (Country_data_final$Pop_15_over_number_2017 * Country_data_final$Inactive_perc)/100

Country_data_final_2 <- merge(Country_data_final, Boots_educ_adjusted2_final, by = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_educ_unadjusted2_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_educ_adjusted_102_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_educ_adjusted_202_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_educ_adjusted_502_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_educ_adjusted_raw2_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_sc_adjusted2_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_sc_unadjusted2_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_sc_adjusted_102_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_sc_adjusted_202_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_sc_adjusted_502_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_sc_adjusted_raw2_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_bills_adjusted2_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_bills_unadjusted2_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_bills_adjusted_102_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_bills_adjusted_202_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_bills_adjusted_502_final, by  = "ID2")
Country_data_final_2 <- merge(Country_data_final_2, Boots_bills_adjusted_raw2_final, by  = "ID2")




## Indirect estimated with adjusted PFP
library("scorer")

Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_mid_educ <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ
Country_data_final_2$number_inactivity_people_averted_no_mid_educ <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_low
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_low <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_up
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_up <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_low
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_low <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_up
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_up <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_low
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_low <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_up
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_up <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_up - Country_data_final_2$observed_number_inactivity_people



## Indirect estimated with unadjusted PFP
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_unadj <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_unadj/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_unadj
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_unadj <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_unadj - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_unadj_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_unadj_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_unadj_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_unadj_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_unadj_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_unadj_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_unadj_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_unadj_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_unadj_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_unadj_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_unadj <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_unadj/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_unadj
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_unadj <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_unadj - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_unadj_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_unadj_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_unadj_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_unadj_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_unadj_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_unadj_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_unadj_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_unadj_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_unadj_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_unadj_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_unadj <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_unadj/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_unadj
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_unadj <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_unadj - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_unadj_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_unadj_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_unadj_low
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_unadj_low <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_unadj_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_unadj_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_unadj_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_unadj_up
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_unadj_up <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_unadj_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_unadj <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_unadj/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_unadj
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_unadj <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_unadj - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_unadj_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_unadj_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_unadj_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_unadj_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_unadj_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_unadj_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_unadj_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_unadj_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_unadj_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_unadj_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_unadj <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_unadj/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_unadj
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_unadj <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_unadj - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_unadj_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_unadj_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_unadj_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_unadj_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_unadj_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_unadj_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_unadj_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_unadj_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_unadj_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_unadj_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_unadj <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_unadj/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_unadj
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_unadj <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_unadj - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_unadj_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_unadj_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_unadj_low
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_unadj_low <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_unadj_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_unadj_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_unadj_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_unadj_up
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_unadj_up <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_unadj_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_unadj <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_unadj/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_unadj
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_unadj <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_unadj - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_unadj_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_unadj_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_unadj_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_unadj_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_unadj_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_unadj_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_unadj_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_unadj_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_unadj_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_unadj_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_unadj <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_unadj/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_unadj
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_unadj <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_unadj - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_unadj_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_unadj_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_unadj_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_unadj_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_unadj_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_unadj_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_unadj_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_unadj_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_unadj_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_unadj_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_unadj <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_unadj/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_unadj
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_unadj <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_unadj - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_unadj_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_unadj_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_unadj_low
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_unadj_low <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_unadj_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_unadj_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_unadj_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_unadj_up
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_unadj_up <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_unadj_up - Country_data_final_2$observed_number_inactivity_people



## Indirect estimated with adjusted PFP - 10%
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_10 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_10/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_10
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_10 <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_10 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_10_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_10_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_10_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_10_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_10_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_10_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_10_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_10_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_10_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_10_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_10 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_10/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_10
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_10 <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_10 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_10_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_10_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_10_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_10_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_10_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_10_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_10_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_10_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_10_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_10_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_10 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_10/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_10
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_10 <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_10 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_10_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_10_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_10_low
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_10_low <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_10_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_10_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_10_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_10_up
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_10_up <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_10_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_10 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_10/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_10
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_10 <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_10 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_10_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_10_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_10_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_10_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_10_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_10_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_10_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_10_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_10_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_10_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_10 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_10/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_10
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_10 <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_10 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_10_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_10_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_10_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_10_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_10_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_10_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_10_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_10_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_10_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_10_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_10 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_10/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_10
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_10 <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_10 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_10_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_10_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_10_low
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_10_low <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_10_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_10_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_10_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_10_up
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_10_up <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_10_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_10 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_10/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_10
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_10 <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_10 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_10_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_10_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_10_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_10_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_10_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_10_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_10_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_10_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_10_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_10_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_10 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_10/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_10
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_10 <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_10 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_10_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_10_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_10_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_10_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_10_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_10_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_10_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_10_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_10_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_10_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_10 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_10/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_10
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_10 <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_10 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_10_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_10_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_10_low
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_10_low <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_10_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_10_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_10_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_10_up
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_10_up <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_10_up - Country_data_final_2$observed_number_inactivity_people




## Indirect estimated with adjusted PFP - 20%
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_20 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_20/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_20
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_20 <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_20 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_20_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_20_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_20_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_20_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_20_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_20_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_20_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_20_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_20_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_20_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_20 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_20/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_20
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_20 <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_20 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_20_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_20_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_20_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_20_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_20_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_20_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_20_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_20_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_20_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_20_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_20 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_20/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_20
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_20 <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_20 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_20_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_20_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_20_low
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_20_low <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_20_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_20_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_20_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_20_up
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_20_up <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_20_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_20 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_20/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_20
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_20 <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_20 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_20_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_20_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_20_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_20_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_20_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_20_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_20_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_20_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_20_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_20_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_20 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_20/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_20
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_20 <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_20 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_20_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_20_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_20_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_20_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_20_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_20_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_20_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_20_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_20_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_20_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_20 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_20/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_20
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_20 <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_20 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_20_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_20_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_20_low
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_20_low <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_20_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_20_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_20_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_20_up
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_20_up <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_20_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_20 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_20/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_20
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_20 <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_20 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_20_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_20_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_20_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_20_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_20_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_20_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_20_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_20_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_20_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_20_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_20 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_20/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_20
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_20 <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_20 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_20_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_20_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_20_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_20_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_20_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_20_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_20_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_20_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_20_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_20_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_20 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_20/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_20
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_20 <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_20 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_20_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_20_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_20_low
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_20_low <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_20_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_20_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_20_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_20_up
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_20_up <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_20_up - Country_data_final_2$observed_number_inactivity_people




## Indirect estimated with adjusted PFP - 50%
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_50 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_50/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_50
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_50 <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_50 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_50_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_50_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_50_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_50_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_50_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_50_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_educ_educ_boot_50_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_50_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_educ_50_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_educ_50_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_50 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_50/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_50
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_50 <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_50 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_50_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_50_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_50_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_50_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_50_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_50_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_educ_educ_boot_50_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_50_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_educ_50_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_educ_50_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_50 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_50/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_50
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_50 <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_50 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_50_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_50_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_50_low
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_50_low <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_50_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_50_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_educ_educ_boot_50_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_50_up
Country_data_final_2$number_inactivity_people_averted_no_mid_educ_50_up <- Country_data_final_2$hipo_number_inactivity_people_no_mid_educ_50_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_50 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_50/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_50
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_50 <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_50 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_50_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_50_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_50_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_50_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_50_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_50_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_sc_boot_50_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_50_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_sc_boot_50_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_sc_boot_50_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_50 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_50/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_50
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_50 <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_50 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_50_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_50_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_50_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_50_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_50_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_50_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_sc_boot_50_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_50_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_sc_boot_50_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_sc_boot_50_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_50 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_50/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_50
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_50 <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_50 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_50_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_50_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_50_low
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_50_low <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_50_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_50_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_sc_boot_50_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_50_up
Country_data_final_2$number_inactivity_people_averted_no_low_sc_boot_50_up <- Country_data_final_2$hipo_number_inactivity_people_no_low_sc_boot_50_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_50 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_50/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_50
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_50 <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_50 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_50_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_50_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_50_low
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_50_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_50_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_50_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_mid_income_boot_50_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_50_up
Country_data_final_2$number_inactivity_people_averted_no_high_mid_income_boot_50_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_mid_income_boot_50_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_50 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_50/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_50
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_50 <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_50 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_50_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_50_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_50_low
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_50_low <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_50_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_50_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_high_low_income_boot_50_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_50_up
Country_data_final_2$number_inactivity_people_averted_no_high_low_income_boot_50_up <- Country_data_final_2$hipo_number_inactivity_people_no_high_low_income_boot_50_up - Country_data_final_2$observed_number_inactivity_people


Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_50 <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_50/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_50
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_50 <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_50 - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_50_low <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_50_low/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_50_low
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_50_low <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_50_low - Country_data_final_2$observed_number_inactivity_people
Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_50_up <- (Country_data_final_2$observed_number_inactivity_people)/(1-(Country_data_final_2$PFP_mid_income_boot_50_up/100)); Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_50_up
Country_data_final_2$number_inactivity_people_averted_no_mid_income_boot_50_up <- Country_data_final_2$hipo_number_inactivity_people_no_mid_income_boot_50_up - Country_data_final_2$observed_number_inactivity_people

