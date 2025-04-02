library(tidyverse)
library(mice)
library(psych)
source('R/merge.mids.R')

set.seed(2112)
mice_seed = 2112

# Load DAACS data
load('data-raw/DAACS-EC.rda')
load('data-raw/DAACS-WGU.rda')

completion.threshold <- 240 # in seconds
feedback.threshold <- 10

dashboard.pages <- c(
	"/dashboard", "/",
	"/assessments/mathematics","/assessments/mathematics/start","/assessments/mathematics/take",
	"/assessments/college_skills","/assessments/college_skills/start","/assessments/college_skills/take",
	"/assessments/writing","/assessments/writing/start","/assessments/take",
	"/assessments/reading","/assessments/reading/start","/assessments/reading/take"
)


# Remove Master's degree students
daacs.ec$DegreeLevel <- substr(daacs.ec$DEGREE_CODE, 1, 1)
table(daacs.ec$DegreeLevel, useNA = 'ifany')
daacs.ec <- daacs.ec %>% filter(DegreeLevel %in% c('A','B'))
daacs.wgu$DegreeLevel <- substr(daacs.wgu$CURRENT_PROGRAM_CODE, 1, 1)
table(daacs.wgu$DegreeLevel, useNA = 'ifany')
daacs.wgu <- daacs.wgu %>% filter(DegreeLevel %in% c('A','B'))

# Create variable identifying speedy students
daacs.ec$srlTime <- as.integer(daacs.ec$srlCompletionDate - daacs.ec$srlStartDate)
daacs.ec$readTime <- as.integer(daacs.ec$readCompletionDate - daacs.ec$readStartDate)
daacs.ec$mathTime <- as.integer(daacs.ec$mathCompletionDate - daacs.ec$mathStartDate)
daacs.ec$writeTime <- as.integer(daacs.ec$writeCompletionDate - daacs.ec$writeStartDate)
speedy.ec <- (!is.na(daacs.ec$srlTime) & daacs.ec$srlTime <= completion.threshold) |
	(!is.na(daacs.ec$readTime) & daacs.ec$readTime <= completion.threshold) |
	(!is.na(daacs.ec$mathTime) & daacs.ec$mathTime <= completion.threshold)
table(speedy.ec, daacs.ec$Treat, useNA = 'ifany')
daacs.ec$Speedy <- speedy.ec

daacs.wgu$srlTime <- as.integer(daacs.wgu$srlCompletionDate - daacs.wgu$srlStartDate)
daacs.wgu$readTime <- as.integer(daacs.wgu$readCompletionDate - daacs.wgu$readStartDate)
daacs.wgu$mathTime <- as.integer(daacs.wgu$mathCompletionDate - daacs.wgu$mathStartDate)
daacs.wgu$writeTime <- as.integer(daacs.wgu$writeCompletionDate - daacs.wgu$writeStartDate)
speedy.wgu <- (!is.na(daacs.wgu$srlTime) & daacs.wgu$srlTime <= completion.threshold) |
	(!is.na(daacs.wgu$readTime) & daacs.wgu$readTime <= completion.threshold) |
	(!is.na(daacs.wgu$mathTime) & daacs.wgu$mathTime <= completion.threshold)
table(speedy.wgu, daacs.wgu$Treat, useNA = 'ifany')
daacs.wgu$Speedy <- speedy.wgu


##### Setup EC

## Releveling and establishing reference groups

first.gen.levels <- c('00-UNKNOWN', '01-HS_DNF', '02-HS_GRAD')
daacs.ec$First_Generation <- daacs.ec$HIGHEST_ED_LVL_CODE_MOTHER %in% first.gen.levels &
	daacs.ec$HIGHEST_ED_LVL_CODE_FATHER %in% first.gen.levels
daacs.ec[is.na(daacs.ec$HIGHEST_ED_LVL_CODE_MOTHER) & is.na(daacs.ec$HIGHEST_ED_LVL_CODE_FATHER),]$First_Generation <- NA
# head(daacs.ec[,c('HIGHEST_ED_LVL_CODE_MOTHER', 'HIGHEST_ED_LVL_CODE_FATHER', 'First_Generation')])

daacs.ec$MilitaryStudent <- daacs.ec$ACTIVE_MIL_STUDENT | daacs.ec$MVET

#daacs.ec$Military <- NA
#daacs.ec[which(daacs.ec$ACTIVE_MIL_STUDENT == TRUE & daacs.ec$MVET == TRUE),]$Military <- 'FALSE'
#daacs.ec[which(daacs.ec$ACTIVE_MIL_STUDENT == TRUE & daacs.ec$MVET == FALSE),]$Military <- 'TRUE'
#daacs.ec[which(daacs.ec$ACTIVE_MIL_STUDENT == FALSE & daacs.ec$MVET == TRUE),]$Military <- 'FALSE'
#daacs.ec[which(daacs.ec$ACTIVE_MIL_STUDENT == FALSE & daacs.ec$MVET == FALSE),]$Military <- 'FALSE'

#EMPLOYMENT_LVL_CODE RECODING

daacs.ec$EMPLOYMENT_STATUS <- NA
daacs.ec[which(daacs.ec$EMPLOYMENT_LVL_CODE == 1),]$EMPLOYMENT_STATUS <- 'Unemployed'
daacs.ec[which(daacs.ec$EMPLOYMENT_LVL_CODE == 2),]$EMPLOYMENT_STATUS <- 'Part-Time'
daacs.ec[which(daacs.ec$EMPLOYMENT_LVL_CODE == 3 | daacs.ec$EMPLOYMENT_LVL_CODE == 4),]$EMPLOYMENT_STATUS <- 'Full-Time'
daacs.ec$EMPLOYMENT_STATUS <- as.factor(daacs.ec$EMPLOYMENT_STATUS)

# str(daacs.ec[,all.vars(ec.formu)])
table(daacs.ec$EMPLOYMENT_STATUS)
daacs.ec$EMPLOYMENT_STATUS <- factor(as.character(daacs.ec$EMPLOYMENT_STATUS),
									 levels = c('Unemployed', 'Part-Time', 'Full-Time'))
daacs.ec$EMPLOYMENT_STATUS <- relevel(daacs.ec$EMPLOYMENT_STATUS, ref = 'Unemployed')
table(daacs.ec$INCOME_RANGE_CODE)
daacs.ec$INCOME_RANGE_CODE <- as.integer(daacs.ec$INCOME_RANGE_CODE)
table(daacs.ec$ETHNICITY)
daacs.ec$ETHNICITY <- relevel(daacs.ec$ETHNICITY, ref = 'White')
table(daacs.ec$GENDER)
daacs.ec$GENDER <- relevel(daacs.ec$GENDER, ref = 'MALE')
daacs.ec$DIVISION_CODE <- as.factor(daacs.ec$DIVISION_CODE)
table(daacs.ec$DIVISION_CODE)
daacs.ec$DIVISION_CODE <- relevel(daacs.ec$DIVISION_CODE, ref = 'LA')


## Demographics

table(daacs.ec$GENDER, daacs.ec$Treat, useNA = "ifany")
table(daacs.ec$ETHNICITY, daacs.ec$Treat, useNA = "ifany")
table(daacs.ec$First_Generation, daacs.ec$Treat, useNA = "ifany")
table(daacs.ec$MilitaryStudent, daacs.ec$Treat, useNA = "ifany")
table(daacs.ec$EMPLOYMENT_STATUS, daacs.ec$Treat, useNA = "ifany")
table(daacs.ec$ENGLISH_LANGUAGE_NATIVE, daacs.ec$Treat, useNA = "ifany")
table(daacs.ec$DegreeLevel, daacs.ec$Treat, useNA = "ifany")
table(daacs.ec$INCOME_RANGE_CODE, daacs.ec$Treat, useNA = "ifany")

# Coding events data

events.students.ec$FeedbackPage <- !events.students.ec$url.base %in% dashboard.pages
# table(events.students.ec$FeedbackPage, useNA = 'ifany') %>% print %>% prop.table

events.students.ec.pageviews <- as.data.frame(table(events.students.ec$DAACS_ID))
names(events.students.ec.pageviews) <- c('DAACS_ID', 'PageViews')

events.students.ec.feedbackviews <- as.data.frame(table(
	events.students.ec[events.students.ec$FeedbackPage,]$DAACS_ID))
names(events.students.ec.feedbackviews) <- c('DAACS_ID', 'FeedbackViews')

daacs.ec <- merge(daacs.ec, events.students.ec.pageviews, by = 'DAACS_ID', all.x = TRUE)
daacs.ec <- merge(daacs.ec, events.students.ec.feedbackviews, by = 'DAACS_ID', all.x = TRUE)

daacs.ec[is.na(daacs.ec$PageViews),]$PageViews <- 0
daacs.ec[is.na(daacs.ec$FeedbackViews),]$FeedbackViews <- 0

daacs.ec$AdvisorUsed <- daacs.ec$DAACS_ID %in% unique(events.advisors.ec[!is.na(events.advisors.ec$DAACS_ID),]$DAACS_ID)
# table(daacs.ec$AdvisorUsed, useNA = 'ifany') %>% print %>% prop.table

daacs.ec$TreatLevels <- NA
daacs.ec[!daacs.ec$Treat & !daacs.ec$CompletedCCS100,]$TreatLevels <- 'Control-No-CCS100'
daacs.ec[!daacs.ec$Treat & daacs.ec$CompletedCCS100,]$TreatLevels <- 'Control-CCS100'
daacs.ec[which(daacs.ec$Treat & #!daacs.ec$AdvisorUsed &
			   	!daacs.ec$FeedbackViews > feedback.threshold),]$TreatLevels <- 'Assessment Only'
daacs.ec[which(daacs.ec$Treat & #!daacs.ec$AdvisorUsed &
			   	daacs.ec$FeedbackViews > feedback.threshold),]$TreatLevels <- 'Feedback'
# daacs.ec[which(daacs.ec$Treat & daacs.ec$AdvisorUsed &
# 					!daacs.ec$FeedbackViews > feedback.threshold),]$TreatLevels <- 'Advisor'
# daacs.ec[which(daacs.ec$Treat & daacs.ec$AdvisorUsed &
# 			   	daacs.ec$FeedbackViews > feedback.threshold),]$TreatLevels <- 'Advisor and Feedback'
# ec.speedy <- daacs.ec$Treat &
# 	(is.na(daacs.ec$srlTime) | daacs.ec$srlTime <= completion.threshold) &
# 	(is.na(daacs.ec$readTime) | daacs.ec$readTime <= completion.threshold) &
# 	(is.na(daacs.ec$mathTime) | daacs.ec$mathTime <= completion.threshold)
# daacs.ec[which(ec.speedy),]$TreatLevels <- 'Fast Completers'
daacs.ec[daacs.ec$Treat & is.na(daacs.ec$srlTotal) & is.na(daacs.ec$mathTotal) &
		 	is.na(daacs.ec$readTotal) & is.na(daacs.ec$writeTotal), ]$TreatLevels <- 'No DAACS'

# table(daacs.ec$TreatLevels, useNA = 'ifany')

# Impute missing values
ec.cols <- c('Age', 'MilitaryStudent', 'ENGLISH_LANGUAGE_NATIVE',
			 	'EMPLOYMENT_STATUS', 'First_Generation', 'GENDER',
			 	'ETHNICITY', 'INCOME_RANGE_CODE', 'DIVISION_CODE',
			 	'Initial_TRANSFER_CREDITS_EARNED')


ec.mice.out <- mice(daacs.ec[,ec.cols], m = 1, seed = mice_seed)
daacs.ec.complete <- merge(ec.mice.out, daacs.ec, shadow.matrix = TRUE)

##### Setup WGU

## Releveling and establishing reference groups

table(daacs.wgu$EMPLOYMENT_STATUS)
# daacs.wgu$EMPLOYMENT_STATUS <- as.factor(daacs.wgu$EMPLOYMENT_STATUS)
# daacs.wgu$EMPLOYMENT_STATUS <- relevel(daacs.wgu$EMPLOYMENT_STATUS, ref = 'Unemployed')
table(daacs.wgu$CITIZENSHIP_STATUS)
daacs.wgu$CITIZENSHIP_STATUS <- relevel(daacs.wgu$CITIZENSHIP_STATUS, ref = 'U.S. Citizen')
table(daacs.wgu$ETHNICITY2)
daacs.wgu$ETHNICITY2 <- relevel(daacs.wgu$ETHNICITY2, ref = 'White')
table(daacs.wgu$GENDER)
daacs.wgu$GENDER <- relevel(daacs.wgu$GENDER, ref = 'Male')
table(daacs.wgu$CURRENT_PROGRAM_CODE, useNA = 'ifany')
program.tab <- as.data.frame(table(daacs.wgu$CURRENT_PROGRAM_CODE, useNA = 'ifany'))
small_programs <- as.character(program.tab[program.tab$Freq < 100,]$Var1)
daacs.wgu[daacs.wgu$CURRENT_PROGRAM_CODE %in% small_programs,]$CURRENT_PROGRAM_CODE <- 'Other'
daacs.wgu$CURRENT_PROGRAM_CODE <- as.factor(daacs.wgu$CURRENT_PROGRAM_CODE)
daacs.wgu$CURRENT_PROGRAM_CODE <- relevel(daacs.wgu$CURRENT_PROGRAM_CODE, ref = 'BSMG')
daacs.wgu$HOUSEHOLD_INCOME <- as.integer(daacs.wgu$HOUSEHOLD_INCOME)

daacs.wgu$EMPLOYMENT_STATUS <- factor(as.character(daacs.wgu$EMPLOYMENT_STATUS),
									 levels = c('Unemployed', 'Part-Time', 'Full-Time'))
daacs.wgu$EMPLOYMENT_STATUS <- relevel(daacs.wgu$EMPLOYMENT_STATUS, ref = 'Unemployed')

## Demographics

table(daacs.wgu$GENDER, daacs.wgu$Treat, useNA = "ifany")
table(daacs.wgu$ETHNICITY, daacs.wgu$Treat, useNA = "ifany")
table(daacs.wgu$FIRST_GEN_STUDENT, daacs.wgu$Treat, useNA = "ifany")
table(daacs.wgu$MilitaryStudent, daacs.wgu$Treat, useNA = "ifany")
table(daacs.wgu$EMPLOYMENT_STATUS, daacs.wgu$Treat, useNA = "ifany")
table(daacs.wgu$CITIZENSHIP_STATUS, daacs.wgu$Treat, useNA = "ifany")
table(daacs.wgu$DegreeLevel, daacs.wgu$Treat, useNA = "ifany")
table(daacs.wgu$HOUSEHOLD_INCOME, daacs.wgu$Treat, useNA = "ifany")

## Coding event data

events.students.wgu$FeedbackPage <- !events.students.wgu$url.base %in% dashboard.pages
# table(events.students.wgu$FeedbackPage, useNA = 'ifany') %>% print %>% prop.table


daacs.wgu$TreatLevels <- NA
daacs.wgu[which(!daacs.wgu$Treat),]$TreatLevels <- 'Control'
# daacs.wgu[which(!daacs.wgu$Treat & !daacs.wgu$Completed_Orientation),]$TreatLevels <- 'Control-No-ORA1'
daacs.wgu[which(daacs.wgu$Treat),]$TreatLevels <- 'Treat-No-DAACS'
daacs.wgu[which(daacs.wgu$Treat & !daacs.wgu$AdvisorUsed &
					!daacs.wgu$FeedbackViews > feedback.threshold),]$TreatLevels <- 'Assessment Only'
daacs.wgu[which(daacs.wgu$Treat & !daacs.wgu$AdvisorUsed &
					daacs.wgu$FeedbackViews > feedback.threshold),]$TreatLevels <- 'Feedback'
daacs.wgu[which(daacs.wgu$Treat & daacs.wgu$AdvisorUsed &
					!daacs.wgu$FeedbackViews > feedback.threshold),]$TreatLevels <- 'Advisor'
daacs.wgu[which(daacs.wgu$Treat & daacs.wgu$AdvisorUsed &
					daacs.wgu$FeedbackViews > feedback.threshold),]$TreatLevels <- 'Feedback and Advisor'

# Impute missing values
wgu.cols <- c("Age", "MilitaryStudent", "CITIZENSHIP_STATUS", "EMPLOYMENT_STATUS",
			  "FIRST_GEN_STUDENT", "GENDER", "ETHNICITY2", "HOUSEHOLD_INCOME",
			  "CURRENT_PROGRAM_CODE", "TRANSFER_CREDITS")
wgu.mice.out <- mice(daacs.wgu[,wgu.cols], m = 1, seed = mice_seed)
daacs.wgu.complete <- merge(wgu.mice.out, daacs.wgu, shadow.matrix = TRUE)

