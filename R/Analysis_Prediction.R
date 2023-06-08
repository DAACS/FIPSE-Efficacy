###### RQ#3 Predictive Modeling

wgu.formu <-  ~ Age + MilitaryStudent + CITIZENSHIP_STATUS + 
	EMPLOYMENT_STATUS + FIRST_GEN_STUDENT + GENDER +
	ETHNICITY2 + HOUSEHOLD_INCOME + CURRENT_PROGRAM_CODE +
	TRANSFER_CREDITS

ec.formu <- ~ INCOME_RANGE_CODE + EMPLOYMENT_LVL_CODE + ENGLISH_LANGUAGE_NATIVE +
	First_Generation + ETHNICITY +
	GENDER + Age + ACTIVE_MIL_STUDENT + MVET + DIVISION_CODE +
	Initial_TRANSFER_CREDITS_EARNED #+ Initial_OVERALL_GPA

# Model with DAACS results
wgu.formu.daacs <- update.formula(wgu.formu, ~ . + 
								  	mathTotal + readTotal + writeTotal +
								  	srl_grit + srl_strategies + srl_metacognition +
								  	srl_self_efficacy + srl_mindset + srl_mastery_orientation + srl_anxiety +
								  	# cluster + 
								  	FeedbackViews)

ec.formu.daacs <- update.formula(ec.formu, ~ . + 
								 	mathTotal + readTotal + writeTotal +
								 	srl_grit + srl_strategies + srl_metacognition +
								 	srl_self_efficacy + srl_mindset + srl_mastery_orientation + srl_anxiety +
								 	cluster + 
								 	FeedbackViews)

daacs.ec.treat <- daacs.ec %>% 
	mutate(srl_time = as.integer(srlCompletionDate - srlStartDate)) %>%
	filter(CCS100_Start < as.Date('2018-08-01') &
		   	#StatusAtCCS100Start == 'EN',
		   	#srl_time > 60 * 5,
		   	srlAttempts > 0)

nrow(daacs.ec.treat)

daacs.wgu.treat <- daacs.wgu %>% 
	mutate(srl_time = as.integer(srlCompletionDate - srlStartDate)) %>%
	filter(
		#StatusAtCCS100Start == 'EN',
		#srl_time > 60 * 5,
		srlAttempts > 0)

names(daacs.ec)
names(coursework.ec)
coursework.ec2 <- merge(coursework.ec, 
						daacs.ec[,c('DAACS_ID', 'CCS100_Start')],
						by = 'DAACS_ID', all.x = TRUE)
coursework.ec2 <- coursework.ec2 %>%
	mutate(DaysFromCCS = COURSE_START_DATE - CCS100_Start) %>%
	filter(DaysFromCCS >= -7 & DaysFromCCS <= 90) %>%
	arrange(DaysFromCCS) %>%
	filter(!duplicated(DAACS_ID))
daacs.ec2 <- merge(daacs.ec,
				   coursework.ec2,
				   by = 'DAACS_ID', all.x = TRUE)
daacs.ec2 <- daacs.ec2 %>%
	mutate(CoursePass = GRADE_CODE %in% c('P','A','B','C'))

table(daacs.ec2$Treat, daacs.ec2$CoursePass, useNA = 'ifany')

daacs.ec2.treat <- daacs.ec2 %>% filter(Treat) %>% 
	mutate(srl_time = srlCompletionDate - srlStartDate)


# Outcomes to test: CoursePass, Success6Months, CreditRatio_6months, CreditRatio_1year, Success1Year

lr.out <- glm(CreditRatio_1year ~ StatusAtCCS100Start + srl_time + #cluster +
			  	srl_grit + 
			  	srl_anxiety + srl_mastery_orientation + srl_mindset + srl_self_efficacy +
			  	srl_strategies + srl_metacognition +
			  	mathTotal + writeTotal + readTotal,
			  data = daacs.ec2.treat,
			  family = binomial())
summary(lr.out)

lr.out <- glm(CreditRatio_1year ~ StatusAtCCS100Start + #srl_time + #cluster +
			  	srl_grit + 
			  	srl_anxiety + srl_mastery_orientation + srl_mindset + srl_self_efficacy +
			  	srl_managing_time + srl_help_seeking + srl_managing_environment + srl_managing_time +
			  	srl_planning + srl_monitoring + srl_evaluation +
			  	mathTotal + writeTotal + readTotal,
			  data = daacs.ec2.treat,
			  family = binomial())
summary(lr.out)


# outcomes to test: OnTime_Term1, OnTime_Term2, CreditRatio_Term1, CreditRatio_Term2, OnTime_Year1

lr.out <- glm(OnTime_Term1 ~ + #log10(srl_time) + #cluster +
			  	srl_grit + 
			  	srl_anxiety + srl_mastery_orientation + srl_mindset + srl_self_efficacy +
			  	srl_strategies + srl_metacognition +
			  	mathTotal + writeTotal + readTotal,
			  data = daacs.ec2.treat,
			  family = binomial())
summary(lr.out)

lr.out1.wgu  <- glm(OnTime_Year1 ~ + #log10(srl_time) + #cluster +
						srl_grit + 
						srl_anxiety + srl_mastery_orientation + srl_mindset + srl_self_efficacy +
						srl_managing_time + srl_help_seeking + srl_managing_environment + srl_managing_time +
						srl_planning + srl_monitoring + srl_evaluation +
						mathTotal + writeTotal + readTotal,
					data = daacs.wgu.treat,
					family = binomial())
summary(lr.out1.wgu )


##### Piecewise Regression (or whatever we are calling it)

library(mice)
cols <- c('INCOME_RANGE_CODE', "EMPLOYMENT_LVL_CODE",
		  "ENGLISH_LANGUAGE_NATIVE", "HIGHEST_ED_LVL_CODE_MOTHER",
		  "HIGHEST_ED_LVL_CODE_FATHER", "ETHNICITY", "GENDER", "Age", 
		  "ACTIVE_MIL_STUDENT", "MVET")
mice.out <- mice(daacs.ec[,cols], m = 1)

daacs.ec.complete <- cbind(daacs.ec[,!(names(daacs.ec) %in% cols)], 
						   complete(mice.out))
daacs.ec.complete <- daacs.ec.complete[daacs.ec.complete$Treat,]
table(daacs.ec.complete$Treat)

set.seed(2112)
train.rows <- sample(nrow(daacs.ec.complete), nrow(daacs.ec.complete) * .75)
daacs.ec.train <- daacs.ec.complete[train.rows,]
daacs.ec.valid <- daacs.ec.complete[-train.rows,]

## create UpsetR figure

daacs.ec.complete <- daacs.ec.complete %>% 
	mutate(SRL_complete = as.integer(!is.na(srlTotal)),
		   Write_complete = as.integer(!is.na(writeTotal)),
		   Math_complete = as.integer(!is.na(mathTotal)),
		   Read_complete = as.integer(!is.na(readTotal)),
		   feedback_view = as.integer(!is.na(FeedbackViews) &
		   						   	(FeedbackViews > feedback.threshold)))


tmp <- daacs.ec.complete[,c('SRL_complete','Write_complete',
							'Math_complete', 'Read_complete', 'feedback_view')]

tmp$SRL_complete <- recode(tmp$SRL_complete, '0' = NULL, '1' = "SRL")
tmp$Write_complete <- recode(tmp$Write_complete, '0' = NULL, '1' = "Write")
tmp$Read_complete <- recode(tmp$Read_complete, '0' = NULL, '1' = "Read")
tmp$Math_complete <- recode(tmp$Math_complete, '0' = NULL, '1' = "Math")
tmp$feedback_view <- recode(tmp$feedback_view, '0' = NULL, '1' = "Feedback")

DT <- data.table(tmp)

tmp1.1 <- apply(DT,1,function(x){
	data.frame(Assessment=paste(na.omit(x[1:5]), collapse=' '))
})

tmp1.1 <- do.call(rbind,tmp1.1)

tmp1.1 %>% 
	ggplot(aes(x=fct_infreq(Assessment))) +
	geom_bar() +
	geom_text(stat='count', aes(label=..count..), vjust=-1, size=3) +
	scale_x_mergelist(sep = " ") +
	axis_combmatrix(sep = " ")


### Prediction

formula.base <- Success6Months ~ INCOME_RANGE_CODE + EMPLOYMENT_LVL_CODE +
	ENGLISH_LANGUAGE_NATIVE + HIGHEST_ED_LVL_CODE_MOTHER +
	HIGHEST_ED_LVL_CODE_FATHER + ETHNICITY + GENDER + Age +
	ACTIVE_MIL_STUDENT + MVET

# base model

lr.out <- glm(update.formula(formula.base, Success6Months ~ .),
			  data = daacs.ec.train, 
			  family = binomial(link = 'logit'))
summary(lr.out)

daacs.ec.valid$lr_prediction <- predict(lr.out, newdata = daacs.ec.valid)
median(daacs.ec.valid$lr_prediction)

cm <- table(daacs.ec.valid$Success6Months, 
			daacs.ec.valid$lr_prediction > median(daacs.ec.valid$lr_prediction)) %>% prop.table
cm
(p.base <- cm['TRUE','TRUE'] + cm['FALSE','FALSE'])

# Model 1: All assessment and Feedback
# Model 2: All assessments only
# Model 3: SRL only
# Model 4: No DAACS info
models <- list()


tmp1 <- daacs.ec.train %>%
	filter(!is.na(srlTotal) & !is.na(writeTotal) & !is.na(readTotal) & !is.na(mathTotal) & !is.na(FeedbackViews) &
		   	(FeedbackViews > feedback.threshold))
tmp2 <- daacs.ec.train %>%
	filter(!DAACS_ID %in% tmp1 & !is.na(srlTotal) & !is.na(writeTotal) & !is.na(readTotal) & !is.na(mathTotal))
tmp3 <- daacs.ec.train %>%
	filter(!DAACS_ID %in% tmp1 & !DAACS_ID %in% tmp2 & !is.na(srlTotal))
tmp4 <- daacs.ec.train %>%
	filter(!DAACS_ID %in% tmp1 & !DAACS_ID %in% tmp2 & !DAACS_ID %in% tmp3)


models$model1 <- glm(update.formula(formula.base, ~ . + 
										srlTotal + 
										writeTotal + readTotal + mathTotal + FeedbackViews), 
					 data = tmp1,
					 family = binomial(link = 'logit'))

models$model2 <- glm(update.formula(formula.base, ~ . + 
										srlTotal + 
										writeTotal + readTotal + mathTotal), 
					 data = tmp2,
					 family = binomial(link = 'logit'))

models$model3 <- glm(update.formula(formula.base, ~ . + 
										srl_strategies + srl_metacognition + 
										srl_anxiety + srl_mastery_orientation + srl_mindset + srl_self_efficacy), 
					 data = tmp3,
					 family = binomial(link = 'logit'))

models$model4 <- glm(formula.base, 
					 data = tmp4,
					 family = binomial(link = 'logit'))

summary(models$model1)
summary(models$model2)
summary(models$model3)
summary(models$model4)

get_prediction <- function(df) {
	tmp1 <- !is.na(df$srlTotal) & 
		!is.na(df$writeTotal) & 
		!is.na(df$readTotal) &
		!is.na(df$mathTotal)
	tmp2 <- !tmp1 & 
		!is.na(df$srlTotal) & 
		!is.na(df$readTotal) & 
		!is.na(df$mathTotal)
	tmp3 <- !tmp1 & !tmp2 & 
		!is.na(df$srlTotal)
	tmp4 <- !tmp1 & !tmp2 & !tmp3
	
	df$Prediction <- NA
	df[tmp1,]$Prediction <- predict(models$model1, newdata = df[tmp1,], type = "response")
	df[tmp2,]$Prediction <- predict(models$model2, newdata = df[tmp2,], type = "response")
	df[tmp3,]$Prediction <- predict(models$model3, newdata = df[tmp3,], type = "response")
	df[tmp4,]$Prediction <- predict(models$model4, newdata = df[tmp4,], type = "response")
	
	df$Prediction_Class <- df$Prediction > 0.5 # TODO: Allow different cut point
	return(df)
}

valid.out <- get_prediction(daacs.ec.valid)
hist(valid.out$Prediction)

table(valid.out$Success6Months, useNA = 'ifany') %>% prop.table()
cm <- table(valid.out$Prediction_Class, valid.out$Success6Months, useNA = 'ifany') %>% prop.table
cm
p.daacs <- cm[1,1] + cm[2,2]

p.daacs - p.base
