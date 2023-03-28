source('R/DataSetup.R')
library(ggpubr)

fig.width <- 10
fig.height <- 5
errorbar.cv <- 1 # Multiplier for the standard error bars.

# Note: This analysis script includes results of success in term 2. These were
# not included in the final paper since we observed null results in term 1.
# Retention was reported since it was an outcome in the grant proposal.

# This will separate out students who attended orientation. This effects but
# treatment and control groups and descriptives suggest students who did not 
# attempt orienation are statistically equivelent on key demographics.
ec.completed.orientation <- daacs.ec$TreatLevels %in%
	c('Control-CCS100', # Control students who attempted orientation
	  'Assessment Only', # Treatment students that completed the assessments
	  'Feedback') # Treatment students that viewed DAACS feedback
daacs.ec2 <- daacs.ec[ec.completed.orientation,]


###### Research Question #1: Overall Effects ###################################
###### Null hypothesis tests                 ###################################

########## EC
# On-time progress
# All students
table(daacs.ec$Treat, daacs.ec$SuccessTerm1, useNA = 'ifany')%>%
	print() %>% prop.table(1)
( chi.ec_term1 <- chisq.test(daacs.ec$Treat, daacs.ec$SuccessTerm1, correct = FALSE) )

##### Students who attended orientation
table(daacs.ec2$Treat, daacs.ec2$SuccessTerm1, useNA = 'ifany')%>%
	print() %>% prop.table(1)
( chi.ec2_term1 <- chisq.test(daacs.ec2$Treat, daacs.ec2$SuccessTerm1, correct = FALSE) )


##### Success term 2
# All Students
table(daacs.ec$Treat, daacs.ec$SuccessTerm2, useNA = 'ifany')%>%
	print() %>% prop.table(1)
( chi.ec_term2 <- chisq.test(daacs.ec$Treat, daacs.ec$SuccessTerm2, correct = FALSE) )

( ttest.ec_term1 <- t.test(CreditRatio_Term1 ~ Treat, data = daacs.ec) )
( ttest.ec_term2 <- t.test(CreditRatio_Term2 ~ Treat, data = daacs.ec) )

# Students who attended orientation
table(daacs.ec2$Treat, daacs.ec2$SuccessTerm2, useNA = 'ifany')%>%
	print() %>% prop.table(1)
( chi.ec2_term2 <- chisq.test(daacs.ec2$Treat, daacs.ec2$SuccessTerm2, correct = FALSE) )

( ttest.ec2_term1 <- t.test(CreditRatio_Term1 ~ Treat, data = daacs.ec2) )
( ttest.ec2_term2 <- t.test(CreditRatio_Term2 ~ Treat, data = daacs.ec2) )


##### Retention
# All students
daacs.ec$Retained <- daacs.ec$CreditsAttempted_Term2 > 0 |
	( !is.na(daacs.ec$Time_to_Graduate) & daacs.ec$Time_to_Graduate >= 365.25 )
table(daacs.ec$Treat, daacs.ec$CreditsAttempted_Term2 > 0, useNA = 'ifany')%>%
	print() %>% prop.table(1)
( chi.ec_retention <- chisq.test(daacs.ec$Treat, 
							   daacs.ec$Retained, 
							   correct = FALSE) )

# Students who attended orientation
daacs.ec2$Retained <- daacs.ec2$CreditsAttempted_Term2 > 0 |
	( !is.na(daacs.ec2$Time_to_Graduate) & daacs.ec2$Time_to_Graduate >= 365.25 )
table(daacs.ec2$Treat, daacs.ec2$CreditsAttempted_Term2 > 0, useNA = 'ifany')%>%
	print() %>% prop.table(1)
( chi.ec_retention <- chisq.test(daacs.ec2$Treat, 
								 daacs.ec2$Retained, 
								 correct = FALSE) )

########## WGU
table(daacs.wgu$Treat, daacs.wgu$OnTime_Term1, useNA = 'ifany')%>%
	print() %>% prop.table(1)
( chi.wgu1 <- chisq.test(daacs.wgu$Treat, daacs.wgu$OnTime_Term1, correct = FALSE) )

table(daacs.wgu$Treat, daacs.wgu$OnTime_Term2, useNA = 'ifany') %>%
	print() %>% prop.table(1)
( chi.wgu2 <- chisq.test(daacs.wgu$Treat, daacs.wgu$OnTime_Term2, correct = FALSE) )

# chisq.test(daacs.wgu$Treat, daacs.wgu$CreditsEarned_Term2 >= 9)

( ttest.wgu_term1 <- t.test(CreditRatio_Term1 ~ Treat, data = daacs.wgu) )
( ttest.wgu_term2 <- t.test(CreditRatio_Term2 ~ Treat, data = daacs.wgu) )

##### Figures ##################################################################
########## EC
# 6 Months On-Time Progress

tab.ec <- describeBy(daacs.ec$SuccessTerm1, group = daacs.ec$TreatLevels, mat = TRUE, skew = FALSE)
tab.ec$Treatment <- !tab.ec$group1 %in% c('Control-CCS100', 'Control-No-CCS100')

p.ec.ontime_term1 <- ggplot(tab.ec, aes(x = group1, y = mean, 
										ymin = mean - errorbar.cv * se, ymax = mean + errorbar.cv * se, 
										color = Treatment)) +
	geom_errorbar(width = .5) +
	geom_point() +
	geom_text(aes(label = paste0(round(100 * mean, digits = 1), '%')), hjust = -0.25, color = 'black') +
	geom_text(aes(label = paste0('n = ', n)), y = 0.0, color = 'black') +
	xlab('') + ylab('Percent Successful\n(earned three credits within six months)') +
	ylim(c(0.0, 0.9)) +
	scale_x_discrete(limits = c(#'Advisor and Feedback',
		'Feedback',
		'Assessment Only', 
		'No DAACS',
		'Control-CCS100' ,
		'Control-No-CCS100'
	),
	labels = c(#'Assessments +\nFeedback +\nAdvisor',
		'Assessments + \nFeedback', 
		'Assessments Only', 
		'Treatment \nNo DAACS/Orientation',
		'Control',
		'Control\nNo Orientation'
	)) +
	scale_color_brewer('Assigned to\nTreatment', type = 'div', palette = 'Dark2', direction = -1) +
	ggtitle('Percent of students making on-time progress at six months in Term 1',
		'Excelsior College'
	) +
	theme_minimal()
p.ec.ontime_term1
ggsave('Figures/EC_OnTime_Term1.png', width = fig.width, height = fig.height)

# Figure - On-Time Progress, Term 2

tab.ec <- describeBy(daacs.ec$SuccessTerm2, group = daacs.ec$TreatLevels, mat = TRUE, skew = FALSE)
tab.ec$Treatment <- !tab.ec$group1 %in% c('Control-CCS100', 'Control-No-CCS100')
p.ec.ontime_term2 <- ggplot(tab.ec, aes(x = group1, y = mean, 
										ymin = mean - errorbar.cv * se, ymax = mean + errorbar.cv * se, 
										color = Treatment)) +
	geom_errorbar(width = .5) +
	geom_point() +
	geom_text(aes(label = paste0(round(100 * mean, digits = 1), '%')), hjust = -0.25, color = 'black') +
	geom_text(aes(label = paste0('n = ', n)), y = 0.0, color = 'black') +
	xlab('') + ylab('Percent Successful\n(earned three credits within six months)') +
	ylim(c(0.0, 0.9)) +
	scale_x_discrete(limits = c(#'Advisor and Feedback',
		'Feedback',
		#'Advisor', 
		'Assessment Only', 
		'No DAACS',
		'Control-CCS100' ,
		'Control-No-CCS100'
	),
	labels = c(#'Assessments +\nFeedback +\nAdvisor',
		'Assessments + \nFeedback', 
		#'Assessment + \nAdvisor',
		'Assessments Only', 
		'Treatment \nNo DAACS/Orientation',
		'Control',
		'Control\nNo Orientation'
	)) +
	scale_color_brewer('Assigned to\nTreatment', type = 'div', palette = 'Dark2', direction = -1) +
	ggtitle('Percent of students making on-time progress at six months in Term 2',
		'Excelsior College'
		# 'Term 2'
		# subtitle = 'Institution A'
	) +
	theme_minimal()
p.ec.ontime_term2
ggsave('Figures/EC_OnTime_Term2.png', width = fig.width, height = fig.height)

# Combine plots
ggarrange(p.ec.ontime_term1, p.ec.ontime_term2 + rremove("x.text"), 
		  labels = c("", ""),
		  ncol = 1, nrow = 2)

## Figure - 6 Months Credit Ratio

# Term 1
tab.ec <- describeBy(daacs.ec$CreditRatio_Term1, group = daacs.ec$TreatLevels, mat = TRUE, skew = FALSE)
tab.ec$Treatment <- !tab.ec$group1 %in% c('Control-CCS100', 'Control-No-CCS100')
p.ec.creditratio_term1 <- ggplot(tab.ec, aes(x = group1, y = mean, ymin = mean - errorbar.cv * se, ymax = mean + errorbar.cv * se, color = Treatment)) +
	geom_errorbar(width = .5) +
	geom_point() +
	geom_text(aes(label = round(mean, digits = 2)), hjust = -0.25, color = 'black') +
	geom_text(aes(label = paste0('n = ', n)), y = 0.0, color = 'black') +
	xlab('') + ylab('Credits Earned-to-Attempted\n within Six Months') +
	ylim(c(0.0, 1.0)) +
	scale_x_discrete(limits = c(#'Advisor and Feedback',
		'Feedback',
		'Assessment Only', 
		'No DAACS',
		'Control-CCS100' ,
		'Control-No-CCS100'
	),
	labels = c(#'Assessments +\nFeedback +\nAdvisor',
		'Assessments + \nFeedback', 
		'Assessments Only', 
		'Treatment \nNo DAACS/Orientation',
		'Control',
		'Control\nNo Orientation'
	)) +
	scale_color_brewer('Assigned to\nTreatment', type = 'div', palette = 'Dark2', direction = -1) +
	ggtitle('Credits earned-to-attempted within six months in Term 1',
		'Excelsior College') +	
	theme_minimal()
p.ec.creditratio_term1
ggsave('Figures/EC_CreditRation_Term1.png', width = fig.width, height = fig.height)


tab.ec <- describeBy(daacs.ec$CreditRatio_Term2, group = daacs.ec$TreatLevels, mat = TRUE, skew = FALSE)
tab.ec$Treatment <- !tab.ec$group1 %in% c('Control-CCS100', 'Control-No-CCS100')
p.ec.creditratio_term2 <- ggplot(tab.ec, aes(x = group1, y = mean, ymin = mean - errorbar.cv * se, ymax = mean + errorbar.cv * se, color = Treatment)) +
	geom_errorbar(width = .5) +
	geom_point() +
	geom_text(aes(label = round(mean, digits = 2)), hjust = -0.25, color = 'black') +
	geom_text(aes(label = paste0('n = ', n)), y = 0.0, color = 'black') +
	xlab('') + ylab('Credits Earned-to-Attempted\n within Six Months') +
	ylim(c(0.0, 1.0)) +
	scale_x_discrete(limits = c(#'Advisor and Feedback',
		'Feedback',
		'Assessment Only', 
		'No DAACS',
		'Control-CCS100' ,
		'Control-No-CCS100'
	),
	labels = c(#'Assessments +\nFeedback +\nAdvisor',
		'Assessments + \nFeedback', 
		'Assessments Only', 
		'Treatment \nNo DAACS/Orientation',
		'Control',
		'Control\nNo Orientation'
	)) +
	scale_color_brewer('Assigned to\nTreatment', type = 'div', palette = 'Dark2', direction = -1) +
	ggtitle('Credits earned-to-attempted within six months in Term 2',
		'Excelsior College') +	
	theme_minimal()
p.ec.creditratio_term2
ggsave('Figures/EC_CreditRation_Term2.png', width = fig.width, height = fig.height)

ggarrange(p.ec.creditratio_term1, p.ec.creditratio_term2 + rremove("x.text"), 
		  labels = c("", ""),
		  ncol = 1, nrow = 2)


# Retention

table(daacs.ec$Treat, daacs.ec$CreditsAttempted_Term2 > 0, useNA = 'ifany')
tab.ec <- describeBy(daacs.ec$CreditsAttempted_Term2 > 0, 
					 group = daacs.ec$TreatLevels, 
					 mat = TRUE, skew = FALSE)
tab.ec$Treatment <- !tab.ec$group1 %in% c('Control-CCS100', 'Control-No-CCS100')
p.ec.ontime <- ggplot(tab.ec, aes(x = group1, y = mean, 
								  ymin = mean - errorbar.cv * se, ymax = mean + errorbar.cv * se, 
								  color = Treatment)) +
	geom_errorbar(width = .5) +
	geom_point() +
	geom_text(aes(label = paste0(round(100 * mean, digits = 1), '%')), hjust = -0.25, color = 'black') +
	geom_text(aes(label = paste0('n = ', n)), y = 0.0, color = 'black') +
	xlab('') + ylab('Percent Retained\n(registered for courses in Term 2)') +
	ylim(c(0.0, 1.0)) +
	scale_x_discrete(limits = c(#'Advisor and Feedback',
		'Feedback',
		'Assessment Only', 
		'No DAACS',
		'Control-CCS100' ,
		'Control-No-CCS100'
	),
	labels = c(#'Assessments +\nFeedback +\nAdvisor',
		'Assessments + \nFeedback', 
		'Assessments Only', 
		'Treatment \nNo DAACS/Orientation',
		'Control',
		'Control\nNo Orientation'
	)) +
	scale_color_brewer('Assigned to\nTreatment', type = 'div', palette = 'Dark2', direction = -1) +
	ggtitle('Term-to-Term Retention',
		subtitle = 'Excelsior College') +
	theme_minimal()
p.ec.ontime
ggsave('Figures/EC_Retention.png', width = fig.width, height = fig.height)

########## WGU
# Figure - On-time progress - 6 months
tab.wgu.overall <- describeBy(daacs.wgu$OnTime_Term1, group = daacs.wgu$Treat, 
							  mat = TRUE, skew = FALSE) #Term1 outcome variable
tab.wgu.overall$Treatment <- tab.wgu.overall$group1 == 'TRUE'
tab.wgu <- describeBy(daacs.wgu$OnTime_Term1, group = daacs.wgu$TreatLevels, mat = TRUE, skew = FALSE)
tab.wgu$Treatment <- !tab.wgu$group1 %in% c('Control')
p.wgu.ontime1 <- ggplot(tab.wgu, aes(x = group1, y = mean, 
									 ymin = mean - errorbar.cv * se, ymax = mean + errorbar.cv * se, 
									 color = Treatment)) +
	geom_errorbar(width = .5) +
	geom_point() +
	geom_text(aes(label = paste0(round(100 * mean, digits = 1), '%')), hjust = -0.25, color = 'black') +
	ylim(c(0.0, 1.0)) +
	geom_text(aes(label = paste0('n = ', n)), y = 0.0, color = 'black') +
	xlab('') + ylab('Percent Successful\n(earned twelve credits within six months)') +
	scale_x_discrete(limits = c(
		'Feedback and Advisor', 
		'Feedback',
		'Advisor',
		'Assessment Only', 
		'Treat-No-DAACS',
		'Control'
	),
	labels = c( 
		'Assessments +\nFeedback +\nAdvisor', 
		'Assessments +\nFeedback',
		'Assessments +\nAdvisor',
		'Assessments\nOnly', 
		'No DAACS',
		'Control'
	)) +
	scale_color_brewer('Assigned to\nTreatment', type = 'div', palette = 'Dark2', direction = -1) +
	ggtitle('Percent of students making on-time progress in Term 1', 
		'Western Governors University') +	
	theme_minimal()
p.wgu.ontime1
ggsave('Figures/WGU_OnTime_Term1.png', width = fig.width, heigth = fig.height)

tab.wgu.overall <- describeBy(daacs.wgu$OnTime_Term1, group = daacs.wgu$Treat, 
							  mat = TRUE, skew = FALSE) #Term1 outcome variable

tab.wgu.overall$Treatment <- tab.wgu.overall$group1 == 'TRUE'
tab.wgu <- describeBy(daacs.wgu$OnTime_Term2, group = daacs.wgu$TreatLevels, mat = TRUE, skew = FALSE)
tab.wgu$Treatment <- !tab.wgu$group1 %in% c('Control')
p.wgu.ontime2 <- ggplot(tab.wgu, aes(x = group1, y = mean, 
									 ymin = mean - errorbar.cv * se, ymax = mean + errorbar.cv * se, 
									 color = Treatment)) +
	geom_errorbar(width = .5) +
	geom_point() +
	geom_text(aes(label = paste0(round(100 * mean, digits = 1), '%')), hjust = -0.25, color = 'black') +
	ylim(c(0.0, 1.0)) +
	geom_text(aes(label = paste0('n = ', n)), y = 0.0, color = 'black') +
	xlab('') + ylab('Percent Successful\n(earned twelve credits within six months)') +
	scale_x_discrete(limits = c(
		'Feedback and Advisor', 
		'Feedback',
		'Advisor',
		'Assessment Only', 
		'Treat-No-DAACS',
		'Control'),
		labels = c( 
			'Assessments +\nFeedback +\nAdvisor', 
			'Assessments +\nFeedback',
			'Assessments +\nAdvisor',
			'Assessments\nOnly', 
			'No DAACS',
			'Control')) +
	scale_color_brewer('Assigned to\nTreatment', type = 'div', palette = 'Dark2', direction = -1) +
	ggtitle('Percent of students making on-time progress in Term 2', 
			subtitle = 'Western Governors University') +
	theme_minimal()
p.wgu.ontime2
ggsave('Figures/WGU_OnTime_Term2.png', width = fig.width, height = fig.height)

ggarrange(p.wgu.ontime1, p.wgu.ontime2 + rremove("x.text"), 
		  labels = c("", ""),
		  ncol = 1, nrow = 2)

# Figure - Credit Ratio
# Term 1
tab.wgu <- describeBy(daacs.wgu$CreditRatio_Term1, group = daacs.wgu$TreatLevels, mat = TRUE, skew = FALSE)
tab.wgu$Treatment <- !tab.wgu$group1 %in% c('Control')
p.wgu.creditratio1 <- ggplot(tab.wgu, aes(x = group1, y = mean, ymin = mean - errorbar.cv * se, ymax = mean + errorbar.cv * se, color = Treatment)) +
	geom_errorbar(width = .5) +
	geom_point() +
	geom_text(aes(label = round(mean, digits = 2)), hjust = -0.25, color = 'black') +
	ylim(c(0.0, 1.0)) +
	geom_text(aes(label = paste0('n = ', n)), y = 0.0, color = 'black') +
	xlab('') + ylab('Credits Earned-to-Attempted \nwithin Six Months') +
	scale_x_discrete(limits = c(
		'Feedback and Advisor', 
		'Feedback',
		'Advisor',
		'Assessment Only', 
		'Treat-No-DAACS',
		'Control'),
		labels = c( 
			'Assessments +\nFeedback +\nAdvisor', 
			'Assessments +\nFeedback',
			'Assessments +\nAdvisor',
			'Assessments\nOnly', 
			'No DAACS',
			'Control')) +
	scale_color_brewer('Assigned to\nTreatment', type = 'div', palette = 'Dark2', direction = -1) +
	ggtitle('Credits earned-to-attempted within six months in Term 1', 
			subtitle = 'Western Governors University') +
	theme_minimal()
p.wgu.creditratio1
ggsave('Figures/WGU_CreditRatio_Term1.png', width = fig.width, height = fig.height)

# Term 2

tab.wgu <- describeBy(daacs.wgu$CreditRatio_Term2, group = daacs.wgu$TreatLevels, mat = TRUE, skew = FALSE)
tab.wgu$Treatment <- !tab.wgu$group1 %in% c('Control')
p.wgu.creditratio2 <- ggplot(tab.wgu, aes(x = group1, y = mean, ymin = mean - errorbar.cv * se, ymax = mean + errorbar.cv * se, color = Treatment)) +
	geom_errorbar(width = .5) +
	geom_point() +
	geom_text(aes(label = round(mean, digits = 2)), hjust = -0.25, color = 'black') +
	ylim(c(0.0, 1.0)) +
	geom_text(aes(label = paste0('n = ', n)), y = 0.0, color = 'black') +
	xlab('') + ylab('Credits Earned-to-Attempted\n within Six Months') +
	scale_x_discrete(limits = c(
		'Feedback and Advisor', 
		'Feedback',
		'Advisor',
		'Assessment Only', 
		'Treat-No-DAACS',
		'Control'),
		labels = c( 
			'Assessments +\nFeedback +\nAdvisor', 
			'Assessments +\nFeedback',
			'Assessments +\nAdvisor',
			'Assessments\nOnly', 
			'No DAACS',
			'Control')) +
	scale_color_brewer('Assigned to\nTreatment', type = 'div', palette = 'Dark2', direction = -1) +
	ggtitle('Credits earned-to-attempted within six months in Term 2', 
			subtitle = 'Western Governors University') +
	theme_minimal()
p.wgu.creditratio2
ggsave('Figures/WGU_CreditRatio_Term2.png', width = fig.width, height = fig.height)


ggarrange(p.wgu.creditratio1, p.wgu.creditratio2 + rremove("x.text"), 
		  labels = c("", ""),
		  ncol = 1, nrow = 2)

# Retention
tab.wgu <- describeBy(daacs.wgu$CreditsAttempted_Term2 > 0, 
					  group = daacs.wgu$TreatLevels, 
					  mat = TRUE, skew = FALSE)
tab.wgu$Treatment <- !tab.wgu$group1 %in% c('Control')
p.wgu.retention <- ggplot(tab.wgu, aes(x = group1, y = mean, 
									   ymin = mean - errorbar.cv * se, ymax = mean + errorbar.cv * se, 
									   color = Treatment)) +
	geom_errorbar(width = .5) +
	geom_point() +
	geom_text(aes(label = paste0(round(100 * mean, digits = 1), '%')), hjust = -0.25, color = 'black') +
	ylim(c(0.0, 1.0)) +
	geom_text(aes(label = paste0('n = ', n)), y = 0.0, color = 'black') +
	xlab('') + ylab('Percent Retained\n(registered for courses in Term 2)') +
	scale_x_discrete(limits = c(
		'Feedback and Advisor', 
		'Feedback',
		'Advisor',
		'Assessment Only', 
		'Treat-No-DAACS',
		'Control'),
		labels = c( 
			'Assessments +\nFeedback +\nAdvisor', 
			'Assessments +\nFeedback',
			'Assessments +\nAdvisor',
			'Assessments\nOnly', 
			'No DAACS',
			'Control')) +
	scale_color_brewer('Assigned to\nTreatment', type = 'div', palette = 'Dark2', direction = -1) +
	ggtitle('Percent of students making term-to-term retention',
			subtitle = 'Western Governors University') +
	theme_minimal()
p.wgu.retention
ggsave('Figures/WGU_Retention.png', width = fig.width, height = fig.height)
