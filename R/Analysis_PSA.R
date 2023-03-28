library(tidyverse)
library(psych)
library(cowplot)
library(gridExtra)
library(plyr)
library(reshape2)
library(ltm)
library(PSAgraphics)

source('R/DataSetup.R')

##### Correlations between page views and success ##############################

ltm::biserial.cor(log(daacs.wgu$PageViews + 1),
				  factor(daacs.wgu$OnTime_Term1),
				  use = c("complete.obs"),
				  level = 2)
cor.test(log(daacs.wgu$PageViews + 1),
		 as.integer(daacs.wgu$OnTime_Term1))

ltm::biserial.cor(log(daacs.ec$PageViews + 1),
				  factor(daacs.ec$SuccessTerm1),
				  use = c("complete.obs"),
				  level = 2)
cor.test(log(daacs.ec$PageViews + 1),
		 as.integer(daacs.ec$SuccessTerm1))


###### Data setup ##############################################################
wgu.ps.formu <- Complier ~ Age + MilitaryStudent + CITIZENSHIP_STATUS +
	EMPLOYMENT_STATUS + FIRST_GEN_STUDENT + GENDER +
	ETHNICITY2 + as.integer(HOUSEHOLD_INCOME) + CURRENT_PROGRAM_CODE +
	TRANSFER_CREDITS

ec.ps.formu <- Complier ~ Age + MilitaryStudent + ENGLISH_LANGUAGE_NATIVE +
	EMPLOYMENT_STATUS + First_Generation + GENDER +
	ETHNICITY + INCOME_RANGE_CODE + DIVISION_CODE +
	Initial_TRANSFER_CREDITS_EARNED

daacs.ec.complete$Retained <- daacs.ec.complete$CreditsAttempted_Term2 > 0 |
	( !is.na(daacs.ec.complete$Time_to_Graduate) & daacs.ec.complete$Time_to_Graduate >= 365.25 )

daacs.wgu.complete$Retained <- daacs.wgu.complete$CreditsAttempted_Term2 > 0

daacs.wgu.complete$Dosage <- sapply(daacs.wgu.complete$TreatLevels, FUN = function(x) {
	switch(x,
		   'Advisor' = 'Assessment Only',
		   'Assessment Only' = 'Assessment Only',
		   'Feedback' = 'Assessment + Feedback',
		   'Feedback and Advisor' = 'Assessment + Feedback',
		   'Treat-No-DAACS' = 'Non-Complier',
		   'Control' = 'Control') })
table(daacs.wgu.complete$TreatLevels, daacs.wgu.complete$Dosage, useNA = 'ifany')
daacs.wgu.complete$Complier <- daacs.wgu.complete$Dosage == 'Assessment + Feedback'
table(daacs.wgu.complete$Dosage, daacs.wgu.complete$Complier, useNA = 'ifany')

# Note sure what to do with the non-compliers (n = 154). These are students who did not
# complete orientation. Unlike EC we have no way of identifying students in the
# control group who did not complete orientation. Excluding them for the PSA
# analysis since 1. we are interested assessment only and feedback students and
# 2. excluding them does not have any impact on the results/conclusions.
daacs.wgu.complete <- daacs.wgu.complete[daacs.wgu.complete$Dosage != 'Non-Complier',]

daacs.ec.complete$Dosage <- sapply(daacs.ec.complete$TreatLevels, FUN = function(x) {
	switch(x,
		   'Assessment Only' = 'Assessment Only',
		   'Control-CCS100' = 'Control',
		   'Control-No-CCS100' = 'Control Never Taker',
		   'Feedback' = 'Assessment + Feedback',
		   'No DAACS' = 'Treatment Never Taker')
})
table(daacs.ec.complete$Dosage, daacs.ec.complete$TreatLevels, useNA = 'ifany')
daacs.ec.complete2 <- daacs.ec.complete |>
	filter(Dosage %in% c('Assessment + Feedback', 'Assessment Only', 'Control'))
daacs.ec.complete2$Complier <- daacs.ec.complete2$Dosage == 'Assessment + Feedback'
table(daacs.ec.complete2$Dosage, daacs.ec.complete2$Complier, useNA = 'ifany')

##### Estimate Propensity Scores
# WGU
lr.out <- glm(wgu.ps.formu,
			  family = binomial(link = 'logit'),
			  data = daacs.wgu.complete[daacs.wgu.complete$Treat,])
summary(lr.out)
daacs.wgu.complete$ps <- predict(lr.out,
								 newdata = daacs.wgu.complete,
								 type = 'response')
ggplot(daacs.wgu.complete, aes(x = ps, color = Dosage)) +
	geom_density()

# EC
ec.lr.out <- glm(ec.ps.formu,
				 family = binomial(link = 'logit'),
				 data = daacs.ec.complete2[daacs.ec.complete2$Treat,])
summary(ec.lr.out)
daacs.ec.complete2$ps <- predict(ec.lr.out,
								 newdata = daacs.ec.complete2,
								 type = 'response')
ggplot(daacs.ec.complete2, aes(x = ps, color = Dosage)) +
	geom_density()

# Stratification breaks
nstrata <- 6
wgu.strata.breaks <- quantile(daacs.wgu.complete$ps, probs = seq(0, 1, 1 / nstrata))
ec.strata.breaks <- quantile(daacs.ec.complete2$ps, probs = seq(0, 1, 1 / nstrata))

wgu.strata.labels <- numeric(length = length(wgu.strata.breaks) - 1)
for(i in 1:length(wgu.strata.labels)) {
	wgu.strata.labels[i] <- mean(wgu.strata.breaks[i:(i+1)])
}
names(wgu.strata.labels) <- LETTERS[1:length(wgu.strata.labels)]
wgu.strata.labels <- as.data.frame(wgu.strata.labels)

ec.strata.labels <- numeric(length = length(ec.strata.breaks) - 1)
for(i in 1:length(ec.strata.labels)) {
	ec.strata.labels[i] <- mean(ec.strata.breaks[i:(i+1)])
}
names(ec.strata.labels) <- LETTERS[1:length(ec.strata.labels)]
ec.strata.labels <- as.data.frame(ec.strata.labels)

table(daacs.wgu.complete$OnTime_Term1, daacs.wgu.complete$Dosage)

ggplot(daacs.wgu.complete[!daacs.wgu.complete$Dosage %in% c('Non-Complier'),],
	   aes(x = ps, y = as.integer(OnTime_Term1), color = Dosage)) +
	geom_vline(xintercept = wgu.strata.breaks, alpha = 0.5) +
	geom_text(data = wgu.strata.labels, label = rownames(wgu.strata.labels), aes(x = wgu.strata.labels), y = 0.05, color = 'black') +
	# geom_point(alpha = 0.1) +
	ylim(c(0,1)) +
	scale_color_brewer('Treatment Group', type = 'qual', palette = 2) +
	geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', span = .8) +
	ylab('On-Time Progress') + xlab('Propensity Scores') +
	ggtitle('On-Time Progress by Propensity Scores and Dosage',
			subtitle = 'Western Govorners University') +
	theme_minimal()
ggsave('Figures/PSA_On_Time_WGU.png', width = 10, height = 5)

ggplot(daacs.wgu.complete[!daacs.wgu.complete$Dosage %in% c('Non-Complier'),],
	   aes(x = ps, y = CreditRatio_Term1, color = Dosage)) +
	geom_vline(xintercept = wgu.strata.breaks, alpha = 0.5) +
	geom_text(data = wgu.strata.labels, label = rownames(wgu.strata.labels), aes(x = wgu.strata.labels), y = 0.05, color = 'black') +
	# geom_point(alpha = 0.01) +
	ylim(c(0,1)) +
	scale_color_brewer('Treatment Group', type = 'qual', palette = 2) +
	geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', span = 1) +
	ylab('Credit Ratio') + xlab('Propensity Scores') +
	ggtitle('Term 1 Credit Ratio by Propensity Scores and Dosage',
			subtitle = 'Western Govorners University') +
	theme_minimal()
ggsave('Figures/PSA_CreditRatio_WGU.png', width = 10, height = 5)

ggplot(daacs.ec.complete2, aes(x = ps, y = as.integer(SuccessTerm1), color = Dosage)) +
	geom_vline(xintercept = ec.strata.breaks, alpha = 0.5) +
	geom_text(data = ec.strata.labels, label = rownames(ec.strata.labels), aes(x = ec.strata.labels), y = 0.05, color = 'black') +
	# geom_point(alpha = 0.1) +
	ylim(c(0,1)) +
	scale_color_brewer('Treatment Group', type = 'qual', palette = 2) +
	geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', span = 1) +
	ylab('On-Time Progress') + xlab('Propensity Scores') +
	ggtitle('On-Time Progress by Propensity Scores and Dosage',
			subtitle = 'Excelsior College') +
	theme_minimal()
ggsave('Figures/PSA_On_Time_EC.png', width = 10, height = 5)

ggplot(daacs.ec.complete2, aes(x = ps, y = CreditRatio_Term1, color = Dosage)) +
	geom_vline(xintercept = ec.strata.breaks, alpha = 0.5) +
	geom_text(data = ec.strata.labels, label = rownames(ec.strata.labels), aes(x = ec.strata.labels), y = 0.05, color = 'black') +
	# geom_point(alpha = 0.1) +
	ylim(c(0,1)) +
	scale_color_brewer('Treatment Group', type = 'qual', palette = 2) +
	geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', span = 1) +
	ylab('Credit Ratio') + xlab('Propensity Scores') +
	ggtitle('Term 1 Credit Ratio by Propensity Scores and Dosage',
			subtitle = 'Excelsior College') +
	theme_minimal()
ggsave('Figures/PSA_CreditRatio_EC.png', width = 10, height = 5)

tmp <- glm(OnTime_Term1 ~ srl_metacognition + srl_strategies + srl_motivation +
		   	mathTotal + writeTotal + readTotal + log(PageViews),
		   # weights = daacs.wgu.complete[daacs.wgu.complete$Treat,]$ps,
		   data = daacs.wgu.complete[daacs.wgu.complete$Treat,],
		   family = binomial(link = 'logit'))
summary(tmp)


##### Stratification

# WGU
daacs.wgu.complete <- daacs.wgu.complete %>%
	mutate(strata = cut(ps,
						breaks = wgu.strata.breaks,
						include.lowest = TRUE,
						labels = letters[1:nstrata]) )
table(daacs.wgu.complete$strata, daacs.wgu.complete$Dosage)

# EC
daacs.ec.complete2 <- daacs.ec.complete2 %>%
	mutate(strata = cut(ps,
						breaks = ec.strata.breaks,
						include.lowest = TRUE,
						labels = letters[1:nstrata]) )
table(daacs.ec.complete2$strata, daacs.ec.complete2$Dosage)


##### Plots
tab <- psych::describeBy(daacs.wgu.complete$OnTime_Term1,
						 group = list(daacs.wgu.complete$strata,
						 			  daacs.wgu.complete$Dosage),
						 mat = TRUE)
ggplot(tab, aes(x = group1, y = mean, color = group2)) +
	# geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1, alpha = 0.6) +
	geom_point(size = 3) +
	theme_minimal() +
	xlab('Strata') + ylab('Percent On-Time Progress') +
	scale_color_brewer('Treatment', type = 'qual', palette = 2) +
	ggtitle('Propensity Score Analysis', subtitle = 'Western Govornors University')


tab.ec <- psych::describeBy(daacs.ec.complete2$SuccessTerm1,
						 group = list(daacs.ec.complete2$strata,
						 			 daacs.ec.complete2$Dosage),
						 mat = TRUE)
tab.ec
ggplot(tab.ec, aes(x = group1, y = mean, color = group2)) +
	# geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1, alpha = 0.6) +
	geom_point(size = 3) +
	theme_minimal() +
	xlab('Strata') + ylab('Percent On-Time Progress') +
	scale_color_brewer('Treatment', type = 'qual', palette = 2) +
	ggtitle('Propensity Score Analysis', subtitle = 'Excelsior College')


##### Groupings ################################################################
# rows1 = Complier vs Non-Complier
# rows2 = Complier vs Control
wgu.rows1 <- daacs.wgu.complete$Dosage %in% c('Assessment + Feedback', 'Assessment Only')
wgu.rows2 <- daacs.wgu.complete$Dosage %in% c('Assessment + Feedback', 'Control')
ec.rows1 <- daacs.ec.complete2$Dosage %in% c('Assessment + Feedback', 'Assessment Only')
ec.rows2 <- daacs.ec.complete2$Dosage %in% c('Assessment + Feedback', 'Control')


##### Balance ##################################################################
source('R/cv.bal.psa.R') # My version to remove title

png('Figures/PSA_balance_wgu_complier_noncomplier.png', width = 700)
par.orig <- par(mai=c(1.2,3,1,0.5))
cv.bal.psa(covariates = cv.trans.psa(daacs.wgu.complete[wgu.rows1, all.vars(wgu.ps.formu)[-1]])[[1]],
		   treatment = daacs.wgu.complete[wgu.rows1,]$Complier,
		   propensity = daacs.wgu.complete[wgu.rows1,]$ps,
		   strata = daacs.wgu.complete[wgu.rows1,]$strata,
		   main = '')
par(par.orig)
dev.off()

png('Figures/PSA_balance_wgu_complier_control.png', width = 700)
par.orig <- par(mai=c(1.2,3,1,0.5))
cv.bal.psa(covariates = cv.trans.psa(daacs.wgu.complete[wgu.rows2, all.vars(wgu.ps.formu)[-1]])[[1]],
		   treatment = daacs.wgu.complete[wgu.rows2,]$Complier,
		   propensity = daacs.wgu.complete[wgu.rows2,]$ps,
		   strata = daacs.wgu.complete[wgu.rows2,]$strata,
		   main = '')
par(par.orig)
dev.off()

png('Figures/PSA_balance_ec_complier_noncomplier.png', width = 700)
par.orig <- par(mai=c(1.2,3,1,0.5))
cv.bal.psa(covariates = cv.trans.psa(daacs.ec.complete2[ec.rows1, all.vars(ec.ps.formu)[-1]])[[1]],
		   treatment = daacs.ec.complete2[ec.rows1,]$Complier,
		   propensity = daacs.ec.complete2[ec.rows1,]$ps,
		   strata = daacs.ec.complete2[ec.rows1,]$strata,
		   main = '')
par(par.orig)
dev.off()

png('Figures/PSA_balance_ec_complier_control.png', width = 700)
par.orig <- par(mai=c(1.2,3,1,0.5))
cv.bal.psa(covariates = cv.trans.psa(daacs.ec.complete2[ec.rows2, all.vars(ec.ps.formu)[-1]])[[1]],
		   treatment = daacs.ec.complete2[ec.rows2,]$Complier,
		   propensity = daacs.ec.complete2[ec.rows2,]$ps,
		   strata = daacs.ec.complete2[ec.rows2,]$strata,
		   main = '')
par(par.orig)
dev.off()


##### Estimate Effect Sizes ####################################################

results_table_strata <- data.frame(
	outcome = character(),
	group = character(),
	institution = character(),
	treatment = numeric(),       
	control = numeric(),
	ATE = numeric(),
	statistic = numeric(),
	p = numeric()
)

##### WGU
##### Complier vs non-complier
# On-Time Progress
wgu.psa1 <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows1,]$OnTime_Term1,
	treatment = daacs.wgu.complete[wgu.rows1,]$Complier,
	# propensity = daacs.wgu.complete[wgu.rows1,]ps,
	strata = daacs.wgu.complete[wgu.rows1,]$strata)
wgu.psa1
# 2 * (1 - pt(wgu.psa1$approx.t, wgu.psa1$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'On-Time Progress',
	group = 'Assessment + Feedback vs. Assessment Only',
	institution = 'Western Governors University',
	treatment = wgu.psa1$wtd.Mn.TRUE,
	control = wgu.psa1$wtd.Mn.FALSE,
	ATE = wgu.psa1$ATE,
	statistic = wgu.psa1$approx.t,
	p = 2 * (1 - pt(wgu.psa1$approx.t, wgu.psa1$df))
))

# Success Rate
wgu.psa1b <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows1,]$CreditRatio_Term1,
	treatment = daacs.wgu.complete[wgu.rows1,]$Complier,
	# propensity = daacs.wgu.complete[wgu.rows1,]ps,
	strata = daacs.wgu.complete[wgu.rows1,]$strata)
wgu.psa1b
# 2 * (1 - pt(wgu.psa1b$approx.t, wgu.psa1b$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Success Rate',
	group = 'Assessment + Feedback vs. Assessment Only',
	institution = 'Western Governors University',
	treatment = wgu.psa1b$wtd.Mn.TRUE,
	control = wgu.psa1b$wtd.Mn.FALSE,
	ATE = wgu.psa1b$ATE,
	statistic = wgu.psa1b$approx.t,
	p = 2 * (1 - pt(wgu.psa1b$approx.t, wgu.psa1b$df))
))

# Retention
wgu.psa1c <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows1,]$Retained,
	treatment = daacs.wgu.complete[wgu.rows1,]$Complier,
	# propensity = daacs.wgu.complete[wgu.rows1,]ps,
	strata = daacs.wgu.complete[wgu.rows1,]$strata)
wgu.psa1c
# 2 * (1 - pt(wgu.psa1c$approx.t, wgu.psa1c$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Retention',
	group = 'Assessment + Feedback vs. Assessment Only',
	institution = 'Western Governors University',
	treatment = wgu.psa1c$wtd.Mn.TRUE,
	control = wgu.psa1c$wtd.Mn.FALSE,
	ATE = wgu.psa1c$ATE,
	statistic = wgu.psa1c$approx.t,
	p = 2 * (1 - pt(wgu.psa1c$approx.t, wgu.psa1c$df))
))

##### Complier vs control
# On-Time Progress
wgu.psa2a <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows2,]$OnTime_Term1,
	treatment = daacs.wgu.complete[wgu.rows2,]$Complier,
	# propensity = daacs.wgu.complete[wgu.rows2,]ps,
	strata = daacs.wgu.complete[wgu.rows2,]$strata)
# 2 * (1 - pt(wgu.psa2a$approx.t, wgu.psa2a$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'On-Time Progress',
	group = 'Assessment + Feedback vs. Control',
	institution = 'Western Governors University',
	treatment = wgu.psa2a$wtd.Mn.TRUE,
	control = wgu.psa2a$wtd.Mn.FALSE,
	ATE = wgu.psa2a$ATE,
	statistic = wgu.psa2a$approx.t,
	p = 2 * (1 - pt(wgu.psa2a$approx.t, wgu.psa2a$df))
))

# Success Rate
wgu.psa2b <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows2,]$CreditRatio_Term1,
	treatment = daacs.wgu.complete[wgu.rows2,]$Complier,
	# propensity = daacs.wgu.complete[wgu.rows2,]ps,
	strata = daacs.wgu.complete[wgu.rows2,]$strata)
wgu.psa2b
# 2 * (1 - pt(wgu.psa2b$approx.t, wgu.psa2b$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Success Rate',
	group = 'Assessment + Feedback vs. Control',
	institution = 'Western Governors University',
	treatment = wgu.psa2b$wtd.Mn.TRUE,
	control = wgu.psa2b$wtd.Mn.FALSE,
	ATE = wgu.psa2b$ATE,
	statistic = wgu.psa2b$approx.t,
	p = 2 * (1 - pt(wgu.psa2b$approx.t, wgu.psa2b$df))
))

# Retention
wgu.psa2c <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows2,]$Retained,
	treatment = daacs.wgu.complete[wgu.rows2,]$Complier,
	# propensity = daacs.wgu.complete[wgu.rows2,]ps,
	strata = daacs.wgu.complete[wgu.rows2,]$strata)
# 2 * (1 - pt(wgu.psa2c$approx.t, wgu.psa2c$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Retention',
	group = 'Assessment + Feedback vs. Control',
	institution = 'Western Governors University',
	treatment = wgu.psa2c$wtd.Mn.TRUE,
	control = wgu.psa2c$wtd.Mn.FALSE,
	ATE = wgu.psa2c$ATE,
	statistic = wgu.psa2c$approx.t,
	p = 2 * (1 - pt(wgu.psa2c$approx.t, wgu.psa2c$df))
))


##### EC
##### Complier vs non-complier
# On-Time Progress
ec.psa1 <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows1,]$SuccessTerm1,
	treatment = daacs.ec.complete2[ec.rows1,]$Complier,
	# propensity = daacs.ec.complete2[ec.rows1,]ps,
	strata = daacs.ec.complete2[ec.rows1,]$strata)
# 2 * (1 - pt(ec.psa1$approx.t, ec.psa1$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'On-Time Progress',
	group = 'Assessment + Feedback vs. Assessment Only',
	institution = 'Excelsior College',
	treatment = ec.psa1$wtd.Mn.TRUE,
	control = ec.psa1$wtd.Mn.FALSE,
	ATE = ec.psa1$ATE,
	statistic = ec.psa1$approx.t,
	p = 2 * (1 - pt(ec.psa1$approx.t, ec.psa1$df))
))

# Success Rate
ec.psa1b <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows1,]$CreditRatio_Term1,
	treatment = daacs.ec.complete2[ec.rows1,]$Complier,
	# propensity = daacs.ec.complete2[ec.rows1,]ps,
	strata = daacs.ec.complete2[ec.rows1,]$strata)
ec.psa1b
# 2 * (1 - pt(ec.psa1b$approx.t, ec.psa1b$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Success Rate',
	group = 'Assessment + Feedback vs. Assessment Only',
	institution = 'Excelsior College',
	treatment = ec.psa1b$wtd.Mn.TRUE,
	control = ec.psa1b$wtd.Mn.FALSE,
	ATE = ec.psa1b$ATE,
	statistic = ec.psa1b$approx.t,
	p = 2 * (1 - pt(ec.psa1b$approx.t, ec.psa1b$df))
))

# Retention
ec.psa1c <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows1,]$Retained,
	treatment = daacs.ec.complete2[ec.rows1,]$Complier,
	# propensity = daacs.ec.complete2[ec.rows1,]ps,
	strata = daacs.ec.complete2[ec.rows1,]$strata)
ec.psa1c
# 2 * (1 - pt(ec.psa1c$approx.t, ec.psa1c$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Retention',
	group = 'Assessment + Feedback vs. Assessment Only',
	institution = 'Excelsior College',
	treatment = ec.psa1c$wtd.Mn.TRUE,
	control = ec.psa1c$wtd.Mn.FALSE,
	ATE = ec.psa1c$ATE,
	statistic = ec.psa1c$approx.t,
	p = 2 * (1 - pt(ec.psa1c$approx.t, ec.psa1c$df))
))


##### Complier vs control
# On-Time Progress
ec.psa2 <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows2,]$SuccessTerm1,
	treatment = daacs.ec.complete2[ec.rows2,]$Complier,
	# propensity = daacs.ec.complete2[ec.rows2,]ps,
	strata = daacs.ec.complete2[ec.rows2,]$strata)
ec.psa2
# 2 * (1 - pt(ec.psa2$approx.t, ec.psa2$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'On-Time Progress',
	group = 'Assessment + Feedback vs. Control',
	institution = 'Excelsior College',
	treatment = ec.psa2$wtd.Mn.TRUE,
	control = ec.psa2$wtd.Mn.FALSE,
	ATE = ec.psa2$ATE,
	statistic = ec.psa2$approx.t,
	p = 2 * (1 - pt(ec.psa2$approx.t, ec.psa2$df))
))

# Success Rate
ec.psa2b <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows2,]$CreditRatio_Term1,
	treatment = daacs.ec.complete2[ec.rows2,]$Complier,
	# propensity = daacs.ec.complete2[ec.rows2,]ps,
	strata = daacs.ec.complete2[ec.rows2,]$strata)
ec.psa2b
# 2 * (1 - pt(ec.psa2b$approx.t, ec.psa2b$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Success Rate',
	group = 'Assessment + Feedback vs. Control',
	institution = 'Excelsior College',
	treatment = ec.psa2b$wtd.Mn.TRUE,
	control = ec.psa2b$wtd.Mn.FALSE,
	ATE = ec.psa2b$ATE,
	statistic = ec.psa2b$approx.t,
	p = 2 * (1 - pt(ec.psa2b$approx.t, ec.psa2b$df))
))

# Retention
ec.psa2c <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows1,]$Retained,
	treatment = daacs.ec.complete2[ec.rows1,]$Complier,
	# propensity = daacs.ec.complete2[ec.rows1,]ps,
	strata = daacs.ec.complete2[ec.rows1,]$strata)
ec.psa2c
# 2 * (1 - pt(ec.psa2c$approx.t, ec.psa2c$df))

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Retention',
	group = 'Assessment + Feedback vs. Control',
	institution = 'Excelsior College',
	treatment = ec.psa2c$wtd.Mn.TRUE,
	control = ec.psa2c$wtd.Mn.FALSE,
	ATE = ec.psa2c$ATE,
	statistic = ec.psa2c$approx.t,
	p = 2 * (1 - pt(ec.psa2c$approx.t, ec.psa2c$df))
))

results_table_strata2 <- merge(
	results_table_strata[results_table_strata$institution == 'Excelsior College',-3],
	results_table_strata[results_table_strata$institution == 'Western Governors University',-3],
	by = c('outcome', 'group'),
	all = TRUE,
	suffixes = c('_EC', '_WGU')
)
results_table_strata2

digits <- 2
for(i in 3:ncol(results_table_strata2)) {
	results_table_strata2[,i] <- sapply(
		results_table_strata2[,i], FUN = function(x) {
			ifelse(x < 1 / 10^digits,
				   paste0('< ', 1 / 10^digits),
				   sprintf("%.2f", x)
			)
		}
	)
}

write.csv(results_table_strata2, 
		  file = 'Tables/PSA_Stratification.csv', 
		  row.names = FALSE,
		  na = '')


##### Matching / Sensitivity ###################################################
library(Matching)
library(rbounds)

results_table_match <- data.frame(
	outcome = character(),
	group = character(),
	institution = character(),
	ATE = numeric(),
	gamma = numeric(),
	statistic = numeric(),
	p = numeric()
)

matching_results <- function(
		df, 
		rows,
		outcome_col,
		outcome_label = outcome_col,
		group = paste0(unique(df[rows,]$Dosage), collapse = ' vs. '),
		treat_col = 'Complier',
		ps_col = 'ps',
		institution = 'Not provided'
) {
	match_out <- Match(Y = df[rows, outcome_col],
						 Tr = df[rows, treat_col, drop = TRUE],
						 X = df[rows,ps_col, drop = TRUE],
						 estimand = 'ATE')
	sensitivity_out <- psens(df[rows,][match_out$index.treated, outcome_col, drop = TRUE],
							   df[rows,][match_out$index.control, outcome_col, drop = TRUE],
							   Gamma = 1.5, GammaInc = 0.01)
	data.frame(
		outcome = outcome_label,
		group = group,
		institution = institution,
		ATE = match_out$est,
		gamma = max(sensitivity_out$bounds[sensitivity_out$bounds$`Upper bound` < 0.05,]$Gamma),
		statistic = match_out$est / match_out$se,
		p = (1 - pnorm(abs(match_out$est / match_out$se))) * 1.96
	)
}

results_table_match <- rbind(
	##### WGU
	matching_results(
		df = daacs.wgu.complete,
		rows = wgu.rows1,
		outcome_col = 'OnTime_Term1',
		outcome_label = 'On-Time Progress',
		institution = 'Western Governors University'
	),

	matching_results(
		df = daacs.wgu.complete,
		rows = wgu.rows2,
		outcome_col = 'OnTime_Term1',
		outcome_label = 'On-Time Progress',
		institution = 'Western Governors University'
	),

	matching_results(
		df = daacs.wgu.complete,
		rows = wgu.rows1,
		outcome_col = 'CreditRatio_Term1',
		outcome_label = 'Success Rate',
		institution = 'Western Governors University'
	),
	
	matching_results(
		df = daacs.wgu.complete,
		rows = wgu.rows2,
		outcome_col = 'CreditRatio_Term1',
		outcome_label = 'Success Rate',
		institution = 'Western Governors University'
	),
	
	matching_results(
		df = daacs.wgu.complete,
		rows = wgu.rows1,
		outcome_col = 'Retained',
		outcome_label = 'Retention',
		institution = 'Western Governors University'
	),
	
	matching_results(
		df = daacs.wgu.complete,
		rows = wgu.rows2,
		outcome_col = 'Retained',
		outcome_label = 'Retention',
		institution = 'Western Governors University'
	),
	
	##### EC
	matching_results(
		df = daacs.ec.complete2,
		rows = ec.rows1,
		outcome_col = 'SuccessTerm1',
		outcome_label = 'On-Time Progress',
		institution = 'Excelsior College'
	),

	matching_results(
		df = daacs.ec.complete2,
		rows = ec.rows2,
		outcome_col = 'SuccessTerm1',
		outcome_label = 'On-Time Progress',
		institution = 'Excelsior College'
	),
	
	matching_results(
		df = daacs.ec.complete2,
		rows = ec.rows1,
		outcome_col = 'CreditRatio_Term1',
		outcome_label = 'Success Rate',
		institution = 'Excelsior College'
	),
	
	matching_results(
		df = daacs.ec.complete2,
		rows = ec.rows2,
		outcome_col = 'CreditRatio_Term1',
		outcome_label = 'Success Rate',
		institution = 'Excelsior College'
	),
	
	matching_results(
		df = daacs.ec.complete2,
		rows = ec.rows1,
		outcome_col = 'Retained',
		outcome_label = 'Retention',
		institution = 'Excelsior College'
	),
	
	matching_results(
		df = daacs.ec.complete2,
		rows = ec.rows2,
		outcome_col = 'Retained',
		outcome_label = 'Retention',
		institution = 'Excelsior College'
	)
)

results_table_match2 <- merge(
	results_table_match[results_table_match$institution == 'Excelsior College',-3],
	results_table_match[results_table_match$institution == 'Western Governors University',-3],
	by = c('outcome', 'group'),
	all = TRUE,
	suffixes = c('_EC', '_WGU')
)
results_table_match2

digits <- 2
for(i in 3:ncol(results_table_match2)) {
	results_table_match2[,i] <- sapply(
		results_table_match2[,i], FUN = function(x) {
			ifelse(x < 1 / 10^digits,
				   paste0('< ', 1 / 10^digits),
				   sprintf("%.2f", x)
			)
		}
	)
}

write.csv(results_table_match2, 
		  file = 'Tables/PSA_Matching.csv', 
		  row.names = FALSE,
		  na = '')

