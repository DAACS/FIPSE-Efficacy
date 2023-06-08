library(tidyverse)
library(psych)
library(cowplot)
library(gridExtra)
library(plyr)
library(reshape2)
library(ltm)
library(PSAgraphics)

source('R/DataSetup.R')

daacs.ec$Retained <- daacs.ec$CreditsAttempted_Term2 > 0 | !is.na(daacs.ec$Time_to_Graduate)
daacs.ec.complete$Retained <- daacs.ec.complete$CreditsAttempted_Term2 > 0 | !is.na(daacs.ec.complete$Time_to_Graduate)

daacs.wgu$Retained <- daacs.wgu$CreditsAttempted_Term2 > 0
daacs.wgu.complete$Retained <- daacs.wgu.complete$CreditsAttempted_Term2 > 0

##### Correlations between page views and success ##############################

##### On-Time Progress
# WGU
ltm::biserial.cor(log(daacs.wgu$PageViews + 1),
				  factor(daacs.wgu$OnTime_Term1),
				  use = c("complete.obs"),
				  level = 2)
wgu.cor.ontime <- cor.test(log(daacs.wgu$PageViews + 1),
		 as.integer(daacs.wgu$OnTime_Term1))

# EC
ltm::biserial.cor(log(daacs.ec$PageViews + 1),
				  factor(daacs.ec$SuccessTerm1),
				  use = c("complete.obs"),
				  level = 2)
ec.cor.ontime <- cor.test(log(daacs.ec$PageViews + 1),
		 as.integer(daacs.ec$SuccessTerm1))

##### Success Rate
# WGU
wgu.cor.creditratio <- cor.test(log(daacs.wgu$PageViews + 1),
		 as.integer(daacs.wgu$CreditRatio_Term1))

# EC
ec.cor.creditratio <- cor.test(log(daacs.ec$PageViews + 1),
		 as.integer(daacs.ec$CreditRatio_Term1))

##### Retention
# WGU
ltm::biserial.cor(log(daacs.wgu$PageViews + 1),
				  factor(daacs.wgu$Retained),
				  use = c("complete.obs"),
				  level = 2)
wgu.cor.retained <-  cor.test(log(daacs.wgu$PageViews + 1),
		 as.integer(daacs.wgu$Retained))

# EC
ltm::biserial.cor(log(daacs.ec$PageViews + 1),
				  factor(daacs.ec$Retained),
				  use = c("complete.obs"),
				  level = 2)
ec.cor.retained <- cor.test(log(daacs.ec$PageViews + 1),
		 as.integer(daacs.ec$Retained))

correlation_table <- data.frame(
	Outcome = c('On-Time Progress', 'Retention', 'Success Rate'),
	EC_Correlation = c(ec.cor.ontime$estimate, ec.cor.retained$estimate, ec.cor.creditratio$estimate),
	EC_t = c(ec.cor.ontime$statistic, ec.cor.retained$statistic, ec.cor.creditratio$statistic),
	EC_df = c(ec.cor.ontime$parameter, ec.cor.retained$parameter, ec.cor.creditratio$parameter),
	EC_p = c(ec.cor.ontime$p.value, ec.cor.retained$p.value, ec.cor.creditratio$p.value),
	WGU_Correlation = c(wgu.cor.ontime$estimate, wgu.cor.retained$estimate, wgu.cor.creditratio$estimate),
	WGU_t = c(wgu.cor.ontime$statistic, wgu.cor.retained$statistic, wgu.cor.creditratio$statistic),
	WGU_df = c(wgu.cor.ontime$parameter, wgu.cor.retained$parameter, wgu.cor.creditratio$parameter),
	WGU_p = c(wgu.cor.ontime$p.value, wgu.cor.retained$p.value, wgu.cor.creditratio$p.value)
)

digits <- 2
for(i in 2:ncol(correlation_table)) {
	correlation_table[,i] <- sapply(
		correlation_table[,i], FUN = function(x) {
			ifelse(x < 1 / 10^digits,
				   paste0('< ', 1 / 10^digits),
				   sprintf(paste0("%.", digits, "f"), x)
			)
		}
	)
}

correlation_table
write.csv(correlation_table, file = 'Tables/Correlations.csv', row.names = FALSE)

###### Data setup ##############################################################
wgu.ps.formu <- Complier ~ Age + MilitaryStudent + CITIZENSHIP_STATUS +
	EMPLOYMENT_STATUS + FIRST_GEN_STUDENT + GENDER +
	ETHNICITY2 + as.integer(HOUSEHOLD_INCOME) + CURRENT_PROGRAM_CODE +
	TRANSFER_CREDITS

ec.ps.formu <- Complier ~ Age + MilitaryStudent + ENGLISH_LANGUAGE_NATIVE +
	EMPLOYMENT_STATUS + First_Generation + GENDER +
	ETHNICITY + INCOME_RANGE_CODE + DIVISION_CODE +
	Initial_TRANSFER_CREDITS_EARNED

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
	ylab('Success Rate') + xlab('Propensity Scores') +
	ggtitle('Success Rate by Propensity Scores and Dosage',
			subtitle = 'Western Govorners University') +
	theme_minimal()
ggsave('Figures/PSA_CreditRatio_WGU.png', width = 10, height = 5)

ggplot(daacs.wgu.complete[!daacs.wgu.complete$Dosage %in% c('Non-Complier'),],
	  aes(x = ps, y = as.integer(Retained), color = Dosage)) +
	geom_vline(xintercept = wgu.strata.breaks, alpha = 0.5) +
	geom_text(data = wgu.strata.labels, label = rownames(wgu.strata.labels), aes(x = wgu.strata.labels), y = 0.05, color = 'black') +
	# geom_point(alpha = 0.01) +
	ylim(c(0,1)) +
	scale_color_brewer('Treatment Group', type = 'qual', palette = 2) +
	geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', span = 1) +
	ylab('Retention') + xlab('Propensity Scores') +
	ggtitle('Term-to-Term Retention Rate by Propensity Scores and Dosage',
			subtitle = 'Western Govorners University') +
	theme_minimal()
ggsave('Figures/PSA_Retention_WGU.png', width = 10, height = 5)



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
	ylab('Success Rate') + xlab('Propensity Scores') +
	ggtitle('Success Rate by Propensity Scores and Dosage',
			subtitle = 'Excelsior College') +
	theme_minimal()
ggsave('Figures/PSA_CreditRatio_EC.png', width = 10, height = 5)

ggplot(daacs.ec.complete2, aes(x = ps, y = as.integer(Retained), color = Dosage)) +
	geom_vline(xintercept = ec.strata.breaks, alpha = 0.5) +
	geom_text(data = ec.strata.labels, label = rownames(ec.strata.labels), aes(x = ec.strata.labels), y = 0.05, color = 'black') +
	# geom_point(alpha = 0.1) +
	ylim(c(0,1)) +
	scale_color_brewer('Treatment Group', type = 'qual', palette = 2) +
	geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', span = 1) +
	ylab('Retention') + xlab('Propensity Scores') +
	ggtitle('Term-to-Term Retention  Rate by Propensity Scores and Dosage',
			subtitle = 'Excelsior College') +
	theme_minimal()
ggsave('Figures/PSA_Retention_EC.png', width = 10, height = 5)

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
wgu.rows3 <- daacs.wgu.complete$Dosage %in% c('Assessment Only', 'Control')
ec.rows1 <- daacs.ec.complete2$Dosage %in% c('Assessment + Feedback', 'Assessment Only')
ec.rows2 <- daacs.ec.complete2$Dosage %in% c('Assessment + Feedback', 'Control')
ec.rows3 <- daacs.ec.complete2$Dosage %in% c('Assessment Only', 'Control')


##### Balance ##################################################################
source('R/cv.bal.psa.R') # My version to remove title

png('Figures/PSA_balance_wgu_complier_noncomplier.png', width = 700)
par.orig <- par(mai=c(1.2,3,1,0.5))
balance_noncomplier_wgu <- cv.bal.psa(covariates = cv.trans.psa(daacs.wgu.complete[wgu.rows1, all.vars(wgu.ps.formu)[-1]])[[1]],
		   treatment = daacs.wgu.complete[wgu.rows1,]$Complier,
		   propensity = daacs.wgu.complete[wgu.rows1,]$ps,
		   strata = daacs.wgu.complete[wgu.rows1,]$strata,
		   main = '')
par(par.orig)
dev.off()

png('Figures/PSA_balance_wgu_complier_control.png', width = 700)
par.orig <- par(mai=c(1.2,3,1,0.5))
balance_control_wgu <- cv.bal.psa(covariates = cv.trans.psa(daacs.wgu.complete[wgu.rows2, all.vars(wgu.ps.formu)[-1]])[[1]],
		   treatment = daacs.wgu.complete[wgu.rows2,]$Complier,
		   propensity = daacs.wgu.complete[wgu.rows2,]$ps,
		   strata = daacs.wgu.complete[wgu.rows2,]$strata,
		   main = '')
par(par.orig)
dev.off()

balance_wgu <- merge(
	balance_noncomplier_wgu$effect.sizes |> as.data.frame() |> rownames_to_column() |> dplyr::select(rowname, stES_unadj, stES_adj),
	balance_control_wgu$effect.sizes |> as.data.frame() |> rownames_to_column() |> dplyr::select(rowname, stES_unadj, stES_adj),
	by = 'rowname',
	all = TRUE,
	suffixes = c('noncomplier', 'control')
)
write.csv(balance_wgu, 'Tables/Balance_WGU.csv', row.names = FALSE)

png('Figures/PSA_balance_ec_complier_noncomplier.png', width = 700)
par.orig <- par(mai=c(1.2,3,1,0.5))
balance_noncomplier_ec <- cv.bal.psa(covariates = cv.trans.psa(daacs.ec.complete2[ec.rows1, all.vars(ec.ps.formu)[-1]])[[1]],
		   treatment = daacs.ec.complete2[ec.rows1,]$Complier,
		   propensity = daacs.ec.complete2[ec.rows1,]$ps,
		   strata = daacs.ec.complete2[ec.rows1,]$strata,
		   main = '')
par(par.orig)
dev.off()

png('Figures/PSA_balance_ec_complier_control.png', width = 700)
par.orig <- par(mai=c(1.2,3,1,0.5))
balance_control_ec <- cv.bal.psa(covariates = cv.trans.psa(daacs.ec.complete2[ec.rows2, all.vars(ec.ps.formu)[-1]])[[1]],
		   treatment = daacs.ec.complete2[ec.rows2,]$Complier,
		   propensity = daacs.ec.complete2[ec.rows2,]$ps,
		   strata = daacs.ec.complete2[ec.rows2,]$strata,
		   main = '')
par(par.orig)
dev.off()

balance_ec <- merge(
	balance_noncomplier_ec$effect.sizes |> as.data.frame() |> rownames_to_column() |> dplyr::select(rowname, stES_unadj, stES_adj),
	balance_control_ec$effect.sizes |> as.data.frame() |> rownames_to_column() |> dplyr::select(rowname, stES_unadj, stES_adj),
	by = 'rowname',
	all = TRUE,
	suffixes = c('noncomplier', 'control')
)
write.csv(balance_ec, 'Tables/Balance_EC.csv', row.names = FALSE)

##### Estimate Effect Sizes ####################################################

results_table_strata <- data.frame(
	outcome = character(),
	group = character(),
	institution = character(),
	treatment = numeric(),       
	control = numeric(),
	ATE = numeric(),
	statistic = numeric(),
	df = integer(),
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
	df = wgu.psa1$df,
	p = 2 * (1 - pt(abs(wgu.psa1$approx.t), wgu.psa1$df))
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
	df = wgu.psa1b$df,
	p = 2 * (1 - pt(abs(wgu.psa1b$approx.t), wgu.psa1b$df))
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
	df = wgu.psa1c$df,
	p = 2 * (1 - pt(abs(wgu.psa1c$approx.t), wgu.psa1c$df))
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
	df = wgu.psa2a$df,
	p = 2 * (1 - pt(abs(wgu.psa2a$approx.t), wgu.psa2a$df))
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
	df = wgu.psa2b$df,
	p = 2 * (1 - pt(abs(wgu.psa2b$approx.t), wgu.psa2b$df))
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
	df = wgu.psa2c$df,
	p = 2 * (1 - pt(abs(wgu.psa2c$approx.t), wgu.psa2c$df))
))


##### Non-Complier vs control
# On-Time Progress
wgu.psa3a <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows3,]$OnTime_Term1,
	treatment = daacs.wgu.complete[wgu.rows3,]$Dosage == 'Assessment Only',
	# propensity = daacs.wgu.complete[wgu.rows3,]ps,
	strata = daacs.wgu.complete[wgu.rows3,]$strata)
wgu.psa3a

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'On-Time Progress',
	group = 'Assessment Only vs. Control',
	institution = 'Western Governors University',
	treatment = wgu.psa3a$wtd.Mn.TRUE,
	control = wgu.psa3a$wtd.Mn.FALSE,
	ATE = wgu.psa3a$ATE,
	statistic = wgu.psa3a$approx.t,
	df = wgu.psa3a$df,
	p = 2 * (1 - pt(abs(wgu.psa3a$approx.t), wgu.psa3a$df))
))

# Success Rate
wgu.psa3b <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows3,]$CreditRatio_Term1,
	treatment = daacs.wgu.complete[wgu.rows3,]$Dosage == 'Assessment Only',
	# propensity = daacs.wgu.complete[wgu.rows3,]ps,
	strata = daacs.wgu.complete[wgu.rows3,]$strata)
wgu.psa3b

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Success Rate',
	group = 'Assessment Only vs. Control',
	institution = 'Western Governors University',
	treatment = wgu.psa3b$wtd.Mn.TRUE,
	control = wgu.psa3b$wtd.Mn.FALSE,
	ATE = wgu.psa3b$ATE,
	statistic = wgu.psa3b$approx.t,
	df = wgu.psa3b$df,
	p = 2 * (1 - pt(abs(wgu.psa3b$approx.t), wgu.psa3b$df))
))

# Retention
wgu.psa3c <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows3,]$Retained,
	treatment = daacs.wgu.complete[wgu.rows3,]$Dosage == 'Assessment Only',
	# propensity = daacs.wgu.complete[wgu.rows3,]ps,
	strata = daacs.wgu.complete[wgu.rows3,]$strata)
wgu.psa3c

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Retention',
	group = 'Assessment Only vs. Control',
	institution = 'Western Governors University',
	treatment = wgu.psa3c$wtd.Mn.TRUE,
	control = wgu.psa3c$wtd.Mn.FALSE,
	ATE = wgu.psa3c$ATE,
	statistic = wgu.psa3c$approx.t,
	df = wgu.psa3c$df,
	p = 2 * (1 - pt(abs(wgu.psa3c$approx.t), wgu.psa3c$df))
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
	df = ec.psa1$df,
	p = 2 * (1 - pt(abs(ec.psa1$approx.t), ec.psa1$df))
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
	df = ec.psa1b$df,
	p = 2 * (1 - pt(abs(ec.psa1b$approx.t), ec.psa1b$df))
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
	df = ec.psa1c$df,
	p = 2 * (1 - pt(abs(ec.psa1c$approx.t), ec.psa1c$df))
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
	df = ec.psa2$df,
	p = 2 * (1 - pt(abs(ec.psa2$approx.t), ec.psa2$df))
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
	df = ec.psa2b$df,
	p = 2 * (1 - pt(abs(ec.psa2b$approx.t), ec.psa2b$df))
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
	df = ec.psa2c$df,
	p = 2 * (1 - pt(abs(ec.psa2c$approx.t), ec.psa2c$df))
))

##### Non-Complier vs control
# On-Time Progress
ec.psa3a <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows3,]$SuccessTerm1,
	treatment = daacs.ec.complete2[ec.rows3,]$Dosage == 'Assessment Only',
	# propensity = daacs.ec.complete2[ec.rows3,]ps,
	strata = daacs.ec.complete2[ec.rows3,]$strata)
ec.psa3a

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'On-Time Progress',
	group = 'Assessment Only vs. Control',
	institution = 'Excelsior College',
	treatment = ec.psa3a$wtd.Mn.TRUE,
	control = ec.psa3a$wtd.Mn.FALSE,
	ATE = ec.psa3a$ATE,
	statistic = ec.psa3a$approx.t,
	df = ec.psa3a$df,
	p = 2 * (1 - pt(abs(ec.psa3a$approx.t), ec.psa3a$df))
))

# Success Rate
ec.psa3b <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows3,]$CreditRatio_Term1,
	treatment = daacs.ec.complete2[ec.rows3,]$Dosage == 'Assessment Only',
	# propensity = daacs.ec.complete2[ec.rows3,]ps,
	strata = daacs.ec.complete2[ec.rows3,]$strata)
ec.psa3b

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Success Rate',
	group = 'Assessment Only vs. Control',
	institution = 'Excelsior College',
	treatment = ec.psa3b$wtd.Mn.TRUE,
	control = ec.psa3b$wtd.Mn.FALSE,
	ATE = ec.psa3b$ATE,
	statistic = ec.psa3b$approx.t,
	df = ec.psa3b$df,
	p = 2 * (1 - pt(abs(ec.psa3b$approx.t), ec.psa3b$df))
))

# Retention
ec.psa3c <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows3,]$Retained,
	treatment = daacs.ec.complete2[ec.rows3,]$Dosage == 'Assessment Only',
	# propensity = daacs.ec.complete2[ec.rows3,]ps,
	strata = daacs.ec.complete2[ec.rows3,]$strata)
ec.psa3c

results_table_strata <- rbind(results_table_strata, data.frame(
	outcome = 'Retention',
	group = 'Assessment Only vs. Control',
	institution = 'Excelsior College',
	treatment = ec.psa3c$wtd.Mn.TRUE,
	control = ec.psa3c$wtd.Mn.FALSE,
	ATE = ec.psa3c$ATE,
	statistic = ec.psa3c$approx.t,
	df = ec.psa3c$df,
	p = 2 * (1 - pt(abs(ec.psa3c$approx.t), ec.psa3c$df))
))

######

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
			sprintf("%.2f", x)
			# ifelse(x < 1 / 10^digits,
			# 	   paste0('< ', 1 / 10^digits),
			# 	   sprintf("%.2f", x)
			# )
		}
	)
}

results_table_strata2$p_EC <- ifelse(as.numeric(results_table_strata2$p_EC) < 1 / 10^digits,
									paste0('< ', 1 / 10^digits),
									results_table_strata2$p_EC)
results_table_strata2$p_WGU <- ifelse(as.numeric(results_table_strata2$p_WGU) < 1 / 10^digits,
									 paste0('< ', 1 / 10^digits),
									 results_table_strata2$p_WGU)


results_table_strata2$statistic_EC <- paste0('t', as.integer(results_table_strata2$df_EC), ' = ', results_table_strata2$statistic_EC)
results_table_strata2$df_EC <- NULL
results_table_strata2$statistic_WGU <- paste0('t', as.integer(results_table_strata2$df_WGU), ' = ', results_table_strata2$statistic_WGU)
results_table_strata2$df_WGU <- NULL

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
	statistic = numeric(),
	df = integer(),
	p = numeric(),
	gamma = numeric()
)

matching_results <- function(
		df, 
		rows,
		outcome_col,
		outcome_label = outcome_col,
		group = paste0(unique(df[rows,]$Dosage), collapse = ' vs. '),
		treat_col = 'Complier',
		ps_col = 'ps',
		replace = TRUE,
		ties = TRUE,
		institution = 'Not provided'
) {
	match_out <- Match(Y = df[rows, outcome_col],
						 Tr = df[rows, treat_col, drop = TRUE],
						 X = df[rows, ps_col, drop = TRUE],
						 replace = replace,
						 ties = ties,
						 estimand = 'ATE',
						 M = 1)
	sensitivity_out <- psens(df[rows,][match_out$index.treated, outcome_col, drop = TRUE],
							   df[rows,][match_out$index.control, outcome_col, drop = TRUE],
							   Gamma = 1.5, GammaInc = 0.01)
	data.frame(
		outcome = outcome_label,
		group = group,
		institution = institution,
		ATE = match_out$est,
		statistic = match_out$est / match_out$se.standard,
		df = match_out$nobs - 1,
		p = (1 - pnorm(abs(match_out$est / match_out$se.standard))) * 1.96,
		gamma = ifelse(
			match_out$est > 0,
			max(sensitivity_out$bounds[sensitivity_out$bounds$`Upper bound` < 0.05,]$Gamma),
			max(sensitivity_out$bounds[sensitivity_out$bounds$`Lower bound` < 0.05,]$Gamma)
		)
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
		rows = wgu.rows3,
		treat_col = 'Treat',
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
		rows = wgu.rows3,
		treat_col = 'Treat',
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
	
	matching_results(
		df = daacs.wgu.complete,
		rows = wgu.rows3,
		treat_col = 'Treat',
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
		rows = ec.rows3,
		treat_col = 'Treat',
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
		rows = ec.rows3,
		treat_col = 'Treat',
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
	),
	
	matching_results(
		df = daacs.ec.complete2,
		rows = ec.rows3,
		treat_col = 'Treat',
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
			sprintf(paste0("%.", digits, "f"), x)
			# ifelse(x < 1 / 10^digits,
			# 	   paste0('< ', 1 / 10^digits),
			# 	   sprintf(paste0("%.", digits, "f"), x)
			# )
		}
	)
}

results_table_match2$p_EC <- ifelse(as.numeric(results_table_match2$p_EC) < 1 / 10^digits,
									paste0('< ', 1 / 10^digits),
									results_table_match2$p_EC)
results_table_match2$p_WGU <- ifelse(as.numeric(results_table_match2$p_WGU) < 1 / 10^digits,
									paste0('< ', 1 / 10^digits),
									results_table_match2$p_WGU)

results_table_match2$statistic_EC <- paste0('t', as.integer(results_table_match2$df_EC), ' = ', results_table_match2$statistic_EC)
results_table_match2$df_EC <- NULL
results_table_match2$statistic_WGU <- paste0('t', as.integer(results_table_match2$df_WGU), ' = ', results_table_match2$statistic_WGU)
results_table_match2$df_WGU <- NULL

write.csv(results_table_match2, 
		  file = 'Tables/PSA_Matching.csv', 
		  row.names = FALSE,
		  na = '')


###### PS Weighting ############################################################
library(psa) # remotes::install_github('jbryer/psa')

wgu.ps.formu.daacs <- Complier ~ Age + MilitaryStudent + CITIZENSHIP_STATUS +
	EMPLOYMENT_STATUS + FIRST_GEN_STUDENT + GENDER +
	ETHNICITY2 + as.integer(HOUSEHOLD_INCOME) + CURRENT_PROGRAM_CODE +
	TRANSFER_CREDITS + srl_anxiety + srl_self_efficacy + srl_mindset + 
	srl_mastery_orientation + srl_strategies + srl_metacognition 

ec.ps.formu.daacs <- Complier ~ Age + MilitaryStudent + ENGLISH_LANGUAGE_NATIVE +
	EMPLOYMENT_STATUS + First_Generation + GENDER +
	ETHNICITY + INCOME_RANGE_CODE + DIVISION_CODE +
	Initial_TRANSFER_CREDITS_EARNED + 
	srl_anxiety + srl_self_efficacy + srl_mindset + srl_understanding + srl_strategies + srl_metacognition

wgu_df <- daacs.wgu.complete[wgu.rows1,]
wgu_df <- wgu_df[complete.cases(wgu_df[,all.vars(wgu.ps.formu.daacs)]),]

ec_df <- daacs.ec.complete2[ec.rows1,]
ec_df <- ec_df[complete.cases(ec_df[,all.vars(ec.ps.formu.daacs)]),]

wgu_weights <- psa::calculate_ps_weights(
	treatment = wgu_df$Complier,
	ps = wgu_df$ps,
	estimand = 'ATE')

ec_weights <- psa::calculate_ps_weights(
	treatment = ec_df$Complier,
	ps = ec_df$ps,
	estimand = 'ATE'
)

wgu_retained_out <- glm(Retained ~ Complier + srl_anxiety + srl_self_efficacy + 
							srl_mindset + srl_mastery_orientation + srl_strategies + 
							srl_metacognition, # + mathTotal + readTotal + writeTotal,
						data = wgu_df,
						weights = wgu_weights,
						family = quasibinomial(link = 'logit'))
wgu_retained_out_summary <- summary(wgu_retained_out)
wgu_retained_out_summary
wgu_retained_out_summary$coefficients |> write.csv('Tables/Posthoc_Retention_WGU.csv')

wgu_creditratio_out <- lm(CreditRatio_Term1 ~ Complier + srl_anxiety + srl_self_efficacy + 
						   	srl_mindset + srl_mastery_orientation + srl_strategies + 
						   	srl_metacognition,
						   data = wgu_df,
						   weights = wgu_weights)
wgu_creditratio_out_summary <- summary(wgu_creditratio_out)
wgu_creditratio_out_summary
wgu_creditratio_out_summary$coefficients |> write.csv('Tables/Posthoc_SuccessRate_WGU.csv')


ec_retained_out <- glm(Retained ~ Complier + srl_anxiety + srl_self_efficacy + 
					   	srl_mindset + srl_mastery_orientation + srl_strategies + 
					   	srl_metacognition,
					   data = ec_df,
					   weights = ec_weights,
					   family = quasibinomial(link = 'logit'))
ec_retained_out_summary <- summary(ec_retained_out)
ec_retained_out_summary
ec_retained_out_summary$coefficients |> write.csv('Tables/Posthoc_Retention_EC.csv')

ec_creditratio_out <- lm(CreditRatio_Term1 ~ Complier + srl_anxiety + srl_self_efficacy + 
						  	srl_mindset + srl_mastery_orientation + srl_strategies + 
						  	srl_metacognition,
						  data = ec_df,
						  weights = ec_weights)
ec_creditratio_out_summary <- summary(ec_creditratio_out)
ec_creditratio_out_summary
ec_creditratio_out_summary$coefficients |> write.csv('Tables/Posthoc_SuccessRate_EC.csv')


##### Add SRL results to PS model
wgu_ps_out <- glm(wgu.ps.formu.daacs,
				  data = wgu_df,
				  family = binomial(link = 'logit'))
wgu_ps_out |> summary()
wgu_ps <- fitted(wgu_ps_out)
wgu_ps_weights <- psa::calculate_ps_weights(
	treatment = wgu_df$Complier, 
	ps = wgu_ps, 
	estimand = 'ATE')

glm(Retained ~ Complier,
	data = wgu_df,
	weights = wgu_ps_weights,
	family = quasibinomial(link = 'logit')) |>
	summary()

lm(CreditRatio_Term1 ~ Complier,
	data = wgu_df,
	weights = wgu_ps_weights) |>
	summary()


##### Differences in DAACS academic measures ###################################
t.test(mathTotal ~ Complier, data = wgu_df)
t.test(mathTotal ~ Complier, data = ec_df)

t.test(readTotal ~ Complier, data = wgu_df)
t.test(readTotal ~ Complier, data = ec_df)

t.test(writeTotal ~ Complier, data = wgu_df)
t.test(writeTotal ~ Complier, data = ec_df)

wgu_academic_diff <- data.frame(
	assessment = c('Mathematics', 'Reading', 'Writing'),
	results_and_feedback = c(mean(wgu_df[wgu_df$Complier,]$mathTotal, na.rm = TRUE),
							 mean(wgu_df[wgu_df$Complier,]$readTotal, na.rm = TRUE),
							 mean(wgu_df[wgu_df$Complier,]$writeTotal, na.rm = TRUE)),
	results_and_feedback_sd = c(sd(wgu_df[wgu_df$Complier,]$mathTotal, na.rm = TRUE),
								sd(wgu_df[wgu_df$Complier,]$readTotal, na.rm = TRUE),
								sd(wgu_df[wgu_df$Complier,]$writeTotal, na.rm = TRUE)),
	results_only = c(mean(wgu_df[!wgu_df$Complier,]$mathTotal, na.rm = TRUE),
					 mean(wgu_df[!wgu_df$Complier,]$readTotal, na.rm = TRUE),
					 mean(wgu_df[!wgu_df$Complier,]$writeTotal, na.rm = TRUE)),
	results_only_sd = c(sd(wgu_df[!wgu_df$Complier,]$mathTotal, na.rm = TRUE),
						sd(wgu_df[!wgu_df$Complier,]$readTotal, na.rm = TRUE),
						sd(wgu_df[!wgu_df$Complier,]$writeTotal, na.rm = TRUE)),
	t_statistic = c(abs(t.test(mathTotal ~ Complier, data = wgu_df)$statistic),
					abs(t.test(readTotal ~ Complier, data = wgu_df)$statistic),
					abs(t.test(writeTotal ~ Complier, data = wgu_df)$statistic)),
	df = c(round(t.test(mathTotal ~ Complier, data = wgu_df)$parameter),
		   round(t.test(readTotal ~ Complier, data = wgu_df)$parameter),
		   round(t.test(writeTotal ~ Complier, data = wgu_df)$parameter)),
	p_value = c(t.test(mathTotal ~ Complier, data = wgu_df)$p.value,
				t.test(readTotal ~ Complier, data = wgu_df)$p.value,
				t.test(writeTotal ~ Complier, data = wgu_df)$p.value)
)

ec_academic_diff <- data.frame(
	assessment = c('Mathematics', 'Reading', 'Writing'),
	results_and_feedback = c(mean(ec_df[ec_df$Complier,]$mathTotal, na.rm = TRUE),
							 mean(ec_df[ec_df$Complier,]$readTotal, na.rm = TRUE),
							 mean(ec_df[ec_df$Complier,]$writeTotal, na.rm = TRUE)),
	results_and_feedback_sd = c(sd(ec_df[ec_df$Complier,]$mathTotal, na.rm = TRUE),
								sd(ec_df[ec_df$Complier,]$readTotal, na.rm = TRUE),
								sd(ec_df[ec_df$Complier,]$writeTotal, na.rm = TRUE)),
	results_only = c(mean(ec_df[!ec_df$Complier,]$mathTotal, na.rm = TRUE),
					 mean(ec_df[!ec_df$Complier,]$readTotal, na.rm = TRUE),
					 mean(ec_df[!ec_df$Complier,]$writeTotal, na.rm = TRUE)),
	results_only_sd = c(sd(ec_df[!ec_df$Complier,]$mathTotal, na.rm = TRUE),
						sd(ec_df[!ec_df$Complier,]$readTotal, na.rm = TRUE),
						sd(ec_df[!ec_df$Complier,]$writeTotal, na.rm = TRUE)),
	t_statistic = c(abs(t.test(mathTotal ~ Complier, data = ec_df)$statistic),
					abs(t.test(readTotal ~ Complier, data = ec_df)$statistic),
					abs(t.test(writeTotal ~ Complier, data = ec_df)$statistic)),
	df = c(round(t.test(mathTotal ~ Complier, data = ec_df)$parameter),
		   round(t.test(readTotal ~ Complier, data = ec_df)$parameter),
		   round(t.test(writeTotal ~ Complier, data = ec_df)$parameter)),
	p_value = c(t.test(mathTotal ~ Complier, data = ec_df)$p.value,
				t.test(readTotal ~ Complier, data = ec_df)$p.value,
				t.test(writeTotal ~ Complier, data = ec_df)$p.value)
)


wgu_academic_diff$results_and_feedback <- paste0(
	round(wgu_academic_diff$results_and_feedback, digits = 2), ' (SD = ',
	round(wgu_academic_diff$results_and_feedback_sd, digits = 2), ')'
)
wgu_academic_diff$results_only <- paste0(
	round(wgu_academic_diff$results_only, digits = 2), ' (SD = ',
	round(wgu_academic_diff$results_only_sd, digits = 2), ')'
)
wgu_academic_diff$results_and_feedback_sd <- NULL
wgu_academic_diff$results_only_sd <- NULL
wgu_academic_diff$t_statistic <- paste0(
	't', wgu_academic_diff$df, ' = ',
	round(wgu_academic_diff$t_statistic, digits = 2)
)
wgu_academic_diff$df <- NULL
wgu_academic_diff$p_value <- sapply(wgu_academic_diff$p_value, FUN = function(x) {
	ifelse(x < 1 / 10^digits,
		   paste0('< ', 1 / 10^digits),
		   sprintf(paste0("%.", digits, "f"), x) ) } )


ec_academic_diff$results_and_feedback <- paste0(
	round(ec_academic_diff$results_and_feedback, digits = 2), ' (SD = ',
	round(ec_academic_diff$results_and_feedback_sd, digits = 2), ')'
)
ec_academic_diff$results_only <- paste0(
	round(ec_academic_diff$results_only, digits = 2), ' (SD = ',
	round(ec_academic_diff$results_only_sd, digits = 2), ')'
)
ec_academic_diff$results_and_feedback_sd <- NULL
ec_academic_diff$results_only_sd <- NULL
ec_academic_diff$t_statistic <- paste0(
	't', ec_academic_diff$df, ' = ',
	round(ec_academic_diff$t_statistic, digits = 2)
)
ec_academic_diff$df <- NULL
ec_academic_diff$p_value <- sapply(ec_academic_diff$p_value, FUN = function(x) {
	ifelse(x < 1 / 10^digits,
		   paste0('< ', 1 / 10^digits),
		   sprintf(paste0("%.", digits, "f"), x) ) } )


write.csv(wgu_academic_diff, file = 'Tables/Academic_Differences_WGU.csv')
write.csv(ec_academic_diff, file = 'Tables/Academic_Differences_EC.csv')

