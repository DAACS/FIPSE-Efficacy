library(tidyverse)
library(psych)
library(cowplot)
library(gridExtra)
library(plyr)
library(reshape2)
library(ltm)
library(PSAgraphics)

source('R/DataSetup.R')

##### Correlations between page views and success

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


######
wgu.ps.formu <- Complier ~ Age + MilitaryStudent + CITIZENSHIP_STATUS +
	EMPLOYMENT_STATUS + FIRST_GEN_STUDENT + GENDER +
	ETHNICITY2 + as.integer(HOUSEHOLD_INCOME) + CURRENT_PROGRAM_CODE +
	TRANSFER_CREDITS

ec.ps.formu <- Complier ~ Age + MilitaryStudent + ENGLISH_LANGUAGE_NATIVE +
	EMPLOYMENT_STATUS + First_Generation + GENDER +
	ETHNICITY + INCOME_RANGE_CODE + DIVISION_CODE +
	Initial_TRANSFER_CREDITS_EARNED

daacs.wgu.complete$CACE_Levels <- sapply(daacs.wgu.complete$TreatLevels, FUN = function(x) {
	switch(x,
		   'Advisor' = 'Assessment Only',
		   'Assessment Only' = 'Assessment Only',
		   'Feedback' = 'Assessment + Feedback',
		   'Feedback and Advisor' = 'Assessment + Feedback',
		   'Treat-No-DAACS' = 'Non-Complier',
		   'Control' = 'Control') })
table(daacs.wgu.complete$TreatLevels, daacs.wgu.complete$CACE_Levels, useNA = 'ifany')
daacs.wgu.complete$Complier <- daacs.wgu.complete$CACE_Levels == 'Assessment + Feedback'
table(daacs.wgu.complete$CACE_Levels, daacs.wgu.complete$Complier, useNA = 'ifany')

daacs.ec.complete$CACE_Levels <- sapply(daacs.ec.complete$TreatLevels, FUN = function(x) {
	switch(x,
		   'Assessment Only' = 'Assessment Only',
		   'Control-CCS100' = 'Control',
		   'Control-No-CCS100' = 'Control Never Taker',
		   'Feedback' = 'Assessment + Feedback',
		   'No DAACS' = 'Treatment Never Taker')
})
table(daacs.ec.complete$CACE_Levels, daacs.ec.complete$TreatLevels, useNA = 'ifany')
daacs.ec.complete2 <- daacs.ec.complete |>
	filter(CACE_Levels %in% c('Assessment + Feedback', 'Assessment Only', 'Control'))
daacs.ec.complete2$Complier <- daacs.ec.complete2$CACE_Levels == 'Assessment + Feedback'
table(daacs.ec.complete2$CACE_Levels, daacs.ec.complete2$Complier, useNA = 'ifany')

##### Estimate Propensity Scores
# WGU
lr.out <- glm(wgu.ps.formu,
			  family = binomial(link = 'logit'),
			  data = daacs.wgu.complete[daacs.wgu.complete$Treat,])
summary(lr.out)
daacs.wgu.complete$ps <- predict(lr.out,
								 newdata = daacs.wgu.complete,
								 type = 'response')
ggplot(daacs.wgu.complete, aes(x = ps, color = CACE_Levels)) +
	geom_density()

# EC
ec.lr.out <- glm(ec.ps.formu,
				 family = binomial(link = 'logit'),
				 data = daacs.ec.complete2[daacs.ec.complete2$Treat,])
summary(ec.lr.out)
daacs.ec.complete2$ps <- predict(ec.lr.out,
								 newdata = daacs.ec.complete2,
								 type = 'response')
ggplot(daacs.ec.complete2, aes(x = ps, color = CACE_Levels)) +
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

# daacs.ec.complete2 |> dplyr::select(ps, SuccessTerm1, CACE_Levels, Treat, Complier) |> View()

table(daacs.wgu.complete$OnTime_Term1, daacs.wgu.complete$CACE_Levels)

ggplot(daacs.wgu.complete[!daacs.wgu.complete$CACE_Levels %in% c('Non-Complier'),],
	   aes(x = ps, y = as.integer(OnTime_Term1), color = CACE_Levels)) +
	geom_vline(xintercept = wgu.strata.breaks, alpha = 0.5) +
	geom_text(data = wgu.strata.labels, label = rownames(wgu.strata.labels), aes(x = wgu.strata.labels), y = 0.05, color = 'black') +
	# geom_point(alpha = 0.1) +
	ylim(c(0,1)) +
	scale_color_brewer('Treatment Group', type = 'qual', palette = 2) +
	geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', span = .8) +
	ylab('On-Time Progress') + xlab('Propensity Scores') +
	ggtitle('On-Time Progress by Propensity Scores and Complier Status',
			subtitle = 'Western Govorners University') +
	theme_minimal()
ggsave('Figures/PSA_On_Time_WGU.png', width = 10, height = 5)

ggplot(daacs.wgu.complete[!daacs.wgu.complete$CACE_Levels %in% c('Non-Complier'),],
	   aes(x = ps, y = CreditRatio_Term1, color = CACE_Levels)) +
	geom_vline(xintercept = wgu.strata.breaks, alpha = 0.5) +
	geom_text(data = wgu.strata.labels, label = rownames(wgu.strata.labels), aes(x = wgu.strata.labels), y = 0.05, color = 'black') +
	# geom_point(alpha = 0.01) +
	ylim(c(0,1)) +
	scale_color_brewer('Treatment Group', type = 'qual', palette = 2) +
	geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', span = 1) +
	ylab('Credit Ratio') + xlab('Propensity Scores') +
	ggtitle('Term 1 Credit Ratio by Propensity Scores and Complier Status',
			subtitle = 'Western Govorners University') +
	theme_minimal()
ggsave('Figures/PSA_CreditRatio_WGU.png', width = 10, height = 5)

# daacs.wgu.complete |>
# 	select(ps, OnTime_Term1, CACE_Levels) |>
# 	View()

ggplot(daacs.ec.complete2, aes(x = ps, y = as.integer(SuccessTerm1), color = CACE_Levels)) +
	geom_vline(xintercept = ec.strata.breaks, alpha = 0.5) +
	geom_text(data = ec.strata.labels, label = rownames(ec.strata.labels), aes(x = ec.strata.labels), y = 0.05, color = 'black') +
	# geom_point(alpha = 0.1) +
	ylim(c(0,1)) +
	scale_color_brewer('Treatment Group', type = 'qual', palette = 2) +
	geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', span = 1) +
	ylab('On-Time Progress') + xlab('Propensity Scores') +
	ggtitle('On-Time Progress by Propensity Scores and Complier Status',
			subtitle = 'Excelsior College') +
	theme_minimal()
ggsave('Figures/PSA_On_Time_EC.png', width = 10, height = 5)

ggplot(daacs.ec.complete2, aes(x = ps, y = CreditRatio_Term1, color = CACE_Levels)) +
	geom_vline(xintercept = ec.strata.breaks, alpha = 0.5) +
	geom_text(data = ec.strata.labels, label = rownames(ec.strata.labels), aes(x = ec.strata.labels), y = 0.05, color = 'black') +
	# geom_point(alpha = 0.1) +
	ylim(c(0,1)) +
	scale_color_brewer('Treatment Group', type = 'qual', palette = 2) +
	geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', span = 1) +
	ylab('Credit Ratio') + xlab('Propensity Scores') +
	ggtitle('Term 1 Credit Ratio by Propensity Scores and Complier Status',
			subtitle = 'Excelsior College') +
	theme_minimal()
ggsave('Figures/PSA_CreditRatio_EC.png', width = 10, height = 5)

tmp <- glm(OnTime_Term1 ~ srl_metacognition + srl_strategies + srl_motivation +
		   	mathTotal + writeTotal + readTotal + log(PageViews),
		   # weights = daacs.wgu.complete[daacs.wgu.complete$Treat,]$ps,
		   data = daacs.wgu.complete[daacs.wgu.complete$Treat,],
		   family = binomial(link = 'logit'))
summary(tmp)



# ggplot(daacs.wgu.complete[daacs.wgu.complete$Treat,],
# 	   aes(x = srlTotal, y = OnTime_Term1)) +
# 	geom_point() +
# 	geom_smooth(formula = y ~ x, na.rm = TRUE, se = FALSE, method = 'loess')

##### Stratification

# WGU
daacs.wgu.complete <- daacs.wgu.complete %>%
	mutate(strata = cut(ps,
						breaks = wgu.strata.breaks,
						include.lowest = TRUE,
						labels = letters[1:nstrata]) )
table(daacs.wgu.complete$strata, daacs.wgu.complete$CACE_Levels)

# EC
daacs.ec.complete2 <- daacs.ec.complete2 %>%
	mutate(strata = cut(ps,
						breaks = ec.strata.breaks,
						include.lowest = TRUE,
						labels = letters[1:nstrata]) )
table(daacs.ec.complete2$strata, daacs.ec.complete2$CACE_Levels)


##### Plots
tab <- psych::describeBy(daacs.wgu.complete$OnTime_Term1,
						 group = list(daacs.wgu.complete$strata,
						 			  daacs.wgu.complete$CACE_Levels),
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
						 			 daacs.ec.complete2$CACE_Levels),
						 mat = TRUE)
tab.ec
ggplot(tab.ec, aes(x = group1, y = mean, color = group2)) +
	# geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1, alpha = 0.6) +
	geom_point(size = 3) +
	theme_minimal() +
	xlab('Strata') + ylab('Percent On-Time Progress') +
	scale_color_brewer('Treatment', type = 'qual', palette = 2) +
	ggtitle('Propensity Score Analysis', subtitle = 'Excelsior College')


##### Groupings
# rows1 = Complier vs Non-Complier
# rows2 = Complier vs Control
wgu.rows1 <- daacs.wgu.complete$CACE_Levels %in% c('Assessment + Feedback', 'Assessment Only')
wgu.rows2 <- daacs.wgu.complete$CACE_Levels %in% c('Assessment + Feedback', 'Control')
ec.rows1 <- daacs.ec.complete2$CACE_Levels %in% c('Assessment + Feedback', 'Assessment Only')
ec.rows2 <- daacs.ec.complete2$CACE_Levels %in% c('Assessment + Feedback', 'Control')


##### Balance
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


##### Estimate Effect Sizes
# WGU
# Complier vs non-complier
wgu.psa1 <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows1,]$OnTime_Term1,
	treatment = daacs.wgu.complete[wgu.rows1,]$Complier,
	# propensity = daacs.wgu.complete[wgu.rows1,]ps,
	strata = daacs.wgu.complete[wgu.rows1,]$strata)
2 * (1 - pt(wgu.psa1$approx.t, wgu.psa1$df))

wgu.psa1b <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows1,]$CreditRatio_Term1,
	treatment = daacs.wgu.complete[wgu.rows1,]$Complier,
	# propensity = daacs.wgu.complete[wgu.rows1,]ps,
	strata = daacs.wgu.complete[wgu.rows1,]$strata)
wgu.psa1b
2 * (1 - pt(wgu.psa1b$approx.t, wgu.psa1b$df))


# Complier vs control
wgu.psa2 <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows2,]$OnTime_Term1,
	treatment = daacs.wgu.complete[wgu.rows2,]$Complier,
	# propensity = daacs.wgu.complete[wgu.rows2,]ps,
	strata = daacs.wgu.complete[wgu.rows2,]$strata)
2 * (1 - pt(wgu.psa2$approx.t, wgu.psa2$df))

wgu.psa2b <- PSAgraphics::circ.psa(
	daacs.wgu.complete[wgu.rows2,]$CreditRatio_Term1,
	treatment = daacs.wgu.complete[wgu.rows2,]$Complier,
	# propensity = daacs.wgu.complete[wgu.rows2,]ps,
	strata = daacs.wgu.complete[wgu.rows2,]$strata)
wgu.psa2b
2 * (1 - pt(wgu.psa2b$approx.t, wgu.psa2b$df))

# EC
# Complier vs non-complier
ec.psa1 <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows1,]$SuccessTerm1,
	treatment = daacs.ec.complete2[ec.rows1,]$Complier,
	# propensity = daacs.ec.complete2[ec.rows1,]ps,
	strata = daacs.ec.complete2[ec.rows1,]$strata)
2 * (1 - pt(ec.psa1$approx.t, ec.psa1$df))

ec.psa1b <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows1,]$CreditRatio_Term1,
	treatment = daacs.ec.complete2[ec.rows1,]$Complier,
	# propensity = daacs.ec.complete2[ec.rows1,]ps,
	strata = daacs.ec.complete2[ec.rows1,]$strata)
ec.psa1b
2 * (1 - pt(ec.psa1b$approx.t, ec.psa1b$df))


# Complier vs control
ec.psa2 <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows2,]$SuccessTerm1,
	treatment = daacs.ec.complete2[ec.rows2,]$Complier,
	# propensity = daacs.ec.complete2[ec.rows2,]ps,
	strata = daacs.ec.complete2[ec.rows2,]$strata)
2 * (1 - pt(ec.psa2$approx.t, ec.psa2$df))

ec.psa2b <- PSAgraphics::circ.psa(
	daacs.ec.complete2[ec.rows2,]$CreditRatio_Term1,
	treatment = daacs.ec.complete2[ec.rows2,]$Complier,
	# propensity = daacs.ec.complete2[ec.rows2,]ps,
	strata = daacs.ec.complete2[ec.rows2,]$strata)
ec.psa2b
2 * (1 - pt(ec.psa2b$approx.t, ec.psa2b$df))

##### Matching / Sensitivity ###################################################
library(Matching)
library(rbounds)
# WGU
# On-Time Term 1
wgu.match1a <- Match(Y = daacs.wgu.complete[wgu.rows1,]$OnTime_Term1,
					 Tr = daacs.wgu.complete[wgu.rows1,]$Complier,
					 X = daacs.wgu.complete[wgu.rows1,]$ps,
					 estimand = 'ATE')
summary(wgu.match1a)

wgu.sensitivity1a <- psens(daacs.wgu.complete[wgu.rows1,][wgu.match1a$index.treated,]$OnTime_Term1,
						   daacs.wgu.complete[wgu.rows1,][wgu.match1a$index.control,]$OnTime_Term1,
						   Gamma = 1.5, GammaInc = 0.01)
wgu.sensitivity1a


wgu.match1b <- Match(Y = daacs.wgu.complete[wgu.rows2,]$OnTime_Term1,
					 Tr = daacs.wgu.complete[wgu.rows2,]$Complier,
					 X = daacs.wgu.complete[wgu.rows2,]$ps,
					 estimand = 'ATE')
summary(wgu.match1b)

wgu.sensitivity1b <- psens(daacs.wgu.complete[wgu.rows2,][wgu.match1b$index.treated,]$OnTime_Term1,
						   daacs.wgu.complete[wgu.rows2,][wgu.match1b$index.control,]$OnTime_Term1,
						   Gamma = 1.5, GammaInc = 0.01)
wgu.sensitivity1b

# Success rate
wgu.match2a <- Match(Y = daacs.wgu.complete[wgu.rows1,]$CreditRatio_Term1,
					 Tr = daacs.wgu.complete[wgu.rows1,]$Complier,
					 X = daacs.wgu.complete[wgu.rows1,]$ps,
					 estimand = 'ATE')
summary(wgu.match2a)

wgu.sensitivity1a <- psens(daacs.wgu.complete[wgu.rows1,][wgu.match2a$index.treated,]$CreditRatio_Term1,
						   daacs.wgu.complete[wgu.rows1,][wgu.match2a$index.control,]$CreditRatio_Term1,
						   Gamma = 1.5, GammaInc = 0.01)
wgu.sensitivity1a


wgu.match2b <- Match(Y = daacs.wgu.complete[wgu.rows2,]$CreditRatio_Term1,
					 Tr = daacs.wgu.complete[wgu.rows2,]$Complier,
					 X = daacs.wgu.complete[wgu.rows2,]$ps,
					 estimand = 'ATE')
summary(wgu.match2b)

wgu.sensitivity1b <- psens(daacs.wgu.complete[wgu.rows2,][wgu.match2b$index.treated,]$CreditRatio_Term1,
						   daacs.wgu.complete[wgu.rows2,][wgu.match2b$index.control,]$CreditRatio_Term1,
						   Gamma = 1.5, GammaInc = 0.01)
wgu.sensitivity1b

# EC
# On-Time term 1
ec.match1a <- Match(Y = daacs.ec.complete2[ec.rows1,]$SuccessTerm1,
					 Tr = daacs.ec.complete2[ec.rows1,]$Complier,
					 X = daacs.ec.complete2[ec.rows1,]$ps,
					 estimand = 'ATE')
summary(ec.match1a)

ec.sensitivity1a <- psens(daacs.ec.complete2[ec.rows1,][ec.match1a$index.treated,]$SuccessTerm1,
						   daacs.ec.complete2[ec.rows1,][ec.match1a$index.control,]$SuccessTerm1,
						   Gamma = 1.5, GammaInc = 0.01)
ec.sensitivity1a


ec.match1b <- Match(Y = daacs.ec.complete2[ec.rows2,]$SuccessTerm1,
					 Tr = daacs.ec.complete2[ec.rows2,]$Complier,
					 X = daacs.ec.complete2[ec.rows2,]$ps,
					 estimand = 'ATE')
summary(ec.match1b)

ec.sensitivity1b <- psens(daacs.ec.complete2[ec.rows2,][ec.match1b$index.treated,]$SuccessTerm1,
						   daacs.ec.complete2[ec.rows2,][ec.match1b$index.control,]$SuccessTerm1,
						   Gamma = 1.5, GammaInc = 0.01)
ec.sensitivity1b

# Success rate
ec.match2a <- Match(Y = daacs.ec.complete2[ec.rows1,]$CreditRatio_Term1,
					 Tr = daacs.ec.complete2[ec.rows1,]$Complier,
					 X = daacs.ec.complete2[ec.rows1,]$ps,
					 estimand = 'ATE')
summary(ec.match2a)

ec.sensitivity1a <- psens(daacs.ec.complete2[ec.rows1,][ec.match2a$index.treated,]$CreditRatio_Term1,
						   daacs.ec.complete2[ec.rows1,][ec.match2a$index.control,]$CreditRatio_Term1,
						   Gamma = 1.5, GammaInc = 0.01)
ec.sensitivity1a


ec.match2b <- Match(Y = daacs.ec.complete2[ec.rows2,]$CreditRatio_Term1,
					 Tr = daacs.ec.complete2[ec.rows2,]$Complier,
					 X = daacs.ec.complete2[ec.rows2,]$ps,
					 estimand = 'ATE')
summary(ec.match2b)

ec.sensitivity1b <- psens(daacs.ec.complete2[ec.rows2,][ec.match2b$index.treated,]$CreditRatio_Term1,
						   daacs.ec.complete2[ec.rows2,][ec.match2b$index.control,]$CreditRatio_Term1,
						   Gamma = 1.5, GammaInc = 0.01)
ec.sensitivity1b

