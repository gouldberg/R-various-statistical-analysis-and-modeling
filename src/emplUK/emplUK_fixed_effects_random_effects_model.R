setwd("/home/kswada/kw/R_statistical_analysis_by_data/emplUK")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# Reference:
# http://tarohmaru.web.fc2.com/R/FixedEffectsModel.html


# ------------------------------------------------------------------------------
# EmplUK data
# ------------------------------------------------------------------------------

data("EmplUK", package = "plm")


str(EmplUK)

dim(EmplUK)

car::some(EmplUK)


# ----------
# note that year is from 1976 to 1984 (max 9 years)
summary(EmplUK)



# ------------------------------------------------------------------------------
# define panel data and check if balanced
# ------------------------------------------------------------------------------

library(plm)

mp <- pdata.frame(EmplUK, index = c("firm", "year"), drop.index = FALSE)


# pdim checks the number of individuals and time observations in the panel and whether it is balanced or not.
# n: a list containing n
# T: the number of time observations
# N: the total number of observations

# This is unbalanced panel.
# year: 7 - 9 years
pdim(mp)


# ----------
table(index(mp)$firm, useNA = "always")

table(index(mp)$year, useNA = "always")

table(index(mp), useNA = "ifany")


# ------------------------------------------------------------------------------
# Variance decomposition between firms and years
# ------------------------------------------------------------------------------

# Variance decomposition for wage
summary(mp$wage)

# -->
# The variation is due to inter-firm is very large  (86.6%) !!


ercomp(wage ~ emp + capital + output, mp)

# -->
# median theta is 84%


# ------------------------------------------------------------------------------
# Variance decomposition between firms and years by ordinary linear regression
# ------------------------------------------------------------------------------

mod <- lm(wage ~ as.factor(firm) + year, data = EmplUK)


summary(mod)


anova(mod)


round(anova(mod)$'Sum Sq' / sum(anova(mod)$'Sum Sq'), 3)


# -->
# here also the variation is due to
# inter-firms 86.6%



# ------------------------------------------------------------------------------
# estimate fixed effects for wage by firm by ordinary linear regression
# ------------------------------------------------------------------------------

N <- length(unique(EmplUK$firm))

lm1 <- lm(wage ~ factor(firm) + emp + capital + output, data=EmplUK)


# Estimate for factor(firm) is fixed effects by firm
summary(lm1)$coefficients



# ------------------------------------------------------------------------------
# within model:  Y(it) - Y-mean(i) = beta * ( X(it) - X-mean(i) ) + e(it) - e-mean(i)
#  - calculate average deviation within individual in terms of each variable and apply to linear regression
#  - The beta (slope) of within model is equal to the beta of fixed effects model.
# ------------------------------------------------------------------------------

fixed1 <- plm(wage ~ emp + capital + output, data=mp, model="within")

# the estimates for emp, capital and output are same as the result of ordinary linear regression above.
summary(fixed1)



# ------------------------------------------------------------------------------
# random effects model:  Y(it) = beta0 + beta1 * X(it) + alpha(i) + e(it)
#  - alpha(i) is random effect, which is assumed to be normal distribution with mean zero, independent of variables.
#  - If random effects model is not significantly different compared to within model (fixed effects model), 
#    random effects model is preferred since random effects model is less biased due to less parameters.
#    This is tested by Hausman Test.
# ------------------------------------------------------------------------------

# now change the regression model
fix1 <- plm(wage ~ capital + I(capital^2) + output, data=mp, model="within")
rand1 <- update(fix1, model="random")


library(texreg)
screenreg(list(fix1, rand1), single.row = T, custom.model.names = c("fixed effects", "random effects"))


# ----------
# Hausman Test
# Both models are not significantly different, meaning that random effects model is preferred.
# Also this means that random effects is not significantly correlated to time-constant variable.
phtest(rand1, fix1)


# ----------
# random effects model:
# alpha(i) is shown in Effects 'individual' and
# e(it) is shown in Effects 'idiosyncratic'
summary(rand1)



# ------------------------------------------------------------------------------
# random effects model by lme4
# ------------------------------------------------------------------------------

library(lme4)

lm1 <- lmer(wage ~ capital + I(capital^2) + output +(1|firm), data=EmplUK)


# lmer have AIC and BIC data
screenreg(list(fix1, rand1, lm1), single.row = TRUE, digits = 2)



# ------------------------------------------------------------------------------
# For Reference:  pooled and between models
# ------------------------------------------------------------------------------

# Even though Hausman Test shows that random effects are not correlated with time-constant variable,
# if you are interested in time-constant variable's effects, try pooled and between models.


# firstly include time-constant variable (sector) to random effects model.
rand2 <- update(rand1, ~.+factor(sector))
screenreg(list(rand1, rand2), single.row = TRUE, digits=3)


# --> Almost no difference. This shows that time-constant variable (sector) is not correlated to time variable.


# try pooled model with time-constant variable included.
pool1 <- update(rand2, model="pooling")


# Lagrange Multiplier Test shows that TCUH (Time-Constant Unobservable Heterogeneity) exists,
# which means random effects exists and fixed effects OR random effects model is preferred.
plmtest(pool1, type="bp")


# try between model (betweem model is preferred when random effects do not exist.)
btw1 <- update(rand2, model="between")

screenreg(list(rand2, pool1, btw1), single.row = T, custom.model.names = c("Rondom", "Pooled", "Between"))





