# setwd("//media//kswada//MyFiles//R//magazine_prices")
setwd("/home/kswada/kw/R_statistical_analysis_by_data/magazine_prices")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Magazine Prices
# ------------------------------------------------------------------------------

data("MagazinePrices", package = "pder")


str(MagazinePrices)


dim(MagazinePrices)


car::some(MagazinePrices)



# ------------------------------------------------------------------------------
# Conditional logit model
#   - 3 year magazine fixed effects are removed.
#   - Use the subjects as their own controls.
#     With panel data we can control stable characteristics whether they are measured or not.
# ------------------------------------------------------------------------------

library(survival)


logitC <- clogit(change ~ length + cuminf + cumsales + strata(id), data = MagazinePrices,
              subset = included == 1)



summary(logitC)



# -->
# Note that the coefficient of the length of the period since the last price change has the expected positive sign
# and is significant only for the conditional logit model.



# ----------
library(texreg)


screenreg(list(logitS = logitS, logitD = logitD, logitC = logitC), digits = 3)



