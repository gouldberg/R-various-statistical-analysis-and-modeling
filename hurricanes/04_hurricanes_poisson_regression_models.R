# setwd("C:\\Users\\kswad\\OneDrive\\?f?X?N?g?b?v\\?Z?p?͋???_???v????\\51_???̓X?N???v?g\\z_for_demo_uncompleted\\hurricanes")
setwd("/home/kswada/kw/R_statistical_analysis_by_data/hurricanes")

packages <- c("dplyr")

purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hurricanes
# ------------------------------------------------------------------------------

# data("Hurricanes", package = "rethinking")

data <- read.csv(file = "Hurricanes.txt", header = T, sep = "\t")


dim(data)


str(data)



car::some(data)




# ------------------------------------------------------------------------------
# Poisson regression model
# ------------------------------------------------------------------------------


mod_p <- glm(deaths ~ min_pressure + damage_norm + femininity, data = data, family = "poisson")


summary(mod_p)



# ----------
par(mfrow = c(2,2))

plot(mod_p)




# ----------
car::residualPlots(mod, groups = data$female)



# ------------------------------------------------------------------------------
# Quassi-Poisson regression model
# ------------------------------------------------------------------------------

# dispersion parameter:  residual deviance divided by df.
with(mod_p, deviance / df.residual)




# ----------
# Pearson estimate of dispersion
( phi <- sum(residuals(mod_p, type = "pearson")^2 / mod_p$df.residual) )


# standard errors of coefficients in this model should be multiplied by, to correct for over-dispersion
sqrt(phi)



# -->
# Indicates that standard errors of coefficients in this model should be multiplied by 7.24, a 624% increase to correct for over-dispersion
# There is substantial over-dispersion in the data,
# possible to the extent that even a negative binomial GLM will not sufficiently adjust for the variation.



# ----------
summary(mod_p, dispersion=phi)




# -->
# now the femininity is not significant.

