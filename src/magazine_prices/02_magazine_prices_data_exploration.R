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
# data exploration
# ------------------------------------------------------------------------------

MagazinePrices %>% filter(id == 31)


MagazinePrices %>% filter(magazine == "Audio")



# -->
# alpha(nt) = hc(nt) - ho(nt):  specific term for enterprise n at period t, which represents the price change policy.
# Cecchetti (1986) assumes that alpha(nt) can be assumed constant for 3 consecutive years.
# In this case, the period of observation being of 27 years, there are 9 different effects for each magazine.



# ----------
xtabs(~ year + magazine, data = MagazinePrices) %>% data.frame() %>% filter(Freq >= 2)


MagazinePrices %>% filter(magazine == "Science Digest")



# -->
# Science Digest have 2 records in same year (year 45)
# The length is changed within year == 45.
# The price of Science Digest was changed within the year.




# ------------------------------------------------------------------------------
# data exploration:  data distribution
# ------------------------------------------------------------------------------

summary(MagazinePrices)


# -->
# many records miss id (NA)



# -->
# length:  min = 1 and max = 20,  median = 3
# price:  min = 0.15 and max = 5.0,  madian = 0.5



