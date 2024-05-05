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
# Data exploration:  data distribution
# ------------------------------------------------------------------------------


summary(data)




# ----------
psych::describe(data)




# ----------
car::densityPlot(data$deaths)


car::densityPlot(deaths ~ as.factor(female), data = data)


car::densityPlot(damage_norm ~ as.factor(female), data = data)




# ----------
table(data$year)

vcd::doubledecker(xtabs(~ year + female, data = data))



# ----------
xyplot(min_pressure ~ year | as.factor(female), 
       data = data, cex = 1.2, pch = 20)


xyplot(deaths ~ year | as.factor(female), 
       data = data, cex = 1.2, pch = 20)



# --> femininity spreads after 1980
xyplot(femininity ~ year, 
       data = data, cex = 1.2, pch = 20)


