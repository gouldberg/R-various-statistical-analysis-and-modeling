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
# data exploration:  Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)



# ----------
# deaths vs. min_pressure
gg <- ggplot(data, aes(min_pressure, deaths)) + geom_point(size = 2, alpha = 0.6) + geom_smooth()

gg


# by group
gg + facet_grid(~ female)




# ----------
# deaths vs. damage_norm
gg <- ggplot(data, aes(damage_norm, deaths)) + geom_point(size = 2, alpha = 0.6) + geom_smooth()

gg


# by group
gg + facet_grid(~ female)



# ----------
# deaths vs. femininity
gg <- ggplot(data, aes(femininity, deaths)) + geom_point(size = 2, alpha = 0.6) + geom_smooth()

gg


# by group
gg + facet_grid(~ female)




# ----------
plot(deaths ~ year, data = data, pch = 2)

