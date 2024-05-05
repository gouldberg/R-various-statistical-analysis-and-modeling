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
# Data exploration:  pairs plot
# ------------------------------------------------------------------------------


MyVar <- c("min_pressure", "damage_norm", "femininity", "deaths")


psych::pairs.panels(data[,MyVar], stars = TRUE)


psych::pairs.panels(data[,MyVar], method = "spearman", stars = TRUE)




# -->
# Femininity is skew = -0.63 and kurtosis = -1.37,
# but very wide and 2 modality distribution

# It seems that there is almost no correlation between femninity and deaths.




# ------------------------------------------------------------------------------
# Data exploration:  corrplot
# ------------------------------------------------------------------------------

library(corrplot)


MyVar <- c("min_pressure", "damage_norm", "femininity", "deaths")


cor_mat <- cor(data[,MyVar], method = "spearman")


corrplot(cor_mat, hclust.method = "ward.D2", addrect = TRUE)




# ------------------------------------------------------------------------------
# Data exploration:  pairs plot by scatterplot matrix
# ------------------------------------------------------------------------------


library(car)


formula <- ~ min_pressure + log(damage_norm) + femininity + log(deaths + 0.0001)


scatterplotMatrix(formula, data = data,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)




# ----------
# grouped by female or male

formula <- ~ min_pressure + log(damage_norm) + femininity + log(deaths + 0.0001) | female


scatterplotMatrix(formula, data = data,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c(gray(0.8), "black"), pch = 1:2)



