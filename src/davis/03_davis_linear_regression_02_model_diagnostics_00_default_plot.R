rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\00_basics\\regression_basics\\davis")


# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------

data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)


data2 <- na.exclude(data)



# ----------
linmod <- lm(weight ~ repwt, data = data2)




# ------------------------------------------------------------------------------
# default model diagnostics plot
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(linmod)
