setwd("/home/kswada/kw/R_statistical_analysis_by_data/males")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# Reference:
# http://tarohmaru.web.fc2.com/R/ExerciseFixedEffects1.html


# ------------------------------------------------------------------------------
# data:  Males
# ------------------------------------------------------------------------------

data("Males", package = "plm")


str(Males)

dim(Males)

car::some(Males)


# ----------
summary(Males)


# ----------
xtabs(~year, Males)



# ------------------------------------------------------------------------------
# basic analysis:  wage vs. year
# ------------------------------------------------------------------------------

tmp <- Males
tmp$wagediff <- c(NA, diff(Males$wage))

par(mfrow=c(1,2), mar=c(3,3,2, 2), cex.main=0.7, mgp=c(2, 0.7, 0))

boxplot(wage ~ year, data=tmp, main="log wage")
abline(h=mean(Males$wage), col="red")

boxplot(wagediff ~ year, data=tmp, main="log wage diff")
abline(h=0, col="red")


# -->
# note that wagediff at 1980 is calculated as 1987 - 1980, so this should be ignored.



# ------------------------------------------------------------------------------
# mosaic plot by year
# ------------------------------------------------------------------------------

par(mfrow=c(3,3), mar=c(3, 3, 1.5, 2), mgp=c(1.5, 0.5, 0))
for(i in 1:7){
  plot( married[year==1980+i] ~ married[year==1979+i], data=Males,
        xlab="married in previou year",
        ylab="married in current year", main=paste(1980+i, "year"), cex.lab=0.7)
}


# -->
# married males is increasing through 1980 to 1987


# ------------------------------------------------------------------------------
# compare log wage distribution of married or not
# Welch 2-sample t-test (the average)
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(wage ~ married, data=Males, main="married and log wage")

t.test(wage ~ married, data=Males)


# -->
# average is significantly different.



# ------------------------------------------------------------------------------
# fixed effects model: wage ~ married
# ------------------------------------------------------------------------------

library(plm)
library(stargazer)

tmp <- Males

# convert long name of occupation to short name
levels(tmp$occupation) <- c("Professionals", "Managers", "Sales", "Clerks", "Craftsmen", "Operatives", "Laborers", "Farm", "Service")


# within model (fixed effects)
pl1 <- plm(wage ~ married, data=tmp, model="within")
pl2 <- update(pl1,  ~exper + I(exper^2) + union + health + occupation)
pl3 <- update(pl2, ~.+ married)


# for comparison, between model
tmp$school <- tmp$school
tmp$ethn <- tmp$ethn
pl4 <- update(pl3, ~.+school+ethn, model="between")


# ----------
stargazer(pl1, pl2, pl3, pl4, type="text", keep.stat=c("n", "adj.rsq"))


# -->
# model1:  wage of married is exp(0.243) = 1.275 times higher than the wage of un-married
# model3:  even if all variables are controlled, wage of married is exp(0.109) = 1.115 times higher than the wage of un-married
# This means that TCUH (Time Constant Unobservable Heterogeinity) and other all variables are considered, anyway the wage of married is higher.
# Remaining exp(0.064) = exp(0.243 - 0.109) is explained by TCUH



# ------------------------------------------------------------------------------
# define panel data and check if balanced
# ------------------------------------------------------------------------------

library(plm)

mp <- pdata.frame(Males, index = c("nr", "year"), drop.index = FALSE)


# pdim checks the number of individuals and time observations in the panel and whether it is balanced or not.
# n: a list containing n
# T: the number of time observations
# N: the total number of observations

# This is balanced panel.
pdim(mp)


# ----------
table(index(mp)$nr, useNA = "always")

table(index(mp)$year, useNA = "always")

table(index(mp), useNA = "ifany")


# ------------------------------------------------------------------------------
# Variance decomposition
# ------------------------------------------------------------------------------

# Variance decomposition for wage
summary(mp$wage)

# -->
# The variation of wage is largely due to inter-individuals 53.7%


