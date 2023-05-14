install.packages('boot')
install.packages('nptest')
install.packages('MBESS')
install.packages('effectsize')
install.packages('lmboot')
install.packages('WRS2')
library(boot)
library(nptest)
library(MBESS)
library(effectsize)
options(es.use_symbols = TRUE)
library(lmboot)
library(WRS2)

?boot

#tutorial from umn, using nptest
npbs.MWhite <- np.boot(x = ident_M_White$sum_ident, statistic = median)
npbs.MWhite
#bootstrap distribution
hist(npbs.MWhite$boot.dist, xlab = "Statistic", main = "Bootstrap Distribution")
box()
abline(v = npbs$t0, lty = 2, col = "red")
legend("topleft", "t0", lty = 2, col = "red", bty = "n")


#tutorial with mtcars

mtcars
# Creating Function to obtain R-Squared from the data
r_squared.tut <- function(formula, data, indices) {
  val <- data[indices,] # selecting sample with boot 
  fit <- lm(formula, data=val)
  return(summary(fit)$r.square)
} 
# Performing 1500 replications with boot 
output <- boot(data=mtcars, statistic=r_squared, 
               R=1500, formula=mpg~wt+disp)

# Plotting the output
output 
plot(output)

# Obtaining a confidence interval of 95%
boot.ci(output, type="bca")

#trying tutorial again but with median
mtcars <- as.data.frame(mtcars)
class(mtcars)
mtcars$mpg <- as.numeric(mtcars$mpg)
class(mtcars$mpg)
median.tut <- function(formula, data, indices) {
  val <- data[indices,] # selecting sample with boot 
  fit <- lm(formula, data=val)
  return(summary(fit)$median)
} 
# Performing 1500 replications with boot 
output <- boot(data=mtcars, statistic=median, 
               R=1500, formula=mpg)

#error message 'need numeric data'... but it is numeric! ?


#trying with identity scores - same error message as above

# Creating Function to obtain median from the data
ident.boot <- function(formula, data, indices) {
  val <- data[indices,] # selecting sample with boot 
  fit <- lm(formula, data=val)
  return(summary(fit)$median)
} 
# Performing 1500 replications with boot 
boot_ident_output <- boot(data=ident_WNB_BIPOC, statistic=median, 
               R=1500, formula=sum_ident)

# Plotting the output
boot_ident_output
plot(boot_ident_output)

# Obtaining a confidence interval of 95%
boot.ci(boot_ident_output, type="bca")


##### Zieffler et al. can compare 2 groups successfully

#testing tutorial from Zieffler et al.
## Function to compute the mean difference
mean.diff <- mean(ident_White$sum_ident) - mean(ident_BIPOC$sum_ident)

mean.diff.np <- function(ident_race, indices) {
d <- ident_race[indices, ]
mean(d$sum_ident[1:68]) - mean(d$sum_ident[69:147])
}

## Carry out the nonparametric bootstrap
nonpar.boot <- boot(data = ident_race, statistic = mean.diff.np,
                    4999)

## Plot the bootstrap distribution
plot(density(nonpar.boot$t))

## Draw a vertical line at 0
abline(v=0)

## Mean of the bootstrap distribution
mean(nonpar.boot$t)

## Standard error of bootstrap distribution
sd(nonpar.boot$t)

## Count the resamples where mean differences are as or more extreme than orig
  ##mean differences
mean.diff.ex <- length(nonpar.boot$t[abs(nonpar.boot$t) >= mean.diff])
(mean.diff.ex+1)/(4999 + 1)

#reporting effect sizes
cohens_d(sum_ident ~ race, data = ident_race)


###lmboot doesn't do pairwise

#lmboot package. this way will only yield p-value, no post-hoc
myANOVA1 <- ANOVA.boot(mpg~as.factor(cyl), data=mtcars)
myANOVA1$`p-values`

aov.boot_racegen <- ANOVA.boot(sum_ident~racegen, B = 9999, type = "residual", wild.dist = "normal", 
           seed = NULL, data = ident_racegen, keep.boot.resp = FALSE)
aov.boot_racegen$'p-values'

### This is the one! WRS2 package by Mair and Wilcox

#WRS2 package. percentile t method of bootstrapping for 1-way ANOVA
#bootstrap with one-way anova, means-trimmed is tr=
boot.racegen <- t1waybt(sum_ident~racegen,tr=.2,nboot=4999, data=ident_racegen)
hist(boot.racegen$test)

#pairwise post-hoc tests
bootpairwise.racegen <- mcppb20(sum_ident~racegen,tr=.2,nboot=4999, data=ident_racegen)

#and can we also do t-tests?
boot.race <- yuenbt(formula = sum_ident~race, data = ident_race, tr = 0.2, nboot = 4999)