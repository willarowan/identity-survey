install.packages('boot')
install.packages('nptest')
library(boot)
library(nptest)

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