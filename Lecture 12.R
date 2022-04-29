lec12_1 <- read.csv("lec12-1.csv")

# create a scatter plot
library(ggplot2)
ggplot(lec12_1, aes(x=absent, y=defects)) + #create the scatter plot
  geom_point(alpha=0.7, size=3, col='red') + # colors the dots
  geom_smooth(formula = y~x, method="lm", se=FALSE) #adds the line

lm_example1 <- lm(defects~absent, lec12_1)
lm_example1
summary(lm_example1)

plot(lm_example1)

lec12_2 <- read.csv('lec12-2.csv')

library(ggplot2)
ggplot(lec12_2, aes(x=x, y=y)) +
  geom_point(alpha=0.7, size=3, col='red') + 
  geom_smooth(formula = y~x, method="lm", se=FALSE)

lm_example2 <- lm(y~x, lec12_2)
lm_example2
plot(lm_example2)

summary(lm_example2)

#Global Significance
# Ho: The regression is insignificant
# H1: The regression is significant


# Let's define a DataFrame manually
# c() is a function for defining lists
xdata <- c(11, 6, 14, 8, 14)
ydata <- c(-35, -18, -46, -22, -45)

example3 <- data.frame(x=xdata, y=ydata)

# no need for library(ggplot2), it's been loaded
ggplot(example3, aes(x=xdata, y=ydata)) +
  geom_point(alpha=0.7, size=3, col='red') + 
  geom_smooth(formula = y~x, method="lm", se=FALSE)

lme3 <- lm(y~x, example3)
lme3

summe3 <- summary(lme3)
summe3


b0 <- lme3$coefficients[[1]] # get int
b1 <- lme3$coefficients[[2]] # get slope

b0_hyp <- 3 	# from hypotheses
b1_hyp <- -4	# from hypotheses

sigma = summe3$sigma

xbar = mean(example3$x)
n = nrow(example3)

Sxx = sum(example3$x^2) - sum(example3$x)^2/n

t0_slope = (b1 - b1_hyp) / (sigma^2/Sxx)^(0.5)

t0_int = (b0 - b0_hyp) / (sigma^2 * ( 1/n + xbar^2/Sxx))^(0.5)

dfe = lme3$df.residual
alpha = 0.05
t_crit = qt(1-alpha/2, dfe) # for both since we have same df errors

# Reject H0 for slope?
t0_slope > t_crit

# Reject H0 for int?
t0_int > t_crit






