# EXAMPLE 1

xdata <- c(11, 6, 14, 8, 14)
ydata <- c(-35, -18, -46, -22, -45)

example1 <- data.frame(x=xdata, y=ydata)

# Create regression model manually

Sxy = sum(example1$x*example1$y)-
  (sum(example1$x)*sum(example1$y))/nrow(example1)

Sxx = sum(example1$x^2)-
  sum(example1$x)^2/nrow(example1)

B1 = Sxy/Sxx
B0 = mean(example1$y)-B1*mean(example1$x)

B1
B0

# Compare with results of LM()
lm(y~x, example1)

# CORRELATION
lm <- lm(y~x, example1)
lm_summ <- summary(lm)


# Show R-sq
lm_summ # You can copy from the output of this

r_sq = lm_summ$r.squared # or you can call this value
r_sq

correlation = sqrt(r_sq)
correlation

cor.test(example1$x, example1$y)

# DELIVERY TIME

deliverydata <- read.csv('deliverytime.csv')

library(ggplot2)
ggplot(deliverydata, aes(x=distance, y=deltime, size = ncases)) +
  geom_point(alpha=0.7)

lm_deltime <- lm(deltime~distance+ncases, deliverydata)
summary(lm_deltime)

# Hypo test
B2 = 1.615907
B2_hyp = 2
sigma = 3.259
# since we are testing for slope of n_cases, 
# we use x as ncases column from delivery data
Sxx = sum(deliverydata$ncases^2)-
  sum(deliverydata$ncases)^2/nrow(deliverydata)

t0 = (B2-B2_hyp)/sqrt(sigma^2/Sxx)
t0

dfe = 22
alpha = 0.05
t_crit = qt(1-alpha/2, dfe) # for both since we have same df errors
t_crit

if(abs(t0)>t_crit) 'REJECT Null' else 'DNR Null'

0.01*60
