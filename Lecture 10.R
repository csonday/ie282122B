battlifedata <- read.csv("battlifedata.csv")

battlifedata$material <- as.factor(battlifedata$material)
battlifedata$temp <- as.factor(battlifedata$temp)

anova <- aov(battlife~material*temp, battlifedata)
summary(anova)

linearmod <- lm(battlife~material*temp, battlifedata)
summary(linearmod)

plot(anova)

softdrinkdata <- read.csv("softdrink.csv")

softdrinkdata$carb <- as.factor(softdrinkdata$carb)
softdrinkdata$psi <- as.factor(softdrinkdata$psi)
softdrinkdata$speed <- as.factor(softdrinkdata$speed)

gen_factorial <- aov(deviation~carb*psi*speed, softdrinkdata)
summary(gen_factorial)

lm_gf <- lm(deviation~carb*psi*speed, softdrinkdata)
summary(lm_gf)

TukeyHSD(gen_factorial, 'carb')
TukeyHSD(gen_factorial, 'psi')
TukeyHSD(gen_factorial, 'speed')

interaction.plot(x.factor = softdrinkdata$carb, 
                 trace.factor = softdrinkdata$psi, 
                 response = softdrinkdata$deviation, 
                 col = c('black','red','green'), 
                 type = 'b', 
                 fun = mean, 
                 pch = 19
                 )

interaction.plot(x.factor = battlifedata$temp, 
                 trace.factor = battlifedata$material, 
                 response = battlifedata$battlife, 
                 col = c('black','red','green'), 
                 type = 'b', 
                 fun = mean, 
                 pch = 19
)

