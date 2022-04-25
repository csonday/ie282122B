5+3
5**2
print("Hello World!")

a <- 3
b = 2
c <- 3**2
c
print(c)

# import data
data <- ToothGrowth
View(data)

# convert dose from num to factor
data$dose <- as.factor(data$dose)

# perform anova
CRD_model <- aov(len~supp, data)
summary(CRD_model)

# get linear model
Mod_CRD <- lm(len~supp, data)
summary(Mod_CRD)

# Tukey's
TukeyHSD(CRD_model)

# Fisher's (response, factor, p.adjust.method = 'none')
pairwise.t.test(data$len, data$supp, 
                p.adjust.method = 'none')

plot(CRD_model)

crd_dose <- aov(len~dose, data)
summary(crd_dose)

mod_dose <- lm(len~dose, data)
summary(mod_dose)

plot(crd_dose)


RCBD <- aov(len~dose+supp, data)
summary(RCBD)

Model_rcbd <- lm(len~dose+supp, data)
summary(Model_rcbd)


print(OrchardSprays)





