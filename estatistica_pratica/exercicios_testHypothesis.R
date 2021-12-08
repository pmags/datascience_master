### Exercicio 1
library(haven)
Cystfibr <- read_sav("estatistica_pratica/Cystfibr.sav")

alpha <- 0.01

#a

summary(Cystfibr$age)

par(mfrow = c(1,2))
hist(Cystfibr$age)
boxplot(Cystfibr$age)

# Reasonable symetric so assume normality



barplot(table(Cystfibr$sex))

table(Cystfibr$sex) 
prop.table(table(Cystfibr$sex) )

### Exercicio 3

#a)

### SEX 
# categorical variable

barplot(table(bp_obese$sex))

prop.table(table(bp_obese$sex))

##### obese
# continuous

analysis <- function(var) {
  
  # graphical
  par(mfrow = c(1,2))
  boxplot(var)
  hist(var)
  
  # main statistics
  summary <- summary(var)
  round(c(summary, sd = sd(var)),2)
}

analysis(bp_obese$obese)

analysis(bp_obese$bp)

## b) Does Gender affect blood preassure? It testing if the mean of bp for women 
# are equal to men so its a and hipotesis test on the difference of means 
# for independent samples

# HO = men - women = 0
# H1: men- women != 0

# Requirements
# - X and Y  are independent, 
# - Its a large samole given it is above n = 50

par(mfrow = c(1,2))
boxplot(bp_obese$bp[bp_obese$sex == 1])
boxplot(bp_obese$bp[bp_obese$sex == 0])

male <- bp_obese$bp[bp_obese$sex == 0]
female <- bp_obese$bp[bp_obese$sex == 1]

# despite the  they are faily symetric so we assume normality

t.test(bp ~ sex, bp_obese)

# We reject the null hipothesis
