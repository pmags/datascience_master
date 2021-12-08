########## Exercício 1

# Criar vector com amosta

# A)
amostra <- c(62,72,51,53,62,49,55,56,58,52,48,72,89,72,66,97,55,52,50,61,50,74,66,62)

round(mean(amostra))
median(amostra)
round(sd(amostra),1)

#B) 

amostra2 <- 2*amostra

round(mean(amostra2))
median(amostra2)
round(sd(amostra2),1)

# A média, mediana e desvio padrão são afetadas pelo mesmo efeito

#c) 

amostra5 <- amostra + 5

round(mean(amostra5))
median(amostra5)
round(sd(amostra5),1)

# Média e Mediana afetadas, mas desvio padrão mantém-se

#D)

plot <- boxplot(amostra)

Q1 <- plot$stats[2]
Q3 <- plot$stats[4]

# outlier é 97

########### Exercicio 2

library(haven)
bp_obese <- read_sav("estatistica_pratica/bp.obese.sav")
bp_obese <- as.data.frame(bp_obese)

# a)
dim(bp_obese)[1]

# b) yes sex, obese, bp

# c) 

table(bp_obese$sex)

barplot(table(bp_obese$sex))

# d)
bp <- bp_obese$bp

# d1)
sum(is.na(bp))

# d2)
mean(bp)
median(bp)
quantile(bp, 0.25)
quantile(bp,0.75)
quantile(bp,0.50)

# d3)
max(bp)
min(bp)

# d4)
sd(bp)

# d5)
sd(bp_obese$bp)/sqrt(length(bp_obese$bp))

# d6)
quantile(bp, probs = c(0.25,0.5,0.75,0.05,0.95))

#d7)
floor <- mean(bp) - 2 * sd(bp)  
cap <- mean(bp) + 2 * sd(bp)  

range <- bp[bp < cap & bp > floor]
round(mean(range))

#d8)
par(mfrow=c(1,3))
boxplot(bp)
hist(bp)
qqnorm(bp)
qqline(bp)

#F)
bp_obese_f <- bp_obese[bp_obese$sex == 1,"bp"]

par(mfrow = c(1,3))
hist(bp_obese_f)
boxplot(bp_obese_f)
qqnorm(bp_obese_f)
qqline(bp_obese)


#G) 
bp_obese$cat <- ifelse( bp_obese$obese>1, "High", "Low") 

t <- table(bp_obese$cat, bp_obese$sex)

round(table(bp_obese$cat, bp_obese$sex) / length(bp_obese$cat),2)

#j

tapply(bp_obese$bp, bp_obese$sex, mean)
tapply(bp_obese$bp, bp_obese$sex, sd)

