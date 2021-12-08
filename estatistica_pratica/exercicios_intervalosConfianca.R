### Exercicio 1

meanRV <-  161
stdRV <- 8

#a) intervalo de confiança para a média com a variância conhecida
# assumimos que a normalidde é respeitada

low <-  round(meanRV - stdRV/sqrt(100)*qnorm(0.975),2)
high <- round(meanRV + stdRV/sqrt(100)*qnorm(0.975),2)

#b) increasing to 99%
low <-  round(meanRV - stdRV/sqrt(100)*qnorm(0.995),2)
high <- round(meanRV + stdRV/sqrt(100)*qnorm(0.995),2)

# it increases

### Exercicio 2

meanRV <- 4.3
sdRV <- 0.03

low <-  round(meanRV - sdRV/sqrt(100)*qnorm(0.995),2)
high <- round(meanRV + sdRV/sqrt(100)*qnorm(0.995),2)

# 99% of the times the population mean will be between 4.29 and 4.31

### Exercicio 3
n <- 50
meanRV <- 17
sdRV <- 1

k = 1-0.9661051

low <- meanRV - sdRV/sqrt(n)*qnorm(1-k/2)
high <- meanRV + sdRV/sqrt(n)*qnorm(1-k/2)

1- (pnorm(0.3/(1/sqrt(50)))-1)*-2 