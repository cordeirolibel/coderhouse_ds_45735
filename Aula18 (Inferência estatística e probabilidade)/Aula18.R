
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
#   /$$            /$$$$$$                                                  /$$          
#  |__/           /$$__  $$                                                |__/          
#  /$$ /$$$$$$$ | $$  \__//$$$$$$   /$$$$$$   /$$$$$$  /$$$$$$$   /$$$$$$$ /$$  /$$$$$$ 
#  | $$| $$__  $$| $$$$   /$$__  $$ /$$__  $$ /$$__  $$| $$__  $$ /$$_____/| $$ |____  $$
#  | $$| $$  \ $$| $$_/  | $$$$$$$$| $$  \__/| $$$$$$$$| $$  \ $$| $$      | $$  /$$$$$$$
#  | $$| $$  | $$| $$    | $$_____/| $$      | $$_____/| $$  | $$| $$      | $$ /$$__  $$
#  | $$| $$  | $$| $$    |  $$$$$$$| $$      |  $$$$$$$| $$  | $$|  $$$$$$$| $$|  $$$$$$$
#  |__/|__/  |__/|__/     \_______/|__/       \_______/|__/  |__/ \_______/|__/ \_______/
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# População

idade <- c(25,30,40,18,17,22,45,19,21,36,33,30,29,55,41,26,20,19,56,39,24,22,35,
           45,16,32,45,22,56,37,45,46,31,35,38,39,42,35,25,22,29,40,45,44,43)
length(idade)

# Media
mean(idade)
# coeficiente de variação
sd(idade)/mean(idade)*100

hist(idade)

hist(idade, prob=TRUE)
abline(v = mean(idade),  col = "red", lwd = 2)
#abline(v = ic(idade)[1], col = "red", lwd = 2, lty=2)
#abline(v = ic(idade)[2], col = "red", lwd = 2, lty=2)



# -----------------------------------------------------------------------
# Amostra
idade_amostra <- sample(x = idade, size = 15)
idade_amostra

# Estimadores

# Media
mean(idade_amostra)
# coeficiente de variação
sd(idade_amostra)/mean(idade_amostra)*100

# -----------------------------------------------------------------------
# Distribuição do estimador 

estimativa_media <- c() 

n_amostral  <- 5
n_iteracoes <- 10000
  
for (i in 1:n_iteracoes) {
  idade_amostra <- sample(x = idade, size = n_amostral)
  estimativa_media[i] <- mean(idade_amostra)
  }

hist(estimativa_media)
min(estimativa_media)
max(estimativa_media)
# coeficiente de variação
sd(estimativa_media)/mean(estimativa_media)*100


# -----------------------------------------------------------------------
# Distribuição Normal das estimativas

hist(estimativa_media, prob=TRUE)
abline(v = mean(estimativa_media), col = "red", lwd = 2)
curve(dnorm(x, mean = mean(estimativa_media), sd = sd(estimativa_media)), 
      col="darkblue", lwd = 2, add=TRUE, yaxt="n")

?dnorm

# -----------------------------------------------------------------------
# Intervalo de confiança

# Tamanho do amostra das estimativas
n <- length(estimativa_media);
n
# Média das estimativas 
theta_bar <- mean(estimativa_media)
theta_bar
# Desvio padrão das estimativas
s <- sd(estimativa_media); 
s

# Margem de erro, 99% de confianca
margem <- qt((1-0.99)/2  , df = n-1,lower.tail=F)*s/sqrt(n)
#calculate lower and upper bounds of confidence interval
low <- theta_bar - margem
low
high <- theta_bar + margem
high

#media esta entre 33.4986 e 33.73712 com 99% de confianca 

hist(estimativa_media, prob=TRUE)
abline(v = mean(estimativa_media), col = "red", lwd = 2)
curve(dnorm(x, mean = mean(estimativa_media), sd = sd(estimativa_media)), 
      col="darkblue", lwd = 2, add=TRUE, yaxt="n")

abline(v = low,  col = "black", lwd = 2)
abline(v = high, col = "black", lwd = 2)


