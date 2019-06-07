dados = read.csv(file.choose(), header = T, sep = ",")
#install.packages("Ecdat", repos="http://R-Forge.R-project.org")
#library(Ecdat)
#data(Icecream)

#Teste para distribuição normal
CDF_sample = ecdf(dados$Latitude)
mean_sample = mean(dados$Latitude)
sd_sample = sd(dados$Latitude)

model_norm = rnorm(length(dados$Latitude), mean_sample, sd_sample)
CDF_norm = ecdf(model_norm)

plot(CDF_norm, col="black") 
plot(CDF_sample, col="blue", add=TRUE)

LogNormal = rlnorm(length(dados$Latitude), mean_sample, sd_sample)
CDF_LogNormal = ecdf(LogNormal)
plot(CDF_LogNormal, col="pink", add=TRUE)

Probabilidade = r(dados$Latitude)

minimum = min(dados$Latitude)
maximum = max(dados$Latitude)
model_uniformDistribution = runif(length(dados$Latitude), minimum, maximum)
CDF_uniformDistribution = ecdf(model_uniformDistribution)
plot(CDF_uniformDistribution, col="green", add=TRUE)
