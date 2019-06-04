dados = read.csv(file.choose(), header = T, sep = ",")

#Teste para distribuição normal
CDF_sample = ecdf(dados$Latitude)
mean_sample = mean(dados$Latitude)
variance_sample = var(dados$Latitude)

summary(dados$Latitude)

model_norm = rnorm(length(dados$Latitude), mean_sample, variance_sample)
CDF_norm = ecdf(model_norm)

plot(CDF_sample, col="red")
plot(CDF_norm, col="green") 
