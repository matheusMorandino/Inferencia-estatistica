#dados = read.csv(file.choose(), header = T, sep = ",")
#install.packages("Ecdat", repos="http://R-Forge.R-project.org")
#library(Ecdat)
data(Icecream)

#Teste para distribuição normal
CDF_sample = ecdf(Icecream$temp)
mean_sample = mean(Icecream$temp)
variance_sample = sd(Icecream$temp)

model_norm = rnorm(length(Icecream$temp), mean_sample, variance_sample)
CDF_norm = ecdf(model_norm)

plot(CDF_norm, col="black") 
plot(CDF_sample, col="blue", add=TRUE)
