dados = read.csv(file.choose(), header = T, sep = ",")
CDF_sample = ecdf(dados$Latitude)
mean_sample = mean(dados$Latitude)
sd_sample = sd(dados$Latitude)
print(summary(dados))

#Normal distribution test
model_norm = rnorm(length(dados$Latitude), mean_sample, sd_sample)
CDF_norm = ecdf(model_norm)
plot(CDF_norm, col="black") 
plot(CDF_sample, col="blue", add=TRUE)

#LogNormal distribution test
LogNormal = rlnorm(length(dados$Latitude), mean_sample, sd_sample)
CDF_LogNormal = ecdf(LogNormal)
plot(CDF_LogNormal, col="pink", add=TRUE)

#Uniform distribution test
minimum = min(dados$Latitude)
maximum = max(dados$Latitude)
model_uniformDistribution = runif(length(dados$Latitude), minimum, maximum)
CDF_uniformDistribution = ecdf(model_uniformDistribution)
plot(CDF_uniformDistribution, col="green", add=TRUE)

#Exponencial distribution test
model_exponencialDistribution=rexp(length(dados$Latitude), sd_sample)
CDF_exponencialDistribution = ecdf(model_exponencialDistribution)
plot(CDF_exponencialDistribution, col="red", add=TRUE)


Probabilidade = prob(dados$Latitude)

#Poisson distribution test
model_poisson_distribution=rpois(length(dados$Latitude), sd_sample)
CDF_poisson_distribution=ecdf(model_poisson_distribution)
plot(CDF_poisson_distribution, col="yellow", add=TRUE)

#Binomial distribution test
model_binomial_distribution=rbinom(length(dados$Latitude), )
CDF_binomial_distribution=ecdf(model_binomial_distribution)
plot(CDF_binomial_distribution, col="brown", add=TRUE)

#Weibull distribution test
model_weibull_distribution=rweibull(length(dados$Latitude), shape, scale = 1)
CDF_binomial_distribution=ecdf(model_weibull_distribution)
plot(CDF_weibull_distribution, col= "orange", add=TRUE)

#Gamma distribution test
model_gamma_distribution=rgamma(length(dados$Latitude), shape, rate = 1, scale = 1/rate)
CDF_gamma_distribution=ecdf(model_gamma_distribution)
plot(CDF_gamma_distribution, col="purple", add=TRUE)
