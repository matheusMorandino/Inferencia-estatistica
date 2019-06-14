dados = read.csv(file.choose(), header = T, sep = ",")
CDF_sample = ecdf(dados$Latitude)
mean_sample = mean(dados$Latitude)
sd_sample = sd(dados$Latitude)
print(summary(dados))
plot(CDF_sample, col="black")

#Normal distribution test
model_norm = rnorm(length(dados$Latitude), mean_sample, sd_sample)
CDF_norm = ecdf(model_norm)
plot(CDF_norm, col="blue", add=TRUE) 

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

#Poisson distribution test
model_poisson_distribution=rpois(length(dados$Latitude), sd_sample^2)
CDF_poisson_distribution=ecdf(model_poisson_distribution)
plot(CDF_poisson_distribution, col="yellow", add=TRUE)

#Binomial distribution test
model_binomial_distribution=rbinom(dados$Latitude, size = length(dados$Latitude),prob = mean_sample/length(dados$Latitude))
CDF_binomial_distribution=ecdf(model_binomial_distribution)
plot(CDF_binomial_distribution, col="brown", add=TRUE)

#Weibull distribution test
model_weibull_distribution=rweibull(length(dados$Latitude), shape = 1, scale = 1)
CDF_weibull=ecdf(model_weibull_distribution)
plot(CDF_weibull, col= "orange", add=TRUE)

#Gamma distribution test
model_gamma_distribution=rgamma(length(dados$Latitude), shape = 1, rate = 1)
CDF_gamma_distribution=ecdf(model_gamma_distribution)
plot(CDF_gamma_distribution, col="purple", add=TRUE)

#Power law distribution test
model_power_law = rpldis(length(dados$Latitude), mean_sample, mean_sample, 0)
CDF_power_law = ecdf(model_power_law)
plot(CDF_power_law)
plot(model_power_law)
