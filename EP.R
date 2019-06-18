dados = read.csv(file.choose(), header = T, sep = ",")

#Sample data colection
CDF_sample = ecdf(dados$y)
mean_sample = mean(dados$y)
sd_sample = sd(dados$y)
minimum = min(dados$y)
maximum = max(dados$y)
print(summary(dados))
plot(CDF_sample, col="black")

#Kullback-leibler function
KL_test = function(px, py, base) {
  sum = 0
  for(i in minimum:maximum) {
    sum = px(i)*log(px(i)/py(i), base) + sum
  }
  return(sum);
}

#Normal distribution model
model_norm = rnorm(length(dados$y), mean_sample, sd_sample)
CDF_norm = ecdf(model_norm)
plot(CDF_norm, col="blue", add=TRUE) 

#LogNormal distribution model
LogNormal = rlnorm(length(dados$y), mean_sample, sd_sample)
CDF_LogNormal = ecdf(LogNormal)
plot(CDF_LogNormal, col="pink", add=TRUE)

#Uniform distribution model
model_uniformDistribution = runif(length(dados$y), minimum, maximum)
CDF_uniformDistribution = ecdf(model_uniformDistribution)
plot(CDF_uniformDistribution, col="green", add=TRUE)

#Exponencial distribution model
model_exponencialDistribution=rexp(length(dados$y), sd_sample)
CDF_exponencialDistribution = ecdf(model_exponencialDistribution)
plot(CDF_exponencialDistribution, col="red", add=TRUE)

#Poisson distribution model
model_poisson_distribution=rpois(length(dados$y), mean_sample)
CDF_poisson_distribution=ecdf(model_poisson_distribution)
plot(CDF_poisson_distribution, col="yellow", add=TRUE)

#Binomial distribution model
model_binomial_distribution=rbinom(dados$y, size = length(dados$y), prob = mean_sample/length(dados$y))
CDF_binomial_distribution=ecdf(model_binomial_distribution)
plot(CDF_binomial_distribution, col="brown", add=TRUE)

#Weibull distribution model
model_weibull_distribution=rweibull(length(dados$y), shape = 1, scale = mean_sample/log(2,exp(1)))
CDF_weibull=ecdf(model_weibull_distribution)
plot(CDF_weibull, col= "orange", add=TRUE)

#Gamma distribution model
model_gamma_distribution=rgamma(length(dados$y), shape = (mean_sample^2)/(sd_sample^2), rate = mean_sample/(sd_sample^2))
CDF_gamma_distribution=ecdf(model_gamma_distribution)
plot(CDF_gamma_distribution, col="purple", add=TRUE)

#Geometric distribution model
model_geometric=rgeom(length(dados$y), 1/mean_sample)
CDF_geometric=ecdf(model_geometric)
plot(CDF_geometric, col="grey", add=TRUE)

#Power law distribution model
ln_sum = 0
x_min = 1

for(i in 1:length(dados$y)) {
  ln_sum = log(abs(dados$y[i])/(x_min-1/2), exp(1)) + ln_sum
}
alpha = 1 + length(dados$y)/(ln_sum)

model_power_law = rpldis(length(dados$y), x_min, alpha, 0)
CDF_power_law = ecdf(model_power_law)
plot(CDF_power_law, col="cyan", add=TRUE)

#Hypergeometric distribution model
model_hyper=rhyper(length(dados$y), )

