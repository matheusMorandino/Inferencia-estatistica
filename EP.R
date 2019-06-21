dados = read.csv(file.choose(), header = T, sep = ",")

#Sample data colection
sample_dados = dados$y
CDF_sample = ecdf(dados$y)
mean_sample = mean(dados$y)
sd_sample = sd(dados$y)
minimum = min(dados$y)
maximum = max(dados$y)
print(summary(dados))
plot(CDF_sample, col="black")

#Kolmogorov-Smirnov function
KS_test = function(px, py) {
  for(i in seq(minimum, maximum, by = 0.1)) {
    if(abs(px(i)-py(i)) > 0.05) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#Normal distribution model
test_normal = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:100) {
    model_norm = rnorm(length(dados$y), mean_sample, sd_sample) 
    model_CDF = ecdf(model_norm)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}

#LogNormal distribution model
test_LogNormal = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:100) {
    LogNormal = rlnorm(length(dados$y), mean_sample, sd_sample)
    model_CDF = ecdf(LogNormal)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}


#Uniform distribution model
test_uniform = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:100) {
    model_uniformDistribution = runif(length(dados$y), minimum, maximum)
    model_CDF = ecdf(model_uniformDistribution)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}

#Exponencial distribution model
test_exp = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:100) {
    model_exponencialDistribution=rexp(length(dados$y), sd_sample) ## alterem essa parte de acordo
    model_CDF = ecdf(model_exponencialDistribution)                ## com a distribuição, é só subistituir com o que fizemos
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}

#Poisson distribution model
test_poisson = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:100) {
    model_poisson_distribution=rpois(length(dados$y), mean_sample)
    model_CDF=ecdf(model_poisson_distribution)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}

#Binomial distribution model
model_binomial_distribution=rbinom(dados$y, size = length(dados$y), prob = mean_sample/length(dados$y))
CDF_binomial_distribution=ecdf(model_binomial_distribution)
plot(CDF_binomial_distribution, col="brown", add=TRUE)

#Weibull distribution model
model_weibull_distribution=rweibull(length(dados$y), shape = 1, scale = mean_sample/log(2,exp(1)))
CDF_weibull=ecdf(model_weibull_distribution)
plot(CDF_weibull, col= "orange", add=TRUE)

#Gamma distribution model
model_gamma=rgamma(length(dados$y), shape = (mean_sample^2)/(sd_sample^2), rate = mean_sample/(sd_sample^2))
CDF_gamma=ecdf(model_gamma_distribution)
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