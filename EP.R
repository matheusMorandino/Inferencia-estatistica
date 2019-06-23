dados = read.csv(file.choose(), header = T, sep = ",")

#Sample data colection
sample_dados = dados[,1]
CDF_sample = ecdf(as.double(dados[,1]))
mean_sample = mean(as.double(dados[,1]))
sd_sample = sd(as.double(dados[,1]))
minimum = min(as.double(dados[,1]))
maximum = max(as.double(dados[,1]))
print(summary(dados))
plot(CDF_sample, col="black")

print(dados[,1])
for (i in 1:length(dados[,1])){
  if(is.na(i)) print("Is null")
  else print("ok")
}

print(length(dados[,1]))

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
  for(i in 1:1000) {
    model_norm = rnorm(length(px), mean_sample, sd_sample) 
    model_CDF = ecdf(model_norm)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000 #pq esta dividindo por mil?
  
  return(KS_coef)
}

#LogNormal distribution model
test_LogNormal = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:1000) {
    LogNormal = rlnorm(length(px), mean_sample, sd_sample)
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
  for(i in 1:1000) {
    model_uniformDistribution = runif(length(px), minimum, maximum)
    model_CDF = ecdf(model_uniformDistribution)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}

#Exponencial distribution model
test_exponencial = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:1000) {
    model_exponencialDistribution=rexp(length(px), mean_sample) 
    model_CDF = ecdf(model_exponencialDistribution)
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
  for(i in 1:1000) {
    model_poisson_distribution=rpois(length(px), mean_sample)
    model_CDF=ecdf(model_poisson_distribution)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}

#Binomial distribution model
test_binomial = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:1000) {
    model_binomial_distribution=rbinom(px, size = length(px), prob = mean_sample/length(px))
    model_CDF=ecdf(model_binomial_distribution)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}

#Weibull distribution model
test_weibull = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:1000) {
    model_weibull_distribution=rweibull(length(px), shape = 1, scale = mean_sample/log(2,exp(1)))
    model_CDF=ecdf(model_weibull_distribution)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}

#Gamma distribution model
test_gamma = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:1000) {
    model_gamma_distribution=rgamma(length(px), shape = (mean_sample^2)/(sd_sample^2), rate = mean_sample/(sd_sample^2))
    model_CDF=ecdf(model_gamma_distribution)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}


#Geometric distribution model
test_geometric = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:1000) {
    model_geometric=rgeom(length(px), 1/mean_sample)
    model_CDF=ecdf(model_geometric)
    plot(model_CDF)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}


#Power law distribution model
test_power_law = function(px) {
  ln_sum = 0
  x_min = 1
  
  for(i in 1:length(px)) {
    ln_sum = log(abs(px[i])/(x_min-1/2), exp(1)) + ln_sum
  }
  alpha = 1 + length(px)/(ln_sum)
  
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:1000) {
    model_power_law = rpldis(length(px), x_min, alpha, 0)
    model_CDF=ecdf(model_power_law)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  return(KS_coef)
}
#Hypergeometric distribution model

print(is.na(dados[,1]))

normal=test_normal(dados[,1])
log_normal=test_LogNormal(dados[,1])
uniform=test_uniform(dados[,1])
exponencial=test_exponencial(dados[,1])
poisson=test_poisson(dados[,1])
binomial=test_binomial(dados[,1])
weibull=test_weibull(dados[,1])
gamma=test_gamma(dados[,1])
geometric=test_geometric(dados[,1])
power_law=test_power_law(dados[,1])

array=c(normal, log_normal, uniform, exponencial, poisson, binomial, weibull, gamma, geometric, power_law)
print(c("Normal: ", normal, "% de chance"))
print(c("Lognormal: "), lognormal, "% de chance")
print(c("Uniforme: ", uniform, "% de chance"))
print(c("Exponencial: ", exponencial, "% de chance"))
print(c("Poisson: ", poisson, "% de chance"))
print(c("Binomial, ", binomial, "% de chance"))
print(c("Weibull: ", weibull, "% de chance"))
print(c("Gamma: ", gamma, "% de chance"))
print(c("Geometrica: ", geometric, "% de chance"))
print(c("Power Law: ", power_law, "% de chance"))
print(as.double(weibull))
print(2)
a=2
print(a)

