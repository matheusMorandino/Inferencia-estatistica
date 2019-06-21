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
  for(i in 1:1000) {
    model_norm = rnorm(length(dados$y), mean_sample, sd_sample) 
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
  for(i in 1:1000) {
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
  for(i in 1:1000) {
    model_exponencialDistribution=rexp(length(dados$y), mean_sample) 
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
test_binomial = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:1000) {
    model_binomial_distribution=rbinom(dados$y, size = length(dados$y), prob = mean_sample/length(dados$y))
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
    model_weibull_distribution=rweibull(length(dados$y), shape = 1, scale = mean_sample/log(2,exp(1)))
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
    model_gamma=rgamma(length(dados$y), shape = (mean_sample^2)/(sd_sample^2), rate = mean_sample/(sd_sample^2))
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
    model_geometric=rgeom(length(dados$y), 1/mean_sample)
    model_CDF=ecdf(model_geometric)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}

#Power law distribution model
ln_sum = 0
x_min = 1

for(i in 1:length(dados$y)) {
  ln_sum = log(abs(dados$y[i])/(x_min-1/2), exp(1)) + ln_sum
}
alpha = 1 + length(dados$y)/(ln_sum)

test_power_law = function(px) {
  sample_CDF = ecdf(px)
  KS_coef =  0
  for(i in 1:1000) {
    model_power_law = rpldis(length(dados$y), x_min, alpha, 0)
    model_CDF=ecdf(model_power_law)
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
  
  #Hypergeometric distribution model
  
  
  #tests
  find_best_distribution = function(px) {
    tests <- ()
    max_KS_coef = 0
    for(i in 1:11){
      if(max_KS_coef < )
        max_KS_coef = 
    }
  }
  
}