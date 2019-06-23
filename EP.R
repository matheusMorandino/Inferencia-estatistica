dados = read.csv(file.choose(), header = T, sep = ",")

#Sample data colection
sample_dados = as.double(dados[,1])
CDF_sample = ecdf(as.double(dados[,1]))
mean_sample = mean(as.double(dados[,1]))
sd_sample = sd(as.double(dados[,1]))
minimum = min(as.double(dados[,1]))
maximum = max(as.double(dados[,1]))
print(summary(dados))
plot(sample_dados, col="blue")
plot(CDF_sample, col="black", add=TRUE)
hist(sample_dados, add=TRUE)

print(dados[,1])

#Kolmogorov-Smirnov function
KS_test = function(px, py) {
  for(i in seq(minimum, maximum, by = 0.1)) {
    if(abs(as.double(px(i))-as.double(py(i))) > 0.05) {
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
  KS_coef = KS_coef/1000
  
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
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(KS_coef)
}


#Power law
test_power_law=function(px){
  sorted_px=sort(px)
  complement=1-ecdf(sorted_px)(sort(px))
  sorted_px=sorted_px[1:length(sorted_px)-1]
  complement=complement[1:length(complement)-1]
  
  x=log(sorted_px, 10)
  y=log(complement, 10)
  correlation=as.double(abs(cor(x,y)))
  
  return (correlation)
}

normal=test_normal(dados[,1])
lognormal=test_LogNormal(dados[,1])
uniform=test_uniform(dados[,1])
exponencial=test_exponencial(dados[,1])
poisson=test_poisson(dados[,1])
binomial=test_binomial(dados[,1])
weibull=test_weibull(dados[,1])
gamma=test_gamma(dados[,1])
geometric=test_geometric(dados[,1])
power_law=test_power_law(dados[,1])
print(c("Normal: ", 100*normal, "% de chance"))
print(c("Lognormal: ", 100*lognormal, "% de chance"))
print(c("Uniforme: ", 100*uniform, "% de chance"))
print(c("Exponencial: ", 100*exponencial, "% de chance"))
print(c("Poisson: ", 100*poisson, "% de chance"))
print(c("Binomial, ", 100*binomial, "% de chance"))
print(c("Weibull: ", 100*weibull, "% de chance"))
print(c("Gamma: ", 100*gamma, "% de chance"))
print(c("Geometrica: ", 100*geometric, "% de chance"))
print(c("Power Law: ", 100*power_law, "% de chance"))
