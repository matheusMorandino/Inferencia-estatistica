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
    if(KS_test(sample_CDF,model_CDF) == TRUE) {
      KS_coef = KS_coef + 1
    }
  }
  KS_coef = KS_coef/1000
  
  return(as.double(KS_coef))
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
  
  #insertion_sort=function(array){
   # for(i in 2:length(array))
    #  aux=array[i]
    #  j=i-1
     # while(j>0 && aux<array[j])
      #  array[j+1]=array[j]
       # j=j-1
    #  array[j+1]=aux
    #return(array)
  #}
   
  a=c(0.1, 0.2)
  print(a)
  normal=test_normal(dados$y)
  log_normal=test_LogNormal(dados$y)
  uniform=test_uniform(dados$y)
  exponencial=test_exponencial(dados$y)
  poisson=test_poisson(dados$y)
  binomial=test_binomial(dados$y)
  weibull=test_weibull(dados$y)
  gamma=test_gamma(dados$y)
  geometric=test_geometric(dados$y)
  power_law=test_power_law(dados$y)
  
  array=c(normal, log_normal, uniform, exponencial, poisson, binomial, weibull, gamma, geometric)#power law
  sorted_array=insertion_sort(array)
  print(array)
  print(a)
  print("Normal: %d", array[1] + "% de chance")
  print("Lognormal: " + array[2] + "% de chance")
  print("Uniforme: "+ array[3] + "% de chance")
  print("Exponencial: " + array[4] + "% de chance")
  print("Poisson: " + array[5] + "% de chance")
  print("Binomial: " + array[6] + "% de chance")
  print("Weibull: " + array[7] + "% de chance")
  print("Gamma: " + array[8] + "% de chance")
  print("Geometrica: " + array[9] + "% de chance")
  print("Power law: " + array[10] + "% de chance")
  
  
  #for(i in 1:length(sorted_array)){
   # print()
  #}   
  #insertion_sort()
  #tests
  #find_best_distribution = function(px) {
    
    #tests <- ()
   # max_KS_coef = 0
    #for(i in 1:11){
     # if(max_KS_coef < )
      #  max_KS_coef = 
    #}
  #}
  
#}
