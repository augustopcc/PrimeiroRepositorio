#Pedro Augusto Cardoso Costa

set.seed(2020054080)

############# Questão 1 #############  

m <- 10000
alpha <- 0.05
n_sizes <- c(20, 30, 100)

simula_cobertura <- function(n, dist_fun, dist_params, true_mean) {
  cobertura <- numeric(length(n))
  
  for (l in 1:length(n)) {
    y <- replicate(m, {
      amostra <- do.call(dist_fun, c(list(n[l]), dist_params))
      
      media_amostral <- mean(amostra)
      erro_padrao <- sd(amostra) / sqrt(n[l])
      margem_erro <- qt(0.975, df = n[l] - 1) * erro_padrao
      ic_inf <- media_amostral - margem_erro
      ic_sup <- media_amostral + margem_erro
      
      ifelse(true_mean >= ic_inf & true_mean <= ic_sup, 1, 0)
    })
    
    cobertura[l] <- mean(y)
  }
  
  return(cobertura)
}
resultados <- list()

true_mean_chisq <- 2
resultados$chisq <- simula_cobertura(n_sizes, rchisq, list(df = 2), true_mean_chisq)

true_mean_exp <- 1  
resultados$exp <- simula_cobertura(n_sizes, rexp, list(rate = 1), true_mean_exp)

par(mfrow = c(1, 2))
par(mar = c(4, 4, 1, 1))

plot(n_sizes, resultados$chisq, type = "b", pch = 16, cex = 0.75, ylim = c(0.45, 1), main = "Distribuição chi² (df = 2)")
abline(h = 0.95, col = 2, lty = 2, lwd = 2)

plot(n_sizes, resultados$exp, type = "b", pch = 16, cex = 0.75, ylim = c(0.45, 1), main = "Distribuição Exponencial (λ = 1)")
abline(h = 0.95, col = 2, lty = 2, lwd = 2)


##################### Exercicio 2 ##################### 
IC_teorico <- function(amostra, conf = 0.95) {
  n <- length(amostra)
  media <- mean(amostra)
  erro_padrao <- sd(amostra) / sqrt(n)
  margem_erro <- qt((1 + conf) / 2, df = n - 1) * erro_padrao
  c(media - margem_erro, media + margem_erro)
}
IC_boot_normal <- function(amostra, B = 200, estimador = mean, conf = 0.95) {
  est_pontual <- estimador(amostra)
  reamostras <- replicate(B, sample(amostra, length(amostra), replace = TRUE))
  est_reamostras <- apply(reamostras, 2, estimador)
  li <- est_pontual - qnorm((1 + conf) / 2) * sd(est_reamostras)
  ls <- est_pontual + qnorm((1 + conf) / 2) * sd(est_reamostras)
  c(li, ls)
}
IC_boot_perc <- function(amostra, B = 200, estimador = mean, conf = 0.95) {
  reamostras <- replicate(B, sample(amostra, length(amostra), replace = TRUE))
  est_reamostras <- apply(reamostras, 2, estimador)
  li <- quantile(est_reamostras, (1 - conf) / 2)
  ls <- quantile(est_reamostras, (1 + conf) / 2)
  c(li, ls)
}

# Parâmetros do estudo
m <- 10000
n_sizes <- c(20, 50, 100)
conf <- 0.95

calcula_cobertura <- function(n, dist_fun, dist_params, true_mean) {
  resultados <- matrix(0, nrow = length(n), ncol = 3)
  colnames(resultados) <- c("t-Student", "Bootstrap Normal", "Bootstrap Percentílico")
  
  for (l in 1:length(n)) {
    y_t <- numeric(m)
    y_boot_normal <- numeric(m)
    y_boot_perc <- numeric(m)
    
    for (j in 1:m) {
      amostra <- do.call(dist_fun, c(list(n[l]), dist_params))
      
      ic_t <- IC_teorico(amostra, conf)
      y_t[j] <- ifelse(true_mean >= ic_t[1] & true_mean <= ic_t[2], 1, 0)
      
      ic_boot_normal <- IC_boot_normal(amostra, B = 200, estimador = mean, conf = conf)
      y_boot_normal[j] <- ifelse(true_mean >= ic_boot_normal[1] & true_mean <= ic_boot_normal[2], 1, 0)
      
      ic_boot_perc <- IC_boot_perc(amostra, B = 200, estimador = mean, conf = conf)
      y_boot_perc[j] <- ifelse(true_mean >= ic_boot_perc[1] & true_mean <= ic_boot_perc[2], 1, 0)
    }
    
    resultados[l, 1] <- mean(y_t)
    resultados[l, 2] <- mean(y_boot_normal)
    resultados[l, 3] <- mean(y_boot_perc)
  }
  
  return(resultados)
}

true_mean_normal <- 0
resultados_normal <- calcula_cobertura(n_sizes, rnorm, list(mean = 0, sd = 1), true_mean_normal)

true_mean_lognormal <- exp(0.5)  
resultados_lognormal <- calcula_cobertura(n_sizes, rlnorm, list(meanlog = 0, sdlog = 1), true_mean_lognormal)

print("Resultados para distribuição Normal:")
print(resultados_normal)

print("Resultados para distribuição Lognormal:")
print(resultados_lognormal)
