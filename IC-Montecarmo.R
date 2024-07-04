############# Questão 1 #############  


# Parâmetros gerais
m <- 10000
alpha <- 0.05
n_sizes <- c(20, 30, 100)

# Função para simular e calcular a cobertura
simula_cobertura <- function(n, dist_fun, dist_params, true_mean) {
  cobertura <- numeric(length(n))
  
  for (l in 1:length(n)) {
    y <- replicate(m, {
      # Gerar amostra
      amostra <- do.call(dist_fun, c(list(n[l]), dist_params))
      
      # Calcular o intervalo de confiança
      media_amostral <- mean(amostra)
      erro_padrao <- sd(amostra) / sqrt(n[l])
      margem_erro <- qt(0.975, df = n[l] - 1) * erro_padrao
      ic_inf <- media_amostral - margem_erro
      ic_sup <- media_amostral + margem_erro
      
      # Verificar se a verdadeira média está no intervalo de confiança
      ifelse(true_mean >= ic_inf & true_mean <= ic_sup, 1, 0)
    })
    
    # Calcular a cobertura empírica
    cobertura[l] <- mean(y)
  }
  
  return(cobertura)
}

# Resultados para cada combinação de tamanho de amostra e distribuição
resultados <- list()

# Distribuição chi² com 2 graus de liberdade
true_mean_chisq <- 2  # A média teórica da chi² com 2 df é 2
resultados$chisq <- simula_cobertura(n_sizes, rchisq, list(df = 2), true_mean_chisq)

# Distribuição Exponencial com taxa = 1
true_mean_exp <- 1  # A média teórica da exponencial com lambda = 1 é 1
resultados$exp <- simula_cobertura(n_sizes, rexp, list(rate = 1), true_mean_exp)

# Plotar os resultados
par(mfrow = c(1, 2))
par(mar = c(4, 4, 1, 1))

# Plot para a distribuição chi²
plot(n_sizes, resultados$chisq, type = "b", pch = 16, cex = 0.75, ylim = c(0.45, 1), main = "Distribuição chi² (df = 2)")
abline(h = 0.95, col = 2, lty = 2, lwd = 2)

# Plot para a distribuição exponencial
plot(n_sizes, resultados$exp, type = "b", pch = 16, cex = 0.75, ylim = c(0.45, 1), main = "Distribuição Exponencial (λ = 1)")
abline(h = 0.95, col = 2, lty = 2, lwd = 2)







No R.Faça um estudo de Monte Carlo para comparar a probabilidade de cobertura dos intervalos de confiança bootstrap para a média populacional gerando amostras das distribuições Normal e Lognormal. Considere os dois métodos bootstrap(percentilico e normal) para a obtenção dos intervalos de confiança. Faça considerando um tamanho amostral pequeno e grande (tamanhos 20 50 100). Compare com o intervalo de confiança teórico que utiliza a distribuição 𝑡-Student.
Exemplo de codigo do intervalo bootstrap normal é:
  IC_boot_normal = function(amostra, B = 200, estimador, conf = 0.95,...){
    # estimador é uma função a ser passada como parâmetro
    est_pontual <- estimador(amostra)
    
    ## Replicação
    reamostras <- replicate(n = B, 
                            sample(x = amostra, size = length(amostra), replace = TRUE),
                            simplify = T )
    est_reamostras <- apply(X = reamostras, FUN = estimador, MARGIN = 2 )
    # Limites do intervalo
    li <- est_pontual - qnorm((1+conf)/2) * sd(est_reamostras)
    ls <- est_pontual + qnorm((1+conf)/2) * sd(est_reamostras)
    return(c(li, ls))
  }

## Teste
dados <- rnorm(100, 10, 3)
IC_boot_normal(amostra = dados, B = 200, estimador = mean, conf = 0.95)
Exemplo de codigo de intervalo bootstrap eprcentilico:
  IC_boot_perc = function(amostra, B = 200, estimador, conf = 0.95,...){
    # estimador é uma função a ser passada como parâmetro
    ## Replicação
    reamostras <- replicate(n = B, 
                            sample(x = amostra, size = length(amostra), replace = TRUE),
                            simplify = T )
    est_reamostras <- apply(X = reamostras, FUN = estimador, MARGIN = 2 )
    # Limites do intervalo
    li <- quantile(est_reamostras, (1-conf)/2)
    ls <- quantile(est_reamostras, (1+conf)/2)
    return(c(li, ls))
  }

## Teste
dados <- rnorm(100, 10, 3)
IC_boot_perc(amostra = dados, B = 200, estimador = mean, conf = 0.95)




##################### Exercicio 2 ##################### 

# Função para IC teórico usando t-Student
IC_teorico <- function(amostra, conf = 0.95) {
  n <- length(amostra)
  media <- mean(amostra)
  erro_padrao <- sd(amostra) / sqrt(n)
  margem_erro <- qt((1 + conf) / 2, df = n - 1) * erro_padrao
  c(media - margem_erro, media + margem_erro)
}

# Função para IC bootstrap normal
IC_boot_normal <- function(amostra, B = 200, estimador = mean, conf = 0.95) {
  est_pontual <- estimador(amostra)
  reamostras <- replicate(B, sample(amostra, length(amostra), replace = TRUE))
  est_reamostras <- apply(reamostras, 2, estimador)
  li <- est_pontual - qnorm((1 + conf) / 2) * sd(est_reamostras)
  ls <- est_pontual + qnorm((1 + conf) / 2) * sd(est_reamostras)
  c(li, ls)
}

# Função para IC bootstrap percentílico
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

# Função para calcular a cobertura dos ICs
calcula_cobertura <- function(n, dist_fun, dist_params, true_mean) {
  resultados <- matrix(0, nrow = length(n), ncol = 3)
  colnames(resultados) <- c("t-Student", "Bootstrap Normal", "Bootstrap Percentílico")
  
  for (l in 1:length(n)) {
    y_t <- numeric(m)
    y_boot_normal <- numeric(m)
    y_boot_perc <- numeric(m)
    
    for (j in 1:m) {
      amostra <- do.call(dist_fun, c(list(n[l]), dist_params))
      
      # IC teórico t-Student
      ic_t <- IC_teorico(amostra, conf)
      y_t[j] <- ifelse(true_mean >= ic_t[1] & true_mean <= ic_t[2], 1, 0)
      
      # IC bootstrap normal
      ic_boot_normal <- IC_boot_normal(amostra, B = 200, estimador = mean, conf = conf)
      y_boot_normal[j] <- ifelse(true_mean >= ic_boot_normal[1] & true_mean <= ic_boot_normal[2], 1, 0)
      
      # IC bootstrap percentílico
      ic_boot_perc <- IC_boot_perc(amostra, B = 200, estimador = mean, conf = conf)
      y_boot_perc[j] <- ifelse(true_mean >= ic_boot_perc[1] & true_mean <= ic_boot_perc[2], 1, 0)
    }
    
    resultados[l, 1] <- mean(y_t)
    resultados[l, 2] <- mean(y_boot_normal)
    resultados[l, 3] <- mean(y_boot_perc)
  }
  
  return(resultados)
}

# Comparação para distribuição Normal
true_mean_normal <- 0  # Média teórica da distribuição Normal(0, 1)
resultados_normal <- calcula_cobertura(n_sizes, rnorm, list(mean = 0, sd = 1), true_mean_normal)

# Comparação para distribuição Lognormal
true_mean_lognormal <- exp(0.5)  # Média teórica da distribuição Lognormal(0, 1)
resultados_lognormal <- calcula_cobertura(n_sizes, rlnorm, list(meanlog = 0, sdlog = 1), true_mean_lognormal)

# Imprimir os resultados
print("Resultados para distribuição Normal:")
print(resultados_normal)

print("Resultados para distribuição Lognormal:")
print(resultados_lognormal)

# Plotar os resultados
par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 1))

plot(n_sizes, resultados_normal[, 1], type = "b", pch = 16, col = "blue", ylim = c(0, 1), main = "Distribuição Normal", xlab = "Tamanho da Amostra", ylab = "Cobertura")
lines(n_sizes, resultados_normal[, 2], type = "b", pch = 16, col = "red")
lines(n_sizes, resultados_normal[, 3], type = "b", pch = 16, col = "green")
legend("bottomright", legend = c("t-Student", "Bootstrap Normal", "Bootstrap Percentílico"), col = c("blue", "red", "green"), lty = 1, pch = 16)

plot(n_sizes, resultados_lognormal[, 1], type = "b", pch = 16, col = "blue", ylim = c(0, 1), main = "Distribuição Lognormal", xlab = "Tamanho da Amostra", ylab = "Cobertura")
lines(n_sizes, resultados_lognormal[, 2], type = "b", pch = 16, col = "red")
lines(n_sizes, resultados_lognormal[, 3], type = "b", pch = 16, col = "green")
legend("bottomright", legend = c("t-Student", "Bootstrap Normal", "Bootstrap Percentílico"), col = c("blue", "red", "green"), lty = 1, pch = 16)
