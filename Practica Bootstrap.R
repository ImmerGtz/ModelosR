

data <- read.csv(file.choose(), header = FALSE)
data

## Técnica Bootstrap
set.seed(12345)
reclam = rlnorm(20, meanlog = 2.5, sdlog = 2.5)
bootstrap_mse = function(data, n_bootstrap){
  mse_values = numeric(n_bootstrap)
  for(i in 1:n_bootstrap){
    sample_data = sample(data, replace = TRUE)
    mse_values[i] = mean((sample_data - mean(sample_data))^2)
  }
  return(mse_values)
}
# Estimación del MSE usando bootstrap
n_bootstrap = 100000
mse_val = bootstrap_mse(reclam, n_bootstrap)
mse_est<-mean(mse_val)
head(mse_val)
cat("El MSE es:", format(mse_estimate, big.mark = ","), "\n")

# Intervalo de confianza
IC <- quantile(mse_val, c(0.025,0.975))
IC
# Error Cuadratico
error <- sd(mse_val) / sqrt(n_bootstrap) 

cat("El error estándar del MSE es", error, "\n") 

# Gráfica de densidad 
plot(density(mse_val), main = "Densidad del MSE Bootstrap", xlab = "MSE", ylab = "Densidad", col = "blue", lwd = 2) 
# También puedes agregar una línea vertical al estimador puntual 
abline(v = mean(mse_boot), col = "red", lwd = 2, lty = 2) 
legend("topright", legend = "Estimador MSE", col = "red", lty = 2, lwd = 2) 


# Para los reclamos
reclam2 <- c(144, 134, 185, 141, 205, 126, 123, 152, 123, 215, 170, 165, 180, 175, 160, 185, 168, 172, 178, 169)
bootstrap_mse = function(data, n_bootstrap){
  mse_values = numeric(n_bootstrap)
  for(i in 1:n_bootstrap){
    sample_data = sample(data, replace = TRUE)
    mse_values[i] = mean((sample_data - mean(sample_data))^2)
  }
  return(mse_values)
}
# Estimación del MSE usando bootstrap
n_bootstrap = 100000
mse_val = bootstrap_mse(reclam2, n_bootstrap)
mse_est<-mean(mse_val)
head(mse_val)
cat("El MSE es:", format(mse_est, big.mark = ","), "\n")

# Intervalo de confianza
IC <- quantile(mse_val, c(0.025,0.975))
IC
# Error Cuadratico
error <- sd(mse_val) / sqrt(n_bootstrap) 

cat("El error estándar del MSE es", error, "\n") 

# Gráfica de densidad 
plot(density(mse_val), main = "Densidad del MSE Bootstrap", xlab = "MSE", ylab = "Densidad", col = "blue", lwd = 2) 
# También puedes agregar una línea vertical al estimador puntual 
abline(v = mean(mse_boot), col = "red", lwd = 2, lty = 2) 
legend("topright", legend = "Estimador MSE", col = "red", lty = 2, lwd = 2) 