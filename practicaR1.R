# - AYUDANTÍA 1 ECONOMETRIA II
# --------------------------------------
#   IDENTIFICACION DE ECUACIONES

#   una ecuacion del sistema está:

# - NO IDENTIFICADO: No se puede estimar su forma estructural.
# - DEBILMENTE IDENTIFICADAS: Si los instrumentos son casi colineales.
# - BIEN IDENTIFICADAS: Si cumple las condiciones y rango.

# CASO NO IDENTIFICADAS

set.seed(123)

n <- 100

precio <- runif(n, 10, 20)


# AMBAS COMPARTEN EL PRECIO COMO RE0GRESOR

Qd <- 100 -2 * precio + rnorm(n, sd=2)

Qs <- -20 + 3 * precio + rnorm(n, sd=2)

datos1 <- data.frame(Qd, Qs, precio)

                         
# estimar la demanda por OLS

modelo_demanda <- lm(Qd ~ precio, data = datos1)
summary(modelo_demanda)


# CASO DEBÍLMENTE IDENTIFICADO

# INCORPORAR UN z1 debilmente correlacionado con el precio

# ayuda a identificar la demanda
# z1 es un instrumento para precio en la ecuacion de demanda

install.packages("AER")

library(AER)


z1 <- rnorm(n, mean = 5,sd = 0.1) # casi constante

precio <- 15 + 0.005 * z1 + rnorm(n, sd = 2)

Qd <- 100 -2 * precio + rnorm(n, sd = 2)


datos2 <- data.frame(Qd, precio, z1)

# validar el instrumento en la primera etapa

lm(precio ~ z1, data = datos2)

summary(lm(precio ~ z1, data = datos2))

# ESTIMACION 2SLS
iv_weak <- ivreg(Qd ~ precio | z1, data = datos2 )

summary(iv_weak)



# CASO BIEN IDENTIFICADO
# CUMPLE CON ORDEN Y RANGO
# INCORPORAMOS INSTRUMENTOS FUERTES

# INGRESO PARA DEMANDA
# COSTO PARA OFERTA

# SE LOGRA IDENTIFICACION POR EXCLUSIÓN

# INSTRUMENTOS EXCLUIDOS MAYORES O IGUALES VARIABLES ENDOGENAS

set.seed(123)

n <- 100

Y <- rnorm(n,100, 20) # ingreso (instrumento de demanda)

W <- rnorm(n, 50, 10) # costo de producción (instrumento para oferta)

P <- 10 + 0.5 * Y - 0.3 * W + rnorm(n) #precio determinado por Y y W (endogeno)

# Demanda
Qd <- 20 - 1.5 * P + 0.5 * Y + rnorm(n) 

# Oferta

Qs <- 15 + 1.2 * P + 0.3 * W + rnorm(n)

datos3 <- data.frame(Qd, Qs, P, Y, W)

# Estimación de demanda por 2SLS, precio endógeno
# Usamos ingreso como regresor directo, solo AFECTA DEMANDA

# Costo de produccion como instrumento valido para precio
# solo afecta oferta

iv_bien <- ivreg(Qd ~ P + Y | Y + W, data = datos3)

summary(iv_bien) 


# ESTIMACION EN 3SLS

# NECESARIO
install.packages("systemfit")

library(systemfit)

eq1 <- Qd ~ P + Y #demanda
eq2 <- Qs ~ P + W #oferta

system_eq <- list(demand = eq1, supply = eq2)

modelo_3SLS <- systemfit(system_eq, data = datos3, method = "3SLS", inst = ~ Y + W)

summary(modelo_3SLS)

# Es fundamental elegir buenos instrumentos que cumplan

# RELEVANCIA v. instrumental predice v. endógena

# EXCLUSION v. instrumental no debe estar correlacionado con error de la ecuacion

# REGLA PRACTICA : F-static > 10 sugiere instrumento NO es débil
# en primera etapa 2SLS




##############################################################################

# MODELO SUR

# - Valido para cuando los **errores de las ecuaciones de regresion estan
# - Correlacionados***, mejora la precision de la estimacion
# NO REQUIERE IDENTIFICACIÓN, NO HAY ENDOGENIDAD
install.packages("MASS")
library(MASS)

# tamaño muestra 
n <- 100
# variables explicativas
x1 <- rnorm(n)
x2 <- rnorm(n)
z1 <- rnorm(n)
z2 <- rnorm(n)

# Matriz de varianzas y covariancias de los errores

Sigma <- matrix(c(1, 0.6, 0.6, 1), nrow = 2) # correlación entre ecuaciones

# simular errores correlacionados entre ecuaciones

errors <- mvrnorm(n, mu = c(0,0), Sigma = Sigma)

# Ecuación 1: y1 depende x1 y x2 + error

y1 <- 1 + 2 * x1 + 3 * x2 + errors[, 1]

# Analagomente para la ecuacion 2
y2 <- 1 + 1 * z1 - 2 * z2 + errors[, 2]

# construir el dataframe

data <- data.frame(y1, y2, x1, x2, z1, z2)

# AJUSTE OLS
lm1 <- lm(y1 ~ x1 + x2, data = data)
lm2 <- lm(y2 ~ z1 + z2, data = data)

# obtener residuos

e1 <- residuals(lm1)
e2 <- residuals(lm2)


# estimar sigma (matriz de cov de errores)

sigma_hat <- crossprod(cbind(e1, e2)) / n


# Construir matriz Ω (Omega = Sigma ⊗ I_n)

omega <- kronecker(diag(n), sigma_hat)


# matrices X
X1 <- model.matrix(lm1)
X2 <- model.matrix(lm2)


# Vector Y 
Y <- c(data$y1, data$y2)


# Matriz X bloque para SUR: 2n x 6
X <- rbind(cbind(X1, matrix(0, n, ncol(X2))),  # ecuación 1
  cbind(matrix(0, n, ncol(X1)), X2)) # ecuacion 2

# Estimador GLS paso a paso

# Estimador GLS paso a paso
beta_GLS <- solve(t(X) %*% solve(omega) %*% X) %*%
  (t(X) %*% solve(omega) %*% Y)

# Mostrar resultados
print(beta_GLS)






install.packages("systemfit")  # Solo si no está instalado
library(systemfit)

# --------------------------------------------

# ejercicio MODELO SUR

#  Simular datos con errores correlacionados

set.seed(123)

# Tamaño de la muestra
n <- 100

# Variables explicativas (exógenas)
ingreso <- rnorm(n, mean = 50000, sd = 10000)
educacion <- rnorm(n, mean = 16, sd = 2)

# Definir matriz de covarianza entre errores
Sigma <- matrix(c(1, 0.6, 0.6, 1), nrow = 2)  # Correlación entre ecuaciones

# Simular errores correlacionados
library(MASS)
errores <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)


#  Generar variables dependientes

# Ecuación 1: Gasto en alimentos
gasto_alimentos <- 500 + 0.2 * ingreso + 50 * educacion + errores[, 1]

# Ecuación 2: Gasto en transporte
gasto_transporte <- 300 + 0.15 * ingreso + 30 * educacion + errores[, 2]

# Armar dataframe
datos <- data.frame(gasto_alimentos,
  gasto_transporte,
  ingreso,
  educacion)


# 4. Definir sistema de ecuaciones

eq1 <- gasto_alimentos ~ ingreso + educacion
eq2 <- gasto_transporte ~ ingreso + educacion

# Armar lista del sistema
sistema <- list(alimentos = eq1, transporte = eq2)


# 5. Estimar el modelo SUR

ajuste_sur <- systemfit(sistema, method = "SUR", data = datos)

summary(ajuste_sur)

 