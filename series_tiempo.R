
# Instalar si es necesario:
# install.packages(c("forecast", "tseries", "readxl", "fpp2"))

library(forecast)
library(tseries)
library(readxl)
library(fpp2) # incluye forecast, ggplot2, tseries, datasets, etc.

#  Crear una Serie Temporal Simulada
# Generamos 100 observaciones aleatorias normales con media 0 y desviación estándar 1
# Para asegurar reproducibilidad
set.seed(123)
data <- ts(rnorm(100), frequency = 12)
# Visualizamos la serie simulada

plot(data, main = "Serie Temporal Aleatoria Simulada", ylab = "Valor", xlab = "Tiempo")
#  Test de Dickey-Fuller para Estacionariedad
# ------------------------------------------

# Verifica si la serie tiene una raíz unitaria (es decir, si es NO estacionaria)
adf_test <- adf.test(data)
print(adf_test)
# Interpreta el resultado:
# - p < 0.05: rechaza H0 → la serie es estacionaria.
# - p > 0.05: no se rechaza H0 → la serie podría ser no estacionaria.

# ------------------------------------------
#  Ajuste de Modelo AR(1)
# ------------------------------------------

# Ajustamos un modelo ARIMA(p=1, d=0, q=0) → modelo AR(1)
modelo_ar <- arima(data, order = c(1, 0, 0))
summary(modelo_ar)
# ------------------------------------------
#  Interpretación de Coeficientes AR
# ------------------------------------------


# Extraemos los coeficientes estimados
# p-valores
# Extraemos los coeficientes estimados
coef_ar <- modelo_ar$coef
# Calculamos errores estándar a partir de la matriz de varianza-covarianza
se_ar <- sqrt(diag(modelo_ar$var.coef))


# Calculamos valores z (coeficiente / error estándar)
z_ar <- coef_ar / se_ar
# Calculamos p-valores asociados
p_ar <- 2 * (1 - pnorm(abs(z_ar)))

# Consolidamos resultados en tabla

resultados_ar <- data.frame(Coefficients = coef_ar,
                            Std_Error = se_ar,
                            Z_value = z_ar,
                            P_value = p_ar)
print(resultados_ar)
# Si el p-valor del coeficiente AR < 0.05, el efecto del rezago es estadísticamente significativo.
# ------------------------------------------
#  Predicción con Modelo AR
# ------------------------------------------


# Pronosticamos los próximos 10 periodos

pred_ar <- predict(modelo_ar, n.ahead = 10)

# Visualizamos la serie original y las predicciones
ts.plot(data, pred_ar$pred, col = c("black", "green"), lty = 1:2,
        main = "AR(1) - Predicción", ylab = "Valores")

#  Ajuste de Modelo ARMA(1,1)
# ------------------------------------------

# ARIMA(p=1, d=0, q=1): incorpora rezago de valores pasados (AR) y errores pasados (MA)
modelo_arma <- arima(da# ------------------------------------------ta, order = c(1, 0, 1))
summary(modelo_arma)
# ------------------------------------------
#  Predicción con Modelo ARMA
# ------------------------------------------

# Pronóstico a 10 pasos adelante
pred_arma <- predict(modelo_arma, n.ahead = 10)
# Visualización opcional
ts.plot(data, pred_arma$pred, col = c("black", "darkgreen"), lty = 1:2,
        main = "ARMA(1,1) - Predicción", ylab = "Valores")

###############################################################
# desde mar# CASO PRACTICO: Analizaremos la serie macroeconomica mensual dolar observado (pesos por dolar)
#marzo de 1990 a junio de 2025
#----------------------------------------------
# Cargar archivo Excel previamente importado como 'dolar_obs'
# dolar_obs <- read_excel("ruta/dolar.xlsx")  # Reemplaza con tu ruta
#--------------------------------------------

# renombramos las columnas

colnames(dolar_obs) <- c("Fecha", "Valor")
# cremos la serie temporal, desde donde inicia e indicando su frecuencia mensual

tipo_cambio <- ts(dolar_obs$Valor, start = c(1990, 3), frequency = 12)
#Graficamos la serie original

plot(tipo_cambio, main = "Tipo de cambio nominal (CLP/USD)",
     ylab = "Pesos por dólar", xlab = "Año")
#---------------------------------------------   
acf(tipo_cambio) # analizamos su ACF y aplicamos el Test-Dickey-Fuller

adf_test_tc <- adf.test(tipo_cambio)
print(adf_test_tc)
#-----------------------------------------------
# cuando una serie es de orden I(1) para volver una serie estacionaria de I(0)
# es necesario calcular su primera diferencia y volver a testear 
# la funcion ndiffs nos indica las diferecencia que hay que calcular para que
# la serie se vuelva estacionaria
ndiffs(tipo_cambio)
#---------------------------------------------
# calculamos la primera diferencia 
diff_tc <- diff(tipo_cambio)
# Graficamos 
plot(diff_tc)
acf(diff_tc)

# probamos estacoionaridad con Dickey-Fuller
adf_test_diff <- adf.test(diff_tc)
print(adf_test_diff)
#--------------------------------------------------
#análisis visual de los graficos de autocorrelacion

par(mfrow = c(2, 2))
plot(tipo_cambio, ylab = "CLP/USD")
acf(tipo_cambio, main = "No Estacionaria")
plot(diff_tc, ylab = "Cambio 1er orden")
acf(diff_tc, main = "Estacionaria")
par(mfrow = c(1, 1))

#--------------------------------------------------------------------
# Ejemplo con la base uschange, contiene los cambios porcentuales trimestrales de variables
# macroeconomicas de EE.UU
# variables: Consumo, ingreso, produccion insdustrial, etc.
# para ello instalamos el paquete fpp2 para trabajar con modelos ARIMA y 
# analisis de series temporales multivariadas
#---------------------------------------------------

# cargamos la data y observamos su estructura
data("uschange")
str(uschange)

# Generamos un grafico con la variable de interés
autoplot(uschange[, "Consumption"]) +
  ggtitle("Cambio % trimestral en consumo (EE.UU.)") +
  ylab("% cambio") + xlab("Año")
# -----------------------------------------------
# como uschange esta expresada en diferencias porcentuales, por lo tanto
# suele ser estacionaria, comprebemos estacioonaridad (ACF + ADF)  

par(mfrow = c(1, 2))
plot(uschange[, "Consumption"], main = "Serie de consumo")
acf(uschange[, "Consumption"])
par(mfrow = c(1, 1))

# usamos Dickey-Fuller para confirmar estacionaridad
adf.test(uschange[, "Consumption"])

#-----------------------------------------------------

# ya que confirmamos estacionaridad, podemos ajustar un modelo ARIMA (p, d, q)

modelo_consumo <- auto.arima(uschange[, "Consumption"])
summary(modelo_consumo)
#------------------------------------------------------
# Validamos supuestos del modelo
# buscamos que:
# ACF de los residuos no tengan autocorrelacion
# Histograma se aproxime a normal
# p-value del test Ljung-Box > 0,05 (residuos  ≈ ruido blanco)

checkresiduals(modelo_consumo)
#-----------------------------------------

# Generar pronóstico para próximos 8 trimestres (2 años)
forecast_consumo <- forecast(modelo_consumo, h = 8)

# Visualizamos los resultados

autoplot(forecast_consumo) +
  ggtitle("Pronóstico de consumo (EE.UU.)") +
  ylab("Cambio % trimestral")
