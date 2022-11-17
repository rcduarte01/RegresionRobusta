# Ejemplo 1 ratas 
library(RobStatTM)
library(robust)

data(shock)
modelo1 <- lm(time~n.shocks, data=shock)
summary(modelo1)
modelo2 <- lm(time~n.shocks, data=shock[-c(1,2,4),])
plot(shock$n.shocks,shock$time, pch=19,
     xlab = "Número de descargas",
     ylab="Tiempo")

abline(modelo1, col="red", lwd=2)
abline(modelo2, col="blue", lwd=2)

################################################

# Ejemplo 2 detección de valores atipicos
data(wood, package='robustbase')
woodLS <- lm(y ~ ., data=wood)
summary(woodLS)
woodLS2 <- lm(y ~ ., data=wood[-c(4,6,8,19),])
summary(woodLS2)

par(mfrow=c(2,2))
plot(woodLS)
par(mfrow=c(1,1))

# Ejemplo: Datos simulados 
set.seed(2022)
x <- runif(20, 2,10)
y <- 5 + 2*x + rnorm(20, sd=2.5)

y[21] <- 40
x[21] <- 6.5

plot(x,y, pch=19)

ajuste1 <- lm(y~x) # ajuste minimos cuadrados todos los datos
ajuste2 <- lm(y[1:20]~x[1:20]) # ajuste sin dato atipico 
ajuste3 <- rlm(y ~ x) # ajuste robusto con perdida de Huber
abline(ajuste1, col="red", lwd=2)
abline(ajuste2, col="blue", lwd=2)
abline(ajuste3, col="violet", lwd=2)

legend("topleft",
       c("LS datos completos","LS sin valor atipico","Huber"),
       col=c("red","blue","violet"),
       lwd=c(2,2,2),
       bty="n")

# valor con alta palanca
set.seed(2022)
x <- runif(20, 2,10)
y <- 5 + 2*x + rnorm(20, sd=2.5)

y[21] <- 40
x[21] <- 20

plot(x,y, pch=19)

ajuste1 <- lm(y~x) # ajuste minimos cuadrados todos los datos
ajuste2 <- lm(y[1:20]~x[1:20]) # ajuste sin dato atipico 
ajuste3 <- rlm(y ~ x) # ajuste robusto con perdida de Huber
abline(ajuste1, col="red", lwd=2)
abline(ajuste2, col="blue", lwd=2)
abline(ajuste3, col="violet", lwd=2)

legend("topleft",
       c("LS datos completos","LS sin valor atipico","Huber"),
       col=c("red","blue","violet"),
       lwd=c(2,2,2),
       bty="n")

# MM estimador

cont <- lmrobdet.control(bb = 0.5,
                         efficiency = 0.85, 
                         family = "bisquare")

#woodMM <- lmrobdetMM(y ~ ., data=wood, control=cont)
ajuste4 <-lmrobdetMM(y ~ x, control = cont)
abline(ajuste4, col="gold", lwd=2)  



# El M estimador me puede dar los pesos para ajustar una regresión 
modelo1 <- lm(time~n.shocks, data=shock)
summary(modelo1)
modelo2 <- lm(time~n.shocks, data=shock[-c(1,2,4),])
plot(shock$n.shocks,shock$time, pch=19,
     xlab = "Número de descargas",
     ylab="Tiempo")

abline(modelo1, col="red", lwd=2)
abline(modelo2, col="blue", lwd=2)

# ajuste robusto
cont <- lmrobdet.control(bb = 0.5,
                         efficiency = 0.80, 
                         family = "bisquare")

modeloRob <-lmrobdetMM(time ~ n.shocks,
                       control = cont,
                       data=shock)
modeloRob$rweights

modelo3 <- lm(time~n.shocks,
              data=shock,
              weights = modeloRob$rweights)

abline(modelo3, col="green", lwd=2)

# Regresión logistica
set.seed(2022)
beta0 <- -2
beta1 <-  3

x <- rnorm(100,1,1)
prob <- exp(beta0 + beta1*x)/(1 + exp(beta0 + beta1*x))
runis <- runif(100,0,1)
y <- ifelse(runis < prob,1,0)

ajuste <- glm(y ~ x, family = "binomial")
summary(ajuste)


beta0_hat <- coef(ajuste)[1]
beta1_hat <- coef(ajuste)[2]
plot(x,y, pch=19,
     xlim = c(-2,7))

curve(exp(beta0_hat+beta1_hat*x)/(1+exp(beta0_hat+beta1_hat*x)),
      add=T, col="blue", lwd=2)



# contaminación
x[101] <- 6 #c(6,7,8,9,10)
y[101] <- 0 #rep(0,5)
ajuste <- glm(y ~ x, family = "binomial")
summary(ajuste)
points(x[101],y[101], pch=19, col="red")

beta0_hat <- coef(ajuste)[1]
beta1_hat <- coef(ajuste)[2]

curve(exp(beta0_hat+beta1_hat*x)/(1+exp(beta0_hat+beta1_hat*x)),
      add=T, lwd=2, col="red")

## Efecto de valores atipicos en los betas 
interceptos <- c()
pendientes <- c()

for(i in 1:10){
  y[100 + i] <- 0
  x[100 + i] <- 5 + i
  
  ajuste <- glm(y ~ x, family = "binomial")
  interceptos[i] <- c(coef(ajuste)[1])
  pendientes[i]  <- c(coef(ajuste)[2])
}

par(mfrow=c(2,1))
plot(6:15,interceptos, type = "b", pch=19,
     xlab = "Outlier", main = "Interceptos")

plot(6:15,pendientes,type = "b", pch=19,
     xlab = "Outlier", main = "Pendiente")
par(mfrow=c(1,1))

########

set.seed(2022)
beta0 <- -2
beta1 <-  3

x <- rnorm(100,1,1)
prob <- exp(beta0 + beta1*x)/(1 + exp(beta0 + beta1*x))
runis <- runif(100,0,1)
y <- ifelse(runis < prob,1,0)

## Efecto de valores atipicos en los betas 
interceptos <- c()
pendientes <- c()

interceptosBY <- c()
pendientesBY <- c()

for(i in 1:10){
  y[100 + i] <- 0
  x[100 + i] <- 5 + i
  
  ajuste <- glm(y ~ x, family = "binomial")
  ajusteBY <- logregWBY(y,x0=as.matrix(x), intercept=1)
  
  interceptos[i] <- c(coef(ajuste)[1])
  pendientes[i]  <- c(coef(ajuste)[2])
  
  interceptosBY[i] <- ajusteBY$coefficients[1]
  pendientesBY[i]  <- ajusteBY$coefficients[2]
  
}

par(mfrow=c(2,1))
plot(6:15,interceptos, type = "b", pch=19,
     ylim=range(interceptos, interceptosBY),
     xlab = "Outlier", main = "Interceptos", col="blue")

lines(6:15,interceptosBY, type = "b", pch=19,
      xlab = "Outlier", main = "Interceptos", col="red")

plot(6:15,pendientes,type = "b", pch=19, col="blue",
     ylim=range(pendientes, pendientesBY),
     xlab = "Outlier", main = "Pendiente")

lines(6:15,pendientesBY, type = "b", pch=19,
      xlab = "Outlier", main = "Interceptos", col="red")

par(mfrow=c(1,1))


## Ejemplo leucemia
data(leuk.dat, package='robust')
plot(leuk.dat$wbc, leuk.dat$y)

modeloMV   <- glm(y~., family = "binomial", data=leuk.dat)
modeloMV32 <- glm(y~., family = "binomial", data=leuk.dat[-c(15),])

cbind(coef(modeloMV),
      coef(modeloMV32))

modeloBY <- logregWBY(leuk.dat$y,
                      x0 = as.matrix(cbind(leuk.dat$wbc,leuk.dat$ag)),
                      intercept = 1)

cbind(coef(modeloMV),
      coef(modeloMV32),
      modeloBY$coefficients)

# predicciones
prediccionesMV <- predict(modeloMV, newdata = leuk.dat, type = "response")
prediccionesMV32 <- predict(modeloMV32, newdata = leuk.dat, type = "response")

prediccionesBY <- 1/(exp(-(modeloBY$coefficients[1]+modeloBY$coefficients[2]*leuk.dat$wbc+
                             modeloBY$coefficients[3]*leuk.dat$ag))+1)

leuk.dat <- data.frame(leuk.dat,round(cbind(prediccionesMV,prediccionesMV32,prediccionesBY),4))