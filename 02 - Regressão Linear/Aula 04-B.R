## Estudo de Simulacao
## Marcelo A Costa
## 06/01/2020

n <- 1000
x <- seq(0, 30, length=n)
y <- 1.8 + 0.1 * x + rnorm(n, sd=5)
dados <- data.frame(y=y, x=x)

plot(y ~ x, data=dados, pch=19, col="blue")
modelo <- lm(y ~ x, data=dados) 
abline(modelo, lty=2, col="red", lwd=2)
summary(modelo)

saida <- predict(modelo, interval = "confidence", level=0.95)
lines(saida[,"lwr"] ~ x, lwd=2, col="red")
lines(saida[,"upr"] ~ x, lwd=2, col="red")

saida <- predict(modelo, interval = "prediction", level=0.95)
lines(saida[,"lwr"] ~ x, lwd=2, col="black")
lines(saida[,"upr"] ~ x, lwd=2, col="black") 