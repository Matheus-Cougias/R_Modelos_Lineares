## Analise de Correlacao
## Dados do exemplo

 dados <- read.csv2("salario_experiencia.csv")
 dados <- dados[order(dados$Experiencia),]

 plot(Salario ~ Experiencia, data=dados, pch=19, col="blue")
 modelo <- lm(Salario ~ Experiencia, data=dados) 
 abline(modelo, lty=2, col="red", lwd=2)
 summary(modelo)

 y <- dados$Salario
 x <- dados$Experiencia

 rho   <- cor(x,y)
 Sx    <- sd(x)
 Sy    <- sd(y)
 beta1 <- rho*Sy/Sx 