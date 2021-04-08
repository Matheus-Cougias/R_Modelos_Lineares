## Codigos da aula de Teoria dos Modelos Lineares
## Marcelo Azevedo Costa
## 16/12/2020

## Dados do exemplo
 dados <- read.csv2("salario_experiencia.csv")
 dados <- dados[order(dados$Experiencia),]
 
 plot(Salario ~ Experiencia, data=dados, pch=19, col="blue")
 modelo <- lm(Salario ~ Experiencia, data=dados) 
 abline(modelo, lty=2, col="red", lwd=2)
 summary(modelo)
 anova(modelo) 
 
 saida <- predict(modelo, interval = "confidence", level=0.95)
 lines(saida[,"lwr"] ~ dados$Experiencia, lwd=2, col="red")
 lines(saida[,"upr"] ~ dados$Experiencia, lwd=2, col="red")
 
 
 saida <- predict(modelo, interval = "prediction", level=0.95)
 lines(saida[,"lwr"] ~ dados$Experiencia, lwd=2, col="black")
 lines(saida[,"upr"] ~ dados$Experiencia, lwd=2, col="black") 
 
 
 y     <- dados$Salario 
 x     <- dados$Experiencia
 n     <- length(y)   # tamanho amostral
 
 cc  <- (x-mean(x))/sum( (x-mean(x))^2 )
 b1  <- sum(y * cc )
 b0  <- mean(y) - b1*mean(x)
 mu  <- b0 + b1*x
 s2  <- sum( (y - mu)^2 )/(n-2)
 Var.b1 <- s2 / sum( (x-mean(x))^2 )
 sqrt(Var.b1)
 
 ## Por que preciso do desvio padrao de um estimador?
 ## (a) Calcular o intervalo de confiança
 b1 + qt(c(0.025, 0.975), df = n-2) * sqrt(Var.b1)
 confint(modelo)
 
 ## (b) para testar a hipotese nula, H0: Beta1 = 0
 tobs    = (b1 - 0)/sqrt(Var.b1)
 p-valor = 1 - pt(tobs, df=n-2)


 
 ## Resultados empiricos -  de simulacao
 ## Trabalhando com dados simulados
 desvio <- sqrt(0.04) # variancia = 0.04
 beta0  <- 1.8        # Mil R$
 beta1  <- 0.1        # Mil R$/ano experiencia
 
 vb1     <- c()
 vSigma2 <- c()
 vx0     <- c()
 x0      <- 15  # Nova estimacao
 
 for(s in 1:10000){
   y <- rnorm(n, mean = beta0 + beta1*x, sd = desvio)
   #plot(y ~ x, pch=19, col="blue")
   cc  <- (x-mean(x))/sum( (x-mean(x))^2 )
   b1  <- sum(y * cc )
   vb1 <- c(vb1, b1)
   b0  <- mean(y) - b1*mean(x)
   
   vx0 <- c(vx0, b0 + b1 * x0)
   
   mu  <- b0 + b1*x
   s2  <- sum( (y - mu)^2 )/(n-2)
   vSigma2 <- c(vSigma2, s2)
 }
 hist(vx0); summary(vx0); sd(vx0)
 
 hist(vSigma2)
 summary(vSigma2)
 
 hist(vb1)
 summary(vb1)
 sd(vb1)
 sqrt(desvio^2/sum( (x-mean(x))^2 ))
 
 #beta1 <- rho*Sy/Sx 
 #Sx    <- sd(x)
 #Sy    <- sd(y)
 #rho   <- cor(y,x)
 
 
 
 

