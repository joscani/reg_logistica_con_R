

library(foreign)
library(car)
library(MASS)
library(effects)
datos <- read.spss(file="t11And_18_mas.sav",use.value.labels=TRUE, to.data.frame=TRUE)
uso_int <- datos$USO_INT
uso_int <- ifelse(uso_int==1,1,0)
sexo <- datos$SEXO
edad <- datos$EDAD
nivelest <- datos$NIVELEST
habitat <- datos$HABITAT
tamhogar <- datos$TAMHOG
factor.elev <- datos$FACTOR_P
factor.hog <- datos$FACTOR_H
datos.bin <- data.frame(uso_int,sexo,edad)
habitat <- datos$HABITAT
habitat <- factor(habitat) # la pasamos a factor
levels(habitat) <- paste("estrato",0:6,sep="") # cambiamos nombre a los niveles
datos.bin$habitat <- habitat # añadimos habitat a datos.bin
datos.bin$habitat <- relevel(datos.bin$habitat, ref = "estrato6")
datos.bin$nivelest <- recode(nivelest, "c(1,9) = 'Analfabetos'; 
2:3 = 'Primaria';
4:6 = 'Secundaria y F.P';
7:8= 'Universitaria o superior'")
datos.bin$nivelest<- as.factor(datos.bin$nivelest)
#barplot(table(datos.bin$nivelest), cex.names=0.7, cex.axis=0.7)
rm(habitat,sexo,nivelest,edad,uso_int,tamhogar,factor.elev,factor.hog) # borramos objetos



modelo.edadsex <- glm(uso_int ~ edad + sexo, data=datos.bin,family=binomial)
summary(modelo.edadsex)



summary(modelo.edadsex)$coefficients



summary(modelo.edadsex)$coefficients[,3]



modelo.sex <- glm(uso_int ~ sexo, data = datos.bin, family = binomial)
G2 <- modelo.sex$deviance - modelo.edadsex$deviance
G2



1-pchisq(G2,df=1)



#anova normal
anova(modelo.sex,modelo.edadsex,test="Chisq")
#Anova del paquete car
library(car)
Anova(modelo.edadsex)



modelo.cte <- glm(uso_int ~ 1, data = datos.bin, family = binomial)
anova(modelo.cte,modelo.edadsex, test="Chisq")



anova(modelo.sex,modelo.edadsex,test="Rao")



# intervalos al 95%
confint.default(modelo.edadsex)
#intervalos al 90%
confint.default(modelo.edadsex,level=0.90)



exp(confint.default(modelo.edadsex))



## library(MASS)



# I.C para los parametros
confint(modelo.edadsex)



# I.C para los exp(theta)
exp(confint(modelo.edadsex))



confint(modelo.edadsex,level=0.90)



exp(confint(modelo.edadsex,level=0.90))



# Primero guardamos los coeficientes del modelo ajustado con glm
betahat <- coef(modelo.edadsex) 
# Ahora le decimos a bootCase que tome 1000 muestras con reemplazamiento y calcule los coeficientes. 
# Los guardamos en el objeto betahat.boot
betahat.boot <- bootCase(modelo.edadsex,B=1000)



head(betahat.boot)



summary(betahat.boot)



par(mfrow=c(2,2))
for(i in 1:3)
 hist(betahat.boot[,i],main=names(coef(modelo.edadsex))[i],xlab="")



# calculamos la desviación típica y los I.C uniéndolos en un solo objeto
boot.interval <- cbind("Bootstrap SD"=apply(betahat.boot,2,sd),
      t(apply(betahat.boot, 2, function(x) quantile(x, c(.025, .975)))))
boot.interval


library(boot)
# Creamos una función que calcule los coeficientes de unos datos
# es importante que uno de los argumentos sean los indices
coeficientes <- function(formula,data,indices){
  d <- data[indices,]
  fit <- glm(formula,data=d, family=binomial)
  return(coef(fit))
}
# La función boot remuestrea los datos y calcula los coeficientes.
# establecemos una semilla para que siempre nos salga el mismo resultado
set.seed(234)
# puede tardar un rato
res.boot <- boot(data=datos.bin,statistic=coeficientes,R=1000,formula=uso_int ~ sexo + edad) 



head(res.boot$t)



plot(res.boot,index=3)



# Intercept
boot.ci(res.boot,index=1,type=c("norm","basic","perc"))
# sexoMujer
boot.ci(res.boot,index=2,type=c("norm","basic","perc"))
# edad
boot.ci(res.boot,index=3,type=c("norm","basic","perc"))



head(modelo.edadsex$fitted.values)
head(fitted.values(modelo.edadsex))



args(predict.glm) 



head(predict(modelo.edadsex))



head(predict(modelo.edadsex,type="response"))



prob.ajustadas <- predict(modelo.edadsex, type="response", se.fit=TRUE)
# valores ajustados
head(prob.ajustadas[[1]])
# errores
head(prob.ajustadas[[2]])



# calculamos los intervalos
valores.inf  <- prob.ajustadas[[1]] - 2 * prob.ajustadas[[2]]
valores.sup <- prob.ajustadas[[1]] + 2 * prob.ajustadas[[2]]
# vemos los valores inferior y superior para los 6 primeros encuestados
head(valores.inf)
head(valores.sup)



# Creamos un nuevo data.frame con el mismo nombre para las variables que el data.frame original
nuevos.datos <- data.frame(edad=45:50, sexo=c(rep("Hombre",6),rep("Mujer",6)))
nuevos.datos
prob.estim <- predict(modelo.edadsex,newdata=nuevos.datos, type="response")
cbind(nuevos.datos, prob.estim)



res.p <- residuals(modelo.edadsex,type="pearson")
# Residuos de pearson de los 6 primeros individuos de la encuesta
head(res.p)



res.p.std <- rstandard(modelo.edadsex,type="pearson")
head(res.p.std)



res.d <- residuals(modelo.edadsex,type="deviance")
head(res.d)



res.dev.std <- rstandard(modelo.edadsex,type="deviance")
head(res.dev.std)



res.student <- rstudent(modelo.edadsex)
head(res.student)



load("sexoedad.RData")
# los datos agrupados están en el data.frame res3
# primeras 6 filas del data.frame
head(res3)
# número de filas, que serán los diferentes perfiles
nrow(res3) 



tail(res3,10)



modelo.agrup <- glm(proporcion ~ edad + sexo, data=res3, weights=Freq, family=binomial)
summary(modelo.agrup)



(residuos.deviance <- sum(residuals(modelo.agrup,type="deviance")^2))



modelo.agrup$df.residual



1-pchisq(residuos.deviance, 151)



(residuos.pearson <- sum(residuals(modelo.agrup,type="pearson")^2))
# p-valor
1-pchisq(residuos.pearson, 151)



yhat <- fitted.values(modelo.edadsex)
yhat.corte <- cut(yhat,breaks=10,include.lowest=TRUE)
# los intervalos tienen igual amplitud pero el número de individuos en cada uno varía
table(yhat.corte)



yhat.corte2 = cut(yhat,breaks = quantile(yhat, probs=seq(0,1, 1/10)), include.lowest=TRUE)
table(yhat.corte2)



y <- datos.bin$uso_int
  (obs <- xtabs(cbind(1 -y, y) ~ yhat.corte2))



  (expect <- xtabs(cbind(1 - yhat, yhat) ~ yhat.corte2))



(chisq <- sum((obs - expect)^2/expect))
( P <- 1 - pchisq(chisq, 8))



hosmerlem <- function(y, yhat, g=10) {
  cutyhat1 = cut(yhat,breaks = quantile(yhat, probs=seq(0,1, 1/g)), include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat1)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat1)
  chisq.C = sum((obs - expect)^2/expect)
  P.C = 1 - pchisq(chisq.C, g - 2)
  cutyhat2 = cut(yhat,breaks =g, include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat2)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat2)
  chisq.H = sum((obs - expect)^2/expect)
  P.H = 1 - pchisq(chisq.H, g - 2)
  res <- data.frame(c(chisq.C,P.C),c(chisq.H,P.H))
  colnames(res)<- c("Hosmer-Lemeshow C statistic","Hosmer-Lemeshow H statistic")
  rownames(res)<- c("X-squared","p.value")
  return(res)  
}



hosmerlem(datos.bin$uso_int,fitted.values(modelo.edadsex))



# var.explicativas, sexo y edad, modelo con 3485 datos
(RsqrMcFadden <- 1- modelo.edadsex$deviance/modelo.edadsex$null.deviance)



# var.explicativas, sexo y edad, modelo datos agrupados
(RsqrMcFadden.agrup <- 1- modelo.agrup$deviance/modelo.agrup$null.deviance)



LR <- modelo.edadsex$null.deviance - modelo.edadsex$deviance
N <- sum(weights(modelo.edadsex)) # 3485 en este caso
(RsqrCN <- 1- exp(-LR/N) )



LR <- modelo.agrup$null.deviance - modelo.agrup$deviance
N <- sum(weights(modelo.agrup))
(RsqrCN.agrup <- 1- exp(-LR/N) )



L0.adj <- exp(-modelo.edadsex$null.deviance/N)
(RsqrNal <- RsqrCN/(1-L0.adj) )



table(datos.bin$uso_int)



prediccion <- ifelse(fitted.values(modelo.edadsex)>=0.5, 1, 0)
table(prediccion)



table(datos.bin$uso_int, prediccion)



tabla.clasif <- table(datos.bin$uso_int, prediccion)
tcc <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)
tcc



library(ROCR)


pred <- prediction(fitted.values(modelo.edadsex),datos.bin$uso_int)



perf1 <- performance(pred, measure="acc")
# el punto de corte que maximiza "acc" es
(posicion.max <- sapply(perf1@y.values,which.max))
(punto.corte <- sapply(perf1@x.values,'[',posicion.max))



plot(perf1, col="darkred")
# Añadimos una línea horizontal al valor de 0.8
abline(h=0.8, lty=2)
# Añadimos recta con el punto de corte que maximiza la tasa de clasificaciones correctas
abline(v=punto.corte,lty=2)



# auc : Area under curve
AUC <- performance(pred,"auc")
AUC@y.name
AUC@y.values



# con performance se selecciona tpr (true positive rate) y fpr (false positive rate)
perf2 <- performance(pred, "tpr", "fpr")
plot(perf2, colorize=TRUE) # mostramos colores según el punto de corte
#Añadimos la recta y=x que sería la correspondiente al peor clasificador
abline(a=0, b=1) 
# añadimos el valor del área bajo la curva
text(0.4,0.6,paste(AUC@y.name,"\n",round(unlist(AUC@y.values),3) ),cex=0.7 )



pred.modelo.sex <- prediction(fitted.values(modelo.sex),datos.bin$uso_int)
perf.modelo.sex <- performance(pred.modelo.sex, "tpr", "fpr")
plot(perf2, col ="darkred")
#Añadimos la recta y=x que sería la correspondiente al peor clasificador
abline(a=0, b=1) 
# añadimos la curva ROC para el modelo.sex utilizando add=TRUE
plot(perf.modelo.sex, col="darkblue", lty=2, add=TRUE)
legend("bottomright", c("Reg.logist uso_int ~ sexo+edad", "Reg.logist uso_int ~ sexo"), col=c("darkred","darkblue"), lty=1:2, cex=0.7)



modelo.full <- glm(uso_int ~ edad * sexo * nivelest * habitat, data=datos.bin, family=binomial)
modelo.inicial <- glm(uso_int ~ 1, data=datos.bin, family=binomial)



modelo.stp <- stepAIC(modelo.inicial, scope=list(upper=modelo.full), direction="both")

summary(modelo.stp)
hosmerlem(y=datos.bin$uso_int, yhat=fitted.values(modelo.stp))



pred.modelo.stp <- prediction(fitted.values(modelo.stp), datos.bin$uso_int)
perf.modelo.stp <- performance(pred.modelo.stp, "tpr", "fpr")
plot(perf.modelo.stp, col="darkred") # modelo.stp (edad+habitat+nivelest+edad:nivelest)
plot(perf2, col="darkblue", lty=2, add=TRUE) # modelo.edadsex (edad+sexo)
plot(perf.modelo.sex, lty=3, add=TRUE) # modelo.sex (sexo)
abline(a=0,b=1)
legend("bottomright", c("R.L uso_int ~ habitat + edad*nivelest", "R.L uso_int ~ sexo + edad",
"R.L uso_int ~ sexo"), col=c("darkred","darkblue","black"), lty=1:3, cex=0.7)



AUC.modelo.stp <- performance(pred.modelo.stp, "auc")
AUC.modelo.stp@y.values



predicho.modelo.stp <- ifelse(fitted.values(modelo.stp)>=0.5,1,0)
(tabla.clas.modelo.stp <- table(datos.bin$uso_int,predicho.modelo.stp))



cbind(coef=coef(modelo.stp),confint(modelo.stp))





library(MuMIn)

# tiene que calcular más de 100 modelos, tarda un rato
allModels <- dredge(modelo.full)

class(allModels)

allModels[1:5,]

allModels[1:3,]



# round redondea los resultados
round(importance(allModels), 3)






plot(importance(allModels),axes=FALSE,pch=19,type="b",xlab="",ylab="Importancia variables")
axis(2,las=2)
axis(1,at=1:15, labels=names(importance(allModels)),las=2,cex.axis=0.7)


