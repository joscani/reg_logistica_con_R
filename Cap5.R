

library(foreign)
library(car)
library(MASS)
library(boot)
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
modelo.stp <- glm(formula = uso_int ~ edad + nivelest + habitat + edad:nivelest, family=binomial, data=datos.bin)



## modelo.stp <- glm(formula = uso_int ~ edad + nivelest + habitat + edad:nivelest, family=binomial, data=datos.bin)



res.p <- residuals(modelo.stp,type="pearson")
# Residuos de pearson de los 6 primeros individuos de la encuesta
head(res.p)



res.p.sig <- abs(res.p)>2
table(res.p.sig)



res.orde <- sort(abs(res.p[res.p.sig]),decreasing=TRUE)
# mostramos solo los más altos
head(res.orde) 
# con names(res.orde) obtenemos la fila a la que corresponde,
# y ordenamos el data.frame utilizando names(res.orde) como índice
head(datos.bin[names(res.orde),])



fitted.values(modelo.stp)[1826]



cbind(observado=datos.bin$uso_int,predicho=fitted.values(modelo.stp))[c(1826,700,172,2434,1646,1457),]



res.p.std <- rstandard(modelo.stp,type="pearson")



res.p.std.sig <- abs(res.p.std)>2
table(res.p.std.sig)
# utilizamos res.p.std.sig como indice para ver el valor de los significativos
head(res.p.std[res.p.std.sig])



res.orde <- sort(abs(res.p.std[res.p.std.sig]),decreasing=TRUE)
# mostramos solo los más altos
head(res.orde) 
head(datos.bin[names(res.orde),])



res.d <- residuals(modelo.stp,type="deviance")
#significativos
res.d.sig <- abs(res.d)>2
table(res.d.sig)



res.dev.std <- rstandard(modelo.stp,type="deviance")
#significativos
table(abs(res.dev.std)>2)



res.student <- rstudent(modelo.stp)
res.orde <- sort(res.student, decreasing=TRUE)
head(res.orde)
head(datos.bin[names(res.orde),])
table(abs(res.student)>2)



plot(res.dev.std,cex=0.6)
abline(h=c(-2,2),col="red")



signif <- which(abs(res.dev.std)>2)
plot(res.dev.std[signif],type="n")
text(1:length(signif),res.dev.std[signif],label=signif,cex=0.4)



hist(res.dev.std,breaks=40)
abline(v=quantile(res.dev.std,probs=c(0.05/2,1-0.05/2)),lty=2)



plot(fitted.values(modelo.stp),res.dev.std, xlab="Prob.predichas", ylab="Residuos")



plot(datos.bin$uso_int,fitted.values(modelo.stp),xlab="Valores observados",ylab="Valores predichos")



plot(jitter(datos.bin$uso_int),fitted.values(modelo.stp),cex=0.5,xlab="Valores observados",ylab="Valores predichos")



plot(datos.bin$edad,res.dev.std,cex=0.5)



with(datos.bin, xtabs(edad==52 ~ habitat+nivelest))



plot(datos.bin$habitat,res.dev.std,cex=0.5,cex.axis=0.7)



residualPlots(modelo.stp,type="deviance",cex=0.6)



distancias.cook <- cooks.distance(modelo.stp)
head(distancias.cook)
hat.valores <- hatvalues(modelo.stp)
head(hat.valores)



table(distancias.cook>1)



medidas.infl <- influence.measures(modelo.stp)
colnames(medidas.infl$infmat)
# mostramos las medidas para las primeras 6 observaciones
head(medidas.infl$infmat)



par(mfrow=c(2,2))
plot(modelo.stp,cex=0.6)



par(mfrow=c(1,2))
plot(modelo.stp,which=4)
plot(modelo.stp,which=6)



 # con id.n=3 indicamos que muestre los tres mayores valores en cada gráfico
influenceIndexPlot(modelo.stp,id.cex=0.7,id.n=3)



# Utilizamos la función vif del paquete car
vif(modelo.stp)



# usamos la función update para quitar el término de interacción en el modelo
vif(update(modelo.stp, ~.-edad:nivelest))



library(boot)
args(cv.glm)



cost <- function(r, pi) mean(abs(r-pi) > 0.5)



res.10.fold <- cv.glm(datos.bin, modelo.stp, cost,K=10)



res.10.fold$delta



1-res.10.fold$delta



# puede tardar en realizar el cálculo, 7-9 minutos
res.loocv <- cv.glm(datos.bin, modelo.stp, cost, K=nrow(datos.bin))
res.loocv$delta

library(DAAG)


args(CVbinary)



res.daag <- CVbinary(modelo.stp)



res.daag$acc.cv



# mostramos sólo los 6 primeros
head(res.daag$cvhat)


