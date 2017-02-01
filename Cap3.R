

library(foreign)
# mostrar directorio actual
getwd()
# si el archivo de datos está en el mismo directorio
datos <- read.spss(file="t11And_18_mas.sav",use.value.labels=TRUE, to.data.frame=TRUE)



uso_int <- datos$USO_INT
uso_int <- ifelse(uso_int==1,1,0)
sexo <- datos$SEXO
edad <- datos$EDAD
nivelest <- datos$NIVELEST
habitat <- datos$HABITAT
datos.bin <- data.frame(uso_int, sexo, edad, nivelest, habitat)
# borramos todos los objetos creados salvo el data.frame datos.bin, para evitar confusiones
rm(uso_int, sexo, edad, nivelest, habitat)



head(datos.bin)



sapply(datos.bin, class)



datos.bin$habitat <- factor(datos.bin$habitat,levels=0:6,labels=paste("estrato",0:6,sep=""))
datos.bin$nivelest <- factor(datos.bin$nivelest)



levels(datos.bin$habitat)
levels(datos.bin$nivelest)



barplot(table(datos.bin$habitat), cex.names=0.7, cex.axis=0.7)



library(car)
datos.bin$nivelest <- with( datos.bin, recode( nivelest, 
" c(1,9) = 'Analfabetos'; 
 2:3 = 'Primaria';
 4:6 = 'Secundaria y F.P';
 7:8= 'Universitaria o superior'
")
)
levels(datos.bin$nivelest)



barplot(table(datos.bin$nivelest), cex.names=0.6, cex.axis=0.7)



summary(datos.bin)



with(datos.bin, prop.table(table(sexo,uso_int),margin=1))



with(datos.bin, prop.table(table(cut(edad,5),uso_int),1))




with(datos.bin, prop.table(table(habitat,uso_int),1))



with(datos.bin, prop.table(table(nivelest,uso_int),1))



args(glm)



## glm(y ~ x,family=binomial,data=mis.datos)



# 3 primeras filas
head(datos.bin)



with(datos.bin,plot(jitter(edad),jitter(uso_int,0.2),xlab="Edad",ylab="Uso de internet",cex=0.4,cex.axis=0.6,cex.lab=0.6))



modelo.1 <- glm(uso_int ~ edad, data = datos.bin, family=binomial)



class(modelo.1)



names(modelo.1)



# coeficientes del modelo
modelo.1[1]
# devianza
modelo.1$deviance 
# primeros 6 valores predichos para p(x)
head(modelo.1$fitted.values)
# primeros 6 valores de los residuos del método de ajuste
head(modelo.1$residuals)
# primeros 6 valores de los residuos de pearson
head(residuals(modelo.1,type="pearson")) 



options(digits=7)



modelo.1



options(digits=5)



summary(modelo.1)



with(datos.bin,plot(jitter(edad),jitter(uso_int,0.2),xlab="Edad",ylab=quote(p(x)),main="Curva ajustada",cex=0.4,cex.axis=0.6,cex.lab=0.6,cex.main=0.7))
# Añadimos la función ajustada
curve(1/(1+exp(-modelo.1$coefficients[1]-modelo.1$coefficients[2]*x)),add=TRUE)





iagrupado<-read.csv("internet_agrupAN.csv")
class(iagrupado)
names(iagrupado)
nrow(iagrupado) # número de filas del data.frame
head(iagrupado) # primeras 6 filas del data.frame



modelo.2 <- glm(cbind(si.internet, no.internet) ~ edad, data=iagrupado, family=binomial)



summary(modelo.2)



iagrupado$prop<- with(iagrupado, si.internet/(si.internet+no.internet) )
head(iagrupado)



plot(iagrupado$edad, iagrupado$prop, cex=0.4, cex.axis=0.6, cex.lab=0.6, cex.main=0.7, 
xlab="Edad", ylab=quote(p(x)), main="Curva ajustada para los datos agrupados")
curve(1/(1+exp(-modelo.2$coefficients[1]-modelo.2$coefficients[2]*x)),add=TRUE)



contrasts(datos.bin$habitat)



# para que sea permanente el cambio, guardamos el resultado de relevel en la misma variable habitat
datos.bin$habitat <- relevel(datos.bin$habitat, ref="estrato6")



levels(datos.bin$habitat)



contrasts(datos.bin$habitat)



modelo.3 <- glm(uso_int ~ habitat, data=datos.bin, family=binomial)



# matriz del modelo para los 3 primeros individuos
head(model.matrix(modelo.3),3)



summary(modelo.3)



# función invlogit para pasar de logit a probabilidades
invlogit <- function (x)  {
    1/(1 + exp(-x)) 
}
# aplicamos la función invlogit al primer coeficiente del modelo.3
invlogit(coef(modelo.3)[1]) 



#estrato0
invlogit(coef(modelo.3)[1]+coef(modelo.3)[2])
#estrato1
invlogit(coef(modelo.3)[1]+coef(modelo.3)[3])
#estrato5
invlogit(coef(modelo.3)[1]+coef(modelo.3)[7])







# lectura del archivo csv dónde tenemos los datos agrupados
habt_agrup <- read.csv("habitat_agrupado.csv")
habt_agrup
levels(habt_agrup$habitat)
# ponemos como categoría de referencia el estrato6.
habt_agrup$habitat <- relevel(habt_agrup$habitat,ref="estrato6")
# ajuste del modelo
modelo.3.agrp <- glm(cbind(si.internet,no.internet) ~ habitat, data=habt_agrup, family=binomial)



summary(modelo.3.agrp)



class(datos.bin$nivelest)


contrasts(datos.bin$nivelest)


datos.bin$nivelest.num <- as.numeric(datos.bin$nivelest)
class(datos.bin$nivelest.num)
table(datos.bin$nivelest.num)



modelo.4 <- glm(uso_int ~ nivelest.num, data=datos.bin, family=binomial)
summary(modelo.4)



datos.bin$nivelest.ord <- ordered(datos.bin$nivelest)
class(datos.bin$nivelest.ord)
table(datos.bin$nivelest.ord)



contrasts(datos.bin$nivelest.ord)







modelo.4.ord <- glm(uso_int ~ nivelest.ord, data=datos.bin, family=binomial)
summary(modelo.4.ord)



datos.bin$nivelest.lin[datos.bin$nivelest == 'Analfabetos'] <- -0.67082 
datos.bin$nivelest.lin[datos.bin$nivelest == 'Primaria'] <- -0.22361 
datos.bin$nivelest.lin[datos.bin$nivelest == 'Secundaria y F.P'] <- 0.22361 
datos.bin$nivelest.lin[datos.bin$nivelest == 'Universitaria o superior'] <- 0.67082 

datos.bin$nivelest.cuad[datos.bin$nivelest == 'Analfabetos'] <- 0.5
datos.bin$nivelest.cuad[datos.bin$nivelest == 'Primaria'] <- -0.5
datos.bin$nivelest.cuad[datos.bin$nivelest == 'Secundaria y F.P'] <- -0.5
datos.bin$nivelest.cuad[datos.bin$nivelest == 'Universitaria o superior'] <- 0.5

# el tipo de  las variables es numérica
class(datos.bin$nivelest.lin)
class(datos.bin$nivelest.cuad)



modelo.4.cuad <- glm(uso_int ~ nivelest.lin + nivelest.cuad, data=datos.bin, family=binomial)
summary(modelo.4.cuad)



# vemos los niveles de ambas variables, el primer nivel será la categoría de referencia
levels(datos.bin$habitat)
levels(datos.bin$nivelest)

modelo.5 <- glm(uso_int ~ habitat + nivelest,  data=datos.bin, family=binomial)
summary(modelo.5)



invlogit(coef(modelo.5)[1])



invlogit(coef(modelo.5)[1]+coef(modelo.5)[8])



modelo.6 <- glm(uso_int ~ edad + habitat + nivelest, data=datos.bin, family=binomial)
summary(modelo.6)




coef.m <- coef(modelo.6)
par(mfrow=c(2,2))
cond <- datos.bin$habitat=="estrato6"
with(datos.bin[cond,],plot(jitter(edad),jitter(uso_int,0.2),xlab="Edad",ylab=quote(p(x)),main="Estrato 6",cex=0.4,cex.axis=0.6,cex.lab=0.6,cex.main=0.7))
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x) )),  col="darkgreen",lwd=2, add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[9]) )), lty=2,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[10]) )),lty=3,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[11]) )),lty=4,lwd=2,add=TRUE)
nombres <- levels(datos.bin$nivelest)
#legend("topright", nombres, col=c("darkgreen",rep("black",3)),cex=0.7,lty=1:4,lwd=rep(2,4))
legend(x=70,y=0.95, nombres, col=c("darkgreen",rep("black",3)),cex=0.7,lty=1:4,lwd=rep(2,4))


# estrato=="estrato0"
cond <- datos.bin$habitat=="estrato0"
with(datos.bin[cond,],plot(jitter(edad),jitter(uso_int,0.2),xlab="Edad",ylab=quote(p(x)),main="Estrato 0",cex=0.4,cex.axis=0.6,cex.lab=0.6,cex.main=0.7))
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[3]) )),  col="darkgreen",lwd=2, add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[3]+coef.m[9]) )), lty=2,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[3]+coef.m[10]) )),lty=3,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[3]+coef.m[11]) )),lty=4,lwd=2,add=TRUE)

legend(x=70,y=0.95, nombres, col=c("darkgreen",rep("black",3)),cex=0.7,lty=1:4,lwd=rep(2,4))


# estrato=="estrato1"
cond <- datos.bin$habitat=="estrato1"
with(datos.bin[cond,],plot(jitter(edad),jitter(uso_int,0.2),xlab="Edad",ylab=quote(p(x)),main="Estrato 1",cex=0.4,cex.axis=0.6,cex.lab=0.6,cex.main=0.7))
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[4]) )),  col="darkgreen",lwd=2, add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[4]+coef.m[9]) )), lty=2,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[4]+coef.m[10]) )),lty=3,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[4]+coef.m[11]) )),lty=4,lwd=2,add=TRUE)

legend(x=70,y=0.95, nombres, col=c("darkgreen",rep("black",3)),cex=0.7,lty=1:4,lwd=rep(2,4))

# estrato=="estrato2"
cond <- datos.bin$habitat =="estrato2"
with(datos.bin[cond,],plot(jitter(edad),jitter(uso_int,0.2),xlab="Edad",ylab=quote(p(x)),main="Estrato 2",cex=0.4,cex.axis=0.6,cex.lab=0.6,cex.main=0.7))
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[5]) )),  col="darkgreen",lwd=2, add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[5]+coef.m[9]) )), lty=2,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[5]+coef.m[10]) )),lty=3,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x+coef.m[5]+coef.m[11]) )),lty=4,lwd=2,add=TRUE)

legend(x=70,y=0.95, nombres, col=c("darkgreen",rep("black",3)),cex=0.7,lty=1:4,lwd=rep(2,4))




modelo.7 <- glm(uso_int ~ edad*nivelest, data=datos.bin, family=binomial)
summary(modelo.7)







modelo.7.sin.inter <- update(modelo.7, ~.-edad:nivelest)
coef.m.sin <- coef(modelo.7.sin.inter)
coef.m <- coef(modelo.7)
par(mfrow=c(2,2))
# modelo con efectos principales
with(datos.bin,plot(jitter(edad),logit(uso_int),type="n",xlab="Edad",ylab="logit(x)",main="Modelo con efectos principales \n glm(uso_int ~ edad+nivelest, family=binomial)",cex=0.4,cex.axis=0.6,cex.lab=0.6,cex.main=0.7))
curve(coef.m.sin[1]+coef.m.sin[2]*x,  col="darkgreen",lwd=2, add=TRUE)
curve(coef.m.sin[1]+coef.m.sin[2]*x+coef.m.sin[3], lty=2,lwd=2,add=TRUE)
curve(coef.m.sin[1]+coef.m.sin[2]*x+coef.m.sin[4],lty=3,lwd=2,add=TRUE)
curve(coef.m.sin[1]+coef.m.sin[2]*x+coef.m.sin[5],lty=4,lwd=2,add=TRUE)
nombres <- levels(datos.bin$nivelest)
legend("topright", nombres, col=c("darkgreen",rep("black",3)),cex=0.5,lty=1:4,lwd=rep(2,4))

# modelo con efectos principales e interación

with(datos.bin,plot(jitter(edad),logit(uso_int),type="n",xlab="Edad",ylab="logit(x)",main="Modelo con interacción\n glm(uso_int ~ edad*nivelest, family=binomial)",cex=0.4,cex.axis=0.6,cex.lab=0.6,cex.main=0.7))
curve(coef.m[1]+coef.m[2]*x,  col="darkgreen", add=TRUE)
curve(coef.m[1]+(coef.m[2]+coef.m[6])*x+coef.m[3], lty=2,lwd=2,add=TRUE)
curve(coef.m[1]+(coef.m[2]+coef.m[7])*x+coef.m[4],lty=3,lwd=2,add=TRUE)
curve(coef.m[1]+(coef.m[2]+coef.m[8])*x+coef.m[5],lty=4,lwd=2,add=TRUE)
nombres <- levels(datos.bin$nivelest)
legend("topright", nombres, col=c("darkgreen",rep("black",3)),cex=0.5,lty=1:4,lwd=rep(2,4))

# modelo con efectos principales
with(datos.bin,plot(jitter(edad),jitter(uso_int,0.05),type="n",xlab="Edad",ylab=quote(p(x)),main="Modelo con efectos principales \n glm(uso_int ~ edad+nivelest, family=binomial)",cex=0.4,cex.axis=0.6,cex.lab=0.6,cex.main=0.7))
curve(1/(1+exp(-(coef.m.sin[1]+coef.m.sin[2]*x) )),  col="darkgreen",lwd=2, add=TRUE)
curve(1/(1+exp(-(coef.m.sin[1]+coef.m.sin[2]*x+coef.m.sin[3]) )), lty=2,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m.sin[1]+coef.m.sin[2]*x+coef.m.sin[4]) )),lty=3,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m.sin[1]+coef.m.sin[2]*x+coef.m.sin[5]) )),lty=4,lwd=2,add=TRUE)
nombres <- levels(datos.bin$nivelest)
legend("topright", nombres, col=c("darkgreen",rep("black",3)),cex=0.5,lty=1:4,lwd=rep(2,4))

# modelo con efectos principales e interación
coef.m <- coef(modelo.7)
with(datos.bin,plot(jitter(edad),jitter(uso_int,0.05),type="n",xlab="Edad",ylab=quote(p(x)),main="Modelo con interacción\n glm(uso_int ~ edad*nivelest, family=binomial)",cex=0.4,cex.axis=0.6,cex.lab=0.6,cex.main=0.7))
curve(1/(1+exp(-(coef.m[1]+coef.m[2]*x) )),  col="darkgreen", add=TRUE)
curve(1/(1+exp(-(coef.m[1]+(coef.m[2]+coef.m[6])*x+coef.m[3]) )), lty=2,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m[1]+(coef.m[2]+coef.m[7])*x+coef.m[4]) )),lty=3,lwd=2,add=TRUE)
curve(1/(1+exp(-(coef.m[1]+(coef.m[2]+coef.m[8])*x+coef.m[5]) )),lty=4,lwd=2,add=TRUE)
nombres <- levels(datos.bin$nivelest)
legend("topright", nombres, col=c("darkgreen",rep("black",3)),cex=0.5,lty=1:4,lwd=rep(2,4))



