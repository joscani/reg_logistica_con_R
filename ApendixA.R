

lreg1 <- function(X, y, max.iter = 10, tol = 1e-6, verbose = FALSE){
# X es el model matrix, si hay variables categóricas hay que introducir la matriz con 
# las variables ya codificadas.
# y es el vector de respuestas de 0,1

X <- cbind(1,X) # se añade una columna de unos, para el intercept
b <- b.last <- rep(0, ncol(X))
it <- 1

while (it<=max.iter){
	if(verbose) cat("\niteration = ", it, ",",b)
	p <- as.vector(1/(1+exp(-X%*%b))) # prob ajustadas
	V <- diag( p * (1 - p) )
	var.b <- solve( t(X) %*% V %*% X ) # inversa de la matriz hessiana, en la diagonal
#se tienen los valores de la varianza de los parámetros
	b <- b+var.b %*% t(X) %*% (y-p)
	if (max(abs(b-b.last)/(abs(b.last)+0.001*tol))< tol) break
	b.last <- b
	it <- it+1
	}
	if (verbose) cat("\n")
	if (it > max.iter) warning("número máximo de iteraciones excedido")
	list(coefficients=as.vector(b),var=var.b, iterations=it)
}



set.seed(234) # para que siempre salgan los mismos valores simulados
# simulamos 40 valores de ensayos de bernoulli con probabilidad de éxito 0.4
y <- rbinom(40,1,0.4) 
head(y)
# 40 valores de una normal con media 2 y desviación típica 5
X <- rnorm(40,2,5) 
head(X)



lreg1(X,y)
lreg1(X,y,verbose=TRUE) # para ver el ajuste en cada iteración



res <- glm(y ~ X, family=binomial)
res



lreg2 <- function(X, y, method="BFGS"){
	X <- cbind(1,X) # si hay variables categóricas se deben introducir
	# las variables auxiliares correspondientes
	negLogL <- function(b, X, y){
	p  <- as.vector(1/(1+exp(-X %*% b)))
	- sum(y*log(p) + (1-y) * log(1-p) )
	}
	grad <- function(b, X, y){
	p <- as.vector(1/(1+exp(-X %*% b)))

    - colSums((y-p)*X) 
	}
	result <- optim(rep(0, ncol(X)), negLogL, gr=grad,
	hessian=TRUE, method=method, X=X, y=y)
	list(coefficients=result$par, var = solve(result$hessian), 
	deviance = 2*result$value, converged=result$convergence ==0)
}



lreg2(X,y)



lreg3 <- function(X,y,max.iter=10,tol=1e-6,verbose=FALSE){
# X es el model matrix
# y es el vector de respuestas de 0,1

X <- cbind(1,X) # se añade columna de unos para estimar el intercept 
# si hay variables categóricas se deben introducir
# las variables auxiliares correspondientes
b <- b.last <- rep(0,ncol(X)) # se inicializa el vector de parámetros en 0's
it <- 1
while (it<=max.iter){
	if(verbose) cat("\niteration = ", it, ",",b)
	p  <- as.vector(1/(1+exp(-X%*%b))) # prob ajustadas
	v <-  (p*(1-p))
	z <- (X%*%b)+(y-p)/v
	# N ha de ser el número de pruebas binomiales en cada i
	N <- 1
	w <- N*v
	V <- diag(v)
	var.b <- solve(t(X) %*% V %*% X) # matriz de varianzas/covarianzas de los parámetros	
	result <- lm(z ~ X[,-1], weights = w)
	b <- result$coefficients
	if (max(abs(b-b.last)/(abs(b.last)+0.001*tol))< tol) break
	b.last <- b
	it <- it+1
	}
	if (verbose) cat("\n")
	#if (it > max.iter) warning("número máximo de iteraciones excedido")
	list(coefficients=as.vector(b),var=var.b, iterations=it)
	
}




lreg3(X,y)


