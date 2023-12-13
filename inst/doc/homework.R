## -----------------------------------------------------------------------------
library(ggplot2)

## -----------------------------------------------------------------------------
length<-iris$Sepal.Length
width<-iris$Sepal.Width
lm1<-lm(length ~ width)
lm1
par(mfrow=c(2,2))
xtable::xtable(head(iris))

## -----------------------------------------------------------------------------
x<-seq(-10,10,by=0.1)
y=1/(1+exp(-0.5*x+1))
plot(x,y,type='l',pch=15,lty=1,col='blue')
text(1,0.7,as.expression(substitute(y==over(1,1+e^(beta*x+alpha)),list(alpha=1,beta=-0.5))))

## -----------------------------------------------------------------------------
iris <- datasets::iris
plot1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + 
    theme_bw(base_size = 9) +
    geom_point(aes(colour = iris$Species)) + 
    labs(title = "散点图")
plot1 
plot2 <- ggplot(iris,aes(Sepal.Width))+
    theme_minimal(base_size = 9)+
    geom_density(aes(colour = Species,fill = Species),alpha = 0.5)+
    labs(title = "Density 密度曲线")+
    theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8,0.8))
plot2

## -----------------------------------------------------------------------------
my.sample<-function(x,size,p){
  l<-length(x)
  r<-rep(0,size)
  for (i in 1:size) {
    cp<-cumsum(p)
    m<-1e5
    U<-runif(m)
    r[i]<-x[findInterval(U,cp)+1]
  }
  r
}

## -----------------------------------------------------------------------------
my.sample(2:6,3,c(0.1,0.2,0.4,0.2,0.1))

## -----------------------------------------------------------------------------
my.sample(1:100,3,rep(0.01,100))

## -----------------------------------------------------------------------------
my.sample(c(99,2022,2023),2,c(0.4,0.2,0.4))

## -----------------------------------------------------------------------------
set.seed(20201021)
u= runif(1000) # generate 1000 random numbers form U(0,1)
x <- ifelse(u<=0.5,log(2*u),log(1/(2*(1-u))))
y <- seq(-10,10,0.2)
hist(x,probability = T)
lines(y,0.5*exp(-abs(y)),lwd = 2)

## -----------------------------------------------------------------------------
n <- 1000
k <- 0 
j <- 0 
y <- numeric(n)
while (k < n) {
	u <- runif(1)
	j <- j + 1
	x <- runif(1) 
	if (x * (1-x) > u) {
		k <- k + 1
		y[k] <- x
	} 
}

hist(y,probability = TRUE, breaks = 10)
xx = seq(0, 1, 0.01)
yy = 12*xx*(1-xx)
lines(xx, yy)

## -----------------------------------------------------------------------------
u<-rep(0,1000)
for (i in 1:1000){
u1<-runif(1,-1,1)
u2<-runif(1,-1,1)
u3<-runif(1,-1,1)
if ((abs(u3)>=abs(u2))&(abs(u3)>=abs(u1)))
  u[i]<-u2 else
    u[i]<-u3
}

hist(u)

## -----------------------------------------------------------------------------
    set.seed(12345)
    l <- 1
    d <- 1
    m <- 1e6
    
    X <- runif(m,0,d/2)
    Y <- runif(m,0,pi/2)
    pihat <- 2*l/d/mean(l/2*sin(Y)>X)
    pihat

## -----------------------------------------------------------------------------
    set.seed(12345)
    l <- 1
    d <- 0.5
    m <- 1e6
    
    X <- runif(m,0,d/2)
    Y <- runif(m,0,pi/2)
    pihat <- 2*l/d/mean(l/2*sin(Y)>X)
    pihat

## -----------------------------------------------------------------------------
    set.seed(12345)
    l <- 1
    d <- 0.75
    m <- 1e6
    
    X <- runif(m,0,d/2)
    Y <- runif(m,0,pi/2)
    pihat <- 2*l/d/mean(l/2*sin(Y)>X)
    pihat

## -----------------------------------------------------------------------------
mc<-replicate(1000,expr={mean(exp(runif(10000)))})
anti<-replicate(1000,expr={u<-runif(5000)
v<-1-u
mean(exp(u)+exp(v))/2})
v1<-var(mc)
v2<-var(anti)
(v1-v2)/v1

## -----------------------------------------------------------------------------
x<-seq(1,10,0.01)
y <- x^2 * exp(-x^2/2)/sqrt(2 * pi)
plot(x, y, type = "l", ylim = c(0, 1))
lines(x, 2 * dnorm(x, 1), lty = 2)
lines(x, dgamma(x - 1, 3/2, 2), lty = 3)
legend("topright", legend = c("g(x)", "f1", "f2"), lty = 1:3)

## -----------------------------------------------------------------------------
plot(x, y/(dgamma(x - 1, 3/2, 2)), type = "l", lty = 3,ylab = "")
lines(x, y/(2 * dnorm(x, 1)), lty = 2)
legend("topright", legend = c("f1", "f2"),lty = 2:3)

## -----------------------------------------------------------------------------
m<-10000
data1<-replicate(1000,expr={
x<-sqrt(rchisq(m,1))+1
f<-2*dnorm(x,1)
g<- x^2*exp(-x^2/2)/sqrt(2 * pi)
mean(g/f)
})
data2<-replicate(1000,expr={
x<-rgamma(m, 3/2, 2) + 1
f<-dgamma(x - 1, 3/2, 2)
g<-x^2 * exp(-x^2/2)/sqrt(2 * pi)
mean(g/f)
})
c(mean(data1),mean(data2))
c(var(data1),var(data2))
var(data1)/var(data2)

## -----------------------------------------------------------------------------
M <- 10000; k <- 10 
r <- M/k 
N <- 50 
T2 <- numeric(k)
est <- matrix(0, N, 2)
g<-function(x) x^2*exp(-x^2/2)/sqrt(2 * pi)
for (i in 1:N) {
est[i, 1] <- mean(g(runif(M)))
for(j in 1:k)T2[j]<-mean(g(runif(M/k,(j-1)/k,j/k)))
est[i, 2] <- mean(T2)
}
round(apply(est,2,mean),4)
round(apply(est,2,sd),4)

## -----------------------------------------------------------------------------
n <- 20
alpha <- .05
t0<- qt(c(0.025,0.975),df=n-1)
cl<- replicate(1000,expr={x <- rchisq(n,df=2)
c <- mean(x) + t0 * sd(x)/sqrt(n)})
lcl<-cl[1,]
ucl<-cl[2,]
mean(lcl<2 & ucl>2)

## -----------------------------------------------------------------------------
    n=5
    x<-rexp(n,rate=2) 
    B <- 1e3; set.seed(12345);thetastar <- numeric(B)
    lambda <- 1/mean(x); 
    a<-replicate(1000,expr={
    for(b in 1:B){
      xstar <- sample(x,replace=TRUE)
      thetastar[b] <- 1/mean(xstar)
      
    }
      c(mean(thetastar)-lambda,sd(thetastar))
    })
  mean(a[1])
  2/(n-1)
  mean(a[2])
  n*2/(n-1)/sqrt(n-2)

## -----------------------------------------------------------------------------
    n=10
    x<-rexp(n,rate=2) 
    B <- 1e3; set.seed(12345);thetastar <- numeric(B)
    lambda <- 1/mean(x); 
    a<-replicate(1000,expr={
    for(b in 1:B){
      xstar <- sample(x,replace=TRUE)
      thetastar[b] <- 1/mean(xstar)
      
    }
      c(mean(thetastar)-lambda,sd(thetastar))
    })
  mean(a[1])
  2/(n-1)
  mean(a[2])
  n*2/(n-1)/sqrt(n-2)

## -----------------------------------------------------------------------------
    n=20
    x<-rexp(n,rate=2) 
    B <- 1e3; set.seed(12345);thetastar <- numeric(B)
    lambda <- 1/mean(x); 
    a<-replicate(1000,expr={
    for(b in 1:B){
      xstar <- sample(x,replace=TRUE)
      thetastar[b] <- 1/mean(xstar)
      
    }
      c(mean(thetastar)-lambda,sd(thetastar))
    })
  mean(a[1])
  2/(n-1)
  mean(a[2])
  n*2/(n-1)/sqrt(n-2)

## -----------------------------------------------------------------------------
library(boot)
library(bootstrap)
cor.stat <- function(x, i = 1:NROW(x)) { cor(x[i, 1], x[i, 2])}
cor.stat2 <- function(x, i = 1:NROW(x)) {
o <- boot(x[i, ], cor.stat, R = 25)
 n <- length(i)
c(o$t0, var(o$t) * (n - 1)/n^2)
}
b <- boot(law, statistic = cor.stat2, R = 1000)
boot.ci(b, type = "stud")

## -----------------------------------------------------------------------------
m <- 1000
M <- 10000
FWER.bonf <- FDR.bonf <- TPR.bonf <- numeric(M)
FWER.bh <- FDR.bh <- TPR.bh <- numeric(M)
for (i in 1:M) {
  p <- numeric(1000)
  p[1:950] <- runif(950)
  p[951:1000] <- rbeta(50, 0.1, 1)
  p.bonf <- p.adjust(p, method = 'bonferroni')
  p.bh <- p.adjust(p, method = 'fdr')
  FWER.bonf[i] <- max(p.bonf[1:950] < 0.1)
  FDR.bonf[i] <- sum(p.bonf[1:950] < 0.1)/sum(p.bonf < 0.1)
  TPR.bonf[i] <- sum(p.bonf[951:1000] < 0.1)/50
  FWER.bh[i] <- max(p.bh[1:950] < 0.1)
  FDR.bh[i] <- sum(p.bh[1:950] < 0.1)/sum(p.bh < 0.1)
  TPR.bh[i] <- sum(p.bh[951:1000] < 0.1)/50
}
c(mean(FWER.bonf),mean(FWER.bh))
c(mean(FDR.bonf),mean(FDR.bh))
c(mean(TPR.bonf),mean(TPR.bh))

## -----------------------------------------------------------------------------
 library(boot)
 x <- aircondit[1]
 meant <- function(x, i) return(mean(as.matrix(x[i, ])))
 b <- boot(x, statistic = meant, R = 2000)
 b
 boot.ci(b, type = c("norm", "perc", "basic", "bca"))
 detach(package:boot)
 hist(b$t, prob = TRUE, main = "")
 points(b$t0, 0, cex = 2, pch = 16)

## -----------------------------------------------------------------------------
 library(bootstrap)
 attach(scor)
 x <- as.matrix(scor)
 n <- nrow(x)
 theta.jack <- numeric(n)
 lambda <- eigen(cov(x))$values
 theta.hat <- max(lambda/sum(lambda))
 for (i in 1:n) {
 y <- x[-i, ]
 s <- cov(y)
 lambda <- eigen(s)$values
 theta.jack[i] <- max(lambda/sum(lambda))
 }
 bias.jack <- (n - 1) * (mean(theta.jack) - theta.hat)
 se.jack <- sqrt((n - 1)/n * sum((theta.jack - mean(theta.jack))^2))
 c(theta.hat, bias.jack, se.jack)
 list(est = theta.hat, bias = bias.jack, se = se.jack)

## -----------------------------------------------------------------------------
 library(DAAG, warn.conflict = FALSE)
 attach(ironslag)
 n <- length(magnetic)
 N <- choose(n, 2)
 e1 <- e2 <- e3 <- e4 <- e5 <- numeric(N)
 ij <- 1
 for (i in 1:(n - 1)) for (j in (i + 1):n) {
 k <- c(i, j)
 y <- magnetic[-k]
 x <- chemical[-k]
 J1 <- lm(y ~ x)
 yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
 e1[ij] <- sum((magnetic[k] - yhat1)^2)
 J2 <- lm(y ~ x + I(x^2))
 yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
 J2$coef[3] * chemical[k]^2
 e2[ij] <- sum((magnetic[k] - yhat2)^2)
 J3 <- lm(log(y) ~ x)
 logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
 yhat3 <- exp(logyhat3)
 e3[ij] <- sum((magnetic[k] - yhat3)^2)
 J4 <- lm(log(y) ~ log(x))
 logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
 yhat4 <- exp(logyhat4)
 e4[ij] <- sum((magnetic[k] - yhat4)^2)
 c2 <- x^2
 c3 <- x^3
 J5 <- lm(y ~ x + c2 + c3)
 yhat5 <- J5$coef[1] + J5$coef[2] * chemical[k] +
 J5$coef[3] * chemical[k]^2 + J5$coef[4] * chemical[k]^3
 e5[ij] <- sum((magnetic[k] - yhat5)^2)
 ij <- ij + 1
 }
 c(sum(e1), sum(e2), sum(e3), sum(e4), sum(e5))/N

## -----------------------------------------------------------------------------
 cvm.test <- function(x, y, R = 199) {
 n <- length(x)
 m <- length(y)
 z <- c(x, y)
N <- n + m
 Fn <- numeric(N)
 Gm <- numeric(N)
 for (i in 1:N) {
 Fn[i] <- mean(as.integer(z[i] <= x))
 Gm[i] <- mean(as.integer(z[i] <= y))
 }
 cvm0 <- ((n * m)/N) * sum((Fn - Gm)^2)
 cvm <- replicate(R, expr = {
 k <- sample(1:N)
 Z <- z[k]
 X <- Z[1:n]
 Y <- Z[(n + 1):N]
 for (i in 1:N) {
 Fn[i] <- mean(as.integer(Z[i] <= X))
 Gm[i] <- mean(as.integer(Z[i] <= Y))
 }
 ((n * m)/N) * sum((Fn - Gm)^2)
 })
 cvm1 <- c(cvm, cvm0)
 return(list(statistic = cvm0, p.value = mean(cvm1 >=
 cvm0)))
 }
 attach(chickwts)
 x1 <- as.vector(chickwts$weight[feed == "soybean"])
 x2 <- as.vector(chickwts$weight[feed == "sunflower"])
  x3 <- as.vector(chickwts$weight[feed == "linseed"])
 detach(chickwts)
 a<-cvm.test(x1,x3)
 a[2]
 b<-cvm.test(x2,x3)
 b[2]

## -----------------------------------------------------------------------------
 test <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
return(max(c(outx, outy)))
}
out <- function(x, y, R = 199) {
z <- c(x, y)
n <- length(x)
N <- length(z)
stats <- replicate(R, expr = {
k <- sample(1:N)
k1 <- k[1:n]
k2 <- k[(n + 1):N]
test(z[k1], z[k2])
})
stat <- test(x, y)
stats1 <- c(stats, stat)
tab <- table(stats1)/(R + 1)
return(list(estimate = stat, p = mean(stats1 >=
stat), freq = tab, cdf = cumsum(tab)))
}
set.seed(111)
n1 <- 20
n2 <- 40
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
a<-out(x, y)
a[1]
a[2]
a[3]

## -----------------------------------------------------------------------------
set.seed(12345)
f0<-rep(0,4)
a<-rep(0,4)
N <- 1e6; b1 <- 0; b2 <- 1;b3<--1; f0[1] <- 0.1
x1 <- rpois(N,1)
x2<-rexp(N,1) 
x3 <- sample(0:1,N,replace=TRUE)
g <- function(a){
tmp <- exp(-a-b1*x1-b2*x2-b3*x3); p <- 1/(1+tmp)
mean(p) - f0[1]
}
solution <- uniroot(g,c(-10,0))
round(unlist(solution),5)[1:3]
a[1]<-solution$root

## -----------------------------------------------------------------------------
set.seed(12345)
N <- 1e6; b1 <- 0; b2 <- 1;b3<--1; f0[2] <- 0.01
x1 <- rpois(N,1)
x2<-rexp(N,1) 
x3 <- sample(0:1,N,replace=TRUE)
g <- function(a){
tmp <- exp(-a-b1*x1-b2*x2-b3*x3); p <- 1/(1+tmp)
mean(p) - f0[2]
}
solution <- uniroot(g,c(-10,0))
round(unlist(solution),5)[1:3]
a[2]<-solution$root

## -----------------------------------------------------------------------------
set.seed(12345)
N <- 1e6; b1 <- 0; b2 <- 1;b3<--1; f0[3] <- 0.001
x1 <- rpois(N,1)
x2<-rexp(N,1) 
x3 <- sample(0:1,N,replace=TRUE)
g <- function(a){
tmp <- exp(-a-b1*x1-b2*x2-b3*x3); p <- 1/(1+tmp)
mean(p) - f0[3]
}
solution <- uniroot(g,c(-10,0))
round(unlist(solution),5)[1:3]
a[3]<-solution$root

## -----------------------------------------------------------------------------
set.seed(12345)
N <- 1e6; b1 <- 0; b2 <- 1;b3<--1; f0[4] <- 0.0001
x1 <- rpois(N,1)
x2<-rexp(N,1) 
x3 <- sample(0:1,N,replace=TRUE)
g <- function(a){
tmp <- exp(-a-b1*x1-b2*x2-b3*x3); p <- 1/(1+tmp)
mean(p) - f0[4]
}
solution <- uniroot(g,c(-15,2))
round(unlist(solution),5)[1:3]
a[4]<-solution$root

## -----------------------------------------------------------------------------
plot(a,f0)

## -----------------------------------------------------------------------------
 rw.Laplace <- function(N, x0, sigma) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
  xt <- x[i - 1]
  y <- rnorm(1, xt, sigma)
  if (u[i] <= exp(abs(xt) - abs(y)))
  x[i] <- y
  else {
  x[i] <- x[i - 1]
  k <- k + 1
  }
  }
 return(list(x = x, k = k))
 }
 N <- 5000
 sigma <- c(0.5, 1, 2, 4)
 x0 <- rnorm(1)
 rw1 <- rw.Laplace(N, x0, sigma[1])
 rw2 <- rw.Laplace(N, x0, sigma[2])
 rw3 <- rw.Laplace(N, x0, sigma[3])
 rw4 <- rw.Laplace(N, x0, sigma[4])
 print(c(rw1$k, rw2$k, rw3$k, rw4$k))
 cat("rejection rates ", (c(rw1$k, rw2$k, rw3$k, rw4$k)/N), "\n")
  b <- 100
 y1 <- rw1$x[(b + 1):N]
 y2 <- rw2$x[(b + 1):N]
 y3 <- rw3$x[(b + 1):N]
 y4 <- rw4$x[(b + 1):N]
  par(mfrow = c(2, 2))
 par(mfrow = c(1, 1))

## -----------------------------------------------------------------------------
N <- 5000
 burn <- 1000
 X <- matrix(0, N, 2)
 rho <- 0.9
 mu1 <- 0
 mu2 <- 0
 sigma1 <- 1
 sigma2 <- 1
 s1 <- sqrt(1 - rho^2) * sigma1
 s2 <- sqrt(1 - rho^2) * sigma2
 X[1, ] <- c(mu1, mu2)
 for (i in 2:N) {
 x2 <- X[i - 1, 2]
 m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
 X[i, 1] <- rnorm(1, m1, s1)
 x1 <- X[i, 1]
 m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
 X[i, 2] <- rnorm(1, m2, s2)
 }
 b <- burn + 1
 x <- X[b:N, ]
 X <- x[, 1]
 Y <- x[, 2]
 L <- lm(Y ~ X)
 L

## -----------------------------------------------------------------------------
plot(X, Y, cex = 0.25)
abline(h = 0, v = 0)

## -----------------------------------------------------------------------------
plot(L$fit, L$res, cex = 0.25)
abline(h = 0)

## -----------------------------------------------------------------------------
qqnorm(L$res, cex = 0.25)
qqline(L$res)

## -----------------------------------------------------------------------------
library(coda)
  f <- function(x, sigma) {
 if (x < 0)
 return(0)
 stopifnot(sigma > 0)
 return((x/sigma^2) * exp(-x^2/(2 * sigma^2)))
 }
Rayleigh.MH.chain1 <- function(sigma, m, x0) {
 x <- numeric(m)
 x[1] <- x0
 u <- runif(m)
 for (i in 2:m) {
 xt <- x[i - 1]
 y <- rchisq(1, df = xt)
 num <- f(y, sigma) * dchisq(xt, df = y)
 den <- f(xt, sigma) * dchisq(y, df = xt)
 if (u[i] <= num/den)
 x[i] <- y
 else x[i] <- xt
 }
 return(x)
 }
 sigma <- 4
 x0 <- c(1/sigma^2, 1/sigma, sigma^2, sigma^3)
 k <- 4
 m <- 2000
 X <- matrix(0, nrow = k, ncol = m)
 for (i in 1:k) X[i, ] <- Rayleigh.MH.chain1(sigma, m,
 x0[i])
 psi <- t(apply(X, 1, cumsum))
 for (i in 1:nrow(psi)) psi[i, ] <- psi[i, ]/(1:ncol(psi))
 

## -----------------------------------------------------------------------------
X1 <- as.mcmc(X[1, ])
X2 <- as.mcmc(X[2, ])
X3 <- as.mcmc(X[3, ])
X4 <- as.mcmc(X[4, ])
Y <- mcmc.list(X1, X2, X3, X4)
print(gelman.diag(Y))
gelman.plot(Y, col = c(1, 1))

## -----------------------------------------------------------------------------
u <- c(11,8,27,13,16,0,23,10,24,2)
n <- sum(u)
f <- function(x) -n-10*exp(-x)/(exp(-x)-1)
s1 <- uniroot(f, interval = c(0.0001,10))
s1$root

## -----------------------------------------------------------------------------

l0 <- 0
l1 <- 1
k <- 0
while (l1 - l0 > 0.00001 | k<10000) {
  l0 <- l1
  l1 <- 10/(n+10/l0+10*exp(-l0)/(exp(-l0)-1))
    k <- k+1
}
l1

## -----------------------------------------------------------------------------
solve.game <- function(A) {
 min.A <- min(A)
 A <- A - min.A
 max.A <- max(A)
 A <- A/max(A)
 m <- nrow(A)
 n <- ncol(A)
 it <- n^3
 a <- c(rep(0, m), 1)
 A1 <- -cbind(t(A), rep(-1, n))
 b1 <- rep(0, n)
 A3 <- t(as.matrix(c(rep(1, m), 0)))
 b3 <- 1
 sx <- simplex(a = a, A1 = A1, b1 = b1, A3 = A3, b3 = b3,
 maxi = TRUE, n.iter = it)
 a <- c(rep(0, n), 1)
 A1 <- cbind(A, rep(-1, m))
 b1 <- rep(0, m)
 A3 <- t(as.matrix(c(rep(1, n), 0)))
 b3 <- 1
 sy <- simplex(a = a, A1 = A1, b1 = b1, A3 = A3, b3 = b3,
 maxi = FALSE, n.iter = it)
 soln <- list(A = A * max.A + min.A, x = sx$soln[1:m],y = sy$soln[1:n], v = sx$soln[m + 1] * max.A +
 min.A)
soln }
 A <- matrix(c(0, -2, -2, 3, 0, 0, 4, 0, 0, 2, 0, 0, 0,
 -3, -3, 4, 0, 0, 2, 0, 0, 3, 0, 0, 0, -4, -4, -3,
 0, -3, 0, 4, 0, 0, 5, 0, 0, 3, 0, -4, 0, -4, 0, 5,
 0, 0, 3, 0, 0, 4, 0, -5, 0, -5, -4, -4, 0, 0, 0,
 5, 0, 0, 6, 0, 0, 4, -5, -5, 0, 0, 0, 6, 0, 0, 4,
 0, 0, 5, -6, -6, 0), 9, 9)
 library(boot)
 B <- A + 2
 s <- solve.game(B)
 s[4]
 round(cbind(s$x, s$y), 7)
 round(s$x * 61, 7)

## -----------------------------------------------------------------------------
scale01 <- function(x) {
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
df<-data.frame(x=1:4,y=seq(0,9,3))
vapply(df,sd,numeric(1))

## -----------------------------------------------------------------------------
df<-data.frame(x=1:4,y=c("a","b","c","d"))
vapply(df,sd,numeric(1))

## -----------------------------------------------------------------------------
N <- 10000
burn <- 2000
a <- 2
b <- 3
n <- 10
x <- y <- rep(0, N)
x[1] <- rbinom(1, prob = 0.5, size = n)
y[1] <- rbeta(1, x[1] + a, n - x[1] + b)
for (i in 2:N) {
x[i] <- rbinom(1, prob = y[i - 1], size = n)
y[i] <- rbeta(1, x[i] + a, n - x[i] + b)
}
xb <- x[(burn + 1):N]
f1 <- table(xb)/length(xb)
i <- 0:n
fx <- choose(n, i) * beta(i + a, n-i+ b)/beta(a, b)
round(rbind(f1, fx), 3)

