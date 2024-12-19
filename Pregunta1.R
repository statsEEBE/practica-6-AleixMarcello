
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 
       505, 493, 496, 506, 502, 509, 496)

n = 16
sigma = sqrt(25)
alpha = 0.1

#a) CASO 1 conf.level=90%
xbar = mean(x)
xbar
z005 = qnorm(0.95)
c(xbar-z005*sigma/sqrt(n), xbar+z005*sigma/sqrt(n) )

library(BSDA)
z.test(x, sigma.x = sigma, conf.level= 0.9)

#ejemplo hipotesis; interés en saber si nuestra media muestral es mayor que la media mu0=500
#cola superior
mu0= 500
zc = qnorm(0.9)
zc
zobs = (xbar-mu0)/(sigma/sqrt(n))
zobs
#zobs > zc => rechazamos H0

p_valor = 1 - pnorm(zobs)
p_valor
#p_valor < alpha => rechazamos H0

z.test(x,sigma.x= sigma, conf.level = 0.9, alternative="greater", mu = mu0)


#b) conf.level=95%

n=?
m = 1 = qnorm(0.975)*sigma/sqrt(n)
n = (qnorm(0.975)*sigma)^2
n #n = 97

#c) CASO 2; alpha = 0.01
s = sd(x)
xbar = mean(x)
n = 16
c(xbar-qt(0.995, n-1)*s/sqrt(n), xbar+qt(0.995, n-1)*s/sqrt(n))

t.test(x,conf.level = 0.99)

#ejemplo hipotesis: 
mu0 = 500
#H0: mu <= mu0 
#H1: mu > mu0 (cola superior)

t.test(x,conf.level = 0.99, alternative = "greater", mu = mu0)
tc = qt(0.99, n-1)
tobs = (xbar-mu0)/(s/sqrt(n))
tobs
#tobs < tc => aceptamos H0
p_valor = 1 - pt(tobs, n-1)
p_valor
#p_valor > alpha => aceptamos H0

#d) CASO 4; alpha = 0.05