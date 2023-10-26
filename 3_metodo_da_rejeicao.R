
## metodo de aceitacao da por funcao envelope

x = seq(-4,4,0.01)
fx = exp(-x^2/2)*(sin(6*x)+3*cos(x)^2*sin(4*x)^2+1)

plot(x,fx,type ='l')


gx = 5*exp(-x^2/2)


lines(x,gx)
