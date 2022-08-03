library(tidyverse)
res<-c()
ag = 0.61 #0.21, 0.41, 0.61 (3 attention conditions)


f<-function(r) {
  k1<-sqrt(r^2+t^2)/sqrt(2)
  #k1<-r*t
  abs(((k1*ag/(1+k1*(1-k1))) + ((r*(1-t))/(1+k1*(1-k1))) - ag))
}

t<-0
for (i in 1:100) {
  optim(0,f,method="BFGS")$par->res[i]
  #if (res[i] < res[1]) {
  #  res[i] <- res[1]
  #}
  t<-t+0.01
}


min.RSS <- function(data, par) {
  with(data, sum(((par[1]/(1+par[2]*x)) - y)^2))
}

exp.RSS <- function(data, par) {
  with(data, sum((((par[1]*exp(par[2]*x)) - y)^2)))
}

resdf<-data.frame(x = seq(0.01, 1, by = 0.01),  y = res)
result <- optim(par = c(1, 0), fn = min.RSS, data = resdf)

eresdf<-data.frame(x = seq(0.01, 1, by = 0.01),  y = res)
eresult <- optim(par = c(1, 0), fn = exp.RSS, data = eresdf)

hyperbola<-c()
x<-0.01
for(i in 1:100) {
  hyperbola[i]<-result$par[1]/(1+result$par[2]*x)
  x<-x+0.01
}

expo<-c()
x<-0.01
for(i in 1:100) {
  expo[i]<-eresult$par[1]*exp(eresult$par[2]*x)
  x<-x+0.01
}

Sim4df<-data.frame(index = 1:100, 
                   reward = res,
                   c1=hyperbola, c2=expo)

ggplot(Sim4df, aes(x=index)) + 
  geom_line(aes(y = reward), color="green") +
  geom_line(aes(y = hyperbola), color="blue") +
  geom_line(aes(y = expo), color="red") +
  ylim(0,1) +
  theme_classic()

sum((res-hyperbola)^2)
sum((res-expo)^2)

