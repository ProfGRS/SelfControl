# July 6, 2022
# Use Foldiaks engine

# Show Distributed Representations of Snacks
# Also show how a goal gets represented

#install.packages("readxl")
library(readxl)
library(tidyverse)

read_excel("foursnacks2.xlsx")->itemdf

set.seed(100)

names(itemdf)<-NULL

itemdf[,2:6]->itemdf
t(itemdf)->itemdf

########Setup Functions: Softmax, Normalize
normalize <- function(M) {
  #center data
  means = apply(M,2,mean)
  Xnorm = t(apply(M,1,function(x) {x-means}))
  Xnorm
}


softmax <- function(vec, lambda) {
  ##Multiply vec by lambda
  vec <- lambda*vec
  ##Exponentiate
  vec<- exp(vec)
  vec<-vec/(0.6*sum(vec))
  return(vec)
}


normalize(itemdf)->sitemdf

#Declare the parameters
eta<-0.02
beta<-0.01
epsilon<-0.01

n_components<-4 #representation layer
n_obs<-5

trainNetwork <- function(Xnorm,repunit,eta) {
  #Xnorm is the normalized matrix (n_obs x features)
  #n_components is the number of representation units 
  
  # initialize
  n_features = ncol(Xnorm) #this is 24 in the present data set
  epochs = 2500
  z = epochs*nrow(Xnorm) 
  Q = matrix(data=runif(n_features*repunit,-1, 1), 
             nrow=repunit, 
             ncol=n_features)
  
  #Q[i,] are the Oja weights into the ith repunit
  #W[j,] are the decorrelating weights into the jth repunit
  
  Q = normalize(Q)
  W = matrix(data = 0, nrow=repunit, ncol=repunit)
  Y = matrix(data=0,nrow=n_obs,ncol=repunit)
  Yhist<-list()
  svdhist<-list()
  # train
  for (n in seq(1:z)) {
    ind = n%%(n_obs)+1
    for (j in seq(1:repunit)) {
      Y[ind,j] = Q[j,]%*%Xnorm[ind,] + W[j,]%*%Y[ind,]
      Q[j,] = Q[j,]+eta*(Y[ind,j]*Xnorm[ind,]-
                           ((Y[ind,j])^2)*(Q[j,]))
      for (k in 1:repunit) {
        if (j!=k) {
          W[j,k] = W[j,k] - beta*Y[ind, k]*Y[ind, j] + 
            (beta/100)*(Y[ind,k]^2+Y[ind,j]^2)*W[j,k]
          #W[j,k] = W[j,k] - beta*Y[ind, k]*Y[ind, j] 
        }  
      }
    } #for each rep unit
    #dwts<<-learndeltawts(Y[ind, ], itemdf[ind, ], dwts)
    Yhist[[n]]<-Y
    #svdhist[[n]]<-svd(Y%*%dwts)$d  
  } #for each observation
  #list(data = Y, pc = Q, history = Yhist, svdh = svdhist)
  list(data = Y, pc = Q, history = Yhist)
}

results = trainNetwork(sitemdf, n_components,eta)

softmax(results$data[1 ,], 1)->apple
softmax(results$data[2, ], 1)->grape
softmax(results$data[3, ], 1)->cake
softmax(results$data[4, ], 1)->cookie
softmax(results$data[5, ], 1)->goal


par(mfrow=c(1,2))
barplot(apple, ylim=c(0,1)) 
barplot(grape, ylim=c(0,1)) 
barplot(cake, ylim=c(0,1)) 
barplot(cookie, ylim=c(0,1))
barplot(goal, ylim=c(0,1))




######Generate and Plot the Correlation Matrix
coremat <- matrix(c(apple, grape, cake, cookie, goal), ncol = 5)

cormat <- round(cor(coremat),2)
cormat

library(reshape2)
melted_cormat <- melt(cormat)
melted_cormat

#lower_tri <- get_lower_tri(cormat)
#lower_tri

#melted_cormat <- melt(lower_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


######End of code for correlation matrix

###apple: 0.07870409 0.21402652 0.13267074 1.24126533
###cake: 0.80898203 0.57052801 0.26534348 0.02181315
###goal: 0.09781264 0.70902785 0.08576166 0.77406451
