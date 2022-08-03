#he variables in 3-11 may be adjusted for line items in Table 1

xa1<-0.5
xr1<-0.7
k11<-0.65
k21<-1-k11

xa2<-0.5
xr2<-0.7
k12<-0.4
k22<-1-k12




#These are network parameters. Best not to change these
maxa = 10; min = -10; rest = -0.1; decay = 0.1; 
estr = 0.1; alpha = 0.1; 
numunits = 4; epsilon <- 0.0001; #a small value to determine convergence

netunits1<-data.frame();
netunits2<-data.frame();

#These are data-frames representing units in the network
newunit11 <- data.frame(num = 1, name = "ref", input = xr1, activation = rest, bias = 0, lm = FALSE);
newunit21 <- data.frame(num = 2, name = "sim", input = 0, activation = rest, bias = 0, lm = TRUE);
newunit31<- data.frame(num = 3, name = "dissim", input = 0, activation = rest, bias = 0, lm = TRUE);
newunit41<- data.frame(num = 4, name = "out", input = xa1, activation = rest, bias = 0, lm = FALSE);
#newunit5<- data.frame(num = 5, name = "rpt", input = -xa, activation = rest, bias = 0, lm = FALSE);

newunit12 <- data.frame(num = 1, name = "ref", input = xr2, activation = rest, bias = 0, lm = FALSE);
newunit22 <- data.frame(num = 2, name = "sim", input = 0, activation = rest, bias = 0, lm = TRUE);
newunit32<- data.frame(num = 3, name = "dissim", input = 0, activation = rest, bias = 0, lm = TRUE);
newunit42<- data.frame(num = 4, name = "out", input = xa2, activation = rest, bias = 0, lm = FALSE);

rbind(newunit11, newunit21, newunit31, newunit41)->netunits1;
rbind(newunit12, newunit22, newunit32, newunit42)->netunits2;



#weight matrix

wts1 <- matrix(c(
  0, k11, 0, 0,
  0, 0, 0, 1,
  -k21, 0, 0, 0,
  0, 0, 1, 0
  ), numunits, numunits,
  byrow = TRUE
); ##reciept in col; send in rows

wts2 <- matrix(c(
  0, k12, 0, 0,
  0, 0, 0, 1,
  -k22, 0, 0, 0,
  0, 0, 1, 0
), numunits, numunits,
byrow = TRUE
); ##reciept in col; send in rows

#This is the activation function
#For the other units it  outputs the input value (bounded by a min and a max)
actout<-function(inp) {
  
    if (inp < min) {return(min)}
    else if (inp > maxa){return(maxa)}
    else {return(inp)}
  
}  

#This function updates the activations
update1<-function() {
  #Calculate all the new activations
  netinput<-c();
  
  t(netunits1["activation"])%*%wts1->activinput;
  #print(activinput[1])
  
  
  for (l in 1:numunits) {
    alpha*activinput[l]+estr*netunits1$input[l]+netunits1$bias[l]-
      decay*netunits1$activation[l]->netinput[l];
    
    actout(netinput[l] + netunits1$activation[l]) -> netunits1$activation[l];
  }
  
  return(netunits1);
  
  
}  

update2<-function() {
  #Calculate all the new activations
  netinput<-c();
  
  t(netunits2["activation"])%*%wts2->activinput;
  #print(activinput[1])
  
  
  for (l in 1:numunits) {
    alpha*activinput[l]+estr*netunits2$input[l]+netunits2$bias[l]-
      decay*netunits2$activation[l]->netinput[l];
    
    actout(netinput[l] + netunits2$activation[l]) -> netunits2$activation[l];
  }
  
  return(netunits2);
  
  
}  



###Main routine starts here

for (i in 1:300) {
  update1()->netunits1;
  update2()->netunits2;
  print(i)
  print(netunits1$activation[4]); #this is to see network output
  print(netunits2$activation[4]);

}

calcY <- function(k1, k2, xr, xa) {
  y<-(k1*xr/(1+k1*k2)) + (xa/(1+k1*k2))
  y
}

