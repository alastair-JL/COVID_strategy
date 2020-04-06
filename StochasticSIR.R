##
##
##
##
##This is a simple "stochastic" SIR model, in a large population.
##
## I start out by using the classical gillespie algorithm (for small populations)
##
## And then switch to tau leaping once the population is large enough.
##
##Note that, generally speaking, stochasticity has a noticable impact on 
## the TIMING of the epidemic,
##
## But a neglibible impace on the peak, unless the population is <5000.
##


N=100000;

InfectionRate<-0.4;
recoveryRate<-0.1;

plot.new()
plot.window(xlim=c(0,20/recoveryRate), ylim=c(0,N/2))
axis(1)
axis(2) 
title(main="Many Stochastic Realizations")
title(xlab="times")
title(ylab="Infectious population")

for(qqq in c(1:10)){

  S=N-1
  I=1;
  R=0;
  N=S+I+R;
  
Ts=(1:5000)*0
Is=(1:5000)*0


Is[1]<-I
Ts[1]<-0;


dt=0.1;
iii=2;

while(I<2700 &I>0){
  CurrentInfectRate<-S*I*InfectionRate/N;
  currentRecovRate<-I*recoveryRate;
  Ts[iii]<-Ts[iii-1]+rexp(1, rate = CurrentInfectRate+currentRecovRate);
  if(rbinom(n = 1,  size = 1, prob = CurrentInfectRate/(CurrentInfectRate+currentRecovRate))){
    S<-S-1;
    I<-I+1;
  }else{
    R<-R+1;
    I<-I-1;
  }
  
  Is[iii]<-I;
  iii=iii+1;
}
  

while(I>2400){
  newInfects<-rpois(1,dt*S*I*InfectionRate/N)
  newRecov<-rpois(1,dt*I*recoveryRate)
  
  S<-S-newInfects;
  I<-I+newInfects-newRecov;
  R<-R+newRecov
  Ts[iii]<-Ts[iii-1]+dt;
  Is[iii]<-I;
  iii=iii+1;
}



while(I<2700 &I>0){
  CurrentInfectRate<-S*I*InfectionRate/N;
  currentRecovRate<-I*recoveryRate;
  Ts[iii]<-Ts[iii-1]+rexp(1, rate = CurrentInfectRate+currentRecovRate);
  if(rbinom(n = 1,  size = 1, prob = CurrentInfectRate/(CurrentInfectRate+currentRecovRate))){
    S<-S-1;
    I<-I+1;
  }else{
    R<-R+1;
    I<-I-1;
  }
  
  Is[iii]<-I;
  iii=iii+1;
}

lines(Ts,Is)
}