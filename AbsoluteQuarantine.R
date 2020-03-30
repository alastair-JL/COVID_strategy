
#install.packages("optimbase")
library(optimbase)

PopulationByAge<- c(615364.71,
                    606025.839,
                    664742.457,
                    611028.789,
                    613469.625,
                    611028.789,
                    488835.159,
                    317385.06,
                    169555.014)

## Population of New Zealand, divided by age. 
## as infered from https://www.stats.govt.nz/tools/2018-census-place-summaries/new-zealand?gclid=EAIaIQobChMItJuSsKW-6AIVi6qWCh3WnANeEAAYASAAEgKhVfD_BwE
## By the age distribution of each gender by the corresponding population.


ICUriskByAge<- c(5E-05,
                 0.00015,
                 0.0006,
                 0.0016,
                 0.003087,
                 0.012444,
                 0.045484,
                 0.104976,
                 0.193557)
## Risk of ICU admission, by age group, as impled by Table 1 of 
## ``Impact of non-pharmaceutical interventions (NPIs) to reduce COVID-19 mortality and healthcare demand''


beta<-0.25 #Infection rate.
gamma<-0.1 #Recovery rate.

N= sum(PopulationByAge)#total Population  
  
dt<-1/100;

ICUdemandByTime<-zeros(1,round(600/dt));

ICUdemandOverThreshold<-zeros(1,round(600/dt));
ICUdemandUnderThreshold<-zeros(1,round(600/dt));


timeVect<- (1:length(ICUdemandByTime))*dt;

currentState<- matrix(c(PopulationByAge,PopulationByAge*0,PopulationByAge*0), length(PopulationByAge) ,3)

currentState[3,2]<-1; ##Patient zero. Assumed to be in 20's, decision is arbitrary.


kappaQuarantine<- 0.1;
kappaCross<-0.0;

kappaB<- ones(length(ICUriskByAge)) #Contact matrix.
kappa=kappaB;

for(ttt in (1:length(ICUdemandByTime))){
  recoveries<- dt*gamma*currentState[,2];
  infections<-  dt*beta*currentState[,1]*(kappa%*%currentState[,2])/N;
  
  currentState[,1]<-currentState[,1]-infections;
  currentState[,2]<-currentState[,2]+infections-recoveries;
  currentState[,3]<-currentState[,3]+recoveries;
  
  ICUdemandByTime[ttt]<- sum(ICUriskByAge*currentState[,2])
  
}


plot(timeVect, timeVect*0+520/100,col="black",
     ylab="ICU demand (hundreds)",
     xlab="Days Since Epidemic start",
     type="l",
     ylim=c(0,12),lwd=2)


K=length(PopulationByAge) ##Number of age classes


Thresholds=c(5)

cl <- rainbow(10);


for(Threshold in Thresholds){
  
  if(Threshold==0){
    kappaQ<-kappaQuarantine*ones(K,K) 
  }else{
    kappaQ<- cbind(rbind(ones(Threshold,Threshold)/(sum(PopulationByAge[1:Threshold])/sum(PopulationByAge)), kappaCross*ones(K-Threshold,Threshold) ),
               rbind( kappaCross*ones(Threshold,K-Threshold), kappaQuarantine*ones(K-Threshold,K-Threshold) ))
   }
currentState<- matrix(c(PopulationByAge,PopulationByAge*0,PopulationByAge*0), length(PopulationByAge) ,3)
currentState[3,2]<-1; ##Patient zero. Assumed to be in 20's, decision is arbitrary.



for(ttt in (1:length(ICUdemandByTime))){
  if(ttt> 50*100 && ttt<170*100){
    kappa=kappaQ;
  }else{
    kappa=kappaB;
  }
  
 recoveries<- dt*gamma*currentState[,2];
 infections<-  dt*beta*currentState[,1]*(kappa%*%currentState[,2])/N;
 
 currentState[,1]<-currentState[,1]-infections;
 currentState[,2]<-currentState[,2]+infections-recoveries;
 currentState[,3]<-currentState[,3]+recoveries;
 
 ICUdemandByTime[ttt]<- sum(ICUriskByAge*currentState[,2])
 ICUdemandOverThreshold[ttt]<-sum(ICUriskByAge[Threshold:length(ICUriskByAge)]*currentState[Threshold:length(ICUriskByAge),2]);
 ICUdemandUnderThreshold[ttt]<-sum(ICUriskByAge[1:Threshold]*currentState[1:Threshold,2]);
}

lines(timeVect, ICUdemandByTime/100,col=cl[Threshold+1],lwd=2)
}


Threshold=5;
kappaQ1<- cbind(rbind( 0.8*ones(Threshold,Threshold)/(sum(PopulationByAge[1:Threshold])/sum(PopulationByAge)), kappaCross*ones(K-Threshold,Threshold) ),
                rbind( kappaCross*ones(Threshold,K-Threshold), kappaQuarantine*ones(K-Threshold,K-Threshold) ))

Threshold=6;
kappaQ2<- cbind(rbind( ones(Threshold,Threshold)/(sum(PopulationByAge[1:Threshold])/sum(PopulationByAge)), kappaCross*ones(K-Threshold,Threshold) ),
                rbind( kappaCross*ones(Threshold,K-Threshold), kappaQuarantine*ones(K-Threshold,K-Threshold) ))


Threshold=7;
kappaQ3<- cbind(rbind( ones(Threshold,Threshold)/(sum(PopulationByAge[1:Threshold])/sum(PopulationByAge)), kappaCross*ones(K-Threshold,Threshold) ),
                rbind( kappaCross*ones(Threshold,K-Threshold), kappaQuarantine*ones(K-Threshold,K-Threshold) ))


currentState<- matrix(c(PopulationByAge,PopulationByAge*0,PopulationByAge*0), length(PopulationByAge) ,3)
currentState[3,2]<-1; ##Patient zero. Assumed to be in 20's, decision is arbitrary.


for(ttt in (1:length(ICUdemandByTime))){
  if(ttt> 65*100 && ttt<160*100){
    kappa=kappaQ1;
  }else if(ttt> 160*100 && ttt<255*100) {
    kappa=kappaQ2;  
  } else{
    kappa=kappaB;
  }
  
  recoveries<- dt*gamma*currentState[,2];
  infections<-  dt*beta*currentState[,1]*(kappa%*%currentState[,2])/N;
  
  currentState[,1]<-currentState[,1]-infections;
  currentState[,2]<-currentState[,2]+infections-recoveries;
  currentState[,3]<-currentState[,3]+recoveries;
  
  ICUdemandByTime[ttt]<- sum(ICUriskByAge*currentState[,2])
}

lines(timeVect, ICUdemandByTime/100,col='red',lwd=2)


legend("topright", legend = c('ICU capacity','Single stage','Multistage'), col=c('black','blue','red'), pch=1)
