
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

ICUdemandByTime<-zeros(1,round(520/dt));

ICUdemandOverThreshold<-zeros(1,round(520/dt));
ICUdemandUnderThreshold<-zeros(1,round(520/dt));


timeVect<- (1:length(ICUdemandByTime))*dt;

currentState<- matrix(c(PopulationByAge,PopulationByAge*0,PopulationByAge*0), length(PopulationByAge) ,3)

currentState[3,2]<-1; ##Patient zero. Assumed to be in 20's, decision is arbitrary.


kappaQuarantine<- 0.1;
kappaCross<-0.1;
Threshold=6;

kappaB<- ones(length(ICUriskByAge)) #Contact matrix.


K=length(PopulationByAge) ##Number of age classes


Thresholds=1:(K-1)

cl <- rainbow(10);

legend=c()

ObservedCaseLeak=Thresholds;
PredictedCaseLeak=Thresholds;
FractionInfected=Thresholds;


for(Threshold in Thresholds){
  
  if(Threshold==0){
    kappaQ<-kappaQuarantine*ones(K,K) 
    legend<-c(legend,'Full Quarantine')
  }else if (Threshold>=K) {
    kappaQ<-ones(K,K) 
    legend<-c(legend,'No Quarantine')
  }else{
    
    kappaQ<- cbind(rbind( ones(Threshold,Threshold)/(sum(PopulationByAge[1:Threshold])/sum(PopulationByAge)), kappaCross*ones(K-Threshold,Threshold) ),
                   rbind( kappaCross*ones(Threshold,K-Threshold), kappaQuarantine*ones(K-Threshold,K-Threshold) ))
    legend<-c(legend, paste0(Threshold*10, '+'))
  }
  currentState<- matrix(c(PopulationByAge,PopulationByAge*0,PopulationByAge*0), length(PopulationByAge) ,3)
  currentState[3,2]<-1; ##Patient zero. Assumed to be in 20's, decision is arbitrary.
  
  
  kappa=kappaQ;
  
  for(ttt in (1:length(ICUdemandByTime))){
    recoveries<- dt*gamma*currentState[,2];
    infections<-  dt*beta*currentState[,1]*(kappa%*%currentState[,2])/N;
    
    currentState[,1]<-currentState[,1]-infections;
    currentState[,2]<-currentState[,2]+infections-recoveries;
    currentState[,3]<-currentState[,3]+recoveries;
    
    ICUdemandByTime[ttt]<- sum(ICUriskByAge*currentState[,2])
    ICUdemandOverThreshold[ttt]<-sum(ICUriskByAge[Threshold:length(ICUriskByAge)]*currentState[Threshold:length(ICUriskByAge),2]);
    ICUdemandUnderThreshold[ttt]<-sum(ICUriskByAge[1:Threshold]*currentState[1:Threshold,2]);
  }
  
   ObservedCaseLeak[Threshold]<-sum((currentState[(Threshold+1):length(ICUriskByAge),2:3]));
   PredictedCaseLeak[Threshold]<- (sum(PopulationByAge[(Threshold+1):length(ICUriskByAge)])*sum(PopulationByAge[1:Threshold])/sum(PopulationByAge) )*(beta*kappaCross/(gamma-beta*kappaQuarantine)) ;
   FractionInfected[Threshold]<-sum((currentState[(1:Threshold),2:3]))/sum((currentState[(1:Threshold),1:3]));
}

plot(Thresholds*10, PredictedCaseLeak*FractionInfected/1000,col="red",
     ylab="Cases (thousands)",
     xlab="Quarantine Threshold (age)",pch = 4,
     type="p",lwd=2)

points(Thresholds*10,ObservedCaseLeak/1000,col='black')

