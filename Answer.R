# Exercise 12
# Define paraemeters and initial conditions 
### When there's no drug present, rN=rM=0.1
### When there's a drug present, rN=-0.1 and rM=0.5
rN1=0.1
rM1=0.1
rM2=0.5
rN2=-0.1
### Carrying capacity K=1000000
K=1000000
timesteps=500
# Set up an output to populate
# non-mutant data
Ns=data.frame(time=1:timesteps,sim1=rep(0,timesteps))
# mutant data
Ms=data.frame(time=1:timesteps,sim1=rep(0,timesteps))
Ns[1,2]=99
Ms[1,2]=1

#N: Nt+1=Nt+rN*Nt*(1-(Nt+Mt)/K)
#M: Mt+1=Mt+rM*Mt*(1-(Nt+Mt)/K)
# Create for loop
for(t in 2:timesteps){
  # drug treatment will come in at t=200
  if(t<200){
  Ns$sim1[t]=Ns$sim1[t-1]+rN1*Ns$sim1[t-1]*(1-(Ns$sim1[t-1]+Ms$sim1[t-1])/K)
  Ms$sim1[t]=Ms$sim1[t-1]+rM1*Ms$sim1[t-1]*(1-(Ns$sim1[t-1]+Ms$sim1[t-1])/K)}
  # once treatment comes in, use rN2 and rM2
  else{
  Ns$sim1[t]=Ns$sim1[t-1]+rN2*Ns$sim1[t-1]*(1-(Ns$sim1[t-1]+Ms$sim1[t-1])/K)
  Ms$sim1[t]=Ms$sim1[t-1]+rM2*Ms$sim1[t-1]*(1-(Ns$sim1[t-1]+Ms$sim1[t-1])/K)}
}
# convert wide data to long form
Ns2<-data.frame(time=c(Ns$time),N=c(Ns$sim1),sim=rep(c("non-mutant"),each=timesteps))
Ms2<-data.frame(time=c(Ms$time),M=c(Ms$sim1),sim=rep(c("mutant"),each=timesteps))
# model interacting cells 
# plot simulation
library(ggplot2)
ggplot(data=Ns2,aes(x=time,y=N,color=sim)) + 
  geom_line() +
  geom_line(data=Ms2,aes(x=time,y=M,color=sim))+
  theme_classic()+
  # dotted line shows when treatment comes in 
  geom_vline(xintercept=c(200), linetype="dotted")

