#####################preparazione dataset (50 soggetti)##########################
setwd("C:/Users/Win/Desktop/TESI MAGLISTRALE/RL_Erdafitnib")
#somministrazione: 8 mg/gg per 30 gg (II=24)
TIME<-0.0001
II<-c(24)
ADDL<-c(29)
AMT<-c(8*10^3) #unita' di misura da verificare
#AMT<-8
DAY<-c(0)
EVID<-c(1)
MDV<-c(1)
CMT<-1
adm<-data.frame(ID=c(1:50),TIME,DAY,II,ADDL,AMT,EVID,MDV,CMT)
adm$DV<-"."

#osservazioni
obs<-data.frame()
for(id in seq(1:50)){
  obs1<-data.frame()
  TIME<-seq(0,24*30,24)
  DAY<-c()
  day=0;
  i=1;
  for (t in TIME) {
    DAY[i]<-day
    if(t%%24==0){
      day=day+1; 
    }
    i=i+1;
  }
  obs1<-data.frame(TIME,DAY)
  obs1$ID<-id
  obs<-rbind(obs,obs1)
}

obs$CMT<-5
obs$MDV<-0
obs$EVID<-0
obs$II<-"."
obs$ADDL<-"."
obs$AMT<-"."
obs$DV<-"."

#parametri e covariate
param_ind_cov<-read.csv("individual_param.csv")

parameters<-data.frame(ID=param_ind_cov$id,CL=param_ind_cov$Cl,V2=param_ind_cov$V2,
                       V3=param_ind_cov$V3,Q3=param_ind_cov$Q3,V4=param_ind_cov$V4,
                       Q4=param_ind_cov$Q4,KA=param_ind_cov$ka,ALAG=param_ind_cov$tlag, fu=param_ind_cov$fu,
                       PO40=param_ind_cov$PO40,PO4P=param_ind_cov$PO4P,SLOPEm=param_ind_cov$m,
                       GAMMA=param_ind_cov$gamma,parKEO=param_ind_cov$ke0,parKIN=param_ind_cov$kin,
                       parKBASE=param_ind_cov$kbase,TLAG=param_ind_cov$tlag_att,TSLD=param_ind_cov$TSLD)

demo<-data.frame(ID=param_ind_cov$id,SEX=param_ind_cov$SEX,IMPREN2=param_ind_cov$IMPREN2,IMPREN3=param_ind_cov$IMPREN3,
                 WT1=param_ind_cov$WT1,WT2=param_ind_cov$WT2)
#condizioni iniziali
ncomp<-6
for(i in 1:ncomp){
  TIME<-0
  II<-"."
  ADDL<-"."
  AMT<-0 
  DAY<-0
  EVID<-1
  MDV<-1
  CMT<-i
  DV<-"."
  adm<-rbind(adm,data.frame(ID=c(1:50),TIME,DAY,II,ADDL,AMT,EVID,MDV,CMT,DV))
}

#costruzione dataset
obs_adm<-rbind(adm,obs)
data<-merge(obs_adm,demo, by.x="ID", by.y="ID")
dataset<-merge(data,parameters,by.x="ID", by.y="ID")
C<-seq(1,length(dataset$ID),1)
dataset<-cbind(C,dataset)
dataset$C<-"."
dataset$C[which(dataset$AMT==0)]<-"C"
dataset<-dataset[order(dataset$ID,dataset$TIME),]
dataset$TIME[which(dataset$TIME==1e-04)]<-"0.0001"
dataset$TIME<-as.character(dataset$TIME)
dataset$AMT[which(dataset$AMT==8*(10^3))]<-"8000"
dataset$AMT<-as.character(dataset$AMT)

#scrittura file
write.csv(dataset, "DATASET_50sog.csv", quote=F, row.names=F)

#####################NONMEM#######################
setwd("C:/Users/Win/Desktop/TESI MAGLISTRALE/RL_Erdafitnib")
inTime <- Sys.time()
message(inTime)
rr<-system2(command="nmfe74",args="mod_fin_sim.mod mod_fin.lst")

#####################lettura sdtab####################
dati1<-read.delim("sdtab2",sep="",skip=1)
#dati1<-dati1[which(dati1$CMT==5),]
outTime <- Sys.time()
message(outTime)

library(ggplot2)
# pdf("Plot.pdf")
# ggplot()+geom_line(data=dati1,aes(x=TIME/24,y=E,group=ID, color=as.factor(ID)))+xlab("TIME(day)")+ylab("Serum phospate concentrations[mg/dL] ")
# 
# ggplot()+geom_line(data=res2$PO4,aes(x=time/24,y=PO4,group=id, color=as.factor(id)))+xlab("TIME(day)")+ylab("Serum phospate concentrations[mg/dL] ")
# 
# ggplot()+geom_line(data=dati1,aes(x=TIME/24,y=Cfree,group=ID, color=as.factor(ID)))+xlab("TIME(day)")+ylab("Erdafitinib free concentration [ng/mL]")
# 
# ggplot()+geom_line(data=res2$Cfree,aes(x=time/24,y=Cfree,group=id, color=as.factor(id)))+xlab("TIME(day)")+ylab("Erdafitinib free concentration [ng/mL]")
# 
# ggplot()+geom_line(data=dati1,aes(x=TIME/24,y=Ctot,group=ID, color=as.factor(ID)))+xlab("TIME(day)")+ylab("Erdafitinib total concentration [ng/mL]")
# 
# ggplot()+geom_line(data=res2$Ctot,aes(x=time/24,y=Ctot,group=id, color=as.factor(id)))+xlab("TIME(day)")+ylab("Erdafitinib total concentration [ng/mL]")
# dev.off()



