# preparazione dataset (1 paziente)
ID<-c(1)
SEX<-c(0)#per NM codifica 0 (M) o 1 (F)
IMPREN2<-c(1)
IMPREN3<-c(0)
wt<-c(71.6)
WT1<-c(0)
WT2<-c(0)
AGP<-c("1.24")
demo<-data.frame(ID,SEX,IMPREN2,IMPREN3,WT1,WT2,AGP)
#####################################################
TIME<-0.0001
II<-c(24)
ADDL<-c(29)
AMT<-c(8*10^3) #unita' di misura da verificare
#AMT<-8
DAY<-c(0)
EVID<-c(1)
MDV<-c(1)
CMT<-1
adm<-data.frame(ID,TIME,DAY,II,ADDL,AMT,EVID,MDV,CMT)
adm$DV<-"."
#####################################################
TIME<-seq(0,24*30,6)
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
#DAY<-seq(0,30,1)
obs<-data.frame(TIME,DAY)
obs$ID<-1
obs$CMT<-5
obs$MDV<-0
obs$EVID<-0
obs$II<-"."
obs$ADDL<-"."
obs$AMT<-"."
obs$DV<-"."
#####################################################
CL<-("83.2")
V2<-"18.9"
V3<-"2480"
Q3<-("129")
V4<-("767")
Q4<-"2.72"
KA<-2.34
ALAG<-0.232
KD<-32
BSEXCL<- -0.188
BIMPREN2CL<-0.211
BIMPREN3CL<-0.219
BWT1V2<- -0.0982
BWT2V2<- -0.198
BSEXV2<- -0.164
POWERAGP<- -0.473
PO40<- 3.08
PO4P<-2.67
SLOPEm<-0.869
GAMMA<-0.860
parKE0<- 0.0199
parKIN<-0.000102
parKBASE<-0.0101
TLAG<-143
TSLD<-24
THETASEX<-0.124
parameters<-data.frame(CL,V2,V3,Q3,V4,Q4,KA,ALAG,KD,BSEXCL,BIMPREN2CL,BIMPREN3CL,BWT1V2,BWT2V2,BSEXV2,POWERAGP,
                       PO40,PO4P,SLOPEm,GAMMA,parKE0,parKIN,parKBASE,TLAG,TSLD,THETASEX)
parameters$ID<-1
#####################################################
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
  adm<-rbind(adm,data.frame(ID,TIME,DAY,II,ADDL,AMT,EVID,MDV,CMT,DV))
}
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
write.csv(dataset, "DATASET2.csv", quote=F, row.names=F)


