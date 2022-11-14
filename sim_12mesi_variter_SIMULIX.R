library(RsSimulx)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

param_ind<-read.csv("individual_param.csv")
param_ind<-param_ind[,1:19]


vett_niter<-c(1,5,15,25,50,75,100)
nmonth=12
sim_day<-30
nsog=50
q_list<-list()
for(i in 1:nsog){
  q_list<-c(q_list,list(matrix(data=runif(2500,0,10),nrow=250,ncol=10)))
}
q_list_pop<-list()
for(i in 1:50000){
  q_list_pop<-c(q_list_pop,list(matrix(data=runif(2500,0,10),nrow=250,ncol=10)))
}

tr<-c()
o1<-c()
res2<-list()
#res<-data.frame()

temp<-c()
for(j in 1:length(vett_niter)){
  niter<-vett_niter[j]
  rm(list=c("tr","o1","res2"))
  print("niter: ")
  print(niter)
  inTime<-Sys.time()
  for(iter in 1:niter){
    print("iter")
    print(iter)
    for(k in 1:nmonth){
      if(k==1){
        param_ind$T0<-0
        param_ind$q10<-0
        param_ind$q20<-0
        param_ind$q30<-0
        param_ind$q40<-0
        param_ind$Ce0<-0
      }
      else{
        for (i in 1:nsog) {
          param_ind$T0[i]<-res2$T$T[which(res2$T$id==i)][length(res2$T$T[which(res2$T$id==i)])]
          param_ind$Ce0[i]<-res2$Ce$Ce[which(res2$Ce$id==i)][length(res2$Ce$Ce[which(res2$Ce$id==i)])]
          param_ind$q10[i]<-res2$q1$q1[which(res2$q1$id==i)][length(res2$q1$q1[which(res2$q1$id==i)])]
          param_ind$q20[i]<-res2$q2$q2[which(res2$q2$id==i)][length(res2$q2$q2[which(res2$q2$id==i)])]
          param_ind$q30[i]<-res2$q3$q3[which(res2$q3$id==i)][length(res2$q3$q3[which(res2$q3$id==i)])]
          param_ind$q40[i]<-res2$q4$q4[which(res2$q4$id==i)][length(res2$q4$q4[which(res2$q4$id==i)])]
        }
      }
    tr<-list(time=seq(0,sim_day*24,by=24), amount=8000)
    o1 <- list(name=c('Ctot','Cfree','PO4', 'Ce','T','q1','q2','q3','q4'),time=seq(0,sim_day*24,by=24))
    
    res2<-simulx(model="modello_PKPD_simulx_fin.txt", parameter = param_ind, output=o1, treatment = tr)
    #res<-rbind(res,data.frame(id=res2$PO4$id,time=res2$PO4$time+(k-1)*30*24,PO4=res2$PO4$PO4))
    }
  }
  outTime<-Sys.time()
  temp[j]<-difftime(outTime,inTime,units="secs")
}
plot(vett_niter,temp,xlab="numero iterazioni",ylab="tempo [secs]")

#ggplot()+geom_line(data=res,aes(x=time/24,y=PO4,group=id,color=as.factor(id)))
