library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

vett_niter<-c(1,5,15,25,50,75,100)
nmonth=12
ncomp=6
sim_day<-30
nsog=50
#res<-data.frame()
temp<-c()
dati<-data.frame()
dataset<-data.frame()

q_list<-list()
for(i in 1:nsog){
  q_list<-c(q_list,list(matrix(data=runif(2500,0,10),nrow=250,ncol=10)))
}
q_list_pop<-list()
for(i in 1:50000){
  q_list_pop<-c(q_list_pop,list(matrix(data=runif(2500,0,10),nrow=250,ncol=10)))
}


for(j in 1:length(vett_niter)){
  niter<-vett_niter[j]
  rm(list=c("dataset","dati"))
  inTime<-Sys.time()
  for(iter in 1:niter){
    dataset<-read.csv("DATASET_50sog.csv")
    dataset$AMT<-as.character(dataset$AMT)
    dataset$C<-as.character(dataset$C)
      for(k in 1:nmonth){
        if(k==1){
          dataset$C[which(dataset$TIME==0 & dataset$MDV==1 & dataset$EVID==1)]<-"C"
        }
        else{
          dati<-read.delim("sdtab2",sep="",skip=1)
          dataset$C[which(dataset$TIME==0 & dataset$MDV==1 & dataset$EVID==1)]<-"."
          for(i in 1:nsog){
            if(dati$admPK[which(dati$ID==i)][length(dati$admPK[which(dati$ID==i)])]<=0){
              dataset$C[which(dataset$TIME==0 & dataset$MDV==1 & dataset$EVID==1 & dataset$ID==i & dataset$CMT==1)]<-"C"
            }
            else{
              dataset$AMT[which(dataset$TIME==0 & dataset$MDV==1 & dataset$EVID==1 & dataset$ID==i & dataset$CMT==1)]<-dati$admPK[which(dati$ID==i)][length(dati$admPK[which(dati$ID==i)])]
            }
              dataset$AMT[which(dataset$TIME==0 & dataset$MDV==1 & dataset$EVID==1 & dataset$ID==i & dataset$CMT==2)]<-dati$centPK[which(dati$ID==i)][length(dati$centPK[which(dati$ID==i)])]
              dataset$AMT[which(dataset$TIME==0 & dataset$MDV==1 & dataset$EVID==1 & dataset$ID==i & dataset$CMT==3)]<-dati$peri1PK[which(dati$ID==i)][length(dati$peri1PK[which(dati$ID==i)])]
              dataset$AMT[which(dataset$TIME==0 & dataset$MDV==1 & dataset$EVID==1 & dataset$ID==i & dataset$CMT==4)]<-dati$peri2PK[which(dati$ID==i)][length(dati$peri2PK[which(dati$ID==i)])]
              dataset$AMT[which(dataset$TIME==0 & dataset$MDV==1 & dataset$EVID==1 & dataset$ID==i & dataset$CMT==5)]<-dati$compeffPD[which(dati$ID==i)][length(dati$compeffPD[which(dati$ID==i)])]
              dataset$AMT[which(dataset$TIME==0 & dataset$MDV==1 & dataset$EVID==1 & dataset$ID==i & dataset$CMT==6)]<-dati$attPD[which(dati$ID==i)][length(dati$attPD[which(dati$ID==i)])]
              }
        }
        write.csv(dataset, "DATASET_50sog.csv", quote=F, row.names=F)
        system2(command="nmfe74",args="mod_fin_sim.mod mod_fin.lst")
        dati<-read.delim("sdtab2",sep="",skip=1)
        dati<-dati[which(dati$MDV==0),]
        #res<-rbind(res,data.frame(ID=dati$ID,TIME=dati$TIME+(k-1)*30*24,PO4=dati$E))
      }
  }
  outTime<-Sys.time()
  temp[j]<-difftime(outTime,inTime,units="secs")
}

png("timesplot.png",width = 800, height = 500,units = 'px')
plot(vett_niter,temp,xlab="numero iterazioni",ylab="tempo [secs]",cex=2)
dev.off()
#ggplot()+geom_line(data=res,aes(x=TIME/24,y=PO4,group=ID,color=as.factor(ID)))

