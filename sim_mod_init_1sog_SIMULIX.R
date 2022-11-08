library(ggplot2)
library(RsSimulx)

setwd("C:/Users/Win/Desktop/TESI MAGLISTRALE/RL_Erdafitnib")

sim_day<-30
Kd=32
AGP=1.24 #AGP mediano
fu=Kd/(Kd+(AGP*10632))
param<-c(tlag_pop=0.232, ka_pop=2.34, CL_pop=83.2, V2_pop=18.9, power_AGP_pop=-0.473, 
         bV2WT1_pop=0.0982, bV2WT2_pop=0.198, bSEXV2_pop= -0.164,bSEXCL_pop=-0.188, bIMPREN2_pop=0.211, bIMPREN3_pop=0.219,
         Q3_pop=129, V3_pop=2480, Q4_pop=2.72, V4_pop=767,fu_pop=fu, 
         ke0_pop=0.0199, gamma_pop=0.860, m_pop=0.869, kin_pop=0.000102, PO40_pop=3.08, PO4P_pop=2.67, 
         kbase_pop=0.0101, TSLD_pop=24, tlag_att_pop=143,bSEXBSL_pop=0.124)
o1 <- list(name=c('Ctot','Cfree','PO4', 'Ce'),time=seq(0,sim_day*24,by=6))
tr <- list(time=seq(0,sim_day*24,by=24), amount=8000)
#sex=0-->maschio
cov<-data.frame(id=1, AGP=1.24, WT1=0,WT2=1, SEX=0, IMPREN2=1, IMPREN3=0)
res<-simulx(model="modello_PKPD_simulx_init.txt", parameter = param, output=o1, treatment = tr, covariate = cov)

time_day<-c()
day=0;
i=1;
for (t in res$Cfree$time) {
  time_day[i]<-day
  if(t%%24==0){
    day=day+1;
  }
  i=i+1;
}

ggplot(data=res$Ctot,aes(x=time_day, y=Ctot)) +geom_line()+xlab("TIME(day)")+ylab("Erdafitinib total concentration [ng/mL]")
ggplot(data=res$Cfree,aes(x=time_day, y=Cfree)) +geom_line()+xlab("TIME(day)")+ylab("Erdafitinib free concentration [ng/mL]")
ggplot(data=res$PO4,aes(x=time_day, y=PO4)) +geom_line()+xlab("TIME(day)")+ylab("Serum phospate concentrations[mg/dL] ")

pdf("Plot_sim_1sog_SIMULIX.pdf")

par(mar=c(4, 4, 4, 4) + 0.1)
plot(time_day, res$PO4$PO4, type = "l", axes = FALSE, xlab = "", ylab = "")
axis(2)
mtext("Serum phospate concentrations[mg/dL]",side=2,line=2.5)
box()
#Draw time axis
axis(1)
mtext("TIME (day)",side=1,line=2.5)
par(new = TRUE)
plot(time_day, res$Cfree$Cfree,type = "l", lty=2,axes=FALSE, xlab="", ylab="",col="blue")
axis(4,col="blue",col.axis="blue")
mtext("Erdafitinib free concentrations [ng/mL]",side=4,line=2.5, col="blue")
lines(time_day, res$Ce$Ce,type = "l", lty=1, xlab="", ylab="",col="blue") 
legend(15,1,legend=c("PO4","Erdafitinib plasma", "Erdafitinib biophase"),
       text.col=c("black","blue","blue"),lty=c(1,2,1),col=c("black","blue","blue"),cex=0.7 )

dev.off()
