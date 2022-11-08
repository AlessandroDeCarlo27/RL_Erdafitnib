library(RsSimulx)
library(ggplot2)

setwd("C:/Users/Win/Desktop/TESI MAGLISTRALE/RL_Erdafitnib")

nsog=50
sim_day<-30
Kd=32
AGP=1.24 #AGP mediano
fu=Kd/(Kd+(AGP*10632))

#parametri
param<-c(tlag_pop=0.232, ka_pop=2.34, CL_pop=83.2, V2_pop=18.9, power_AGP_pop=-0.473, 
                  bV2WT1_pop=0.0982, bV2WT2_pop=0.198, bSEXV2_pop= -0.164,bSEXCL_pop=-0.188, bIMPREN2_pop=0.211, bIMPREN3_pop=0.219,
                  Q3_pop=129, V3_pop=2480, Q4_pop=2.72, V4_pop=767,fu_pop=fu, 
                  ke0_pop=0.0199, gamma_pop=0.860, m_pop=0.869, kin_pop=0.000102, PO40_pop=3.08, PO4P_pop=2.67, 
                  kbase_pop=0.0101, TSLD_pop=24, tlag_att_pop=143,bSEXBSL_pop=0.124)

#trattamento
tr <- data.frame(time=seq(0,sim_day*24,by=24), amount=8000)

#covariate
cov<-data.frame()
for(i in 1:50){
  r<-round(runif(5,min=0,max=1))
  cov<-rbind(cov,data.frame(id=i,AGP=1.24,WT1=r[1],WT2=r[2],SEX=r[3],IMPREN2=r[4], IMPREN3=r[5]))
}
#output
o1 <- list(name=c('Ctot','Cfree','PO4', 'Ce'),time=seq(0,sim_day*24,by=24))


res<-simulx(model="modello_PKPD_simulx_init.txt", parameter = param, output=o1, treatment = tr,covariate=cov)

write.csv(res[["parameter"]], "individual_param.csv", quote=F, row.names=F)

#########simulazione con param individuali#####################
sim_day<-30
param_ind_cov<-read.csv("individual_param.csv")
param_ind<-param_ind_cov[,1:19]
tr <- data.frame(time=seq(0,sim_day*24,by=24), amount=8000)
o1 <- list(name=c('Ctot','Cfree','PO4', 'Ce','T','q1','q2','q3','q4'),time=seq(0,sim_day*24,by=24))

inTime <- Sys.time()
message(inTime)

res2<-simulx(model="modello_PKPD_simulx_fin.txt", parameter = param_ind, output=o1, treatment = tr)

outTime <- Sys.time()
message(outTime)



